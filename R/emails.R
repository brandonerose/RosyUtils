#' @title summarize_emails
#' @export
summarize_emails <- function(emails){
  x <- data.frame(
    i = seq_along(emails),
    id = emails %>%
      purrr::map_depth(1,"properties") %>%
      purrr::map_depth(1,"id") %>%  unlist(),
    subject = emails %>%
      purrr::map_depth(1,"properties") %>%
      purrr::map_depth(1,"subject") %>%  unlist(),
    from = emails %>%
      purrr::map_depth(1,"properties") %>%
      purrr::map_depth(1,"from")%>%
      purrr::map_depth(1,"emailAddress")%>%
      purrr::map_depth(1,"address") %>%
      unlist(),
    from_root = NA,
    from_name = emails %>%
      purrr::map_depth(1,"properties") %>%
      purrr::map_depth(1,"from")%>%
      purrr::map_depth(1,"emailAddress")%>%
      purrr::map_depth(1,"name") %>%
      unlist(),
    sender = emails %>%
      purrr::map_depth(1,"properties") %>%
      purrr::map_depth(1,"sender")%>%
      purrr::map_depth(1,"emailAddress")%>%
      purrr::map_depth(1,"address") %>%
      unlist(),
    sender_root = NA,
    sender_name = emails %>%
      purrr::map_depth(1,"properties") %>%
      purrr::map_depth(1,"sender")%>%
      purrr::map_depth(1,"emailAddress")%>%
      purrr::map_depth(1,"name") %>%
      unlist()
  )
  x$from_root <- x$from %>% strsplit(split = "@") %>% sapply(dplyr::last) %>% unlist()
  x$sender_root <- x$sender %>% strsplit(split = "@") %>% sapply(dplyr::last) %>% unlist()
  x$same <- x$from == x$sender
  x$same_root <- x$from_root == x$sender_root
  return(x)
}
#' @title count_emails
#' @export
count_emails <- function(emails_sum,ADDRESS_TYPE){
  x <- emails_sum[[ADDRESS_TYPE]] %>% table() %>% sort(decreasing = T)
  y <- names(x)
  z <- names(x) %>% sapply(function(address){emails_sum[[paste0(ADDRESS_TYPE,"_name")]][which(emails_sum[[ADDRESS_TYPE]]==address)][1]}) %>% as.character()
  return(data.frame(
    count = as.integer(x),
    address = y,
    name = z
  ))
}
#' @title choose_emails_to_delete_in_bulk
#' @export
choose_emails_to_delete_in_bulk <- function(outlook = Microsoft365R::get_business_outlook(),full_address = T,use_sender = T,n=2000){
  ADDRESS_TYPE <- use_sender %>% ifelse("sender","from")
  ADDRESS_TYPE <- full_address %>% ifelse(ADDRESS_TYPE,paste0(ADDRESS_TYPE,"_root"))
  message("Getting emails... This can take several seconds!")
  emails <- outlook$list_emails(n=n,pagesize = 100)
  emails_sum <- summarize_emails(emails)
  emails_counted <- count_emails(emails_sum = emails_sum,ADDRESS_TYPE = ADDRESS_TYPE)
  for (row in seq_len(nrow(emails_counted))){ # row <- seq_len(nrow(emails_counted) %>% sample(1)
    address <- emails_counted$address[row]
    name <- emails_counted$name[row]
    message("Searching for emails from '",address,"' (",name,") ...")
    vec <- emails_sum$subject[which(tolower(emails_sum[[ADDRESS_TYPE]]) == tolower(address))]
    vec %>% print()
    choice_from <- utils::menu(choices = c("Yes","No","Choose Individually"),title = paste0("Would like to delete email(s) from '",address,"'? (n = ",length(vec),")"))
    if(choice_from %in% c(1,3)){
      choose_emails_to_delete_from(outlook = outlook, address = address,ask = F, individual_choice = choice_from == 3)
    }
  }
}
#' @title choose_emails_to_delete_from
#' @export
choose_emails_to_delete_from <- function(outlook,address,n=2000, individual_choice = F,ask = T){
  choice_from <- 1
  if(ask){
    choice_from <- 0
    if(!individual_choice){
      choice_from <- utils::menu(choices = c("Yes","No","Choose Individually"),title = paste0("Would like to delete email(s) from '",address,"'?"))
    }
    if(choice_from == 3) individual_choice <- T
  }
  if(choice_from %in% c(1,3)){
    emails_from <- outlook$list_emails(search = paste0("from:",address),n=n)
    if(length(emails_from)==0)return(message("No emails from '",address,"'"))
    emails_from_sum <- summarize_emails(emails_from)
    emails_from_sum$subject %>% print()
    message("n = ",nrow(emails_from_sum),")")
    for (i in seq_along(emails_from)){# i <- seq_along(emails_from) %>% sample(1)
      the_address <- emails_from[[i]]$properties$from$emailAddress$address
      the_subject <- emails_from[[i]]$properties$subject
      if(individual_choice){
        choice_from <- utils::menu(choices = c("Yes","No"),title = paste0("Would like to delete email from '",the_address,"'? --> ",the_subject))
      }
      if(choice_from==1){
        try({emails_from[[i]]$delete(confirm=F)})
        message("Deleted email: ",the_address," --> ",the_subject)
      }
    }
  }
}
