#' @title summarize_emails
#' @export
summarize_emails <- function(emails){
  x <- data.frame(
    i = 1:length(emails),
    id = emails %>%
      purrr::map_depth(1,"properties") %>%
      purrr::map_depth(1,"id") %>%  unlist(),
    subject = emails %>%
      purrr::map_depth(1,"properties") %>%
      purrr::map_depth(1,"subject") %>%  unlist(),
    email = emails %>%
      purrr::map_depth(1,"properties") %>%
      purrr::map_depth(1,"from")%>%
      purrr::map_depth(1,"emailAddress")%>%
      purrr::map_depth(1,"address") %>%
      unlist(),
    name = emails %>%
      purrr::map_depth(1,"properties") %>%
      purrr::map_depth(1,"from")%>%
      purrr::map_depth(1,"emailAddress")%>%
      purrr::map_depth(1,"name") %>%
      unlist()
  )
  x$email2 <- x$email %>% strsplit(split = "@") %>% sapply(dplyr::last) %>% unlist()
  return(x)
}
#' @title choose_emails_to_delete_in_bulk
#' @export
choose_emails_to_delete_in_bulk <- function(inbox,full_address = T,n=1000){
  ADDRESS_TYPE <- "email"
  if(!full_address) ADDRESS_TYPE <- "email2"
  emails <- inbox$list_emails(n=n)
  emails_sum <- summarize_emails(emails)
  email_addresses <- emails_sum[[ADDRESS_TYPE]] %>% table() %>% sort(decreasing = T) %>% names()
  email_addresses_count <- emails_sum[[ADDRESS_TYPE]] %>% table() %>% sort(decreasing = T)
  for (address in email_addresses){ # address <- email_addresses%>% sample(1)
    rows <- which(emails_sum[[ADDRESS_TYPE]]==address)
    emails_sum$subject[rows] %>% print()
    # emails_sum$i[rows] %>% print()
    choice <- utils::menu(choices = c("Yes","No"),title = paste0("You want to delete emails from ",address,"?"))
    if(choice==1){
      emails_from <- outlook$list_emails(search = paste0("from:",address),n=n)
      emails_from_sum <- summarize_emails(emails_from)
      email_from_addresses <- emails_from_sum[[ADDRESS_TYPE]] %>% table() %>% sort(decreasing = T) %>% names()
      email_from_addresses_count <- emails_from_sum[[ADDRESS_TYPE]] %>% table() %>% sort(decreasing = T)
      rows_from <- which(emails_from_sum[[ADDRESS_TYPE]]==address)
      emails_from_sum$subject[rows_from] %>% print()
      choice_from <- utils::menu(choices = c("Yes","No"),title = paste0("Are you sure (see above)? ",address,"?"))
      if(choice_from==1){
        for (row_from in rows_from){# row_from <- rows_from %>% sample(1)
          try({emails_from[[emails_from_sum$i[row_from]]]$delete("confirm"==F)})
          the_address <- emails_from[[emails_from_sum$i[row_from]]]$properties$from$emailAddress$address
          the_subject <- emails_from[[emails_from_sum$i[row_from]]]$properties$subject
          message("Deleted email: ",the_address," --> ",the_subject)
        }
      }
    }
  }
}
