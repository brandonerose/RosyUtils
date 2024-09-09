#' @export
add_df_flag <- function(DF,flag_field_name,id_field_name,ids,flag_name,read_split=" [:|:] ",write_split = " | ",remove_previous = T){
  if(any(!ids%in%DF[[id_field_name]]))stop( "Some of your record IDs are not included in the set of the current database IDs! Add them first...")
  flag_vector <- DF[[flag_field_name]] %>% strsplit(split=read_split)
  DF[[flag_field_name]] <- 1:nrow(DF) %>% sapply(function(ROW){
    if(!is.na(DF[[flag_field_name]][ROW])){
      IN <- flag_vector[[ROW]]
    }else{
      IN <- NULL
    }
    if(remove_previous){
      IN <- IN[which(IN!=flag_name)] #remove flag_name
    }
    if(DF[[id_field_name]][ROW]%in%ids){
      IN <- IN %>% append(flag_name) # add id if it should be there
    }
    if(is.null(IN))return(NA)
    return(IN %>% sort() %>% unique() %>% trimws() %>% paste0(collapse = " | ") )#sort and clean
  })
  return(DF)
}
#' @export
check_df_flag <- function(DF,flag_field_name,split=" [|] "){
  #add warning for grep conflicts
  DF[[flag_field_name]] %>% strsplit(split = split) %>% unlist() %>% unique() %>% drop_nas() %>% sort() %>% return()
}
#' @export
remove_df_flag <- function(DF,flag_field_name,flag_name){
  if(nrow(DF)>0){
    flag_vector<- DF[[flag_field_name]] %>% strsplit(" [:|:] ")
    DF[[flag_field_name]] <- 1:nrow(DF) %>% sapply(function(ROW){
      if(!is.na(DF[[flag_field_name]][ROW])){
        IN <- flag_vector[[ROW]]
      }else{
        IN <- NULL
      }
      IN <- IN[which(IN!=flag_name)] #remove flag_name
      if(!is.null(IN)){
        return(IN %>% sort() %>% unique() %>% trimws() %>% paste0(collapse = " | ") )#sort and clean
      }else{
        return(NA)
      }
    })
  }
  return(DF)
}
#' @export
get_df_flag_rows <- function(DF,flag_field_name,flag_name){
  return(which(DF[[flag_field_name]] %>% strsplit(" [:|:] ") %>% sapply(function(ROW){flag_name%in%drop_nas(ROW)})))
}
#' @export
combine_two_split_vector_flags <- function(v1,v2,read_split=" [:|:] ",write_split = " | "){
  combined_list <- Map(function(x, y) c(x, y), strsplit(v1,split = read_split), strsplit(v2,split = read_split))
  v3 <- sapply(combined_list,function(E){
    x <- trimws(sort(drop_nas(unique(E))),whitespace = "[\\h\\v]")
    if(length(x)==0)return(NA)
    return(paste0(x,collapse = write_split))
  })
  return(v3)
}
#' @export
clean_split_vec <- function(vec,read_split=" [:|:] ",write_split = " | ",sort_them=T,lowercase_them=T){
  vec %>% trimws(whitespace = "[\\h\\v]") %>% lapply(function(E){
    if(is.na(E))return(NA)
    x <- strsplit(E,split= read_split) %>% unlist() %>% unique() %>% trimws(whitespace = "[\\h\\v]")
    if(sort_them)x <- sort(x)
    if(lowercase_them)x <- tolower(x)
    return(paste0(x,collapse = write_split))
  }) %>% unlist() %>% return()
}
#' @export
assign_vec_in_console <- function(vec,choices,sort_type = "smart"){
  v1 <- vec %>% as.character()
  v2 <- rep(NA,length(v1))
  vec %>% length() %>% message(" elements")
  vec %>% ul() %>% message(" unique elements")
  vec %>% is.na() %>% wl() %>% message(" NA elements")
  if(sort_type=="smart"){
    x <- v1 %>% sort() %>% rle()
    x<- data.frame(
      lengths = x$lengths,
      values = x$values
    )
    vec <- x$values[order(x$lengths,decreasing = T)]
  }
  vec <- vec %>% drop_nas() %>% unique()
  if(sort_type=="alphabetic") vec <- vec %>% sort()
  if(length(vec)==0){return(v2)}
  the_choices <- c("Skip","Stop")
  no_choices <- missing(choices)
  if(no_choices){
    the_choices <- the_choices %>% append("Type Your Answer")
  }else{
    the_choices <- the_choices %>% append(choices)
  }
  keep_going <- T
  i <- 1
  vec_length <- length(vec)
  v1_length <- length(v1)
  while(keep_going){
    rows <- which(v1==vec[i])
    message(i, " of ",vec_length, " (",round(i/vec_length*100,digits = 1),"%) Unique Elements Done After This")
    v2_na <- wl(!is.na(v2))
    message(v2_na, " of ",v1_length, " (",round(v2_na/v1_length*100,digits = 1),"%) Elements not NA now")
    message(length(rows)," elements can change based on next choice")
    the_current_choice <- utils::menu(the_choices,title=paste0("What would you like to do for '",vec[i],"'?"))
    if(the_current_choice==1) the_final_v2_choice <- NA
    if(the_current_choice==2) keep_going <- F
    if(the_current_choice>=3){
      the_final_v2_choice <- the_choices[the_current_choice]
      if(no_choices){
        the_final_v2_choice <- readline("Type Your Choice!")
      }
      if(!is.na(the_final_v2_choice))v2[rows] <- the_final_v2_choice
    }
    i <- i + 1
    if(i>vec_length) keep_going <- F
  }
  return(v2)
}
