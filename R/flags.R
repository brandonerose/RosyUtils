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
      IN<-IN[which(IN!=flag_name)] #remove flag_name
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
      IN<-IN[which(IN!=flag_name)] #remove flag_name
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
    x<-sort(drop_nas(unique(E)))
    if(length(x)==0)return(NA)
    return(paste0(x,collapse = write_split))
  })
}
