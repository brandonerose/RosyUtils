#' @title excel_to_list
#' @export
excel_to_list <- function(path){
  sheets <- readxl::excel_sheets(path)
  clean_sheets <- clean_env_names(sheets)
  out <- list()
  for (i in 1:length(sheets)){
    out[[i]]<- rio::import(path,col_types="text",sheet = i)
  }
  names(out) <- clean_sheets
  return(out)
}

#' @title list_to_excel
#' @export
list_to_excel <- function(list,dir,append_name,combine = T){
  if(!dir.exists(dir))stop("dir doesn't exist")
 list_names <- names(list)
  if(!missing(append_name)){

  }
}
