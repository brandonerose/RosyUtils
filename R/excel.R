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

#' @title list_to_wb
#' @export
list_to_wb <- function(list,link_col_list=list(),str_trunc_length=32000){
  wb <- openxlsx::createWorkbook()
  list <- process_df_list(list)
  list_names <- names(list)
  list_link_names <- list()
  if(length(link_col_list)>0){
    if(is_named_list(link_col_list)){
      if(!all(names(link_col_list)%in%list_names)){
        for(list_name in list_names){
          list_link_names[[list_name]] <- link_col_list
        }
      }
    }
  }
  list_names_rename <- stringr::str_trunc(list_names,width = 31,side = "right",ellipsis = "")
  BAD<-dw(list_names_rename)
  if(length(BAD)>0)stop("Duplicated names when trimmed: ",list_names[BAD] %>% paste0(collapse = ", "))
  for(i in seq_along(list_names)){
    wb <- DF_to_wb(
      DF = list[[list_names[i]]],
      DF_name = list_names_rename[i],
      wb = wb,
      link_col_list = list_link_names[[list_names[i]]],
      str_trunc_length = str_trunc_length
    )
  }

  return(wb)
}
#' @title DF_to_wb
#' @export
DF_to_wb <- function(DF,DF_name,wb = openxlsx::createWorkbook(),link_col_list=list(),str_trunc_length=32000){
  if(nchar(DF_name)>31)stop(DF_name, " is longer than 31 char")
  DF <-  DF %>% lapply(stringr::str_trunc, str_trunc_length, ellipsis = "") %>% as.data.frame()
  if (nrow(DF)>0){
    openxlsx::addWorksheet(wb, DF_name)
    if(length(link_col_list)>0){
      for(link_col in link_col_list){
        class (DF[[link_col]]) <- "hyperlink"
      }
      if(!is.null(names(link_col_list))){
        for(i in seq_along(link_col_list)){
          COL <- which(colnames(DF)==names(link_col_list)[i])
          openxlsx::writeData(wb, sheet = DF_name, x = DF[[link_col_list[[i]]]],startRow = 2,startCol = COL)
          DF[[link_col_list[[i]]]] <- NULL
        }
      }
    }
    openxlsx::writeData(wb, sheet = DF_name, x = DF)
    return(wb)
  }
}
#' @title list_to_wb
#' @export
list_to_excel <- function(list,dir,file_name=NULL,separate = FALSE,overwrite =TRUE,link_col_list=list(),str_trunc_length=32000){
  wb <- openxlsx::createWorkbook()
  list <- process_df_list(list)
  list_names <- names(list)
  if(separate){
    for(i in seq_along(list)){
      sub_list <- list[i]
      sheet <- names(sub_list)
      file_name2 <- sheet
      if(!is.null(file_name)){
        file_name2 <- paste0(file_name,"_",file_name2)
      }
      save_wb(
        wb = list_to_wb(
          list = sub_list,
          link_col_list = link_col_list,
          str_trunc_length = str_trunc_length
        ),
        dir = dir,
        file_name = file_name2,
        overwrite = overwrite
      )
    }
  }else{
    save_wb(
      wb = list_to_wb(
        list = list,
        link_col_list = link_col_list,
        str_trunc_length = str_trunc_length
      ),
      dir = dir,
      file_name = file_name,
      overwrite = overwrite
    )
  }
}

#' @title save_wb
#' @export
save_wb <- function(wb,dir,file_name,overwrite =TRUE){
  if(!dir.exists(dir))stop("dir doesn't exist")
  path <- file.path(dir,paste0(file_name,".xlsx"))
  openxlsx::saveWorkbook(
    wb = wb,
    file = path,
    overwrite = overwrite
  )
  message("Saved at -> ","'",path,"'")
}
process_df_list <- function(list){
  if(!is_df_list(list))stop("list must be ...... a list :)")
  is_a_df_tf <- list %>% sapply(function(IN){is.data.frame(IN)})
  keeps <- which(is_a_df_tf)
  drops <-which(!is_a_df_tf)
  if(length(drops)>0){
    message("Dropping non-data.frames... ", paste0(names(drops),collapse = ", "))
  }
  list <- list[keeps]
  if(!is_named_df_list(list)){
    names(list) <- paste0("sheet",seq_along(list))
  }
  return(list)
}

