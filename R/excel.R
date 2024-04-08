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
#' @title DF_to_wb
#' @export
DF_to_wb <- function(
    DF,
    DF_name,
    wb = openxlsx::createWorkbook(),
    link_col_list = list(),
    str_trunc_length = 32000,
    header_df,
    tableStyle = "none",
    header_style = default_header_style,
    body_style = default_body_style,
    freeze_header = T,
    pad_rows = 0,
    pad_cols = 0,
    freeze_keys = T,
    key_cols = NULL
) {
  if(nchar(DF_name)>31)stop(DF_name, " is longer than 31 char")
  DF <-  DF %>% lapply(stringr::str_trunc, str_trunc_length, ellipsis = "") %>% as.data.frame()
  hyperlink_col <- NULL

  if (nrow(DF)>0){
    openxlsx::addWorksheet(wb, DF_name)
    startRow_header <-pad_rows + 1
    startRow_table <- startRow_header + 1
    startCol <-pad_cols + 1
    if(missing(header_df))  header_df<- data.frame()
    if(is_something(header_df)){
      openxlsx::writeData(wb, sheet = DF_name, x = header_df,startRow = startRow_header,startCol = startCol,colNames = F)
      startRow_table <- startRow_header + nrow(header_df)
    }
    if(length(link_col_list)>0){
      has_names <- !is.null(names(link_col_list))
      for(i in seq_along(link_col_list)){
        if(link_col_list[[i]]%in%colnames(DF)){
          class (DF[[link_col_list[[i]]]]) <- "hyperlink"
        }else{
          # warning("",immediate. = T)
        }
        if(has_names){
          if(names(link_col_list)[i]%in%colnames(DF)){
            hyperlink_col <- which(colnames(DF)==names(link_col_list)[i])
            openxlsx::writeData(wb, sheet = DF_name, x = DF[[link_col_list[[i]]]],startRow = startRow_table+1,startCol = hyperlink_col + pad_cols)
            DF[[link_col_list[[i]]]] <- NULL
          }else{
            # warning("",immediate. = T)
          }
        }
      }
    }
    openxlsx::writeDataTable(wb, sheet = DF_name, x = DF,startRow = startRow_table,startCol = startCol, tableStyle = tableStyle)
    style_cols <- seq(ncol(DF))+pad_cols

    openxlsx::addStyle(
      wb,
      sheet = DF_name,
      style = header_style,
      rows = seq(from=startRow_header,to=startRow_table),
      cols = style_cols,
      gridExpand = T,
      stack = T
    )
    openxlsx::addStyle(
      wb,
      sheet = DF_name,
      style = body_style,
      rows = seq(nrow(DF))+startRow_table,
      cols = style_cols,
      gridExpand = T,
      stack = T
    )
    if(freeze_header||freeze_keys){
      firstActiveRow <- NULL
      if(freeze_header){
        firstActiveRow <-  startRow_table+1
      }
      firstActiveCol <- NULL
      if(freeze_keys){
        firstActiveCol <- startCol
        freeze_key_rows <- which(colnames(DF)%in%key_cols)
        if(length(freeze_key_rows)>0){
          if (is_consecutive_srt_1(freeze_key_rows)){
            firstActiveCol <- firstActiveCol + freeze_key_rows[length(freeze_key_rows)]
          }else{
            warning("key_cols must be consecutive and start from the left most column.",immediate. = T)
          }
        }
        openxlsx::freezePane(wb, DF_name, firstActiveRow = firstActiveRow, firstActiveCol = firstActiveCol)
      }
    }
    return(wb)
  }
}
#' @title list_to_wb
#' @export
list_to_wb <- function(
    list,
    link_col_list = list(),
    str_trunc_length = 32000,
    header_df_list = list(),
    tableStyle = "none",
    header_style = default_header_style,
    body_style = default_body_style,
    freeze_header = T,
    pad_rows = 0,
    pad_cols = 0,
    freeze_keys = T,
    key_cols_list = NULL
){
  if(missing(header_df_list))  header_df_list<- list()
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
      str_trunc_length = str_trunc_length,
      header_df = header_df_list[[list_names[i]]],
      tableStyle = tableStyle,
      header_style = header_style,
      body_style = body_style,
      freeze_header = freeze_header,
      pad_rows = pad_rows,
      pad_cols = pad_cols,
      freeze_keys = freeze_keys,
      key_cols = key_cols_list[[list_names[i]]]
    )
  }
  return(wb)
}
#' @title list_to_wb
#' @export
list_to_excel <- function(
    list,
    dir,
    file_name = NULL,
    separate = FALSE,
    overwrite = TRUE,
    link_col_list = list(),
    str_trunc_length = 32000,
    header_df_list,
    tableStyle = "none",
    header_style = default_header_style,
    body_style = default_body_style,
    freeze_header = T,
    pad_rows = 0,
    pad_cols = 0,
    freeze_keys = T,
    key_cols_list = NULL
) {
  if(missing(header_df_list))  header_df_list<- list()
  wb <- openxlsx::createWorkbook()
  list <- process_df_list(list)
  list_names <- names(list)
  if(separate){
    for(i in seq_along(list)){
      sub_list <- list[i]
      file_name2 <- names(sub_list)
      if(!is.null(file_name)){
        file_name2 <- paste0(file_name,"_",file_name2)
      }
      save_wb(
        wb = list_to_wb(
          list = sub_list,
          link_col_list = link_col_list,
          str_trunc_length = str_trunc_length,
          header_df_list = header_df_list,
          tableStyle = tableStyle,
          header_style = header_style,
          body_style = body_style,
          freeze_header = freeze_header,
          pad_rows = pad_rows,
          pad_cols = pad_cols,
          freeze_keys = freeze_keys,
          key_cols = key_cols_list[[list_names[i]]]
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
        str_trunc_length = str_trunc_length,
        header_df_list = header_df_list,
        tableStyle = tableStyle,
        header_style = header_style,
        body_style = body_style,
        freeze_header = freeze_header,
        pad_rows = pad_rows,
        pad_cols = pad_cols,
        freeze_keys = freeze_keys,
        key_cols = key_cols_list[[list_names[i]]]
      ),
      dir = dir,
      file_name = file_name,
      overwrite = overwrite
    )
  }
}
#' @export
list_to_csv <- function(list,dir,file_name=NULL,overwrite = TRUE){
  list <- process_df_list(list)
  list_names <- names(list)
  for(i in seq_along(list)){
    sub_list <- list[i]
    file_name2 <- names(sub_list)
    if(!is.null(file_name)){
      file_name2 <- paste0(file_name,"_",file_name2)
    }
    save_csv(
      df = sub_list[[1]],
      dir = dir,
      file_name = file_name2,
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
save_csv <- function(df,dir,file_name,overwrite =TRUE){
  if(!dir.exists(dir))stop("dir doesn't exist")
  path <- file.path(dir,paste0(file_name,".csv"))
  write_it <- T
  if(!overwrite){
    if(file.exists(path)){
      write_it <- F
      message("Already a file at -> ","'",path,"'")
    }
  }
  if(write_it){
    write.csv(
      x = df,
      file = path
    )
    message("Saved at -> ","'",path,"'")
  }
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

default_header_style <-
  openxlsx::createStyle(
    fgFill = "#74DFFF",
    halign = "center",
    valign = "center",
    textDecoration = "Bold",
    fontSize = 14,
    fontColour = "black",
    border = "TopBottomLeftRight",
    # borderColour = "black"
  )

default_body_style <-
  openxlsx::createStyle(
    halign = "left",
    valign = "center",
    # border = "Bottom",
    # fontColour = "black",
    fontSize = 12
  )
