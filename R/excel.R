#' @title excel_to_list
#' @export
excel_to_list <- function(path) {
  sheets <- readxl::excel_sheets(path)
  clean_sheets <- clean_env_names(sheets)
  out <- list()
  for (i in seq_along(sheets)) {
    out[[i]] <- rio::import(path, col_types = "text", sheet = i)
  }
  names(out) <- clean_sheets
  return(out)
}
#' @title csv_to_list
#' @export
csv_to_list <- function(paths) {
  paths <- normalizePath(paths)
  OUT <- list()
  clean_names <- paths %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    clean_env_names()
  for (i in seq_along(paths)) {
    OUT[[i]] <- read.csv(paths[i], stringsAsFactors = FALSE, na.strings = c("", "NA"))
  }
  names(OUT) <- clean_names
  return(OUT)
}
#' @title csv_folder_to_list
#' @export
csv_folder_to_list <- function(folder) {
  folder <- normalizePath(folder)
  if (!dir.exists(folder)) stop("Folder does not exist: ", folder)
  paths <- list.files.real(folder)
  paths <- paths[which(paths %>% endsWith(".csv"))]
  return(csv_to_list(paths = paths))
}
#' @title wb_to_list
#' @export
wb_to_list <- function(wb) {
  # wb <- openxlsx::loadWorkbook(file = path)
  # test for if user does not have excel
  sheets <- openxlsx::sheets(wb)
  clean_sheets <- clean_env_names(sheets)
  out <- list()
  for (i in seq_along(sheets)) {
    col_row <- 1
    x <- openxlsx::getTables(wb, sheet = i)
    if (length(x) > 0) {
      col_row <- as.integer(gsub("[A-Za-z]", "", unlist(x %>% attr("refs") %>% strsplit(":"))[[1]])) # test for xlsx without letters for cols
    }
    out[[i]] <- openxlsx::read.xlsx(wb, sheet = i, startRow = col_row)
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
    header_df = NULL,
    tableStyle = "none",
    header_style = default_header_style,
    body_style = default_body_style,
    freeze_header = TRUE,
    pad_rows = 0,
    pad_cols = 0,
    freeze_keys = TRUE,
    key_cols = NULL) {
  if (nchar(DF_name) > 31) stop(DF_name, " is longer than 31 char")
  DF <- DF %>%
    lapply(stringr::str_trunc, str_trunc_length, ellipsis = "") %>%
    as.data.frame()
  hyperlink_col <- NULL
  if (freeze_keys) {
    all_cols <- colnames(DF)
    if (any(!key_cols %in% all_cols)) stop("all key_cols must be in the DFs")
    freeze_key_cols <- which(all_cols %in% key_cols)
    if (length(freeze_key_cols) > 0) {
      if (!is_consecutive_srt_1(freeze_key_cols)) {
        warning("please keep your key cols on the left consecutively. Fixing ", DF_name, ": ", paste0(key_cols, collapse = ", "), ".", immediate. = TRUE)
        non_key_cols <- seq_len(ncol(DF))
        non_key_cols <- non_key_cols[which(!non_key_cols %in% freeze_key_cols)]
        new_col_order <- c(freeze_key_cols, non_key_cols)
        if (is_something(header_df)) {
          header_df <- header_df[, new_col_order]
        }
        DF <- DF[, new_col_order]
      }
    }
  }
  if (nrow(DF) > 0) {
    openxlsx::addWorksheet(wb, DF_name)
    startRow_header <- pad_rows + 1
    startRow_table <- startRow_header
    startCol <- pad_cols + 1
    if (is_something(header_df)) {
      openxlsx::writeData(wb, sheet = DF_name, x = header_df, startRow = startRow_header, startCol = startCol, colNames = FALSE)
      startRow_table <- startRow_header + nrow(header_df)
    }
    if (length(link_col_list) > 0) {
      has_names <- !is.null(names(link_col_list))
      for (i in seq_along(link_col_list)) {
        if (link_col_list[[i]] %in% colnames(DF)) {
          class(DF[[link_col_list[[i]]]]) <- "hyperlink"
        } else {
          # warning("",immediate. = TRUE)
        }
        if (has_names) {
          if (names(link_col_list)[i] %in% colnames(DF)) {
            hyperlink_col <- which(colnames(DF) == names(link_col_list)[i])
            openxlsx::writeData(wb, sheet = DF_name, x = DF[[link_col_list[[i]]]], startRow = startRow_table + 1, startCol = hyperlink_col + pad_cols)
            DF[[link_col_list[[i]]]] <- NULL
          } else {
            # warning("",immediate. = TRUE)
          }
        }
      }
    }
    openxlsx::writeDataTable(wb, sheet = DF_name, x = DF, startRow = startRow_table, startCol = startCol, tableStyle = tableStyle)
    style_cols <- seq(ncol(DF)) + pad_cols
    openxlsx::addStyle(
      wb,
      sheet = DF_name,
      style = header_style,
      rows = seq(from = startRow_header, to = startRow_table),
      cols = style_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
    openxlsx::addStyle(
      wb,
      sheet = DF_name,
      style = body_style,
      rows = seq(nrow(DF)) + startRow_table,
      cols = style_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
    if (freeze_header || freeze_keys) {
      firstActiveRow <- NULL
      if (freeze_header) {
        firstActiveRow <- startRow_table + 1
      }
      firstActiveCol <- NULL
      if (freeze_keys) {
        firstActiveCol <- startCol
        freeze_key_cols <- which(colnames(DF) %in% key_cols)
        if (length(freeze_key_cols) > 0) {
          if (is_consecutive_srt_1(freeze_key_cols)) {
            firstActiveCol <- firstActiveCol + freeze_key_cols[length(freeze_key_cols)]
          } else {
            warning("key_cols must be consecutive and start from the left most column.", immediate. = TRUE)
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
    header_df_list = NULL,
    tableStyle = "none",
    header_style = default_header_style,
    body_style = default_body_style,
    freeze_header = TRUE,
    pad_rows = 0,
    pad_cols = 0,
    freeze_keys = TRUE,
    key_cols_list = NULL,
    drop_empty = TRUE) {
  wb <- openxlsx::createWorkbook()
  list <- process_df_list(list, drop_empty = drop_empty)
  list_names <- names(list)
  list_link_names <- list()
  if (length(link_col_list) > 0) {
    if (is_named_list(link_col_list)) {
      if (!all(names(link_col_list) %in% list_names)) {
        for (list_name in list_names) {
          list_link_names[[list_name]] <- link_col_list
        }
      }
    }
  }
  list_names_rename <- stringr::str_trunc(list_names, width = 31, side = "right", ellipsis = "")
  BAD <- dw(list_names_rename)
  if (length(BAD) > 0) {
    warning("Duplicated names when trimmed from right 31 max in Excel: ", list_names[BAD] %>% paste0(collapse = ", "), immediate. = TRUE)
    message("Use CSV or shorten the names and make sure they are unique if they are trimmed to 31 char. For now will make unique by adding number.")
    list_names_rename <- unique_trimmed_strings(list_names_rename, max_length = 31)
  }
  for (i in seq_along(list_names)) {
    header_df <- header_df_list[[list_names[i]]]
    key_cols <- key_cols_list[[list_names[i]]]
    wb <- DF_to_wb(
      DF = list[[list_names[i]]],
      DF_name = list_names_rename[i],
      wb = wb,
      link_col_list = list_link_names[[list_names[i]]],
      str_trunc_length = str_trunc_length,
      header_df = header_df,
      tableStyle = tableStyle,
      header_style = header_style,
      body_style = body_style,
      freeze_header = freeze_header,
      pad_rows = pad_rows,
      pad_cols = pad_cols,
      freeze_keys = freeze_keys,
      key_cols = key_cols
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
    header_df_list = NULL,
    tableStyle = "none",
    header_style = default_header_style,
    body_style = default_body_style,
    freeze_header = TRUE,
    pad_rows = 0,
    pad_cols = 0,
    freeze_keys = TRUE,
    key_cols_list = NULL,
    drop_empty = TRUE) {
  wb <- openxlsx::createWorkbook()
  list <- process_df_list(list, drop_empty = drop_empty)
  list_names <- names(list)
  if (length(list) == 0) {
    return(warning("empty list cannot be saved", immediate. = TRUE))
  }
  if (separate) {
    for (i in seq_along(list)) {
      sub_list <- list[i]
      file_name2 <- names(sub_list)
      if (!is.null(file_name)) {
        file_name2 <- paste0(file_name, "_", file_name2)
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
          key_cols_list = key_cols_list[[list_names[i]]],
          drop_empty = drop_empty
        ),
        dir = dir,
        file_name = file_name2,
        overwrite = overwrite
      )
    }
  } else {
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
        key_cols_list = key_cols_list,
        drop_empty = drop_empty
      ),
      dir = dir,
      file_name = file_name,
      overwrite = overwrite
    )
  }
}
#' @export
list_to_csv <- function(list, dir, file_name = NULL, overwrite = TRUE, drop_empty = TRUE) {
  list <- process_df_list(list, drop_empty = drop_empty)
  list_names <- names(list)
  for (i in seq_along(list)) {
    sub_list <- list[i]
    file_name2 <- names(sub_list)
    if (!is.null(file_name)) {
      file_name2 <- paste0(file_name, "_", file_name2)
    }
    save_csv(
      DF = sub_list[[1]],
      dir = dir,
      file_name = file_name2,
      overwrite = overwrite
    )
  }
}
#' @title save_wb
#' @export
save_wb <- function(wb, dir, file_name, overwrite = TRUE) {
  if (!dir.exists(dir)) stop("dir doesn't exist")
  path <- file.path(dir, paste0(file_name, ".xlsx"))
  openxlsx::saveWorkbook(
    wb = wb,
    file = path,
    overwrite = overwrite
  )
  bullet_in_console(paste0("Saved '", basename(path), "'!"), file = path)
}
save_csv <- function(DF, dir, file_name, overwrite = TRUE) {
  if (!dir.exists(dir)) stop("dir doesn't exist")
  path <- file.path(dir, paste0(file_name, ".csv"))
  write_it <- TRUE
  if (!overwrite) {
    if (file.exists(path)) {
      write_it <- FALSE
      bullet_in_console(paste0("Already a file!"), file = path)
    }
  }
  if (write_it) {
    write.csv(
      x = DF,
      file = path
    )
    bullet_in_console(paste0("Saved '", basename(path), "'!"), file = path)
  }
}
#' @title process_df_list
#' @export
process_df_list <- function(list, drop_empty = TRUE, silent = FALSE) {
  if (is_something(list)) {
    if (!is_df_list(list)) stop("list must be ...... a list :)")
    if (drop_empty) {
      is_a_df_with_rows <- list %>%
        lapply(function(x) {
          is_df <- is.data.frame(x)
          out <- FALSE
          if (is_df) {
            out <- nrow(x) > 0
          }
          out
        }) %>%
        unlist()
      keeps <- which(is_a_df_with_rows)
      drops <- which(!is_a_df_with_rows)
      if (length(drops) > 0) {
        if (!silent) {
          cli_alert_wrap(
            "Dropping non-data.frames and empties... ",
            toString(names(drops))
          )
        }
      }
      list <- list[keeps]
    }
    if (length(list) > 0) {
      if (!is_named_df_list(list)) {
        names(list) <- paste0(seq_along(list))
      }
    }
  }
  list
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
