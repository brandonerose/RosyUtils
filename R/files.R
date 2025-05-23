#' @title full.file.info
#' @param path a file path
#' @param showWarnings logical for showing warnings
#' @return file_info data.frame
#' @export
full.file.info <- function(path, showWarnings = TRUE) {
  if (showWarnings) {
    if (!file.exists(path)) warning("path does not exist: ", path, immediate. = TRUE)
  }
  file_info <- data.frame(
    file = list.files(path),
    path = list.files(path, full.names = TRUE)
  )
  file_info <- cbind(
    file_info,
    file.info(file_info$path)
  )
  rownames(file_info) <- NULL
  return(file_info)
}
#' @title sync_dir
#' @param from a file path for from
#' @param to a file path for to
#' @param top_level logical for being at top level of recursive function
#' @return message
#' @export
sync_dir <- function(from, to, top_level = TRUE) {
  if (top_level) {
    if (!file.exists(from)) stop("from path '", from, "' doesn't exist")
    if (!file.info(from)[["isdir"]]) stop("from path '", from, "' must be a folder")
    if (!file.exists(to)) {
      dir.create(to, showWarnings = FALSE)
    } # stop("to path '",to, "' doesn't exist")
    if (!file.info(to)[["isdir"]]) stop("to path '", to, "' must be a folder")
  }
  file_info_from <- full.file.info(from)
  file_info_to <- full.file.info(to, showWarnings = FALSE)
  if (nrow(file_info_from) > 0) {
    for (i in seq_len(nrow(file_info_from))) {
      file_from <- file_info_from$file[i]
      isdir_from <- file_info_from$isdir[i]
      path_from <- file_info_from$path[i]
      mtime_from <- file_info_from$mtime[i]
      COPY_TF <- TRUE
      add_or_update <- "Adding"
      MATCHING_FILE_ROW <- which(file_info_to$file == file_from)
      if (length(MATCHING_FILE_ROW) > 0) {
        if (length(MATCHING_FILE_ROW) > 1) {
          stop("Strange case of from and to file names seen more than once")
        }
        isdir_to <- file_info_to$isdir[MATCHING_FILE_ROW]
        if (isdir_to != isdir_from) {
          stop("Strange case of from and to paths not being both file-file or folder-folder")
        }
        add_or_update <- "Updating"
        file_to <- file_info_to$file[MATCHING_FILE_ROW]
        path_to <- file_info_to$path[MATCHING_FILE_ROW]
        mtime_to <- file_info_to$mtime[MATCHING_FILE_ROW]
        if (isdir_from) {
          COPY_TF <- FALSE # no need to copy folders that exist
        }
        if (!isdir_from) { # if it's a file... check mtimes
          COPY_TF <- mtime_from > mtime_to
        }
      }
      if (COPY_TF) {
        file.copy(
          from = path_from,
          to = to,
          overwrite = TRUE,
          recursive = TRUE
        )
        message(add_or_update, " file: ", file_from, " to '", to, "'")
      }
      if (!COPY_TF && isdir_from) {
        sync_dir( # recursive dive down if it's a folder
          from = path_from,
          to = path_to,
          top_level = FALSE
        )
      }
    }
  } else {
    warning(from, " is empty!", immediate. = TRUE)
  }
  if (top_level) {
    message("Up to date!")
  }
}
#' @title list.files.real
#' @export
list.files.real <- function(path, full.names = TRUE, recursive = FALSE) {
  grep("~$", normalizePath(list.files(path, full.names = full.names, recursive = recursive)), fixed = TRUE, value = TRUE, invert = TRUE)
}
#' @title view_file
#' @description view_file
#' @param browser logical for launching from your PC default, not RStudio
#' @return file opens
#' @export
view_file <- function(path, browser = FALSE) {
  its_there <- file.exists(path)
  if (!its_there) {
    return(message("No file there: ", path))
  }
  if (its_there) {
    its_dir <- dir.exists(path)
    if (browser) browseURL(url = path)
    if (!browser) {
      if (its_dir) rstudioapi::filesPaneNavigate(path = path)
      if (!its_dir) rstudioapi::navigateToFile(file = path)
    }
  }
}
#' @title delete_file
#' @description delete_file
#' @param path a file path
#' @param silent logical for silencing messages
#' @return file deleted and maybe a message
#' @export
delete_file <- function(path, silent = FALSE) {
  its_there <- file.exists(path)
  if (!its_there) {
    if (!silent) {
      return(message("No file to delete: ", path))
    }
    return()
  }
  if (its_there) {
    deleted <- file.remove(path)
    if (deleted && !silent) {
      return(message("File was deleted: ", path))
    }
  }
}
#' @title replace_word_file
#' @export
replace_word_file <- function(file, pattern, replace) {
  suppressWarnings(tx <- readLines(file))
  tx2 <- gsub(pattern = pattern, replacement = replace, x = tx)
  writeLines(tx2, con = file)
}
#' @title file_tree
#' @export
file_tree <- function(path = ".", prefix = "") {
  # List files and directories in the current path
  entries <- list.files(path, full.names = TRUE, recursive = FALSE, include.dirs = TRUE)
  n <- length(entries)
  # Initialize result
  result <- character()
  for (i in seq_along(entries)) {
    entry <- entries[i]
    is_last <- (i == n)
    entry_name <- basename(entry)
    # Add the entry with appropriate prefix
    result <- c(result, paste0(prefix, if (is_last) "└── " else "├── ", entry_name))
    # If entry is a directory, recurse and add subdirectory contents
    if (file.info(entry)$isdir) {
      sub_prefix <- paste0(prefix, if (is_last) "    " else "│   ")
      result <- c(result, file_tree(entry, sub_prefix))
    }
  }
  return(vec_cat(result))
}
