#' @title is_something
#' @export
is_something <- function(thing, row = 0) {
  out <- FALSE
  if (is.function(thing)) {
    return(TRUE)
  }
  if (!is.null(thing)) {
    if (is.data.frame(thing)) {
      if (nrow(thing) > row) {
        out <- TRUE
      }
    } else {
      if (length(thing) > 0) {
        if (is.list(thing)) {
          out <- TRUE
        } else {
          if (length(thing) == 1) {
            if (!is.na(thing)) {
              if (is.character(thing)) {
                if (thing != "") {
                  out <- TRUE
                }
              } else {
                out <- TRUE
              }
            }
          } else {
            out <- TRUE
          }
        }
      }
    }
  }
  return(out)
}
#' @title is_nested_list
#' @export
is_nested_list <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (is.data.frame(x)) {
    return(FALSE)
  }
  OUT <- length(x) == 0
  for (i in seq_along(x)) {
    OUT <- OUT || is_nested_list(x[[i]])
    # print(OUT)
  }
  return(OUT)
}
#' @title is_named_list
#' @export
is_named_list <- function(x, silent = TRUE, recursive = FALSE) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (is.null(names(x))) {
    return(FALSE)
  }
  named_all <- TRUE
  if (recursive) {
    for (n in names(x)) {
      element <- x[[n]]
      if (is.list(element)) {
        named_all <- named_all && is_named_list(element)
        if (!silent && !named_all) message("'", n, "' is not named")
      }
    }
  }
  return(named_all) # Return the result
}
#' @title is_df_list
#' @export
is_df_list <- function(x, strict = FALSE) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (length(x) == 0) {
    return(FALSE)
  }
  if (is_nested_list(x)) {
    return(FALSE)
  }
  out <- unlist(lapply(x, is.data.frame))
  if (strict) {
    return(all(out))
  }
  return(any(out))
}
#' @title is_named_df_list
#' @export
is_named_df_list <- function(x, strict = FALSE) {
  is_named_list(x) && is_df_list(x, strict = strict)
}
#' @title is_env_name
#' @export
is_env_name <- function(env_name, silent = FALSE) {
  result <- tryCatch(
    {
      if (is.null(env_name)) stop("env_name is NULL")
      if (nchar(env_name) == 0) {
        stop("Short name cannot be empty.")
      }
      if (grepl("^\\d", env_name)) {
        stop("Short name cannot start with a number.")
      }
      if (grepl("[^A-Za-z0-9_]", env_name)) {
        stop("Short name can only contain letters, numbers, and underscores.")
      }
      return(TRUE) # Return TRUE if all checks pass
    },
    error = function(e) {
      if (!silent) message(e$message)
      return(FALSE) # Return FALSE if any error occurs
    }
  )
  return(result)
}
#' @title is_web_link
#' @export
is_web_link <- function(link, silent = FALSE, strict = FALSE) {
  result <- tryCatch(
    {
      if (is.null(link)) stop("link is NULL")
      # Check if the link starts with "https://" or "http://"
      if (!grepl("^https?://", link)) {
        stop("Invalid web link. It must start with 'http://' or 'https://'.")
      }
      # Remove trailing slash if present
      link <- gsub("/$", "", link)
      # Check if the link ends with one of the specified web endings
      allowed_endings <- c("edu", "com", "org", "net", "gov", "io", "xyz", "info", "co", "uk")
      searching <- paste0("\\.(", paste0(allowed_endings, collapse = "|"), ")", ifelse(strict, "$", ""))
      if (!grepl(searching, link)) {
        stop("Invalid web link. It must end with a valid web ending (.edu, .com, etc.).")
      }
      return(TRUE) # Return TRUE if all checks pass
    },
    error = function(e) {
      if (!silent) message(e$message)
      return(FALSE) # Return FALSE if any error occurs
    }
  )
  return(result)
}
#' @title is_consecutive_srt_1
#' @export
is_consecutive_srt_1 <- function(vec) {
  if (vec[1] != 1) {
    return(FALSE)
  }
  if (length(vec) > 1) {
    for (i in 2:length(vec)) {
      if (vec[i] != vec[i - 1] + 1) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}
#' @title validate_env_name
#' @export
validate_env_name <- function(env_name) {
  # Check if the name is empty
  if (is.null(env_name)) stop("env_name is NULL")
  if (nchar(env_name) == 0) {
    stop("Short name cannot be empty.")
  }
  # Check if the name starts with a number
  if (grepl("^\\d", env_name)) {
    stop("Short name cannot start with a number.")
  }
  # Check if the name contains any invalid characters
  if (grepl("[^A-Za-z0-9_]", env_name)) {
    stop("Short name can only contain letters, numbers, and underscores.")
  }
  return(env_name)
}
#' @title validate_web_link
#' @export
validate_web_link <- function(link) {
  if (is.null(link)) stop("link is NULL")
  # Check if the link starts with "https://" or "http://"
  if (!grepl("^https?://", link)) {
    stop("Invalid web link. It must start with 'http://' or 'https://'.")
  }
  # Remove trailing slash if present
  link <- gsub("/$", "", link)
  # Check if the link ends with one of the specified web endings
  if (!grepl("\\.(edu|com|org|net|gov|io|xyz|info|co|uk)$", link)) {
    stop("Invalid web link. It must end with a valid web ending (.edu, .com, etc.).")
  }
  # Add a trailing slash
  link <- paste0(link, "/")
  return(link)
}
#' @title generate_hex
#' @export
generate_hex <- function(length = 32) {
  toupper(paste0(sample(c(0:9, letters[1:6]), length, replace = TRUE), collapse = ""))
}
