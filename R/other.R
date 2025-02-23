#' @title Clear Environment
#' @description clear the global environment
#' @return cleared environment
#' @export
clear_env <- function() {
  pos <- 1
  rm(list = ls(all.names = TRUE, pos = pos), pos = pos)
  cat("\014")
  if (!is.null(grDevices::dev.list())) grDevices::dev.off()
  grDevices::graphics.off()
}
#' @title sample1
#' @export
sample1 <- function(x) {
  sample(x, 1)
}
#' @title ul
#' @export
ul <- function(x) {
  length(unique(x))
}
#' @title wl
#' @export
wl <- function(x) {
  length(which(x))
}
#' @title dwl
#' @export
dwl <- function(x) {
  length(which(duplicated(x)))
}
#' @title dw
#' @export
dw <- function(x) {
  which(duplicated(x))
}
#' @title drop_nas
#' @export
drop_nas <- function(x) {
  x[!unlist(lapply(x, is.na))]
}
#' @title drop_if
#' @export
drop_if <- function(x, drops) {
  x[which(!x %in% drops)]
}
#' @title vec_which_duplicated
#' @export
vec_which_duplicated <- function(vec) {
  vec[which(duplicated(vec))]
}
#' @title clean_num
#' @export
clean_num <- function(num) {
  formatC(num, format = "d", big.mark = ",")
}
#' @title size
#' @export
size <- function(x) {
  format(object.size(x), units = "auto")
}
#' @title file_size_mb
#' @export
file_size_mb <- function(path) {
  (file.info(path)[["size"]] / 1048576) %>%
    round(1) %>%
    paste0(" Mb")
}
#' @title read_pdf
#' @export
read_pdf <- function(path, no_double_spaces = FALSE) {
  pdf_text_raw <- pdftools::pdf_text(path)
  pdf_text <- pdf_text_raw %>% paste(collapse = " ")
  pdf_text <- gsub("\\n", " ", pdf_text)
  if (no_double_spaces) {
    while (grepl("  ", pdf_text)) {
      pdf_text <- gsub("  ", " ", pdf_text)
    }
  }
  pdf_text <- trimws(pdf_text)
}
#' @title wrap_string_to_lines
#' @export
wrap_string_to_lines <- function(text, max_length, spacer = "") {
  result_vector <- c()
  n <- nchar(text)
  start <- 1
  end <- min(start + max_length - 1, n)
  chunk <- substr(text, start, end)
  stringr::str_length(chunk)
  result_vector <- c(result_vector, chunk)
  start <- end + 1
  while (start <= n) {
    end <- min(start + (max_length - stringr::str_length(spacer) - 1), n)
    chunk <- paste(spacer, substr(text, start, end), sep = "")
    stringr::str_length(chunk)
    result_vector <- c(result_vector, chunk)
    start <- end + 1
  }
  return(result_vector)
}
#' @title wrap_string_to_lines2
#' @export
wrap_string_to_lines2 <- function(text, max_length = 80, spacer = "") {
  final_end <- end <- nchar(text)
  if (end <= max_length) {
    return(text)
  }
  protected_pattern <- '"[^"]*"|\\[[^\\]]*\\]|\\{[^}]*\\}'
  masked_text <- text
  matches <- stringr::str_extract_all(text, protected_pattern)[[1]]
  for (match in matches) {
    masked_text <- sub(stringr::fixed(match), strrep("_", nchar(match)), masked_text, fixed = TRUE)
  }
  space_positions <- as.data.frame(stringr::str_locate_all(masked_text, " ")[[1]])$end
  if (length(space_positions) == 0) {
    bullet_in_console(paste0("No unprotected spaces to wrap by for: ", text), bullet_type = "!")
    return(text)
  }
  spacer_length <- nchar(spacer)
  space_positions <- space_positions - 1
  result_vector <- c()
  final_start <- start <- 1
  go <- TRUE
  while (go) {
    is_start <- final_start == 1
    final_max_length <- max_length
    if (!is_start) {
      final_max_length <- (max_length - spacer_length)
    }
    short_enough <- (end - final_start) <= final_max_length
    if (short_enough) {
      chunk <- substr(text, final_start, end)
      if (!is_start) {
        chunk <- paste(spacer, chunk, sep = "")
      }
      result_vector <- c(result_vector, chunk)
      go <- FALSE
    }
    if (go) {
      available_spaces <- space_positions[which(
        space_positions <= (final_max_length + final_start) &
          space_positions >= final_start
      )]
      if (length(available_spaces) > 0) {
        final_end <- max(available_spaces)
      } else {
        final_end <- end
      }
      chunk <- substr(text, final_start, final_end)
      if (!is_start) {
        chunk <- paste(spacer, chunk, sep = "")
      }
      result_vector <- c(result_vector, chunk)
      final_start <- final_end + 2
      if (final_start >= end) {
        go <- FALSE
      }
    }
  }
  return(result_vector)
}
#' @title wrap_text
#' @export
wrap_text <- function(text, max_length = 40, spacer = "\n") {
  words <- unlist(strsplit(text, " "))
  current_line <- ""
  result <- ""
  for (word in words) {
    if (nchar(current_line) + nchar(word) + 1 > max_length) {
      result <- paste0(result, current_line, spacer)
      current_line <- word
    } else {
      if (nchar(current_line) == 0) {
        current_line <- word
      } else {
        current_line <- paste0(current_line, " ", word)
      }
    }
  }
  result <- paste0(result, current_line)
  return(result)
}
#' @title unique_trimmed_strings
#' @export
unique_trimmed_strings <- function(strings, max_length) {
  trim_string <- function(s, max_length) {
    substr(s, 1, max_length)
  }
  trimmed_strings <- unlist(lapply(strings, trim_string, max_length = max_length))
  # Initialize a vector to store unique strings
  unique_strings <- character(length(trimmed_strings))
  # Initialize a counter to keep track of occurrences
  counts <- integer(length(trimmed_strings))
  for (i in seq_along(trimmed_strings)) {
    base_string <- trimmed_strings[i]
    new_string <- base_string
    counter <- 1
    # Keep adjusting the string until it's unique
    while (new_string %in% unique_strings) {
      new_string <- paste0(stringr::str_trunc(base_string, width = max_length - (counter), side = "right", ellipsis = ""), counter)
      counter <- counter + 1
    }
    unique_strings[i] <- new_string
    counts[i] <- counter
  }
  return(unique_strings)
}
#' @title as_comma_string
#' @export
as_comma_string <- function(vec) {
  paste0(vec, collapse = ", ")
}
#' @title choice_vector_string
#' @export
choice_vector_string <- function(vec) {
  if (!is_something(vec)) {
    return(NA)
  }
  return(paste0(paste0(seq_along(vec), ", ", vec), collapse = " | "))
}
#' @title matches
#' @export
matches <- function(x, ref, count_only = FALSE) {
  final_match <- list()
  final_match[seq_along(x)] <- NA
  next_match <- match(x, ref)
  next_match_index <- which(!is.na(next_match))
  while (length(next_match_index) > 0) {
    final_match[next_match_index] <- next_match_index %>% lapply(function(index) {
      if (all(is.na(final_match[[index]]))) {
        return(next_match[index])
      } else {
        return(c(final_match[[index]], next_match[index]))
      }
    })
    ref[next_match[which(!is.na(next_match))]] <- NA
    next_match <- match(x, ref)
    next_match_index <- which(!is.na(next_match))
  }
  if (count_only) {
    final_match <- final_match %>%
      lapply(function(IN) {
        if (is.na(IN[1])) {
          return(NA)
        }
        return(length(IN))
      }) %>%
      unlist()
  }
  return(final_match)
}
#' @title numeric_to_cats
#' @export
numeric_to_cats <- function(vec, method = "quantile", quantiles = 5, more_descriptive_label = FALSE) {
  if (method == "sd") {
    med <- median(vec)
    sd_val <- sd(vec)
    # Define the actual value ranges rounded to 1 decimal place
    low_threshold <- round(med - 2 * sd_val, 1)
    mid_low_threshold <- round(med - 1 * sd_val, 1)
    mid_high_threshold <- round(med + 1 * sd_val, 1)
    high_threshold <- round(med + 2 * sd_val, 1)
    # Define the labels with both SD and actual values
    labels <- c(
      "Very Low",
      "Low",
      "Moderate",
      "High",
      "Very High"
    )
    if (more_descriptive_label) {
      labels <- c(
        paste0("Very Low (≤", low_threshold, ", median - 2 SD)"),
        paste0("Low (", low_threshold, "to", mid_low_threshold, ", median - 2 to - 1 SD)"),
        paste0("Middle (", mid_low_threshold, "to", mid_high_threshold, ", median ± 1 SD)"),
        paste0("High (", mid_high_threshold, "to", high_threshold, ", median + 1 to 2 SD)"),
        paste0("Very High (>", high_threshold, ", median + 2 SD)")
      )
      # labels <- c(
      #   paste0("Very Low (≤", low_threshold, ", median - 1.5 SD)"),
      #   paste0("Low (", low_threshold, "to", mid_low_threshold, ", median - 1.5 to -0.5 SD)"),
      #   paste0("Middle (", mid_low_threshold, "to", mid_high_threshold, ", median ± 0.5 SD)"),
      #   paste0("High (", mid_high_threshold, "to", high_threshold, ", median + 0.5 to 1.5 SD)"),
      #   paste0("Very High (>", high_threshold, ", median + 1.5 SD)")
      # )
    }
    # Create a factor column with 5 categories and descriptive labels
    data_category <- cut(
      vec,
      breaks = c(-Inf, low_threshold, mid_low_threshold, mid_high_threshold, high_threshold, Inf),
      labels = labels,
      right = TRUE
    )
  } else if (method == "sd_mod") {
    # Standard deviation-based binning
    med <- median(vec)
    sd_val <- sd(vec)
    # Define the actual value ranges rounded to 1 decimal place
    low_threshold <- round(med - 1.5 * sd_val, 1)
    mid_low_threshold <- round(med - 0.5 * sd_val, 1)
    mid_high_threshold <- round(med + 0.5 * sd_val, 1)
    high_threshold <- round(med + 1.5 * sd_val, 1)
    # Define the labels with both SD and actual values
    labels <- c("Very Low", "Low", "Moderate", "High", "Very High")
    if (more_descriptive_label) {
      labels <- c(
        paste0("Very Low (≤", low_threshold, ", median - 1.5 SD)"),
        paste0("Low (", low_threshold, " to ", mid_low_threshold, ", median - 1.5 to -0.5 SD)"),
        paste0("Moderate (", mid_low_threshold, " to ", mid_high_threshold, ", median ± 0.5 SD)"),
        paste0("High (", mid_high_threshold, " to ", high_threshold, ", median + 0.5 to 1.5 SD)"),
        paste0("Very High (>", high_threshold, ", median + 1.5 SD)")
      )
    }
    # Create SD-based binning
    data_category <- cut(
      vec,
      breaks = c(-Inf, low_threshold, mid_low_threshold, mid_high_threshold, high_threshold, Inf),
      labels = labels,
      right = TRUE
    )
  } else if (method == "quantile") {
    # Quantile-based binning
    quantile_cutoffs <- quantile(vec, probs = seq(0, 1, length.out = quantiles + 1), na.rm = TRUE)
    # Generate labels for the specified number of quantiles
    labels <- paste("Q", 1:quantiles, sep = "")
    if (more_descriptive_label) {
      labels <- paste0(
        "Quantile ", 1:quantiles, " (",
        round(quantile_cutoffs[-length(quantile_cutoffs)], 1), " to ",
        round(quantile_cutoffs[-1], 1), ")"
      )
    }
    # Create quantile-based binning
    data_category <- cut(
      vec,
      breaks = quantile_cutoffs,
      labels = labels,
      include.lowest = TRUE
    )
  } else {
    stop("Invalid method. Choose either 'sd' or 'quantile'.")
  }
  data_category <- factor(data_category, levels = levels(data_category), ordered = TRUE)
  return(data_category)
}
#' @title sanitize_path
#' @export
sanitize_path <- function(path) {
  sanitized <- gsub("\\\\", "/", path)
  sanitized <- normalizePath(sanitized, winslash = "/", mustWork = FALSE)
  return(sanitized)
}
