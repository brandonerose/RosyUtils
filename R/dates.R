#' @export
age <- function(dob, age.day = lubridate::today(), units = "years", floor = TRUE) {
  calc.age <- lubridate::interval(dob, age.day) / lubridate::duration(num = 1, units = units)
  if (floor) {
    return(as.integer(floor(calc.age)))
  }
  return(calc.age)
}
#' @export
is_date <- function(date) {
  OUT <- grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d{4}-\\d{2}$|^\\d{4}$", date)
  if (OUT) {
    OUT2 <- date %>%
      strsplit(split = "-") %>%
      unlist()
    year <- OUT2[[1]]
    check_date <- year
    if (length(OUT2) == 1) {
      check_date <- check_date %>% paste0("-01")
      OUT2[[2]] <- "01"
    }
    if (length(OUT2) == 2) {
      check_date <- check_date %>% paste0("-01")
      OUT2[[3]] <- "01"
    }
    year <- year %>% as.integer()
    month <- OUT2[[2]] %>% as.integer()
    day <- OUT2[[3]] %>% as.integer()
    this_year <-
      OUT <- month >= 1 && month <= 12 && day >= 1 && day <= 31 && year >= 1900 && year <= lubridate::year(Sys.Date())
  }
  OUT
}
#' @export
is_date_full <- function(date) {
  grepl("^\\d{4}-\\d{2}-\\d{2}$", date)
}
#' @export
extract_dates <- function(input_string, allow_partial = TRUE) {
  # Regular expression pattern to match different date formats
  date_patterns <- c(
    "\\b(0?[1-9]|1[0-2])/(0?[1-9]|[12][0-9]|3[01])/(\\d{2})\\b", # MM/DD/YY
    "\\b(0?[1-9]|1[0-2])/(0?[1-9]|[12][0-9]|3[01])/(19[0-9]{2}|20[0-9]{2})\\b", # MM/DD/YYYY
    "\\b(0?[1-9]|1[0-2])-(0?[1-9]|[12][0-9]|3[01])-(19[0-9]{2}|20[0-9]{2})\\b", # MM-DD-YYYY
    "\\b(0?[1-9]|1[0-2])-(0?[1-9]|[12][0-9]|3[01])-(\\d{2})\\b", # MM-DD-YY is this weird?
    "\\b(0?[1-9]|1[0-2])-(0?[1-9]|[12][0-9]|3[01])-(19[0-9]{2}|20[0-9]{2})\\b", # MM-DD-YYYY
    "\\b(19[0-9]{2}|20[0-9]{2})-(0?[1-9]|1[0-2])-(0?[1-9]|[12][0-9]|3[01])\\b" # YYYY-MM-DD
  )
  if (allow_partial) {
    date_patterns <- date_patterns %>% append(
      c(
        "\\b(19[0-9]{2}|20[0-9]{2})-(0?[1-9]|1[0-2])(?!-[0-9])\\b", # YYYY-MM
        "\\b(0?[1-9]|1[0-2])/(19[0-9]{2}|20[0-9]{2})\\b" # MM/YYYY
      )
    )
  }
  # Initialize an empty list to store matches
  matched_dates <- list()
  # Extract date matches using str_extract_all for each pattern
  for (pattern in date_patterns) {
    matches <- stringr::str_extract_all(input_string, pattern)
    matched_dates <- matched_dates %>% append(matches)
  }
  return(unlist(matched_dates))
}
#' @export
extract_dates2 <- function(input_string) {
  # Regular expression pattern to match different date formats
  date_patterns <- c(
    "\\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])"
  ) # "\\d{4}-(0?[1-9]|1[0-2])-(0?[1-9]|[12][0-9]|3[01])"
  # Initialize an empty list to store matches
  matched_dates <- list()
  # Extract date matches using str_extract_all for each pattern
  for (pattern in date_patterns) {
    matches <- stringr::str_extract_all(input_string, pattern)
    matched_dates <- matched_dates %>% append(matches)
  }
  if (length(matched_dates[[1]]) == 0) {
    return(NA)
  }
  return(unlist(matched_dates))
}
#' @export
delete_dates2 <- function(input_string) {
  # Extract valid dates and remove the rest of the text
  date_patterns <- c(
    "\\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])"
  )
  gsub(paste(date_patterns, collapse = "|"), "", input_string)
}
#' @export
convert_dates <- function(input_string, allow_partial = FALSE) {
  if (!is.na(input_string)) {
    input_string <- input_string %>% trimws()
    if (input_string != "") {
      dates <- extract_dates(input_string, allow_partial = allow_partial)
      output_string <- input_string
      for (the_date in dates) {
        the_date_final <- gsub("-", "/", the_date)
        split_pattern <- the_date_final %>%
          strsplit("/") %>%
          unlist() %>%
          lapply(function(E) {
            E %>%
              as.integer() %>%
              stringr::str_pad(2, "left", 0)
          }) %>%
          unlist()
        y_n <- 3
        m_n <- 1
        d_n <- 2
        check_year <- which(nchar(split_pattern) == 4)
        if (length(check_year) == 1) {
          if (check_year == 1) {
            y_n <- 1
            m_n <- 2
            d_n <- 3
          }
        }
        year <- split_pattern[[y_n]]
        if (nchar(year) == 2) {
          year <- year %>% stringr::str_pad(2, "left", 0)
          if (year >= 0 && year < 25) {
            year <- paste0("20", year)
          }
          if (year >= 50 && year <= 99) {
            year <- paste0("19", year)
          }
        }
        the_date_final <- paste0(year)
        if (length(split_pattern) > 1) {
          month <- split_pattern[[m_n]]
          the_date_final <- the_date_final %>% paste0("-", month)
        }
        if (length(split_pattern) > 2) {
          day <- split_pattern[[d_n]]
          the_date_final <- the_date_final %>% paste0("-", day)
        }
        output_string <- gsub(the_date, the_date_final, output_string)
      }
      return(output_string)
    }
  }
}
#' @export
date_imputation <- function(dates_in, date_imputation) {
  # followup add min max
  z <- lapply(dates_in, is_date) %>% as.logical()
  x <- which(z & !is_date_full(dates_in))
  y <- which(!z)
  date_out <- dates_in
  if (length(y) > 0) {
    date_out[y] <- NA
  }
  if (length(x) > 0) {
    if (missing(date_imputation)) date_imputation <- NULL
    if (is.null(date_imputation)) {
      date_out[x] <- NA
    }
    if (!is.null(date_imputation)) {
      date_out[x] <- admiral::impute_dtc_dt(
        dates_in[x],
        highest_imputation = "M",
        date_imputation = date_imputation
      )
    }
  }
  date_out
}
