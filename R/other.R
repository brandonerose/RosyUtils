#' @title Clear Environment
#' @description clear the global environment
#' @return cleared environment
#' @export
clear_env <- function(){
  pos <- 1
  rm(list=ls(all.names = T,pos = pos),pos = pos)
  cat("\014")
  if(!is.null(grDevices::dev.list())) grDevices::dev.off()
  grDevices::graphics.off()
}
#' @title sample1
#' @export
sample1 <- function(x){
  sample(x,1)
}
#' @title ul
#' @export
ul <- function(x){
  length(unique(x))
}
#' @title wl
#' @export
wl <- function(x){
  length(which(x))
}
#' @title dwl
#' @export
dwl <- function(x){
  length(which(duplicated(x)))
}
#' @title dw
#' @export
dw <- function(x){
  which(duplicated(x))
}
#' @title drop_nas
#' @export
drop_nas <- function(x) {
  x[!sapply(x, is.na)]
}
#' @title drop_if
#' @export
drop_if <- function(x,drops) {
  x[which(!x%in%drops)]
}
#' @title vec_which_duplicated
#' @export
vec_which_duplicated <- function(vec){
  vec[which(duplicated(vec))]
}
#' @title clean_num
#' @export
clean_num <- function(num){
  formatC(num, format="d", big.mark=",")
}
#' @title size
#' @export
size <- function(x){
  format(object.size(x),units = "auto")
}
#' @title read_pdf
#' @export
read_pdf <- function(path,no_double_spaces = F){
  pdf_text_raw <- pdftools::pdf_text(path)
  pdf_text <- pdf_text_raw %>% paste(collapse = " ")
  pdf_text <- gsub("\\n"," ",pdf_text)
  if(no_double_spaces){
    while(grepl("  ",pdf_text)){
      pdf_text <- gsub("  "," ",pdf_text)
    }
  }
  pdf_text <- trimws(pdf_text)
}
#' @title wrap_string_to_lines
#' @export
wrap_string_to_lines <- function(text, max_length,spacer="") {
  result_vector <- c()
  n <- nchar(text)
  start <- 1
  end <- min(start + max_length-1, n)
  chunk <- substr(text, start, end)
  stringr::str_length(chunk)
  result_vector <- c(result_vector, chunk)
  start <- end + 1
  while (start <= n) {
    end <- min(start + (max_length - stringr::str_length(spacer)-1), n)
    chunk <- paste(spacer, substr(text, start, end), sep = "")
    stringr::str_length(chunk)
    result_vector <- c(result_vector, chunk)
    start <- end + 1
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
unique_trimmed_strings <- function(strings,max_length) {
  trim_string <- function(s, max_length) {
    substr(s, 1, max_length)
  }
  trimmed_strings <- sapply(strings, trim_string, max_length = max_length)
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
      new_string <- paste0(stringr::str_trunc(base_string,width = max_length-(counter),side = "right",ellipsis = ""), counter)
      counter <- counter + 1
    }
    unique_strings[i] <- new_string
    counts[i] <- counter
  }
  return(unique_strings)
}
#' @title as_comma_string
#' @export
as_comma_string <- function(vec){
  paste0(vec,collapse = ", ")
}
#' @title choice_vector_string
#' @export
choice_vector_string <- function(vec){
  if(!is_something(vec))return(NA)
  return(paste0(paste0(1:length(vec),", ",vec),collapse = " | "))
}
#' @title matches
#' @export
matches <- function(x,ref,count_only=F){
  final_match <- list()
  final_match[1:length(x)] <- NA
  next_match <- match(x,ref)
  next_match_index <- which(!is.na(next_match))
  while(length(next_match_index)>0){
    final_match[next_match_index] <- next_match_index %>% lapply(function(index){
      if(all(is.na(final_match[[index]]))){
        return(next_match[index])
      }else{
        return(c(final_match[[index]],next_match[index]))
      }
    })
    ref[next_match[which(!is.na(next_match))]] <- NA
    next_match <- match(x,ref)
    next_match_index <- which(!is.na(next_match))
  }
  if(count_only){
    final_match <- final_match %>% sapply(function(IN){
      if(is.na(IN[1]))return(NA)
      return(length(IN))
    })
  }
  return(final_match)
}
