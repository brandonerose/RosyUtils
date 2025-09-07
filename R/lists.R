#' @title add_to_global
#' @param only_dfs logical for only including data.frames in output
#' @return environment adds
#' @export
add_to_global <- function(...,only_dfs = FALSE){
  x <- list(...)
  add_list_to_global(x,only_dfs)
}
#' @title add_list_to_global
#' @param x a named list
#' @param only_dfs logical for only including data.frames in output
#' @return environment adds
#' @export
add_list_to_global <- function(x, only_dfs = FALSE) {
  if (!is.list(x)) stop("x must be a list")
  if (length(x) == 0) stop("length(x) has to be at least 1")
  if (class(x) == "character") {
    if (is.null(names(x))) stop("if x is a character vector it must have names")
    x <- as.list(x)
  }
  if (class(x) != "list") stop("x must be a named list or named character vector")
  if (is.null(names(x))) stop("x list must have names")
  if (only_dfs) x <- x[unlist(lapply(x, is.data.frame))]
  list2env(x, envir = .GlobalEnv)
}
