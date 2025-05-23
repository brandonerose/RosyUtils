#' @title warn_or_stop
#' @param m message character string
#' @param warn_only logical for only warn
#' @return message
#' @export
warn_or_stop <- function(m, warn_only = FALSE) {
  if (warn_only) {
    return(warning(m, immediate. = TRUE))
  }
  return(stop(m))
}
