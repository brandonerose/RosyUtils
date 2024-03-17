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
