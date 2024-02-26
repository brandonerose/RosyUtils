#' @export
sample1 <- function(x){
  sample(x,1)
}
#' @export
ul <- function(x){
  length(unique(x))
}
#' @export
wl <- function(x){
  length(which(x))
}
