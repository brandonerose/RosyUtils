#' @title function_to_string
#' @export
function_to_string <- function(func) {
  # Deparse the function and collapse into a single string using "\n"
  deparse(func) %>% paste(collapse = "\n")
}
#' @title function_to_string
#' @export
string_to_function <- function(func_string) {
  # Split the string into a character vector by "\n" and parse/eval it
  eval(parse(text = strsplit(func_string, "\n")[[1]]))
}
