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
#' @title clean_function
#' @export
clean_function <- function(func) {
  if (!is.function(func)) {
    stop("Input must be a function")
  }
  environment(func) <- emptyenv()
  return(func)
}
#' @title clean_function_list
#' @export
clean_function_list <- function(func_list) {
  if (!is.list(func_list)) {
    stop("Input must be a list")
  }
  for (i in seq_along(func_list)) {
    if (is.function(func_list[[i]])) {
      func_list[[i]] <- clean_function(func_list[[i]])
    }
  }
  return(func_list)
}
#' @title size_func
#' @export
size_func <- function(x) {
  # Start with the function's own size
  total_size <- object.size(x)
  # Check if x is a function with a non-empty environment
  if (is.function(x) && !identical(environment(x), emptyenv())) {
    # Add the size of the environment
    env_objects <- ls(envir = environment(x), all.names = TRUE)
    env_size <- sum(unlist(lapply(env_objects, function(obj) object.size(get(obj, envir = environment(x))))))
    # Add environment size to total
    total_size <- total_size + env_size
  }
  format(total_size, units = "auto")
}
