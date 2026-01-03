clean_for_cli <- function(path){
  gsub("'", "\\\\'", path)
}
#' @title cli_alert_wrap
#' @export
cli_alert_wrap <- function(text = "",
                           url = NULL,
                           bullet_type = "i",
                           collapse = TRUE,
                           file = NULL,
                           silent = FALSE) {
  if (silent) {
    return(invisible())
  }
  url_if <- ""
  file_if <- ""
  if (length(url) > 0) {
    # url |> lapply(function(x){assert_web_link(x)})
    # doesnt work for /subheaders/
    # url_if <- " {.url {url}}"
    url_names <- names(url)
    if (is.list(url)) {
      url_names <- unlist(url)
      if (is_named_list(url)) {
        url_names <- names(url)
      }
      url <- unlist(url)
    }
    if (is.null(url_names)) url_names <- url
    if (collapse) url_if <- paste0(url_if, collapse = " and ")
    url_if <- paste0(
      " {cli::col_blue(cli::style_hyperlink('",
      url_names|> clean_for_cli(),
      "', '",
      url|> clean_for_cli(),
      "'))}"
    )
  }
  if (length(file) > 0) {
    file_names <- names(file)
    if (is.list(file)) {
      file_names <- unlist(file)
      if (is_named_list(file)) file_names <- names(file)
      file <- unlist(file)
    }
    if (is.null(file_names)) file_names <- file
    if (collapse) file_if <- paste0(file_if, collapse = " and ")
    file_if <- paste0(
      " {cli::col_blue(cli::style_hyperlink('",
      sanitize_path(file_names) |> clean_for_cli(),
      "', '",
      sanitize_path(paste0("file://", file)) |> clean_for_cli(),
      "'))}"
    )
  }
  for (i in seq_along(url_if)) text[i] <- paste0(text[i], url_if[i])
  for (i in seq_along(file_if)) text[i] <- paste0(text[i], file_if[i])
  names(text)[seq_along(text)] <- bullet_type
  cli::cli_bullets(text)
}
