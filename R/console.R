#' @title bullet_in_console
#' @export
bullet_in_console <- function(text = "",url = NULL,bullet_type = "i",collapse = T, file = NULL,verbosity=2){
  url_if <- ""
  file_if <- ""
  if(length(url)>0){
    # url %>% lapply(function(IN){validate_web_link(IN)}) # doesnt work for /subheaders/
    # url_if <- " {.url {url}}"
    url_names <- names(url)
    if(is.list(url)){
      url_names <- url %>% unlist()
      if(is_named_list(url))url_names <- names(url)
      url <- unlist(url)
    }
    if(is.null(url_names))url_names <- url
    if(collapse)url_if <- paste0(url_if,collapse = " and ")
    url_if <- paste0(" {cli::col_blue(cli::style_hyperlink('",url_names,"', '",url,"'))}")
  }
  if(length(file)>0){
    file_names <- names(file)
    if(is.list(file)){
      file_names <- file %>% unlist()
      if(is_named_list(file))file_names <- names(file)
      file <- unlist(file)
    }
    if(is.null(file_names))file_names <- file
    if(collapse)file_if <- paste0(file_if,collapse = " and ")
    file_if <- paste0(
      " {cli::col_blue(cli::style_hyperlink('",
      sanitize_path(file_names),
      "', '",
      sanitize_path(paste0("file://",file)),
      "'))}"
    )
  }
  for(i in 1:length(url_if))text[i] <- paste0(text[i],url_if[i])
  for(i in 1:length(file_if))text[i] <- paste0(text[i],file_if[i])
  names(text)[1:length(text)]<- bullet_type
  return(cli::cli_bullets(text))
  # * = • = bullet
  # > = → = arrow
  # v = ✔ = success
  # x = ✖ = danger
  # ! = ! = warning
  # i = ℹ = info
}
