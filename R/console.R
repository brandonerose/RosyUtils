#' @title clickable_url_in_console
#' @export
bullet_in_console <- function(text = "",url = NULL,bullet_type = "i",collape_urls = F){
  url_if <- ""
  if(length(url)>0){
    url %>% lapply(function(IN){validate_web_link(IN)})
    url_if <- " {.url {url}}"
    url_names <- names(url)
    if(is.list(url)){
      url_names <- url %>% unlist()
      if(is_named_list(url))url_names <- names(url)
      url <- unlist(url)
    }
    if(!is.null(url_names)){
      url_if <- paste0(" {cli::col_blue(cli::style_hyperlink('",url_names,"', '",url,"'))}")
      if(collape_urls)url_if <- paste0(url_if,collapse = " and ")
    }else{
      url_names <- url
    }
  }
  for(i in 1:length(url_if))text[i] <- paste0(text[i],url_if[i])
  names(text)[1:length(text)]<- bullet_type
  return(cli::cli_bullets(text))
  # * = • = bullet
  # > = → = arrow
  # v = ✔ = success
  # x = ✖ = danger
  # ! = ! = warning
  # i = ℹ = info
}
