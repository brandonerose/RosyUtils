#' @title clean_env_names
#' @export
clickable_url_in_console <- function(url,prefix = ""){
  if(length(url)>0){
    prefix <- prefix %>% trimws()
    prefix_subsequent <- paste0(rep(".",nchar(prefix)),collapse = "")
    for(i in 1:length(url)){
      prefix_used <- prefix
      if(i != 1){
        prefix_used <- prefix_subsequent
      }
      cli::cli_text(paste0(prefix_used," {cli::col_blue(cli::style_hyperlink('",url[i],"', '",url[i],"'))}"))
    }
  }
}
