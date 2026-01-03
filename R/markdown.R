#' @title validate_web_link
#' @export
recursive_markdown <- function(
    DF,
    order_master,
    level = 1,
    display_cols = NULL,
    link_cols = NULL,
    bold_cols = NULL,
    named_cols = NULL,
    header_col = NULL,
    name_master = NULL,
    new_pages_levels = NULL,
    newpage_triggered = FALSE
) {
  if (!is_something(DF)) {
    return()
  }
  # Bottom-level: print rows
  if (level > length(order_master)) {
    the_cols <- c(
      header_col,
      display_cols,
      bold_cols,
      named_cols
    ) |>
      unlist() |>
      unname()|>
      unique()
    has_header <- !is.null(header_col)
    has_links <- !is.null(link_cols)
    has_named_links <- is_named_list(link_cols)
    for (i in seq_len(nrow(DF))) {
      row <- DF[i, ]
      for(the_col in the_cols){
        name_val <- escape_latex(row[[the_col]])
        # LaTeX: bold name, blue clickable link
        # \t- \n\n
        if(!is.na(name_val)){
          final_text <- "%s"
          is_header <- the_col == header_col
          is_bold <- the_col %in% bold_cols
          is_named <- the_col %in% named_cols
          is_link <- FALSE
          if(has_links){
            if(has_named_links){
              comparison <- names(link_cols)
            }else{
              comparison <- link_cols
            }
            is_link <- the_col %in% comparison
            is_named_link <- has_named_links
          }
          if(is_link){
            final_text <- "\\href{%s}{\\textcolor{blue}{%s}}"
            the_link <- escape_latex(row[[link_cols[[which(comparison==the_col)]]]])
          }
          if(is_bold){
            paste0("\\textbf{",final_text,"}")
          }
          if(is_named){
            the_col2 <- the_col
            if(is_named_list(name_master)){
              the_col2 <- names(name_master)[which(name_master==the_col)]
            }
            final_text <- paste0(the_col2,": ",final_text)
          }
          final_text <- paste0("- ",final_text,"\n\n")
          if(has_header && !is_header){
            final_text <- paste0("\n \t",final_text)
          }
          if(is_link){
            cat(sprintf(
              final_text,
              the_link,
              name_val
            ))
          }else{
            cat(sprintf(
              final_text,
              name_val
            ))
          }
        }
      }
    }
    return()
  }
  # Intermediate levels: recurse
  col_name <- names(order_master)[level]
  values <- order_master[[level]]
  is_included <- col_name %in% names(DF)
  if(!is_included){
    recursive_markdown(
      DF = DF,
      order_master = order_master,
      level = level + 1,
      display_cols = display_cols,
      link_cols = link_cols,
      bold_cols = bold_cols,
      named_cols = named_cols,
      header_col = header_col,
      name_master = name_master,
      new_pages_levels = new_pages_levels,
      newpage_triggered = newpage_triggered
    )
  }
  if (is_included) {
    remaining_vals <- unique(DF[[col_name]])
    for (val in c(values, setdiff(remaining_vals, values))) {
      sub_data <- DF[DF[[col_name]] == val, , drop = FALSE]
      if (nrow(sub_data) == 0) next
      # Print header with proper escaping
      if (!newpage_triggered && level %in% new_pages_levels) {
        cat("\\newpage\n")
        message("NEW PAGE for level ", level, ": ", col_name, " = ", val)
        newpage_triggered <- TRUE
      }
      cat(paste0(strrep("#", level), " ", escape_latex(val), "\n\n"))
      recursive_markdown(
        DF = sub_data,
        order_master = order_master,
        level = level + 1,
        display_cols = display_cols,
        link_cols = link_cols,
        bold_cols = bold_cols,
        named_cols = named_cols,
        header_col = header_col,
        name_master = name_master,
        new_pages_levels = new_pages_levels,
        newpage_triggered = newpage_triggered
      )
    }
  }
}
escape_latex <- function(text) {
  text <- gsub("&amp;", "&", text)
  text <- gsub("&lt;", "<", text)
  text <- gsub("&gt;", ">", text)
  text <- gsub("&quot;", "\"", text)
  text <- gsub("&#39;", "'", text)
  text <- gsub("\\\\", "\\textbackslash{}", text)
  text <- gsub("([&%$#_{}])", "\\\\\\1", text)
  text <- gsub("~", "\\textasciitilde{}", text)
  text <- gsub("\\^", "\\textasciicircum{}", text)
  return(text)
}
