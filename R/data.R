#' @title find the difference between two data.frames
#' @description
#' This function will compare two data.frames: new and old.
#' You define the reference columns with ref_cols.
#' Reference columns are always included in the return data.frame and their combination should always lead to a unique key for each row.
#' @param new a new data.frame to compare to old. All new cols must be included in the set of the old ones.
#' @param old a reference data.frame to be compared to
#' @param ref_cols character vector of reference columns. They are always included in the return data.frame and their combination should always lead to a unique key for each row.
#' @return messages and data.frame of only changes and reference cols
#' @export
find_df_diff <- function (new, old,ref_cols=NULL,message_pass=""){
  new <- all_character_cols(new)
  old <- all_character_cols(old)
  if (!all(colnames(new) %in% colnames(old))) {
    stop("All new df columns must be included in old df")
  }
  if (!all(ref_cols %in% colnames(new))| !all(ref_cols %in% colnames(old))) {
    stop("ref_cols must be included in both dfs")
  }
  if (length(ref_cols)>1){
    new$key <- apply( new[ , ref_cols] , 1 , paste , collapse = "_" )
    old$key <- apply( old[ , ref_cols ] , 1 , paste , collapse = "_" )
  }else{
    new$key <- new[ , ref_cols]
    old$key <- old[ , ref_cols]
  }
  if (anyDuplicated(old$key) > 0) {
    stop("Keys must lead to unique rows! (old df)")
  }
  if (anyDuplicated(new$key) > 0) {
    stop("Keys must lead to unique rows! (new df)")
  }
  new_keys <- integer(0)
  if(any(!new$key %in% old$key)){
    # warning("You have at least one new key compared to old df therefore all columns will be included by default",immediate. = T)
    new_keys <- which(!new$key %in% old$key)
  }
  indices <- data.frame(
    row = integer(0),
    col = integer(0)
  )
  for(new_key in new_keys){
    indices <- indices %>% dplyr::bind_rows(
      data.frame(
        row = new_key,
        col = which(!colnames(new)%in%c(ref_cols,"key"))
      )
    )
  }

  for (KEY in new$key[which(new$key%in%old$key)]){
    row <- which(new$key == KEY)
    row_old <- which(old$key == KEY)
    for (COL in colnames(new)[which(!colnames(new)%in%c(ref_cols,"key"))]){
      col <- which(colnames(new) == COL)
      if(!identical(new[row,COL],old[row_old,COL])){
        indices <- indices %>% dplyr::bind_rows(
          data.frame(
            row = row,
            col = col
          )
        )
      }
    }
  }
  if(nrow(indices)>0){
    rows <- indices$row %>% unique() %>% sort()
    cols <- which(colnames(new)%in%ref_cols) %>% append(indices$col %>% unique() %>% sort())
    OUT <- new[rows,cols]
    message(message_pass,nrow(OUT), " rows have updates")
  }else{
    OUT <- NULL
    message(message_pass,"No changes!")
  }
  OUT
}
#' @title find the difference between two data.frames find_df_diff2
#' @description
#' This function will compare two data.frames: new and old.
#' You define the reference columns with ref_cols.
#' Reference columns are always included in the return data.frame and their combination should always lead to a unique key for each row.
#' @param new a new data.frame to compare to old. All new cols must be included in the set of the old ones.
#' @param old a reference data.frame to be compared to
#' @param ref_cols character vector of reference columns. They are always included in the return data.frame and their combination should always lead to a unique key for each row.
#' @return messages and data.frame of only changes and reference cols
#' @export
find_df_diff2 <- function (new, old,ref_cols=NULL,message_pass=""){
  new <- all_character_cols(new)
  old <- all_character_cols(old)
  if (!all(colnames(new) %in% colnames(old))) {
    stop("All new df columns must be included in old df")
  }
  if (!all(ref_cols %in% colnames(new))| !all(ref_cols %in% colnames(old))) {
    stop("ref_cols must be included in both dfs")
  }
  if (length(ref_cols)>1){
    new$key <- apply( new[ , ref_cols] , 1 , paste , collapse = "_" )
    old$key <- apply( old[ , ref_cols ] , 1 , paste , collapse = "_" )
  }else{
    new$key <- new[ , ref_cols]
    old$key <- old[ , ref_cols]
  }
  if (anyDuplicated(old$key) > 0) {
    stop("Keys must lead to unique rows! (old df)")
  }
  if (anyDuplicated(new$key) > 0) {
    stop("Keys must lead to unique rows! (new df)")
  }
  appended_old_col_suffix <- "_old_col"
  if(any(endsWith(unique(colnames(old),colnames(new)),appended_old_col_suffix)))stop("colnames cant end with '",appended_old_col_suffix,"'")
  merged_df <- merge(new, old, by = ref_cols, suffixes = c("",appended_old_col_suffix ))
  placeholder <- "NA_placeholder"
  rows_to_keep <- NULL
  cols_to_keep <- which(colnames(new) %in% ref_cols)
  COLS <- colnames(new)[which(!colnames(new)%in%ref_cols)]
  for (COL in COLS){
    vector1 <- merged_df[[COL]]
    vector2 <- merged_df[[paste0(COL, appended_old_col_suffix)]]
    vector1_no_na <- ifelse(is.na(vector1), placeholder, vector1)
    vector2_no_na <- ifelse(is.na(vector2), placeholder, vector2)
    # Compare vectors element-wise
    are_not_equal <- which(vector1_no_na != vector2_no_na)
    if(length(are_not_equal)>0){
      rows_to_keep <- rows_to_keep %>% append(are_not_equal)
      cols_to_keep <- cols_to_keep %>% append(which(colnames(new) == COL))
    }
  }
  if(length(rows_to_keep)>0){
    rows_to_keep <- rows_to_keep %>% unique() %>% sort()
    cols_to_keep <- cols_to_keep %>% unique() %>% sort()
    message(message_pass,length(rows_to_keep), " rows have updates")
    return(new[rows_to_keep,cols_to_keep])
  }else{
    message(message_pass,"No changes!")
    return(NULL)
  }
}


#' @title all_character_cols
#' @export
all_character_cols <- function(df){
  as.data.frame(lapply(df,as.character))
}

#' @title all_character_cols_list
#' @export
all_character_cols_list <- function(list){
  lapply(list,all_character_cols)
}

#' @title collapse_DF
#' @export
collapse_DF <- function(DF,ref_id,list_mod){
  if( ! ref_id %in% colnames(DF))stop("`ref_id` must be a colname in `DF`")
  new_DF <- NULL
  new_DF[[ref_id]] <- unique(DF[[ref_id]])
  other_cols <-colnames(DF)[which(!colnames(DF) %in% ref_id)]
  if(!missing(list_mod)){
    for(i in 1:length(list_mod)){
      new_DF[[names(list_mod[i])]] <- new_DF[[ref_id]] %>% sapply(function(ID){
        sub_df <- DF[which(DF[[ref_id]]==ID),list_mod[[i]]]
        n_col <- ncol(sub_df)
        n_row <- nrow(sub_df)
        out_final <- NULL
        for(j in 1:n_row){
          out <- NULL
          for(k in 1:n_col){
            out <- out %>% append(paste0(colnames(sub_df)[k]," - ",sub_df[j,k]))
          }
          out <- out %>% paste(collapse = " | ")
          out_final <- out_final %>% append(out)
        }
        out_final <- out_final %>% paste(collapse = " || ")
        out_final
      }) %>% as.character()
    }
    DF <- DF[,which(!colnames(DF)%in%list_mod[[i]])]
    other_cols <- other_cols[which(!other_cols%in%list_mod[[i]])]
  }
  other_col <- other_cols %>% sample(1)
  for (other_col in other_cols) {
    new_DF[[other_col]] <- sapply( new_DF[[ref_id]], function(ID){
      x<- unique(DF[[other_col]][which(DF[[ref_id]]==ID)]) %>% drop_nas()
      if(length(x)==0)return(NA)
      return(paste0(x,collapse = " | "))
    })
  }
  as.data.frame(new_DF)
}

#' @title clean_df_cols
#' @export
clean_df_cols <- function(df) {
  str <- tolower(colnames(df))
  str <- gsub("[^a-z0-9\\s]+", " _", str)
  str <- gsub("\\s+", "_", str)
  str <- gsub("^_|_$", "", str)
  str <- gsub("^_|_$", "", str)
  str <- gsub("__", "_", str)
  str <- gsub("__", "_", str)
  if(anyDuplicated(str)>0)stop("Duplicate col names!")
  colnames(df) <- str
  return(df)
}

#' @title clean_env_names
#' @export
clean_env_names <- function(env_names,silent = F,lowercase=T){
  cleaned_names <- character(length(env_names))
  for (i in seq_along(env_names)) {
    name <- env_names[i]
    is_valid <- is_env_name(name, silent = TRUE)
    if (is_valid) cleaned_names[i] <- name
    if (!is_valid) {
      if (!silent) message("Invalid environment name: '", name)
      cleaned_name <- gsub("__","_",gsub(" ","_",gsub("-","",name)))
      if(lowercase)cleaned_name <- tolower(cleaned_name)
      if(cleaned_name%in%cleaned_names){
        if (!silent) message("Non-unique environment name: '", name, "', added numbers...")
        cleaned_name <- cleaned_name %>% paste0("_",max(wl(cleaned_name%in%cleaned_names))+1)
      }
      cleaned_names[i] <- cleaned_name
    }
  }
  return(cleaned_names)
}

#' @title addSlashIfNeeded
#' @export
addSlashIfNeeded <- function(input_string) {
  if (!endsWith(input_string, "/")) {
    output_string <- gsub("$", "/", input_string)
  } else {
    output_string <- input_string
  }
  return(output_string)
}

#' @title remove_html_tags
#' @export
remove_html_tags <- function(text_vector) {
  # Regular expression to match HTML tags
  html_pattern <- "<[^>]+>"
  # Use gsub to remove the HTML tags from each element in the vector
  cleaned_vector <- gsub(html_pattern, "", text_vector)
  return(cleaned_vector)
}

#' @title check_match
#' @export
check_match <- function(vec_list) {
  sorted_vecs <- lapply(vec_list, sort)
  all(sapply(sorted_vecs[-1], function(x) identical(sorted_vecs[[1]], x)))
}

#' @title vec1_in_vec2
#' @export
vec1_in_vec2 <- function(vec1,vec2){
  vec1[which(vec1 %in% vec2)]
}

#' @title vec1_not_in_vec2
#' @export
vec1_not_in_vec2 <- function(vec1,vec2){
  vec1[which(!vec1 %in% vec2)]
}

#' @title find_in_DF_list
#' @export
find_in_df_list <- function(df_list,text,exact = F){
  df_list <- process_df_list(df_list)
  out <- data.frame(
    record_id = character(0),
    col = character(0),
    row = character(0)
  )
  if (!exact){
    text <- tolower(text)
  }
  for(form in names(df_list)){
    DF <- df_list[[form]]
    for(col in colnames(DF)){
      if (!exact){
        DF[[col]] <- tolower(DF[[col]])
      }
      rows <- which(grepl(text,DF[[col]]))
      if(length(rows)>0){
        out <- out %>%dplyr::bind_rows(
          data.frame(
            record_id = DF[[DB$redcap$id_col]][rows],
            col = col,
            row = as.character(rows)
          )
        )
      }
    }
  }
  return(out)
}

