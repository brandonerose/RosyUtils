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
#' @export
all_character_cols <- function(df){
  as.data.frame(lapply(df,as.character))
}

#' @export
all_character_cols_list <- function(list){
  lapply(list,all_character_cols)
}
