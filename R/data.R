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
#' @param view_old logical for viewing old
#' @return messages and data.frame of only changes and reference cols
#' @export
find_df_diff2 <- function (new, old,ref_cols=NULL,message_pass="",view_old = T, n_row_view = 20){
  new <- all_character_cols(new)
  old <- all_character_cols(old)
  if (!all(colnames(new) %in% colnames(old))) {
    stop("All new df columns must be included in old df")
  }
  if (!all(ref_cols %in% colnames(new))| !all(ref_cols %in% colnames(old))) {
    stop("ref_cols must be included in both dfs")
  }
  if (length(ref_cols)>1){
    new_keys <- apply( new[ , ref_cols] , 1 , paste , collapse = "_" )
    old_keys <- apply( old[ , ref_cols ] , 1 , paste , collapse = "_" )
  }else{
    new_keys <- new[ , ref_cols]
    old_keys <- old[ , ref_cols]
  }
  if (anyDuplicated(old_keys) > 0) {
    stop("Keys must lead to unique rows! (old df)")
  }
  if (anyDuplicated(new_keys) > 0) {
    stop("Keys must lead to unique rows! (new df)")
  }
  appended_old_col_suffix <- "__old"
  if(any(endsWith(unique(colnames(old),colnames(new)),appended_old_col_suffix)))stop("colnames cant end with '",appended_old_col_suffix,"'")
  merged_df <- merge(new, old, by = ref_cols, suffixes = c("",appended_old_col_suffix ),all.x = T)
  placeholder <- "NA_placeholder"
  rows_to_keep <- NULL
  cols_to_view <- cols_to_keep <- which(colnames(merged_df) %in% ref_cols)
  COLS <- colnames(new)[which(!colnames(new)%in%ref_cols)]
  for (COL in COLS){
    vector1 <- merged_df[[COL]]
    compare_COL <- paste0(COL, appended_old_col_suffix)
    vector2 <- merged_df[[compare_COL]]
    vector1_no_na <- ifelse(is.na(vector1), placeholder, vector1)
    vector2_no_na <- ifelse(is.na(vector2), placeholder, vector2)
    # Compare vectors element-wise
    are_not_equal <- which(vector1_no_na != vector2_no_na)
    if(length(are_not_equal)>0){
      rows_to_keep <- rows_to_keep %>% append(are_not_equal)
      additional_cols <- which(colnames(merged_df) == COL)
      cols_to_keep <- cols_to_keep %>% append(additional_cols)
      if(view_old){
        cols_to_view <- cols_to_view %>% append(additional_cols) %>% append(which(colnames(merged_df) == compare_COL))
      }
    }
  }
  if(length(rows_to_keep)>0){
    rows_to_keep <- rows_to_keep %>% unique()
    cols_to_keep <- cols_to_keep %>% unique()
    if(view_old){
      rows_to_keep2 <- rows_to_keep
      done <- F
      while ( ! done) {
        length_of_rows_to_keep <- length(rows_to_keep2)
        if(length_of_rows_to_keep==0){
          done <- T
        }else{
          indices <- 1:ifelse(length_of_rows_to_keep<n_row_view,length_of_rows_to_keep,n_row_view)
          rows_to_keep3 <- rows_to_keep2[indices]
          print.data.frame(merged_df[rows_to_keep3,unique(cols_to_view)])
          choice <- utils::menu(choices = c("Check more rows","Proceed with no more checking", "Stop the function"),title = "What would you like to do?")
          if(choice==3)stop("Stopped as requested!")
          if(choice==2)done <- T
          if(choice==1)rows_to_keep2 <- rows_to_keep2[-indices]
        }
      }
    }
    message(message_pass,length(rows_to_keep), " rows have updates")
    return(merged_df[rows_to_keep,cols_to_keep])
  }else{
    message(message_pass,"No changes!")
    return(NULL)
  }
}
#' @title find the difference between two lists of related data.frames
#' @description
#' This function will compare two data.frames: new and old.
#' You define the reference columns with ref_cols.
#' Reference columns are always included in the return data.frame and their combination should always lead to a unique key for each row.
#' @param new_list a new data.frame to compare to old. All new cols must be included in the set of the old ones.
#' @param old_list a reference data.frame to be compared to
#' @param ref_col_list character vector of reference columns. They are always included in the return data.frame and their combination should always lead to a unique key for each row.
#' @param view_old logical for viewing old
#' @return messages and data.frame of only changes and reference cols
#' @export
find_df_list_diff <- function(new_list, old_list,ref_col_list,view_old = T, n_row_view = 20){
  if(!is_something(new_list)){
    message("new_list is empty")
    return(list())
  }
  if(!is_something(old_list)){
    message("old_list is empty")
    return(list())
  }
  if(!is_df_list(new_list))stop("new_list must be a list of data.frames")
  if(!is_df_list(old_list))stop("old_list must be a list of data.frames")
  if(any(!names(new_list)%in%names(old_list)))stop("All new_list names must be included in the set of old_list names.")
  if(!is.list(ref_col_list)){
    ref_col_list <- names(new_list) %>% lapply(function(IN){
      ref_col_list
    })
    names(ref_col_list) <- names(new_list)
  }
  for(df_name in names(new_list)){
    new_list[[df_name]] <- find_df_diff2(new = new_list[[df_name]], old = old_list[[df_name]],ref_cols = ref_col_list[[df_name]], message_pass = paste0(df_name,": "),view_old = view_old, n_row_view = n_row_view)
  }
  return(new_list)
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
#' @title clean_df_blanks
#' @export
clean_df_blanks <- function(df,other_blanks=NULL) {
  df <- df %>% lapply(function(IN){
    IN[which(IN%in%c("NA","",other_blanks))] <- NA
    return(IN)
  }) %>% as.data.frame()
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
#' @title count_vec_df
#' @export
count_vec_df <- function(vec){
  vec <- vec %>% table() %>% sort(decreasing = T)
  df <- data.frame(
    count = as.integer(vec),
    name = names(vec)
  )
  return(df)
}
#' @title vec_to_cvec
#' @export
vec_to_cvec <- function(vec){
  vec %>% paste0("\"",.,"\"",collapse = ",\n") %>% paste0("c(\n",.,"\n)") %>% cat()
}
#' @title reassign_variable_in_bulk
#' @export
reassign_variable_in_bulk <- function(df,old_colname,new_colname,optional_choices){
  if( ! new_colname %in% colnames(df))df[[new_colname]] <- NA
  df_vec_counted <- count_vec_df(df[[old_colname]])
  choices <- "Do Nothing (Skip)"
  has_choices <- F
  if(!missing(optional_choices)) {
    choices <- choices %>% append(c("Best Guess --> ", optional_choices))
    has_choices <- T
  }
  choices <- choices %>% append(c("Manual Entry","Stop and Return Current DF"))
  df_vec_counted %>% head(20) %>% print.data.frame()
  for(i in 1:nrow(df_vec_counted)){ #i <- 1:nrow(df_vec_counted) %>% sample(1)
    the_name <- df_vec_counted$name[i]
    choices_mod <- choices
    best_guess <- NA
    if(has_choices) {
      # best_guess <- stringdist::stringdist(the_name,optional_choices,method = "osa") %>% which.min() %>% optional_choices[.]
      # best_guess <- stringdist::stringdist(the_name,optional_choices,method = "lv") %>% which.min() %>% optional_choices[.]
      # best_guess <- stringdist::stringdist(the_name,optional_choices,method = "dl") %>% which.min() %>% optional_choices[.]
      # best_guess <- stringdist::stringdist(the_name,optional_choices,method = "lcs") %>% which.max() %>% optional_choices[.]
      # best_guess <- stringdist::stringdist(the_name,optional_choices,method = "qgram") %>% which.min() %>% optional_choices[.]
      # best_guess <- stringdist::stringdist(the_name,optional_choices,method = "cosine") %>% which.min() %>% optional_choices[.]
      # best_guess <- stringdist::stringdist(the_name,optional_choices,method = "jw") %>% which.max() %>% optional_choices[.]
      best_guess <- stringdist::stringdist(the_name,optional_choices,method = "jaccard") %>% which.min() %>% optional_choices[.]
      choices_mod[which(choices_mod=="Best Guess --> ")] <- paste0("Best Guess --> ",best_guess)
    }
    choice <- utils::menu(choices_mod,title=paste0("What would you like to do for...?\n\n --> ",the_name," <--"))
    clean_choice <- choices[choice]
    if(clean_choice=="Stop and Return Current DF") return(df)
    if(clean_choice != "Do Nothing (Skip)"){
      if(clean_choice == "Best Guess --> "){
        clean_choice <- best_guess
      }
      if(clean_choice=="Manual Entry")clean_choice <- readline("Enter reassignment here --> ")
      rows <- which(df[[old_colname]]==the_name)
      df[[new_colname]][rows] <- clean_choice
    }
  }
}
edit_variable_while_viewing <- function(DF,optional_DF, field_name_to_change, field_names_to_view=NULL){
  change_form <- field_names_to_instruments(DB,field_name_to_change)
  view_forms <- field_names_to_instruments(DB,field_names_to_view)
  field_names_to_view <- c(field_name_to_change,field_names_to_view) %>% unique()
  # if(length(view_forms)>1)stop("only one form combinations are allowed.")
  if(missing(records)) records <- DB$data_extract[[view_forms]][[DB$redcap$id_col]] %>% unique()
  all_forms <- c(change_form,view_forms) %>% unique()
  ref_cols_change <- DB$redcap$instrument_key_cols[[change_form]]
  # ref_cols_view <- DB$redcap$instrument_key_cols[[view_forms]]
  if(missing(optional_DF)){
    optional_DF <- DB[["data_extract"]][[change_form]][,unique(c(ref_cols_change,field_names_to_view))]
  }
  if(is.null(field_names_to_view)) field_names_to_view <- colnames(optional_DF)
  # if(any(!ref_cols%in%colnames(DF)))stop("DF must contain all ref_cols")
  if(length(records)>0){
    # message("fix these in REDCap --> ",paste0(out,collapse = " | "))
    rows_of_choices <- which(DB$redcap$codebook$field_name==field_name_to_change)
    has_choices <- length(rows_of_choices)>0
    choices1 <- c("Do Nothing", "Edit","Launch Redcap Link Only")
    if(has_choices){
      choices2 <- c("Do Nothing",DB$redcap$codebook$name[rows_of_choices],"Launch Redcap Link Only")
    }else{
      choices2 <- c("Do Nothing","Manual Entry","Launch Redcap Link Only")
    }
    is_repeating_form <- change_form %in% DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)]
    OUT <- NULL
    for (record in records){ # record <- records%>% sample(1)
      record_was_updated <- F
      VIEW <- optional_DF[which(optional_DF[[DB$redcap$id_col]]==record),]
      VIEW_simp <- VIEW[,unique(c(DB$redcap$id_col,field_names_to_view))] %>% unique()
      row.names(VIEW_simp) <- NULL
      VIEW_simp %>%t() %>% print()
      CHANGE <- filter_DB(DB, records = record, form_names = change_form)[[1]]
      row.names(CHANGE) <- NULL
      CHANGE <- CHANGE[,unique(c(ref_cols_change,field_name_to_change))]
      if(nrow(CHANGE)==0){
        print("Nothing in CHANGE. If you choose edit it will add an instance...")
        blank_row <- data.frame(
          record
        )
        colnames(blank_row)[[1]] <- DB$redcap$id_col
        if("redcap_repeat_instance"%in%ref_cols_change){
          blank_row$redcap_repeat_instance <- "1"
          blank_row$redcap_repeat_instrument <- change_form
        }
        blank_row[[field_name_to_change]] <- NA
      }else{
        print(CHANGE)
      }
      choice1 <- utils::menu(choices1,title=paste0("What would you like to do?"))
      if(choice1 == 3){
        DB %>% link_REDCap_record(record = record)
      }
      if(choice1 == 2){
        if(nrow(CHANGE)==0)CHANGE <- blank_row
        for(j in 1:nrow(CHANGE)){
          message("Old answer (",field_name_to_change, "): ",CHANGE[j,field_name_to_change])
          choice2 <- utils::menu(choices2,title=paste0("What would you like to do?"))
          choice <- choices2[choice2]
          OUT_sub <- CHANGE[j,]
          if(choice %in% c("Manual Entry","Do Nothing","Launch Redcap Link Only")){
            if(choice=="Do Nothing"){
              message("Did not change anything")
            }
            if(choice=="Manual Entry"){
              OUT_sub[[field_name_to_change]] <- readline("What would you like it to be? ")
              if(upload_individually){
                OUT_sub %>% labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
                message("Uploaded: ",OUT_sub %>% paste0(collapse = " | "))
                record_was_updated <- T
              }else{
                OUT <- OUT %>% dplyr::bind_rows(OUT_sub)
              }
            }
            if(choice=="Launch Redcap Link Only"){#account for repeat? instance
              DB %>% link_REDCap_record(record = record,page = change_form,instance = CHANGE[j,"redcap_repeat_instance"])
            }
          }else{
            OUT_sub[[field_name_to_change]] <- choice
            if(upload_individually){
              OUT_sub %>% labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
              message("Uploaded: ",OUT_sub %>% paste0(collapse = " | "))
              record_was_updated <- T
            }else{
              OUT <- OUT %>% dplyr::bind_rows(OUT_sub)
            }
          }
        }
        if(is_repeating_form){
          choice3 <- 2
          the_max <- 0
          if(nrow(CHANGE)>0)the_max <- CHANGE$redcap_repeat_instance %>% as.integer() %>% max()
          while (choice3 == 2) {
            choice3 <- utils::menu(c("No","Yes"),title=paste0("Would you like to add an additional instance?"))
            if(choice3 == 2){
              OUT_sub <- data.frame(
                record_id = record,
                redcap_repeat_instrument = change_form,
                redcap_repeat_instance = as.character(the_max + 1)
              )
              colnames(OUT_sub)[1]<-DB$redcap$id_col
              choice2 <- utils::menu(choices2,title=paste0("What would you like to do?"))
              choice <- choices2[choice2]
              if(choice %in% c("Manual Entry","Do Nothing","Launch Redcap Link Only")){
                if(choice=="Do Nothing"){
                  message("Did not change anything")
                }
                if(choice=="Manual Entry"){
                  OUT_sub[[field_name_to_change]] <- readline("What would you like it to be? ")
                  if(upload_individually){
                    OUT_sub %>% labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
                    message("Uploaded: ",OUT_sub %>% paste0(collapse = " | "))
                    record_was_updated <- T
                  }else{
                    OUT <- OUT %>% dplyr::bind_rows(OUT_sub)
                  }
                  the_max <- the_max + 1
                }
                if(choice=="Launch Redcap Link Only"){#account for repeat? instance
                  DB %>% link_REDCap_record(record = record,page = change_form,instance = CHANGE[j,"redcap_repeat_instance"])
                }
              }else{
                OUT_sub[[field_name_to_change]] <- choice
                if(upload_individually){
                  OUT_sub %>% labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
                  message("Uploaded: ",OUT_sub %>% paste0(collapse = " | "))
                  record_was_updated <- T
                }else{
                  OUT <- OUT %>% dplyr::bind_rows(OUT_sub)
                }
                the_max <- the_max + 1
              }
            }
          }
        }
      }
    }
    if(record_was_updated)DB <- update_DB(DB)
  }
  if(!upload_individually)OUT %>% labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
}
#' @title count_instances
#' @export
count_instances <- function(df,ref_id,inst_name){
  vec_ori<- df[[ref_id]]
  x<- vec_ori %>% rle()
  x<- data.frame(
    lengths = x$lengths,
    values = x$values
  )
  vec <- x$lengths %>% sapply(function(IN){1:IN}) %>% unlist()
  vec2 <- 1:nrow(x) %>% sapply(function(ROW){rep(x$values[ROW],x$lengths[ROW])}) %>% unlist()
  if(any(vec_ori!=vec2))stop("mismatch!")
  df[[inst_name]] <- vec
  key_check <- paste0(df[[ref_id]],"__",df[[inst_name]])
  dup_keys <- key_check[which(duplicated(key_check))]
  if(length(dup_keys)>0)stop("You can't have ids that are not sorted by the ref_id ('",ref_id,"') ",dup_keys %>% paste0(collapse = ", "))
  return(df)
}
#' @title scale_vec_to_range
#' @export
scale_vec_to_range <- function(vec, max_target, min_target = 1) {
  scaled_vec <- round((vec - min(vec)) / (max(vec) - min(vec)) * (max_target - min_target) + min_target)
  min_vec <- min(vec)
  max_vec <- max(vec)
  scaled_vec <- round(((vec - min_vec) / (max_vec - min_vec)) * (max_target - min_target) + min_target)
  return(scaled_vec)
}
#' @title vec_to_empty_df
#' @export
vec_to_empty_df <- function(vec,nrow = 0) {
  df <- data.frame(matrix(data = NA, ncol = length(vec), nrow = 0))
  colnames(df) <- vec
  return(df)
}
