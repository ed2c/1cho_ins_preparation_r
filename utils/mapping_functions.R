

#' Mapping Translate
#'
#' Translates a categorical value into a new categorical value. This function is
#' set up in such a way that it uses the information in the mapping tables and for this
#' all such 'mappings' are documented immediately.
#'
#' @param Data The data frame.
#' @param current Current name.
#' @param new New name.
#' @param KeepOriginal Defaults to TRUE to keep the original value.
#' If FALSE then the values of 'current' are not preserved.
#' @param mapping_table_input Defaults to NULL. If a mapping
#' table must be read.
#' @param mapping_table_name Default NULL. If there is an existing mapping table
#' must be read from the name in the folder
#' @family mapping
#' @return A new data frame with new values based on the 'mapping tables'.
#' @export
mapping_translate <- function(Data, current, new, mapping_table_input = NULL, mapping_table_name = NULL, KeepOriginal = T) {
  Data$CURRENT <- as.character(unlist(Data[, current])) ## To use data from a tibble

  ## Check that mapping_table contains the columns "from" and "to". If that
  ## is not so, an error message is given
  if (!is.null(mapping_table_input)) {
    if (!(any(names(mapping_table_input) == "from") &&
          any(names(mapping_table_input) == "to"))) {
      stop("mapping_table must contain the columns 'from' and 'to'.")
    }
  }

  #If a mapping table is included, there is no need to read a csv file anymore.
  if(!is.null(mapping_table_input)) {
    translate <- mapping_table_input
  } else if(!is.null(mapping_table_name)){
    translate <- utils::read.csv2(paste0(Sys.getenv("MAP_TABLE_DIR"), mapping_table_name, ".csv"), stringsAsFactors = F)
  } else {
    translate <- utils::read.csv2(paste0(Sys.getenv("MAP_TABLE_DIR"),
                                         "Mapping_",current,"_",new,".csv"))
  }


  translate$from <- as.character(translate$from)
  translate$to <- as.character(translate$to)

  ## Translate to factors with sorted levels as in the csv file
  # translate$to <- factor(translate$to, levels=unique(translate$to))
  # Data$TO <- translate$to[match(Data$CURRENT,translate$from)]

  Data <- Data %>% dplyr::left_join(translate, by = c("CURRENT" = "from"))

  if (!KeepOriginal) {
    Data <- Data[ , !names(Data) %in% c(current)]
  }
  ## Change factors to characters
  # if(is.factor(Data$TO)){
  #   Data$TO <- as.character(Data$TO)
  # }

  ## If a column already exists with the name of the new variable name
  if(new %in% colnames(Data)) {
    stop("the specified new column name already exists in the specified dataframe")
  }
  colnames(Data)[which(names(Data) == "to")] <- new
  Data$CURRENT <- NULL

  return(Data)
}






#' Mapping Translate2
#'
#' Translates a categorical value into a new categorical value. This function is
#' set up in such a way that it uses the information in the mapping tables and for this
#' all such 'mappings' are documented immediately.
#'
#' @param dataframe The data frame.
#' @param current1 Current name 1.
#' @param current2 Current name 2.
#' @param new New name.
#' @param KeepOriginal Defaults to TRUE to keep the original value.
#' If FALSE then the values of 'current' are not preserved.
#' @param mapping_table_name Default NULL. If there is an existing mapping table
#' must be read from the name in the folder
#' @param mapping_table_object Defaults to NULL. If a mapping
#' table must be read.
#' @family mapping
#' @return A new data frame with new values based on the 'mapping tables'.
#' @export
mapping_translate2 <- function(df, current1, current2, new, mapping_table_name = NULL, mapping_table_object = NULL, KeepOriginal = TRUE) {

  #'* INFO* Start with checks and errors

  # If a column already exists with the name of the new variable name
  if (new %in% colnames(df)) {
    stop("the specified new column name already exists in the specified dataframe")
  }

  # Load mappping table, if possible
  if (!is.null(mapping_table_object)) {
    mapping_table <- mapping_table_object
  } else {
    # Deduce location of mapping table based on env var and given reference
    if (!is.null(mapping_table_name)) {
      mapping_table_file_path <- paste0(Sys.getenv("MAP_TABLE_DIR"), mapping_table_name, ".csv")

    } else {
      mapping_table_file_path <- paste0(Sys.getenv("MAP_TABLE_DIR"),
                                        "Mapping_",
                                        current1,
                                        "_and_",
                                        current2,
                                        "_",
                                        new,
                                        ".csv",
                                        sep = "")
    }

    if (file.exists(mapping_table_file_path) == FALSE) {
      stop(paste("Your mapping table reference leads to a location without a file. The deduced path is: ", mapping_table_file_path, ".\n Please update the reference to the mapping or envvar MAP_TABLE_DIR."))
    }
    mapping_table <- utils::read.csv2(mapping_table_file_path)
  }

  # Check that mapping_table contains the columns "from" and "to". If that
  # is not so, an error message is given
  if (!is.null(mapping_table)) {
    if (!(any(names(mapping_table) == "from1") &&
          any(names(mapping_table) == "from2") &&
          any(names(mapping_table) == "to"))) {
      stop("mapping_table must contain the columns 'from1', 'from2' and 'to'.")
    }
  }

  ## TODO Check that values in mapping table 'from'-column are unique
  if (nrow(mapping_table) != length(unique(paste(mapping_table$from1, mapping_table$from2)))) {
    stop("mapping table doesn't have combined unique values in the from1 and from2 field and is thus ambiguous.")
  }

  #'*INFO* Now apply mapping table

  mapping_table$from1 <- as.character(mapping_table$from1)
  mapping_table$from2 <- as.character(mapping_table$from2)
  mapping_table$to <- as.character(mapping_table$to)

  df$current1 <- as.character(unlist(df[, current1])) # To use data from a tibble
  df$current2 <- as.character(unlist(df[, current2]))


  ## TODO Why is this done?
  ## Translate to factors with sorted levels as in the csv file
  # mapping_table$to <- factor(mapping_table$to, levels = unique(mapping_table$to))
  # df$to <- mapping_table$to[match(df$current,mapping_table$from)]

  df <- df %>% dplyr::left_join(mapping_table, by = c("current1" = "from1",
                                                      "current2" = "from2"))


  if (KeepOriginal == FALSE) {
    df <- df[ , !names(df) %in% c(current1)]
    df <- df[ , !names(df) %in% c(current2)]
  }

  colnames(df)[which(names(df) == "to")] <- new

  df$current1 <- NULL
  df$current2 <- NULL

  return(df)
}

## TODO Function is needlessly complex
maptbl_config2suffix <- function(config_settings, mapping_table_name = "Mapping_config") {

  ## prepare for mapping_table_n
  num_settings <- length(config_settings)
  col_names <- paste0("from", seq_len(num_settings))
  df <- as.data.frame(matrix(unlist(config_settings), nrow = 1, byrow = TRUE))
  names(df) <- col_names

  result_df <- mapping_translate_n(
    df,
    currents = names(df),
    "suffix",
    mapping_table_name = mapping_table_name,
    KeepOriginal = FALSE
  )

  file_name_suffix <- result_df %>% pull(suffix)


  return(file_name_suffix)
}




#' Mapping Translate2
#'
#' Translates a categorical value into a new categorical value. This function is
#' set up in such a way that it uses the information in the mapping tables and for this
#' all such 'mappings' are documented immediately.
#'
#' @param dataframe The data frame.
#' @param current1 A vector of column names that are used
#' @param new New name.
#' @param KeepOriginal Defaults to TRUE to keep the original value.
#' If FALSE then the values of 'current' are not preserved.
#' @param mapping_table_name Default NULL. If there is an existing mapping table
#' must be read from the name in the folder
#' @param mapping_table_object Defaults to NULL. If a mapping
#' table must be read.
#' @family mapping
#' @return A new data frame with new values based on the 'mapping tables'.
#' @export
mapping_translate_n <- function(df, currents, new, mapping_table_name = NULL, mapping_table_object = NULL, KeepOriginal = TRUE) {

  #'* INFO* Start with checks and errors

  # If a column already exists with the name of the new variable name
  if (new %in% colnames(df)) {
    stop("the specified new column name already exists in the specified dataframe")
  }

  # Load mappping table, if possible
  if (!is.null(mapping_table_object)) {
    mapping_table <- mapping_table_object
  } else {
    # Deduce location of mapping table based on env var and given reference
    if (!is.null(mapping_table_name)) {
      mapping_table_file_path <- paste0(Sys.getenv("MAP_TABLE_DIR"), mapping_table_name, ".csv")

    } else {
      mapping_table_file_path <- paste0(Sys.getenv("MAP_TABLE_DIR"),
                                        "Mapping_",
                                        paste(currents, collapse = "_and_"),
                                        new,
                                        ".csv",
                                        sep = "")
    }

    if (file.exists(mapping_table_file_path) == FALSE) {
      stop(paste("Your mapping table reference leads to a location without a file. The deduced path is: ", mapping_table_file_path, ".\n Please update the reference to the mapping or envvar MAP_TABLE_DIR."))
    }
    mapping_table <- utils::read.csv2(mapping_table_file_path)
  }

  # Check that mapping_table contains the columns "from" and "to". If that
  # is not so, an error message is given
  if (!is.null(mapping_table)) {
    if (length(mapping_table) != length(currents) + 1) {
      stop(paste(
        "mapping table must have one more column than the number of currents.\n",
        "However, mapping table is ", length(mapping_table), " columns long and the number of currents is ", length(currents), ".\n"
      ))
    }

    if (!(names(mapping_table)[length(mapping_table)] == "to")) {
      stop("the last column in the mapping table must be 'to'")
    }
  }

  # Get the colnames in the mapping table apart from to
  froms <- names(mapping_table)[1:length(mapping_table)-1]

  # Create a single string per row by pasting together the values of the columns specified in 'froms'
  combined_values <- apply(mapping_table[froms], 1, paste, collapse = "-")

  # Check if the length of unique combined values is equal to the number of rows in the table
  if (nrow(mapping_table) != length(unique(combined_values))) {
    stop("Mapping table doesn't have combined unique values in the specified fields and is thus ambiguous.")
  }


  #'*INFO* Now apply mapping table

  mapping_table <- mapping_table %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  df <- df %>%
    dplyr::mutate(dplyr::across(all_of(currents), as.character, .names = "__temp__{.col}"))

  currents_temp <- paste0("__temp__", currents)

  # Construct the 'by' argument dynamically
  by_join <- setNames(froms, currents_temp)

  df <- df %>%
    left_join(mapping_table, by = by_join)

  if (KeepOriginal == FALSE) {
    df <- df[ , !names(df) %in% currents]
  }

  colnames(df)[which(names(df) == "to")] <- new

  ## Remove temp columns
  df <- df[, !grepl("^__temp__", names(df)), drop = FALSE]

  return(df)
}



#' mapping fix
#'
#' Translate "wrong" values from a column according to a given mapping
#' table. Only the values that appear in the mapping table in the column "from"
#' are translated to the corresponding "to" value. If a value is not in
#' occurs in the mapping table, the original value is preserved.
#' You can use this function to fix incorrectly encoded values.
#'
#' @param x The vector to be translated.
#' @param mapping_table The mapping table: a data frame with the columns "from"
#' and to". The classes of these columns must be equal to the class of
#' the specified "x".
#' @param mapping_table_name The name of the mapping table in folder Mapping Tables fix/ without .csv
#' @param merge_original If TRUE, original values are also returned
#' if they cannot be mapped. if FALSE only the mapped values will be
#' returned.
#' @family mapping
#' @export
mapping_fix <- function(x, mapping_table = NULL, mapping_table_name = NULL, merge_original = T) {
  from <- to <- NULL
  ## Check that mapping_table contains the columns "from" and "to". If that
  ## is not so, an error message is given

  if(is.null(mapping_table_name) & is.null(mapping_table)){
    stop("no mapping table is given")
  } else if (is.null(mapping_table) & !is.null(mapping_table_name)){
    mapping_table <- vvmover::read_documentation(filename = paste0("Mapping Tables fixes/", mapping_table_name, ".csv"))
  }

  if (!(any(names(mapping_table) == "from") &&
        any(names(mapping_table) == "to"))) {
    stop("mapping_table must contain the columns 'from' and 'to'.")
  }

  if (length(mapping_table$from) != length(unique(mapping_table$from))) {
    stop("There are duplicate values in the column mapping_table$from")
  }

  ## Check if the classes of the vectors match
  if (class(x) != class(mapping_table$from)){
    stop("class of 'x' does not match class of mapping_table$from")
  }
  if (class(x) != class(mapping_table$to)) {
    stop("class of 'x' does not match class of mapping_table$to")
  }

  ## First create a tibble containing only the specified vector "x", if the column
  ## "from"
  Mapping <- tibble::tibble(from = x) %>%
    ## Do a left_join from the mapping table, on the column "from"
    dplyr::left_join(mapping_table, by = "from") %>%
    ## Create a new column where "to" is leading, and if it is NA, it becomes
    ## padded with the original value
    dplyr::mutate(to_from_merged = dplyr::coalesce(to, from))

  ## Return the new column containing the values contained in the mapping table
  ## have been transformed.
  if (merge_original == T) {
    return(Mapping$to_from_merged)
  } else {
    return(Mapping$to)
  }

}


#' mapping Category
#'
#' Analogous to 'mapping_translate', takes a csv from the documentation folder
#' 'mapping tables'. The input of this function is a continuous value, which is mapped
#' is moved to a category based on intervals.
#'
#' @param Data The data frame.
#' @param current Current name.
#' @param new New name.
#' @param mapping_table_input Defaults to NULL. If a mapping
#' table must be read.
#' @param mapping_table_name Default NULL. If there is an existing mapping table
#' must be read from the name in the folder
#' @family mapping
#' @return A new data frame with new values based on the 'mapping tables'.
#' @export
mapping_category <- function(Data,current,new, mapping_table_input = NULL, mapping_table_name = NULL) {

  ## Check that mapping_table contains the columns "lower", "upper" and "category". If that
  ## is not so, an error message is given
  if (!is.null(mapping_table_input)) {
    if (!(any(names(mapping_table_input) == "lower") &&
          any(names(mapping_table_input) == "upper") &&
          any(names(mapping_table_input) == "category"))) {
      stop("mapping_table must contain the columns 'lower', 'upper' and 'category'.")
    }
  }

  Data$CURRENT <- Data[,current]
  Data$CURRENT <- unlist(Data$CURRENT) ## To use data from a tibble

  #If a mapping table is included, there is no need to read a csv file anymore.
  if(!is.null(mapping_table_input)) {
    categories <- mapping_table_input
  } else if(!is.null(mapping_table_name)){
    categories <- utils::read.csv2(paste0(Sys.getenv("MAP_TABLE_DIR"), mapping_table_name, ".csv"))
  } else {
    categories <- utils::read.csv2(paste0(Sys.getenv("MAP_TABLE_DIR"),
                                          "Mapping_",current,"_",new,".csv"))
  }

  boundaries <- append(categories$lower,utils::tail(categories$upper,n=1))
  Data$TO <- cut(Data$CURRENT, boundaries, categories$category,right=F) #labels=categories$category)

  colnames(Data)[which(names(Data) == "TO")] <- new
  Data$CURRENT <- NULL
  return(Data)
}
