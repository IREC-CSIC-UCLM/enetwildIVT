#' Submission Panel Test
#'
#' This function performs a validation test on the submission panel form inputs.
#' It checks for the presence of required fields and returns a list of missing fields if any are found.
#' If all required fields are provided, it returns TRUE.
#'
#' @param revised_by_surname Surname of the reviser.
#' @param revised_by_lastname Lastname of the reviser.
#' @param revised_by_mail Email of the reviser.
#' @param revised_by_institution Name of the institution.
#' @param version Version.
#' @param shape_format Shapefile format.
#' @param num_shapefiles Number of shapefiles.
#' @param data_file Data file.
#' @param shape_file Spatial file.
#' @param id_column ID column from the spatial file.
#' @param geom_column Geometry column from the spatial file.
#' @param data_agreement_selection Data agreement selection ("Yes" or "No").
#' @param data_agreement Data agreement file.
#'
#' @return A character vector containing the missing fields, or TRUE if all required fields are provided.
#'
#' @examples
#' revised_by_surname <- "Doe"
#' revised_by_lastname <- "John"
#' revised_by_mail <- "johndoe@example.com"
#' revised_by_institution <- "Example Institution"
#' version <- "1.0"
#' shape_format <- "wktxy"
#' num_shapefiles <- 2
#' data_file <- "data.csv"
#' shape_file <- "shapefile.zip"
#' id_column <- "ID"
#' geom_column <- "Geometry"
#' data_agreement_selection <- "Yes"
#' data_agreement <- "agreement.pdf"
#' result <- submission_panel_test(revised_by_surname, revised_by_lastname, revised_by_mail, revised_by_institution, version, shape_format, num_shapefiles, data_file, shape_file, id_column, geom_column, data_agreement_selection, data_agreement)
#' if (isTRUE(result)) {
#'   # All required fields are provided
#'   print("Submission panel is valid.")
#' } else {
#'   # Missing fields
#'   print(paste("Missing fields:", result))
#' }
#'
#' @export
submission_panel_test <- function(revised_by_surname, revised_by_lastname,
                                  revised_by_mail, revised_by_institution,
                                  version, shape_format, num_shapefiles,
                                  data_file, shape_file, id_column, geom_column,
                                  data_agreement_selection, data_agreement) {

  error_list <- c()

  if (revised_by_surname == "") {
    error_list <- c(error_list, "Surname of the reviser")
  }

  if (revised_by_lastname == "") {
    error_list <- c(error_list, "Lastname of the reviser")
  }

  if (revised_by_mail == "") {
    error_list <- c(error_list, "email of the reviser")
  }

  if (revised_by_institution == "") {
    error_list <- c(error_list, "Name of the institution")
  }

  if (version == "") {
    error_list <- c(error_list, "Version")
  }

  if (is.null(shape_format)) {
    error_list <- c(error_list, "Shapefile format")
  }

  if (shape_format == "wktxy") {
    if (is.null(data_file)) {
      error_list <- c(error_list, "Upload data file")
    }

  } else {

    if (is.null(shape_file)) {
      error_list <- c(error_list, "Upload spatial file")
    }
    if (id_column == ""){
      error_list <- c(error_list, "Select ID column from the spatial file")
    }
    if (geom_column == ""){
      error_list <- c(error_list, "Select geometry column from the spatial file")
    }
    if (is.null(data_file)) {
      error_list <- c(error_list, "Upload data file")
    }
  }


  if (data_agreement_selection == "Yes") {
    if (is.null(data_agreement)) {
      error_list <- c(error_list, "Upload data agreement file")
    }
  }

  if (length(error_list) > 0) {
    return(error_list)
  } else {
    return(TRUE)
  }

}

#' Check Data Structure
#'
#' This function checks the structure of a data frame against a given vocabulary.
#' It verifies the presence of mandatory rows and columns in the data frame.
#' If any required rows or columns are missing, it returns an error message.
#' If the data frame structure is valid, it returns TRUE.
#'
#' @param df The input data frame.
#' @param vocabulary The vocabulary containing the structure requirements.
#'
#' @return TRUE if the data structure is valid, or an error message indicating the missing rows or columns.
#'
#' @examples
#' df <- data.frame(
#'   A = c(1, 2, 3),
#'   B = c("x", "y", "z")
#' )
#' vocabulary <- list(
#'   structure = list(
#'     mandatoryRows = c("row1", "row2"),
#'     mandatoryCols = c("A", "C")
#'   )
#' )
#' result <- check_structure(df, vocabulary)
#' if (isTRUE(result)) {
#'   # Data structure is valid
#'   print("Data structure is valid.")
#' } else {
#'   # Missing rows or columns
#'   print(result)
#' }
#'
#' @export
check_structure <- function(df, vocabulary) {

  mandatory_rows <- vocabulary$structure$mandatoryRows
  mandatory_cols <- vocabulary$structure$mandatoryCols

  #misscols
  missing_cols <- mandatory_cols[!mandatory_cols %in% colnames(df)]

  #missrows
  missing_rows <- mandatory_rows[!mandatory_rows %in% rownames(df)]

  if (length(missing_cols) > 0) {
    msg <- paste0("Error: Missing the following required columns: ", paste(missing_cols, collapse = ", "), "\n")
    return(msg)
  }

  if (length(missing_rows) > 0) {
    msg <- paste0("Error: Missing the following required rows: ", paste(missing_rows, collapse = ", "), "\n")
    return(msg)
  }

  return(TRUE)
}


#' Check Completeness of Rows
#'
#' This function checks the completeness of rows in a data frame based on a given vocabulary.
#' It verifies if the specified columns in each row have missing values (NA).
#' If any rows are found to be incomplete, it returns an error message indicating the rows and columns with missing values.
#' If all rows are complete, it returns TRUE.
#'
#' @param data The input data frame.
#' @param vocabulary The vocabulary containing the columns to check.
#'
#' @return TRUE if all rows are complete, or an error message indicating the rows and columns with missing values.
#'
#' @examples
#' data <- data.frame(
#'   A = c(1, NA, 3),
#'   B = c("x", "y", NA),
#'   C = c("a", "b", "c")
#' )
#' vocabulary <- list(
#'   index = list(
#'     checkCols = c("A", "B"),
#'     checkRows = c(1, 2, 3)
#'   )
#' )
#' result <- check_rows_complete(data, vocabulary)
#' if (isTRUE(result)) {
#'   # All rows are complete
#'   print("All rows are complete.")
#' } else {
#'   # Incomplete rows
#'   print(result)
#' }
#'
#' @export
check_rows_complete <- function(data, vocabulary) {

  check_cols <- vocabulary$index$checkCols
  check_rows <- vocabulary$index$checkRows
  error_rows <- c()

  for (row in check_rows) {
    incomplete <- FALSE
    for (col in check_cols) {
      if (is.na(data[row, col])) {
        error_rows <- c(error_rows, row)
        incomplete <- TRUE
        break
      }
    }
    if (!incomplete) {
      next
    }
  }

  if (length(error_rows) > 0) {
    msg <- paste0("The following rows have incomplete values in the column/s (", paste(check_cols, collapse = ", "), "): ", paste(error_rows, collapse = ", "))
    return(msg)
  }

  return(TRUE)
}

#' Check Vocabulary Metadata
#'
#' This function checks the metadata of a data frame against a given vocabulary.
#' It verifies if the values in specified columns of each row are allowed according to the vocabulary.
#' If any value is found to be not allowed, it returns an error message indicating the row, column, and allowed values.
#' If all values are allowed, it returns TRUE.
#'
#' @param data The input data frame.
#' @param vocabulary The vocabulary containing the allowed values for each column.
#'
#' @return TRUE if all values are allowed, or an error message indicating the row, column, and allowed values.
#'
#' @examples
#' data <- data.frame(
#'   A = c("x", "y", "z"),
#'   B = c("a", "b", "c")
#' )
#' vocabulary <- list(
#'   index = list(
#'     checkCols = c("A", "B")
#'   ),
#'   vocabulary = list(
#'     A = c("x", "y"),
#'     B = c("a", "c")
#'   )
#' )
#' result <- check_vocabulary_metadata(data, vocabulary)
#' if (isTRUE(result)) {
#'   # All values are allowed
#'   print("All values are allowed.")
#' } else {
#'   # Disallowed values
#'   print(result)
#' }
#'
#' @export
check_vocabulary_metadata <- function(data, vocabulary) {

  check_cols <- vocabulary$index$checkCols

  for (row_name in names(vocabulary$vocabulary)) {
    allowed_values <- vocabulary$vocabulary[[row_name]]
    row_value <- data[row_name, check_cols]
    if (!is.na(row_value) && !(row_value %in% allowed_values)) {
      error_message <- paste0("The value '", row_value, "' is not allowed for '", row_name, "'. Allowed values are: ", paste(allowed_values, collapse = ", "))
      return(error_message)
    }
  }

  return(TRUE)
}

#' Validate Metadata
#'
#' This function validates the metadata of a data frame based on a given vocabulary.
#' It performs three validation checks using the following functions:
#' - `check_structure`: Checks the structure of the metadata.
#' - `check_rows_complete`: Checks for completeness of rows in the metadata.
#' - `check_vocabulary_metadata`: Checks the metadata against the allowed values specified in the vocabulary.
#'
#' If all validation checks pass successfully, it returns TRUE.
#' If any of the validation checks fail, it returns an error message summarizing the failed checks.
#'
#' @param data The input data frame containing the metadata.
#' @param vocab The vocabulary containing the structure, mandatory values, and allowed values for the metadata.
#'
#' @return TRUE if all validation checks pass, or an error message summarizing the failed checks.
#'
#' @examples
#' data <- data.frame(
#'   A = c(1, 2, 3),
#'   B = c("x", "y", "z"),
#'   C = c("a", "b", "c")
#' )
#' vocab <- list(
#'   structure = list(
#'     mandatoryRows = c("A"),
#'     mandatoryCols = c("B")
#'   ),
#'   index = list(
#'     checkCols = c("A", "B"),
#'     checkRows = c(1, 2, 3)
#'   ),
#'   vocabulary = list(
#'     A = c(1, 2, 3),
#'     B = c("x", "y")
#'   )
#' )
#' result <- validate_metadata(data, vocab)
#' if (isTRUE(result)) {
#'   # All validation checks passed
#'   print("Metadata is valid.")
#' } else {
#'   # Validation checks failed
#'   print(result)
#' }
#'
#' @export
validate_metadata <- function(data, vocab) {
  # Check metadata structure
  structure_check <- check_structure(data, vocab)

  # Check metadata mandatory values
  mandatory_check <- check_rows_complete(data, vocab)

  # Check metadata dropdowns values (vocabulary)
  vocab_check <- check_vocabulary_metadata(data, vocab)

  # Check if all validation checks passed successfully
  if (structure_check == TRUE && mandatory_check == TRUE && vocab_check == TRUE) {
    return(TRUE)
  } else {
    # Concatenate error messages
    msg <- c()
    if (structure_check != TRUE) {
      msg <- append(msg, structure_check)
    }
    if (mandatory_check != TRUE) {
      msg <- append(msg, mandatory_check)
    }
    if (vocab_check != TRUE) {
      msg <- append(msg, vocab_check)
    }

    return(paste0("Validation checks failed with the following errors: \n", paste(msg, collapse = "\n")))
  }
}


#' Validate Match between Data and Shape
#'
#' This function validates the match between the data and shape files based on the specified ID columns.
#' It checks if all the unique location IDs in the data file have a corresponding ID in the shape file.
#'
#' @param data_uploaded The data frame containing the uploaded data.
#' @param shape_uploaded The data frame containing the uploaded shape file.
#' @param id_column The name of the ID column in the data file.
#' @param id_column_shape The name of the ID column in the shape file.
#'
#' @return A list with two elements:
#'   - The first element is a boolean indicating if the match is valid (TRUE) or not (FALSE).
#'   - The second element is a message explaining the validation result.
#'
#' @examples
#' data <- data.frame(
#'   locationID = c("A", "B", "C", "D"),
#'   value = c(1, 2, 3, 4)
#' )
#' shape <- data.frame(
#'   ID = c("A", "C", "D"),
#'   geometry = c("POINT", "POLYGON", "POLYGON")
#' )
#' result <- validate_match(data, shape, "locationID", "ID")
#' if (result[[1]]) {
#'   # Match is valid
#'   print(result[[2]])
#' } else {
#'   # Match is not valid
#'   print(result[[2]])
#' }
#'
#' @export
validate_match <- function(data_uploaded, shape_uploaded, id_column, id_column_shape) {
  # Get unique values of locationID in data_uploaded
  data_locationIDs <- unique(data_uploaded[[id_column]])

  # Get unique values of ID column in shape_uploaded
  shape_IDs <- unique(shape_uploaded[[id_column_shape]])

  # Check if there is a match between the two
  if (all(data_locationIDs %in% shape_IDs)) {
    correct_message <- "All of the locations ID have a corresponding WKT"
    return(list(TRUE, correct_message))
  } else {
    missing_locationIDs <- setdiff(data_locationIDs, shape_IDs)
    if (length(missing_locationIDs) == length(data_locationIDs)) {
      error_message <- "None of the locationIDs in the uploaded data match with the selected ID column in the shapefile."
      return(list(FALSE, error_message))
    } else {
      warning_message <- paste("Some locationIDs in the uploaded data do not have a corresponding shape in the selected shapefile. These locationIDs are:",
                               paste(missing_locationIDs, collapse = ", "))
      return(list(TRUE, warning_message))
    }
  }
}

#' Check Completeness of Columns
#'
#' This function checks the completeness of specified columns in a data frame.
#' It identifies any columns that contain missing values (NA).
#'
#' @param data The data frame to be checked.
#' @param vocabulary The vocabulary containing the index of columns to be checked.
#'
#' @return A boolean indicating if all specified columns are complete (TRUE) or if there are incomplete columns (FALSE).
#' If there are incomplete columns, a message listing the columns with missing values is returned.
#'
#' @examples
#' data <- data.frame(
#'   ID = c(1, 2, NA, 4),
#'   Name = c("John", "Jane", "Tom", "Mary"),
#'   Age = c(25, 30, NA, 40)
#' )
#' vocabulary <- list(
#'   index = list(
#'     checkCols = c("ID", "Age")
#'   )
#' )
#' result <- check_columns_complete(data, vocabulary)
#' if (result) {
#'   # All columns are complete
#'   print("All columns are complete.")
#' } else {
#'   # Incomplete columns exist
#'   print(result)
#' }
#'
#' @export
check_columns_complete <- function(data, vocabulary) {
  error_cols <- c()
  check_cols <- vocabulary$index$checkCols

  for (col in check_cols) {
    if (any(is.na(data[, col]))) {
      error_cols <- c(error_cols, col)
    }
  }

  if (length(error_cols) > 0) {
    msg <- paste0("The following columns have incomplete values: ", paste(error_cols, collapse = ", "))
    return(msg)
  }

  return(TRUE)
}


#' Check Vocabulary Data
#'
#' This function checks if the values in specified columns of a data frame are allowed based on a vocabulary.
#' It compares the values in each column with the allowed values specified in the vocabulary.
#' If any values are found that are not allowed, an error message is returned.
#'
#' @param data The data frame to be checked.
#' @param vocabulary The vocabulary containing the allowed values for each column.
#'
#' @return A boolean indicating if all values in the specified columns are allowed (TRUE) or if there are disallowed values (FALSE).
#' If there are disallowed values, an error message is returned indicating the column and the allowed values.
#'
#' @examples
#' data <- data.frame(
#'   ID = c(1, 2, 3, 4),
#'   Status = c("Active", "Inactive", "Unknown", "Active")
#' )
#' vocabulary <- list(
#'   vocabulary = list(
#'     Status = c("Active", "Inactive")
#'   )
#' )
#' result <- check_vocabulary_data(data, vocabulary)
#' if (result) {
#'   # All values in specified columns are allowed
#'   print("All values are allowed.")
#' } else {
#'   # Disallowed values exist
#'   print(result)
#' }
#'
#' @export
check_vocabulary_data <- function(data, vocabulary) {
  for (col_name in names(vocabulary$vocabulary)) {
    allowed_values <- vocabulary$vocabulary[[col_name]]
    col_values <- data[[col_name]]

    if (!is.null(col_values) && !all(is.na(col_values))) {
      invalid_values <- col_values[!col_values %in% allowed_values]

      if (length(invalid_values) > 0) {
        error_message <- paste0("Some values in column '", col_name, "' are not allowed. Allowed values are: ", paste(allowed_values, collapse = ", "))
        return(error_message)
      }
    }
  }

  return(TRUE)
}

#' Validate Data
#'
#' This function performs data validation based on a specified vocabulary.
#' It checks the structure, mandatory values, and vocabulary values of the data.
#' If any validation checks fail, an error message is returned indicating the specific errors.
#'
#' @param data The data frame to be validated.
#' @param vocab The vocabulary containing the validation rules and allowed values.
#'
#' @return A boolean indicating if the data passes all validation checks (TRUE) or if there are validation errors (FALSE).
#' If there are validation errors, an error message is returned listing the specific errors.
#'
#' @examples
#' data <- data.frame(
#'   ID = c(1, 2, 3, 4),
#'   Status = c("Active", "Inactive", "Unknown", "Active")
#' )
#' vocabulary <- list(
#'   structure = list(
#'     mandatoryCols = c("ID", "Status")
#'   ),
#'   index = list(
#'     checkCols = c("ID")
#'   ),
#'   vocabulary = list(
#'     Status = c("Active", "Inactive")
#'   )
#' )
#' result <- validate_data(data, vocabulary)
#' if (result) {
#'   # Data passed all validation checks
#'   print("Data is valid.")
#' } else {
#'   # Validation errors exist
#'   print(result)
#' }
#'
#' @export
validate_data <- function(data, vocab) {
  # Check data structure
  data_structure_check <- check_structure(data, vocab)

  # Check data mandatory values
  data_mandatory_check <- check_columns_complete(data, vocab)

  # Check data dropdowns values (vocabulary)
  data_vocab_check <- check_vocabulary_data(data, vocab)

  # Check if all validation checks passed successfully
  if (data_structure_check == TRUE && data_mandatory_check == TRUE && data_vocab_check == TRUE) {
    return(TRUE)
  } else {
    # Concatenate error messages
    msg <- c()
    if (data_structure_check != TRUE) {
      msg <- append(msg, data_structure_check)
    }
    if (data_mandatory_check != TRUE) {
      msg <- append(msg, data_mandatory_check)
    }
    if (data_vocab_check != TRUE) {
      msg <- append(msg, data_vocab_check)
    }

    return(list(FALSE, paste0("Validation checks failed with the following errors: \n", paste(msg, collapse = "\n"))))
  }
}

