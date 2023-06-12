#' Generate report in modal
#'
#' This function generates a report in a modal dialog with a specified title, text, and button.
#' The button type can be either an action button or a download button, based on the value of `btn_type`.
#' The generated report is displayed as pre-formatted text in the modal dialog.
#'
#' @param report_title The title of the report modal dialog.
#' @param report_text The text content of the report.
#' @param btn_id The ID of the button.
#' @param btn_name The label/name of the button.
#' @param btn_type The type of the button ("action" or "download").
#'
#' @importFrom shiny showModal modalDialog withTags pre HTML actionButton downloadButton
#'
#' @examples
#' report_title <- "Report"
#' report_text <- c("Line 1", "Line 2", "Line 3")
#' btn_id <- "btn1"
#' btn_name <- "Download"
#' btn_type <- "download"
#' generate_report(report_title, report_text, btn_id, btn_name, btn_type)
#'
#' @export
generate_report <- function(report_title, report_text, btn_id, btn_name, btn_type) {
  btntype <- ifelse(btn_type == "action", actionButton, downloadButton)
  showModal(
    modalDialog(
      title = report_title,
      withTags(
        pre(
          HTML(paste(report_text, collapse = "<br>"))
        )
      ),
      easyClose = TRUE,
      footer = btntype(btn_id, btn_name, class = "btn-primary")
    )
  )
}

#' Generate data summary
#'
#' This function generates a summary of the data by concatenating unique values for each column.
#' The summary text includes the column name followed by the unique non-missing values separated by commas.
#' Columns with only missing values are excluded from the summary.
#'
#' @param data The input data frame.
#'
#' @return A character string containing the data summary.
#'
#' @examples
#' data <- data.frame(
#'   A = c(1, 2, 3),
#'   B = c("x", "y", "z"),
#'   C = c(NA, "a", NA)
#' )
#' summary_text <- data_summary(data)
#' cat(summary_text)
#'
#' @export
data_summary <- function(data) {
  cols <- colnames(data)
  summary_text <- ""
  for (col in cols) {
    col_data <- unique(data[, col])
    if (!all(is.na(col_data))) {
      summary_text <- paste0(summary_text, col, ": ", paste(na.omit(col_data), collapse = ", "), "\n")
    }
  }
  return(summary_text)
}


# Define reactive values!
values <- reactiveValues(
  num_shapefiles = 1,
  format_selected = "shp",
  data_uploaded = NULL,
  shape_uploaded = NULL
)
