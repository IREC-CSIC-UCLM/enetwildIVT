#' dataValidate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dataValidate_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' dataValidate Server Functions
#'
#' @noRd
mod_dataValidate_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    removeModal()

    report_text <- character()

    data_uploaded <- read_data_global(input$data_file)

    # DraftEW validations: structure and values
    draftEW_uploaded <- data_uploaded$DraftEW[-c(1:2), ] #CLEAN

    # Sumcols and filtering
    sumcols <- c("type", "locationID", "locationType", "countryCode", "areaType", "timeLevel", "yearBeginDate", "yearEndDate", "dataTime", "species")
    hasValue <- apply(draftEW_uploaded[, sumcols], 1, function(row) any(!is.na(row)))
    draftEW_uploaded <- draftEW_uploaded[hasValue, ]

    # Validation
    data_check <- validate_data(draftEW_uploaded, readRDS("inst/extdata/draftEW_dict.rds"))

    # Data Summary
    elementToRemove <- "locationID"
    sumcols <- setdiff(sumcols, elementToRemove)
    data_summary <- data_summary(dplyr::select(draftEW_uploaded,sumcols))

    #spatial checks
    if (!input$shape_format == "wktxy") {

      if (input$shape_format == "shp") {
        shape_uploaded <- read_shapefile(input$shape_file)
      } else if (input$shape_format == "gpkg") {
        shape_uploaded <- read_gpkg_global(input$shape_file)
      }

      # adding geom #puede estar aqui?
      draftEW_uploaded <- fromshapetowkt(draftEW_uploaded, shape_uploaded, "locationID", input$id_column, input$geom_column)

      # checking geom
      spatial_union_check <- validate_match(draftEW_uploaded, shape_uploaded, "locationID", input$id_column)

      # Appending reports
      data_check <- append(data_check, spatial_union_check)

      #global shape
      shape_uploaded$datasetID <- uniqueidentifier
      shape_uploaded_raw <<- sf::st_as_sf(shape_uploaded)

    }

    if (any(FALSE %in% data_check)) {
      report_text <- Filter(is.character, data_check)
      generate_report("Data Report", report_text, "close_btn", "Close", "action")
    } else {
      warning_msg <- Filter(is.character, data_check)
      print(warning_msg)

      report_text <- paste0("Data Report:\n",
                            "------------------------------------------\n",
                            data_summary,
                            if (length(warning_msg) > 0) {
                              paste("Warning: ", warning_msg, "\n")
                            } else {
                              print("no warning")
                            },
                            "###########################################")

      generate_report("Data sheet report", report_text, "download_zip_btn", "Download outputs", "down")
      report_text_data <<- report_text
    }

    # Adding UUID
    draftEW_uploaded$datasetID <- uniqueidentifier
    draftEW_uploaded_export <<- draftEW_uploaded

    #Raw
    data_uploaded_raw <<- input$data_file$datapath

  })
}
