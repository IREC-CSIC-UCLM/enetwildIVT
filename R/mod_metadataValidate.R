#' metadataValidate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_metadataValidate_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' metadataValidate Server Functions
#'
#' @noRd
mod_metadataValidate_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    removeModal()

    report_text <- character()

    data_uploaded <- read_data_global(input$data_file)

    # Metadata
    metadata_uploaded <- as.data.frame(data_uploaded$Metadata)
    rownames(metadata_uploaded) <- metadata_uploaded$code # --> CARE WITH NAMING

    # Validate metadata
    metadata_check <- validate_metadata(metadata_uploaded, readRDS("inst/extdata/metadata_dict.rds"))

    if (!TRUE %in% metadata_check) {

      report_text <- paste0("Error on metadata sheet, check the following errors:\n", #EMOJI
                            "------------------------------------------\n",
                            paste(metadata_check, collapse = "\n"))

      generate_report("Metadata report", report_text, "close_btn", "Close", "action")

    } else { #SUCCESS

      # Create summary of metadata values
      metadata_summary <- paste(na.omit(paste(rownames(metadata_uploaded[complete.cases(metadata_uploaded),]),
                                              metadata_uploaded[complete.cases(metadata_uploaded),"fill"], sep = ": ")),
                                collapse = "\n")

      report_text <- paste0("Metadata Report:\n",
                            "------------------------------------------\n",
                            paste0(metadata_summary, "\n"),
                            "###########################################\n")

      metadata_report <- generate_report("Metadata report", report_text, "data_validate_btn", "Validate Data", "action")
      metadata_report # --> Final report
      report_text_metadata <<- report_text

    }

    #transform the metadata and add UUID
    metadata_uploaded <- as.data.frame(t(metadata_uploaded))
    names(metadata_uploaded) <- as.matrix(metadata_uploaded[1, ])
    metadata_uploaded <-  dplyr::filter(metadata_uploaded, row.names(metadata_uploaded) %in% 'fill')
    metadata_uploaded$datasetID <- uniqueidentifier

    #Data Agreement inclusion
    metadata_uploaded$dsaStatus <- input$data_agreement_selection

    if (input$data_agreement_selection == "Yes") {
      metadata_uploaded$dsaFile <- input$data_agreement$name
      dsa_file_raw <<- input$data_agreement$datapath
    } else {
      metadata_uploaded$dsaFile <- NA
    }

    metadata_uploaded_export <<- metadata_uploaded

  })
}


