#' submissionPanel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_submissionPanel_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h3("Submission panel"),
    wellPanel(
      tags$h4("Reviser data"),
      fluidRow(
        column(3, textInput(ns("revised_by_surname"), "Surname of the reviser:", placeholder = "Sergio")),
        column(3, textInput(ns("revised_by_lastname"), "Lastname of the reviser:", placeholder = "Lopez")),
        column(3, textInput(ns("revised_by_mail"), "Mail of the reviser:", placeholder = "example@institution.com")),
        column(3, textInput(ns("revised_by_institution"), "Name of the institution:", placeholder = "IREC"))
      ),

      tags$h4("Data model file section"),
      fluidRow(
        column(6,
               fileInput(ns("data_file"), "Upload data file:", accept = ".xlsx")
        ),
        column(6,
               selectInput(ns("version"), "Version:", choices = "1.6"))
      ),

      tags$h4("Spatial file section"),
      fluidRow(
        column(6,
               radioButtons(ns("shape_format"), "Spatial file format:",
                            choices = list("Shapefile (.shp)" = "shp", "GeoPackage (.gpkg)" = "gpkg", "WKT or XY(inside .xlsx)" = "wktxy"),
                            selected = "shp")
        ),
        column(6,
               selectInput(ns("num_shapefiles"), "Number of spatial file:", choices = "1")
               #selectInput(ns("shapefile_id"), "Prueba para ver unique values del xlsx", choices = NULL)
               )
      ),

      fluidRow(
        column(6,
               fileInput(ns("shape_file"), "Upload spatial:", accept=c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj", ".cpg", ".gpkg"), multiple=TRUE)
        ),
        column(3,
               selectInput(ns("id_column"), "ID column:", choices = NULL)
        ),
        column(3,
               selectInput(ns("geom_column"), "Geometry column:", choices = NULL)
        ),
      ),

      tags$h4("Data Agreement"),
      fluidRow(
        column(6,
               selectInput(ns("data_agreement_selection"), "Uploaded data agreement?:", choices = c("Yes", "No"))
        ),
        column(6,
               fileInput(ns("data_agreement"), "Upload data agreement file:", accept = ".pdf")
        )
      ),

      # Buttons
      fluidRow(
        column(12,
               div(style = "display: flex; justify-content: center;",
                   actionButton(ns("submit_btn"), "Submit", class = "btn-success", style = "width: 150px;"),
                   actionButton(ns("clear_btn"), "Clear", class = "btn-danger", style = "width: 150px; margin-left: 15px;")
               )
        )
      )
    )
  )
}

#' submissionPanel Server Functions
#'
#' @noRd
#'
mod_submissionPanel_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Observe changes in submission panel
    observeEvent(input$shape_format, {
      #number of shapefiles
      if (input$shape_format == "wktxy") {
        values$num_shapefiles <- 0
        shinyjs::hide("shape_file")
        shinyjs::hide("num_shapefiles")
        shinyjs::hide("id_column")
        shinyjs::hide("geom_column")
      } else {
        values$num_shapefiles <- 1
        updateNumericInput(session, "num_shapefiles", value = values$num_shapefiles, min = values$num_shapefiles)
        shinyjs::show("shape_file")
        shinyjs::show("num_shapefiles")
        shinyjs::show("id_column")
        shinyjs::show("geom_column")
      }
    })

    #Data, make the read here
    observeEvent(input$data_file, {
      if (!is.null(input$data_file)) {

        data_uploaded <- read_data_global(input$data_file)

        # DraftEW validations: structure and values
        draftEW_uploaded <- data_uploaded$DraftEW[-c(1:2), ] #CLEAN

        updateSelectInput(inputId = "shapefile_id", choices = unique(na.omit(draftEW_uploaded$locationAccordingTo)), selected = "") #
      }
    })

    #Shape
    observeEvent(input$shape_file, {
      if (!is.null(input$shape_file)) {

        if (input$shape_format == "shp") {
          shape_uploaded <- read_shapefile(input$shape_file)
        } else if (input$shape_format == "gpkg") {
          shape_uploaded <- read_gpkg_global(input$shape_file)
        }
      }

      updateSelectInput(inputId = "id_column", choices = names(shape_uploaded), selected = "")
      updateSelectInput(inputId = "geom_column", choices = names(shape_uploaded), selected = "")

    })

    observeEvent(input$data_agreement_selection, {
      if (input$data_agreement_selection == "No") {
        shinyjs::hide("data_agreement")
      } else {
        shinyjs::show("data_agreement")
      }
    })

    # Submit Panel observer
    observeEvent(input$submit_btn, {

      uniqueidentifier <<- uuid::UUIDgenerate() #global

      # Clear any previous report text
      report_text <- character()

      output_zip <- ""

      # Submission panel
      submission_panel_check <- submission_panel_test(
        input$revised_by_surname,
        input$revised_by_lastname,
        input$revised_by_mail,
        input$revised_by_institution,
        input$version,
        input$shape_format,
        input$num_shapefiles,
        input$data_file,
        input$shape_file,
        input$id_column,
        input$geom_column,
        input$data_agreement_selection,
        input$data_agreement
      )

      if (!TRUE %in% submission_panel_check) {

        report_text <- paste0("Error on submission, check the following fields:\n", #EMOJI
                              "------------------------------------------\n",
                              paste(submission_panel_check, collapse = "\n"))


        generate_report("Submission report", report_text, "close_btn", "Close", "action")


      } else { # SUCCESS

        output_zip <- paste0(paste(
          input$revised_by_surname,
          input$revised_by_lastname,
          input$revised_by_institution,
          Sys.Date(),
          input$version,
          uniqueidentifier,
          sep = "_"
        ), ".zip")

        output_zip_export <<- output_zip

        report_text <- paste0("Submission Panel Report:\n",
                              "------------------------------------------\n",
                              paste("Name:", input$revised_by_surname, "\n"),
                              paste("Lastname:", input$revised_by_lastname, "\n"),
                              paste("Mail:", input$revised_by_mail, "\n"),
                              paste("Institution:", input$revised_by_institution, "\n"),
                              paste("Data model version:", input$version, "\n"),
                              paste("Submission date:", Sys.Date(), "\n"),
                              paste("Data model file submitted: ", input$data_file$name, "\n"),
                              if (length(input$shape_file) > 0) {
                                paste("Spatial file(s) submitted: ", paste(input$shape_file$name, collapse = ", "), "\n")
                              } else {
                                "No spatial file submitted\n"
                              },
                              if (input$data_agreement_selection == "Yes"){
                                paste("Data agreement submitted: ", paste(input$data_agreement$name, collapse = ", "), "\n")
                              } else {
                                "No data agreement file submitted\n"
                              },
                              paste0("Output ZIP name: ", output_zip, "\n"),
                              "###########################################\n")

        # validate_btn_metadata
        submission_report <- generate_report("Submission report", report_text, "metadata_validate_btn", "Validate Metadata", "action")
        submission_report # --> Final report
        report_text_submission <<- report_text
      }
    })

    # Clear button
    observeEvent(input$clear_btn, {
      session$reload()
    })
  })
}

