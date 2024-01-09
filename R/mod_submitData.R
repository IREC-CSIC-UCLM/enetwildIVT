#' submitData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_submitData_ui <- function(id){
  ns <- NS(id)
  tagList(

    ################################
    # Introduction and WLDM download
    ################################

    #style
    tags$div(

      style = "margin-left: 50px; margin-right: 50px;",

      #welcome
      tags$h1("Thank you for considering becoming an ENETWILD data provider!"),

      #intro
      tags$p("Integrated Validation Tool is an app that enables users to
                  validate data for the ENETWILD project, which involves the collection of fauna information.
                  The app ensures compliance with Darwin Core guidelines for post-processing.
                  The tool consists of several modules that allow users to upload and validate data,
                  including metadata and spatial data. The app provides detailed reports on the validation results,
                  highlighting any errors or issues that need to be addressed.
                  The user-friendly interface makes it easy for users to navigate and understand the validation process,
                  making it an essential tool for researchers and data managers working on the ENETWILD project."),

      tags$p("Once the validation checks are completed, the user will receive a .zip file containing the validated data,
                  as well as a text file with information for data coordinators on how to process the data.
                  This allows for a streamlined and efficient workflow, ensuring that data collected for the ENETWILD project
                  meets the highest quality standards."),

      tags$p("Additionally, in order to work with this app, it is necessary to use a specific data model,
                  which is downloadable from the app. Once the Metadata and Data sheets have been filled out,
                  they can be uploaded to the app, which will perform the validation as specified.
                  This ensures that the data follows the necessary standards for the project and can be properly processed.
                  Download the last version of the Wild Life Data Model (WLDM) here:"),

      br(),

      #download template
      tags$div(style = "text-align: center;",
               downloadButton("downloadData", label = "Download template (WLDM)", class = "btn-primary")
      ),

    ),


    ##################
    # Submission panel
    ##################

    tags$h3("Submission panel"),
    wellPanel(

      # Reviewer info
      tags$h4("Reviewer info"),
      fluidRow(
        column(3, textInput(ns("revised_by_surname"),
                            "Name of the data reviewer:",
                            placeholder = "Sergio")),
        column(3, textInput(ns("revised_by_lastname"),
                            "Lastname of data reviewer:",
                            placeholder = "Lopez")),
        column(3, textInput(ns("revised_by_mail"),
                            "Mail of reviewer:",
                            placeholder = "example@institution.com")),
        column(3, textInput(ns("revised_by_institution"),
                            "Name of the institution from the reviewer:",
                            placeholder = "IREC"))
      ),

      # WLDM update
      tags$h4("WLDM upload"),
      fluidRow(
        column(6,
               fileInput(ns("data_file"), "Upload data file:", accept = ".xlsx")
        )
      ),

      # Spatial file
      tags$h4("Associated spatial file upload"),
      fluidRow(
        column(6,
               radioButtons(ns("shape_format"), "Spatial file format:",
                            choices = list("Shapefile (.shp)" = "shp",
                                           "GeoPackage (.gpkg)" = "gpkg",
                                           "WKT or XY(inside .xlsx)" = "wktxy"),
                            selected = "shp")
        )
      ),

      uiOutput(ns("shapefile_inputs")), #?

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


      # DSA file
      tags$h4("Data Agreement"),
      fluidRow(
        column(6,
               selectInput(ns("data_agreement_selection"), "Uploaded data agreement?:", choices = c("Yes", "No"))
        ),
        column(6,
               fileInput(ns("data_agreement"), "Upload data agreement file:", accept = ".pdf")
        )
      ),

      # Action buttons
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

#' submitData Server Functions
#'
#' @noRd
mod_submitData_server <- function(id,dockerVolume){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ##########
    # General
    ##########

    # Reactive values to use data between observers
    values <- reactiveValues(data_uploaded = NULL,
                             uniqueidentifier = NULL,
                             shape_uploaded = NULL,
                             shape_uploaded_raw = NULL,
                             draftEW_uploaded_export = NULL,
                             data_uploaded_raw = NULL)

    # Close btn from the modal
    observeEvent(input$close_btn, {
      removeModal()
    })

    # Download btn WLDM
    output$downloadData <- downloadHandler(
      filename = function() {
        "WLDM.xlsx"
      },
      content = function(file) {
        file.copy("inst/extdata/WLDM.xlsx", file)
      }
    )

    ##############################
    # Changes on submission panel
    ##############################

    # Shape format observer
    observeEvent(input$shape_format, {
      if (input$shape_format == "wktxy") {
        shinyjs::hide("shape_file")
        shinyjs::hide("num_shapefiles")
        shinyjs::hide("id_column")
        shinyjs::hide("geom_column")
      } else {
        shinyjs::show("shape_file")
        shinyjs::show("num_shapefiles")
        shinyjs::show("id_column")
        shinyjs::show("geom_column")
      }
    })

    # DSA observer
    observeEvent(input$data_agreement_selection, {
      if (input$data_agreement_selection == "No") {
        shinyjs::hide("data_agreement")
      } else {
        shinyjs::show("data_agreement")
      }
    })





    ###############
    # Data upload
    ###############

    # WLDM read
    observeEvent(input$data_file, {
      if (!is.null(input$data_file)) {
        values$data_uploaded <- read_data_global(input$data_file)
      }
    })

    # Spatial file read
    observeEvent(input$shape_file, {
      if (!is.null(input$shape_file)) {

        if (input$shape_format == "shp") {
          shape_uploaded <- read_shapefile(input$shape_file)
        } else if (input$shape_format == "gpkg") {
          shape_uploaded <- read_gpkg_global(input$shape_file)
        }
      }

      values$shape_uploaded <- shape_uploaded

      updateSelectInput(inputId = "id_column", choices = names(shape_uploaded), selected = "")
      updateSelectInput(inputId = "geom_column", choices = names(shape_uploaded), selected = "")

    })





    #############################
    # Submission panel validation
    #############################

    # Clear button
    #--------------
    observeEvent(input$clear_btn, {
      session$reload()
    })

    # Submit Button
    #--------------
    observeEvent(input$submit_btn, {

      # UUID each time user submits
      values$uniqueidentifier <- uuid::UUIDgenerate()

      # Clear any previous report text
      report_text_submissionPanel <- character()

      # Submission panel check
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

      # Fail
      if (!TRUE %in% submission_panel_check) {

        report_text_submissionPanel <- paste0(
          "Error on submission, check the following fields:\n",
          "------------------------------------------\n",
          paste(submission_panel_check, collapse = "\n"))

        # Gen fail report
        generate_report("Submission report",
                        report_text_submissionPanel,
                        ns("close_btn"),
                        "Close",
                        "action")

      } else { # Pass

        report_text_submissionPanel <- paste0("Submission Panel Report:\n",
                              "------------------------------------------\n",
                              paste("Name:", input$revised_by_surname, "\n"),
                              paste("Lastname:", input$revised_by_lastname, "\n"),
                              paste("Mail:", input$revised_by_mail, "\n"),
                              paste("Institution:", input$revised_by_institution, "\n"),
                              paste("Data model version:", input$version, "\n"),
                              paste("Submission date:", Sys.Date(), "\n"),
                              paste("Data model file submitted: ", input$data_file$name, "\n"),

                              # If shapefile uploaded
                              if (length(input$shape_file) > 0) {
                                paste("Spatial file(s) submitted: ",
                                      paste(input$shape_file$name, collapse = ", "), "\n")
                              } else {
                                "No spatial file submitted\n"
                              },

                              # If DSA uploaded
                              if (input$data_agreement_selection == "Yes"){
                                paste("Data agreement submitted: ",
                                      paste(input$data_agreement$name, collapse = ", "), "\n")
                              } else {
                                "No data agreement file submitted\n"
                              },
                              "###########################################\n")

        # Gen Pass report
        submission_report <- generate_report("Submission report",
                                             report_text_submissionPanel,
                                             ns("metadata_validate_btn"),
                                             "Validate Metadata",
                                             "action")
        submission_report
      }
    })






    #######################
    # Metadata validation
    ######################

    # Validate metadata Button
    #--------------
    observeEvent(input$metadata_validate_btn, {

      # Closing modal
      removeModal()

      # Clear any previous report text
      report_text_metadata <- character()

      # Metadata
      metadata_uploaded <- as.data.frame(values$data_uploaded$Metadata)
      # Rownames code col (CARE!)
      rownames(metadata_uploaded) <- metadata_uploaded$code

      # Validate metadata
      metadata_check <- validate_metadata(metadata_uploaded, readRDS("inst/extdata/metadata_dict.rds"))

      # Fail
      if (!TRUE %in% metadata_check) {

        report_text_metadata <- paste0(
          "Error on metadata sheet, check the following errors:\n",
          "------------------------------------------\n",
          paste(metadata_check, collapse = "\n"))

        # Gen Fail report
        generate_report("Metadata report",
                        report_text_metadata,
                        ns("close_btn"),
                        "Close",
                        "action")

      } else { #Pass

        # Create summary of metadata values
        metadata_summary <- paste(na.omit(paste(rownames(metadata_uploaded[complete.cases(metadata_uploaded),]),
                                                metadata_uploaded[complete.cases(metadata_uploaded),"fill"], sep = ": ")),
                                  collapse = "\n")

        report_text_metadata <- paste0("Metadata Report:\n",
                              "------------------------------------------\n",
                              paste0(metadata_summary, "\n"),
                              "###########################################\n")

        # Gen Pass report
        metadata_report <- generate_report("Metadata report",
                                           report_text_metadata,
                                           ns("data_validate_btn"),
                                           "Validate Data",
                                           "action")
        metadata_report

        # Generate df for .csv metadata
        #------------------------------

        # Transform the metadata and add UUID
        metadata_uploaded <- as.data.frame(t(metadata_uploaded))
        names(metadata_uploaded) <- as.matrix(metadata_uploaded[1, ])
        metadata_uploaded <-  dplyr::filter(metadata_uploaded, row.names(metadata_uploaded) %in% 'fill')
        metadata_uploaded$datasetID <- values$uniqueidentifier

        #Data Agreement inclusion
        metadata_uploaded$dsaStatus <- input$data_agreement_selection

        if (input$data_agreement_selection == "Yes") {
          metadata_uploaded$dsaFile <- input$data_agreement$name
          dsa_file_raw <<- input$data_agreement$datapath
        } else {
          metadata_uploaded$dsaFile <- NA
        }

        metadata_uploaded_export <- metadata_uploaded

      }

    })





    ###################
    # Data validation
    ##################

    # Validate Data Button
    #---------------------
    observeEvent(input$data_validate_btn, {

      # Closing modal
      removeModal()

      # Clear any previous report text
      report_text_data <- character()

      # DraftEW validations: structure and values
      draftEW_uploaded <- as.data.frame(values$data_uploaded$DraftEW)

      # Clean info and example rows
      draftEW_uploaded <- draftEW_uploaded[-c(1:2), ]

      # Sumcols and filtering
      sumcols <- c("type",
                   "locationID",
                   "locationType",
                   "countryCode",
                   "areaType",
                   "timeLevel",
                   "yearBeginDate",
                   "yearEndDate",
                   "dataTime",
                   "species")

      hasValue <- apply(draftEW_uploaded[, sumcols], 1, function(row) any(!is.na(row)))
      draftEW_uploaded <- draftEW_uploaded[hasValue, ]

      # Validation
      data_check <- validate_data(draftEW_uploaded, readRDS("inst/extdata/draftEW_dict.rds"))

      # Data Summary
      elementToRemove <- "locationID"
      sumcols <- setdiff(sumcols, elementToRemove)
      data_summary <- data_summary(dplyr::select(draftEW_uploaded,sumcols))

      # Spatial checks
      if (!input$shape_format == "wktxy") {

        # Adding geom
        draftEW_uploaded <- fromshapetowkt(draftEW_uploaded, shape_uploaded, "locationID", input$id_column, input$geom_column)

        # Checking geom
        spatial_union_check <- validate_match(draftEW_uploaded, shape_uploaded, "locationID", input$id_column)

        # Appending reports
        data_check <- append(data_check, spatial_union_check)

        #global shape
        shape_uploaded$datasetID <- values$uniqueidentifier
        values$shape_uploaded_raw <- sf::st_as_sf(shape_uploaded)

      }

      if (any(FALSE %in% data_check)) {
        report_text <- Filter(is.character, data_check)
        generate_report("Data Report", report_text, ns("close_btn"), "Close", "action")
      } else {
        warning_msg <- Filter(is.character, data_check)
        report_text <- paste0("Data Report:\n",
                              "------------------------------------------\n",
                              data_summary,
                              if (length(warning_msg) > 0) {
                                paste("Warning: ", warning_msg, "\n")
                              } else {
                                paste("No warning\n")
                              },
                              "###########################################")

        generate_report("Data sheet report", report_text, ns("download_zip_btn"), "Download outputs", "down")
        report_text_data <<- report_text
      }

      # Adding UUID
      draftEW_uploaded$datasetID <- values$uniqueidentifier
      values$draftEW_uploaded_export <- draftEW_uploaded

      #Raw
      values$data_uploaded_raw <- input$data_file$datapath

      output_zip_export <- paste0(values$uniqueidentifier)

    })






    ####################
    # Output generation
    ####################

    # Download ZIP
    output$download_zip_btn <- downloadHandler(
      filename = function() {
        paste0(output_zip_export) # Zip name
      },
      content = function(file) {
        temp_zip <- tempfile(fileext = ".zip")

        # Creating raw/ directory and copying files
        if (!dir.exists("raw")) {
          dir.create("raw")
        }

        # Raws
        file.copy(data_uploaded_raw, "raw/wldm_raw.xlsx")

        # Check if shape_uploaded_raw exists and is an sf object
        if (exists("shape_uploaded_raw") && isTRUE(inherits(shape_uploaded_raw, "sf"))) {
          sf::st_write(shape_uploaded_raw, "raw/spatial.gpkg", append = FALSE)
        }

        if (exists("dsa_file_raw")) {
          file.copy(dsa_file_raw, "dsa.pdf")
        }

        # Creating CSV files and reports_resume.txt
        sf::st_write(draftEW_uploaded_export, "data.gpkg")
        write.csv(metadata_uploaded_export, "metadata.csv", row.names = FALSE)
        reports_resume <- paste(report_text_submission, report_text_metadata, report_text_data, collapse = "\n")
        writeLines(reports_resume, "reports_resume.txt")

        # Adding Files to ZIP
        zip(zipfile = temp_zip, files = c("data.gpkg", "metadata.csv", "reports_resume.txt", "raw"))

        if (exists("dsa_file_raw")) {
          zip(zipfile = temp_zip, files = "dsa.pdf")
        }

        # Removing temp files and directories
        file.remove("data.gpkg", "metadata.csv", "reports_resume.txt")


        if (exists("dsa_file_raw")) {
          file.remove("dsa.pdf")
        }

        unlink("raw", recursive = TRUE)

        # Copying zip to output file
        file.copy(temp_zip, file)
      }
    )




  })
}
