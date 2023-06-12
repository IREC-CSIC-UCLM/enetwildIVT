# Project: IVT
#
# server_IVT - Load Data/IVT
#
# Author: Sergio López IVT
###############################################################################
# Download Manual
output$manualLink <- downloadHandler(
  filename = function() {
    "EW-IVT-manual.pdf"
  },
  content = function(file) {
    file.copy("www/manual.pdf", file)
  }
)

# Download Template
output$downloadData <- downloadHandler(
  filename = function() {
    "WLDM_1.6.xlsx"
  },
  content = function(file) {
    file.copy("inst/extdata/WLDM_1.6.xlsx", file)
  }
)

# Cancel Btn
observeEvent(input$close_btn, {
  removeModal()
})

# Dynamic Submit Panel
mod_submissionPanel_server("submissionPanel")

# Validate metadata
observeEvent(input$metadata_validate_btn, {
  mod_metadataValidate_server("submissionPanel")
})

# Validate Data
observeEvent(input$data_validate_btn, {
  mod_dataValidate_server("submissionPanel")
})

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
