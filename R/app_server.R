# Project: enetwildIVT
#
# Author: shevelp(sergio.lopez@uclm.es)
#
# server file: globalFunction, conditional rendering and tabs-server logic
###############################################################################


#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session, dockerVolume  = '/opt/enetwildIVT') {

  options(shiny.maxRequestSize = 1000 * 1024^5) #define new upload maxSize

  # Manual download #
  observeEvent(input$manualLink, {
    showModal(modalDialog(
      tags$iframe(src = "www/enetwildDET-manual.pdf",
                  width = 860, height = 600),
      size = "l", easyClose = TRUE
    ))

  })


  #Executing global.R copy files to dockerVolume
  globalFunction()

  # Admin control using shinyproxy here! 2.0

  # BASE TABSETPANEL
    output$tabPanels <- renderUI({
        tabsetPanel(
          id = "tabs",
          tabPanel("Data Submit tab", mod_submitData_ui("dataSubmit")) #Submit tab
        )
      })

  ################
  # Modules/TABS #
  ################

  # Tab About #
    mod_submitData_server(id = "dataSubmit",
                      dockerVolume = dockerVolume)


}
