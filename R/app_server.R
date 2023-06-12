#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#'
#' @noRd
#'

#en lugar de llamar a las tabs, que las llame como módulos!
app_server <- function(input, output, session) {

  options(shiny.maxRequestSize = 30 * 1024^5) # to let them upload large data

  # Load code for all tabpages
  source(file.path("R/tabs/dataSubmitTab_server.R"), local = TRUE)

  # Show tabpanels
  output$tabPanels <- renderUI({
    allTabs <- list(
      ivt = source(file.path("R/tabs/dataSubmitTab_ui.R"), local = TRUE)$value
    )

    # Unnaming tabs
    names(allTabs) <- NULL

    do.call(tabsetPanel, c(id = "tabs", allTabs))

  })

}
