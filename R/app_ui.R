# Project: enetwildDET
#
# Author: shevelp(sergio.lopez@uclm.es)
#
# ui file: header, footer, loading tabs from server
###############################################################################

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    #  Adding external resources
    golem_add_external_resources(),
    # UI logic
    fluidPage(
      title = "EW-Integrated Validation Tool",

      shinyjs::useShinyjs(),

      # Spacer
      tags$div(style = "height: 20px;"),

      # Header
      tags$p(titlePanel(title = div(
        img(src = "www/EFSA_logo.png", height = "60px", hspace = "50px"),
        "Integrated Validation Tool (v1.1.0)", #VERSION!
        img(src = "www/logo-enetwild.jpg", height = "60px", hspace = "50px"),

        # Useful links
        tags$div(style = "margin-right:15px; margin-top: 10px; font-size: 14px;",
                 actionLink("manualLink", label = "Manual"), #subir manual!
                 "-",
                 a(href="https://enetwild.com/help-desk/",
                   target="_blank", "Report new issue"),
                 align = "right")
      ))),

      #Tabs defined in app_server.R
      uiOutput("tabPanels"),

      # Footer
      tags$footer(
        style = "text-align: center; font-size: 12px;",
        div(
          class = "navbar fixed-bottom bg-light",
          fluidRow(
            column(12,
                   tags$div(
                     style = "text-align: center; font-size: 12px;",
                     # Horizontal line
                     hr(),
                     # Footer content
                     HTML("&copy; 2023 IVT | "),
                     a(href = "https://twitter.com/enetwild", "Twitter"),
                     HTML(" | "),
                     a(href = "https://enetwild.com/", "Contact Us")
                   )
            )
          )
        )
      )
    )
  )
}
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "enetwildIVT"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
