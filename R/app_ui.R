#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom uuid UUIDgenerate
#' @import sf
#' @import dplyr
#' @import readxl
#' @import purrr
#' @noRd
#'
#'

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      uiOutput("debug"),
      tags$em(textOutput("userTypes")),

      tags$div(style = "margin-right:15px",
               downloadLink("manualLink", label = "Manual"),
               "-",
               a(href="https://github.com/IREC-CSIC-UCLM/IVT-shinyapp/issues",
                 target="_blank", "Report new issue"),
               align = "right"),
      tags$div(style = "color:gray; margin-right:15px", textOutput("version"), align = "right"),

      tags$p(titlePanel(title = div(
        img(src = "www/EFSA_logo.png", height = "60px", hspace = "50px"),
        "Integrated Validation Tool",
        img(src = "www/logo-enetwild.jpg", height = "60px", hspace = "50px")),
        windowTitle = "IVT")
      ),

      #NEW
      uiOutput("tabPanels"),

      hr(),
      br(),

      # footer
      fluidRow(
        column(12,
               tags$div(
                 style = "text-align: center; font-size: 12px;",
                 HTML("&copy; 2023 IVT | "),
                 a(href = "https://twitter.com/enetwild", "Twitter"),
                 HTML(" | "),
                 a(href = "https://enetwild.com/", "Contact Us")
               )
        )
      ),
      br(),
    ) #end fluidPage
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
    ),
    # Add here other external resources
    shinyjs::useShinyjs()
  )
}
