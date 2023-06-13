# Project: IVT
#
# ui file - Load Data/IVT
#
# Author: Sergio López IVT
###############################################################################

tabPanel(title = "Data Submit",

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
                  Download the last version of the template here:"),

           br(),

           #download template
           tags$div(style = "text-align: center;",
                    downloadButton("downloadData", label = "Download template", class = "btn-primary")
           ),

           #submission panel module UI
           mod_submissionPanel_ui("submissionPanel")
         )
)
