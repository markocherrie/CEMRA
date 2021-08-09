#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  library(shinycssloaders)
  library(dplyr)
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      # App title ----
      titlePanel("Covid Exposure Model and Risk App (CEMRA)"),
      
      sidebarLayout(
        sidebarPanel(
          fileInput("file1", "Build your own - Scenario File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          selectInput("SETTING", "Preloaded Scenario:",
                      list("None"="None",
                           "Hospital" = "Hospital",
                           "Office" = "Office",
                           "Restaurant"="Restaurant",
                           "Small Retailer"="Small Retailer"
                      )),
          selectInput("INFECTED", "Infected status:",
                      list("Extremely high" = "EHI",
                           "Very high" = "VHI",
                           "High"="HI",
                           "Moderate" = "MI",
                           "Low"="LI",
                           "Very low" = "VLI",
                           "Extremely low"="ELI"
                      ), selected="MI"),
          selectInput("INTCOMPARISON", "Control Comparison:",
                      list("No interventions" = "NoInt",
                           "Susceptible wearing surgical mask"="SurgicalMask"
                      )),
          selectInput("ENGVAR", "Engineering controls:",
                      list("None" = "None",
                           "Fresh Air"="Freshair",
                           "NIOSH Ventilated headboard" = "VentHead",
                           "Sodeco Mobile Air Purification unit with UVC (UPH/EC-220-F7+F9-CG - 450m³/h)"="UVC"
                      )),
          selectInput("ADMVAR", "Administrative controls:",
                      list("None"="None",
                           "Surface Disinfection (Kurgat et al., 2019)" = "Hygiene",
                           "Surface Disinfection + hand hygiene (Kurgat et al., 2019)" = "Hygiene2"
                      )),
          selectInput("PPEVAR", "PPE controls:",
                      list("None" = "None",
                           "Surgical Mask" = "Surgical Mask",
                           "FFP2" = "FFP2",
                           "FFP3" = "FFP3",
                           "Worksafe AirHood"="Airhood"
                      )),
          actionButton("Run", "Run"),
          
          
          
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Information", includeHTML("docs/info.html")),
            tabPanel("Number infected", plotOutput("summary")%>% withSpinner(color="#428bca")),
            tabPanel("Route of transmission", plotOutput("relcon")%>% withSpinner(color="#428bca")),
            tabPanel("Parameters", tableOutput("params"))
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
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'CEMRA'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

