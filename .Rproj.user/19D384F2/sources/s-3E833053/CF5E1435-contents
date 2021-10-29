#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
#' 

app_ui <- function(request) {
  library(shinycssloaders)
  library(dplyr)
  library(plotly)
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      # App title ----
      #titlePanel("Covid Exposure Model and Risk App (CEMRA)"),
      headerPanel(
        fluidRow(
          column(11, h1("Covid Exposure Model and Risk App (CEMRA)", 
                        style = "'font-weight': bold; 'font-size': 39px"))
          #column(1, offset=-1, tags$img(src = "img/iomlogo.png"))
        )
        , windowTitle = "Covid Exposure Model and Risk App (CEMRA)"),
      
      
      sidebarLayout(
        sidebarPanel(
          fileInput("file1", "Build your own - Setting File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")),
          selectInput("SETTING", "Preloaded Setting:",
                      list("Hospital (Single Patient room)" = "Hospital_singlepatient",
                           "Hospital (Single Patient room) - high face touches"="Hospital_singlepatient_hightouch",
                           "Hospital (Two Patient room)"= "Hospital_twopatient",
                           "Hospital (Treatment room)" = "Hospital_singlepatienttreatment",
                           "Office (Open plan)" = "Office (Open plan)",
                           "Office (Meeting room)"="Office (meeting room)",
                           "Restaurant"="Restaurant",
                           "Small Retailer"="Small Retailer",
                           "Communal toilet"="Communal toilet"
                      )),
          #radioButtons("DURATION", "Duration in room:",
          #            list("Short Task (between 3 and 17 minutes)" = "shorttask",
          #                 "Long Task (between 25 and 35 minutes)" = "longtask"
          #               
          #            )),
          selectInput("INFECTED", "Infectiousness:",
                      list("As specified in setting file"="ASISF",
                           "Extremely high" = "EHI",
                           "Very high" = "VHI",
                           "High"="HI",
                           "Moderate" = "MI",
                           "Low"="LI",
                           "Very low" = "VLI",
                           "Extremely low"="ELI",
                           "Unknown"= "Unknown"
                      ), selected="ASISF"),
          selectInput("STAGEOFINFECTION", "Stage of Infection:",
                      list("As specified in setting file"="ASISF",
                        "Pre-peak"="Pre-peak",
                           "Peak" = "Peak",
                           "Around Peak" = "Around Peak",
                           "Post-Peak"="Post-Peak"
                      ), selected="ASISF"),
          selectInput("ENGVAR", "Engineering controls:",
                      list("None" = "None",
                           "Fresh air from small window open 10%"="Freshair",
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
          sliderInput("simu", "Number of simulations:",
                      min = 0, max = 1000,
                      value = 100, step = 100),
          actionButton("button", "Model"),
          downloadButton("downloadData", "Download parameters"),
          
          
          
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Model", includeHTML("data/docs/info.html")),
            #tabPanel("Scenarios", includeHTML("data/docs/scenarios.html")),
            #tabPanel("Viral load and shedding", includeHTML("data/docs/infectiousness.html")),
            #tabPanel("Controls", includeHTML("data/docs/controls.html")),
            tabPanel("How to use", includeHTML("data/docs/howtouse.html")),
            tabPanel("Parameters", tableOutput("params")),
            tabPanel("Risk of Infection", plotOutput("numberinfectedgraph")%>% withSpinner(color="#428bca"), htmlOutput("infectedtextcomparison")),
            tabPanel("Route of transmission", plotOutput("relcon", width = "100%")%>% withSpinner(color="#428bca"), htmlOutput("infectedrelcontext")),
            tabPanel("Acknowledgments", includeHTML("data/docs/acknowledgements.html"))
            
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


