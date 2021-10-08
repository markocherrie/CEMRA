#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 

 #source("R/COVIDinfectioncalculator.R")
 #source("R/COVIDinfectioncalculatorBATCHnumberinfected.R")
 #source("R/COVIDinfectioncalculatorBATCHrelativecontributions.R")

devtools::load_all("R/")
  
#################### NEEDS UPDATED WITH CURRENT FILES ###############################  
  
# This function is responsible for loading in the selected file
# more variation in the scenario
  # hospital - single patient room
  # hospital - multi patient room
  # hosptial - 
  
filedata <- reactive({
    if (!is.null(input$file1)) {
      infile<-input$file1
      read.csv(infile$datapath)
    } else if(is.null(input$file1) & input$SETTING=="Hospital"){
      infile<-"data/runs/Hospital.csv"
      read.csv(infile)
    } else if(is.null(input$file1) & input$SETTING=="Office"){
      infile<-"data/runs/Office.csv"
      read.csv(infile)
    } else if(is.null(input$file1) & input$SETTING=="Restaurant"){
      infile<-"data/runs/Restaurant.csv"
      read.csv(infile)
    } else if(is.null(input$file1) & input$SETTING=="Small Retailer"){
        # put in proper setting file
        infile<-"data/runs/SmallRetailer.csv"
        read.csv(infile)
    }
  })
  
#################### NEEDS UPDATED WITH CURRENT FILES ###############################  


# Generate the input data

# allow more than one to be selected
modeldata <- reactive({
    
  df <- filedata()
      # INFECTIOUSNESS
      if(input$INFECTED=="EHI"){
        df$Infcoughrateperhourmax<-70
        df$Infcoughrateperhourmin<-60
        df$Infcoughrateperhourmode<-65
        df$InfsalivaChenscale<-7
        df$InfEairTalkSmean<-7
        df$ID<-paste0(df$ID, "_EHI")
        df
      } else if(input$INFECTED=="VHI"){
        df$Infcoughrateperhourmax<-60
        df$Infcoughrateperhourmin<-50
        df$Infcoughrateperhourmode<-55
        df$InfsalivaChenscale<-6
        df$InfEairTalkSmean<-6
        df$ID<-paste0(df$ID, "_VHI")
        df
      } else if(input$INFECTED=="HI"){
        df$Infcoughrateperhourmax<-50
        df$Infcoughrateperhourmin<-40
        df$Infcoughrateperhourmode<-45
        df$InfsalivaChenscale<-5
        df$InfEairTalkSmean<-5
        df$ID<-paste0(df$ID, "_HI")
        df
      }else if(input$INFECTED=="MI"){
        df$Infcoughrateperhourmax<-40
        df$Infcoughrateperhourmin<-30
        df$Infcoughrateperhourmode<-35
        df$InfsalivaChenscale<-4
        df$InfEairTalkSmean<-4
        df$ID<-paste0(df$ID, "_MI")
        df
      } else if(input$INFECTED=="LI"){
        df$Infcoughrateperhourmax<-30
        df$Infcoughrateperhourmin<-20
        df$Infcoughrateperhourmode<-25
        df$InfsalivaChenscale<-3
        df$InfEairTalkSmean<-3
        df$ID<-paste0(df$ID, "_LI")
        df
      } else if(input$INFECTED=="VLI"){
        df$Infcoughrateperhourmax<-20
        df$Infcoughrateperhourmin<-10
        df$Infcoughrateperhourmode<-15
        df$InfsalivaChenscale<-2
        df$InfEairTalkSmean<-2
        df$ID<-paste0(df$ID, "_VLI")
        df
      }else if(input$INFECTED=="ELI"){
        df$Infcoughrateperhourmax<-10
        df$Infcoughrateperhourmin<-1
        df$Infcoughrateperhourmode<-5
        df$InfsalivaChenscale<-1
        df$InfEairTalkSmean<-1
        df$ID<-paste0(df$ID, "_ELI")
        df
      }else{
        df
      }

    # ENGINEERING CONTROLS
      if(input$ENGVAR=="UVC"){
      df$RoomUVCpurificationinroom<-"Y"
      df$RoomUVCmaxflowrate<-450
      df$RoomUVCeffmin<-0.9
      df$RoomUVCeffmax<-1
      df$ID<-paste0(df$ID, "_UVC")
      df
    } else if (input$ENGVAR=="Freshair"){
      df$Roomwindowsopen<-"Y"
      df$Roomwindspeedmin<-1
      df$Roomwindspeedmax<-4
      df$RoomsoaW<-0.8
      df$RoomsoaH<-1
      df$RoomsoaP<-0.1
      df$ID<-paste0(df$ID, "_Freshair")
      df
    } else if (input$ENGVAR=="VentHead"){
      df$InfCexhaleprobmin<-0.06
      df$InfCexhaleprobmax<-0.27
      df$InfCexhaleprobmode<-0.17
      df$ID<-paste0(df$ID, "_VentHead")
      df
    } else{
      df
    }
    
    ################## ADMINISTRATIVE CONTROLS #################### 
  
    if(input$ADMVAR=="Hygiene"){
      df$SuCfomiteprobmin<-0.38
      df$SuCfomiteprobmax<-0.86
      df$SuCfomiteprobmode<-0.583
      df$ID<-paste0(df$ID, "_SurfaceDisinfection")
      df
    } else if(input$ADMVAR=="Hygiene2"){
      df$SuCfomiteprobmin<-0
      df$SuCfomiteprobmax<-0.47
      df$SuCfomiteprobmode<-0.146
      df$ID<-paste0(df$ID, "_SurfaceDisinfectionandhandhygiene")
      df
    }
  
  ##############################################################
    
    # PPE CONTROLS
    if(input$PPEVAR=="Surgical Mask"){
      df$SuCinhaleprobmin<-0.2
      df$SuCinhaleprobmax<-0.65
      df$SuCinhaleprobmode<-0.35
      df$ID<-paste0(df$ID, "_SurgicalMask")
      df
    } else if(input$PPEVAR=="FFP2"){
      df$SuCinhaleprobmin<-0.01
      df$SuCinhaleprobmax<-0.35
      df$SuCinhaleprobmode<-0.1
      df$ID<-paste0(df$ID, "_FFP2")
      df  
    } else if(input$PPEVAR=="FFP3"){
      df$SuCinhaleprobmin<-0.005
      df$SuCinhaleprobmax<-0.3
      df$SuCinhaleprobmode<-0.05
      df$ID<-paste0(df$ID, "_FFP3")
      df  
    } else if(input$PPEVAR=="AirHood"){
      df$SuCinhaleprobmin<-0.0003
      df$SuCinhaleprobmax<-0.05
      df$SuCinhaleprobmode<-0.01
      df$SuChandtouchmin<-0	
      df$SuChandtouchmax<-0
      df$SuChandtouchmode<-0
      df$ID<-paste0(df$ID, "_Airhood")
      df
    } else{
      df
    }
    
  })
baselinedata <- reactive({
    df <- filedata()
    df
})

  
# Create the params table for output

# change the metadata based on pre-loaded scenario - if user loaded then no refs??


paramdata <- reactive({
  df  <- modeldata()
  df2 <- tidyr::gather(df, key="Parameter")
  data(metadata)
  df2 <- merge(df2, metadata, by="Parameter", all=T)
  df2 <- with(df2,df2[order(ID) , ])
  df2 <- subset(df2, select = -c(ID) )
})


# run the model on the "button"

masteroutput <-eventReactive(input$button, {
  modeldata <- modeldata()
  baselinedata  <-baselinedata()
  
  # Specify how many iterations
  RUN<-do.call("rbind", replicate(200, modeldata, simplify = FALSE))
  RUN2<-do.call("rbind", replicate(200, baselinedata, simplify = FALSE))
  RUN3<-rbind(RUN, RUN2)
  
  # Run the function
  masteroutput<-plyr::mdply(RUN3, COVIDinfectioncalculator)
})

#
output$params <- renderTable({
    paramdata()
  })

# Generate number of infected plot
output$numberinfectedgraph <- renderPlotly({

    masteroutput <- masteroutput()
    masteroutput$numberinfected<-as.numeric(masteroutput$numberinfected)
    masteroutput <- masteroutput %>% select(ID,numberinfected)
  
    library(ggplot2)
    d<-ggplot(masteroutput, aes(x=ID, y=numberinfected))+
      geom_violin()+
      facet_wrap(~ID, scales="free_x")+
      theme(
        axis.title.x=element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank())+
      theme(text = element_text(size=12))
    d<-d + scale_y_continuous(trans='log10')+
      ylab("Risk per single activity")
    library(plotly)
    d<-ggplotly(d)
    
  })

# Generate number of infected text
output$infectedtextcomparison <- renderText({
 
  masteroutput <- masteroutput()
  masteroutput$numberinfected<-as.numeric(masteroutput$numberinfected)
  masteroutput <- masteroutput %>% select(ID,numberinfected)
  
  masteroutput<-masteroutput %>%
    group_by(ID) %>%
    summarise(mediannumberinfected=median(numberinfected, na.rm=T))
  
  
  # comparison
  masteroutput2 <- masteroutput()
  masteroutput2$numberinfected<-as.numeric(masteroutput2$numberinfected)
  masteroutput2 <- masteroutput2 %>% select(ID,numberinfected)
  
  masteroutput2<-masteroutput2 %>%
    group_by(ID) %>%
    summarise(mediannumberinfected=median(numberinfected, na.rm=T))
  
  
  scenariorisk<-(round(masteroutput$mediannumberinfected[1]*100000,2))
  
  scenario2risk<-(round(masteroutput2$mediannumberinfected[2]*100000,2))
  changeperc<-round(100-(round(masteroutput$mediannumberinfected[1]*100000,2)/(round(masteroutput2$mediannumberinfected[2]*100000,2))*100),2)
  changetext<-NA
  changetext[changeperc>0]<-"increase"
  changetext[changeperc==0]<-"difference"
  changetext[changeperc<0]<-"reduction"
  
  
  paste("The median number of infected for ",masteroutput$ID[1], " is ",
        "<font color=\"#FF0000\"><b>", scenariorisk , "per 100,000","</b></font>",
        "and for ",masteroutput2$ID[2], " is ", 
        "<font color=\"#FF0000\"><b>", scenario2risk , "per 100,000","</b></font>", 
        " which corresponds to a ","<font color=\"#FF0000\"><b>", changeperc,"%","</b></font>"," ", changetext," in risk.")
  
})


# Generate relcon plot
output$relcon <- renderPlot({
  
  masteroutput <- masteroutput()
  
  masteroutput<-masteroutput %>% 
    group_by(ID) %>%
    select(ID, rFACE, rLUNGNF, rLUNGFF, rSPRAY) %>%
    mutate_at(., c("rFACE", "rLUNGNF", "rLUNGFF", "rSPRAY"), ~as.numeric(.)) %>%
    summarise(CONTACT_mean =      mean(rFACE  /(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100),
              INHALATION_NF_mean = mean(rLUNGNF/(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100),
              INHALATION_FF_mean = mean(rLUNGFF/(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100),
              SPRAY_mean =        mean(rSPRAY /(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100))
  
  # to get 100%
  # https://stackoverflow.com/questions/13483430/how-to-make-rounded-percentages-add-up-to-100
  
  
  masteroutput <- tidyr::pivot_longer(masteroutput,cols=CONTACT_mean :SPRAY_mean) 
  
  library(hrbrthemes)
  library(waffle)
  library(ggplot2)
  library(dplyr)
  masteroutput %>%
    ggplot(aes(fill = name, values = value)) +
    expand_limits(x=c(0,0), y=c(0,0)) +
    coord_equal() +
    labs(fill = NULL, colour = NULL) +
    theme_ipsum_rc(grid="") +
    theme_enhance_waffle() -> waffleplot
  
  #group.colors <- c(Contact = "#E69F00", Spray = "#0072B2", `NF Inhalation`= "#CC79A7", `FF Inhalation`="#d9c7d1")
  
  
  waffleplot +
    geom_waffle(
      color = "white", size = 0.33
    ) +
    facet_wrap(~ID) +
    theme(strip.text.x = element_text(hjust = 0.5))
  
})

# Generate rel contr text
output$infectedrelcontext<- renderText({
  
  masteroutput <- masteroutput()
  
  masteroutput<-masteroutput %>% 
    group_by(ID) %>%
    select(ID, rFACE, rLUNGNF, rLUNGFF, rSPRAY) %>%
    mutate_at(., c("rFACE", "rLUNGNF", "rLUNGFF", "rSPRAY"), ~as.numeric(.)) %>%
    summarise(CONTACT_mean =      mean(rFACE  /(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100),
              INHALATION_NF_mean = mean(rLUNGNF/(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100),
              INHALATION_FF_mean = mean(rLUNGFF/(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100),
              SPRAY_mean =        mean(rSPRAY /(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100))
  
  masteroutput<-masteroutput %>% tidyr::pivot_longer(cols=CONTACT_mean :SPRAY_mean, names_to="route", values_to="risk")
  masteroutput2<-masteroutput[1:4,1:3]
  
  domroute<-with(masteroutput2, route[which.max(risk)])
  domroute<-gsub("_mean", "",domroute)
  domperc<-round(max(masteroutput2$risk),0)
  scenario2<-with(masteroutput2, ID[which.max(risk)])
  
  
  masteroutput3<-masteroutput[5:8,1:3]
  domroute2<-with(masteroutput3, route[which.max(risk)])
  domroute2<-gsub("_mean", "",domroute2)
  domperc2<-round(max(masteroutput3$risk),0)
  scenario3<-with(masteroutput3, ID[which.max(risk)])
  
  
  paste0("The dominant route in ", scenario2, " is ",
         "<font color=\"#FF0000\"><b>", domroute,"</b></font>", 
         " contributing ", 
         "<font color=\"#FF0000\"><b>",domperc,"%","</b></font>",
         " to the total risk",
         
         ". The dominant route in ", scenario3, " is ",
         "<font color=\"#FF0000\"><b>", domroute2,"%","</b></font>", 
         " contributing ", 
         "<font color=\"#FF0000\"><b>",domperc2,"%","</b></font>",
         " to the total risk"
         )

})




  
  
  
}
