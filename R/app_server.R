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
  
# This function is responsible for loading in the selected file
filedata <- reactive({
    if (!is.null(input$file1)) {
      infile<-input$file1
      read.csv(infile$datapath)
    } else if(is.null(input$file1) & input$SETTING=="Hospital"){
      # put in proper setting file
      infile<-"data/runs/Hospital.csv"
      read.csv(infile)
    } else if(is.null(input$file1) & input$SETTING=="Office"){
      # put in proper setting file
      infile<-"data/runs/Office.csv"
      read.csv(infile)
    } else if(is.null(input$file1) & input$SETTING=="Restaurant"){
      # put in proper setting file
      infile<-"data/runs/Restaurant.csv"
      read.csv(infile)
    } else if(is.null(input$file1) & input$SETTING=="Small Retailer"){
        # put in proper setting file
        infile<-"data/runs/SmallRetailer.csv"
        read.csv(infile)
    }
  })
  
# Control data changes based on input
modeldata <- reactive({
    
  # Change the inputted dataset using a number of params 
  df<-
      df <- filedata()
      # INFECTIOUSNESS
      if(input$INFECTED=="EHI"){
        df$Infcoughrateperhourmax<-70
        df$Infcoughrateperhourmin<-60
        df$Infcoughrateperhourmode<-65
        df$InfsalivaChenscale<-7
        df$InfEairTalkSmean<-7
        df
      } else if(input$INFECTED=="VHI"){
        df$Infcoughrateperhourmax<-60
        df$Infcoughrateperhourmin<-50
        df$Infcoughrateperhourmode<-55
        df$InfsalivaChenscale<-6
        df$InfEairTalkSmean<-6
        df
      } else if(input$INFECTED=="HI"){
        df$Infcoughrateperhourmax<-50
        df$Infcoughrateperhourmin<-40
        df$Infcoughrateperhourmode<-45
        df$InfsalivaChenscale<-5
        df$InfEairTalkSmean<-5
        df
      }else if(input$INFECTED=="MI"){
        df$Infcoughrateperhourmax<-40
        df$Infcoughrateperhourmin<-30
        df$Infcoughrateperhourmode<-35
        df$InfsalivaChenscale<-4
        df$InfEairTalkSmean<-4
        df
      } else if(input$INFECTED=="LI"){
        df$Infcoughrateperhourmax<-30
        df$Infcoughrateperhourmin<-20
        df$Infcoughrateperhourmode<-25
        df$InfsalivaChenscale<-3
        df$InfEairTalkSmean<-3
        df
      } else if(input$INFECTED=="VLI"){
        df$Infcoughrateperhourmax<-20
        df$Infcoughrateperhourmin<-10
        df$Infcoughrateperhourmode<-15
        df$InfsalivaChenscale<-2
        df$InfEairTalkSmean<-2
        df
      }else if(input$INFECTED=="ELI"){
        df$Infcoughrateperhourmax<-10
        df$Infcoughrateperhourmin<-1
        df$Infcoughrateperhourmode<-5
        df$InfsalivaChenscale<-1
        df$InfEairTalkSmean<-1
        df
      }else{
        df
      }

    # ENGINEERING CONTROLS
      if(input$ENGVAR=="UVC"){
      df$RoomUVCpurificationinroom<-"Y"
      RoomUVCmaxflowrate<-450
      RoomUVCeffmin<-0.9
      RoomUVCeffmax<-1
      df
    } else if (input$ENGVAR=="Freshair"){
      df$Roomwindowsopen<-"Y"
      df$Roomwindspeedmin<-1
      df$Roomwindspeedmax<-4
      df$RoomsoaW<-0.8
      df$RoomsoaH<-1
      df$RoomsoaP<-0.1
      df
    } else if (input$ENGVAR=="VentHead"){
      df$InfCexhaleprobmin<-0.06
      df$InfCexhaleprobmax<-0.27
      df$InfCexhaleprobmode<-0.17
      df
    } else{
      df
    }
    
    # ADMINISTRATIVE CONTROLS
    if(input$ADMVAR=="Hygiene"){
      df$SuCfomiteprob <-0.146
      df
    } else{
      df
    }
    
    # PPE CONTROLS
    if(input$PPEVAR=="Surgical Mask"){
      df$SuCinhaleprobmin<-0.2
      df$SuCinhaleprobmax<-0.65
      df$SuCinhaleprobmode<-0.35
      df
    } else if(input$PPEVAR=="FFP2"){
      df$SuCinhaleprobmin<-0.01
      df$SuCinhaleprobmax<-0.35
      df$SuCinhaleprobmode<-0.1
      df  
    } else if(input$PPEVAR=="FFP3"){
      df$SuCinhaleprobmin<-0.005
      df$SuCinhaleprobmax<-0.3
      df$SuCinhaleprobmode<-0.05
      df  
    } else if(input$PPEVAR=="AirHood"){
      df$SuCinhaleprobmin<-0.0003
      df$SuCinhaleprobmax<-0.05
      df$SuCinhaleprobmode<-0.01
      df$SuChandtouchmin<-0	
      df$SuChandtouchmax<-0
      df$SuChandtouchmode<-0
      df
    } else{
      df
    }
    
  })
  
# output params data
paramdata <- reactive({
  df  <- modeldata()
  df2 <- tidyr::gather(df, key="Parameter")
  data(metadata)
  df2 <- merge(df2, metadata, by="Parameter", all=T)
  df2 <- with(df2,df2[order(ID) , ])
  df2 <- subset(df2, select = -c(ID) )
})
output$params <- renderTable({
    paramdata()
  })
  
# Generate a summary of the data
output$summary <- renderPlot({
    if (is.null(input$file1)){return("")}                                  
    
    modeldata <-modeldata()
    
    modeldataoutput<- COVIDinfectioncalculator(modeldata, 10)
    
    print(modeldataoutput)
    
    numberinfected<-round(as.numeric(modeldataoutput$mean)*100000)
    numberinfectedLL<-round(as.numeric(modeldataoutput$numberinfectedLL)*100000)
    numberinfectedUL<-round(as.numeric(modeldataoutput$numberinfectedUL)*100000)
    
    
    confdiff<-numberinfectedUL-numberinfectedLL
    
    if(numberinfected>0){
      infected<-as.data.frame(rep("Infected", numberinfectedLL))
      colnames(infected)<-"Status"
      
      maybeinfected<-as.data.frame(rep("Possibly Infected", confdiff))
      colnames(maybeinfected)<-"Status"
      
      library(ggwaffle)
      library(emojifont)
      waffledf<-as.data.frame(rbind(infected, maybeinfected))
      waffledf$Status <- as.character(waffledf$Status)
      waffledf$id<-1
      
      waffledata<-waffle_iron(waffledf, aes_d(group = Status), rows = 25)
      
      colnames(waffledata)<-c("x", "y", "For every 100,000 people exposed")
      
      
      library(emojifont)  
      library(dplyr)
      waffledata<-waffledata %>% mutate(label = fontawesome('fa-female'))
      
      group.colors <- c(Infected = "#d93f34", `Possibly Infected` = "#eda49f")
      
      ggplot(waffledata, aes(x, y, fill = `For every 100,000 people exposed`)) + 
        geom_waffle() +
        
        geom_text(aes(label=label), family='fontawesome-webfont', size=4) +
        coord_equal() +
        theme_waffle() +
        scale_fill_manual(values=group.colors)+
        xlab("")+
        ylab("")+
        ggtitle(" ")+
        theme(legend.position="bottom")+
        theme(plot.title = element_text(hjust = 0.5))+
        theme(plot.title = element_text(size = 15, face = "bold"))
    }else{
      ggplot() +
        theme_waffle() +
        xlab("")+
        ylab("")+
        ggtitle("No one infected")+
        theme(legend.position="bottom")+
        theme(legend.title = element_blank()) +
        theme(plot.title = element_text(size = 15, face = "bold"))
    }
  })

# Generate relcon plot
output$relcon <- renderPlot({
    if (is.null(input$file1)){return("")} 
    modeldata <-modeldata()
    modeldataoutput<- COVIDinfectioncalculatorBATCHrelativecontributions(modeldata, input$nusimu)
    mean <- t(modeldataoutput[,grep("_mean*", names(modeldataoutput))])
    mean <- mean[c(1,3,2),1]
    LL<-t(modeldataoutput[,grep("_LL*", names(modeldataoutput))])
    UL<-t(modeldataoutput[,grep("_UL*", names(modeldataoutput))])
    plotdata<-as.data.frame(cbind(mean, LL, UL))
    colnames(plotdata)<-c("mean", "LL", "UL")
    plotdata$route<-gsub("_mean", "",row.names(plotdata))
    plotdata
    
    print(plotdata)
    
    repfactor<-round(plotdata$mean)
    contactvals<-rep("Contact",repfactor[1])
    sprayvals<-rep("Spray",repfactor[2])
    inhalationvals<-rep("Inhalation",repfactor[3])
    allvals<-c(contactvals, sprayvals, inhalationvals)
    # make to 100%
    allvals<-as.data.frame(allvals)
    allvals$allvals<-as.character(allvals$allvals)
    allvals$id<-1
    
    
    group.colors <- c(Contact = "#E69F00", Spray = "#0072B2", Inhalation= "#CC79A7")
    
    
    waffledata<-waffle_iron(allvals, aes_d(group = allvals), rows=10)
    
    
    colnames(waffledata)<-c("x", "y", "z")
    
    library(emojifont)  
    library(dplyr)
    ggplot(waffledata, aes(x, y, fill = z)) + 
      geom_waffle() +
      scale_fill_manual(values=group.colors)+
      coord_equal() + 
      theme_waffle() +
      xlab("")+
      ylab("")+
      ggtitle(" ")+
      theme(legend.position="bottom")+
      theme(legend.title = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5))+
      theme(plot.title = element_text(size = 15, face = "bold"))
    
  })
  
  
  
}
