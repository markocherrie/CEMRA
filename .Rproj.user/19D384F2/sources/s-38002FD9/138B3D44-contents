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
    infile <- input$file1
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  
# Control data changes based on input
modeldata <- eventReactive(input$Run, {
    df<- if(input$ENGVAR=="UVC"){
      df <- filedata()
      ACHadd<-850/((df$Roomvolumemin+df$Roomvolumemax)/2)
      df$RoomACHmin <- df$RoomACHmin + ACHadd
      df$RoomACHmax <- df$RoomACHmax + ACHadd
      df
    } else if (input$ENGVAR=="VentHead"){
      df <- filedata()
      df$InfEairTalkSmin<-0.10*df$InfEairTalkSmin
      df$InfEairTalkSmin<-0.10*df$InfEairTalkSmax
      df$Infcoughrateperhour<-0.10*df$Infcoughrateperhour
      df
    } else{
      df <- filedata()
      df
    }
    
    if(input$ADMVAR=="Hygiene"){
      df$SuCfomiteprob <-0.146
      df
    } else{
      df
    }
    
    if(input$PPEVAR=="N95"){
      df$SuCeyeprob<-0.04
      df$SuCsprayprob<-0.05
      df$SuCinhaleprob<-0.5
      df
    } else if(input$PPEVAR=="N95inf"){
      df$InfCsprayprob<-0.05
      df$InfCexhaleprob<-0.5
      df  
    } else if(input$PPEVAR=="AirHood"){
      df$SuCeyeprob<-0
      df$SuCsprayprob<-0
      df$SuCinhaleprob<-0.01
      df
    } else{
      df
    }
    
  })
  
# output params data
paramdata <- reactive({
  df <- modeldata()
  df2 <- tidyr::gather(df, key="Parameter")
  auxdata<-read.csv("auxdata.csv")
  df2<-merge(df2, auxdata, by="Parameter", all=T)
  df2 <- with(df2,df2[order(ID) , ])
  df2 <- subset(df2, select = -c(ID) )
})
output$params <- renderTable({
    req(input$file1)
    paramdata()
  })
  
# Generate a summary of the data
output$summary <- renderPlot({
    if (is.null(input$file1)){return("")}                                  
    
    modeldata <-modeldata()
    
    modeldataoutput<- COVIDinfectioncalculatorBATCHnumberinfected(modeldata, input$nusimu)
    
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
