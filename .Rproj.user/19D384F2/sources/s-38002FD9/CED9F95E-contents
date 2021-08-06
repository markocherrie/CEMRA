COVIDinfectioncalculatorBATCHnumberinfected<-function(RUN, B){
  
  # Specify how many iterations
  RUN<-do.call("rbind", replicate(B, RUN, simplify = FALSE))
  
  # Run the function
  output<-plyr::mdply(RUN, COVIDinfectioncalculator)
  
  # Raw output
  #write.csv(output, paste0("C:/Users/mark.cherrie/Box/projects/COVID-19/code/COVID19_transmission/output/",filename,"_",B,".csv"), row.names=F)
  
  ##### Output 1: Number of people infected 
  
  numberinfected<-output %>% 
    group_by(Infactivity) %>%
    summarise(mean=mean(numberinfected), sd=sd(numberinfected), Su=unique(Su)) %>%
    mutate(numberinfectedLL=mean-(sd/sqrt(nrow(output))), 
           numberinfectedUL=mean+(sd/sqrt(nrow(output))))
  
  #write.csv(numberinfected, paste0("C:/Users/mark.cherrie/Box/projects/COVID-19/code/COVID19_transmission/output/",filename,"_", B, "_numberinfected.csv"), row.names=F)
  
  return(numberinfected)
}
