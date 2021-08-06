COVIDinfectioncalculatorBATCHrelativecontributions<-function(RUN, B){
  
  
  # Specify how many iterations
  RUN<-do.call("rbind", replicate(B, RUN, simplify = FALSE))
  
  # Run the function
  output<-plyr::mdply(RUN, COVIDinfectioncalculator)
  
  ##### Output 2: Relative contributions
  
  relativecontributions<-output %>% 
    group_by(Infactivity) %>%
    summarise(CONTACT_mean=mean(rFACE/(rLUNG+rFACE+rSPRAY)*100), sdcontact=sd(rFACE/(rLUNG+rFACE+rSPRAY)*100),
              INHALATION_mean=mean(rLUNG/(rLUNG+rFACE+rSPRAY)*100), sdinhalation=sd(rLUNG/(rLUNG+rFACE+rSPRAY)*100),
              SPRAY_mean=mean(rSPRAY/(rLUNG+rFACE+rSPRAY)*100), sdspray=sd(rSPRAY/(rLUNG+rFACE+rSPRAY)*100))%>%
    mutate(CONTACT_LL= CONTACT_mean-(sdcontact/sqrt(nrow(output))),
           CONTACT_UL= CONTACT_mean+(sdcontact/sqrt(nrow(output))),
           SPRAY_LL= SPRAY_mean-(sdspray/sqrt(nrow(output))),
           SPRAY_UL= SPRAY_mean+(sdspray/sqrt(nrow(output))),
           INHALATION_LL= INHALATION_mean-(sdinhalation/sqrt(nrow(output))),
           INHALATION_UL= INHALATION_mean+(sdinhalation/sqrt(nrow(output))))
  
  #write.csv(relativecontributions, paste0("C:/Users/mark.cherrie/Box/projects/COVID-19/code/COVID19_transmission/output/",filename,"_", B, "_relativecontributions.csv"), row.names=F)
  return(relativecontributions)
}
