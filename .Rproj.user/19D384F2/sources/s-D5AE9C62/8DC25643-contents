


masteroutput <- CEMRA::run_model("Hospital", 200)

masteroutput<-masteroutput %>% 
  group_by(ID) %>%
  select(ID, rFACE, rLUNGNF, rLUNGFF, rSPRAY) %>%
  mutate_at(., c("rFACE", "rLUNGNF", "rLUNGFF", "rSPRAY"), ~as.numeric(.)) %>%
  summarise(CONTACT_mean =      mean(rFACE  /(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100),
            INHALATION_NF_mean = mean(rLUNGNF/(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100),
            INHALATION_FF_mean = mean(rLUNGFF/(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100),
            SPRAY_mean =        mean(rSPRAY /(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100))

masteroutput <- tidyr::pivot_longer(masteroutput,cols=CONTACT_mean :SPRAY_mean) 

masteroutput$floor<-floor(masteroutput$value)

# 98

masteroutput$round<-round(masteroutput$value)


percents<-c(1.12, 58.3, 27.7, 12.9)

error_gen<-function(actual, rounded){
  divisor <- ifelse(sqrt(actual < 1.0, 1, actual))
  output <- (rounded - actual)^2 / divisor
  return(output)
}

  
round_to_100<-function(){
  if(sum(percents)!=100){

    
    
    }
}

#################################
def round_to_100(percents):
  if not isclose(sum(percents), 100):
  raise ValueError
n = len(percents)
rounded = [int(x) for x in percents]
up_count = 100 - sum(rounded)
errors = [(error_gen(percents[i], rounded[i] + 1) - error_gen(percents[i], rounded[i]), i) for i in range(n)]
rank = sorted(errors)
for i in range(up_count):
  rounded[rank[i][1]] += 1
return rounded


###############################



