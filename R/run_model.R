run_model<-function(ID, B){
  
    set.seed(999)
    
    modeldata<-read.csv(paste0("data/runs/",ID,".csv"))
    
    # Specify how many iterations
    RUN<-do.call("rbind", replicate(B, modeldata, simplify = FALSE))
    
    # sort on id so that we get the correct temporal sequence
    RUN<-RUN[order(RUN$ID),]
    
    # Run the function
    output<-plyr::mdply(RUN, COVIDinfectioncalculator)
    
    # output to csv
    write.csv(output, paste0("data/output/", ID ,".csv"), row.names = F)
    
    # get output
    return(output)
}