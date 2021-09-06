#repfactor<-round(masteroutput$value)
#contactvals<-rep("Contact",repfactor[1])
#NFinhalationvals<-rep("NF Inhalation",repfactor[2])
#FFinhalationvals<-rep("FF Inhalation",repfactor[3])
#sprayvals<-rep("Spray",repfactor[4])

#allvals<-c(contactvals,NFinhalationvals, FFinhalationvals, sprayvals)
# make to 100%
#allvals<-as.data.frame(allvals)
#allvals$allvals<-as.character(allvals$allvals)
#allvals$id<-1



#library(ggwaffle)

#waffledata<-waffle_iron(allvals, aes_d(group = allvals), rows=10)


#colnames(waffledata)<-c("x", "y", "z")

#library(emojifont)  
#library(dplyr)
#ggplot(waffledata, aes(x, y, fill = z)) + 
#  geom_waffle() +
#  scale_fill_manual(values=group.colors)+
#  coord_equal() + 
#  theme_waffle() +
#  xlab("")+
#  ylab("")+
#  ggtitle(" ")+
#  theme(legend.position="bottom")+
#  theme(legend.title = element_blank()) +
#  theme(plot.title = element_text(hjust = 0.5))+
#  theme(plot.title = element_text(size = 15, face = "bold"))