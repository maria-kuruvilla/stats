
count <- 0
newdata <- matrix(0,226,3)
colnames(newdata)=c("day","temperatureIHA","temperaturePRA")
tempdata <- as.data.frame(newdata)
for(j in 90:315){
  tempdata$day[(j-89)] <- j
 
  for(i in 2137:7560){
    
    if(IHAtemp$`Day of year`[i]==j){
      
      tempdata$temperatureIHA[(j-89)] <- tempdata$temperatureIHA[(j-89)] + IHAtemp$`Temperature (C)`[i]/24
    }
    if(PRAtemp$`Day of year`[i]==j){
      if(is.na(PRAtemp$`Temperature (C)`[i])==TRUE){
        tempdata$temperaturePRA[(j-89)] <- tempdata$temperaturePRA[(j-89)] + RItemp$`Temperature (C)`[i]/24
      }
      else{
        tempdata$temperaturePRA[(j-89)] <- tempdata$temperaturePRA[(j-89)] + PRAtemp$`Temperature (C)`[i]/24
      }
      
    }
    
  }
  
    
}
write.csv(tempdata,'C:\\Users\\mariakur\\Documents\\Spring 2019\\QERM 514\\project\\tempdata.csv')
