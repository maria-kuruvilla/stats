#list of unique release sites

(unique.release.sites <- unique(PIT$`Release Site Name`))

# list of unique species detected

(unique.species <- unique(PIT$`Species Name`))

# list of the sites at which fish were detected

(unique.sites <- unique(PIT$`Site Name`))

# number of stray pittag detections

n.pit <- nrow(PIT)

#make empty data frame

newdata <- matrix(NA,n.pit,13)
colnames(newdata) <- c("day", "IHA","PRA", "MC", "Species", "RS", "Rear","IHAtotal","PRAtotal","MCtotal","IHAprevtotal","PRAprevtotal","MCprevtotal")
newdata <- as.data.frame(newdata)

for(i in 1:n.pit){
  
  # if fish was detected at IHA, then IHA = 1 and day of year is noted
  if(PIT$`Site Name`[i]=="IHA - Ice Harbor Adult"){
    newdata$IHA[i] = 1
    newdata$day[i] <- PIT$`Last Day Num`[i]
    
  }
  else{
    newdata$IHA[i] = 0
  }
  # if fish was detected at PRA, then PRA = 1 and day of year is noted
  if(PIT$`Site Name`[i]=="PRA - Priest Rapids Adult"){
    newdata$PRA[i] = 1
    newdata$day[i] <- PIT$`Last Day Num`[i] - 3
    
  }
  else{
    newdata$PRA[i] = 0
  }
  # if fish was detected at MC, then Mc = 1 and day of year is noted
  if(PIT$`Site Name`[i]=="MC1 - McNary Oregon Shore Ladder" || PIT$`Site Name`[i]=="MC2 - McNary Washington Shore Ladder"){
    newdata$MC[i] = 1
    newdata$day[i] <- PIT$`Last Day Num`[i]
    
  }
  else{
    newdata$MC[i] = 0
  }
  
  #record species name
  
  newdata$Species[i] <- PIT$`Species Name`[i]
  
  #record release site name
  
  newdata$RS[i] <- PIT$`Release Site Name`[i]
  
  #record rear type (hatchery or wild)
  
  newdata$Rear[i] <- PIT$`Rear Type Code`[i]
  
  
  for(j in 1:(nrow(IHA)-2)){
    # if the species detected was chinook, look at how many other chinook was there on the same day
    if(IHA$Day[j] == newdata$day[i] && newdata$Species[i]=="Chinook"){
      newdata$IHAtotal[i] <- IHA$ChinookAdult[j] + IHA$ChinookJack[j]
    }
    # if the species detected was steelhead, look at how many other steelhead was there on the same day
    
    if(IHA$Day[j] == newdata$day[i] && newdata$Species[i]=="Steelhead"){
      newdata$IHAtotal[i] <- IHA$Steelhead[j] + IHA$UnclippedSteelhead[j]
    }
    #look at chinook number on the previous day
    if(IHA$Day[j] == (newdata$day[i]-1) && newdata$Species[i]=="Chinook"){
      newdata$IHAprevtotal[i] <- IHA$ChinookAdult[j] + IHA$ChinookJack[j]
    }
    #look at steelhead number on the previous day
    if(IHA$Day[j] == (newdata$day[i]-1) && newdata$Species[i]=="Steelhead"){
      newdata$IHAprevtotal[i] <- IHA$Steelhead[j] + IHA$UnclippedSteelhead[j]
    }
  }
  #same thing for PRA but it takes 3 extra days to reach PRA 
  for(k in 1:(nrow(PRA)-2)){
    if(PRA$Day[k] == (newdata$day[i] +3) && newdata$Species[i]=="Chinook"){
      newdata$PRAtotal[i] <- PRA$ChinookAdult[k] + PRA$ChinookJack[k]
    }
    if(PRA$Day[k] == (newdata$day[i]+3) && newdata$Species[i]=="Steelhead"){
      newdata$PRAtotal[i] <- PRA$Steelhead[k] + PRA$UnclippedSteelhead[k]
    }
    #3 extra days - 1 day for previous 
    if(PRA$Day[k] == (newdata$day[i] +2) && newdata$Species[i]=="Chinook"){
      newdata$PRAprevtotal[i] <- PRA$ChinookAdult[k] + PRA$ChinookJack[k]
    }
    if(PRA$Day[k] == (newdata$day[i]+2) && newdata$Species[i]=="Steelhead"){
      newdata$PRAprevtotal[i] <- PRA$Steelhead[k] + PRA$UnclippedSteelhead[k]
    }
  }
  for(l in 1:(nrow(MC)-2)){
    if(MC$Day[l] == newdata$day[i] && newdata$Species[i]=="Chinook"){
      newdata$MCtotal[i] <- MC$ChinookAdult[l] + MC$ChinookJack[l]
    }
    if(MC$Day[l] == newdata$day[i] && newdata$Species[i]=="Steelhead"){
      newdata$MCtotal[i] <- MC$Steelhead[l] + MC$UnclippedSteelhead[l]
    }
    
    if(MC$Day[l] == (newdata$day[i]-1) && newdata$Species[i]=="Chinook"){
      newdata$MCprevtotal[i] <- MC$ChinookAdult[l] + MC$ChinookJack[l]
    }
    if(MC$Day[l] == (newdata$day[i]-1) && newdata$Species[i]=="Steelhead"){
      newdata$MCprevtotal[i] <- MC$Steelhead[l] + MC$UnclippedSteelhead[l]
    }
  }
  
  
}
#compile the data

data.project <- matrix(NA,585,13)
colnames(data.project) <- c("day", "IHA", "Species", "RS", "Rear","IHAtotal","PRAtotal","proportion","count","IHAprevtotal","PRAprevtotal","prevproportion","prevcount")
data <- as.data.frame(data.project)
count <- 0
for(i in 33:2006){
  if(newdata$IHA[i]==1 || newdata$PRA[i]==1){ #only thise detected at PRA and IHA, not Mc.
    count <- count + 1
    data$day[count] <- newdata$day[i] #day of year
    data$IHA[count] <- newdata$IHA[i] #chose IHA =1, did not choose IHA = 0
    data$Species[count] <- newdata$Species[i] #species
    data$RS[count] <- newdata$RS[i] #release site
    data$Rear[count] <- newdata$Rear[i] #rear type (but most are unknown)
    data$IHAtotal[count] <- newdata$IHAtotal[i] # total number at IHA on same day
    data$PRAtotal[count] <- newdata$PRAtotal[i] #total number at PRA on same day
    data$proportion[count] <- newdata$IHAtotal[i]/(newdata$IHAtotal[i] + newdata$PRAtotal[i])
    #proportion that chose IHA on same day
    data$count[count] <- newdata$IHAtotal[i] + newdata$PRAtotal[i]
    #total count at both IHA and PRA on same day
    data$IHAprevtotal[count] <- newdata$IHAprevtotal[i]
    #total fish at IHA on the previous day
    data$PRAprevtotal[count] <- newdata$PRAprevtotal[i]
    #total fish at PRA on the previous day
    data$prevproportion[count] <- newdata$IHAprevtotal[i]/(newdata$IHAprevtotal[i] + newdata$PRAprevtotal[i])
    #proportion of fish that chose IHA the previous day
    data$prevcount[count] <- newdata$IHAprevtotal[i] + newdata$PRAprevtotal[i]
    #total count at both IHA and PRA on previous day
  }
  
}
write.csv(data,'C:\\Users\\mariakur\\Documents\\Spring 2019\\QERM 514\\project\\datanew.csv')
plot(data$proportion,data$IHA)
plot(data$prevproportion,data$IHA)

###################################################################################################################################################




#including temperature ratio and flow ratio



###################################################################################################################################################

temp <- matrix(NA,nrow(datanew),4)
colnames(temp) <- c("difference","scale difference","ratio","scale ratio")
temp <- as.data.frame(temp)

for(i in 1:nrow(datanew)){
  for(j in 1:nrow(tempdata))
    if(datanew$day[i] == tempdata$day[j]){
      temp[i,1] <- tempdata$difference[j] #temperature difference between the two streams 
      temp[i,2] <- tempdata$`scale difference`[j] #scaling the temperature difference
      temp[i,3] <- tempdata$ratio[j] #ratio of temperatures of the two streams IHA/PRA
      temp[i,4] <- tempdata$`scale ratio`[j] #scaling the ratio
    }
}

datanew <- cbind(datanew,temp) #bind it to the previous data frame
datanew <- datanew[-c(584,585),]
write.csv(datanew,'C:\\Users\\mariakur\\Documents\\Spring 2019\\QERM 514\\project\\datanew.csv')

flow <- matrix(NA,nrow(datanew),1)
colnames(flow) <- c("outflowratio")
flow <- as.data.frame(flow)

for(i in 1:nrow(datanew)){
  for(j in 1:nrow(outflow))
    if(datanew$day[i] == outflow$DOY[j]){
      flow[i,1] <- outflow$ratio[j] #ratio of flow of two streams IHA/PRA
      
    }
}
datanew <- cbind(datanew,flow) #bind it to the previous data frame
write.csv(datanew,'C:\\Users\\mariakur\\Documents\\Spring 2019\\QERM 514\\project\\datanew.csv')