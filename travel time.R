
####################################### Estimating travel time from Mc to IHA ##############

IHAtag <- as.data.frame(Complete_Tag_History_MC_IHA)
n <- nrow(IHAtag)
uniquetag <- length(unique(IHAtag$`Tag Code`)) # number of unique fish
count <- 0
c <- rep(NA,(n-2*uniquetag))
for(i in 1:(n)){
  if(IHAtag$`Event Site Name`[i]=="MC1 - McNary Oregon Shore Ladder" || IHAtag$`Event Site Name`[i]=="MC2 - McNary Washington Shore Ladder"){
    
    IHAtag$`Event Site Name`[i] <- "MC"
  }
}

# MC1 and MC2 are both at the same dam.


for(i in 1:(n-1)){
  
  
    
    if(IHAtag$`Tag Code`[i]==IHAtag$`Tag Code`[(i+1)]){
      
      if(IHAtag$`Event Site Name`[i] == IHAtag$`Event Site Name`[i+1]){
        count <- count +1
        c[count] <- i
      }
        
      
    
    
  }
}
# looking at all the fish that were detected multiple times at the same site
IHAtag <- IHAtag[-c,]
# removing all those detections.

write.csv(IHAtag,'C:\\Users\\mariakur\\Documents\\Spring 2019\\QERM 514\\project\\traveltimeMCtoIHA.csv')

#########################################################################################################################


#doing the same thing for Priest Rapids dam.

IHAtag <- as.data.frame(PRAtag)
n <- nrow(IHAtag)
uniquetag <- length(unique(IHAtag$`Tag Code`))
count <- 0
c <- rep(NA,(n-2*uniquetag))
for(i in 1:(n)){
  if(IHAtag$`Event Site Name`[i]=="MC1 - McNary Oregon Shore Ladder" || IHAtag$`Event Site Name`[i]=="MC2 - McNary Washington Shore Ladder"){
    
    IHAtag$`Event Site Name`[i] <- "MC"
  }
}

for(i in 1:(n-1)){
  
  
  
  if(IHAtag$`Tag Code`[i]==IHAtag$`Tag Code`[(i+1)]){
    
    if(IHAtag$`Event Site Name`[i] == IHAtag$`Event Site Name`[i+1]){
      count <- count +1
      c[count] <- i
    }
    
    
    
    
  }
}
IHAtag <- IHAtag[-c,]
write.csv(IHAtag,'C:\\Users\\mariakur\\Documents\\Spring 2019\\QERM 514\\project\\traveltimeMCtoPRA.csv')