require(here)
sens_data <- read.csv(here("Documents","data","temp_collective","roi","sensitivity_analysis_new.csv"),header=TRUE,na.strings=c("[nan]"))
my_new_data2 <- sens_data[-c(750,752,327,695),]
#making matrix to store response variables and model outputs
matrix_data <- matrix(data= NA, nrow = length(10:225), ncol = 16)#, colnames = c("Intercept","T","T2","GS",Loom","int",p_int","p_t", "p_t2","p_gs", "p_loom","p_int"))
ii = 1
for(i in 10:225){
  response <- colnames(sens_data)[i]
  if ( i < 109){
    startle_threshold <- as.numeric(substr(response, 8,8))
    if(startle_threshold < 5){
      startle_threshold <- as.numeric(substr(response, 8,9))
      speed_threshold <- as.numeric(substr(response, 11,12))
      acc_threshold <- as.numeric(substr(response, 14,18))
      
    }
    else {
      speed_threshold <- as.numeric(substr(response, 10,11))
      acc_threshold <- as.numeric(substr(response,13,17))
      
    }
    model1 <-  lm(get(response)/60-10 ~ Temperature + I(Temperature^2) + log(Groupsize,2) + Loom,my_new_data2)
    matrix_data[ii,5:9] <- model1$coefficients[1:5]
    matrix_data[ii,11:15] <- coef(summary(model1))[,4][1:5]
    
    
  }
  if(i>=109 & i<208){
    
    startle_threshold <- as.numeric(substr(response, 14,14))
    if(startle_threshold < 5){
      startle_threshold <- as.numeric(substr(response, 14,15))
      speed_threshold <- as.numeric(substr(response, 17,18))
      acc_threshold <- as.numeric(substr(response, 20,23))
      
    }
    else {
      speed_threshold <- as.numeric(substr(response, 16,17))
      acc_threshold <- as.numeric(substr(response,19,22))
      
    }
    model1 <- glm(get(response) ~ Temperature + I(Temperature^2) + log(Groupsize,2) + Loom, family = binomial,sens_data)
    matrix_data[ii,5:9] <- model1$coefficients[1:5]
    matrix_data[ii,11:15] <- coef(summary(model1))[,4][1:5]
    
    
  }
  if(i>208 & i < 217){
    
    acc_threshold <- as.numeric(substr(response,9,13))
    speed_threshold <- as.numeric(substr(response,6,7))
    model1 <- lm(sqrt(get(response)) ~ Temperature + I(Temperature^2) + log(Groupsize,2) + Loom,sens_data)
    matrix_data[ii,5:9] <- model1$coefficients[1:5]
    matrix_data[ii,11:15] <- coef(summary(model1))[,4][1:5]
    
    
  }
  if(i>=217){
    
    speed_threshold <- as.numeric(substr(response,4,5))
    acc_threshold <- as.numeric(substr(response,7,11))
    model1 <- lm(log(get(response)+1) ~ Temperature + I(Temperature^2) + log(Groupsize,2) + Loom + Temperature*log(Groupsize,2),sens_data)
    matrix_data[ii,5:10] <- model1$coefficients[1:6]
    matrix_data[ii,11:16] <- coef(summary(model1))[,4][1:6]
    
    
  }
  matrix_data[ii,1] <- response
  matrix_data[ii,2] <- startle_threshold
  matrix_data[ii,3] <- speed_threshold
  matrix_data[ii,4] <- acc_threshold
  ii = ii +1
  
  # 
  # model_glm6 <-  glm(get(response) ~ Temperature + I(Temperature^2) + Loom, family = binomial,sens_data)
  # matrix_data[ii,1:4] <- model_glm6$coefficients[1:4]
  # matrix_data[ii,5:8] <- coef(summary(model_glm6))[,4][1:4]
  # matrix_data[ii,9] <-model_glm6$null.deviance
  # matrix_data[ii,10] <-model_glm6$deviance
  # matrix_data[ii,11] <-model_glm6$aic
  # matrix_data[ii,12] <- startle_threshold
  # matrix_data[ii,13] <- speed_threshold
  # ii = ii +1
  
}

outcome <- as.data.frame(matrix_data)
colnames(outcome)  <- c("x","startle","speed","acc","Intercept","T","T2","GS","Loom","int","p_int","p_t", "p_t2","p_gs", "p_loom","p_interaction")


for(i in 2:14){
  outcome[,i] <- as.numeric(as.character(outcome[,i]))
}

list1 <- grep("^lat",outcome$x)
list2 <- grep("^prop",outcome$x)
list3 <- grep("^speed",outcome$x)
list4 <- grep("^acc",outcome$x)


plot(outcome$startle[list1],outcome$T[list1])
plot(outcome$startle[list2],outcome$T[list2])
plot(outcome$speed[list3],outcome$T[list3])

par(mar = c(4.1, 4.4, 4.1, 1.9))

mean(outcome$T[grepl(a,outcome$x)][outcome$startle==12], na.rm=TRUE)


matrix_t <- matrix(data= NA, nrow = 4, ncol = 3)
matrix_tt <- matrix(data= NA, nrow = 4, ncol = 3)
matrix_gs <- matrix(data= NA, nrow = 4, ncol = 3)
matrix_loom <- matrix(data= NA, nrow = 4, ncol = 3)
matrix_int <- matrix(data= NA, nrow = 4, ncol = 3)

matrix_p_t <- matrix(data= NA, nrow = 4, ncol = 3)
matrix_p_tt <- matrix(data= NA, nrow = 4, ncol = 3)#, colnames = c("Intercept","T","T2","GS",Loom","int",p_int","p_t", "p_t2","p_gs", "p_loom","p_int"))
matrix_p_gs <- matrix(data= NA, nrow = 4, ncol = 3)
matrix_p_loom <- matrix(data= NA, nrow = 4, ncol = 3)
matrix_p_int <- matrix(data= NA, nrow = 4, ncol = 3)

rownames(matrix_t) <- c("Latency","Proportion","Speed","Acceleration")
rownames(matrix_p_t) <- c("Latency","Proportion","Speed","Acceleration")

colnames(matrix_t) <- c("Startle","Speed","Acceleration")
colnames(matrix_p_t) <- c("Startle","Speed","Acceleration")


for(i in 1:4){
  if(i ==1){
    a <- "^lat"
  }
  if(i ==2){
    a <- "^prop"
  }
  if(i ==3){
    a <- "^speed"
  }
  if(i ==4){
    a <- "^acc"
  }
  
  for(j in 1:3){
    if(j ==1){
      t2 <- 12
      t1 <- 10
    }
    if(j ==2){
      t2 <- 35
      t1 <- 30
    }
    if(j ==3){
      t2 <- 4000
      t1 <- 3000
    }
    for( k  in 1:5){
      v1 = mean(outcome[,k+1][grepl(a,outcome$x)][outcome[,j+1]==t1], na.rm=TRUE)
      v2 = mean(outcome[,k+1][grepl(a,outcome$x)][outcome[,j+1]==t2], na.rm=TRUE)
      
      v1_p = mean(outcome[,k+11][grepl(a,outcome$x)][outcome[,j+1]==t1], na.rm=TRUE)
      v2_p = mean(outcome[,k+11][grepl(a,outcome$x)][outcome[,j+1]==t2], na.rm=TRUE)
      
      matrix_t[i,j] <- ((v2-v1)/v1)/((t2-t1)/t1)
      print(matrix_t)
      matrix_p_t[i,j] <- ((v2_p-v1_p)/v1_p)/((t2-t1)/t1)
      print(matrix_p_t)
    }
    v1 = mean(outcome$T[grepl(a,outcome$x)][outcome[,j+1]==t1], na.rm=TRUE)
    v2 = mean(outcome$T[grepl(a,outcome$x)][outcome[,j+1]==t2], na.rm=TRUE)
    
    v1_tt = mean(outcome$T2[grepl(a,outcome$x)][outcome[,j+1]==t1], na.rm=TRUE)
    v2_tt = mean(outcome$T2[grepl(a,outcome$x)][outcome[,j+1]==t2], na.rm=TRUE)
    
    v1_gs = mean(outcome$GS[grepl(a,outcome$x)][outcome[,j+1]==t1], na.rm=TRUE)
    v2_gs = mean(outcome$GS[grepl(a,outcome$x)][outcome[,j+1]==t2], na.rm=TRUE)
    
    v1_loom = mean(outcome$Loom[grepl(a,outcome$x)][outcome[,j+1]==t1], na.rm=TRUE)
    v2_loom = mean(outcome$Loom[grepl(a,outcome$x)][outcome[,j+1]==t2], na.rm=TRUE)
    
    v1_int = mean(outcome$int[grepl(a,outcome$x)][outcome[,j+1]==t1], na.rm=TRUE)
    v2_int = mean(outcome$int[grepl(a,outcome$x)][outcome[,j+1]==t2], na.rm=TRUE)
    
    matrix_t[i,j] <- ((v2-v1)/v1)/((t2-t1)/t1)
    matrix_tt[i,j] <- ((v2_tt-v1_tt)/v1_tt)/((t2-t1)/t1)
    matrix_gs[i,j] <- ((v2_gs-v1_gs)/v1_gs)/((t2-t1)/t1)
    matrix_loom[i,j] <- ((v2_loom-v1_loom)/v1_loom)/((t2-t1)/t1)
    matrix_int[i,j] <- ((v2_int-v1_int)/v1_int)/((t2-t1)/t1)
  }
}
print(matrix_t)
# print(matrix_tt)
# print(matrix_gs)
# print(matrix_loom)
# print(matrix_int)

sig <- c("Temperature", "Significance of Temperature","Temperature^2", 
         "Significance of Temperature^2","Group size", 
         "Significance of Group size","Loom", "Significance of Loom",
         "Interaction", "Significance of Interaction")

for( k  in 1:5){
  
  matrix_t <- matrix(data= NA, nrow = 4, ncol = 3)
  matrix_p_t <- matrix(data= NA, nrow = 4, ncol = 3)
  
  for(i in 1:4){
    if(i ==1){
      a <- "^lat"
    }
    if(i ==2){
      a <- "^prop"
    }
    if(i ==3){
      a <- "^speed"
    }
    if(i ==4){
      a <- "^acc"
    }
    
    for(j in 1:3){
      if(j ==1){
        t2 <- 12
        t1 <- 10
      }
      if(j ==2){
        t2 <- 35
        t1 <- 30
      }
      if(j ==3){
        t2 <- 4000
        t1 <- 3000
      }
      
      v1 = mean(outcome[,k+5][grepl(a,outcome$x)][outcome[,j+1]==t1], na.rm=TRUE)
      v2 = mean(outcome[,k+5][grepl(a,outcome$x)][outcome[,j+1]==t2], na.rm=TRUE)
      
      v1_p = mean(outcome[,k+11][grepl(a,outcome$x)][outcome[,j+1]==t1], na.rm=TRUE)
      v2_p = mean(outcome[,k+11][grepl(a,outcome$x)][outcome[,j+1]==t2], na.rm=TRUE)
      
      matrix_t[i,j] <- ((v2-v1)/v1)/((t2-t1)/t1)
      
      matrix_p_t[i,j] <- ((v2_p-v1_p)/v1_p)/((t2-t1)/t1)
    }
  }
  
  rownames(matrix_t) <- c("Latency","Proportion","Speed","Acceleration")
  rownames(matrix_p_t) <- c("Latency","Proportion","Speed","Acceleration")
  
  colnames(matrix_t) <- c("Startle","Speed","Acceleration")
  colnames(matrix_p_t) <- c("Startle","Speed","Acceleration")
  print(sig[k*2-1])
  print(matrix_t)
  print(sig[k*2])
  print(matrix_p_t)
}

