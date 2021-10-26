sens_data <- read.csv("../../data/temp_collective/roi/sensitivity_analysis_prop_ind_startles.csv",header=TRUE,na.strings=c("[nan]"))

matrix_data <- matrix(data= NA, nrow = length(10:97), ncol = 13)#, colnames = c("Intercept","T","T2","Loom","p_int","p_t", "p_t2", "p_loom","null","residual","aic"))
ii = 1
for(i in 10:97){
  response <- colnames(sens_data)[i]
  startle_threshold <- as.numeric(substr(response, 14,14))
  if(startle_threshold < 5){
    startle_threshold <- as.numeric(substr(response, 14,15))
    speed_threshold <- as.numeric(substr(response, 17,18))
    if(speed_threshold < 30){
      speed_threshold <- as.numeric(substr(response, 17,19))
    }
  }
  else { 
    speed_threshold <- as.numeric(substr(response, 16,17))
    if(speed_threshold < 30){
      speed_threshold <- as.numeric(substr(response, 16,18))
    }
  }
    
  model_glm6 <-  glm(get(response) ~ Temperature + I(Temperature^2) + Loom, family = binomial,sens_data)
  matrix_data[ii,1:4] <- model_glm6$coefficients[1:4]
  matrix_data[ii,5:8] <- coef(summary(model_glm6))[,4][1:4]
  matrix_data[ii,9] <-model_glm6$null.deviance
  matrix_data[ii,10] <-model_glm6$deviance
  matrix_data[ii,11] <-model_glm6$aic
  matrix_data[ii,12] <- startle_threshold
  matrix_data[ii,13] <- speed_threshold
  ii = ii +1
  
}

outcome <- as.data.frame(matrix_data)
colnames(outcome)  <- c("Intercept","T","T2","Loom","p_int","p_t", "p_t2", "p_loom","null","residual","aic","startle_threshold","speed_threshold")

############ 2nd dataset ##################

sens_data2 <- read.csv("../../data/temp_collective/roi/sensitivity_analysis_prop_ind_startles_2.csv",header=TRUE,na.strings=c("[nan]"))

matrix_data <- matrix(data= NA, nrow = length(10:30), ncol = 12)#, colnames = c("Intercept","T","T2","Loom","p_int","p_t", "p_t2", "p_loom","null","residual","aic"))
ii = 1
for(i in 10:30){
  response <- colnames(sens_data2)[i]
  startle_threshold <- as.numeric(substr(response, 14,14))
  if(startle_threshold < 5){
    startle_threshold <- as.numeric(substr(response, 14,15))
  
  }
  
  model_glm6 <-  glm(get(response) ~ Temperature + I(Temperature^2) + Loom, family = binomial,sens_data2)
  matrix_data[ii,1:4] <- model_glm6$coefficients[1:4]
  matrix_data[ii,5:8] <- coef(summary(model_glm6))[,4][1:4]
  matrix_data[ii,9] <-model_glm6$null.deviance
  matrix_data[ii,10] <-model_glm6$deviance
  matrix_data[ii,11] <-model_glm6$aic
  matrix_data[ii,12] <- startle_threshold
  
  ii = ii +1
  
}

outcome <- as.data.frame(matrix_data)
colnames(outcome)  <- c("Intercept","T","T2","Loom","p_int","p_t", "p_t2", "p_loom","startle_threshold")



