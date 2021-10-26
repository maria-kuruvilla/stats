require(here)

data <- read.csv(here("Documents","data","temp_collective","roi",
                      "sensitivity_analysis_prop_ind_startles_2.csv"),header=TRUE,na.strings=c("[nan]"))

matrix_data <- matrix(data= NA, nrow = length(10:30), ncol = 10)
ii = 1
for(i in 10:30){
  response <- colnames(data)[i]
  startle_threshold <- as.numeric(substr(response, 14,14))
  if(startle_threshold < 5){
    startle_threshold <- as.numeric(substr(response, 14,15))
    
  }
  model_glm6 <-  glm(get(response) ~ Temperature + I(Temperature^2) + log(Groupsize,2) + Loom, family = binomial,data)
  matrix_data[ii,1:5] <- model_glm6$coefficients[1:5]
  matrix_data[ii,6:9] <- coef(summary(model_glm6))[,4][1:5]
  matrix_data[ii,10] <- startle_threshold
  
  ii = ii +1
  
}
  
outcome <- as.data.frame(matrix_data)
colnames(outcome)  <- c("Intercept","T","T2","gs","Loom","p_int","p_t", "p_t2", "p_gs","p_loom","startle_threshold")


plot(outcome$startle_threshold, outcome$T)
plot(outcome$startle_threshold, outcome$T2)
plot(outcome$startle_threshold, outcome$Intercept)

plot(outcome$startle_threshold, outcome$p_loom)
