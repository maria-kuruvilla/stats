# Data
GS_1_T_9_rep_1 <- read_csv('../../data/temp_collective/roi/GS_1_T_9_rep_1.csv')

ggplot(data = GS_1_T_9_rep_1, aes(x= speed+1)) +
  geom_histogram(binwidth = 0.05,fill = "cornflowerblue", color = "black") + 
  scale_y_log10(expand = c(0,0), breaks = c(1, 2, 3, 10, 100, 1000, 10000),
                limits = c(.9, 30000),
                name = "Count") +
  scale_x_log10(#limits = c(0.0001, 100),
    breaks = c(1.001, 1.01, 1.1, 2, 11, 101),
    labels = c("1.001", "1.01", "1.1", "2", "11", "101"),
    name = "Body Length/Second")  +
  annotate("text", x = 2, size = 6, y = 1000, hjust = 0,  fontface = "bold",label = "Top 0.1%", color = "red3") +
  geom_histogram(data = GS_1_T_9_rep_1 %>% slice_max(speed, prop = 0.001), aes(x= speed+1),
                 binwidth = 0.05, fill = "firebrick1", color = "black")

ggplot(data = GS_1_T_9_rep_1, aes(x= speed)) +
  geom_histogram(binwidth = 0.05,fill = "cornflowerblue", color = "black") + 
  scale_y_log10(expand = c(0,0), breaks = c(1, 2, 3, 10, 100, 1000, 10000),
                limits = c(.9, 30000),
                name = "Count") +
  scale_x_log10(#limits = c(0.0001, 100),
    breaks = c(0.001, 0.01, 0.1, 1, 2, 5, 10, 100),
    labels = c("0.001", "0.01", "0.1", "1", "2","5", "10", "100"),
    name = "Body Length/Second")  +
  annotate("text", x = 2, size = 6, y = 1000, hjust = 0,  fontface = "bold",label = "Top 0.1%", color = "red3") +
  geom_histogram(data = GS_1_T_9_rep_1 %>% slice_max(speed, prop = 0.001), aes(x= speed),
                 binwidth = 0.05, fill = "firebrick1", color = "black")


model_speed99 <- lm((X99_speed)^0.5 ~  Temperature  +I(Temperature^2)+ log(Groupsize,2) + I(log(Groupsize,2)^2) + loom ,percentiles)
summary(model_speed99)
plot(fitted(model_speed99), residuals(model_speed99))

qqnorm(residuals(model_speed99))
qqline(residuals(model_speed99))


ggplot(percentiles,aes(x=Temperature, y = X99_speed,color = Groupsize)) + 
  geom_point(aes(y = X99_speed), shape = 16) +
  stat_smooth(aes(y = X99_speed),method = "lm", formula = y ~ x + I(x^2), size = 1) +
  ylab("99th percentile of Speed")



model_speed99 <- lm((X99_speed)^0.5 ~  I(1/Temperature)  +I(Temperature^2)+ log(Groupsize,2) + I(log(Groupsize,2)^2) + loom ,percentiles)
summary(model_speed99)
plot(fitted(model_speed99), residuals(model_speed99))


