library(tidyverse)
library(scales) 
library(grid)

#Plot theme
theme_plot <- theme(panel.grid = element_blank(), 
                    aspect.ratio = .75,
                    axis.text = element_text(size = 12, color = "black"), 
                    axis.ticks.length=unit(0.1,"cm"),
                    axis.title = element_text(size = 12),
                    axis.title.y = element_text(margin = margin(r = 5)),
                    axis.title.x = element_text(margin = margin(t = 5)),
                    axis.title.x.top = element_text(margin = margin(b = 5)),
                    plot.title = element_text(size = 15),
                    panel.border = element_rect(colour = "black", fill=NA),
                    panel.background = element_blank(),
                    strip.background = element_blank(),
                    legend.position = "none",
                    text = element_text(family = 'Helvetica'))
# with legend
theme_plot_leg <- theme(panel.grid = element_blank(), 
                    aspect.ratio = .75,
                    axis.text = element_text(size = 12, color = "black"), 
                    axis.ticks.length=unit(0.1,"cm"),
                    axis.title = element_text(size = 12),
                    axis.title.y = element_text(margin = margin(r = 5)),
                    axis.title.x = element_text(margin = margin(t = 5)),
                    axis.title.x.top = element_text(margin = margin(b = 5)),
                    plot.title = element_text(size = 15),
                    panel.border = element_rect(colour = "black", fill=NA),
                    panel.background = element_blank(),
                    strip.background = element_blank(),
                    legend.position = "right",
                    text = element_text(family = 'Helvetica'))

#################### Some Individual plots - Read One at a Time ###################
# Data
GS_1_T_9_rep_1 <- read_csv('/Users/jgradym/Downloads/GS_1_T_9_rep_1.csv')
str(GS_1_T_9_rep_1)
unique(GS_1_T_9_rep_1$Individual)

loom <- read_csv('/Users/jgradym/Downloads/metadata_w_loom_jg.csv')


#Individual Plots

# speed distribution; top 0.1% is highlighted; x and y log transformed


ggplot(data = GS_1_T_9_rep_1, aes(x= speed)) +
  geom_histogram(binwidth = 0.05,fill = "cornflowerblue", color = "black") + 
  scale_y_log10(expand = c(0,0), breaks = c(1, 2, 3, 10, 100, 1000, 10000),
                limits = c(.9, 30000),
                name = "Count") +
  scale_x_log10(#limits = c(0.0001, 100),
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
    labels = c("0.001", "0.01", "0.1", "1", "10", "100"),
    name = "Body Length/Second")  +
  annotate("text", x = 2, size = 6, y = 1000, hjust = 0,  fontface = "bold",label = "Top 0.1%", color = "red3") +
  geom_histogram(data = GS_1_T_9_rep_1 %>% slice_max(speed, prop = 0.001), aes(x= speed),
                 binwidth = 0.05, fill = "firebrick1", color = "black") + 
  theme_plot

# count (y) is not transformed
ggplot(data = GS_1_T_9_rep_1, aes(x= speed)) +
  geom_histogram(binwidth = 0.05,fill = "cornflowerblue", color = "black") + 
  scale_y_continuous(expand = c(0,0), #breaks = c(1, 2, 3, 10, 100, 1000),
                     #limits = c(0, 10), 
                     name = "Count") +
  scale_x_log10(#limits = c(0.0001, 100),
    breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
    labels = c("0.001", "0.01", "0.1", "1", "10", "100"),
    name = "Body Length/Second")  +
  annotate("text", x = 2, size = 6, y = 1000, hjust = 0,  fontface = "bold",label = "Top 0.1%", color = "red3") +
  geom_histogram(data = GS_1_T_9_rep_1 %>% slice_max(speed, prop = 0.001), aes(x= speed),
                 binwidth = 0.05, fill = "firebrick1", color = "black") + 
  theme_plot

# Speed over time
   # get loom pulses
loom_GS_1_T_9_rep_1 <- loom %>% 
  filter(id == "GS_1_T_9_rep_1") %>%
  select(9:13) %>%
  pivot_longer(cols = c(1:5)) %>%
  pull(value)
loom_GS_1_T_9_rep_1


ggplot(data = GS_1_T_9_rep_1,  aes(x = Frame, y= speed)) +
  geom_point(shape = 21, stroke = .1, color = "cornflowerblue") + 
  geom_point(data = GS_1_T_9_rep_1 %>% slice_max(speed, prop = 0.001),
             aes(x = Frame, y= speed), 
             shape = 21, fill = "firebrick1", color = "firebrick4") + 
  geom_vline(xintercept = loom_GS_1_T_9_rep_1, color = "red" ) + 
  scale_y_log10(name = "Speed", 
                # limit = c(0.01, 3),
                limit = c(0.003, 20),
                breaks = c(0.01, 0.1, 1, 10),
                labels = c("0.01", "0.1", "1", "10"))+
  scale_x_continuous( ) +
  #geom_line(size = .1) +
  geom_smooth(method = "loess", span = 0.02, se = F, size = 1) +
  theme_plot
  
 # limit to near loom, to reduce data load

ggplot(data = GS_1_T_9_rep_1 %>% 
         filter(near(Frame, loom_GS_1_T_9_rep_1, 1000)),  
       aes(x = Frame, y= speed)) +
  geom_point(shape = 21, stroke = .5, color = "cornflowerblue") + 
  geom_vline(xintercept = loom_GS_1_T_9_rep_1, color = "red" ) + 
  scale_y_log10(name = "Speed", 
                #limit = c(0.003, 20),
                breaks = c(0.01, 0.1, 1, 10),
                labels = c("0.01", "0.1", "1", "10"))+
  scale_x_continuous() +
  theme_plot

# Acceleration distribution 
ggplot(data = GS_1_T_9_rep_1 %>% filter(acceleration > 0.001), aes(x= acceleration)) +
  geom_histogram(binwidth = 0.05,fill = "cornflowerblue", color = "black") + 
  scale_y_log10(expand = c(0,0), breaks = c(1, 2, 3, 10, 100, 1000, 10000),
                limits = c(1, 10000), name = "Count") +
  scale_x_log10(limits = c(0.01, 1000),
                breaks = c(0.1,  1, 10, 100, 1000),
                labels = c("0.1", "1", "10", "100", "1000"),
                name = "Acceleration")  +
  geom_histogram(data = GS_1_T_9_rep_1 %>% slice_max(acceleration, prop = 0.001), aes(x= acceleration),
                 binwidth = 0.05, fill = "firebrick1", color = "black") + 
  theme_plot



# Acceleration Time series
ggplot(data = GS_1_T_9_rep_1, aes(x = Frame, y= acceleration)) +
  geom_point(shape = 21, stroke = 0.1, color = "cornflowerblue") +
  geom_point(data = GS_1_T_9_rep_1 %>%slice_max(acceleration, prop = 0.001), 
             aes(x = Frame, y= acceleration), shape = 21, fill = "firebrick1") + 
  geom_vline(xintercept = loom_GS_1_T_9_rep_1, color = "red" ) + 
  theme_plot +
  scale_y_log10(limits = c(0.03, 1000),
                breaks = c(0.1, 1, 10, 100, 1000), 
                labels = c("0.1", "1", "10", "100","1000"),   
                name = "Acceleration") 

# loom frames
loom <- read_csv('/Users/jgradym/Downloads/metadata_w_loom_jg.csv')
loom_short <- loom %>%
  select(9:14)

###############################################################
######### Practice - Test aggregating data with smaller dataset
###############################################################
speed0 <- move_files  %>%
  #set_names(file_names) %>% 
  map_dfr(read_csv, .id = "id") # 14s 
speed0 <- speed0 %>% 
  filter(id < 3)
#2,103,094 rows
speed0 <- speed %>%
  filter(row_number() <= 2103094)
system.time(speed0$id <- word(speed0$id, 1, sep = ".csv"))


speed0 <- speed0 %>% 
  filter(id < 3)
system.time(speed0<- speed0 %>%
              mutate(group_size = word(id, 2, sep = "_"),
                     temp = word(id, 4, sep = "_"),
                     rep = word(id, 6, sep = "_")) # about 7 minutes
)

# add in loom times

speed01 <- speed0 %>%
  left_join(loom_short, by ="id")
speed_reduce0 <- speed01 %>%
  filter(near(Frame, Loom_1 | Loom_2 |Loom_3 | Loom_4 | Loom_5, 1000)) # works


########################################################################
##################### all files - takes some time ##################
########################################################################

# update your filepaths as needed....
loom <- read_csv('/Users/jgradym/Downloads/metadata_w_loom_jg.csv')
loom_short <- loom %>%
  select(9:14)


move_files1 <- list.files('/Users/jgradym/Downloads/drive-download-20201206T213844Z-001/', pattern = ".csv", full.names = T)
move_files1 <- move_files1[1:51] #drop metadata
move_files2 <- list.files('/Users/jgradym/Downloads/drive-download-20201206T213844Z-002/', pattern = ".csv", full.names = T)
move_files3 <- list.files('/Users/jgradym/Downloads/drive-download-20201206T213844Z-003/', pattern = ".csv", full.names = T)
move_files4 <- list.files('/Users/jgradym/Downloads/drive-download-20210112T152004Z-001/', pattern = ".csv", full.names = T)
move_files4 <- move_files1[1:21] #drop metadata
move_files5 <- list.files('/Users/jgradym/Downloads/drive-download-20210112T152004Z-002/', pattern = ".csv", full.names = T)
move_files6 <- list.files('/Users/jgradym/Downloads/drive-download-20210112T152004Z-003/', pattern = ".csv", full.names = T)


file_names1 <- list.files('/Users/jgradym/Downloads/drive-download-20201206T213844Z-001/', pattern = ".csv", full.names = F)
file_names1 <- word(file_names1, 1, sep = ".csv")
file_names1 <- file_names1[1:51]
file_names2 <- list.files('/Users/jgradym/Downloads/drive-download-20201206T213844Z-002/', pattern = ".csv", full.names = F)
file_names2 <- word(file_names2 , 1, sep = ".csv")
file_names3 <- list.files('/Users/jgradym/Downloads/drive-download-20201206T213844Z-003/', pattern = ".csv", full.names = F)
file_names3 <- word(file_names3, 1, sep = ".csv")
file_names4 <-list.files('/Users/jgradym/Downloads/drive-download-20210112T152004Z-001/', pattern = ".csv", full.names = F)
file_names4 <- word(file_names4, 1, sep = ".csv")
file_names4 <- file_names4[1:21]
file_names5 <- list.files('/Users/jgradym/Downloads/drive-download-20210112T152004Z-002/', pattern = ".csv", full.names = F)
file_names5 <- word(file_names5, 1, sep = ".csv")
file_names6 <- list.files('/Users/jgradym/Downloads/drive-download-20210112T152004Z-003/', pattern = ".csv", full.names = F)
file_names6 <- word(file_names6, 1, sep = ".csv")

#read in names
speed1 <- move_files1  %>%
  set_names(file_names1) %>% 
  map_dfr(read_csv, .id = "id") # 14s 

#longest step
system.time(speed1 <- speed1 %>%
              mutate(group_size = word(id, 2, sep = "_"),
                     temp = word(id, 4, sep = "_"),
                     rep = word(id, 6, sep = "_")) # about 7 minutes for my computer
)
unique(speed1$id)

speed2 <- move_files2  %>%
  set_names(file_names2) %>% 
  map_dfr(read_csv, .id = "id") # 14s 
unique(speed2$id)

system.time(speed2 <- speed2 %>%
              mutate(group_size = word(id, 2, sep = "_"),
                     temp = word(id, 4, sep = "_"),
                     rep = word(id, 6, sep = "_")) # about 7 minutes
)
speed3 <- move_files3  %>%
  set_names(file_names3) %>% 
  map_dfr(read_csv, .id = "id") # 14s 


system.time(speed3 <- speed3 %>%
              mutate(group_size = word(id, 2, sep = "_"),
                     temp = word(id, 4, sep = "_"),
                     rep = word(id, 6, sep = "_")) # about 7 minutes
)

speed4 <- move_files4  %>%
  set_names(file_names4) %>% 
  map_dfr(read_csv, .id = "id") # 14s 

system.time(speed4 <- speed4 %>%
              mutate(group_size = word(id, 2, sep = "_"),
                     temp = word(id, 4, sep = "_"),
                     rep = word(id, 6, sep = "_")) # about 7 minutes
)

speed5 <- move_files5  %>%
  set_names(file_names5) %>% 
  map_dfr(read_csv, .id = "id") # 14s 

system.time(speed5 <- speed5 %>%
              mutate(group_size = word(id, 2, sep = "_"),
                     temp = word(id, 4, sep = "_"),
                     rep = word(id, 6, sep = "_")) # about 7 minutes
)

speed6 <- move_files6  %>%
  set_names(file_names6) %>% 
  map_dfr(read_csv, .id = "id") # 14s 

system.time(speed6 <- speed6 %>%
              mutate(group_size = word(id, 2, sep = "_"),
                     temp = word(id, 4, sep = "_"),
                     rep = word(id, 6, sep = "_")) # about 7 minutes
)
# combine


speed <- bind_rows(speed1, speed2, speed3, speed4, speed5, speed6) %>%
  arrange(id)
unique(speed$id)
length(unique(speed$id))
rm(speed1)
rm(speed2)
rm(speed3)
rm(speed4)
rm(speed5)
rm(speed6)

speed$group_size <- as.integer(speed$group_size)
speed$temp <- as.numeric(speed$temp)
speed$rep <- as.integer(speed$rep)
speed$one_kT <- 1/(0.00008617 * (speed$temp +273.1)) # This is Boltzmann factor/temperature (1/kT) aka inverse temperature
speed$group_size_cat <- as.factor(speed$group_size) #categorical for plotting
unique(speed$group_size)
speed <- speed %>%
  left_join(loom_short, by ="id")
library(pryr) #12.8 gigs
object_size(speed)
Speed <- speed
rm(speed)
#write_csv(speed, "~/Downloads/speed_camera_full_Jan_12_2021.csv")
#make sure speed is near loom (if you want), can reduce file size too
speed_reduce <- speed %>%
  filter(near(Frame, Loom_1 | Loom_2 |Loom_3 | Loom_4 | Loom_5, 1000)) 

###############################################
################# Analysis ####################
###############################################

# Speed top 0.1% by temp
speed_top_0.1perc <- Speed %>%
  group_by(id) %>%
  filter(speed < quantile(speed, 0.001)) %>%
  ungroup(id) %>%
  filter(near(Frame, Loom_1 | Loom_2 |Loom_3 | Loom_4 | Loom_5, 1000)) #near loom

ggplot(data = speed_top_0.1perc, aes(x = temp, y = speed)) + 
  scale_y_log10() + theme_plot +
  geom_point(shape = 21, size = 2) + geom_smooth(method = "lm", alpha = 0.2) # some non-relevant values in there, possibly negative speeds to be removed

lm_speed_0.1 <- lm(log(speed) ~ one_kT + log(group_size), data = speed_top_0.1perc)
summary(lm_speed_0.1 )

# acceleration top 0.1% by temp
accel_top_0.1perc <- Speed %>%
  group_by(id) %>%
  filter(acceleration < quantile(acceleration, 0.001)) %>%
  ungroup(id) %>%
  filter(near(Frame, Loom_1 | Loom_2 |Loom_3 | Loom_4 | Loom_5, 1000)) #near loom

ggplot(data = accel_top_0.1perc, aes(x = temp, y = acceleration)) + 
  scale_y_log10() + theme_plot +
  geom_point(shape = 21, size = 2) + geom_smooth(method = "lm", alpha = 0.2) # inludes much lower values... not obvious trend
#some low values still making it in

lm_accel_0.1 <- lm(log(acceleration) ~ one_kT + group_size, data = accel_top_0.1perc )
summary(lm_accel_0.1)

Speed <- Speed %>%
  arrange(id)

# Median Speed by temp  #filter for median not working for some reason, should be 209 rows
speed_median<- Speed %>%
  group_by(id) %>%
  filter(speed == median(speed)) %>%
  slice_head()

   #work aournd

speed_median_a <- Speed %>%
  group_by(id) %>%
  summarize(med_speed = median(speed, na.rm = T)) 

speed_median_b <- Speed %>%
  group_by(id) %>%
  filter(speed == max(speed)) %>%
  slice_head()

speed_median <- speed_median_b %>%
  left_join(speed_median_a, by = "id")
speed_median$speed <- NULL

ggplot(data = speed_median, aes(x = temp, y = med_speed)) + 
  geom_smooth(method = "lm", alpha = 0.2, color = "black") +
  scale_y_log10(name = "Median Speed") + theme_plot_leg +
  scale_x_continuous(name = "Temperature Cº") +
  #geom_point(shape = 21, size = 2, stroke = 1, aes(color = group_size_cat))  
  geom_jitter(shape = 21, size = 2, stroke = .8, aes(color = group_size_cat), width = 0.2)  
  

lm_median_speed <- lm(log(speed) ~ one_kT , data = speed_median )
summary(lm_median_speed) # hey actually significant and near expected value (slope ~0.65) - nevermind

lm_median_speed <- lm(log(med_speed) ~ one_kT , data = speed_median )
summary(lm_median_speed) # hey actually significant and near expected value (slope ~0.65) - nevermind
lm_median_speed_group <- lm(log(speed) ~ one_kT + log(group_size), data = speed_median )
summary(lm_median_speed_group) # still significant

speed_median_no_29 <- speed_median %>%
  filter(temp != 29)
lm_median_speed_no_29 <- lm(log(speed) ~ one_kT , data = speed_median_no_29)
summary(lm_median_speed_no_29) 
# use mixed model, replicate is a 
library(lme4)
library(lmerTest)
lmer_median_speed <- lmer(log(speed) ~ one_kT + group_size + (1|rep), data = speed_median)
summary(lmer_median_speed)
confint.merMod(lmer_median_speed, method = "Wald") #confidence intervals
r.squaredGLMM(lmer_median_speed)

# Max Speed by temp
speed_max <- speed %>%
  group_by(id) %>%
  filter(speed == max(speed))

ggplot(data = speed_max, aes(x = temp, y = speed)) + 
  scale_y_log10() + theme_plot +
  geom_jitter(shape = 21, size = 2, stroke = .5, aes(color = group_size_cat), width = 1)  +
  geom_smooth(method = "lm", alpha = 0.2)

lm_max_speed <- lm(log(speed) ~ one_kT + log(group_size), data = speed_max)
summary(lm_max_speed)

# treat group size a
# acceleration
  #median acceleration
accel_median <- speed %>%
  group_by(id) %>%
  filter(acceleration == median(acceleration))

ggplot(data = accel_median, aes(x = temp, y = acceleration)) + 
  scale_y_log10(name = "Median Acceleration") + theme_plot_leg +
  scale_x_continuous(name = "Temperature Cº") +
  geom_jitter(shape = 21, size = 2, stroke = 1, aes(color = group_size_cat), width = 1)   +
  geom_smooth(method = "lm", alpha = 0.2)

lm_accel_median <- lm(log(speed) ~ one_kT , data =  accel_median)
summary(lm_accel_median)

    # max acceleration  
accel_max <- speed %>%
  group_by(id) %>%
  filter(acceleration == max(acceleration))
         
ggplot(data = accel_max , aes(x = temp, y = acceleration)) + 
  scale_y_log10() + theme_plot +
  geom_point(shape = 21, size = 2) + geom_smooth(method = "lm", alpha = 0.2)

lm_accel_max <- lm(log(speed) ~ one_kT , data =  accel_max)
summary(lm_accel_max)


########################################################################
######################## Generating Plots ####################
########################################################################

theme_plot <- theme(panel.grid = element_blank(), 
                    aspect.ratio = .75,
                    axis.text = element_text(size = 10, color = "black"), 
                    axis.ticks.length=unit(0.1,"cm"),
                    axis.title = element_text(size = 10),
                    axis.title.y = element_text(margin = margin(r = 5)),
                    axis.title.x = element_text(margin = margin(t = 5)),
                    axis.title.x.top = element_text(margin = margin(b = 5)),
                    plot.title = element_text(size = 15),
                    panel.border = element_rect(colour = "black", fill=NA),
                    panel.background = element_blank(),
                    strip.background = element_blank(),
                    legend.position = "none",
                    text = element_text(family = 'Helvetica'))

# Loop for to generate plot for each replicate
# loom pulse not added but probably would be a nice touch
uniq_reps <- unique(speed$id)
for (i in uniq_reps ) {
  
  temp_plot <- ggplot(data = subset(speed, id == i), aes(x = speed)) + 
    geom_histogram(binwidth = 0.05,fill = "cornflowerblue", color = "black") + 
    scale_y_log10(expand = c(0,0), breaks = c(1, 2, 3, 10, 100, 1000, 10000),
                  limits = c(.9, NA),
                  name = "Count") +
    scale_x_log10(#limits = c(0.0001, 100),
      breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
      labels = c("0.001", "0.01", "0.1", "1", "10", "100"),
      name = "Speed")  +  
    geom_histogram(data= subset(speed, id == i) %>% slice_max(speed, prop = 0.001), aes(x= speed),
                   binwidth = 0.05, fill = "firebrick1", color = "black") + 
    annotate("text", x = 1.5, size = 3, y = 9000, hjust = 0,  fontface = "bold",label = "Top 0.1%", color = "red3") +
    theme_plot +
    ggtitle(i)
  
  ggsave(temp_plot, file = paste0("~/Desktop/Speed_Distribution/plot_", i,".pdf"), width = 14, height = 10, units = "cm")
}


