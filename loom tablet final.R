library(loomeR)

#use constant speed model

loom1 <- constant_speed_model(
  screen_distance = 20,    # How far from the screen is your observing specimen?
  frame_rate = 30,         # Frame rate you want the final animation to be 
  speed = 500,             # Speed of the simulated oncoming object
  attacker_diameter = 50,  # Diameter of the simulated oncoming object
  start_distance = 5000)   # Starting distance of the simulated oncoming object
setwd('C:/Users/Maria Kuruvilla/Documents/QERM/fish project/loom tablet final')


looming_animation(loom1,
                  frame_number = TRUE,
                  frame_number_size = 2,
                  width = 976,
                  height = 610,
                  frame_number_tag = "A",
                  filename = "animation_01",
                  pad = 600)



looming_animation(loom1,
                  frame_number = TRUE,
                  frame_number_size = 2,
                  width = 976,
                  height = 610,
                  frame_number_tag = "B",
                  filename = "animation_02",
                  pad = 180)




looming_animation(loom1,
                  frame_number = TRUE,
                  frame_number_size = 2,
                  width = 976,
                  height = 610,
                  frame_number_tag = "C",
                  filename = "animation_03",
                  pad = 180)




looming_animation(loom1,
                  frame_number = TRUE,
                  frame_number_size = 2,
                  width = 976,
                  height = 610,
                  frame_number_tag = "D",
                  filename = "animation_04",
                  pad = 180)




looming_animation(loom1,
                  frame_number = TRUE,
                  frame_number_size = 2,
                  width = 976,
                  height = 610,
                  frame_number_tag = "E",
                  filename = "animation_05",
                  pad = 180)




looming_animation(loom1,
                  frame_number = TRUE,
                  frame_number_size = 2,
                  width = 976,
                  height = 610,
                  frame_number_tag = "F",
                  filename = "animation_06",
                  pad = 600)

setwd('C:/Users/Maria Kuruvilla/Documents/QERM/fish project/loom tablet final')
system("ffmpeg -y -safe 0 -f concat -i mylist.txt -c copy complete.mp4")