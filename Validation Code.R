## Selecting sites to withold for model validation

## Author: I Shuman
## Date: December 19 2022
## Prepare workspace
rm(list = ls())


#Select Validation Sites#
#Largest site (Shawnee) was originally selected, it was replaced because it is much larger than the other sites

sample(1:22, 7, replace = FALSE)

#Confirm that species are represented in the rest of the dataset

setwd("D:/Data for Chris/GJAM Formatting/Illinois Output")

IL_Forest1 <- read.csv("IL_Forest1_Y_DataStructure.csv")
IL_Mixed1 <- read.csv("IL_Mixed1_Y_DataStructure.csv")
IL_Prairie1 <- read.csv("IL_Prairie1_Y_DataStructure.csv")
IL_Prairie2 <- read.csv("IL_Prairie2_Y_DataStructure.csv")
IL_Prairie3 <- read.csv("IL_Prairie3_Y_DataStructure.csv")
IL_River1 <- read.csv("IL_River1_Y_DataStructure.csv")
IL_River2 <- read.csv("IL_River2_Y_DataStructure.csv")
IL_Shawnee <- read.csv("IL_Shawnee_Y_DataStructure.csv")
IL_Small1 <- read.csv("IL_Small1_Y_DataStructure.csv")
IL_Small2 <- read.csv("IL_Small2_Y_DataStructure.csv")
IL_Small3 <- read.csv("IL_Small3_Y_DataStructure.csv")
IL_StudyArea <- read.csv("IL_StudyArea_Y_DataStructure.csv")

setwd("D:/Data for Chris/GJAM Formatting/Indiana Output")

IN_Dunes <- read.csv("IN_Dunes_Y_DataStructure.csv")
IN_Forest1 <- read.csv("IN_Forest1_Y_DataStructure.csv")
IN_Forest2 <- read.csv("IN_Forest2_Y_DataStructure.csv")
IN_Forest3 <- read.csv("IN_Forest3_Y_DataStructure.csv")
IN_Forest4 <- read.csv("IN_Forest4_Y_DataStructure.csv")
IN_HoosierNorth <- read.csv("IN_HoosierNorth_Y_DataStructure.csv")
IN_HoosierSouth <- read.csv("IN_HoosierSouth_Y_DataStructure.csv")
IN_Indianapolis <- read.csv("IN_Indianapolis_Y_DataStructure.csv")
IN_NE <- read.csv("IN_NE_Y_DataStructure.csv")
IN_Prairie1 <- read.csv("IN_Prairie1_Y_DataStructure.csv")






