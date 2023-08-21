
#Read in data


####STEP 0: INSTALL LIBRARIES 
install.packages("tidyverse")
library(tidyverse)
install.packages("psych")
library(psych)
install.packages("GPArotation")
library(GPArotation)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library(corrplot) #plotting correlation matrices
install.packages("lavaan")
library(lavaan)  #for fitting structural equation models
install.packages("semPlot")
library(semPlot)  #for automatically making diagrams
#install.packages("sqldf")
#library(sqldf)
install.packages("writexl")
library(writexl)
install.packages("fastDummies")
library(fastDummies)
install.packages("recipes")
library(recipes)
install.packages("psychTools")
library(psychTools)


######STEP 1: READ IN DATA
#Read in data
cbcl_nm <- read.delim(file.choose())
#Remove comment if need to debug 
#View(cbcl_nm)
#head(cbcl_nm)
#ncol(cbcl_nm) #119, (1:4 and 119 should not be changed to numeric)
#str(cbcl_nm)

#####STEP 2: DELETE “Drinks alcohol without parents' approval”, “Sexual problems”, “Smokes, chews, or sniffs tobacco”, “Truancy, skips school”, “Uses drugs for non-medical purposes (don't include alcohol or tobacco)”.
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q02_p, cbcl_q73_p,
                                       cbcl_q99_p, cbcl_q101_p, 
                                       cbcl_q105_p))

#####STEP 3. AGGREGATE NEEDED ITEMS INTO COMPOSITES

#removes first row to start making everything numeric
cbcl_nm <- cbcl_nm[2:29685,] 

##CONVERT character columns to numeric
#count number of columns 
ncol(cbcl_nm)

#Columns that must be numeric are 5:118 so those will become their own data.frame
columns.to.numeric <- c(5:118)
#view(columns.to.numeric)

#changes 5:118 (columns.to.numeric) to numeric data but keeps everything else as characters 
cbcl_nm[ , columns.to.numeric] <- apply(cbcl_nm[ ,columns.to.numeric], 2,
                                        function(x) as.numeric(as.character(x))) 
#Remove comment below code if there is a need to troubleshoot 
#head(cbcl_nm)
#str(columns.to.numeric) #check what type of data
#str(cbcl_nm) # should have both characters and numeric data


##CREATE composites 

#Attacks/threatens (“Physically attacks people”, “Threatens people”)
cbcl_nm$`Attacks/threatens` <- rowMeans(cbcl_nm[,c("cbcl_q57_p", "cbcl_q97_p")],
                                        na.rm = TRUE)
#view(cbcl_nm$`Attacks/threatens`)

#Destroys (“Destroys his/her own things”, “Destroys things belonging to his/her family or others”, “Vandalism”)
cbcl_nm$Destroys <- rowMeans(cbcl_nm[,c("cbcl_q20_p", "cbcl_q21_p", "cbcl_q106_p")],
                             na.rm = TRUE)
#Disobeys rules (“Disobedient at home”, “Disobedient at school”, “Breaks rules at home, school or elsewhere”)
cbcl_nm$`Disobeys rules` <- rowMeans(cbcl_nm[,c("cbcl_q22_p", "cbcl_q23_p", "cbcl_q28_p")],
                                     na.rm = TRUE)
#Steals (“Steals at home”, “Steals outside the home”)
cbcl_nm$Steals <- rowMeans(cbcl_nm[,c("cbcl_q81_p", "cbcl_q82_p")],
                           na.rm = TRUE)
# Peer problems (“Doesn't get along with other kids”, “Not liked by other kids”)
cbcl_nm$`Peer problems` <- rowMeans(cbcl_nm[,c("cbcl_q25_p", "cbcl_q48_p")],
                                    na.rm = TRUE)
#Distracted/Hyperactive (“Can't concentrate, can't pay attention for long”, “Inattentive or easily distracted”, “Can't sit still, restless, or hyperactive”)
cbcl_nm$`Distracted/Hyperactive` <- rowMeans(cbcl_nm[,c("cbcl_q08_p", "cbcl_q78_p", "cbcl_q10_p")],
                                             na.rm = TRUE)
#Hallucinations (“Hears sound or voices that aren't there”, “Sees things that aren't there”)
cbcl_nm$Hallucinations <- rowMeans(cbcl_nm[,c("cbcl_q40_p", "cbcl_q70_p")],
                                   na.rm = TRUE)
#Sex play (“Plays with own sex parts in public”, “Plays with own sex parts too much”)
cbcl_nm$`Sex play` <- rowMeans(cbcl_nm[,c("cbcl_q59_p", "cbcl_q60_p")],
                               na.rm = TRUE)
#Weight problems (“Overeating”, “Overweight”)
cbcl_nm$`Weight problems` <- rowMeans(cbcl_nm[,c("cbcl_q53_p", "cbcl_q55_p")],
                                      na.rm = TRUE)
#Remove Comment below code if there is a need to troubleshoot 
#ncol(cbcl_nm) #Check number of columns: 128  

##REMOVE the extra columns which were aggregated to composites 
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q57_p, cbcl_q97_p)) #attacks/threatens
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q20_p)) #destroys
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q21_p, cbcl_q106_p)) #destroys
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q22_p, cbcl_q23_p, cbcl_q28_p)) #Disobeys
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q81_p, cbcl_q82_p)) #steals 
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q25_p, cbcl_q48_p)) #peer problems
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q08_p, cbcl_q78_p, cbcl_q10_p)) #Distracted/Hyperactive
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q40_p, cbcl_q70_p)) #Hallucinations
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q59_p, cbcl_q60_p)) #Sex play 
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q53_p, cbcl_q55_p)) #Weight problems 

setwd("/Users/nandininema/Downloads/Sample ABCD Work")
write_xlsx(cbcl_nm, "cbcl_nm_final.xlsx")
View(cbcl_nm)

#Remove Comment below code if there is a need to troubleshoot
#ncol(cbcl_nm)
#Check # of columns: 107 - 5(subject key, interview age, sex, language, event name) = 102 


#####STEP 4. SEPERATE INTO DIFFERENT TIMELINES (baseline_year_1_arm_1 , 1_year_follow_up_y_arm_1, 2_year_follow_up_y_arm_1 )

#baseline_year_1_arm_1
baselineData <- cbcl_nm %>% 
  filter(eventname == "baseline_year_1_arm_1")
ncol(baselineData)
#view(baselineData)

# 1_year_follow_up_y_arm_1
oneyearData <-cbcl_nm %>%
  filter(eventname == "1_year_follow_up_y_arm_1")

#2_year_follow_up_y_arm_1
twoyearData <- cbcl_nm %>% 
  filter(eventname == "2_year_follow_up_y_arm_1")
