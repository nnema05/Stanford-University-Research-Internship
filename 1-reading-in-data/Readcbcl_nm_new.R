
# This program reads in the Child Behavior Checklist from the ABCD data set, that each subject completes
# Then the program cleans up the data
# 1. Items/columns (answers to certain questions/data from each child) for which frequency was too low were removed
# 2. Combined data of behavior from columns that were highly related to each other into new categorical columns called composites 
  # These composites included Attacks/threatens, Destroys, Disobeys rules, Steals, Peer problems, Distracted/Hyperactive, Hallucinations, Sex play and Weight Problems
  # This organized multiple highly related variables into more digestible or meaningful information
# 3. Separated the data into multiple data sets based on timeline the data was collected
  # Time lines: data gathered at baseline, data gathered at 1 year follow up, data gathered at 2 year follow up

####STEP 0: INSTALL LIBRARIES 
library(tidyverse)
library(psych)
library(GPArotation)
library(dplyr)
library(ggplot2)
library(corrplot) #plotting correlation matrices
library(lavaan)  #for fitting structural equation models
library(semPlot)  #for automatically making diagrams
library(sqldf)
library(writexl)
library(fastDummies)
library(recipes)
library(psychTools)
library(rmarkdown)
library(knitr)



#####STEP 1: READ DATA
#Read Script and upload
cbcl_nm <- read.delim("/PATH/abcd_cbcl01.txt")
## EXAMPLE: cbcl_nm <- read.delim("/home/users/xxx/abcd_cbcl01.txt")
#Remove comment if need to debug 
#View(cbcl_nm)
#head(cbcl_nm)
#ncol(cbcl_nm) #119, (1:4 and 119 should not be changed to numeric)
#str(cbcl_nm)

#####STEP 2: DELETE ITEMS/COLUMNS FOR WHICH FREQUENCY WAS TOO LOW
## DELETE “Drinks alcohol without parents' approval”, “Sexual problems”, “Smokes, chews, or sniffs tobacco”, “Truancy, skips school”, “Uses drugs for non-medical purposes (don't include alcohol or tobacco)”.
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q02_p, cbcl_q73_p,
                                       cbcl_q99_p, cbcl_q101_p, 
                                       cbcl_q105_p))

#####STEP 3. AGGREGATE NEEDED ITEMS INTO COMPOSITES
  ## Grouped similar data of behavior into composites like Attacks/threatens, Destroys, Disobeys rules, Steals, Peer problems, Distracted/Hyperactive, Hallucinations, Sex play and Weight Problems

#removes first row to start making everything numeric
cbcl_nm <- cbcl_nm[2:29685,] 

##CONVERT character columns to numeric
#count number of columns 
ncol(cbcl_nm)

#Columns that must be numeric are 5:118 so those will become their own data.frame
columns.to.numeric <- c(10:123)
#view(columns.to.numeric)

#changes 5:118 (columns.to.numeric) to numeric data but keeps everything else as characters 
cbcl_nm[ , columns.to.numeric] <- apply(cbcl_nm[ ,columns.to.numeric], 2,
                                        function(x) as.numeric(as.character(x))) 
#Remove comment below code if there is a need to troubleshoot 
#head(cbcl_nm)
#str(columns.to.numeric) #check what type of data
#str(cbcl_nm) # should have both characters and numeric data


##CREATE Composites 

#Attacks/threatens (“Physically attacks people”, “Threatens people”)
#cbcl_nm$AttacksThreatens <- rowMeans(cbcl_nm[,c("cbcl_q57_p", "cbcl_q97_p")],na.rm = TRUE)
#cbcl_nm$AttacksThreatens <- pmax(cbcl_nm$"cbcl_q57_p", cbcl_nm$"cbcl_q97_p")
cbcl_nm$AttacksThreatens <- round(rowMeans(cbcl_nm[,c("cbcl_q57_p", "cbcl_q97_p")],na.rm = TRUE), 0)

#view(cbcl_nm$`Attacks/threatens`)

#Destroys (“Destroys his/her own things”, “Destroys things belonging to his/her family or others”, “Vandalism”)
#cbcl_nm$Destroys <- rowMeans(cbcl_nm[,c("cbcl_q20_p", "cbcl_q21_p", "cbcl_q106_p")],na.rm = TRUE)
#cbcl_nm$Destroys <- pmax(cbcl_nm$"cbcl_q20_p", cbcl_nm$"cbcl_q21_p", cbcl_nm$"cbcl_q106_p")
cbcl_nm$Destroys <- round(rowMeans(cbcl_nm[,c("cbcl_q20_p", "cbcl_q21_p", "cbcl_q106_p")],na.rm = TRUE), 0)

#Disobeys rules (“Disobedient at home”, “Disobedient at school”, “Breaks rules at home, school or elsewhere”)
#cbcl_nm$Disobeysrules <- rowMeans(cbcl_nm[,c("cbcl_q22_p", "cbcl_q23_p", "cbcl_q28_p")],na.rm = TRUE)
#cbcl_nm$Disobeysrules <- pmax(cbcl_nm$"cbcl_q22_p", cbcl_nm$"cbcl_q23_p", cbcl_nm$"cbcl_q28_p")
cbcl_nm$Disobeysrules <- round(rowMeans(cbcl_nm[,c("cbcl_q22_p", "cbcl_q23_p", "cbcl_q28_p")],na.rm = TRUE), 0)

#Steals (“Steals at home”, “Steals outside the home”)
#cbcl_nm$Steals <- rowMeans(cbcl_nm[,c("cbcl_q81_p", "cbcl_q82_p")],na.rm = TRUE)
#cbcl_nm$Steals <- pmax(cbcl_nm$"cbcl_q81_p", cbcl_nm$"cbcl_q82_p")
cbcl_nm$Steals <- round(rowMeans(cbcl_nm[,c("cbcl_q81_p", "cbcl_q82_p")],na.rm = TRUE), 0)

# Peer problems (“Doesn't get along with other kids”, “Not liked by other kids”)
#cbcl_nm$PeerProblems <- rowMeans(cbcl_nm[,c("cbcl_q25_p", "cbcl_q48_p")],na.rm = TRUE)
#cbcl_nm$PeerProblems <- pmax(cbcl_nm$"cbcl_q25_p", cbcl_nm$"cbcl_q48_p")
cbcl_nm$PeerProblems <- round(rowMeans(cbcl_nm[,c("cbcl_q25_p", "cbcl_q48_p")],na.rm = TRUE), 0)

#Distracted/Hyperactive (“Can't concentrate, can't pay attention for long”, “Inattentive or easily distracted”, “Can't sit still, restless, or hyperactive”)
#cbcl_nm$DistractedHyperactive <- rowMeans(cbcl_nm[,c("cbcl_q08_p", "cbcl_q78_p", "cbcl_q10_p")],na.rm = TRUE)
#cbcl_nm$DistractedHyperactive <-  pmax(cbcl_nm$"cbcl_q08_p", cbcl_nm$"cbcl_q78_p", cbcl_nm$"cbcl_q10_p")
cbcl_nm$DistractedHyperactive <- round(rowMeans(cbcl_nm[,c("cbcl_q08_p", "cbcl_q78_p", "cbcl_q10_p")],na.rm = TRUE), 0)

#Hallucinations (“Hears sound or voices that aren't there”, “Sees things that aren't there”)
#cbcl_nm$Hallucinations <- rowMeans(cbcl_nm[,c("cbcl_q40_p", "cbcl_q70_p")],na.rm = TRUE)
#cbcl_nm$Hallucinations <- pmax(cbcl_nm$"cbcl_q40_p", cbcl_nm$"cbcl_q70_p")
cbcl_nm$Hallucinations <- round(rowMeans(cbcl_nm[,c("cbcl_q40_p", "cbcl_q70_p")],na.rm = TRUE), 0)

#Sex play (“Plays with own sex parts in public”, “Plays with own sex parts too much”)
#cbcl_nm$SexPlay <- rowMeans(cbcl_nm[,c("cbcl_q59_p", "cbcl_q60_p")], na.rm = TRUE)
#cbcl_nm$SexPlay <- pmax(cbcl_nm$"cbcl_q59_p", cbcl_nm$"cbcl_q60_p")
cbcl_nm$SexPlay <- round(rowMeans(cbcl_nm[,c("cbcl_q59_p", "cbcl_q60_p")], na.rm = TRUE), 0)

#Weight problems (“Overeating”, “Overweight”)
#cbcl_nm$Weightproblems <- rowMeans(cbcl_nm[,c("cbcl_q53_p", "cbcl_q55_p")], na.rm = TRUE)
#cbcl_nm$Weightproblems <- pmax(cbcl_nm$"cbcl_q53_p", cbcl_nm$"cbcl_q55_p")
cbcl_nm$Weightproblems <- round(rowMeans(cbcl_nm[,c("cbcl_q53_p", "cbcl_q55_p")], na.rm = TRUE), 0)

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


#Remove Comment below code if there is a need to troubleshoot
#ncol(cbcl_nm)
#Check # of columns: 107 - 5(subject key, interview age, sex, language, event name) = 102 

cbcl_nm <- cbcl_nm[complete.cases(cbcl_nm),]

#####STEP 4. SEPERATE DATA INTO MULTIPLE DATASETS 
  # BASED ON THE DIFFERENT TIMELINES DATA WAS COLLECTED (baseline_year_1_arm_1 , 1_year_follow_up_y_arm_1, 2_year_follow_up_y_arm_1 )

#baseline_year_1_arm_1
baselineData <- cbcl_nm %>% 
  filter(eventname == "baseline_year_1_arm_1")
colnames(baselineData) <- paste0("base", colnames(baselineData))

# 1_year_follow_up_y_arm_1
oneyearData <- cbcl_nm %>% 
  filter(eventname == "1_year_follow_up_y_arm_1")
colnames(oneyearData) <- paste0("one", colnames(oneyearData))
  
#2_year_follow_up_y_arm_1
twoyearData <- cbcl_nm %>% 
  filter(eventname == "2_year_follow_up_y_arm_1")
colnames(twoyearData) <- paste0("two", colnames(twoyearData))
