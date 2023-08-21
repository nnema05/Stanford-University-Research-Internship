
### BASELINE DATA OF Area Deprivation Index (ADI) scores
  ## ADI is Composite index of a census tract’s socioeconomic disadvantage based on income, education, employment, and housing quality using data from the American Community Survey
  ## #A weighted average of ADI scores was computed based on months lived at each residence.
## This program is reading in ADI scores from ABCD dataset
## This data is used to create a final data set of this information organized in a meaningful way
## This is being done for ADI scores from BASELINE!




#####STEP 0: LOAD LIBRARIES
library(tidyverse)
library(psych)
library(GPArotation)
library(dplyr)
library(base)
library(naniar)
library(matrixStats)

#File: abcd_rhds01
#####STEP 1: READ IN DATA
rhds01 <- read.delim("/PATH//abcd_rhds01_d.txt", sep="\t")
print("Read data")

####Seperate into timelines 
#baseline_year_1_arm_1
rhds01 <- rhds01 %>% 
  filter(eventname == "baseline_year_1_arm_1")
print("Different timeline")

##deal with NA
rhds01 <- rhds01 %>%
  replace_with_na_all(condition = ~.x == "")

#make numeric
rhds01[ , 10:279] <- apply(rhds01[ , 10:279], 2,
                          function(x) as.numeric(as.character(x))) 




#####STEP 2 : WEIGHTED AVERAGE ADRESS 1
#an object containing the values whose weighted mean is to be computed.
adiScoresAdr1 <- rhds01 %>% 
  select(reshist_addr1_adi_edu_l
         , reshist_addr1_adi_edu_h
         , reshist_addr1_adi_work_c
         , reshist_addr1_adi_income
         , reshist_addr1_adi_in_dis
         , reshist_addr1_adi_home_v
         , reshist_addr1_adi_rent
         , reshist_addr1_adi_mortg
         , reshist_addr1_adi_home_o
         , reshist_addr1_adi_crowd
         , reshist_addr1_adi_unemp
         , reshist_addr1_adi_pov
         , reshist_addr1_adi_b138
         , reshist_addr1_adi_sp
         , reshist_addr1_adi_ncar
         , reshist_addr1_adi_ntel
         , reshist_addr1_adi_nplumb
         , reshist_addr1_adi_wsum
         , reshist_addr1_adi_perc)
print("ADI scores made")

#a numerical vector of weights 
monthsAdr1 <- rhds01 %>% 
  select(reshist_addr1_duration)
monthsAdr1 <- rep(monthsAdr1, times = ncol(adiScoresAdr1))
#monthsAdr1 <- as.vector(monthsAdr1)
print("Months made")

##weighted average 
adiScoresAdr1 <- as.matrix(adiScoresAdr1)
adiAdr1 <- rowWeightedMeans(adiScoresAdr1, w = rhds01$monthsAdr1, na.rm = TRUE)
print("weighted average done")



#####STEP 2 : WEIGHTED AVERAGE ADRESS 2
#an object containing the values whose weighted mean is to be computed.
adiScoresAdr2 <- rhds01 %>% 
  select(reshist_addr2_adi_edu_l
         , reshist_addr2_adi_edu_h
         , reshist_addr2_adi_work_c
         , reshist_addr2_adi_income
         , reshist_addr2_adi_in_dis
         , reshist_addr2_adi_home_v
         , reshist_addr2_adi_rent
         , reshist_addr2_adi_mortg
         , reshist_addr2_adi_home_o
         , reshist_addr2_adi_crowd
         , reshist_addr2_adi_unemp
         , reshist_addr2_adi_pov
         , reshist_addr2_adi_b138
         , reshist_addr2_adi_sp
         , reshist_addr2_adi_ncar
         , reshist_addr2_adi_ntel
         , reshist_addr2_adi_nplumb
         , reshist_addr2_adi_wsum
         , reshist_addr2_adi_perc)
print("ADI scores made")

#a numerical vector of weights 
monthsAdr2 <- rhds01 %>% 
  select(reshist_addr2_duration)
monthsAdr2 <- as.vector(monthsAdr2)
print("Months made")

##weighted average 
adiScoresAdr2 <- as.matrix(adiScoresAdr2)
adiAdr2 <- weighted.mean(adiScoresAdr2, w = monthsAdr2, na.rm = TRUE)
print("weighted average done")



#####STEP 2 : WEIGHTED AVERAGE ADRESS 2
#an object containing the values whose weighted mean is to be computed.
adiScoresAdr3 <- rhds01 %>% 
  select(reshist_addr3_adi_edu_l
         , reshist_addr3_adi_edu_h
         , reshist_addr3_adi_work_c
         , reshist_addr3_adi_income
         , reshist_addr3_adi_in_dis
         , reshist_addr3_adi_home_v
         , reshist_addr3_adi_rent
         , reshist_addr3_adi_mortg
         , reshist_addr3_adi_home_o
         , reshist_addr3_adi_crowd
         , reshist_addr3_adi_unemp
         , reshist_addr3_adi_pov
         , reshist_addr3_adi_b138
         , reshist_addr3_adi_sp
         , reshist_addr3_adi_ncar
         , reshist_addr3_adi_ntel
         , reshist_addr3_adi_nplumb
         , reshist_addr3_adi_wsum
         , reshist_addr3_adi_perc)
print("ADI scores made")

#a numerical vector of weights 
monthsAdr3 <- rhds01 %>% 
  select(reshist_addr3_duration)
monthsAdr3 <- as.vector(monthsAdr3)
print("Months made")

##weighted average 
adiScoresAdr3 <- as.matrix(adiScoresAdr3)
adiAdr3 <- weighted.mean(adiScoresAdr3, w = monthsAdr3, na.rm = TRUE)
print("weighted average done")


#####STEP 3: COMBINE BY TAKING MEAN INTO ONE COLUMN
adi_weightavg_kind <- cbind(adiAdr1, adiAdr2, adiAdr3)
adi_weightavg_kind <- as.data.frame(adi_weightavg_kind)
adi_weightavg_kind <- rowMeans(adi_weightavg_kind)
adi_weightavg_kind <- cbind(rhds01$subjectkey, rhds01$eventname)


#####STEP 4: WRITE INTO CSV
write.csv(adi_weightavg_kind, "Baseadi_weightavg_kind.csv")


