#Sample ABCD - Goal #1

#####STEP 1: OPEN DATA
install.packages(tidyverse)
library(tidyverse)
install.packages(tidyverse)
library(psych)
install.packages(GPArotation)
library(GPArotation)
install.packages(dplyr)
library(dplyr)
install.packages(ggplot2)
library(ggplot2)
install.packages(corrplot)
library(corrplot) #plotting correlation matrices
install.packages(lavaan)
library(lavaan)  #for fitting structural equation models
install.packages(semPlot)
library(semPlot)  #for automatically making diagrams
install.packages(sqldf)
library(sqldf)
install.packages(writexl)
library(writexl)

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
columnsToNumeric <- c(5:118)
#view(columnsToNumeric)

#changes 5:118 (columnsToNumeric) to numeric data but keeps everything else as characters 
cbcl_nm[ , columnsToNumeric] <- apply(cbcl_nm[ ,columnsToNumeric], 2,
                                            function(x) as.numeric(as.character(x))) 
#Remove comment below code if there is a need to troubleshoot 
#head(cbcl_nm)
#str(columnsToNumeric) #check what type of data
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


#Remove Comment below code if there is a need to troubleshoot
#ncol(cbcl_nm)
#Check # of columns: 107 - 5(subject key, interview age, sex, language, event name) = 102 


#####STEP 4. SEPERATE INTO DIFFERENT TIMELINES (baseline_year_1_arm_1 , 1_year_follow_up_y_arm_1, 2_year_follow_up_y_arm_1 )

#baseline_year_1_arm_1
baselineData <- cbcl_nm %>% 
  filter(eventname == "baseline_year_1_arm_1")
ncol(baselineData)
view(baselineData)

# 1_year_follow_up_y_arm_1
oneyearData <- cbcl_nm %>% 
  filter(eventname == "1_year_follow_up_y_arm_1")

#2_year_follow_up_y_arm_1
twoyearData <- cbcl_nm %>% 
  filter(eventname == "2_year_follow_up_y_arm_1")


#####STEP 5. PARALLEL ANALYSIS, PCA AND EFA IN BASELINE

####STEP 5a. PARALLEL ANALYSIS IN BASELINE
#To run, data must be numeric. Out of 107 columns, 5:97, 99:107 are the numeric columns 
#Remove the columns that are not numeric 
baselineDataNumeric <- select(baselineData, c(5:97, 99:107)) 

#Remove Comment below code if there is a need to troubleshoot
#str(baselineDataNumeric) 
#head(baselineDataNumeric)
#view(baselineDataNumeric) 

##Run parallel analysis  
#Use fa = pc because the article recommends finding principal components (pc) instead of principal factors (fa) 
baselineParallel <- fa.parallel(baselineDataNumeric, fa = "pc") 



####STEP 5b PCA (delineating paper stated to empirically extract with PCA) IN BASELINE
baselinePca <- princomp(baselineDataNumeric) 

#Assumption: parallel analysis gave me 16 factors, PCA gave me 5 based on delineating paper 


####STEP 5c: EFA FOR EACH FACTOR, EXTRACT AND ROTATE  IN BASELINE

#FIRST FACTOR
#Use fm = ml or maximum likelihood because this is the correct factor analysis for normally distributed data
baselineFa1 <- fa(r = baselineDataNumeric, 
                    nfactors = 1, 
                    fm = "ml",
                    residuals = TRUE) 

#evaluate loadings (need to be presence of >3 clear primary loading, greater then .1 for each factor)
#save scores for each successive number of components 
baselinefa1Scores <- baselineFa1$scores

#TWO FACTORS (now rotate with geominT)
#Rotate with geomin as stated in Delineating paper
#Use geominT instead of geominQ because geominT is orthogonal and the Goldberg method uses orthoganol rotations
baselineFa2 <- fa(r = baselineDataNumeric,
                    nfactors = 2,
                    fm = "ml",
                    rotate = "geominT",
                    residuals = TRUE) 
baselinefa2Scores <- baselineFa2$scores

#THREE FACTORS 
baselineFa3 <- fa(r = baselineDataNumeric,
                    nfactors = 3,
                    fm = "ml",
                    rotate = "geominT",
                    residuals = TRUE)
baselinefa3Scores <- baselineFa3$scores

#FOUR FACTORS 
baselineFa4 <- fa(r = baselineDataNumeric,
                    nfactors = 4,
                    fm = "ml",
                    rotate = "geominT",
                    residuals = TRUE)
baselinefa4Scores <- baselineFa4$scores

#FIVE FACTORS
baselineFa5 <- fa(r = baselineDataNumeric,
                    nfactors = 5,
                    fm = "ml",
                    rotate = "geominT",
                    residuals = TRUE)
baselinefa5scores <- baselineFa5$scores

#SIX FACTORS  
baselineFa6 <- fa(r = baselineDataNumeric,
                    nfactors = 6,
                    fm = "ml",
                    rotate = "geominT",
                    residuals = TRUE)
baselinefa6Scores <- baselineFa6$scores
#Assumption that at 6 factors and beyond it is no longer interpretable

#Names of 5 factors based on loadings: 



#####STEP 6: CORRELATION ANALYSIS IN BASELINE
#correlation between factor scores starting from the first factor at the top all the way to the last factor

#1 and 2 scores 
baselineCor1_2 <- cor.test(baselinefa1Scores, baselinefa2Scores, 
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

#2 and 3 scores
baselineCor2_3 <- cor.test(baselinefa2Scores, baselinefa3Scores,
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

#3 and 4 scores
baselineCor3_4 <- cor.test(baselinefa3Scores, baselinefa4Scores, 
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

#4 and 5 scores 
baselineCor4_5 <- cor.test(baselinefa4Scores, baselinefa5Scores, 
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

##ARE THESE FACTORS SIMILAR TO THE FACTORS FROM THE DELIENTATING STUDY?


######STEP 7: PARALLEL ANALYSIS, PCA AND EFA IN TIMELINE 1
#To run, data must be numeric. Out of 107 columns, 5:97, 99:107 are the numeric columns 
#Remove the columns that are not numeric 
oneyearDataNumeric <- select(oneyearData, c(5:97, 99:107)) 

#Remove Comment below code if there is a need to troubleshoot
#str(oneyearDataNumeric) 
#head(oneyearDataNumeric)
#view(oneyearDataNumeric) 

##Run parallel analysis  
#Use fa = pc because the article recommends finding principal components (pc) instead of principal factors (fa) 
oneyearParallel <- fa.parallel(oneyearDataNumeric, fa = "pc")

#Run PCA
oneyearPca <- princomp(oneyearDataNumeric) 
#Assumption: parallel analysis gave me 16 factors, PCA gave me 5 based on delineating paper

#EFA
#FIRST FACTOR
oneyearFa1 <- fa(r = oneyearDataNumeric, 
                   nfactors = 1, 
                   fm = "ml",
                   residuals = TRUE) 

#evaluate loadings (need to be presence of >3 clear primary loading, greater then .1 for each factor)
#save scores for each successive number of components 
oneyearfa1Scores <- oneyearFa1$scores

#TWO FACTORS (now rotate with geomin)
oneyearFa2 <- fa(r = oneyearDataNumeric,
                   nfactors = 2,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE) 
oneyearfa2Scores <- oneyearFa2$scores

#THREE FACTORS 
oneyearFa3 <- fa(r = oneyearDataNumeric,
                   nfactors = 3,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
oneyearfa3Scores <- oneyearFa3$scores

#FOUR FACTORS 
oneyearFa4 <- fa(r = oneyearDataNumeric,
                   nfactors = 4,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
oneyearfa4Scores <- oneyearFa4$scores

#FIVE FACTORS
oneyearFa5 <- fa(r = oneyearDataNumeric,
                   nfactors = 5,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
oneyearfa5scores <- oneyearFa5$scores

#SIX FACTORS  
oneyearFa6 <- fa(r = oneyearDataNumeric,
                   nfactors = 6,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
oneyearfa6Scores <- oneyearFa6$scores
#Assumption that at 6 factors and beyond it is no longer interpretable

#Names of 5 factors based on loadings: 


######STEP 8 CORRELATION ANALYSIS IN TIMELINE 1
#correlation between factor scores starting from the first factor at the top all the way to the last factor

#1 and 2 scores 
oneyearCor1_2 <- cor.test(oneyearfa2Scores, oneyearfa2Scores, 
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#2 and 3 scores
oneyearCor2_3 <- cor.test(oneyearfa2Scores, oneyearfa3Scores,
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#3 and 4 scores
oneyearCor3_4 <- cor.test(oneyearfa3Scores, oneyearfa4Scores, 
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#4 and 5 scores 
oneyearCor4_5 <- cor.test(oneyearfa4Scores, oneyearfa5Scores, 
                            method = "pearson")
#correlation coefficient or r: 
#p-value:


######STEP 9: PARALLEL ANALYSIS, PCA AND EFA IN TIMELINE 2
#To run, data must be numeric. Out of 107 columns, 5:97, 99:107 are the numeric columns 
#Remove the columns that are not numeric 
twoyearDataNumeric <- select(twoyearData, c(5:97, 99:107)) 

#Remove Comment below code if there is a need to troubleshoot
#str(twoyearDataNumeric) 
#head(twoyearDataNumeric)
#view(twoyearDataNumeric) 

##Run parallel analysis  
#Use fa = pc because the article recommends finding principal components (pc) instead of principal factors (fa) 
twoyearParallel <- fa.parallel(twoyearDataNumeric, fa = "pc")

#Run PCA
twoyearPca <- princomp(twoyearDataNumeric) 
#Assumption: parallel analysis gave me 16 factors, PCA gave me 5 based on delineating paper

#EFA
#FIRST FACTOR
twoyearFa1 <- fa(r = twoyearDataNumeric, 
                   nfactors = 1, 
                   fm = "ml",
                   residuals = TRUE) 

#evaluate loadings (need to be presence of >3 clear primary loading, greater then .1 for each factor)
#save scores for each successive number of components 
twoyearfa1Scores <- twoyearFa1$scores

#TWO FACTORS (now rotate with geominT)
twoyearFa2 <- fa(r = twoyearDataNumeric,
                   nfactors = 2,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE) 
twoyearfa2Scores <- twoyearFa2$scores

#THREE FACTORS 
twoyearFa3 <- fa(r = twoyearDataNumeric,
                   nfactors = 3,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
twoyearfa3Scores <- twoyearFa3$scores

#FOUR FACTORS 
twoyearFa4 <- fa(r = twoyearDataNumeric,
                   nfactors = 4,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
twoyearfa4Scores <- twoyearFa4$scores

#FIVE FACTORS
twoyearFa5 <- fa(r = twoyearDataNumeric,
                   nfactors = 5,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
twoyearfa5scores <- twoyearFa5$scores

#SIX FACTORS  
twoyearFa6 <- fa(r = twoyearDataNumeric,
                   nfactors = 6,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
twoyearfa6Scores <- twoyearFa6$scores
#Assumption that at 6 factors and beyond it is no longer interpretable

#Names of 5 factors based on loadings: 


######STEP 9 CORRELATION ANALYSIS IN TIMELINE 2
#correlation between factor scores starting from the first factor at the top all the way to the last factor

#1 and 2 scores 
twoyearCOR1_2 <- cor.test(twoyearfa1Scores, twoyearfa2Scores, 
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#2 and 3 scores
twoyearCOR2_3 <- cor.test(twoyearfa2Scores, twoyearfa3Scores,
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#3 and 4 scores
twoyearCOR3_4 <- cor.test(twoyearfa3Scores, twoyearfa4Scores, 
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#4 and 5 scores 
twoyearCOR4_5 <- cor.test(twoyearfa4Scores, twoyearfa5Scores, 
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#####STEP 10: Check if factors are congruent 
factorBaseLoadings <- loadings(baselineFa5)  
factorOneLoadings <- loadings(oneyearFa5)
factorTwoLoadings <- loadings(twoyearFa5)

#Factor congruence on baseline and one year
congruenceBaseOne <- factor.congruence(factorBaseLoadings,factorOneLoadings)
congruenceBaseTwo <- factor.congruence(factorBaseLoadings,factorTwoLoadings)
