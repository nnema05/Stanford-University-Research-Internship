#Longitudnal Factor Analysis 
#Sample ABCD - Goal #1 and #2

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


#####STEP 5. EFA AND HIERCHACHAL METHOD

####STEP 5a. PARALLEL ANALYSIS 
#To run, data must be numeric. Out of 107 columns, 5:97, 99:107 are the numeric columns 
#Remove the columns that are not numeric 
baselineDataNumeric <- select(baseline.data, c(5:97, 99:107)) 

#Remove Comment below code if there is a need to troubleshoot
#str(baseline.data.numeric) 
#head(baseline.data.numeric)
#view(baseline.data.numeric) 

##Run parallel analysis  
#Use fa = pc because the article recommends finding principal components (pc) instead of principal factors (fa) 
baseline.parallel <- fa.parallel(baseline.data.numeric, fa = "pc") 



####STEP 5b PCA (delineating paper stated to empirically extract with PCA)
baseline.pca <- princomp(baseline.data.numeric) 

#Assumption: parallel analysis gave me 16 factors, PCA gave me 5 based on delineating paper 


####STEP 5c: EFA FOR EACH FACTOR, EXTRACT AND ROTATE 

#FIRST FACTOR
#Use fm = ml or maximum likelihood because this is the correct factor analysis for normally distributed data
baseline.fa.1 <- fa(r = baseline.data.numeric, 
                    nfactors = 1, 
                    fm = "ml",
                    residuals = TRUE) 

#evaluate loadings (need to be presence of >3 clear primary loading, greater then .1 for each factor)
#save scores for each successive number of components 
baselinefa1Scores <- baseline.fa.1$scores

#TWO FACTORS (now rotate with geominT)
#Rotate with geomin as stated in Delineating paper
#Use geominT instead of geominQ because geominT is orthogonal and the Goldberg method uses orthoganol rotations
baseline.fa.2 <- fa(r = baseline.data.numeric,
                    nfactors = 2,
                    fm = "ml",
                    rotate = "geominT",
                    residuals = TRUE) 
baselinefa2Scores <- baseline.fa.2$scores

#THREE FACTORS 
baseline.fa.3 <- fa(r = baseline.data.numeric,
                    nfactors = 3,
                    fm = "ml",
                    rotate = "geominT",
                    residuals = TRUE)
baselinefa3Scores <- baseline.fa.3$scores

#FOUR FACTORS 
baseline.fa.4 <- fa(r = baseline.data.numeric,
                    nfactors = 4,
                    fm = "ml",
                    rotate = "geominT",
                    residuals = TRUE)
baselinefa4Scores <- baseline.fa.4$scores

#FIVE FACTORS
baseline.fa.5 <- fa(r = baseline.data.numeric,
                    nfactors = 5,
                    fm = "ml",
                    rotate = "geominT",
                    residuals = TRUE)
baselinefa5scores <- baseline.fa.5$scores

#SIX FACTORS  
baseline.fa.6 <- fa(r = baseline.data.numeric,
                    nfactors = 6,
                    fm = "ml",
                    rotate = "geominT",
                    residuals = TRUE)
baselinefa6Scores <- baseline.fa.6$scores
#Assumption that at 6 factors and beyond it is no longer interpretable

#Names of 5 factors based on loadings: 


#####STEP 6: CORRELATION ANALYSIS
#correlation between factor scores starting from the first factor at the top all the way to the last factor

#1 and 2 scores 
baseline.cor.1_2 <- cor.test(baseline.fa.1.scores, baseline.fa.2.scores, 
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

#2 and 3 scores
baseline.cor.2_3 <- cor.test(baseline.fa.2.scores, baseline.fa.3.scores,
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

#3 and 4 scores
baseline.cor.3_4 <- cor.test(baseline.fa.3.scores, baseline.fa.4.scores, 
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

#4 and 5 scores 
baseline.cor.4_5 <- cor.test(baseline.fa.4.scores, baseline.fa.5.scores, 
                             method = "pearson")
#correlation coefficient or r: 
#p-value:


######STEP 6: PREP for longitudinal CFA by creating a new data frame where... 
#...each subject key has a baseline, one year and two year score for each question


#Two CFA will be done (one for baseline vs one year and one for baseline vs twoyear)

#New dataframe - baseline and one year 
cbclDF1 <- sqldf("SELECT base.subjectkey, base.interview_age, base.sex
,base.cbcl_q01_p AS basecbcl_q01_p
,base.cbcl_q03_p AS basecbcl_q03_p
,base.cbcl_q04_p AS basecbcl_q04_p
,base.cbcl_q05_p AS basecbcl_q05_p
,base.cbcl_q06_p AS basecbcl_q06_p
,base.cbcl_q07_p AS basecbcl_q07_p
,base.cbcl_q09_p AS basecbcl_q09_p
,base.cbcl_q11_p AS basecbcl_q11_p
,base.cbcl_q12_p AS basecbcl_q12_p
,base.cbcl_q13_p AS basecbcl_q13_p
,base.cbcl_q14_p AS basecbcl_q14_p
,base.cbcl_q15_p AS basecbcl_q15_p
,base.cbcl_q16_p AS basecbcl_q16_p
,base.cbcl_q17_p AS basecbcl_q17_p
,base.cbcl_q18_p AS basecbcl_q18_p
,base.cbcl_q19_p AS basecbcl_q19_p
,base.cbcl_q24_p AS basecbcl_q24_p
,base.cbcl_q26_p AS basecbcl_q26_p
,base.cbcl_q27_p AS basecbcl_q27_p
,base.cbcl_q29_p AS basecbcl_q29_p
,base.cbcl_q30_p AS basecbcl_q30_p
,base.cbcl_q31_p AS basecbcl_q31_p
,base.cbcl_q32_p AS basecbcl_q32_p
,base.cbcl_q33_p AS basecbcl_q33_p
,base.cbcl_q34_p AS basecbcl_q34_p
,base.cbcl_q35_p AS basecbcl_q35_p
,base.cbcl_q36_p AS basecbcl_q36_p
,base.cbcl_q37_p AS basecbcl_q37_p
,base.cbcl_q38_p AS basecbcl_q38_p
,base.cbcl_q39_p AS basecbcl_q39_p
,base.cbcl_q41_p AS basecbcl_q41_p
,base.cbcl_q42_p AS basecbcl_q42_p
,base.cbcl_q43_p AS basecbcl_q43_p
,base.cbcl_q44_p AS basecbcl_q44_p
,base.cbcl_q45_p AS basecbcl_q45_p
,base.cbcl_q46_p AS basecbcl_q46_p
,base.cbcl_q47_p AS basecbcl_q47_p
,base.cbcl_q49_p AS basecbcl_q49_p
,base.cbcl_q50_p AS basecbcl_q50_p
,base.cbcl_q51_p AS basecbcl_q51_p
,base.cbcl_q52_p AS basecbcl_q52_p
,base.cbcl_q54_p AS basecbcl_q54_p
,base.cbcl_q56a_p AS basecbcl_q56a_p
,base.cbcl_q56b_p AS basecbcl_q56b_p
,base.cbcl_q56c_p AS basecbcl_q56c_p
,base.cbcl_q56d_p AS basecbcl_q56d_p
,base.cbcl_q56e_p AS basecbcl_q56e_p
,base.cbcl_q56f_p AS basecbcl_q56f_p
,base.cbcl_q56g_p AS basecbcl_q56g_p
,base.cbcl_q56h_p AS basecbcl_q56h_p
,base.cbcl_q58_p AS basecbcl_q58_p
,base.cbcl_q61_p AS basecbcl_q61_p
,base.cbcl_q62_p AS basecbcl_q62_p
,base.cbcl_q63_p AS basecbcl_q63_p
,base.cbcl_q64_p AS basecbcl_q64_p
,base.cbcl_q65_p AS basecbcl_q65_p
,base.cbcl_q66_p AS basecbcl_q66_p
,base.cbcl_q67_p AS basecbcl_q67_p
,base.cbcl_q68_p AS basecbcl_q68_p
,base.cbcl_q69_p AS basecbcl_q69_p
,base.cbcl_q71_p AS basecbcl_q71_p
,base.cbcl_q72_p AS basecbcl_q72_p
,base.cbcl_q74_p AS basecbcl_q74_p
,base.cbcl_q75_p AS basecbcl_q75_p
,base.cbcl_q76_p AS basecbcl_q76_p
,base.cbcl_q77_p AS basecbcl_q77_p
,base.cbcl_q79_p AS basecbcl_q79_p
,base.cbcl_q80_p AS basecbcl_q80_p
,base.cbcl_q83_p AS basecbcl_q83_p
,base.cbcl_q84_p AS basecbcl_q84_p
,base.cbcl_q85_p AS basecbcl_q85_p
,base.cbcl_q86_p AS basecbcl_q86_p
,base.cbcl_q87_p AS basecbcl_q87_p
,base.cbcl_q88_p AS basecbcl_q88_p
,base.cbcl_q89_p AS basecbcl_q89_p
,base.cbcl_q90_p AS basecbcl_q90_p
,base.cbcl_q91_p AS basecbcl_q91_p
,base.cbcl_q92_p AS basecbcl_q92_p
,base.cbcl_q93_p AS basecbcl_q93_p
,base.cbcl_q94_p AS basecbcl_q94_p
,base.cbcl_q95_p AS basecbcl_q95_p
,base.cbcl_q96_p AS basecbcl_q96_p
,base.cbcl_q98_p AS basecbcl_q98_p
,base.cbcl_q100_p AS basecbcl_q100_p
,base.cbcl_q102_p AS basecbcl_q102_p
,base.cbcl_q103_p AS basecbcl_q103_p
,base.cbcl_q104_p AS basecbcl_q104_p
,base.cbcl_q107_p AS basecbcl_q107_p
,base.cbcl_q108_p AS basecbcl_q108_p
,base.cbcl_q109_p AS basecbcl_q109_p
,base.cbcl_q110_p AS basecbcl_q110_p
,base.cbcl_q111_p AS basecbcl_q111_p
,base.cbcl_q112_p AS basecbcl_q112_p
,base.`Attacks/threatens` AS `baseAttacks/threatens`
,base.Destroys AS baseDestroys
,base.`Disobeys rules` AS baseDisobeysrules
,base.Steals AS baseSteals
,base.`Peer Problems` AS basePeerProblems
,base.`Distracted/Hyperactive` AS `baseDistracted/Hyperactive`
,base.Hallucinations AS baseHallucinations
,base.`Sex play` AS baseSexplay
,base.`Weight problems` AS baseWeightproblems
,one.cbcl_q01_p AS onecbcl_q01_p
,one.cbcl_q03_p AS onecbcl_q03_p
,one.cbcl_q04_p AS onecbcl_q04_p
,one.cbcl_q05_p AS onecbcl_q05_p
,one.cbcl_q06_p AS onecbcl_q06_p
,one.cbcl_q07_p AS onecbcl_q07_p
,one.cbcl_q09_p AS onecbcl_q09_p
,one.cbcl_q11_p AS onecbcl_q11_p
,one.cbcl_q12_p AS onecbcl_q12_p
,one.cbcl_q13_p AS onecbcl_q13_p
,one.cbcl_q14_p AS onecbcl_q14_p
,one.cbcl_q15_p AS onecbcl_q15_p
,one.cbcl_q16_p AS onecbcl_q16_p
,one.cbcl_q17_p AS onecbcl_q17_p
,one.cbcl_q18_p AS onecbcl_q18_p
,one.cbcl_q19_p AS onecbcl_q19_p
,one.cbcl_q24_p AS onecbcl_q24_p
,one.cbcl_q26_p AS onecbcl_q26_p
,one.cbcl_q27_p AS onecbcl_q27_p
,one.cbcl_q29_p AS onecbcl_q29_p
,one.cbcl_q30_p AS onecbcl_q30_p
,one.cbcl_q31_p AS onecbcl_q31_p
,one.cbcl_q32_p AS onecbcl_q32_p
,one.cbcl_q33_p AS onecbcl_q33_p
,one.cbcl_q34_p AS onecbcl_q34_p
,one.cbcl_q35_p AS onecbcl_q35_p
,one.cbcl_q36_p AS onecbcl_q36_p
,one.cbcl_q37_p AS onecbcl_q37_p
,one.cbcl_q38_p AS onecbcl_q38_p
,one.cbcl_q39_p AS onecbcl_q39_p
,one.cbcl_q41_p AS onecbcl_q41_p
,one.cbcl_q42_p AS onecbcl_q42_p
,one.cbcl_q43_p AS onecbcl_q43_p
,one.cbcl_q44_p AS onecbcl_q44_p
,one.cbcl_q45_p AS onecbcl_q45_p
,one.cbcl_q46_p AS onecbcl_q46_p
,one.cbcl_q47_p AS onecbcl_q47_p
,one.cbcl_q49_p AS onecbcl_q49_p
,one.cbcl_q50_p AS onecbcl_q50_p
,one.cbcl_q51_p AS onecbcl_q51_p
,one.cbcl_q52_p AS onecbcl_q52_p
,one.cbcl_q54_p AS onecbcl_q54_p
,one.cbcl_q56a_p AS onecbcl_q56a_p
,one.cbcl_q56b_p AS onecbcl_q56b_p
,one.cbcl_q56c_p AS onecbcl_q56c_p
,one.cbcl_q56d_p AS onecbcl_q56d_p
,one.cbcl_q56e_p AS onecbcl_q56e_p
,one.cbcl_q56f_p AS onecbcl_q56f_p
,one.cbcl_q56g_p AS onecbcl_q56g_p
,one.cbcl_q56h_p AS onecbcl_q56h_p
,one.cbcl_q58_p AS onecbcl_q58_p
,one.cbcl_q61_p AS onecbcl_q61_p
,one.cbcl_q62_p AS onecbcl_q62_p
,one.cbcl_q63_p AS onecbcl_q63_p
,one.cbcl_q64_p AS onecbcl_q64_p
,one.cbcl_q65_p AS onecbcl_q65_p
,one.cbcl_q66_p AS onecbcl_q66_p
,one.cbcl_q67_p AS onecbcl_q67_p
,one.cbcl_q68_p AS onecbcl_q68_p
,one.cbcl_q69_p AS onecbcl_q69_p
,one.cbcl_q71_p AS onecbcl_q71_p
,one.cbcl_q72_p AS onecbcl_q72_p
,one.cbcl_q74_p AS onecbcl_q74_p
,one.cbcl_q75_p AS onecbcl_q75_p
,one.cbcl_q76_p AS onecbcl_q76_p
,one.cbcl_q77_p AS onecbcl_q77_p
,one.cbcl_q79_p AS onecbcl_q79_p
,one.cbcl_q80_p AS onecbcl_q80_p
,one.cbcl_q83_p AS onecbcl_q83_p
,one.cbcl_q84_p AS onecbcl_q84_p
,one.cbcl_q85_p AS onecbcl_q85_p
,one.cbcl_q86_p AS onecbcl_q86_p
,one.cbcl_q87_p AS onecbcl_q87_p
,one.cbcl_q88_p AS onecbcl_q88_p
,one.cbcl_q89_p AS onecbcl_q89_p
,one.cbcl_q90_p AS onecbcl_q90_p
,one.cbcl_q91_p AS onecbcl_q91_p
,one.cbcl_q92_p AS onecbcl_q92_p
,one.cbcl_q93_p AS onecbcl_q93_p
,one.cbcl_q94_p AS onecbcl_q94_p
,one.cbcl_q95_p AS onecbcl_q95_p
,one.cbcl_q96_p AS onecbcl_q96_p
,one.cbcl_q98_p AS onecbcl_q98_p
,one.cbcl_q100_p AS onecbcl_q100_p
,one.cbcl_q102_p AS onecbcl_q102_p
,one.cbcl_q103_p AS onecbcl_q103_p
,one.cbcl_q104_p AS onecbcl_q104_p
,one.cbcl_q107_p AS onecbcl_q107_p
,one.cbcl_q108_p AS onecbcl_q108_p
,one.cbcl_q109_p AS onecbcl_q109_p
,one.cbcl_q110_p AS onecbcl_q110_p
,one.cbcl_q111_p AS onecbcl_q111_p
,one.cbcl_q112_p AS onecbcl_q112_p
,one.`Attacks/threatens` AS `oneAttacks/threatens`
,one.Destroys AS oneDestroys
,one.`Disobeys rules` AS oneDisobeysrules
,one.Steals AS oneSteals
,one.`Peer Problems` AS onePeerproblems
,one.`Distracted/Hyperactive` AS `oneDistracted/Hyperactive`
,one.Hallucinations AS oneHallucinations
,one.`Sex Play` AS oneSexplay
,one.`Weight problems` AS oneWeightproblems
      FROM baselineData AS base 
      INNER JOIN oneyearData AS one USING (subjectkey)")
      
      

#New Data frame - baseline vs twoyear
cbclDF2 <- sqldf("SELECT base.subjectkey, base.interview_age, base.sex
,base.cbcl_q01_p AS basecbcl_q01_p
,base.cbcl_q03_p AS basecbcl_q03_p
,base.cbcl_q04_p AS basecbcl_q04_p
,base.cbcl_q05_p AS basecbcl_q05_p
,base.cbcl_q06_p AS basecbcl_q06_p
,base.cbcl_q07_p AS basecbcl_q07_p
,base.cbcl_q09_p AS basecbcl_q09_p
,base.cbcl_q11_p AS basecbcl_q11_p
,base.cbcl_q12_p AS basecbcl_q12_p
,base.cbcl_q13_p AS basecbcl_q13_p
,base.cbcl_q14_p AS basecbcl_q14_p
,base.cbcl_q15_p AS basecbcl_q15_p
,base.cbcl_q16_p AS basecbcl_q16_p
,base.cbcl_q17_p AS basecbcl_q17_p
,base.cbcl_q18_p AS basecbcl_q18_p
,base.cbcl_q19_p AS basecbcl_q19_p
,base.cbcl_q24_p AS basecbcl_q24_p
,base.cbcl_q26_p AS basecbcl_q26_p
,base.cbcl_q27_p AS basecbcl_q27_p
,base.cbcl_q29_p AS basecbcl_q29_p
,base.cbcl_q30_p AS basecbcl_q30_p
,base.cbcl_q31_p AS basecbcl_q31_p
,base.cbcl_q32_p AS basecbcl_q32_p
,base.cbcl_q33_p AS basecbcl_q33_p
,base.cbcl_q34_p AS basecbcl_q34_p
,base.cbcl_q35_p AS basecbcl_q35_p
,base.cbcl_q36_p AS basecbcl_q36_p
,base.cbcl_q37_p AS basecbcl_q37_p
,base.cbcl_q38_p AS basecbcl_q38_p
,base.cbcl_q39_p AS basecbcl_q39_p
,base.cbcl_q41_p AS basecbcl_q41_p
,base.cbcl_q42_p AS basecbcl_q42_p
,base.cbcl_q43_p AS basecbcl_q43_p
,base.cbcl_q44_p AS basecbcl_q44_p
,base.cbcl_q45_p AS basecbcl_q45_p
,base.cbcl_q46_p AS basecbcl_q46_p
,base.cbcl_q47_p AS basecbcl_q47_p
,base.cbcl_q49_p AS basecbcl_q49_p
,base.cbcl_q50_p AS basecbcl_q50_p
,base.cbcl_q51_p AS basecbcl_q51_p
,base.cbcl_q52_p AS basecbcl_q52_p
,base.cbcl_q54_p AS basecbcl_q54_p
,base.cbcl_q56a_p AS basecbcl_q56a_p
,base.cbcl_q56b_p AS basecbcl_q56b_p
,base.cbcl_q56c_p AS basecbcl_q56c_p
,base.cbcl_q56d_p AS basecbcl_q56d_p
,base.cbcl_q56e_p AS basecbcl_q56e_p
,base.cbcl_q56f_p AS basecbcl_q56f_p
,base.cbcl_q56g_p AS basecbcl_q56g_p
,base.cbcl_q56h_p AS basecbcl_q56h_p
,base.cbcl_q58_p AS basecbcl_q58_p
,base.cbcl_q61_p AS basecbcl_q61_p
,base.cbcl_q62_p AS basecbcl_q62_p
,base.cbcl_q63_p AS basecbcl_q63_p
,base.cbcl_q64_p AS basecbcl_q64_p
,base.cbcl_q65_p AS basecbcl_q65_p
,base.cbcl_q66_p AS basecbcl_q66_p
,base.cbcl_q67_p AS basecbcl_q67_p
,base.cbcl_q68_p AS basecbcl_q68_p
,base.cbcl_q69_p AS basecbcl_q69_p
,base.cbcl_q71_p AS basecbcl_q71_p
,base.cbcl_q72_p AS basecbcl_q72_p
,base.cbcl_q74_p AS basecbcl_q74_p
,base.cbcl_q75_p AS basecbcl_q75_p
,base.cbcl_q76_p AS basecbcl_q76_p
,base.cbcl_q77_p AS basecbcl_q77_p
,base.cbcl_q79_p AS basecbcl_q79_p
,base.cbcl_q80_p AS basecbcl_q80_p
,base.cbcl_q83_p AS basecbcl_q83_p
,base.cbcl_q84_p AS basecbcl_q84_p
,base.cbcl_q85_p AS basecbcl_q85_p
,base.cbcl_q86_p AS basecbcl_q86_p
,base.cbcl_q87_p AS basecbcl_q87_p
,base.cbcl_q88_p AS basecbcl_q88_p
,base.cbcl_q89_p AS basecbcl_q89_p
,base.cbcl_q90_p AS basecbcl_q90_p
,base.cbcl_q91_p AS basecbcl_q91_p
,base.cbcl_q92_p AS basecbcl_q92_p
,base.cbcl_q93_p AS basecbcl_q93_p
,base.cbcl_q94_p AS basecbcl_q94_p
,base.cbcl_q95_p AS basecbcl_q95_p
,base.cbcl_q96_p AS basecbcl_q96_p
,base.cbcl_q98_p AS basecbcl_q98_p
,base.cbcl_q100_p AS basecbcl_q100_p
,base.cbcl_q102_p AS basecbcl_q102_p
,base.cbcl_q103_p AS basecbcl_q103_p
,base.cbcl_q104_p AS basecbcl_q104_p
,base.cbcl_q107_p AS basecbcl_q107_p
,base.cbcl_q108_p AS basecbcl_q108_p
,base.cbcl_q109_p AS basecbcl_q109_p
,base.cbcl_q110_p AS basecbcl_q110_p
,base.cbcl_q111_p AS basecbcl_q111_p
,base.cbcl_q112_p AS basecbcl_q112_p
,base.`Attacks/threatens` AS `baseAttacks/threatens`
,base.Destroys AS baseDestroys
,base.`Disobeys rules` AS baseDisobeysrules
,base.Steals AS baseSteals
,base.`Peer Problems` AS basePeerProblems
,base.`Distracted/Hyperactive` AS `baseDistracted/Hyperactive`
,base.Hallucinations AS baseHallucinations
,base.`Sex play` AS baseSexplay
,base.`Weight problems` AS baseWeightproblems
,two.cbcl_q01_p AS twocbcl_q01_p
,two.cbcl_q03_p AS twocbcl_q03_p
,two.cbcl_q04_p AS twocbcl_q04_p
,two.cbcl_q05_p AS twocbcl_q05_p
,two.cbcl_q06_p AS twocbcl_q06_p
,two.cbcl_q07_p AS twocbcl_q07_p
,two.cbcl_q09_p AS twocbcl_q09_p
,two.cbcl_q11_p AS twocbcl_q11_p
,two.cbcl_q12_p AS twocbcl_q12_p
,two.cbcl_q13_p AS twocbcl_q13_p
,two.cbcl_q14_p AS twocbcl_q14_p
,two.cbcl_q15_p AS twocbcl_q15_p
,two.cbcl_q16_p AS twocbcl_q16_p
,two.cbcl_q17_p AS twocbcl_q17_p
,two.cbcl_q18_p AS twocbcl_q18_p
,two.cbcl_q19_p AS twocbcl_q19_p
,two.cbcl_q24_p AS twocbcl_q24_p
,two.cbcl_q26_p AS twocbcl_q26_p
,two.cbcl_q27_p AS twocbcl_q27_p
,two.cbcl_q29_p AS twocbcl_q29_p
,two.cbcl_q30_p AS twocbcl_q30_p
,two.cbcl_q31_p AS twocbcl_q31_p
,two.cbcl_q32_p AS twocbcl_q32_p
,two.cbcl_q33_p AS twocbcl_q33_p
,two.cbcl_q34_p AS twocbcl_q34_p
,two.cbcl_q35_p AS twocbcl_q35_p
,two.cbcl_q36_p AS twocbcl_q36_p
,two.cbcl_q37_p AS twocbcl_q37_p
,two.cbcl_q38_p AS twocbcl_q38_p
,two.cbcl_q39_p AS twocbcl_q39_p
,two.cbcl_q41_p AS twocbcl_q41_p
,two.cbcl_q42_p AS twocbcl_q42_p
,two.cbcl_q43_p AS twocbcl_q43_p
,two.cbcl_q44_p AS twocbcl_q44_p
,two.cbcl_q45_p AS twocbcl_q45_p
,two.cbcl_q46_p AS twocbcl_q46_p
,two.cbcl_q47_p AS twocbcl_q47_p
,two.cbcl_q49_p AS twocbcl_q49_p
,two.cbcl_q50_p AS twocbcl_q50_p
,two.cbcl_q51_p AS twocbcl_q51_p
,two.cbcl_q52_p AS twocbcl_q52_p
,two.cbcl_q54_p AS twocbcl_q54_p
,two.cbcl_q56a_p AS twocbcl_q56a_p
,two.cbcl_q56b_p AS twocbcl_q56b_p
,two.cbcl_q56c_p AS twocbcl_q56c_p
,two.cbcl_q56d_p AS twocbcl_q56d_p
,two.cbcl_q56e_p AS twocbcl_q56e_p
,two.cbcl_q56f_p AS twocbcl_q56f_p
,two.cbcl_q56g_p AS twocbcl_q56g_p
,two.cbcl_q56h_p AS twocbcl_q56h_p
,two.cbcl_q58_p AS twocbcl_q58_p
,two.cbcl_q61_p AS twocbcl_q61_p
,two.cbcl_q62_p AS twocbcl_q62_p
,two.cbcl_q63_p AS twocbcl_q63_p
,two.cbcl_q64_p AS twocbcl_q64_p
,two.cbcl_q65_p AS twocbcl_q65_p
,two.cbcl_q66_p AS twocbcl_q66_p
,two.cbcl_q67_p AS twocbcl_q67_p
,two.cbcl_q68_p AS twocbcl_q68_p
,two.cbcl_q69_p AS twocbcl_q69_p
,two.cbcl_q71_p AS twocbcl_q71_p
,two.cbcl_q72_p AS twocbcl_q72_p
,two.cbcl_q74_p AS twocbcl_q74_p
,two.cbcl_q75_p AS twocbcl_q75_p
,two.cbcl_q76_p AS twocbcl_q76_p
,two.cbcl_q77_p AS twocbcl_q77_p
,two.cbcl_q79_p AS twocbcl_q79_p
,two.cbcl_q80_p AS twocbcl_q80_p
,two.cbcl_q83_p AS twocbcl_q83_p
,two.cbcl_q84_p AS twocbcl_q84_p
,two.cbcl_q85_p AS twocbcl_q85_p
,two.cbcl_q86_p AS twocbcl_q86_p
,two.cbcl_q87_p AS twocbcl_q87_p
,two.cbcl_q88_p AS twocbcl_q88_p
,two.cbcl_q89_p AS twocbcl_q89_p
,two.cbcl_q90_p AS twocbcl_q90_p
,two.cbcl_q91_p AS twocbcl_q91_p
,two.cbcl_q92_p AS twocbcl_q92_p
,two.cbcl_q93_p AS twocbcl_q93_p
,two.cbcl_q94_p AS twocbcl_q94_p
,two.cbcl_q95_p AS twocbcl_q95_p
,two.cbcl_q96_p AS twocbcl_q96_p
,two.cbcl_q98_p AS twocbcl_q98_p
,two.cbcl_q100_p AS twocbcl_q100_p
,two.cbcl_q102_p AS twocbcl_q102_p
,two.cbcl_q103_p AS twocbcl_q103_p
,two.cbcl_q104_p AS twocbcl_q104_p
,two.cbcl_q107_p AS twocbcl_q107_p
,two.cbcl_q108_p AS twocbcl_q108_p
,two.cbcl_q109_p AS twocbcl_q109_p
,two.cbcl_q110_p AS twocbcl_q110_p
,two.cbcl_q111_p AS twocbcl_q111_p
,two.cbcl_q112_p AS twocbcl_q112_p
,two.`Attacks/threatens` AS `twoAttacks/threatens`
,two.Destroys AS twoDestroys
,two.`Disobeys rules` AS twoDisobeysrules
,two.Steals AS twoSteals
,two.`Peer Problems` AS twoPeerproblems
,two.`Distracted/Hyperactive` AS `twoDistracted/Hyperactive`
,two.Hallucinations AS twoHallucinations
,two.`Sex play` AS twoSexplay
,two.`Weight problems` AS twoWeightproblems
      FROM baselineData AS base 
      INNER JOIN twoyearData AS two USING (subjectkey)")



#remove comment to debug
#nrow(baselineData)
#ncol(baselineData)
#nrow(cbclDF1)
#nrow(oneyearData)
#nrow(cbclDF2)
#nrow(twoyearData)
#head(cbclDF1)
#ncol(cbclDF1)
#ncol(cbclDF2)
#view(cbclDF1)
#view(cbclDF2)

######STEP 7: LONGITUDNAL CFA
#libraries downloaded above

#create a configural and CFA for baseline and one year
configural1 <- '
# Define the latent factors.
baselineFactor2 =~ NA*basecbcl_q50_p + lambda1*basecbcl_q50_p + basecbcl_q112_p + basecbcl_q32_p + basecbcl_q52_p + basecbcl_q45_p + basecbcl_q31_p + basecbcl_q35_p + basecbcl_q71_p + basecbcl_q30_p + basecbcl_q29_p + basecbcl_q12_p
onelineFactor2 =~ NA*onecbcl_q50_p + lambda1*onecbcl_q50_p + onecbcl_q112_p + onecbcl_q32_p + onecbcl_q52_p + onecbcl_q45_p + onecbcl_q31_p + onecbcl_q35_p + onecbcl_q71_p + onecbcl_q30_p + onecbcl_q29_p + onecbcl_q12_p
'
CFA_fit1 <- cfa(configural1, data = cbclDF, mimic = "mplus")
summary(CFA_fit1, fit.measures = TRUE)
#get factor scores
factor.scores(cbclDF1, CFA_fit1)

#create a configural and CFA for baseline and two year
configural2 <- '
# Define the latent factors.
baselineFactor2 =~ NA*basecbcl_q50_p + lambda1*basecbcl_q50_p + basecbcl_q112_p + basecbcl_q32_p + basecbcl_q52_p + basecbcl_q45_p + basecbcl_q31_p + basecbcl_q35_p + basecbcl_q71_p + basecbcl_q30_p + basecbcl_q29_p + basecbcl_q12_p
twolineFactor2 =~ NA*twocbcl_q50_p + lambda1*twocbcl_q50_p + twocbcl_q112_p + twocbcl_q32_p + twocbcl_q52_p + twocbcl_q45_p + twocbcl_q31_p + twocbcl_q35_p + twocbcl_q71_p + twocbcl_q30_p + twocbcl_q29_p + twocbcl_q12_p
'
CFA_fit2 <- cfa(configural2, data = cbclDF, mimic = "mplus")
summary(CFA_fit2, fit.measures = TRUE)
#get factor scores
factor.scores(cbclDF2, CFA_fit2)

#Model diagram for baseline-one year
semPaths(CFA_fit1, what="est", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#Model diagram for baseline-two year
semPaths(CFA_fit2, what="est", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)
