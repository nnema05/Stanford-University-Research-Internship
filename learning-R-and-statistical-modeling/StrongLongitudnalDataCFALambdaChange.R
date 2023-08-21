#Longitudnal Factor Analysis 
#Sample ABCD - Goal #1 and #2

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
#install.packages("semPlot")
#library(semPlot)  #for automatically making diagrams
install.packages("sqldf")
library(sqldf)
install.packages("writexl")
library(writexl)
install.packages("fastDummies")
library(fastDummies)
install.packages("recipes")
library(recipes)
install.packages("psychTools")
library(psychTools)



######STEP 2: PREP for longitudinal CFA by creating a new data frame where... 
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
,base.`Attacks/threatens` AS baseAttacksThreatens
,base.Destroys AS baseDestroys
,base.`Disobeys rules` AS baseDisobeysrules
,base.Steals AS baseSteals
,base.`Peer Problems` AS basePeerProblems
,base.`Distracted/Hyperactive` AS baseDistractedHyperactive
,base.Hallucinations AS baseHallucinations
,base.`Sex play` AS baseSexPlay
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
,one.`Attacks/threatens` AS oneAttacksThreatens
,one.Destroys AS oneDestroys
,one.`Disobeys rules` AS oneDisobeysrules
,one.Steals AS oneSteals
,one.`Peer problems` AS onePeerProblems
,one.`Distracted/Hyperactive` AS oneDistractedHyperactive
,one.Hallucinations AS oneHallucinations
,one.`Sex Play` AS oneSexPlay
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
,base.`Attacks/threatens` AS baseAttacksThreatens
,base.Destroys AS baseDestroys
,base.`Disobeys rules` AS baseDisobeysrules
,base.Steals AS baseSteals
,base.`Peer problems` AS basePeerProblems
,base.`Distracted/Hyperactive` AS baseDistractedHyperactive
,base.Hallucinations AS baseHallucinations
,base.`Sex play` AS baseSexPlay
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
,two.`Attacks/threatens` AS twoAttacksThreatens
,two.Destroys AS twoDestroys
,two.`Disobeys rules` AS twoDisobeysrules
,two.Steals AS twoSteals
,two.`Peer problems` AS twoPeerProblems
,two.`Distracted/Hyperactive` AS twoDistractedHyperactive
,two.Hallucinations AS twoHallucinations
,two.`Sex play` AS twoSexPlay
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

######STEP 4: LONGITUDNAL CFA STRONG INVARIANCE 
#libraries downloaded above

#create a model and CFA for baseline and one year
strong1<- '
#Define latent 
   baseFactor1=~
NA*baseAttacksThreatens
+lambda1*baseAttacksThreatens
+lambda2*basecbcl_q16_p
+lambda3*baseDisobeysrules
+lambda4*basecbcl_q37_p
+lambda5*basecbcl_q95_p
+lambda6*basecbcl_q03_p
+lambda7*baseDestroys
+lambda8*basecbcl_q68_p
+lambda9*basecbcl_q26_p
+lambda10*basecbcl_q90_p
+lambda11*basecbcl_q94_p
+lambda12*baseSteals
+lambda13*basecbcl_q86_p
+lambda14*basecbcl_q43_p
+lambda15*basecbcl_q15_p
+lambda16*basecbcl_q67_p
+lambda17*basecbcl_q87_p
+lambda18*basecbcl_q27_p
+lambda19*basePeerProblems
+lambda20*basecbcl_q89_p
+lambda21*basecbcl_q19_p
+lambda22*basecbcl_q96_p
+lambda23*basecbcl_q39_p
+lambda24*basecbcl_q34_p
+lambda25*basecbcl_q72_p
+lambda26*basecbcl_q88_p
+lambda27*basecbcl_q74_p
+lambda28*basecbcl_q07_p
+lambda29*basecbcl_q109_p
   oneFactor1=~
NA*oneAttacksThreatens
+lambda1*oneAttacksThreatens
+lambda2*onecbcl_q16_p
+lambda3*oneDisobeysrules
+lambda4*onecbcl_q37_p
+lambda5*onecbcl_q95_p
+lambda6*onecbcl_q03_p
+lambda7*oneDestroys
+lambda8*onecbcl_q68_p
+lambda9*onecbcl_q26_p
+lambda10*onecbcl_q90_p
+lambda11*onecbcl_q94_p
+lambda12*oneSteals
+lambda13*onecbcl_q86_p
+lambda14*onecbcl_q43_p
+lambda15*onecbcl_q15_p
+lambda16*onecbcl_q67_p
+lambda17*onecbcl_q87_p
+lambda18*onecbcl_q27_p
+lambda19*onePeerProblems
+lambda20*onecbcl_q89_p
+lambda21*onecbcl_q19_p
+lambda22*onecbcl_q96_p
+lambda23*onecbcl_q39_p
+lambda24*onecbcl_q34_p
+lambda25*onecbcl_q72_p
+lambda26*onecbcl_q88_p
+lambda27*onecbcl_q74_p
+lambda28*onecbcl_q07_p
+lambda29*onecbcl_q109_p
   baseFactor2=~
NA*basecbcl_q50_p
+lambda30*basecbcl_q50_p
+lambda31*basecbcl_q112_p
+lambda32*basecbcl_q32_p
+lambda33*basecbcl_q52_p
+lambda34*basecbcl_q45_p
+lambda35*basecbcl_q31_p
+lambda36*basecbcl_q35_p
+lambda37*basecbcl_q71_p
+lambda38*basecbcl_q30_p
+lambda39*basecbcl_q29_p
+lambda40*basecbcl_q12_p
   oneFactor2=~
NA*onecbcl_q50_p
+lambda30*onecbcl_q50_p
+lambda31*onecbcl_q112_p
+lambda32*onecbcl_q32_p
+lambda33*onecbcl_q52_p
+lambda34*onecbcl_q45_p
+lambda35*onecbcl_q31_p
+lambda36*onecbcl_q35_p
+lambda37*onecbcl_q71_p
+lambda38*onecbcl_q30_p
+lambda39*onecbcl_q29_p
+lambda40*onecbcl_q12_p
   baseFactor3=~
NA*baseDistractedHyperactive
+lambda41*baseDistractedHyperactive
+lambda42*basecbcl_q17_p
+lambda43*basecbcl_q80_p
+lambda44*basecbcl_q13_p
+lambda45*basecbcl_q62_p
+lambda46*basecbcl_q46_p
+lambda47*basecbcl_q04_p
+lambda48*basecbcl_q93_p
+lambda49*basecbcl_q09_p
+lambda50*basecbcl_q61_p
+lambda51*basecbcl_q66_p
+lambda52*basecbcl_q85_p
+lambda53*basecbcl_q01_p
+lambda54*basecbcl_q36_p
+lambda55*basecbcl_q64_p
   oneFactor3=~
NA*oneDistractedHyperactive
+lambda41*oneDistractedHyperactive
+lambda42*onecbcl_q17_p
+lambda43*onecbcl_q80_p
+lambda44*onecbcl_q13_p
+lambda45*onecbcl_q62_p
+lambda46*onecbcl_q46_p
+lambda47*onecbcl_q04_p
+lambda48*onecbcl_q93_p
+lambda49*onecbcl_q09_p
+lambda50*onecbcl_q61_p
+lambda51*onecbcl_q66_p
+lambda52*onecbcl_q85_p
+lambda53*onecbcl_q01_p
+lambda54*onecbcl_q36_p
+lambda55*onecbcl_q64_p
   baseFactor4=~
NA*basecbcl_q56c_p
+lambda56*basecbcl_q56c_p
+lambda57*basecbcl_q56f_p
+lambda58*basecbcl_q56g_p
+lambda59*basecbcl_q56b_p
+lambda60*basecbcl_q56a_p
+lambda61*basecbcl_q51_p
+lambda62*basecbcl_q56h_p
+lambda63*basecbcl_q56d_p
+lambda64*basecbcl_q56e_p
   oneFactor4=~
NA*onecbcl_q56c_p
+lambda56*onecbcl_q56c_p
+lambda57*onecbcl_q56f_p
+lambda58*onecbcl_q56g_p
+lambda59*onecbcl_q56b_p
+lambda60*onecbcl_q56a_p
+lambda61*onecbcl_q51_p
+lambda62*onecbcl_q56h_p
+lambda63*onecbcl_q56d_p
+lambda64*onecbcl_q56e_p
   baseFactor5=~
NA*basecbcl_q111_p
+lambda65*basecbcl_q111_p
+lambda66*basecbcl_q42_p
+lambda67*basecbcl_q75_p
+lambda68*basecbcl_q65_p
+lambda69*basecbcl_q102_p
   oneFactor5=~
NA*onecbcl_q111_p
+lambda65*onecbcl_q111_p
+lambda66*onecbcl_q42_p
+lambda67*onecbcl_q75_p
+lambda68*onecbcl_q65_p
+lambda69*onecbcl_q102_p

#Intercepts
basecbcl_q01_p ~ i1*1
basecbcl_q03_p ~ i2*1
basecbcl_q04_p ~ i3*1
basecbcl_q05_p ~ i4*1
basecbcl_q06_p ~ i5*1
basecbcl_q07_p ~ i6*1
basecbcl_q09_p ~ i7*1
basecbcl_q11_p ~ i8*1
basecbcl_q12_p ~ i9*1
basecbcl_q13_p ~ i10*1
basecbcl_q14_p ~ i11*1
basecbcl_q15_p ~ i12*1
basecbcl_q16_p ~ i13*1
basecbcl_q17_p ~ i14*1
basecbcl_q18_p ~ i15*1
basecbcl_q19_p ~ i16*1
basecbcl_q24_p ~ i17*1
basecbcl_q26_p ~ i18*1
basecbcl_q27_p ~ i19*1
basecbcl_q29_p ~ i20*1
basecbcl_q30_p ~ i21*1
basecbcl_q31_p ~ i22*1
basecbcl_q32_p ~ i23*1
basecbcl_q33_p ~ i24*1
basecbcl_q34_p ~ i25*1
basecbcl_q35_p ~ i26*1
basecbcl_q36_p ~ i27*1
basecbcl_q37_p ~ i28*1
basecbcl_q38_p ~ i29*1
basecbcl_q39_p ~ i30*1
basecbcl_q41_p ~ i31*1
basecbcl_q42_p ~ i32*1
basecbcl_q43_p ~ i33*1
basecbcl_q44_p ~ i34*1
basecbcl_q45_p ~ i35*1
basecbcl_q46_p ~ i36*1
basecbcl_q47_p ~ i37*1
basecbcl_q49_p ~ i38*1
basecbcl_q50_p ~ i39*1
basecbcl_q51_p ~ i40*1
basecbcl_q52_p ~ i41*1
basecbcl_q54_p ~ i42*1
basecbcl_q56a_p ~ i43*1
basecbcl_q56b_p ~ i44*1
basecbcl_q56c_p ~ i45*1
basecbcl_q56d_p ~ i46*1
basecbcl_q56e_p ~ i47*1
basecbcl_q56f_p ~ i48*1
basecbcl_q56g_p ~ i49*1
basecbcl_q56h_p ~ i50*1
basecbcl_q58_p ~ i51*1
basecbcl_q61_p ~ i52*1
basecbcl_q62_p ~ i53*1
basecbcl_q63_p ~ i54*1
basecbcl_q64_p ~ i55*1
basecbcl_q65_p ~ i56*1
basecbcl_q66_p ~ i57*1
basecbcl_q67_p ~ i58*1
basecbcl_q68_p ~ i59*1
basecbcl_q69_p ~ i60*1
basecbcl_q71_p ~ i61*1
basecbcl_q72_p ~ i62*1
basecbcl_q74_p ~ i63*1
basecbcl_q75_p ~ i64*1
basecbcl_q76_p ~ i65*1
basecbcl_q77_p ~ i66*1
basecbcl_q79_p ~ i67*1
basecbcl_q80_p ~ i68*1
basecbcl_q83_p ~ i69*1
basecbcl_q84_p ~ i70*1
basecbcl_q85_p ~ i71*1
basecbcl_q86_p ~ i72*1
basecbcl_q87_p ~ i73*1
basecbcl_q88_p ~ i74*1
basecbcl_q89_p ~ i75*1
basecbcl_q90_p ~ i76*1
basecbcl_q91_p ~ i77*1
basecbcl_q92_p ~ i78*1
basecbcl_q93_p ~ i79*1
basecbcl_q94_p ~ i80*1
basecbcl_q95_p ~ i81*1
basecbcl_q96_p ~ i82*1
basecbcl_q98_p ~ i83*1
basecbcl_q100_p ~ i84*1
basecbcl_q102_p ~ i85*1
basecbcl_q103_p ~ i86*1
basecbcl_q104_p ~ i87*1
basecbcl_q107_p ~ i88*1
basecbcl_q108_p ~ i89*1
basecbcl_q109_p ~ i90*1
basecbcl_q110_p ~ i91*1
basecbcl_q111_p ~ i92*1
basecbcl_q112_p ~ i93*1
baseAttacksThreatens ~ i94*1
baseDestroys ~ i95*1
baseDisobeysrules ~ i96*1
baseSteals ~ i97*1
basePeerProblems ~ i98*1
baseDistractedHyperactive ~ i99*1
baseHallucinations ~ i100*1
baseSexPlay ~ i101*1
baseWeightproblems~ i102*1 
onecbcl_q01_p ~ i1*1
onecbcl_q03_p ~ i2*1
onecbcl_q04_p ~ i3*1
onecbcl_q05_p ~ i4*1
onecbcl_q06_p ~ i5*1
onecbcl_q07_p ~ i6*1
onecbcl_q09_p ~ i7*1
onecbcl_q11_p ~ i8*1
onecbcl_q12_p ~ i9*1
onecbcl_q13_p ~ i10*1
onecbcl_q14_p ~ i11*1
onecbcl_q15_p ~ i12*1
onecbcl_q16_p ~ i13*1
onecbcl_q17_p ~ i14*1
onecbcl_q18_p ~ i15*1
onecbcl_q19_p ~ i16*1
onecbcl_q24_p ~ i17*1
onecbcl_q26_p ~ i18*1
onecbcl_q27_p ~ i19*1
onecbcl_q29_p ~ i20*1
onecbcl_q30_p ~ i21*1
onecbcl_q31_p ~ i22*1
onecbcl_q32_p ~ i23*1
onecbcl_q33_p ~ i24*1
onecbcl_q34_p ~ i25*1
onecbcl_q35_p ~ i26*1
onecbcl_q36_p ~ i27*1
onecbcl_q37_p ~ i28*1
onecbcl_q38_p ~ i29*1
onecbcl_q39_p ~ i30*1
onecbcl_q41_p ~ i31*1
onecbcl_q42_p ~ i32*1
onecbcl_q43_p ~ i33*1
onecbcl_q44_p ~ i34*1
onecbcl_q45_p ~ i35*1
onecbcl_q46_p ~ i36*1
onecbcl_q47_p ~ i37*1
onecbcl_q49_p ~ i38*1
onecbcl_q50_p ~ i39*1
onecbcl_q51_p ~ i40*1
onecbcl_q52_p ~ i41*1
onecbcl_q54_p ~ i42*1
onecbcl_q56a_p ~ i43*1
onecbcl_q56b_p ~ i44*1
onecbcl_q56c_p ~ i45*1
onecbcl_q56d_p ~ i46*1
onecbcl_q56e_p ~ i47*1
onecbcl_q56f_p ~ i48*1
onecbcl_q56g_p ~ i49*1
onecbcl_q56h_p ~ i50*1
onecbcl_q58_p ~ i51*1
onecbcl_q61_p ~ i52*1
onecbcl_q62_p ~ i53*1
onecbcl_q63_p ~ i54*1
onecbcl_q64_p ~ i55*1
onecbcl_q65_p ~ i56*1
onecbcl_q66_p ~ i57*1
onecbcl_q67_p ~ i58*1
onecbcl_q68_p ~ i59*1
onecbcl_q69_p ~ i60*1
onecbcl_q71_p ~ i61*1
onecbcl_q72_p ~ i62*1
onecbcl_q74_p ~ i63*1
onecbcl_q75_p ~ i64*1
onecbcl_q76_p ~ i65*1
onecbcl_q77_p ~ i66*1
onecbcl_q79_p ~ i67*1
onecbcl_q80_p ~ i68*1
onecbcl_q83_p ~ i69*1
onecbcl_q84_p ~ i70*1
onecbcl_q85_p ~ i71*1
onecbcl_q86_p ~ i72*1
onecbcl_q87_p ~ i73*1
onecbcl_q88_p ~ i74*1
onecbcl_q89_p ~ i75*1
onecbcl_q90_p ~ i76*1
onecbcl_q91_p ~ i77*1
onecbcl_q92_p ~ i78*1
onecbcl_q93_p ~ i79*1
onecbcl_q94_p ~ i80*1
onecbcl_q95_p ~ i81*1
onecbcl_q96_p ~ i82*1
onecbcl_q98_p ~ i83*1
onecbcl_q100_p ~ i84*1
onecbcl_q102_p ~ i85*1
onecbcl_q103_p ~ i86*1
onecbcl_q104_p ~ i87*1
onecbcl_q107_p ~ i88*1
onecbcl_q108_p ~ i89*1
onecbcl_q109_p ~ i90*1
onecbcl_q110_p ~ i91*1
onecbcl_q111_p ~ i92*1
onecbcl_q112_p ~ i93*1
oneAttacksThreatens ~ i94*1
oneDestroys ~ i95*1
oneDisobeysrules ~ i96*1
oneSteals ~ i97*1
onePeerProblems ~ i98*1
oneDistractedHyperactive ~ i99*1
oneHallucinations ~ i100*1
oneSexPlay ~ i101*1
oneWeightproblems~ i102*1 


#Unique Variances

basecbcl_q01_p ~~ basecbcl_q01_p
basecbcl_q03_p ~~ basecbcl_q03_p
basecbcl_q04_p ~~ basecbcl_q04_p
basecbcl_q05_p ~~ basecbcl_q05_p
basecbcl_q06_p ~~ basecbcl_q06_p
basecbcl_q07_p ~~ basecbcl_q07_p
basecbcl_q09_p ~~ basecbcl_q09_p
basecbcl_q11_p ~~ basecbcl_q11_p
basecbcl_q12_p ~~ basecbcl_q12_p
basecbcl_q13_p ~~ basecbcl_q13_p
basecbcl_q14_p ~~ basecbcl_q14_p
basecbcl_q15_p ~~ basecbcl_q15_p
basecbcl_q16_p ~~ basecbcl_q16_p
basecbcl_q17_p ~~ basecbcl_q17_p
basecbcl_q18_p ~~ basecbcl_q18_p
basecbcl_q19_p ~~ basecbcl_q19_p
basecbcl_q24_p ~~ basecbcl_q24_p
basecbcl_q26_p ~~ basecbcl_q26_p
basecbcl_q27_p ~~ basecbcl_q27_p
basecbcl_q29_p ~~ basecbcl_q29_p
basecbcl_q30_p ~~ basecbcl_q30_p
basecbcl_q31_p ~~ basecbcl_q31_p
basecbcl_q32_p ~~ basecbcl_q32_p
basecbcl_q33_p ~~ basecbcl_q33_p
basecbcl_q34_p ~~ basecbcl_q34_p
basecbcl_q35_p ~~ basecbcl_q35_p
basecbcl_q36_p ~~ basecbcl_q36_p
basecbcl_q37_p ~~ basecbcl_q37_p
basecbcl_q38_p ~~ basecbcl_q38_p
basecbcl_q39_p ~~ basecbcl_q39_p
basecbcl_q41_p ~~ basecbcl_q41_p
basecbcl_q42_p ~~ basecbcl_q42_p
basecbcl_q43_p ~~ basecbcl_q43_p
basecbcl_q44_p ~~ basecbcl_q44_p
basecbcl_q45_p ~~ basecbcl_q45_p
basecbcl_q46_p ~~ basecbcl_q46_p
basecbcl_q47_p ~~ basecbcl_q47_p
basecbcl_q49_p ~~ basecbcl_q49_p
basecbcl_q50_p ~~ basecbcl_q50_p
basecbcl_q51_p ~~ basecbcl_q51_p
basecbcl_q52_p ~~ basecbcl_q52_p
basecbcl_q54_p ~~ basecbcl_q54_p
basecbcl_q56a_p ~~ basecbcl_q56a_p
basecbcl_q56b_p ~~ basecbcl_q56b_p
basecbcl_q56c_p ~~ basecbcl_q56c_p
basecbcl_q56d_p ~~ basecbcl_q56d_p
basecbcl_q56e_p ~~ basecbcl_q56e_p
basecbcl_q56f_p ~~ basecbcl_q56f_p
basecbcl_q56g_p ~~ basecbcl_q56g_p
basecbcl_q56h_p ~~ basecbcl_q56h_p
basecbcl_q58_p ~~ basecbcl_q58_p
basecbcl_q61_p ~~ basecbcl_q61_p
basecbcl_q62_p ~~ basecbcl_q62_p
basecbcl_q63_p ~~ basecbcl_q63_p
basecbcl_q64_p ~~ basecbcl_q64_p
basecbcl_q65_p ~~ basecbcl_q65_p
basecbcl_q66_p ~~ basecbcl_q66_p
basecbcl_q67_p ~~ basecbcl_q67_p
basecbcl_q68_p ~~ basecbcl_q68_p
basecbcl_q69_p ~~ basecbcl_q69_p
basecbcl_q71_p ~~ basecbcl_q71_p
basecbcl_q72_p ~~ basecbcl_q72_p
basecbcl_q74_p ~~ basecbcl_q74_p
basecbcl_q75_p ~~ basecbcl_q75_p
basecbcl_q76_p ~~ basecbcl_q76_p
basecbcl_q77_p ~~ basecbcl_q77_p
basecbcl_q79_p ~~ basecbcl_q79_p
basecbcl_q80_p ~~ basecbcl_q80_p
basecbcl_q83_p ~~ basecbcl_q83_p
basecbcl_q84_p ~~ basecbcl_q84_p
basecbcl_q85_p ~~ basecbcl_q85_p
basecbcl_q86_p ~~ basecbcl_q86_p
basecbcl_q87_p ~~ basecbcl_q87_p
basecbcl_q88_p ~~ basecbcl_q88_p
basecbcl_q89_p ~~ basecbcl_q89_p
basecbcl_q90_p ~~ basecbcl_q90_p
basecbcl_q91_p ~~ basecbcl_q91_p
basecbcl_q92_p ~~ basecbcl_q92_p
basecbcl_q93_p ~~ basecbcl_q93_p
basecbcl_q94_p ~~ basecbcl_q94_p
basecbcl_q95_p ~~ basecbcl_q95_p
basecbcl_q96_p ~~ basecbcl_q96_p
basecbcl_q98_p ~~ basecbcl_q98_p
basecbcl_q100_p ~~ basecbcl_q100_p
basecbcl_q102_p ~~ basecbcl_q102_p
basecbcl_q103_p ~~ basecbcl_q103_p
basecbcl_q104_p ~~ basecbcl_q104_p
basecbcl_q107_p ~~ basecbcl_q107_p
basecbcl_q108_p ~~ basecbcl_q108_p
basecbcl_q109_p ~~ basecbcl_q109_p
basecbcl_q110_p ~~ basecbcl_q110_p
basecbcl_q111_p ~~ basecbcl_q111_p
basecbcl_q112_p ~~ basecbcl_q112_p
baseAttacksThreatens ~~ baseAttacksThreatens
baseDestroys ~~ baseDestroys
baseDisobeysrules ~~ baseDisobeysrules
baseSteals ~~ baseSteals
basePeerProblems ~~ basePeerProblems
baseDistractedHyperactive ~~ baseDistractedHyperactive
baseHallucinations ~~ baseHallucinations
baseSexPlay ~~ baseSexPlay
baseWeightproblems ~~ baseWeightproblems

onecbcl_q01_p ~~ onecbcl_q01_p
onecbcl_q03_p ~~ onecbcl_q03_p
onecbcl_q04_p ~~ onecbcl_q04_p
onecbcl_q05_p ~~ onecbcl_q05_p
onecbcl_q06_p ~~ onecbcl_q06_p
onecbcl_q07_p ~~ onecbcl_q07_p
onecbcl_q09_p ~~ onecbcl_q09_p
onecbcl_q11_p ~~ onecbcl_q11_p
onecbcl_q12_p ~~ onecbcl_q12_p
onecbcl_q13_p ~~ onecbcl_q13_p
onecbcl_q14_p ~~ onecbcl_q14_p
onecbcl_q15_p ~~ onecbcl_q15_p
onecbcl_q16_p ~~ onecbcl_q16_p
onecbcl_q17_p ~~ onecbcl_q17_p
onecbcl_q18_p ~~ onecbcl_q18_p
onecbcl_q19_p ~~ onecbcl_q19_p
onecbcl_q24_p ~~ onecbcl_q24_p
onecbcl_q26_p ~~ onecbcl_q26_p
onecbcl_q27_p ~~ onecbcl_q27_p
onecbcl_q29_p ~~ onecbcl_q29_p
onecbcl_q30_p ~~ onecbcl_q30_p
onecbcl_q31_p ~~ onecbcl_q31_p
onecbcl_q32_p ~~ onecbcl_q32_p
onecbcl_q33_p ~~ onecbcl_q33_p
onecbcl_q34_p ~~ onecbcl_q34_p
onecbcl_q35_p ~~ onecbcl_q35_p
onecbcl_q36_p ~~ onecbcl_q36_p
onecbcl_q37_p ~~ onecbcl_q37_p
onecbcl_q38_p ~~ onecbcl_q38_p
onecbcl_q39_p ~~ onecbcl_q39_p
onecbcl_q41_p ~~ onecbcl_q41_p
onecbcl_q42_p ~~ onecbcl_q42_p
onecbcl_q43_p ~~ onecbcl_q43_p
onecbcl_q44_p ~~ onecbcl_q44_p
onecbcl_q45_p ~~ onecbcl_q45_p
onecbcl_q46_p ~~ onecbcl_q46_p
onecbcl_q47_p ~~ onecbcl_q47_p
onecbcl_q49_p ~~ onecbcl_q49_p
onecbcl_q50_p ~~ onecbcl_q50_p
onecbcl_q51_p ~~ onecbcl_q51_p
onecbcl_q52_p ~~ onecbcl_q52_p
onecbcl_q54_p ~~ onecbcl_q54_p
onecbcl_q56a_p ~~ onecbcl_q56a_p
onecbcl_q56b_p ~~ onecbcl_q56b_p
onecbcl_q56c_p ~~ onecbcl_q56c_p
onecbcl_q56d_p ~~ onecbcl_q56d_p
onecbcl_q56e_p ~~ onecbcl_q56e_p
onecbcl_q56f_p ~~ onecbcl_q56f_p
onecbcl_q56g_p ~~ onecbcl_q56g_p
onecbcl_q56h_p ~~ onecbcl_q56h_p
onecbcl_q58_p ~~ onecbcl_q58_p
onecbcl_q61_p ~~ onecbcl_q61_p
onecbcl_q62_p ~~ onecbcl_q62_p
onecbcl_q63_p ~~ onecbcl_q63_p
onecbcl_q64_p ~~ onecbcl_q64_p
onecbcl_q65_p ~~ onecbcl_q65_p
onecbcl_q66_p ~~ onecbcl_q66_p
onecbcl_q67_p ~~ onecbcl_q67_p
onecbcl_q68_p ~~ onecbcl_q68_p
onecbcl_q69_p ~~ onecbcl_q69_p
onecbcl_q71_p ~~ onecbcl_q71_p
onecbcl_q72_p ~~ onecbcl_q72_p
onecbcl_q74_p ~~ onecbcl_q74_p
onecbcl_q75_p ~~ onecbcl_q75_p
onecbcl_q76_p ~~ onecbcl_q76_p
onecbcl_q77_p ~~ onecbcl_q77_p
onecbcl_q79_p ~~ onecbcl_q79_p
onecbcl_q80_p ~~ onecbcl_q80_p
onecbcl_q83_p ~~ onecbcl_q83_p
onecbcl_q84_p ~~ onecbcl_q84_p
onecbcl_q85_p ~~ onecbcl_q85_p
onecbcl_q86_p ~~ onecbcl_q86_p
onecbcl_q87_p ~~ onecbcl_q87_p
onecbcl_q88_p ~~ onecbcl_q88_p
onecbcl_q89_p ~~ onecbcl_q89_p
onecbcl_q90_p ~~ onecbcl_q90_p
onecbcl_q91_p ~~ onecbcl_q91_p
onecbcl_q92_p ~~ onecbcl_q92_p
onecbcl_q93_p ~~ onecbcl_q93_p
onecbcl_q94_p ~~ onecbcl_q94_p
onecbcl_q95_p ~~ onecbcl_q95_p
onecbcl_q96_p ~~ onecbcl_q96_p
onecbcl_q98_p ~~ onecbcl_q98_p
onecbcl_q100_p ~~ onecbcl_q100_p
onecbcl_q102_p ~~ onecbcl_q102_p
onecbcl_q103_p ~~ onecbcl_q103_p
onecbcl_q104_p ~~ onecbcl_q104_p
onecbcl_q107_p ~~ onecbcl_q107_p
onecbcl_q108_p ~~ onecbcl_q108_p
onecbcl_q109_p ~~ onecbcl_q109_p
onecbcl_q110_p ~~ onecbcl_q110_p
onecbcl_q111_p ~~ onecbcl_q111_p
onecbcl_q112_p ~~ onecbcl_q112_p
oneAttacksThreatens ~~ oneAttacksThreatens
oneDestroys ~~ oneDestroys
oneDisobeysrules ~~ oneDisobeysrules
oneSteals ~~ oneSteals
onePeerProblems ~~ onePeerProblems
oneDistractedHyperactive ~~ oneDistractedHyperactive
oneHallucinations ~~ oneHallucinations
oneSexPlay ~~ oneSexPlay
oneWeightproblems ~~ oneWeightproblems

#Latent Variable Means
baseFactor1 ~ 0*1
baseFactor2 ~ 0*1
baseFactor3 ~ 0*1
baseFactor4 ~ 0*1
baseFactor5 ~ 0*1
oneFactor1 ~ 1
oneFactor2 ~ 1
oneFactor3 ~ 1
oneFactor4 ~ 1
oneFactor5 ~ 1

#Latent Variable Variances and Covariance
baseFactor1 ~~ 1*baseFactor1
baseFactor2 ~~ 1*baseFactor2
baseFactor3 ~~ 1*baseFactor3
baseFactor4 ~~ 1*baseFactor4
baseFactor5 ~~ 1*baseFactor5
oneFactor1 ~~ oneFactor1
oneFactor2 ~~ oneFactor2
oneFactor3 ~~ oneFactor3
oneFactor4 ~~ oneFactor4
oneFactor5 ~~ oneFactor5
baseFactor1 ~~ oneFactor1
baseFactor2 ~~ oneFactor2
baseFactor3 ~~ oneFactor3
baseFactor4 ~~ oneFactor4
baseFactor5 ~~ oneFactor5
'

strong_fit1 <- cfa(strong1, data = cbclDF1, mimic = "mplus")
summary(strong_fit1, fit.measures = TRUE, standardized=TRUE)
#factor.scores(cbclDF1, strong_fit1)

#create a model and CFA for baseline and two year STRONG INVARAINCE 
strong2<- '
#Define latent 
   baseFactor1=~
NA*baseAttacksThreatens
+lambda1*baseAttacksThreatens
+lambda2*basecbcl_q16_p
+lambda3*baseDisobeysrules
+lambda4*basecbcl_q37_p
+lambda5*basecbcl_q95_p
+lambda6*basecbcl_q03_p
+lambda7*baseDestroys
+lambda8*basecbcl_q68_p
+lambda9*basecbcl_q26_p
+lambda10*basecbcl_q90_p
+lambda11*basecbcl_q94_p
+lambda12*baseSteals
+lambda13*basecbcl_q86_p
+lambda14*basecbcl_q43_p
+lambda15*basecbcl_q15_p
+lambda16*basecbcl_q67_p
+lambda17*basecbcl_q87_p
+lambda18*basecbcl_q27_p
+lambda19*basePeerProblems
+lambda20*basecbcl_q89_p
+lambda21*basecbcl_q19_p
+lambda22*basecbcl_q96_p
+lambda23*basecbcl_q39_p
+lambda24*basecbcl_q34_p
+lambda25*basecbcl_q72_p
+lambda26*basecbcl_q88_p
+lambda27*basecbcl_q74_p
+lambda28*basecbcl_q07_p
+lambda29*basecbcl_q109_p
   twoFactor1=~
NA*twoAttacksThreatens
+lambda1*twoAttacksThreatens
+lambda2*twocbcl_q16_p
+lambda3*twoDisobeysrules
+lambda4*twocbcl_q37_p
+lambda5*twocbcl_q95_p
+lambda6*twocbcl_q03_p
+lambda7*twoDestroys
+lambda8*twocbcl_q68_p
+lambda9*twocbcl_q26_p
+lambda10*twocbcl_q90_p
+lambda11*twocbcl_q94_p
+lambda12*twoSteals
+lambda13*twocbcl_q86_p
+lambda14*twocbcl_q43_p
+lambda15*twocbcl_q15_p
+lambda16*twocbcl_q67_p
+lambda17*twocbcl_q87_p
+lambda18*twocbcl_q27_p
+lambda19*twoPeerProblems
+lambda20*twocbcl_q89_p
+lambda21*twocbcl_q19_p
+lambda22*twocbcl_q96_p
+lambda23*twocbcl_q39_p
+lambda24*twocbcl_q34_p
+lambda25*twocbcl_q72_p
+lambda26*twocbcl_q88_p
+lambda27*twocbcl_q74_p
+lambda28*twocbcl_q07_p
+lambda29*twocbcl_q109_p
   baseFactor2=~
NA*basecbcl_q50_p
+lambda30*basecbcl_q50_p
+lambda31*basecbcl_q112_p
+lambda32*basecbcl_q32_p
+lambda33*basecbcl_q52_p
+lambda34*basecbcl_q45_p
+lambda35*basecbcl_q31_p
+lambda36*basecbcl_q35_p
+lambda37*basecbcl_q71_p
+lambda38*basecbcl_q30_p
+lambda39*basecbcl_q29_p
+lambda40*basecbcl_q12_p
   twoFactor2=~
NA*twocbcl_q50_p
+lambda30*twocbcl_q50_p
+lambda31*twocbcl_q112_p
+lambda32*twocbcl_q32_p
+lambda33*twocbcl_q52_p
+lambda34*twocbcl_q45_p
+lambda35*twocbcl_q31_p
+lambda36*twocbcl_q35_p
+lambda37*twocbcl_q71_p
+lambda38*twocbcl_q30_p
+lambda39*twocbcl_q29_p
+lambda40*twocbcl_q12_p
   baseFactor3=~
NA*baseDistractedHyperactive
+lambda41*baseDistractedHyperactive
+lambda42*basecbcl_q17_p
+lambda43*basecbcl_q80_p
+lambda44*basecbcl_q13_p
+lambda45*basecbcl_q62_p
+lambda46*basecbcl_q46_p
+lambda47*basecbcl_q04_p
+lambda48*basecbcl_q93_p
+lambda49*basecbcl_q09_p
+lambda50*basecbcl_q61_p
+lambda51*basecbcl_q66_p
+lambda52*basecbcl_q85_p
+lambda53*basecbcl_q01_p
+lambda54*basecbcl_q36_p
+lambda55*basecbcl_q64_p
   twoFactor3=~
NA*twoDistractedHyperactive
+lambda41*twoDistractedHyperactive
+lambda42*twocbcl_q17_p
+lambda43*twocbcl_q80_p
+lambda44*twocbcl_q13_p
+lambda45*twocbcl_q62_p
+lambda46*twocbcl_q46_p
+lambda47*twocbcl_q04_p
+lambda48*twocbcl_q93_p
+lambda49*twocbcl_q09_p
+lambda50*twocbcl_q61_p
+lambda51*twocbcl_q66_p
+lambda52*twocbcl_q85_p
+lambda53*twocbcl_q01_p
+lambda54*twocbcl_q36_p
+lambda55*twocbcl_q64_p
   baseFactor4=~
NA*basecbcl_q56c_p
+lambda56*basecbcl_q56c_p
+lambda57*basecbcl_q56f_p
+lambda58*basecbcl_q56g_p
+lambda59*basecbcl_q56b_p
+lambda60*basecbcl_q56a_p
+lambda61*basecbcl_q51_p
+lambda62*basecbcl_q56h_p
+lambda63*basecbcl_q56d_p
+lambda64*basecbcl_q56e_p
   twoFactor4=~
NA*twocbcl_q56c_p
+lambda56*twocbcl_q56c_p
+lambda57*twocbcl_q56f_p
+lambda58*twocbcl_q56g_p
+lambda59*twocbcl_q56b_p
+lambda60*twocbcl_q56a_p
+lambda61*twocbcl_q51_p
+lambda62*twocbcl_q56h_p
+lambda63*twocbcl_q56d_p
+lambda64*twocbcl_q56e_p
   baseFactor5=~
NA*basecbcl_q111_p
+lambda65*basecbcl_q111_p
+lambda66*basecbcl_q42_p
+lambda67*basecbcl_q75_p
+lambda68*basecbcl_q65_p
+lambda69*basecbcl_q102_p
   twoFactor5=~
NA*twocbcl_q111_p
+lambda65*twocbcl_q111_p
+lambda66*twocbcl_q42_p
+lambda67*twocbcl_q75_p
+lambda68*twocbcl_q65_p
+lambda69*twocbcl_q102_p

#Intercepts
basecbcl_q01_p ~ i1*1
basecbcl_q03_p ~ i2*1
basecbcl_q04_p ~ i3*1
basecbcl_q05_p ~ i4*1
basecbcl_q06_p ~ i5*1
basecbcl_q07_p ~ i6*1
basecbcl_q09_p ~ i7*1
basecbcl_q11_p ~ i8*1
basecbcl_q12_p ~ i9*1
basecbcl_q13_p ~ i10*1
basecbcl_q14_p ~ i11*1
basecbcl_q15_p ~ i12*1
basecbcl_q16_p ~ i13*1
basecbcl_q17_p ~ i14*1
basecbcl_q18_p ~ i15*1
basecbcl_q19_p ~ i16*1
basecbcl_q24_p ~ i17*1
basecbcl_q26_p ~ i18*1
basecbcl_q27_p ~ i19*1
basecbcl_q29_p ~ i20*1
basecbcl_q30_p ~ i21*1
basecbcl_q31_p ~ i22*1
basecbcl_q32_p ~ i23*1
basecbcl_q33_p ~ i24*1
basecbcl_q34_p ~ i25*1
basecbcl_q35_p ~ i26*1
basecbcl_q36_p ~ i27*1
basecbcl_q37_p ~ i28*1
basecbcl_q38_p ~ i29*1
basecbcl_q39_p ~ i30*1
basecbcl_q41_p ~ i31*1
basecbcl_q42_p ~ i32*1
basecbcl_q43_p ~ i33*1
basecbcl_q44_p ~ i34*1
basecbcl_q45_p ~ i35*1
basecbcl_q46_p ~ i36*1
basecbcl_q47_p ~ i37*1
basecbcl_q49_p ~ i38*1
basecbcl_q50_p ~ i39*1
basecbcl_q51_p ~ i40*1
basecbcl_q52_p ~ i41*1
basecbcl_q54_p ~ i42*1
basecbcl_q56a_p ~ i43*1
basecbcl_q56b_p ~ i44*1
basecbcl_q56c_p ~ i45*1
basecbcl_q56d_p ~ i46*1
basecbcl_q56e_p ~ i47*1
basecbcl_q56f_p ~ i48*1
basecbcl_q56g_p ~ i49*1
basecbcl_q56h_p ~ i50*1
basecbcl_q58_p ~ i51*1
basecbcl_q61_p ~ i52*1
basecbcl_q62_p ~ i53*1
basecbcl_q63_p ~ i54*1
basecbcl_q64_p ~ i55*1
basecbcl_q65_p ~ i56*1
basecbcl_q66_p ~ i57*1
basecbcl_q67_p ~ i58*1
basecbcl_q68_p ~ i59*1
basecbcl_q69_p ~ i60*1
basecbcl_q71_p ~ i61*1
basecbcl_q72_p ~ i62*1
basecbcl_q74_p ~ i63*1
basecbcl_q75_p ~ i64*1
basecbcl_q76_p ~ i65*1
basecbcl_q77_p ~ i66*1
basecbcl_q79_p ~ i67*1
basecbcl_q80_p ~ i68*1
basecbcl_q83_p ~ i69*1
basecbcl_q84_p ~ i70*1
basecbcl_q85_p ~ i71*1
basecbcl_q86_p ~ i72*1
basecbcl_q87_p ~ i73*1
basecbcl_q88_p ~ i74*1
basecbcl_q89_p ~ i75*1
basecbcl_q90_p ~ i76*1
basecbcl_q91_p ~ i77*1
basecbcl_q92_p ~ i78*1
basecbcl_q93_p ~ i79*1
basecbcl_q94_p ~ i80*1
basecbcl_q95_p ~ i81*1
basecbcl_q96_p ~ i82*1
basecbcl_q98_p ~ i83*1
basecbcl_q100_p ~ i84*1
basecbcl_q102_p ~ i85*1
basecbcl_q103_p ~ i86*1
basecbcl_q104_p ~ i87*1
basecbcl_q107_p ~ i88*1
basecbcl_q108_p ~ i89*1
basecbcl_q109_p ~ i90*1
basecbcl_q110_p ~ i91*1
basecbcl_q111_p ~ i92*1
basecbcl_q112_p ~ i93*1
baseAttacksThreatens ~ i94*1
baseDestroys ~ i95*1
baseDisobeysrules ~ i96*1
baseSteals ~ i97*1
basePeerProblems ~ i98*1
baseDistractedHyperactive ~ i99*1
baseHallucinations ~ i100*1
baseSexPlay ~ i101*1
baseWeightproblems~ i102*1 
twocbcl_q01_p ~ i1*1
twocbcl_q03_p ~ i2*1
twocbcl_q04_p ~ i3*1
twocbcl_q05_p ~ i4*1
twocbcl_q06_p ~ i5*1
twocbcl_q07_p ~ i6*1
twocbcl_q09_p ~ i7*1
twocbcl_q11_p ~ i8*1
twocbcl_q12_p ~ i9*1
twocbcl_q13_p ~ i10*1
twocbcl_q14_p ~ i11*1
twocbcl_q15_p ~ i12*1
twocbcl_q16_p ~ i13*1
twocbcl_q17_p ~ i14*1
twocbcl_q18_p ~ i15*1
twocbcl_q19_p ~ i16*1
twocbcl_q24_p ~ i17*1
twocbcl_q26_p ~ i18*1
twocbcl_q27_p ~ i19*1
twocbcl_q29_p ~ i20*1
twocbcl_q30_p ~ i21*1
twocbcl_q31_p ~ i22*1
twocbcl_q32_p ~ i23*1
twocbcl_q33_p ~ i24*1
twocbcl_q34_p ~ i25*1
twocbcl_q35_p ~ i26*1
twocbcl_q36_p ~ i27*1
twocbcl_q37_p ~ i28*1
twocbcl_q38_p ~ i29*1
twocbcl_q39_p ~ i30*1
twocbcl_q41_p ~ i31*1
twocbcl_q42_p ~ i32*1
twocbcl_q43_p ~ i33*1
twocbcl_q44_p ~ i34*1
twocbcl_q45_p ~ i35*1
twocbcl_q46_p ~ i36*1
twocbcl_q47_p ~ i37*1
twocbcl_q49_p ~ i38*1
twocbcl_q50_p ~ i39*1
twocbcl_q51_p ~ i40*1
twocbcl_q52_p ~ i41*1
twocbcl_q54_p ~ i42*1
twocbcl_q56a_p ~ i43*1
twocbcl_q56b_p ~ i44*1
twocbcl_q56c_p ~ i45*1
twocbcl_q56d_p ~ i46*1
twocbcl_q56e_p ~ i47*1
twocbcl_q56f_p ~ i48*1
twocbcl_q56g_p ~ i49*1
twocbcl_q56h_p ~ i50*1
twocbcl_q58_p ~ i51*1
twocbcl_q61_p ~ i52*1
twocbcl_q62_p ~ i53*1
twocbcl_q63_p ~ i54*1
twocbcl_q64_p ~ i55*1
twocbcl_q65_p ~ i56*1
twocbcl_q66_p ~ i57*1
twocbcl_q67_p ~ i58*1
twocbcl_q68_p ~ i59*1
twocbcl_q69_p ~ i60*1
twocbcl_q71_p ~ i61*1
twocbcl_q72_p ~ i62*1
twocbcl_q74_p ~ i63*1
twocbcl_q75_p ~ i64*1
twocbcl_q76_p ~ i65*1
twocbcl_q77_p ~ i66*1
twocbcl_q79_p ~ i67*1
twocbcl_q80_p ~ i68*1
twocbcl_q83_p ~ i69*1
twocbcl_q84_p ~ i70*1
twocbcl_q85_p ~ i71*1
twocbcl_q86_p ~ i72*1
twocbcl_q87_p ~ i73*1
twocbcl_q88_p ~ i74*1
twocbcl_q89_p ~ i75*1
twocbcl_q90_p ~ i76*1
twocbcl_q91_p ~ i77*1
twocbcl_q92_p ~ i78*1
twocbcl_q93_p ~ i79*1
twocbcl_q94_p ~ i80*1
twocbcl_q95_p ~ i81*1
twocbcl_q96_p ~ i82*1
twocbcl_q98_p ~ i83*1
twocbcl_q100_p ~ i84*1
twocbcl_q102_p ~ i85*1
twocbcl_q103_p ~ i86*1
twocbcl_q104_p ~ i87*1
twocbcl_q107_p ~ i88*1
twocbcl_q108_p ~ i89*1
twocbcl_q109_p ~ i90*1
twocbcl_q110_p ~ i91*1
twocbcl_q111_p ~ i92*1
twocbcl_q112_p ~ i93*1
twoAttacksThreatens ~ i94*1
twoDestroys ~ i95*1
twoDisobeysrules ~ i96*1
twoSteals ~ i97*1
twoPeerProblems ~ i98*1
twoDistractedHyperactive ~ i99*1
twoHallucinations ~ i100*1
twoSexPlay ~ i101*1
twoWeightproblems~ i102*1 


#Unique Variances

basecbcl_q01_p ~~ basecbcl_q01_p
basecbcl_q03_p ~~ basecbcl_q03_p
basecbcl_q04_p ~~ basecbcl_q04_p
basecbcl_q05_p ~~ basecbcl_q05_p
basecbcl_q06_p ~~ basecbcl_q06_p
basecbcl_q07_p ~~ basecbcl_q07_p
basecbcl_q09_p ~~ basecbcl_q09_p
basecbcl_q11_p ~~ basecbcl_q11_p
basecbcl_q12_p ~~ basecbcl_q12_p
basecbcl_q13_p ~~ basecbcl_q13_p
basecbcl_q14_p ~~ basecbcl_q14_p
basecbcl_q15_p ~~ basecbcl_q15_p
basecbcl_q16_p ~~ basecbcl_q16_p
basecbcl_q17_p ~~ basecbcl_q17_p
basecbcl_q18_p ~~ basecbcl_q18_p
basecbcl_q19_p ~~ basecbcl_q19_p
basecbcl_q24_p ~~ basecbcl_q24_p
basecbcl_q26_p ~~ basecbcl_q26_p
basecbcl_q27_p ~~ basecbcl_q27_p
basecbcl_q29_p ~~ basecbcl_q29_p
basecbcl_q30_p ~~ basecbcl_q30_p
basecbcl_q31_p ~~ basecbcl_q31_p
basecbcl_q32_p ~~ basecbcl_q32_p
basecbcl_q33_p ~~ basecbcl_q33_p
basecbcl_q34_p ~~ basecbcl_q34_p
basecbcl_q35_p ~~ basecbcl_q35_p
basecbcl_q36_p ~~ basecbcl_q36_p
basecbcl_q37_p ~~ basecbcl_q37_p
basecbcl_q38_p ~~ basecbcl_q38_p
basecbcl_q39_p ~~ basecbcl_q39_p
basecbcl_q41_p ~~ basecbcl_q41_p
basecbcl_q42_p ~~ basecbcl_q42_p
basecbcl_q43_p ~~ basecbcl_q43_p
basecbcl_q44_p ~~ basecbcl_q44_p
basecbcl_q45_p ~~ basecbcl_q45_p
basecbcl_q46_p ~~ basecbcl_q46_p
basecbcl_q47_p ~~ basecbcl_q47_p
basecbcl_q49_p ~~ basecbcl_q49_p
basecbcl_q50_p ~~ basecbcl_q50_p
basecbcl_q51_p ~~ basecbcl_q51_p
basecbcl_q52_p ~~ basecbcl_q52_p
basecbcl_q54_p ~~ basecbcl_q54_p
basecbcl_q56a_p ~~ basecbcl_q56a_p
basecbcl_q56b_p ~~ basecbcl_q56b_p
basecbcl_q56c_p ~~ basecbcl_q56c_p
basecbcl_q56d_p ~~ basecbcl_q56d_p
basecbcl_q56e_p ~~ basecbcl_q56e_p
basecbcl_q56f_p ~~ basecbcl_q56f_p
basecbcl_q56g_p ~~ basecbcl_q56g_p
basecbcl_q56h_p ~~ basecbcl_q56h_p
basecbcl_q58_p ~~ basecbcl_q58_p
basecbcl_q61_p ~~ basecbcl_q61_p
basecbcl_q62_p ~~ basecbcl_q62_p
basecbcl_q63_p ~~ basecbcl_q63_p
basecbcl_q64_p ~~ basecbcl_q64_p
basecbcl_q65_p ~~ basecbcl_q65_p
basecbcl_q66_p ~~ basecbcl_q66_p
basecbcl_q67_p ~~ basecbcl_q67_p
basecbcl_q68_p ~~ basecbcl_q68_p
basecbcl_q69_p ~~ basecbcl_q69_p
basecbcl_q71_p ~~ basecbcl_q71_p
basecbcl_q72_p ~~ basecbcl_q72_p
basecbcl_q74_p ~~ basecbcl_q74_p
basecbcl_q75_p ~~ basecbcl_q75_p
basecbcl_q76_p ~~ basecbcl_q76_p
basecbcl_q77_p ~~ basecbcl_q77_p
basecbcl_q79_p ~~ basecbcl_q79_p
basecbcl_q80_p ~~ basecbcl_q80_p
basecbcl_q83_p ~~ basecbcl_q83_p
basecbcl_q84_p ~~ basecbcl_q84_p
basecbcl_q85_p ~~ basecbcl_q85_p
basecbcl_q86_p ~~ basecbcl_q86_p
basecbcl_q87_p ~~ basecbcl_q87_p
basecbcl_q88_p ~~ basecbcl_q88_p
basecbcl_q89_p ~~ basecbcl_q89_p
basecbcl_q90_p ~~ basecbcl_q90_p
basecbcl_q91_p ~~ basecbcl_q91_p
basecbcl_q92_p ~~ basecbcl_q92_p
basecbcl_q93_p ~~ basecbcl_q93_p
basecbcl_q94_p ~~ basecbcl_q94_p
basecbcl_q95_p ~~ basecbcl_q95_p
basecbcl_q96_p ~~ basecbcl_q96_p
basecbcl_q98_p ~~ basecbcl_q98_p
basecbcl_q100_p ~~ basecbcl_q100_p
basecbcl_q102_p ~~ basecbcl_q102_p
basecbcl_q103_p ~~ basecbcl_q103_p
basecbcl_q104_p ~~ basecbcl_q104_p
basecbcl_q107_p ~~ basecbcl_q107_p
basecbcl_q108_p ~~ basecbcl_q108_p
basecbcl_q109_p ~~ basecbcl_q109_p
basecbcl_q110_p ~~ basecbcl_q110_p
basecbcl_q111_p ~~ basecbcl_q111_p
basecbcl_q112_p ~~ basecbcl_q112_p
baseAttacksThreatens ~~ baseAttacksThreatens
baseDestroys ~~ baseDestroys
baseDisobeysrules ~~ baseDisobeysrules
baseSteals ~~ baseSteals
basePeerProblems ~~ basePeerProblems 
baseDistractedHyperactive ~~ baseDistractedHyperactive
baseHallucinations ~~ baseHallucinations
baseSexPlay ~~ baseSexPlay
baseWeightproblems ~~ baseWeightproblems

twocbcl_q01_p ~~ twocbcl_q01_p
twocbcl_q03_p ~~ twocbcl_q03_p
twocbcl_q04_p ~~ twocbcl_q04_p
twocbcl_q05_p ~~ twocbcl_q05_p
twocbcl_q06_p ~~ twocbcl_q06_p
twocbcl_q07_p ~~ twocbcl_q07_p
twocbcl_q09_p ~~ twocbcl_q09_p
twocbcl_q11_p ~~ twocbcl_q11_p
twocbcl_q12_p ~~ twocbcl_q12_p
twocbcl_q13_p ~~ twocbcl_q13_p
twocbcl_q14_p ~~ twocbcl_q14_p
twocbcl_q15_p ~~ twocbcl_q15_p
twocbcl_q16_p ~~ twocbcl_q16_p
twocbcl_q17_p ~~ twocbcl_q17_p
twocbcl_q18_p ~~ twocbcl_q18_p
twocbcl_q19_p ~~ twocbcl_q19_p
twocbcl_q24_p ~~ twocbcl_q24_p
twocbcl_q26_p ~~ twocbcl_q26_p
twocbcl_q27_p ~~ twocbcl_q27_p
twocbcl_q29_p ~~ twocbcl_q29_p
twocbcl_q30_p ~~ twocbcl_q30_p
twocbcl_q31_p ~~ twocbcl_q31_p
twocbcl_q32_p ~~ twocbcl_q32_p
twocbcl_q33_p ~~ twocbcl_q33_p
twocbcl_q34_p ~~ twocbcl_q34_p
twocbcl_q35_p ~~ twocbcl_q35_p
twocbcl_q36_p ~~ twocbcl_q36_p
twocbcl_q37_p ~~ twocbcl_q37_p
twocbcl_q38_p ~~ twocbcl_q38_p
twocbcl_q39_p ~~ twocbcl_q39_p
twocbcl_q41_p ~~ twocbcl_q41_p
twocbcl_q42_p ~~ twocbcl_q42_p
twocbcl_q43_p ~~ twocbcl_q43_p
twocbcl_q44_p ~~ twocbcl_q44_p
twocbcl_q45_p ~~ twocbcl_q45_p
twocbcl_q46_p ~~ twocbcl_q46_p
twocbcl_q47_p ~~ twocbcl_q47_p
twocbcl_q49_p ~~ twocbcl_q49_p
twocbcl_q50_p ~~ twocbcl_q50_p
twocbcl_q51_p ~~ twocbcl_q51_p
twocbcl_q52_p ~~ twocbcl_q52_p
twocbcl_q54_p ~~ twocbcl_q54_p
twocbcl_q56a_p ~~ twocbcl_q56a_p
twocbcl_q56b_p ~~ twocbcl_q56b_p
twocbcl_q56c_p ~~ twocbcl_q56c_p
twocbcl_q56d_p ~~ twocbcl_q56d_p
twocbcl_q56e_p ~~ twocbcl_q56e_p
twocbcl_q56f_p ~~ twocbcl_q56f_p
twocbcl_q56g_p ~~ twocbcl_q56g_p
twocbcl_q56h_p ~~ twocbcl_q56h_p
twocbcl_q58_p ~~ twocbcl_q58_p
twocbcl_q61_p ~~ twocbcl_q61_p
twocbcl_q62_p ~~ twocbcl_q62_p
twocbcl_q63_p ~~ twocbcl_q63_p
twocbcl_q64_p ~~ twocbcl_q64_p
twocbcl_q65_p ~~ twocbcl_q65_p
twocbcl_q66_p ~~ twocbcl_q66_p
twocbcl_q67_p ~~ twocbcl_q67_p
twocbcl_q68_p ~~ twocbcl_q68_p
twocbcl_q69_p ~~ twocbcl_q69_p
twocbcl_q71_p ~~ twocbcl_q71_p
twocbcl_q72_p ~~ twocbcl_q72_p
twocbcl_q74_p ~~ twocbcl_q74_p
twocbcl_q75_p ~~ twocbcl_q75_p
twocbcl_q76_p ~~ twocbcl_q76_p
twocbcl_q77_p ~~ twocbcl_q77_p
twocbcl_q79_p ~~ twocbcl_q79_p
twocbcl_q80_p ~~ twocbcl_q80_p
twocbcl_q83_p ~~ twocbcl_q83_p
twocbcl_q84_p ~~ twocbcl_q84_p
twocbcl_q85_p ~~ twocbcl_q85_p
twocbcl_q86_p ~~ twocbcl_q86_p
twocbcl_q87_p ~~ twocbcl_q87_p
twocbcl_q88_p ~~ twocbcl_q88_p
twocbcl_q89_p ~~ twocbcl_q89_p
twocbcl_q90_p ~~ twocbcl_q90_p
twocbcl_q91_p ~~ twocbcl_q91_p
twocbcl_q92_p ~~ twocbcl_q92_p
twocbcl_q93_p ~~ twocbcl_q93_p
twocbcl_q94_p ~~ twocbcl_q94_p
twocbcl_q95_p ~~ twocbcl_q95_p
twocbcl_q96_p ~~ twocbcl_q96_p
twocbcl_q98_p ~~ twocbcl_q98_p
twocbcl_q100_p ~~ twocbcl_q100_p
twocbcl_q102_p ~~ twocbcl_q102_p
twocbcl_q103_p ~~ twocbcl_q103_p
twocbcl_q104_p ~~ twocbcl_q104_p
twocbcl_q107_p ~~ twocbcl_q107_p
twocbcl_q108_p ~~ twocbcl_q108_p
twocbcl_q109_p ~~ twocbcl_q109_p
twocbcl_q110_p ~~ twocbcl_q110_p
twocbcl_q111_p ~~ twocbcl_q111_p
twocbcl_q112_p ~~ twocbcl_q112_p
twoAttacksThreatens ~~ twoAttacksThreatens
twoDestroys ~~ twoDestroys
twoDisobeysrules ~~ twoDisobeysrules
twoSteals ~~ twoSteals
twoPeerProblems ~~ twoPeerProblems
twoDistractedHyperactive ~~ twoDistractedHyperactive
twoHallucinations ~~ twoHallucinations
twoSexPlay ~~ twoSexPlay
twoWeightproblems ~~ twoWeightproblems

#Latent Variable
baseFactor1 ~ 0*1
baseFactor2 ~ 0*1
baseFactor3 ~ 0*1
baseFactor4 ~ 0*1
baseFactor5 ~ 0*1
twoFactor1 ~ 1
twoFactor2 ~ 1
twoFactor3 ~ 1
twoFactor4 ~ 1
twoFactor5 ~ 1

#Latent Variable Variances and Covariance
baseFactor1 ~~ 1*baseFactor1
baseFactor2 ~~ 1*baseFactor2
baseFactor3 ~~ 1*baseFactor3
baseFactor4 ~~ 1*baseFactor4
baseFactor5 ~~ 1*baseFactor5
twoFactor1 ~~ twoFactor1
twoFactor2 ~~ twoFactor2
twoFactor3 ~~ twoFactor3
twoFactor4 ~~ twoFactor4
twoFactor5 ~~ twoFactor5
baseFactor1 ~~ twoFactor1
baseFactor2 ~~ twoFactor2
baseFactor3 ~~ twoFactor3
baseFactor4 ~~ twoFactor4
baseFactor5 ~~ twoFactor4
'

strong_fit2 <- cfa(strong2, data = cbclDF2, mimic = "mplus")
summary(strong_fit2, fit.measures = TRUE, standardized=TRUE)
#factor.scores(cbclDF2, strong_fit2)

#Model diagram for baseline-one year
#semPaths(strong_fit1, what="est", sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#Model diagram for baseline-two year
#semPaths(strong_fit2, what="est", sizeLat = 7, sizeMan = 7, edge.label.cex = .75)


#####STEP 6: FACTOR LOADINGS 
inspect(strong_fit1 ,what="std")$lambda
inspect(strong_fit2 ,what="std")$lambda

######STEP 7: FACTOR SCORES
strong1_scores <- lavPredict(strong_fit1)
strong2_scores <- lavPredict(strong_fit2)

