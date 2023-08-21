#Longitudnal Factor Analysis 
#Sample ABCD - Goal #1 and #2

#####STEP 1:
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



######STEP 2: PREP for longitudinal CFA by creating a new data frame where... 
#...each subject key has a baseline, one year and two year score for each question


#Two CFA will be done (one for baseline vs one year and one for baseline vs twoyear)

#combine dataframe - baseline and one year 
cbclDF1=inner_join(baselineData,oneyearData, 
                   by = c("basesubjectkey" = "onesubjectkey")
) 

head(cbclDF1)

#combine Data frame - baseline vs twoyear
cbclDF2=inner_join(baselineData,twoyearData, 
                   by = c("basesubjectkey" = "twosubjectkey")
) 

head(cbclDF2)


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

######STEP 4: LONGITUDNAL CFA CONFIGURAL INVARIANCE 
#libraries downloaded above

#create a model and CFA for baseline and one year
configural1<- '
#Define latent 
   baseFactor1=~
NA*baseAttacksThreatens
+lambda1*baseAttacksThreatens
+basecbcl_q16_p
+baseDisobeysrules
+basecbcl_q37_p
+basecbcl_q95_p
+basecbcl_q03_p
+baseDestroys
+basecbcl_q68_p
+basecbcl_q26_p
+basecbcl_q90_p
+basecbcl_q94_p
+baseSteals
+basecbcl_q86_p
+basecbcl_q43_p
+basecbcl_q15_p
+basecbcl_q67_p
+basecbcl_q87_p
+basecbcl_q27_p
+basePeerProblems
+basecbcl_q89_p
+basecbcl_q19_p
+basecbcl_q96_p
+basecbcl_q39_p
+basecbcl_q34_p
+basecbcl_q72_p
+basecbcl_q88_p
+basecbcl_q74_p
+basecbcl_q07_p
+basecbcl_q109_p
   oneFactor1=~
NA*oneAttacksThreatens
+lambda1*oneAttacksThreatens
+onecbcl_q16_p
+oneDisobeysrules
+onecbcl_q37_p
+onecbcl_q95_p
+onecbcl_q03_p
+oneDestroys
+onecbcl_q68_p
+onecbcl_q26_p
+onecbcl_q90_p
+onecbcl_q94_p
+oneSteals
+onecbcl_q86_p
+onecbcl_q43_p
+onecbcl_q15_p
+onecbcl_q67_p
+onecbcl_q87_p
+onecbcl_q27_p
+onePeerProblems
+onecbcl_q89_p
+onecbcl_q19_p
+onecbcl_q96_p
+onecbcl_q39_p
+onecbcl_q34_p
+onecbcl_q72_p
+onecbcl_q88_p
+onecbcl_q74_p
+onecbcl_q07_p
+onecbcl_q109_p
   baseFactor2=~
NA*basecbcl_q50_p
+lambda1*basecbcl_q50_p
+basecbcl_q112_p
+basecbcl_q32_p
+basecbcl_q52_p
+basecbcl_q45_p
+basecbcl_q31_p
+basecbcl_q35_p
+basecbcl_q71_p
+basecbcl_q30_p
+basecbcl_q29_p
+basecbcl_q12_p
   oneFactor2=~
NA*onecbcl_q50_p
+lambda1*onecbcl_q50_p
+onecbcl_q112_p
+onecbcl_q32_p
+onecbcl_q52_p
+onecbcl_q45_p
+onecbcl_q31_p
+onecbcl_q35_p
+onecbcl_q71_p
+onecbcl_q30_p
+onecbcl_q29_p
+onecbcl_q12_p
   baseFactor3=~
NA*baseDistractedHyperactive
+lambda1*baseDistractedHyperactive
+basecbcl_q17_p
+basecbcl_q80_p
+basecbcl_q13_p
+basecbcl_q62_p
+basecbcl_q46_p
+basecbcl_q04_p
+basecbcl_q93_p
+basecbcl_q09_p
+basecbcl_q61_p
+basecbcl_q66_p
+basecbcl_q85_p
+basecbcl_q01_p
+basecbcl_q36_p
+basecbcl_q64_p
   oneFactor3=~
NA*oneDistractedHyperactive
+lambda1*oneDistractedHyperactive
+onecbcl_q17_p
+onecbcl_q80_p
+onecbcl_q13_p
+onecbcl_q62_p
+onecbcl_q46_p
+onecbcl_q04_p
+onecbcl_q93_p
+onecbcl_q09_p
+onecbcl_q61_p
+onecbcl_q66_p
+onecbcl_q85_p
+onecbcl_q01_p
+onecbcl_q36_p
+onecbcl_q64_p
   baseFactor4=~
NA*basecbcl_q56c_p
+lambda1*basecbcl_q56c_p
+basecbcl_q56f_p
+basecbcl_q56g_p
+basecbcl_q56b_p
+basecbcl_q56a_p
+basecbcl_q51_p
+basecbcl_q56h_p
+basecbcl_q56d_p
+basecbcl_q56e_p
   oneFactor4=~
NA*onecbcl_q56c_p
+lambda1*onecbcl_q56c_p
+onecbcl_q56f_p
+onecbcl_q56g_p
+onecbcl_q56b_p
+onecbcl_q56a_p
+onecbcl_q51_p
+onecbcl_q56h_p
+onecbcl_q56d_p
+onecbcl_q56e_p
   baseFactor5=~
NA*basecbcl_q111_p
lambda1*basecbcl_q111_p
+basecbcl_q42_p
+basecbcl_q75_p
+basecbcl_q65_p
+basecbcl_q102_p
   oneFactor5=~
NA*onecbcl_q111_p
+onecbcl_q111_p
+onecbcl_q42_p
+onecbcl_q75_p
+onecbcl_q65_p
+onecbcl_q102_p

#Intercepts
basecbcl_q01_p ~ i1*1
basecbcl_q03_p ~ 1
basecbcl_q04_p ~ 1
basecbcl_q05_p ~ 1
basecbcl_q06_p ~ 1
basecbcl_q07_p ~ 1
basecbcl_q09_p ~ 1
basecbcl_q11_p ~ 1
basecbcl_q12_p ~ 1
basecbcl_q13_p ~ 1
basecbcl_q14_p ~ 1
basecbcl_q15_p ~ 1
basecbcl_q16_p ~ 1
basecbcl_q17_p ~ 1
basecbcl_q18_p ~ 1
basecbcl_q19_p ~ 1
basecbcl_q24_p ~ 1
basecbcl_q26_p ~ 1
basecbcl_q27_p ~ 1
basecbcl_q29_p ~ 1
basecbcl_q30_p ~ 1
basecbcl_q31_p ~ 1
basecbcl_q32_p ~ 1
basecbcl_q33_p ~ 1
basecbcl_q34_p ~ 1
basecbcl_q35_p ~ 1
basecbcl_q36_p ~ 1
basecbcl_q37_p ~ 1
basecbcl_q38_p ~ 1
basecbcl_q39_p ~ 1
basecbcl_q41_p ~ 1
basecbcl_q42_p ~ 1
basecbcl_q43_p ~ 1
basecbcl_q44_p ~ 1
basecbcl_q45_p ~ 1
basecbcl_q46_p ~ 1
basecbcl_q47_p ~ 1
basecbcl_q49_p ~ 1
basecbcl_q50_p ~ 1
basecbcl_q51_p ~ 1
basecbcl_q52_p ~ 1
basecbcl_q54_p ~ 1
basecbcl_q56a_p ~ 1
basecbcl_q56b_p ~ 1
basecbcl_q56c_p ~ 1
basecbcl_q56d_p ~ 1
basecbcl_q56e_p ~ 1
basecbcl_q56f_p ~ 1
basecbcl_q56g_p ~ 1
basecbcl_q56h_p ~ 1
basecbcl_q58_p ~ 1
basecbcl_q61_p ~ 1
basecbcl_q62_p ~ 1
basecbcl_q63_p ~ 1
basecbcl_q64_p ~ 1
basecbcl_q65_p ~ 1
basecbcl_q66_p ~ 1
basecbcl_q67_p ~ 1
basecbcl_q68_p ~ 1
basecbcl_q69_p ~ 1
basecbcl_q71_p ~ 1
basecbcl_q72_p ~ 1
basecbcl_q74_p ~ 1
basecbcl_q75_p ~ 1
basecbcl_q76_p ~ 1
basecbcl_q77_p ~ 1
basecbcl_q79_p ~ 1
basecbcl_q80_p ~ 1
basecbcl_q83_p ~ 1
basecbcl_q84_p ~ 1
basecbcl_q85_p ~ 1
basecbcl_q86_p ~ 1
basecbcl_q87_p ~ 1
basecbcl_q88_p ~ 1
basecbcl_q89_p ~ 1
basecbcl_q90_p ~ 1
basecbcl_q91_p ~ 1
basecbcl_q92_p ~ 1
basecbcl_q93_p ~ 1
basecbcl_q94_p ~ 1
basecbcl_q95_p ~ 1
basecbcl_q96_p ~ 1
basecbcl_q98_p ~ 1
basecbcl_q100_p ~ 1
basecbcl_q102_p ~ 1
basecbcl_q103_p ~ 1
basecbcl_q104_p ~ 1
basecbcl_q107_p ~ 1
basecbcl_q108_p ~ 1
basecbcl_q109_p ~ 1
basecbcl_q110_p ~ 1
basecbcl_q111_p ~ 1
basecbcl_q112_p ~ 1
baseAttacksThreatens ~ 1
baseDestroys ~ 1
baseDisobeysrules ~ 1
baseSteals ~ 1
basePeerProblems ~ 1
baseDistractedHyperactive ~ 1
baseHallucinations ~ 1
baseSexPlay ~ 1
baseWeightproblems ~ 1 
onecbcl_q01_p ~ i1*1
onecbcl_q03_p ~ 1
onecbcl_q04_p ~ 1
onecbcl_q05_p ~ 1
onecbcl_q06_p ~ 1
onecbcl_q07_p ~ 1
onecbcl_q09_p ~ 1
onecbcl_q11_p ~ 1
onecbcl_q12_p ~ 1
onecbcl_q13_p ~ 1
onecbcl_q14_p ~ 1
onecbcl_q15_p ~ 1
onecbcl_q16_p ~ 1
onecbcl_q17_p ~ 1
onecbcl_q18_p ~ 1
onecbcl_q19_p ~ 1
onecbcl_q24_p ~ 1
onecbcl_q26_p ~ 1
onecbcl_q27_p ~ 1
onecbcl_q29_p ~ 1
onecbcl_q30_p ~ 1
onecbcl_q31_p ~ 1
onecbcl_q32_p ~ 1
onecbcl_q33_p ~ 1
onecbcl_q34_p ~ 1
onecbcl_q35_p ~ 1
onecbcl_q36_p ~ 1
onecbcl_q37_p ~ 1
onecbcl_q38_p ~ 1
onecbcl_q39_p ~ 1
onecbcl_q41_p ~ 1
onecbcl_q42_p ~ 1
onecbcl_q43_p ~ 1
onecbcl_q44_p ~ 1
onecbcl_q45_p ~ 1
onecbcl_q46_p ~ 1
onecbcl_q47_p ~ 1
onecbcl_q49_p ~ 1
onecbcl_q50_p ~ 1
onecbcl_q51_p ~ 1
onecbcl_q52_p ~ 1
onecbcl_q54_p ~ 1
onecbcl_q56a_p ~ 1
onecbcl_q56b_p ~ 1
onecbcl_q56c_p ~ 1
onecbcl_q56d_p ~ 1
onecbcl_q56e_p ~ 1
onecbcl_q56f_p ~ 1
onecbcl_q56g_p ~ 1
onecbcl_q56h_p ~ 1
onecbcl_q58_p ~ 1
onecbcl_q61_p ~ 1
onecbcl_q62_p ~ 1
onecbcl_q63_p ~ 1
onecbcl_q64_p ~ 1
onecbcl_q65_p ~ 1
onecbcl_q66_p ~ 1
onecbcl_q67_p ~ 1
onecbcl_q68_p ~ 1
onecbcl_q69_p ~ 1
onecbcl_q71_p ~ 1
onecbcl_q72_p ~ 1
onecbcl_q74_p ~ 1
onecbcl_q75_p ~ 1
onecbcl_q76_p ~ 1
onecbcl_q77_p ~ 1
onecbcl_q79_p ~ 1
onecbcl_q80_p ~ 1
onecbcl_q83_p ~ 1
onecbcl_q84_p ~ 1
onecbcl_q85_p ~ 1
onecbcl_q86_p ~ 1
onecbcl_q87_p ~ 1
onecbcl_q88_p ~ 1
onecbcl_q89_p ~ 1
onecbcl_q90_p ~ 1
onecbcl_q91_p ~ 1
onecbcl_q92_p ~ 1
onecbcl_q93_p ~ 1
onecbcl_q94_p ~ 1
onecbcl_q95_p ~ 1
onecbcl_q96_p ~ 1
onecbcl_q98_p ~ 1
onecbcl_q100_p ~ 1
onecbcl_q102_p ~ 1
onecbcl_q103_p ~ 1
onecbcl_q104_p ~ 1
onecbcl_q107_p ~ 1
onecbcl_q108_p ~ 1
onecbcl_q109_p ~ 1
onecbcl_q110_p ~ 1
onecbcl_q111_p ~ 1
onecbcl_q112_p ~ 1
oneAttacksThreatens ~ 1
oneDestroys ~ 1
oneDisobeysrules ~ 1
oneSteals ~ 1
onePeerProblems ~ 1
oneDistractedHyperactive ~ 1
oneHallucinations ~ 1
oneSexPlay ~ 1
oneWeightproblems ~ 1


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

configural_fit1 <- cfa(configural1, data = cbclDF1, mimic = "mplus")
summary(configural_fit1, fit.measures = TRUE, standardized=TRUE)
#get factor scores
factor.scores(cbclDF1, configural_fit1)

#create a model and CFA for baseline and two year CONFIGURAL INVARAINCE 
configural2<- '
#Define latent 
   baseFactor1=~
NA*baseAttacksThreatens
+lambda1*baseAttacksThreatens
+basecbcl_q16_p
+baseDisobeysrules
+basecbcl_q37_p
+basecbcl_q95_p
+basecbcl_q03_p
+baseDestroys
+basecbcl_q68_p
+basecbcl_q26_p
+basecbcl_q90_p
+basecbcl_q94_p
+baseSteals
+basecbcl_q86_p
+basecbcl_q43_p
+basecbcl_q15_p
+basecbcl_q67_p
+basecbcl_q87_p
+basecbcl_q27_p
+basePeerProblems
+basecbcl_q89_p
+basecbcl_q19_p
+basecbcl_q96_p
+basecbcl_q39_p
+basecbcl_q34_p
+basecbcl_q72_p
+basecbcl_q88_p
+basecbcl_q74_p
+basecbcl_q07_p
+basecbcl_q109_p
   twoFactor1=~
NA*twoAttacksThreatens
+lambda1*twoAttacksThreatens
+twocbcl_q16_p
+twoDisobeysrules
+twocbcl_q37_p
+twocbcl_q95_p
+twocbcl_q03_p
+twoDestroys
+twocbcl_q68_p
+twocbcl_q26_p
+twocbcl_q90_p
+twocbcl_q94_p
+twoSteals
+twocbcl_q86_p
+twocbcl_q43_p
+twocbcl_q15_p
+twocbcl_q67_p
+twocbcl_q87_p
+twocbcl_q27_p
+twoPeerProblems
+twocbcl_q89_p
+twocbcl_q19_p
+twocbcl_q96_p
+twocbcl_q39_p
+twocbcl_q34_p
+twocbcl_q72_p
+twocbcl_q88_p
+twocbcl_q74_p
+twocbcl_q07_p
+twocbcl_q109_p
   baseFactor2=~
NA*basecbcl_q50_p
+lambda1*basecbcl_q50_p
+basecbcl_q112_p
+basecbcl_q32_p
+basecbcl_q52_p
+basecbcl_q45_p
+basecbcl_q31_p
+basecbcl_q35_p
+basecbcl_q71_p
+basecbcl_q30_p
+basecbcl_q29_p
+basecbcl_q12_p
   twoFactor2=~
NA*twocbcl_q50_p
+lambda1*twocbcl_q50_p
+twocbcl_q112_p
+twocbcl_q32_p
+twocbcl_q52_p
+twocbcl_q45_p
+twocbcl_q31_p
+twocbcl_q35_p
+twocbcl_q71_p
+twocbcl_q30_p
+twocbcl_q29_p
+twocbcl_q12_p

baseFactor3 =~ NA*baseDistractedHyperactive
+lambda1*baseDistractedHyperactive
+basecbcl_q17_p
+basecbcl_q80_p
+basecbcl_q13_p
+basecbcl_q62_p
+basecbcl_q46_p
+basecbcl_q04_p
+basecbcl_q93_p
+basecbcl_q09_p
+basecbcl_q61_p
+basecbcl_q66_p
+basecbcl_q85_p
+basecbcl_q01_p
+basecbcl_q36_p
+basecbcl_q64_p

   twoFactor3=~
NA*twoDistractedHyperactive
+lambda1*twoDistractedHyperactive
+twocbcl_q17_p
+twocbcl_q80_p
+twocbcl_q13_p
+twocbcl_q62_p
+twocbcl_q46_p
+twocbcl_q04_p
+twocbcl_q93_p
+twocbcl_q09_p
+twocbcl_q61_p
+twocbcl_q66_p
+twocbcl_q85_p
+twocbcl_q01_p
+twocbcl_q36_p
+twocbcl_q64_p

   baseFactor4=~
NA*basecbcl_q56c_p
+lambda1*basecbcl_q56c_p
+basecbcl_q56f_p
+basecbcl_q56g_p
+basecbcl_q56b_p
+basecbcl_q56a_p
+basecbcl_q51_p
+basecbcl_q56h_p
+basecbcl_q56d_p
+basecbcl_q56e_p

   twoFactor4=~
NA*twocbcl_q56c_p
+lambda1*twocbcl_q56c_p
+twocbcl_q56f_p
+twocbcl_q56g_p
+twocbcl_q56b_p
+twocbcl_q56a_p
+twocbcl_q51_p
+twocbcl_q56h_p
+twocbcl_q56d_p
+twocbcl_q56e_p

   baseFactor5=~
NA*basecbcl_q111_p
lambda1*basecbcl_q111_p
+basecbcl_q42_p
+basecbcl_q75_p
+basecbcl_q65_p
+basecbcl_q102_p

   twoFactor5=~
NA*twocbcl_q111_p
+twocbcl_q111_p
+twocbcl_q42_p
+twocbcl_q75_p
+twocbcl_q65_p
+twocbcl_q102_p

#Intercepts
basecbcl_q01_p ~ i1*1
basecbcl_q03_p ~ 1
basecbcl_q04_p ~ 1
basecbcl_q05_p ~ 1
basecbcl_q06_p ~ 1
basecbcl_q07_p ~ 1
basecbcl_q09_p ~ 1
basecbcl_q11_p ~ 1
basecbcl_q12_p ~ 1
basecbcl_q13_p ~ 1
basecbcl_q14_p ~ 1
basecbcl_q15_p ~ 1
basecbcl_q16_p ~ 1
basecbcl_q17_p ~ 1
basecbcl_q18_p ~ 1
basecbcl_q19_p ~ 1
basecbcl_q24_p ~ 1
basecbcl_q26_p ~ 1
basecbcl_q27_p ~ 1
basecbcl_q29_p ~ 1
basecbcl_q30_p ~ 1
basecbcl_q31_p ~ 1
basecbcl_q32_p ~ 1
basecbcl_q33_p ~ 1
basecbcl_q34_p ~ 1
basecbcl_q35_p ~ 1
basecbcl_q36_p ~ 1
basecbcl_q37_p ~ 1
basecbcl_q38_p ~ 1
basecbcl_q39_p ~ 1
basecbcl_q41_p ~ 1
basecbcl_q42_p ~ 1
basecbcl_q43_p ~ 1
basecbcl_q44_p ~ 1
basecbcl_q45_p ~ 1
basecbcl_q46_p ~ 1
basecbcl_q47_p ~ 1
basecbcl_q49_p ~ 1
basecbcl_q50_p ~ 1
basecbcl_q51_p ~ 1
basecbcl_q52_p ~ 1
basecbcl_q54_p ~ 1
basecbcl_q56a_p ~ 1
basecbcl_q56b_p ~ 1
basecbcl_q56c_p ~ 1
basecbcl_q56d_p ~ 1
basecbcl_q56e_p ~ 1
basecbcl_q56f_p ~ 1
basecbcl_q56g_p ~ 1
basecbcl_q56h_p ~ 1
basecbcl_q58_p ~ 1
basecbcl_q61_p ~ 1
basecbcl_q62_p ~ 1
basecbcl_q63_p ~ 1
basecbcl_q64_p ~ 1
basecbcl_q65_p ~ 1
basecbcl_q66_p ~ 1
basecbcl_q67_p ~ 1
basecbcl_q68_p ~ 1
basecbcl_q69_p ~ 1
basecbcl_q71_p ~ 1
basecbcl_q72_p ~ 1
basecbcl_q74_p ~ 1
basecbcl_q75_p ~ 1
basecbcl_q76_p ~ 1
basecbcl_q77_p ~ 1
basecbcl_q79_p ~ 1
basecbcl_q80_p ~ 1
basecbcl_q83_p ~ 1
basecbcl_q84_p ~ 1
basecbcl_q85_p ~ 1
basecbcl_q86_p ~ 1
basecbcl_q87_p ~ 1
basecbcl_q88_p ~ 1
basecbcl_q89_p ~ 1
basecbcl_q90_p ~ 1
basecbcl_q91_p ~ 1
basecbcl_q92_p ~ 1
basecbcl_q93_p ~ 1
basecbcl_q94_p ~ 1
basecbcl_q95_p ~ 1
basecbcl_q96_p ~ 1
basecbcl_q98_p ~ 1
basecbcl_q100_p ~ 1
basecbcl_q102_p ~ 1
basecbcl_q103_p ~ 1
basecbcl_q104_p ~ 1
basecbcl_q107_p ~ 1
basecbcl_q108_p ~ 1
basecbcl_q109_p ~ 1
basecbcl_q110_p ~ 1
basecbcl_q111_p ~ 1
basecbcl_q112_p ~ 1
baseAttacksThreatens ~ 1
baseDestroys ~ 1
baseDisobeysrules ~ 1
baseSteals ~ 1
basePeerProblems ~ 1
baseDistractedHyperactive ~ 1
baseHallucinations ~ 1
baseSexPlay ~ 1
baseWeightproblems ~ 1 
twocbcl_q01_p ~ i1*1
twocbcl_q03_p ~ 1
twocbcl_q04_p ~ 1
twocbcl_q05_p ~ 1
twocbcl_q06_p ~ 1
twocbcl_q07_p ~ 1
twocbcl_q09_p ~ 1
twocbcl_q11_p ~ 1
twocbcl_q12_p ~ 1
twocbcl_q13_p ~ 1
twocbcl_q14_p ~ 1
twocbcl_q15_p ~ 1
twocbcl_q16_p ~ 1
twocbcl_q17_p ~ 1
twocbcl_q18_p ~ 1
twocbcl_q19_p ~ 1
twocbcl_q24_p ~ 1
twocbcl_q26_p ~ 1
twocbcl_q27_p ~ 1
twocbcl_q29_p ~ 1
twocbcl_q30_p ~ 1
twocbcl_q31_p ~ 1
twocbcl_q32_p ~ 1
twocbcl_q33_p ~ 1
twocbcl_q34_p ~ 1
twocbcl_q35_p ~ 1
twocbcl_q36_p ~ 1
twocbcl_q37_p ~ 1
twocbcl_q38_p ~ 1
twocbcl_q39_p ~ 1
twocbcl_q41_p ~ 1
twocbcl_q42_p ~ 1
twocbcl_q43_p ~ 1
twocbcl_q44_p ~ 1
twocbcl_q45_p ~ 1
twocbcl_q46_p ~ 1
twocbcl_q47_p ~ 1
twocbcl_q49_p ~ 1
twocbcl_q50_p ~ 1
twocbcl_q51_p ~ 1
twocbcl_q52_p ~ 1
twocbcl_q54_p ~ 1
twocbcl_q56a_p ~ 1
twocbcl_q56b_p ~ 1
twocbcl_q56c_p ~ 1
twocbcl_q56d_p ~ 1
twocbcl_q56e_p ~ 1
twocbcl_q56f_p ~ 1
twocbcl_q56g_p ~ 1
twocbcl_q56h_p ~ 1
twocbcl_q58_p ~ 1
twocbcl_q61_p ~ 1
twocbcl_q62_p ~ 1
twocbcl_q63_p ~ 1
twocbcl_q64_p ~ 1
twocbcl_q65_p ~ 1
twocbcl_q66_p ~ 1
twocbcl_q67_p ~ 1
twocbcl_q68_p ~ 1
twocbcl_q69_p ~ 1
twocbcl_q71_p ~ 1
twocbcl_q72_p ~ 1
twocbcl_q74_p ~ 1
twocbcl_q75_p ~ 1
twocbcl_q76_p ~ 1
twocbcl_q77_p ~ 1
twocbcl_q79_p ~ 1
twocbcl_q80_p ~ 1
twocbcl_q83_p ~ 1
twocbcl_q84_p ~ 1
twocbcl_q85_p ~ 1
twocbcl_q86_p ~ 1
twocbcl_q87_p ~ 1
twocbcl_q88_p ~ 1
twocbcl_q89_p ~ 1
twocbcl_q90_p ~ 1
twocbcl_q91_p ~ 1
twocbcl_q92_p ~ 1
twocbcl_q93_p ~ 1
twocbcl_q94_p ~ 1
twocbcl_q95_p ~ 1
twocbcl_q96_p ~ 1
twocbcl_q98_p ~ 1
twocbcl_q100_p ~ 1
twocbcl_q102_p ~ 1
twocbcl_q103_p ~ 1
twocbcl_q104_p ~ 1
twocbcl_q107_p ~ 1
twocbcl_q108_p ~ 1
twocbcl_q109_p ~ 1
twocbcl_q110_p ~ 1
twocbcl_q111_p ~ 1
twocbcl_q112_p ~ 1
twoAttacksThreatens ~ 1
twoDestroys ~ 1
twoDisobeysrules ~ 1
twoSteals ~ 1
twoPeerProblems ~ 1
twoDistractedHyperactive ~ 1
twoHallucinations ~ 1
twoSexPlay ~ 1
twoWeightproblems ~ 1 



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

#Latent Variable Means
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

configural_fit2 <- cfa(configural2, data = cbclDF2, mimic = "mplus")
summary(configural_fit2, fit.measures = TRUE, standardized=TRUE)
#get factor scores
factor.scores(cbclDF2, configural_fit2)

#Model diagram for baseline-one year
semPaths(configural_fit1, what="est", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#Model diagram for baseline-two year
semPaths(configural_fit2, what="est", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#####STEP 6: FACTOR LOADINGS 
inspect(configural_fit1 ,what="std")$lambda
inspect(configural_fit2 ,what="std")$lambda

######STEP 7: FACTOR SCORES
configural1_scores <- lavPredict(configural_fit1)
configural2_scores <- lavPredict(configural_fit2)

