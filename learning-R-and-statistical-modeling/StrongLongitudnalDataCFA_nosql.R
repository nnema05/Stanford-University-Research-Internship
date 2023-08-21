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
#install.packages(sqldf)
#library(sqldf)
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

######STEP 4: LONGITUDNAL CFA STRONG INVARIANCE 
#libraries downloaded above

#create a model and CFA for baseline and one year
strong1<- '
#Define latent 
   baseFactor1=~
NA*`baseAttacksThreatens`
+lambda1*baseAttacksThreatens
+lambda2*basecbcl_q16_p
+lambda34*baseDisobeysrules
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
+lambda1*basecbcl_q50_p
+lambda2*basecbcl_q112_p
+lambda3*basecbcl_q32_p
+lambda4*basecbcl_q52_p
+lambda5*basecbcl_q45_p
+lambda6*basecbcl_q31_p
+lambda7*basecbcl_q35_p
+lambda8*basecbcl_q71_p
+lambda9*basecbcl_q30_p
+lambda10*basecbcl_q29_p
+lambda11*basecbcl_q12_p
   oneFactor2=~
NA*onecbcl_q50_p
+lambda1*onecbcl_q50_p
+lambda2*onecbcl_q112_p
+lambda3*onecbcl_q32_p
+lambda4*onecbcl_q52_p
+lambda5*onecbcl_q45_p
+lambda6*onecbcl_q31_p
+lambda7*onecbcl_q35_p
+lambda8*onecbcl_q71_p
+lambda9*onecbcl_q30_p
+lambda10*onecbcl_q29_p
+lambda11*onecbcl_q12_p
   baseFactor3=~
NA*baseDistractedHyperactive
+lambda1*baseDistractedHyperactive
+lambda2*basecbcl_q17_p
+lambda3*basecbcl_q80_p
+lambda4*basecbcl_q13_p
+lambda5*basecbcl_q62_p
+lambda6*basecbcl_q46_p
+lambda7*basecbcl_q04_p
+lambda8*basecbcl_q93_p
+lambda10*basecbcl_q09_p
+lambda11*basecbcl_q61_p
+lambda12*basecbcl_q66_p
+lambda13*basecbcl_q85_p
+lambda14*basecbcl_q01_p
+lambda15*basecbcl_q36_p
+lambda16*basecbcl_q64_p
   oneFactor3=~
NA*oneDistractedHyperactive
+lambda1*oneDistractedHyperactive
+lambda2*onecbcl_q17_p
+lambda3*onecbcl_q80_p
+lambda4*onecbcl_q13_p
+lambda5*onecbcl_q62_p
+lambda6*onecbcl_q46_p
+lambda7*onecbcl_q04_p
+lambda8*onecbcl_q93_p
+lambda10*onecbcl_q09_p
+lambda11*onecbcl_q61_p
+lambda12*onecbcl_q66_p
+lambda13*onecbcl_q85_p
+lambda14*onecbcl_q01_p
+lambda15*onecbcl_q36_p
+lambda16*onecbcl_q64_p
   baseFactor4=~
NA*basecbcl_q56c_p
+lambda1*basecbcl_q56c_p
+lambda2*basecbcl_q56f_p
+lambda3*basecbcl_q56g_p
+lambda4*basecbcl_q56b_p
+lambda5*basecbcl_q56a_p
+lambda6*basecbcl_q51_p
+lambda7*basecbcl_q56h_p
+lambda8*basecbcl_q56d_p
+lambda9*basecbcl_q56e_p
   oneFactor4=~
NA*onecbcl_q56c_p
+lambda1*onecbcl_q56c_p
+lambda2*onecbcl_q56f_p
+lambda3*onecbcl_q56g_p
+lambda4*onecbcl_q56b_p
+lambda5*onecbcl_q56a_p
+lambda6*onecbcl_q51_p
+lambda7*onecbcl_q56h_p
+lambda8*onecbcl_q56d_p
+lambda9*onecbcl_q56e_p
   baseFactor5=~
NA*basecbcl_q111_p
lambda1*basecbcl_q111_p
+lambda2*basecbcl_q42_p
+lambda3*basecbcl_q75_p
+lambda4*basecbcl_q65_p
+lambda5*basecbcl_q102_p
   oneFactor5=~
NA*onecbcl_q111_p
+lambda1*onecbcl_q111_p
+lambda2*onecbcl_q42_p
+lambda3*onecbcl_q75_p
+lambda4*onecbcl_q65_p
+lambda5*onecbcl_q102_p

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
#get factor scores
factor.scores(cbclDF1, strong_fit1)

#create a model and CFA for baseline and two year STRONG INVARAINCE 
strong2<- '
#Define latent 
   baseFactor1=~
NA*baseAttacksThreatens
+lambda1*baseAttacksThreatens
+lambda2*basecbcl_q16_p
+lambda34*baseDisobeysrules
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
+lambda1*basecbcl_q50_p
+lambda2*basecbcl_q112_p
+lambda3*basecbcl_q32_p
+lambda4*basecbcl_q52_p
+lambda5*basecbcl_q45_p
+lambda6*basecbcl_q31_p
+lambda7*basecbcl_q35_p
+lambda8*basecbcl_q71_p
+lambda9*basecbcl_q30_p
+lambda10*basecbcl_q29_p
+lambda11*basecbcl_q12_p
   twoFactor2=~
NA*twocbcl_q50_p
+lambda1*twocbcl_q50_p
+lambda2*twocbcl_q112_p
+lambda3*twocbcl_q32_p
+lambda4*twocbcl_q52_p
+lambda5*twocbcl_q45_p
+lambda6*twocbcl_q31_p
+lambda7*twocbcl_q35_p
+lambda8*twocbcl_q71_p
+lambda9*twocbcl_q30_p
+lambda10*twocbcl_q29_p
+lambda11*twocbcl_q12_p
   baseFactor3=~
NA*baseDistractedHyperactive
+lambda1*baseDistractedHyperactive
+lambda2*basecbcl_q17_p
+lambda3*basecbcl_q80_p
+lambda4*basecbcl_q13_p
+lambda5*basecbcl_q62_p
+lambda6*basecbcl_q46_p
+lambda7*basecbcl_q04_p
+lambda8*basecbcl_q93_p
+lambda10*basecbcl_q09_p
+lambda11*basecbcl_q61_p
+lambda12*basecbcl_q66_p
+lambda13*basecbcl_q85_p
+lambda14*basecbcl_q01_p
+lambda15*basecbcl_q36_p
+lambda16*basecbcl_q64_p
   twoFactor3=~
NA*twoDistractedHyperactive
+lambda1*twoDistractedHyperactive
+lambda2*twocbcl_q17_p
+lambda3*twocbcl_q80_p
+lambda4*twocbcl_q13_p
+lambda5*twocbcl_q62_p
+lambda6*twocbcl_q46_p
+lambda7*twocbcl_q04_p
+lambda8*twocbcl_q93_p
+lambda10*twocbcl_q09_p
+lambda11*twocbcl_q61_p
+lambda12*twocbcl_q66_p
+lambda13*twocbcl_q85_p
+lambda14*twocbcl_q01_p
+lambda15*twocbcl_q36_p
+lambda16*twocbcl_q64_p
   baseFactor4=~
NA*basecbcl_q56c_p
+lambda1*basecbcl_q56c_p
+lambda2*basecbcl_q56f_p
+lambda3*basecbcl_q56g_p
+lambda4*basecbcl_q56b_p
+lambda5*basecbcl_q56a_p
+lambda6*basecbcl_q51_p
+lambda7*basecbcl_q56h_p
+lambda8*basecbcl_q56d_p
+lambda9*basecbcl_q56e_p
   twoFactor4=~
NA*twocbcl_q56c_p
+lambda1*twocbcl_q56c_p
+lambda2*twocbcl_q56f_p
+lambda3*twocbcl_q56g_p
+lambda4*twocbcl_q56b_p
+lambda5*twocbcl_q56a_p
+lambda6*twocbcl_q51_p
+lambda7*twocbcl_q56h_p
+lambda8*twocbcl_q56d_p
+lambda9*twocbcl_q56e_p
   baseFactor5=~
NA*basecbcl_q111_p
lambda1*basecbcl_q111_p
+lambda2*basecbcl_q42_p
+lambda3*basecbcl_q75_p
+lambda4*basecbcl_q65_p
+lambda5*basecbcl_q102_p
   twoFactor5=~
NA*twocbcl_q111_p
+lambda1*twocbcl_q111_p
+lambda2*twocbcl_q42_p
+lambda3*twocbcl_q75_p
+lambda4*twocbcl_q65_p
+lambda5*twocbcl_q102_p

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
#get factor scores
factor.scores(cbclDF2, strong_fit2)

#Model diagram for baseline-one year
semPaths(strong_fit1, what="est", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#Model diagram for baseline-two year
semPaths(strong_fit2, what="est", 
         sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#####STEP 6: FACTOR LOADINGS 
inspect(strong_fit1 ,what="std")$lambda
inspect(strong_fit2 ,what="std")$lambda

######STEP 7: FACTOR SCORES
strong1_scores <- lavPredict(strong_fit1)
strong2_scores <- lavPredict(strong_fit2)

