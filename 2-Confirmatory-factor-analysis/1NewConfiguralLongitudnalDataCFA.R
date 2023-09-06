
## LONGITUDNAL CFA
###CONFIGURAL MODEL FOR LONGITUDNAL CFA 
# This program creates the configural model for Longitudinal CFA 
# 1. Create 2 datasets, one that has baseline and one year data, and one that has baseline and two year data
# 2. Create 2 configural models. Each model has for all 5 factors 
   # one model has factor information for baseline and one year
   # the other model factor information for baseline and two year
# 3. Using cfa(), fit a Confirmatory Factor Analysis (CFA) for both models
# 4. Get factor scores from each configural model


####STEP 0: INSTALL LIBRARIES 
library(sqldf)
library(psych)
library(lavaan)



######STEP 2: PREP for longitudinal CFA by creating 2 new data frame where... 
#...each subject key has a baseline, one year and a baseline, two year score for each question


#Two CFA will be done (one for baseline vs one year and one for baseline vs twoyear)

#New dataframe - baseline and one year 
cbclDF1 <- inner_join(baselineData, oneyearData, 
                      by = c("basesubjectkey" = "onesubjectkey")
) 

head(cbclDF1)     

#New Data frame - baseline vs twoyear
cbclDF2 <- inner_join(baselineData, twoyearData, 
                      by = c("basesubjectkey" = "twosubjectkey")
) 



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


#####STEP 3: ERROR 

######STEP 4: LONGITUDNAL CFA CONFIGURAL INVARIANCE 
#libraries downloaded above


print("CONFGURAL 1 MODEL")
#create a model and CFA for baseline and one year
configural1<- '
#Define Factor 1  
   baseFactor1=~
NA*baseAttacksThreatens
 + basecbcl_q16_p
 + baseDisobeysrules
 + basecbcl_q37_p
 + basecbcl_q95_p
 + basecbcl_q03_p
 + baseDestroys
 + basecbcl_q68_p
 + basecbcl_q26_p
 + basecbcl_q90_p
 + basecbcl_q94_p
 + baseSteals
 + basecbcl_q86_p
 + basecbcl_q43_p
 + basecbcl_q15_p
 + basecbcl_q67_p
 + basecbcl_q87_p
 + basecbcl_q27_p
 + basePeerProblems
 + basecbcl_q89_p
 + basecbcl_q19_p
 + basecbcl_q96_p
 + basecbcl_q39_p
 + basecbcl_q34_p
 + basecbcl_q72_p
 + basecbcl_q88_p
 + basecbcl_q74_p
 + basecbcl_q07_p
 + basecbcl_q109_p
   oneFactor1 =~
 NA*oneAttacksThreatens
 + onecbcl_q16_p
 + oneDisobeysrules
 + onecbcl_q37_p
 + onecbcl_q95_p
 + onecbcl_q03_p
 + oneDestroys
 + onecbcl_q68_p
 + onecbcl_q26_p
 + onecbcl_q90_p
 + onecbcl_q94_p
 + oneSteals
 + onecbcl_q86_p
 + onecbcl_q43_p
 + onecbcl_q15_p
 + onecbcl_q67_p
 + onecbcl_q87_p
 + onecbcl_q27_p
 + onePeerProblems
 + onecbcl_q89_p
 + onecbcl_q19_p
 + onecbcl_q96_p
 + onecbcl_q39_p
 + onecbcl_q34_p
 + onecbcl_q72_p
 + onecbcl_q88_p
 + onecbcl_q74_p
 + onecbcl_q07_p
 + onecbcl_q109_p

#Factor 1 Variance constrained to 1 
baseFactor1 ~~ 1*baseFactor1
oneFactor1 ~~ 1*oneFactor1

#Define Factor 2
   baseFactor2 =~
 NA*basecbcl_q50_p
 + basecbcl_q112_p
 + basecbcl_q32_p
 + basecbcl_q52_p
 + basecbcl_q45_p
 + basecbcl_q31_p
 + basecbcl_q35_p
 + basecbcl_q71_p
 + basecbcl_q30_p
 + basecbcl_q29_p
 + basecbcl_q12_p

   oneFactor2 =~
 NA*onecbcl_q50_p
 + onecbcl_q112_p
 + onecbcl_q32_p
 + onecbcl_q52_p
 + onecbcl_q45_p
 + onecbcl_q31_p
 + onecbcl_q35_p
 + onecbcl_q71_p
 + onecbcl_q30_p
 + onecbcl_q29_p
 + onecbcl_q12_p

#Factor 2 Variance constrained to 1 
baseFactor2 ~~ 1*baseFactor2
oneFactor2 ~~ 1*oneFactor2

#Define Factor 3
   baseFactor3=~
 NA*baseDistractedHyperactive
 + basecbcl_q17_p
 + basecbcl_q80_p
 + basecbcl_q13_p
 + basecbcl_q62_p
 + basecbcl_q46_p
 + basecbcl_q04_p
 + basecbcl_q93_p
 + basecbcl_q09_p
 + basecbcl_q61_p
 + basecbcl_q66_p
 + basecbcl_q85_p
 + basecbcl_q01_p
 + basecbcl_q36_p
 + basecbcl_q64_p
   oneFactor3 =~
 NA*oneDistractedHyperactive
 + onecbcl_q17_p
 + onecbcl_q80_p
 + onecbcl_q13_p
 + onecbcl_q62_p
 + onecbcl_q46_p
 + onecbcl_q04_p
 + onecbcl_q93_p
 + onecbcl_q09_p
 + onecbcl_q61_p
 + onecbcl_q66_p
 + onecbcl_q85_p
 + onecbcl_q01_p
 + onecbcl_q36_p
 + onecbcl_q64_p

#Factor 3 Variance constrained to 1 
baseFactor3 ~~ 1*baseFactor3
oneFactor3 ~~ 1*oneFactor3


#Define Factor 4
   baseFactor4 =~
 NA*basecbcl_q56c_p
 + basecbcl_q56f_p
 + basecbcl_q56g_p
 + basecbcl_q56b_p
 + basecbcl_q56a_p
 + basecbcl_q51_p
 + basecbcl_q56h_p
 + basecbcl_q56d_p
 + basecbcl_q56e_p
   oneFactor4=~
NA*onecbcl_q56c_p
 + onecbcl_q56f_p
 + onecbcl_q56g_p
 + onecbcl_q56b_p
 + onecbcl_q56a_p
 + onecbcl_q51_p
 + onecbcl_q56h_p
 + onecbcl_q56d_p
 + onecbcl_q56e_p

#Factor 4 Variance constrained to 1 
baseFactor4 ~~ 1*baseFactor4
oneFactor4 ~~ 1*oneFactor4

#Define Factor 5
   baseFactor5 =~
 NA*basecbcl_q111_p
 + basecbcl_q42_p
 + basecbcl_q75_p
 + basecbcl_q65_p
 + basecbcl_q102_p
   oneFactor5 =~
NA*onecbcl_q111_p
 + onecbcl_q42_p
 + onecbcl_q75_p
 + onecbcl_q65_p
 + onecbcl_q102_p

#Factor 5 Variance constrained to 1 
baseFactor5 ~~ 1*baseFactor5
oneFactor5 ~~ 1*oneFactor5

'
print("CONFGURAL 1 MODEL DONE")

configuralModel1 <- paste(configural1, errorstructure1, sep = ' ', collapse = NULL)

## Fit a Confirmatory Factor Analysis (CFA) model.
configural_fit1 <- cfa(configuralModel1, 
                       data = cbclDF1)
print("CFA MODEL 1")

summary(configural_fit1, fit.measures = TRUE,
        standardized = TRUE,
        ci = TRUE,
        rsquare = TRUE)
#factor.scores(cbclDF1, configural_fit1)

#create a model and CFA for baseline and two year CONFIGURAL INVARAINCE 
print("CONFGURAL 2 MODEL")
configural2<- '
#Define Factor 1  
   baseFactor1=~
NA*baseAttacksThreatens
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

#Factor 1 Variance constrained to 1 
baseFactor1 ~~ 1*baseFactor1
twoFactor1 ~~ 1*twoFactor1

#Define Factor 2
   baseFactor2=~
NA*basecbcl_q50_p
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

#Factor 2 Variance constrained to 1 
baseFactor2 ~~ 1*baseFactor2
twoFactor2 ~~ 1*twoFactor2

#Define Factor 3
   baseFactor3=~
NA*baseDistractedHyperactive
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

#Factor 3 Variance constrained to 1 
baseFactor3 ~~ 1*baseFactor3
twoFactor3 ~~ 1*twoFactor3


#Define Factor 4
   baseFactor4=~
NA*basecbcl_q56c_p
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
+twocbcl_q56f_p
+twocbcl_q56g_p
+twocbcl_q56b_p
+twocbcl_q56a_p
+twocbcl_q51_p
+twocbcl_q56h_p
+twocbcl_q56d_p
+twocbcl_q56e_p

#Factor 4 Variance constrained to 1 
baseFactor4 ~~ 1*baseFactor4
twoFactor4 ~~ 1*twoFactor4

#Define Factor 5
   baseFactor5=~
NA*basecbcl_q111_p
+basecbcl_q42_p
+basecbcl_q75_p
+basecbcl_q65_p
+basecbcl_q102_p
   twoFactor5=~
NA*twocbcl_q111_p
+twocbcl_q42_p
+twocbcl_q75_p
+twocbcl_q65_p
+twocbcl_q102_p

#Factor 5 Variance constrained to 1 
baseFactor5 ~~ 1*baseFactor5
twoFactor5 ~~ 1*twoFactor5
'
print("CONFGURAL 2 MODEL DONE")

configuralModel2 <- paste(configural2, errorstructure2, sep = ' ', collapse = NULL)

## Fit a Confirmatory Factor Analysis (CFA) model. 
configural_fit2 <- cfa(configuralModel2, 
                       data = cbclDF2)
print("CFA MODEL 2")

summary(configural_fit2, fit.measures = TRUE,
        standardized = TRUE,
        ci = TRUE,
        rsquare = TRUE)

#factor.scores(cbclDF2, configural_fit2)

#Model diagram for baseline-one year
#semPaths(configural_fit1, what="est", sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#Model diagram for baseline-two year
#semPaths(configural_fit2, what="est", sizeLat = 7, sizeMan = 7, edge.label.cex = .75)


#####STEP 6: FACTOR LOADINGS 
inspect(configural_fit1 ,what="std")$lambda
inspect(configural_fit2 ,what="std")$lambda

######STEP 7: FACTOR SCORES
configural1_scores <- lavPredict(configural_fit1)
configural2_scores <- lavPredict(configural_fit2)

