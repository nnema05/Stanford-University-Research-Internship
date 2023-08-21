
## LONGITUDNAL CFA
###CONFIGURAL MODEL FOR LONGITUDNAL CFA --> Specifcially for P-Factor
# This program creates the configural model for Longitudinal CFA 
# 1. Create 2 datasets, one that has baseline and one year data, and one that has baseline and two year data
# 2. Create 2 configural models. Each model has information for P-Factor specifcally!
    # one model has factor information for baseline and one year
    # the other model factor information for baseline and two year
# 3. Using cfa(), fit a Confirmatory Factor Analysis (CFA) for both models
# 4. Get factor scores from each configural model

####STEP 0: INSTALL LIBRARIES 
library(sqldf)
library(psych)
library(lavaan)



######STEP 2: PREP for longitudinal CFA by creating a new data frame where... 
#...each subject key has a baseline, one year and two year score for each question


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

######STEP 4: P-FACTOR LONGITUDNAL CFA CONFIGURAL INVARIANCE 
#libraries downloaded above

#create a model and CFA for baseline and one year
pFactorConfigural1<- '
#Define P-factor  
   basePFactor=~
NA*basecbcl_q56a_p
 + basecbcl_q01_p 
 + basecbcl_q03_p
 + basecbcl_q06_p
 + basecbcl_q07_p
 + basecbcl_q09_p
 + basecbcl_q11_p
 + basecbcl_q12_p
 + basecbcl_q13_p
 + basecbcl_q14_p
 + basecbcl_q15_p
 + basecbcl_q16_p
 + basecbcl_q17_p
 + basecbcl_q18_p
 + basecbcl_q19_p
 + basecbcl_q24_p
 + basecbcl_q26_p
 + basecbcl_q27_p
 + basecbcl_q04_p
 + basecbcl_q29_p
 + basecbcl_q30_p
 + basecbcl_q31_p
 + basecbcl_q51_p
 + basecbcl_q32_p
 + basecbcl_q33_p
 + basecbcl_q34_p
 + basecbcl_q52_p
 + basecbcl_q35_p
 + basecbcl_q36_p
 + basecbcl_q37_p
 + basecbcl_q38_p
 + basecbcl_q39_p
 + basecbcl_q56b_p
 + basecbcl_q41_p
 + basecbcl_q43_p
 + basecbcl_q56c_p
 + basecbcl_q46_p
 + basecbcl_q45_p
 + basecbcl_q47_p
 + basecbcl_q56h_p
 + basecbcl_q54_p
 + basecbcl_q58_p
 + basecbcl_q61_p
 + basecbcl_q62_p
 + basecbcl_q63_p
 + basecbcl_q64_p
 + basecbcl_q56d_p
 + basecbcl_q56e_p
 + basecbcl_q65_p
 + basecbcl_q66_p
 + basecbcl_q67_p
 + basecbcl_q68_p
 + basecbcl_q69_p
 + basecbcl_q71_p
 + basecbcl_q74_p
 + basecbcl_q76_p
 + basecbcl_q77_p
 + basecbcl_q79_p
 + basecbcl_q80_p
 + basecbcl_q56f_p
 + basecbcl_q83_p
 + basecbcl_q84_p
 + basecbcl_q85_p
 + basecbcl_q86_p
 + basecbcl_q87_p
 + basecbcl_q88_p
 + basecbcl_q89_p
 + basecbcl_q90_p
 + basecbcl_q91_p
 + basecbcl_q93_p
 + basecbcl_q94_p
 + basecbcl_q95_p
 + basecbcl_q96_p
 + basecbcl_q50_p
 + basecbcl_q75_p
 + basecbcl_q100_p
 + basecbcl_q102_p
 + basecbcl_q103_p
 + basecbcl_q104_p
 + basecbcl_q56g_p
 + basecbcl_q107_p
 + basecbcl_q109_p
 + basecbcl_q110_p
 + basecbcl_q111_p
 + basecbcl_q112_p
 + basecbcl_q13_p
 + basecbcl_q42_p
 + baseAttacksThreatens
 + baseDestroys
 + baseDisobeysrules
 + baseDistractedHyperactive
 + baseHallucinations
 + basePeerProblems
 + baseSexPlay
 + baseSteals
 + baseWeightproblems
    onePFactor=~
NA*onecbcl_q56a_p
 + onecbcl_q01_p 
 + onecbcl_q03_p
 + onecbcl_q06_p
 + onecbcl_q07_p
 + onecbcl_q09_p
 + onecbcl_q11_p
 + onecbcl_q12_p
 + onecbcl_q13_p
 + onecbcl_q14_p
 + onecbcl_q15_p
 + onecbcl_q16_p
 + onecbcl_q17_p
 + onecbcl_q18_p
 + onecbcl_q19_p
 + onecbcl_q24_p
 + onecbcl_q26_p
 + onecbcl_q27_p
 + onecbcl_q04_p
 + onecbcl_q29_p
 + onecbcl_q30_p
 + onecbcl_q31_p
 + onecbcl_q51_p
 + onecbcl_q32_p
 + onecbcl_q33_p
 + onecbcl_q34_p
 + onecbcl_q52_p
 + onecbcl_q35_p
 + onecbcl_q36_p
 + onecbcl_q37_p
 + onecbcl_q38_p
 + onecbcl_q39_p
 + onecbcl_q56b_p
 + onecbcl_q41_p
 + onecbcl_q43_p
 + onecbcl_q56c_p
 + onecbcl_q46_p
 + onecbcl_q45_p
 + onecbcl_q47_p
 + onecbcl_q56h_p
 + onecbcl_q54_p
 + onecbcl_q58_p
 + onecbcl_q61_p
 + onecbcl_q62_p
 + onecbcl_q63_p
 + onecbcl_q64_p
 + onecbcl_q56d_p
 + onecbcl_q56e_p
 + onecbcl_q65_p
 + onecbcl_q66_p
 + onecbcl_q67_p
 + onecbcl_q68_p
 + onecbcl_q69_p
 + onecbcl_q71_p
 + onecbcl_q74_p
 + onecbcl_q76_p
 + onecbcl_q77_p
 + onecbcl_q79_p
 + onecbcl_q80_p
 + onecbcl_q56f_p
 + onecbcl_q83_p
 + onecbcl_q84_p
 + onecbcl_q85_p
 + onecbcl_q86_p
 + onecbcl_q87_p
 + onecbcl_q88_p
 + onecbcl_q89_p
 + onecbcl_q90_p
 + onecbcl_q91_p
 + onecbcl_q93_p
 + onecbcl_q94_p
 + onecbcl_q95_p
 + onecbcl_q96_p
 + onecbcl_q50_p
 + onecbcl_q75_p
 + onecbcl_q100_p
 + onecbcl_q102_p
 + onecbcl_q103_p
 + onecbcl_q104_p
 + onecbcl_q56g_p
 + onecbcl_q107_p
 + onecbcl_q109_p
 + onecbcl_q110_p
 + onecbcl_q111_p
 + onecbcl_q112_p
 + onecbcl_q13_p
 + onecbcl_q42_p
 + oneAttacksThreatens
 + oneDestroys
 + oneDisobeysrules
 + oneDistractedHyperactive
 + oneHallucinations
 + onePeerProblems
 + oneSexPlay
 + oneSteals
 + oneWeightproblems

#P Factor Variance constrained to 1 
basePFactor ~~ 1*basePFactor
onePFactor ~~ 1*onePFactor
'

pFactorConfiguralModel1 <- paste(pFactorConfigural1, errorstructure1, sep = ' ', collapse = NULL)

pFactorConfigural_fit1 <- cfa(pFactorConfiguralModel1, 
                       data = cbclDF1)

summary(pFactorConfigural_fit1, fit.measures = TRUE,
        standardized = TRUE,
        ci = TRUE,
        rsquare = TRUE)
#factor.scores(cbclDF1, configural_fit1)

#create a model and CFA for baseline and two year CONFIGURAL INVARAINCE 
pFactorConfigural2<- '
#Define Factor 1  
   basePFactor=~
NA*basecbcl_q56a_p
 + basecbcl_q01_p 
 + basecbcl_q03_p
 + basecbcl_q06_p
 + basecbcl_q07_p
 + basecbcl_q09_p
 + basecbcl_q11_p
 + basecbcl_q12_p
 + basecbcl_q13_p
 + basecbcl_q14_p
 + basecbcl_q15_p
 + basecbcl_q16_p
 + basecbcl_q17_p
 + basecbcl_q18_p
 + basecbcl_q19_p
 + basecbcl_q24_p
 + basecbcl_q26_p
 + basecbcl_q27_p
 + basecbcl_q04_p
 + basecbcl_q29_p
 + basecbcl_q30_p
 + basecbcl_q31_p
 + basecbcl_q51_p
 + basecbcl_q32_p
 + basecbcl_q33_p
 + basecbcl_q34_p
 + basecbcl_q52_p
 + basecbcl_q35_p
 + basecbcl_q36_p
 + basecbcl_q37_p
 + basecbcl_q38_p
 + basecbcl_q39_p
 + basecbcl_q56b_p
 + basecbcl_q41_p
 + basecbcl_q43_p
 + basecbcl_q56c_p
 + basecbcl_q46_p
 + basecbcl_q45_p
 + basecbcl_q47_p
 + basecbcl_q56h_p
 + basecbcl_q54_p
 + basecbcl_q58_p
 + basecbcl_q61_p
 + basecbcl_q62_p
 + basecbcl_q63_p
 + basecbcl_q64_p
 + basecbcl_q56d_p
 + basecbcl_q56e_p
 + basecbcl_q65_p
 + basecbcl_q66_p
 + basecbcl_q67_p
 + basecbcl_q68_p
 + basecbcl_q69_p
 + basecbcl_q71_p
 + basecbcl_q74_p
 + basecbcl_q76_p
 + basecbcl_q77_p
 + basecbcl_q79_p
 + basecbcl_q80_p
 + basecbcl_q56f_p
 + basecbcl_q83_p
 + basecbcl_q84_p
 + basecbcl_q85_p
 + basecbcl_q86_p
 + basecbcl_q87_p
 + basecbcl_q88_p
 + basecbcl_q89_p
 + basecbcl_q90_p
 + basecbcl_q91_p
 + basecbcl_q93_p
 + basecbcl_q94_p
 + basecbcl_q95_p
 + basecbcl_q96_p
 + basecbcl_q50_p
 + basecbcl_q75_p
 + basecbcl_q100_p
 + basecbcl_q102_p
 + basecbcl_q103_p
 + basecbcl_q104_p
 + basecbcl_q56g_p
 + basecbcl_q107_p
 + basecbcl_q109_p
 + basecbcl_q110_p
 + basecbcl_q111_p
 + basecbcl_q112_p
 + basecbcl_q13_p
 + basecbcl_q42_p
 + baseAttacksThreatens
 + baseDestroys
 + baseDisobeysrules
 + baseDistractedHyperactive
 + baseHallucinations
 + basePeerProblems
 + baseSexPlay
 + baseSteals
 + baseWeightproblems
   twoPFactor=~
NA*twocbcl_q56a_p
 + twocbcl_q01_p 
 + twocbcl_q03_p
 + twocbcl_q06_p
 + twocbcl_q07_p
 + twocbcl_q09_p
 + twocbcl_q11_p
 + twocbcl_q12_p
 + twocbcl_q13_p
 + twocbcl_q14_p
 + twocbcl_q15_p
 + twocbcl_q16_p
 + twocbcl_q17_p
 + twocbcl_q18_p
 + twocbcl_q19_p
 + twocbcl_q24_p
 + twocbcl_q26_p
 + twocbcl_q27_p
 + twocbcl_q04_p
 + twocbcl_q29_p
 + twocbcl_q30_p
 + twocbcl_q31_p
 + twocbcl_q51_p
 + twocbcl_q32_p
 + twocbcl_q33_p
 + twocbcl_q34_p
 + twocbcl_q52_p
 + twocbcl_q35_p
 + twocbcl_q36_p
 + twocbcl_q37_p
 + twocbcl_q38_p
 + twocbcl_q39_p
 + twocbcl_q56b_p
 + twocbcl_q41_p
 + twocbcl_q43_p
 + twocbcl_q56c_p
 + twocbcl_q46_p
 + twocbcl_q45_p
 + twocbcl_q47_p
 + twocbcl_q56h_p
 + twocbcl_q54_p
 + twocbcl_q58_p
 + twocbcl_q61_p
 + twocbcl_q62_p
 + twocbcl_q63_p
 + twocbcl_q64_p
 + twocbcl_q56d_p
 + twocbcl_q56e_p
 + twocbcl_q65_p
 + twocbcl_q66_p
 + twocbcl_q67_p
 + twocbcl_q68_p
 + twocbcl_q69_p
 + twocbcl_q71_p
 + twocbcl_q74_p
 + twocbcl_q76_p
 + twocbcl_q77_p
 + twocbcl_q79_p
 + twocbcl_q80_p
 + twocbcl_q56f_p
 + twocbcl_q83_p
 + twocbcl_q84_p
 + twocbcl_q85_p
 + twocbcl_q86_p
 + twocbcl_q87_p
 + twocbcl_q88_p
 + twocbcl_q89_p
 + twocbcl_q90_p
 + twocbcl_q91_p
 + twocbcl_q93_p
 + twocbcl_q94_p
 + twocbcl_q95_p
 + twocbcl_q96_p
 + twocbcl_q50_p
 + twocbcl_q75_p
 + twocbcl_q100_p
 + twocbcl_q102_p
 + twocbcl_q103_p
 + twocbcl_q104_p
 + twocbcl_q56g_p
 + twocbcl_q107_p
 + twocbcl_q109_p
 + twocbcl_q110_p
 + twocbcl_q111_p
 + twocbcl_q112_p
 + twocbcl_q13_p
 + twocbcl_q42_p
 + twoAttacksThreatens
 + twoDestroys
 + twoDisobeysrules
 + twoDistractedHyperactive
 + twoHallucinations
 + twoPeerProblems
 + twoSexPlay
 + twoSteals
 + twoWeightproblems

#P-Factor Variance constrained to 1 
basePFactor ~~ 1*basePFactor
twoPFactor ~~ 1*twoPFactor
'
pFactorConfiguralModel2 <- paste(pFactorConfigural2, errorstructure2, sep = ' ', collapse = NULL)

pFactorConfigural_fit2 <- cfa(pFactorConfiguralModel2, 
                              data = cbclDF2)

summary(pFactorConfigural_fit2, fit.measures = TRUE,
        standardized = TRUE,
        ci = TRUE,
        rsquare = TRUE)

#factor.scores(cbclDF2, configural_fit2)

#Model diagram for baseline-one year
#semPaths(configural_fit1, what="est", sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#Model diagram for baseline-two year
#semPaths(configural_fit2, what="est", sizeLat = 7, sizeMan = 7, edge.label.cex = .75)


#####STEP 6: FACTOR LOADINGS 
inspect(pFactorConfigural_fit1 ,what="std")$lambda
inspect(pFactorConfigural_fit2 ,what="std")$lambda

######STEP 7: FACTOR SCORES
pFactorConfigural1_scores <- lavPredict(pFactorConfigural_fit1)
pFactorConfigural2_scores <- lavPredict(pFactorConfigural_fit2)

