
## LONGITUDNAL CFA
### METRIC MODEL FOR LONGITUDNAL CFA 
# This program creates the METRIC model for Longitudinal CFA 
# 1. Create 2 datasets, one that has baseline and one year data, and one that has baseline and two year data
# 2. Create 2 METRIC models. Each model has for all 5 factors 
    # one model has factor information for baseline and one year
    # the other model factor information for baseline and two year
# 3. Using cfa(), fit a Confirmatory Factor Analysis (CFA) for both models
# 4. Get factor scores from each metric model! 

####STEP 0: INSTALL LIBRARIES 
library(sqldf)
library(psych)
library(lavaan)



######STEP 2: PREP for longitudinal CFA by creating a new data frame where... 
#...each subject key has a baseline, one year and two year score for each question

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
#head(cbclDF1)
#ncol(cbclDF1)
#view(cbclDF1)


######STEP 4: LONGITUDNAL CFA STRONG INVARIANCE 
#libraries downloaded above

#create a model and CFA for baseline and one year
print("METRIC MODEL 1")
metric1<- '
#Define Factor1
   baseFactor1=~
lambda2*basecbcl_q16_p
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
lambda1*oneAttacksThreatens
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

### Factor1 variance constrained to 1
baseFactor1 ~~ 1*baseFactor1
oneFactor1 ~~ 1*oneFactor1

###Factor2
   baseFactor2=~
lambda30*basecbcl_q50_p
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
lambda30*onecbcl_q50_p
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

### Factor2 variance constrained to 1
baseFactor2 ~~ 1*baseFactor2
oneFactor2 ~~ 1*oneFactor2


###Factor3
   baseFactor3=~
lambda41*baseDistractedHyperactive
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
lambda41*oneDistractedHyperactive
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

### Factor3 variance constrained to 1
baseFactor3 ~~ 1*baseFactor3
oneFactor3 ~~ 1*oneFactor3


###Factor4
   baseFactor4=~
lambda56*basecbcl_q56c_p
+lambda57*basecbcl_q56f_p
+lambda58*basecbcl_q56g_p
+lambda59*basecbcl_q56b_p
+lambda60*basecbcl_q56a_p
+lambda61*basecbcl_q51_p
+lambda62*basecbcl_q56h_p
+lambda63*basecbcl_q56d_p
+lambda64*basecbcl_q56e_p
   oneFactor4=~
lambda56*onecbcl_q56c_p
+lambda57*onecbcl_q56f_p
+lambda58*onecbcl_q56g_p
+lambda59*onecbcl_q56b_p
+lambda60*onecbcl_q56a_p
+lambda61*onecbcl_q51_p
+lambda62*onecbcl_q56h_p
+lambda63*onecbcl_q56d_p
+lambda64*onecbcl_q56e_p

### Factor4 variance constrained to 1
baseFactor4 ~~ 1*baseFactor4
oneFactor4 ~~ 1*oneFactor4

###Factor5
   baseFactor5=~
lambda65*basecbcl_q111_p
+lambda66*basecbcl_q42_p
+lambda67*basecbcl_q75_p
+lambda68*basecbcl_q65_p
+lambda69*basecbcl_q102_p
   oneFactor5=~
lambda65*onecbcl_q111_p
+lambda66*onecbcl_q42_p
+lambda67*onecbcl_q75_p
+lambda68*onecbcl_q65_p
+lambda69*onecbcl_q102_p


### Factor5 variance constrained to 1
baseFactor5 ~~ 1*baseFactor5
oneFactor5 ~~ 1*oneFactor5
'


metricModel1 <- paste(metric1, errorstructure1, sep = ' ', collapse = NULL)
print("METRIC MODEL 1 DONE ")


## Model Output

metric_fit1 <- cfa(metricModel1,
					   data = cbclDF1)
print("METRIC 1 CFA DONE")

summary(metric_fit1, fit.measures = TRUE,
		standardized = TRUE,
		ci = TRUE,
		rsquare = TRUE)

#factor.scores(cbclDF1, metric_fit1)


#create a model and CFA for baseline and two year STRONG INVARAINCE 
print("METRIC MODEL 2 ")
metric2<- '
### FACTOR1
   baseFactor1=~
lambda1*baseAttacksThreatens
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
lambda1*twoAttacksThreatens
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

### Factor1 variance constrained to 1
baseFactor1 ~~ 1*baseFactor1
twoFactor1 ~~ 1*twoFactor1

### FACTOR2
   baseFactor2=~
lambda30*basecbcl_q50_p
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
lambda30*twocbcl_q50_p
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

### Factor2 variance constrained to 1
baseFactor2 ~~ 1*baseFactor2
twoFactor2 ~~ 1*twoFactor2


### FACTOR3
   baseFactor3=~
lambda41*baseDistractedHyperactive
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
lambda41*twoDistractedHyperactive
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

### Factor3 variance constrained to 1
baseFactor3 ~~ 1*baseFactor3
twoFactor3 ~~ 1*twoFactor3

### FACTOR4 
   baseFactor4=~
lambda56*basecbcl_q56c_p
+lambda57*basecbcl_q56f_p
+lambda58*basecbcl_q56g_p
+lambda59*basecbcl_q56b_p
+lambda60*basecbcl_q56a_p
+lambda61*basecbcl_q51_p
+lambda62*basecbcl_q56h_p
+lambda63*basecbcl_q56d_p
+lambda64*basecbcl_q56e_p
   twoFactor4=~
lambda56*twocbcl_q56c_p
+lambda57*twocbcl_q56f_p
+lambda58*twocbcl_q56g_p
+lambda59*twocbcl_q56b_p
+lambda60*twocbcl_q56a_p
+lambda61*twocbcl_q51_p
+lambda62*twocbcl_q56h_p
+lambda63*twocbcl_q56d_p
+lambda64*twocbcl_q56e_p

### Factor4 variance constrained to 1
baseFactor4 ~~ 1*baseFactor4
twoFactor4 ~~ 1*twoFactor4
  
### FACTOR5 
   baseFactor5=~
lambda65*basecbcl_q111_p
+lambda66*basecbcl_q42_p
+lambda67*basecbcl_q75_p
+lambda68*basecbcl_q65_p
+lambda69*basecbcl_q102_p
   twoFactor5=~
lambda65*twocbcl_q111_p
+lambda66*twocbcl_q42_p
+lambda67*twocbcl_q75_p
+lambda68*twocbcl_q65_p
+lambda69*twocbcl_q102_p

### Factor5 variance constrained to 1
baseFactor5 ~~ 1*baseFactor5
twoFactor5 ~~ 1*twoFactor5
'
print("METRIC MODEL 2 DONE ")

metricModel2 <- paste(metric2, errorstructure2, sep = ' ', collapse = NULL)


metric_fit2 <- cfa(metricModel2,
                  data = cbclDF2)
print("METRIC 2 CFA ")

summary(metric_fit2, fit.measures = TRUE,
        standardized = TRUE,
        ci = TRUE,
        rsquare = TRUE)
#factor.scores(cbclDF2, metric_fit2)

#Model diagram for baseline-one year
#semPaths(strong_fit1, what="est", sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#Model diagram for baseline-two year
#semPaths(strong_fit2, what="est", sizeLat = 7, sizeMan = 7, edge.label.cex = .75)


#####STEP 6: FACTOR LOADINGS 
inspect(metric_fit1 ,what="std")$lambda
inspect(metric_fit2 ,what="std")$lambda

######STEP 7: FACTOR SCORES
metric1_scores <- lavPredict(metric_fit1)
metric2_scores <- lavPredict(metric_fit2)

