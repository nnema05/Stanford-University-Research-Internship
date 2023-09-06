
## LONGITUDNAL CFA
### SCALAR MODEL FOR LONGITUDNAL CFA 
# This program creates the SCALAR model for Longitudinal CFA 
# 1. Create 2 datasets, one that has baseline and one year data, and one that has baseline and two year data
# 2. Create 2 SCALAR models. Each model has for all 5 factors 
        # one model has factor information for baseline and one year
        # the other model factor information for baseline and two year
# 3. Using cfa(), fit a Confirmatory Factor Analysis (CFA) for both models
# 4. Get factor scores from each SCALAR model! 

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

######STEP 4: LONGITUDNAL CFA SCALAR INVARIANCE 
#libraries downloaded above

#create a model and CFA for baseline and one year
print("SCALAR MODEL 1")
scalar1<- '
#Intercepts cnnstrained 
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
'
print("SCALAR MODEL 1 DONE")

scalarModel1 <- paste(metric1, errorstructure1, scalar1, sep = ' ', collapse = NULL)

scalar_fit1 <- cfa(scalarModel1,
                  data = cbclDF1)
print("SCALAR CFA")

summary(scalar_fit1, fit.measures = TRUE,
        standardized = TRUE,
        ci = TRUE,
        rsquare = TRUE)

#factor.scores(cbclDF1, scalar_fit1)

#create a model and CFA for baseline and two year  
print("SCALAR MODEL 2")
scalar2 <- '
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
'
print("SCALAR MODEL 2 DONE")
scalarModel2 <- paste(metric2, errorstructure2, scalar2, sep = ' ', collapse = NULL)

scalar_fit2 <- cfa(scalarModel2,
                   data = cbclDF2)
print("SCALAR 2 CFA")

summary(scalar_fit2, fit.measures = TRUE,
        standardized = TRUE,
        ci = TRUE,
        rsquare = TRUE)

#factor.scores(cbclDF2, scalar_fit2)


#Model diagram for baseline-one year
#semPaths(scalar_fit1, what="est", sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#Model diagram for baseline-two year
#semPaths(scalar_fit2, what="est", sizeLat = 7, sizeMan = 7, edge.label.cex = .75)


#####STEP 6: FACTOR LOADINGS 
inspect(scalar_fit1 ,what="std")$lambda
inspect(scalar_fit2 ,what="std")$lambda

######STEP 7: FACTOR SCORES
scalar1_scores <- lavPredict(scalar_fit1)
scalar2_scores <- lavPredict(scalar_fit2)

