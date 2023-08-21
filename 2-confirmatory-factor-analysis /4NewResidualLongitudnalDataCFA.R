## LONGITUDNAL CFA
### RESIDUAL MODEL FOR LONGITUDNAL CFA 
# This program creates the RESIDUAL model for Longitudinal CFA 
# 1. Create 2 datasets, one that has baseline and one year data, and one that has baseline and two year data
# 2. Create 2 RESIDUAL models. Each model has for all 5 factors 
        # one model has factor information for baseline and one year
        # the other model factor information for baseline and two year
# 3. Using cfa(), fit a Confirmatory Factor Analysis (CFA) for both models
# 4. Get factor scores from each RESIDUAL model! 

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

######STEP 4: LONGITUDNAL CFA Re
#libraries downloaded above

#create a model and CFA for baseline and one year
print("RESIDUAL MODEL 1")
residual1<- '
# Residual error constrained across waves
basecbcl_q01_p ~~ u2*basecbcl_q01_p
basecbcl_q03_p ~~ u3*basecbcl_q03_p
basecbcl_q04_p ~~ u4*basecbcl_q04_p
basecbcl_q05_p ~~ u5*basecbcl_q05_p
basecbcl_q06_p ~~ u6*basecbcl_q06_p
basecbcl_q07_p ~~ u7*basecbcl_q07_p
basecbcl_q09_p ~~ u8*basecbcl_q09_p
basecbcl_q11_p ~~ u9*basecbcl_q11_p
basecbcl_q12_p ~~ u10*basecbcl_q12_p
basecbcl_q13_p ~~ u11*basecbcl_q13_p
basecbcl_q14_p ~~ u12*basecbcl_q14_p
basecbcl_q15_p ~~ u13*basecbcl_q15_p
basecbcl_q16_p ~~ u14*basecbcl_q16_p
basecbcl_q17_p ~~ u15*basecbcl_q17_p
basecbcl_q18_p ~~ u16*basecbcl_q18_p
basecbcl_q19_p ~~ u17*basecbcl_q19_p
basecbcl_q24_p ~~ u18*basecbcl_q24_p
basecbcl_q26_p ~~ u19*basecbcl_q26_p
basecbcl_q27_p ~~ u20*basecbcl_q27_p
basecbcl_q29_p ~~ u21*basecbcl_q29_p
basecbcl_q30_p ~~ u22*basecbcl_q30_p
basecbcl_q31_p ~~ u23*basecbcl_q31_p
basecbcl_q32_p ~~ u24*basecbcl_q32_p
basecbcl_q33_p ~~ u25*basecbcl_q33_p
basecbcl_q34_p ~~ u26*basecbcl_q34_p
basecbcl_q35_p ~~ u27*basecbcl_q35_p
basecbcl_q36_p ~~ u28*basecbcl_q36_p
basecbcl_q37_p ~~ u29*basecbcl_q37_p
basecbcl_q38_p ~~ u30*basecbcl_q38_p
basecbcl_q39_p ~~ u31*basecbcl_q39_p
basecbcl_q41_p ~~ u32*basecbcl_q41_p
basecbcl_q42_p ~~ u33*basecbcl_q42_p
basecbcl_q43_p ~~ u34*basecbcl_q43_p
basecbcl_q44_p ~~ u35*basecbcl_q44_p
basecbcl_q45_p ~~ u36*basecbcl_q45_p
basecbcl_q46_p ~~ u37*basecbcl_q46_p
basecbcl_q47_p ~~ u38*basecbcl_q47_p
basecbcl_q49_p ~~ u39*basecbcl_q49_p
basecbcl_q50_p ~~ u40*basecbcl_q50_p
basecbcl_q51_p ~~ u41*basecbcl_q51_p
basecbcl_q52_p ~~ u42*basecbcl_q52_p
basecbcl_q54_p ~~ u43*basecbcl_q54_p
basecbcl_q56a_p ~~ u44*basecbcl_q56a_p
basecbcl_q56b_p ~~ u45*basecbcl_q56b_p
basecbcl_q56c_p ~~ u46*basecbcl_q56c_p
basecbcl_q56d_p ~~ u47*basecbcl_q56d_p
basecbcl_q56e_p ~~ u48*basecbcl_q56e_p
basecbcl_q56f_p ~~ u49*basecbcl_q56f_p
basecbcl_q56g_p ~~ u50*basecbcl_q56g_p
basecbcl_q56h_p ~~ u51*basecbcl_q56h_p
basecbcl_q58_p ~~ u52*basecbcl_q58_p
basecbcl_q61_p ~~ u53*basecbcl_q61_p
basecbcl_q62_p ~~ u54*basecbcl_q62_p
basecbcl_q63_p ~~ u55*basecbcl_q63_p
basecbcl_q64_p ~~ u56*basecbcl_q64_p
basecbcl_q65_p ~~ u57*basecbcl_q65_p
basecbcl_q66_p ~~ u58*basecbcl_q66_p
basecbcl_q67_p ~~ u59*basecbcl_q67_p
basecbcl_q68_p ~~ u60*basecbcl_q68_p
basecbcl_q69_p ~~ u61*basecbcl_q69_p
basecbcl_q71_p ~~ u62*basecbcl_q71_p
basecbcl_q72_p ~~ u63*basecbcl_q72_p
basecbcl_q74_p ~~ u64*basecbcl_q74_p
basecbcl_q75_p ~~ u65*basecbcl_q75_p
basecbcl_q76_p ~~ u66*basecbcl_q76_p
basecbcl_q77_p ~~ u67*basecbcl_q77_p
basecbcl_q79_p ~~ u68*basecbcl_q79_p
basecbcl_q80_p ~~ u69*basecbcl_q80_p
basecbcl_q83_p ~~ u70*basecbcl_q83_p
basecbcl_q84_p ~~ u71*basecbcl_q84_p
basecbcl_q85_p ~~ u72*basecbcl_q85_p
basecbcl_q86_p ~~ u73*basecbcl_q86_p
basecbcl_q87_p ~~ u74*basecbcl_q87_p
basecbcl_q88_p ~~ u75*basecbcl_q88_p
basecbcl_q89_p ~~ u76*basecbcl_q89_p
basecbcl_q90_p ~~ u77*basecbcl_q90_p
basecbcl_q91_p ~~ u78*basecbcl_q91_p
basecbcl_q92_p ~~ u79*basecbcl_q92_p
basecbcl_q93_p ~~ u80*basecbcl_q93_p
basecbcl_q94_p ~~ u81*basecbcl_q94_p
basecbcl_q95_p ~~ u82*basecbcl_q95_p
basecbcl_q96_p ~~ u83*basecbcl_q96_p
basecbcl_q98_p ~~ u84*basecbcl_q98_p
basecbcl_q100_p ~~ u85*basecbcl_q100_p
basecbcl_q102_p ~~ u86*basecbcl_q102_p
basecbcl_q103_p ~~ u87*basecbcl_q103_p
basecbcl_q104_p ~~ u88*basecbcl_q104_p
basecbcl_q107_p ~~ u89*basecbcl_q107_p
basecbcl_q108_p ~~ u90*basecbcl_q108_p
basecbcl_q109_p ~~ u91*basecbcl_q109_p
basecbcl_q110_p ~~ u92*basecbcl_q110_p
basecbcl_q111_p ~~ u93*basecbcl_q111_p
basecbcl_q112_p ~~ u94*basecbcl_q112_p
baseAttacksThreatens ~~ u95*baseAttacksThreatens
baseDestroys ~~ u96*baseDestroys
baseDisobeysrules ~~ u97*baseDisobeysrules
baseSteals ~~ u98*baseSteals
basePeerProblems ~~ u99*basePeerProblems
baseDistractedHyperactive ~~ u100*baseDistractedHyperactive
baseHallucinations ~~ u101*baseHallucinations
baseSexPlay ~~ u102*baseSexPlay
baseWeightproblems ~~ u103*baseWeightproblems

onecbcl_q01_p ~~ u2*onecbcl_q01_p
onecbcl_q03_p ~~ u3*onecbcl_q03_p
onecbcl_q04_p ~~ u4*onecbcl_q04_p
onecbcl_q05_p ~~ u5*onecbcl_q05_p
onecbcl_q06_p ~~ u6*onecbcl_q06_p
onecbcl_q07_p ~~ u7*onecbcl_q07_p
onecbcl_q09_p ~~ u8*onecbcl_q09_p
onecbcl_q11_p ~~ u9*onecbcl_q11_p
onecbcl_q12_p ~~ u10*onecbcl_q12_p
onecbcl_q13_p ~~ u11*onecbcl_q13_p
onecbcl_q14_p ~~ u12*onecbcl_q14_p
onecbcl_q15_p ~~ u13*onecbcl_q15_p
onecbcl_q16_p ~~ u14*onecbcl_q16_p
onecbcl_q17_p ~~ u15*onecbcl_q17_p
onecbcl_q18_p ~~ u16*onecbcl_q18_p
onecbcl_q19_p ~~ u17*onecbcl_q19_p
onecbcl_q24_p ~~ u18*onecbcl_q24_p
onecbcl_q26_p ~~ u19*onecbcl_q26_p
onecbcl_q27_p ~~ u20*onecbcl_q27_p
onecbcl_q29_p ~~ u21*onecbcl_q29_p
onecbcl_q30_p ~~ u22*onecbcl_q30_p
onecbcl_q31_p ~~ u23*onecbcl_q31_p
onecbcl_q32_p ~~ u24*onecbcl_q32_p
onecbcl_q33_p ~~ u25*onecbcl_q33_p
onecbcl_q34_p ~~ u26*onecbcl_q34_p
onecbcl_q35_p ~~ u27*onecbcl_q35_p
onecbcl_q36_p ~~ u28*onecbcl_q36_p
onecbcl_q37_p ~~ u29*onecbcl_q37_p
onecbcl_q38_p ~~ u30*onecbcl_q38_p
onecbcl_q39_p ~~ u31*onecbcl_q39_p
onecbcl_q41_p ~~ u32*onecbcl_q41_p
onecbcl_q42_p ~~ u33*onecbcl_q42_p
onecbcl_q43_p ~~ u34*onecbcl_q43_p
onecbcl_q44_p ~~ u35*onecbcl_q44_p
onecbcl_q45_p ~~ u36*onecbcl_q45_p
onecbcl_q46_p ~~ u37*onecbcl_q46_p
onecbcl_q47_p ~~ u38*onecbcl_q47_p
onecbcl_q49_p ~~ u39*onecbcl_q49_p
onecbcl_q50_p ~~ u40*onecbcl_q50_p
onecbcl_q51_p ~~ u41*onecbcl_q51_p
onecbcl_q52_p ~~ u42*onecbcl_q52_p
onecbcl_q54_p ~~ u43*onecbcl_q54_p
onecbcl_q56a_p ~~ u44*onecbcl_q56a_p
onecbcl_q56b_p ~~ u45*onecbcl_q56b_p
onecbcl_q56c_p ~~ u46*onecbcl_q56c_p
onecbcl_q56d_p ~~ u47*onecbcl_q56d_p
onecbcl_q56e_p ~~ u48*onecbcl_q56e_p
onecbcl_q56f_p ~~ u49*onecbcl_q56f_p
onecbcl_q56g_p ~~ u50*onecbcl_q56g_p
onecbcl_q56h_p ~~ u51*onecbcl_q56h_p
onecbcl_q58_p ~~ u52*onecbcl_q58_p
onecbcl_q61_p ~~ u53*onecbcl_q61_p
onecbcl_q62_p ~~ u54*onecbcl_q62_p
onecbcl_q63_p ~~ u55*onecbcl_q63_p
onecbcl_q64_p ~~ u56*onecbcl_q64_p
onecbcl_q65_p ~~ u57*onecbcl_q65_p
onecbcl_q66_p ~~ u58*onecbcl_q66_p
onecbcl_q67_p ~~ u59*onecbcl_q67_p
onecbcl_q68_p ~~ u60*onecbcl_q68_p
onecbcl_q69_p ~~ u61*onecbcl_q69_p
onecbcl_q71_p ~~ u62*onecbcl_q71_p
onecbcl_q72_p ~~ u63*onecbcl_q72_p
onecbcl_q74_p ~~ u64*onecbcl_q74_p
onecbcl_q75_p ~~ u65*onecbcl_q75_p
onecbcl_q76_p ~~ u66*onecbcl_q76_p
onecbcl_q77_p ~~ u67*onecbcl_q77_p
onecbcl_q79_p ~~ u68*onecbcl_q79_p
onecbcl_q80_p ~~ u69*onecbcl_q80_p
onecbcl_q83_p ~~ u70*onecbcl_q83_p
onecbcl_q84_p ~~ u71*onecbcl_q84_p
onecbcl_q85_p ~~ u72*onecbcl_q85_p
onecbcl_q86_p ~~ u73*onecbcl_q86_p
onecbcl_q87_p ~~ u74*onecbcl_q87_p
onecbcl_q88_p ~~ u75*onecbcl_q88_p
onecbcl_q89_p ~~ u76*onecbcl_q89_p
onecbcl_q90_p ~~ u77*onecbcl_q90_p
onecbcl_q91_p ~~ u78*onecbcl_q91_p
onecbcl_q92_p ~~ u79*onecbcl_q92_p
onecbcl_q93_p ~~ u80*onecbcl_q93_p
onecbcl_q94_p ~~ u81*onecbcl_q94_p
onecbcl_q95_p ~~ u82*onecbcl_q95_p
onecbcl_q96_p ~~ u83*onecbcl_q96_p
onecbcl_q98_p ~~ u84*onecbcl_q98_p
onecbcl_q100_p ~~ u85*onecbcl_q100_p
onecbcl_q102_p ~~ u86*onecbcl_q102_p
onecbcl_q103_p ~~ u87*onecbcl_q103_p
onecbcl_q104_p ~~ u88*onecbcl_q104_p
onecbcl_q107_p ~~ u89*onecbcl_q107_p
onecbcl_q108_p ~~ u90*onecbcl_q108_p
onecbcl_q109_p ~~ u91*onecbcl_q109_p
onecbcl_q110_p ~~ u92*onecbcl_q110_p
onecbcl_q111_p ~~ u93*onecbcl_q111_p
onecbcl_q112_p ~~ u94*onecbcl_q112_p
oneAttacksThreatens ~~ u95*oneAttacksThreatens
oneDestroys ~~ u96*oneDestroys
oneDisobeysrules ~~ u97*oneDisobeysrules
oneSteals ~~ u98*oneSteals
onePeerProblems ~~ u99*onePeerProblems
oneDistractedHyperactive ~~ u100*oneDistractedHyperactive
oneHallucinations ~~ u101*oneHallucinations
oneSexPlay ~~ u102*oneSexPlay
oneWeightproblems ~~ u103*oneWeightproblems
'
print("RESIDUAL MODEL 1 DONE")

residualModel1 <- paste(metric1, errorstructure1, scalar1, residual1, sep = ' ', collapse = NULL)

residual_fit1 <- cfa(residual1,
                   data = cbclDF1)
print("RESIDUAL CFA")

summary(residual_fit1, fit.measures = TRUE,
        standardized = TRUE,
        ci = TRUE,
        rsquare = TRUE)

#factor.scores(cbclDF1, residual_fit1)

#create a model and CFA for baseline and two year RESIDUAl INVARAINCE 
print("RESIDUAL MODEL 2")
residual2<- '
# Residual error constrained across waves

basecbcl_q01_p ~~ u2*basecbcl_q01_p
basecbcl_q03_p ~~ u3*basecbcl_q03_p
basecbcl_q04_p ~~ u4*basecbcl_q04_p
basecbcl_q05_p ~~ u5*basecbcl_q05_p
basecbcl_q06_p ~~ u6*basecbcl_q06_p
basecbcl_q07_p ~~ u7*basecbcl_q07_p
basecbcl_q09_p ~~ u8*basecbcl_q09_p
basecbcl_q11_p ~~ u9*basecbcl_q11_p
basecbcl_q12_p ~~ u10*basecbcl_q12_p
basecbcl_q13_p ~~ u11*basecbcl_q13_p
basecbcl_q14_p ~~ u12*basecbcl_q14_p
basecbcl_q15_p ~~ u13*basecbcl_q15_p
basecbcl_q16_p ~~ u14*basecbcl_q16_p
basecbcl_q17_p ~~ u15*basecbcl_q17_p
basecbcl_q18_p ~~ u16*basecbcl_q18_p
basecbcl_q19_p ~~ u17*basecbcl_q19_p
basecbcl_q24_p ~~ u18*basecbcl_q24_p
basecbcl_q26_p ~~ u19*basecbcl_q26_p
basecbcl_q27_p ~~ u20*basecbcl_q27_p
basecbcl_q29_p ~~ u21*basecbcl_q29_p
basecbcl_q30_p ~~ u22*basecbcl_q30_p
basecbcl_q31_p ~~ u23*basecbcl_q31_p
basecbcl_q32_p ~~ u24*basecbcl_q32_p
basecbcl_q33_p ~~ u25*basecbcl_q33_p
basecbcl_q34_p ~~ u26*basecbcl_q34_p
basecbcl_q35_p ~~ u27*basecbcl_q35_p
basecbcl_q36_p ~~ u28*basecbcl_q36_p
basecbcl_q37_p ~~ u29*basecbcl_q37_p
basecbcl_q38_p ~~ u30*basecbcl_q38_p
basecbcl_q39_p ~~ u31*basecbcl_q39_p
basecbcl_q41_p ~~ u32*basecbcl_q41_p
basecbcl_q42_p ~~ u33*basecbcl_q42_p
basecbcl_q43_p ~~ u34*basecbcl_q43_p
basecbcl_q44_p ~~ u35*basecbcl_q44_p
basecbcl_q45_p ~~ u36*basecbcl_q45_p
basecbcl_q46_p ~~ u37*basecbcl_q46_p
basecbcl_q47_p ~~ u38*basecbcl_q47_p
basecbcl_q49_p ~~ u39*basecbcl_q49_p
basecbcl_q50_p ~~ u40*basecbcl_q50_p
basecbcl_q51_p ~~ u41*basecbcl_q51_p
basecbcl_q52_p ~~ u42*basecbcl_q52_p
basecbcl_q54_p ~~ u43*basecbcl_q54_p
basecbcl_q56a_p ~~ u44*basecbcl_q56a_p
basecbcl_q56b_p ~~ u45*basecbcl_q56b_p
basecbcl_q56c_p ~~ u46*basecbcl_q56c_p
basecbcl_q56d_p ~~ u47*basecbcl_q56d_p
basecbcl_q56e_p ~~ u48*basecbcl_q56e_p
basecbcl_q56f_p ~~ u49*basecbcl_q56f_p
basecbcl_q56g_p ~~ u50*basecbcl_q56g_p
basecbcl_q56h_p ~~ u51*basecbcl_q56h_p
basecbcl_q58_p ~~ u52*basecbcl_q58_p
basecbcl_q61_p ~~ u53*basecbcl_q61_p
basecbcl_q62_p ~~ u54*basecbcl_q62_p
basecbcl_q63_p ~~ u55*basecbcl_q63_p
basecbcl_q64_p ~~ u56*basecbcl_q64_p
basecbcl_q65_p ~~ u57*basecbcl_q65_p
basecbcl_q66_p ~~ u58*basecbcl_q66_p
basecbcl_q67_p ~~ u59*basecbcl_q67_p
basecbcl_q68_p ~~ u60*basecbcl_q68_p
basecbcl_q69_p ~~ u61*basecbcl_q69_p
basecbcl_q71_p ~~ u62*basecbcl_q71_p
basecbcl_q72_p ~~ u63*basecbcl_q72_p
basecbcl_q74_p ~~ u64*basecbcl_q74_p
basecbcl_q75_p ~~ u65*basecbcl_q75_p
basecbcl_q76_p ~~ u66*basecbcl_q76_p
basecbcl_q77_p ~~ u67*basecbcl_q77_p
basecbcl_q79_p ~~ u68*basecbcl_q79_p
basecbcl_q80_p ~~ u69*basecbcl_q80_p
basecbcl_q83_p ~~ u70*basecbcl_q83_p
basecbcl_q84_p ~~ u71*basecbcl_q84_p
basecbcl_q85_p ~~ u72*basecbcl_q85_p
basecbcl_q86_p ~~ u73*basecbcl_q86_p
basecbcl_q87_p ~~ u74*basecbcl_q87_p
basecbcl_q88_p ~~ u75*basecbcl_q88_p
basecbcl_q89_p ~~ u76*basecbcl_q89_p
basecbcl_q90_p ~~ u77*basecbcl_q90_p
basecbcl_q91_p ~~ u78*basecbcl_q91_p
basecbcl_q92_p ~~ u79*basecbcl_q92_p
basecbcl_q93_p ~~ u80*basecbcl_q93_p
basecbcl_q94_p ~~ u81*basecbcl_q94_p
basecbcl_q95_p ~~ u82*basecbcl_q95_p
basecbcl_q96_p ~~ u83*basecbcl_q96_p
basecbcl_q98_p ~~ u84*basecbcl_q98_p
basecbcl_q100_p ~~ u85*basecbcl_q100_p
basecbcl_q102_p ~~ u86*basecbcl_q102_p
basecbcl_q103_p ~~ u87*basecbcl_q103_p
basecbcl_q104_p ~~ u88*basecbcl_q104_p
basecbcl_q107_p ~~ u89*basecbcl_q107_p
basecbcl_q108_p ~~ u90*basecbcl_q108_p
basecbcl_q109_p ~~ u91*basecbcl_q109_p
basecbcl_q110_p ~~ u92*basecbcl_q110_p
basecbcl_q111_p ~~ u93*basecbcl_q111_p
basecbcl_q112_p ~~ u94*basecbcl_q112_p
baseAttacksThreatens ~~ u95*baseAttacksThreatens
baseDestroys ~~ u96*baseDestroys
baseDisobeysrules ~~ u97*baseDisobeysrules
baseSteals ~~ u98*baseSteals
basePeerProblems ~~ u99*basePeerProblems 
baseDistractedHyperactive ~~ u100*baseDistractedHyperactive
baseHallucinations ~~ u101*baseHallucinations
baseSexPlay ~~ u102*baseSexPlay
baseWeightproblems ~~ baseWeightproblems

twocbcl_q01_p ~~ u2*twocbcl_q01_p
twocbcl_q03_p ~~ u3*twocbcl_q03_p
twocbcl_q04_p ~~ u4*twocbcl_q04_p
twocbcl_q05_p ~~ u5*twocbcl_q05_p
twocbcl_q06_p ~~ u6*twocbcl_q06_p
twocbcl_q07_p ~~ u7*twocbcl_q07_p
twocbcl_q09_p ~~ u8*twocbcl_q09_p
twocbcl_q11_p ~~ u9*twocbcl_q11_p
twocbcl_q12_p ~~ u10*twocbcl_q12_p
twocbcl_q13_p ~~ u11*twocbcl_q13_p
twocbcl_q14_p ~~ u12*twocbcl_q14_p
twocbcl_q15_p ~~ u13*twocbcl_q15_p
twocbcl_q16_p ~~ u14*twocbcl_q16_p
twocbcl_q17_p ~~ u15*twocbcl_q17_p
twocbcl_q18_p ~~ u16*twocbcl_q18_p
twocbcl_q19_p ~~ u17*twocbcl_q19_p
twocbcl_q24_p ~~ u18*twocbcl_q24_p
twocbcl_q26_p ~~ u19*twocbcl_q26_p
twocbcl_q27_p ~~ u20*twocbcl_q27_p
twocbcl_q29_p ~~ u21*twocbcl_q29_p
twocbcl_q30_p ~~ u22*twocbcl_q30_p
twocbcl_q31_p ~~ u23*twocbcl_q31_p
twocbcl_q32_p ~~ u24*twocbcl_q32_p
twocbcl_q33_p ~~ u25*twocbcl_q33_p
twocbcl_q34_p ~~ u26*twocbcl_q34_p
twocbcl_q35_p ~~ u27*twocbcl_q35_p
twocbcl_q36_p ~~ u28*twocbcl_q36_p
twocbcl_q37_p ~~ u29*twocbcl_q37_p
twocbcl_q38_p ~~ u30*twocbcl_q38_p
twocbcl_q39_p ~~ u31*twocbcl_q39_p
twocbcl_q41_p ~~ u32*twocbcl_q41_p
twocbcl_q42_p ~~ u33*twocbcl_q42_p
twocbcl_q43_p ~~ u34*twocbcl_q43_p
twocbcl_q44_p ~~ u35*twocbcl_q44_p
twocbcl_q45_p ~~ u36*twocbcl_q45_p
twocbcl_q46_p ~~ u37*twocbcl_q46_p
twocbcl_q47_p ~~ u38*twocbcl_q47_p
twocbcl_q49_p ~~ u39*twocbcl_q49_p
twocbcl_q50_p ~~ u40*twocbcl_q50_p
twocbcl_q51_p ~~ u41*twocbcl_q51_p
twocbcl_q52_p ~~ u42*twocbcl_q52_p
twocbcl_q54_p ~~ u43*twocbcl_q54_p
twocbcl_q56a_p ~~ u44*twocbcl_q56a_p
twocbcl_q56b_p ~~ u45*twocbcl_q56b_p
twocbcl_q56c_p ~~ u46*twocbcl_q56c_p
twocbcl_q56d_p ~~ u47*twocbcl_q56d_p
twocbcl_q56e_p ~~ u48*twocbcl_q56e_p
twocbcl_q56f_p ~~ u49*twocbcl_q56f_p
twocbcl_q56g_p ~~ u50*twocbcl_q56g_p
twocbcl_q56h_p ~~ u51*twocbcl_q56h_p
twocbcl_q58_p ~~ u52*twocbcl_q58_p
twocbcl_q61_p ~~ u53*twocbcl_q61_p
twocbcl_q62_p ~~ u54*twocbcl_q62_p
twocbcl_q63_p ~~ u55*twocbcl_q63_p
twocbcl_q64_p ~~ u56*twocbcl_q64_p
twocbcl_q65_p ~~ u57*twocbcl_q65_p
twocbcl_q66_p ~~ u58*twocbcl_q66_p
twocbcl_q67_p ~~ u59*twocbcl_q67_p
twocbcl_q68_p ~~ u60*twocbcl_q68_p
twocbcl_q69_p ~~ u61*twocbcl_q69_p
twocbcl_q71_p ~~ u62*twocbcl_q71_p
twocbcl_q72_p ~~ u63*twocbcl_q72_p
twocbcl_q74_p ~~ u64*twocbcl_q74_p
twocbcl_q75_p ~~ u65*twocbcl_q75_p
twocbcl_q76_p ~~ u66*twocbcl_q76_p
twocbcl_q77_p ~~ u67*twocbcl_q77_p
twocbcl_q79_p ~~ u68*twocbcl_q79_p
twocbcl_q80_p ~~ u69*twocbcl_q80_p
twocbcl_q83_p ~~ u70*twocbcl_q83_p
twocbcl_q84_p ~~ u71*twocbcl_q84_p
twocbcl_q85_p ~~ u72*twocbcl_q85_p
twocbcl_q86_p ~~ u73*twocbcl_q86_p
twocbcl_q87_p ~~ u74*twocbcl_q87_p
twocbcl_q88_p ~~ u75*twocbcl_q88_p
twocbcl_q89_p ~~ u76*twocbcl_q89_p
twocbcl_q90_p ~~ u77*twocbcl_q90_p
twocbcl_q91_p ~~ u78*twocbcl_q91_p
twocbcl_q92_p ~~ u79*twocbcl_q92_p
twocbcl_q93_p ~~ u80*twocbcl_q93_p
twocbcl_q94_p ~~ u81*twocbcl_q94_p
twocbcl_q95_p ~~ u82*twocbcl_q95_p
twocbcl_q96_p ~~ u83*twocbcl_q96_p
twocbcl_q98_p ~~ u84*twocbcl_q98_p
twocbcl_q100_p ~~ u85*twocbcl_q100_p
twocbcl_q102_p ~~ u86*twocbcl_q102_p
twocbcl_q103_p ~~ u87*twocbcl_q103_p
twocbcl_q104_p ~~ u88*twocbcl_q104_p
twocbcl_q107_p ~~ u89*twocbcl_q107_p
twocbcl_q108_p ~~ u90*twocbcl_q108_p
twocbcl_q109_p ~~ u91*twocbcl_q109_p
twocbcl_q110_p ~~ u92*twocbcl_q110_p
twocbcl_q111_p ~~ u93*twocbcl_q111_p
twocbcl_q112_p ~~ u94*twocbcl_q112_p
twoAttacksThreatens ~~ u95*twoAttacksThreatens
twoDestroys ~~ u96*twoDestroys
twoDisobeysrules ~~ u97*twoDisobeysrules
twoSteals ~~ u98*twoSteals
twoPeerProblems ~~ u99*twoPeerProblems
twoDistractedHyperactive ~~ u100*twoDistractedHyperactive
twoHallucinations ~~ u101*twoHallucinations
twoSexPlay ~~ u102*twoSexPlay
twoWeightproblems ~~ u103*twoWeightproblems
'
print("RESIDUAL MODEL 2 DONE")

residualModel2 <- paste(metric2, errorstructure2, scalar2, residual2, sep = ' ', collapse = NULL)

residual_fit2 <- cfa(residual2,
                     data = cbclDF2)
print("RESIDUAL CFA 2")

summary(residual_fit2, fit.measures = TRUE,
        standardized = TRUE,
        ci = TRUE,
        rsquare = TRUE)

#factor.scores(cbclDF2, residual_fit2)

#Model diagram for baseline-one year
#semPaths(residual_fit1, what="est", sizeLat = 7, sizeMan = 7, edge.label.cex = .75)

#Model diagram for baseline-two year
#semPaths(residual_fit2, what="est", sizeLat = 7, sizeMan = 7, edge.label.cex = .75)





