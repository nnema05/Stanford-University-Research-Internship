
## LONGITUDNAL CFA
### METRIC, SCALAR, AND RESIDUAL MODEL FOR LONGITUDNAL CFA --> Specifcially for P-Factor
# This program creates the METRIC, SCALAR, AND RESIDUAL model for Longitudinal CFA 
# 1. Create 2 datasets, one that has baseline and one year data, and one that has baseline and two year data
# 2. Create 2 metric models, 2 scalar models, and 2 residual. Each model has information for P-Factor specifcally!
    # one model has factor information for baseline and one year
    # the other model factor information for baseline and two year
# 3. Using cfa(), fit a Confirmatory Factor Analysis (CFA) for both models
# 4. Get factor scores from each METRIC, SCALAR, AND RESIDUAL MODELS

###METRIC
pFactorMetric1 <- '
#Define P-factor  
   basePFactor=~
basecbcl_q56a_p
+lambda1*basecbcl_q01_p 
+lambda2*basecbcl_q03_p
+lambda3*basecbcl_q06_p
+lambda4*basecbcl_q07_p
+lambda5*basecbcl_q09_p
+lambda6*basecbcl_q11_p
+lambda7*basecbcl_q12_p
+lambda8*basecbcl_q13_p
+lambda9*basecbcl_q14_p
+lambda10*basecbcl_q15_p
+lambda11*basecbcl_q16_p
+lambda12*basecbcl_q17_p
+lambda13*basecbcl_q18_p
+lambda14*basecbcl_q19_p
+lambda15*basecbcl_q24_p
+lambda16*basecbcl_q26_p
+lambda17*basecbcl_q27_p
+lambda18*basecbcl_q04_p
+lambda19*basecbcl_q29_p
+lambda20*basecbcl_q30_p
+lambda21*basecbcl_q31_p
+lambda22*basecbcl_q51_p
+lambda23*basecbcl_q32_p
+lambda24*basecbcl_q33_p
+lambda25*basecbcl_q34_p
+lambda26*basecbcl_q52_p
+lambda27*basecbcl_q35_p
+lambda28*basecbcl_q36_p
+lambda29*basecbcl_q37_p
+lambda30*basecbcl_q38_p
+lambda31*basecbcl_q39_p
+lambda32*basecbcl_q56b_p
+lambda33*basecbcl_q41_p
+lambda34*basecbcl_q43_p
+lambda35*basecbcl_q56c_p
+lambda36*basecbcl_q46_p
+lambda37*basecbcl_q45_p
+lambda38*basecbcl_q47_p
+lambda39*basecbcl_q56h_p
+lambda40*basecbcl_q54_p
+lambda41*basecbcl_q58_p
+lambda42*basecbcl_q61_p
+lambda43*basecbcl_q62_p
+lambda44*basecbcl_q63_p
+lambda45*basecbcl_q64_p
+lambda46*basecbcl_q56d_p
+lambda47*basecbcl_q56e_p
+lambda48*basecbcl_q65_p
+lambda49*basecbcl_q66_p
+lambda50*basecbcl_q67_p
+lambda51*basecbcl_q68_p
+lambda52*basecbcl_q69_p
+lambda53*basecbcl_q71_p
+lambda54*basecbcl_q74_p
+lambda55*basecbcl_q76_p
+lambda56*basecbcl_q77_p
+lambda57*basecbcl_q79_p
+lambda58*basecbcl_q80_p
+lambda59*basecbcl_q56f_p
+lambda60*basecbcl_q83_p
+lambda61*basecbcl_q84_p
+lambda62*basecbcl_q85_p
+lambda63*basecbcl_q86_p
+lambda64*basecbcl_q87_p
+lambda65*basecbcl_q88_p
+lambda66*basecbcl_q89_p
+lambda67*basecbcl_q90_p
+lambda68*basecbcl_q91_p
+lambda69*basecbcl_q93_p
+lambda70*basecbcl_q94_p
+lambda71*basecbcl_q95_p
+lambda72*basecbcl_q96_p
+lambda73*basecbcl_q50_p
+lambda74*basecbcl_q75_p
+lambda75*basecbcl_q100_p
+lambda76*basecbcl_q102_p
+lambda77*basecbcl_q103_p
+lambda78*basecbcl_q104_p
+lambda79*basecbcl_q56g_p
+lambda80*basecbcl_q107_p
+lambda81*basecbcl_q109_p
+lambda82*basecbcl_q110_p
+lambda83*basecbcl_q111_p
+lambda84*basecbcl_q112_p
+lambda85*basecbcl_q13_p
+lambda86*basecbcl_q42_p
+lambda87*baseAttacksThreatens
+lambda88*baseDestroys
+lambda89*baseDisobeysrules
+lambda90*baseDistractedHyperactive
+lambda91*baseHallucinations
+lambda92*basePeerProblems
+lambda93*baseSexPlay
+lambda94*baseSteals
+lambda95*baseWeightproblems
    onePFactor=~
onecbcl_q56a_p
+lambda1*onecbcl_q01_p 
+lambda2*onecbcl_q03_p
+lambda3*onecbcl_q06_p
+lambda4*onecbcl_q07_p
+lambda5*onecbcl_q09_p
+lambda6*onecbcl_q11_p
+lambda7*onecbcl_q12_p
+lambda8*onecbcl_q13_p
+lambda9*onecbcl_q14_p
+lambda10*onecbcl_q15_p
+lambda11*onecbcl_q16_p
+lambda12*onecbcl_q17_p
+lambda13*onecbcl_q18_p
+lambda14*onecbcl_q19_p
+lambda15*onecbcl_q24_p
+lambda16*onecbcl_q26_p
+lambda17*onecbcl_q27_p
+lambda18*onecbcl_q04_p
+lambda19*onecbcl_q29_p
+lambda20*onecbcl_q30_p
+lambda21*onecbcl_q31_p
+lambda22*onecbcl_q51_p
+lambda23*onecbcl_q32_p
+lambda24*onecbcl_q33_p
+lambda25*onecbcl_q34_p
+lambda26*onecbcl_q52_p
+lambda27*onecbcl_q35_p
+lambda28*onecbcl_q36_p
+lambda29*onecbcl_q37_p
+lambda30*onecbcl_q38_p
+lambda31*onecbcl_q39_p
+lambda32*onecbcl_q56b_p
+lambda33*onecbcl_q41_p
+lambda34*onecbcl_q43_p
+lambda35*onecbcl_q56c_p
+lambda36*onecbcl_q46_p
+lambda37*onecbcl_q45_p
+lambda38*onecbcl_q47_p
+lambda39*onecbcl_q56h_p
+lambda40*onecbcl_q54_p
+lambda41*onecbcl_q58_p
+lambda42*onecbcl_q61_p
+lambda43*onecbcl_q62_p
+lambda44*onecbcl_q63_p
+lambda45*onecbcl_q64_p
+lambda46*onecbcl_q56d_p
+lambda47*onecbcl_q56e_p
+lambda48*onecbcl_q65_p
+lambda49*onecbcl_q66_p
+lambda50*onecbcl_q67_p
+lambda51*onecbcl_q68_p
+lambda52*onecbcl_q69_p
+lambda53*onecbcl_q71_p
+lambda54*onecbcl_q74_p
+lambda55*onecbcl_q76_p
+lambda56*onecbcl_q77_p
+lambda57*onecbcl_q79_p
+lambda58*onecbcl_q80_p
+lambda59*onecbcl_q56f_p
+lambda60*onecbcl_q83_p
+lambda61*onecbcl_q84_p
+lambda62*onecbcl_q85_p
+lambda63*onecbcl_q86_p
+lambda64*onecbcl_q87_p
+lambda65*onecbcl_q88_p
+lambda66*onecbcl_q89_p
+lambda67*onecbcl_q90_p
+lambda68*onecbcl_q91_p
+lambda69*onecbcl_q93_p
+lambda70*onecbcl_q94_p
+lambda71*onecbcl_q95_p
+lambda72*onecbcl_q96_p
+lambda73*onecbcl_q50_p
+lambda74*onecbcl_q75_p
+lambda75*onecbcl_q100_p
+lambda76*onecbcl_q102_p
+lambda77*onecbcl_q103_p
+lambda78*onecbcl_q104_p
+lambda79*onecbcl_q56g_p
+lambda80*onecbcl_q107_p
+lambda81*onecbcl_q109_p
+lambda82*onecbcl_q110_p
+lambda83*onecbcl_q111_p
+lambda84*onecbcl_q112_p
+lambda85*onecbcl_q13_p
+lambda86*onecbcl_q42_p
+lambda87*oneAttacksThreatens
+lambda88*oneDestroys
+lambda89*oneDisobeysrules
+lambda90*oneDistractedHyperactive
+lambda91*oneHallucinations
+lambda92*onePeerProblems
+lambda93*oneSexPlay
+lambda94*oneSteals
+lambda95*oneWeightproblems

#P Factor Variance constrained to 1 
basePFactor ~~ 1*basePFactor
onePFactor ~~ 1*onePFactor
'

pFactorMetricModel1 <- paste(pFactorMetric1, errorstructure1, sep = ' ', collapse = NULL)

pFactorMetric_fit1 <- cfa(pFactorMetricModel1,
                          data = cbclDF1)
pFactorMetric2 <- '
#Define P-factor  
basePFactor=~
  basecbcl_q56a_p
+lambda1*basecbcl_q01_p 
+lambda2*basecbcl_q03_p
+lambda3*basecbcl_q06_p
+lambda4*basecbcl_q07_p
+lambda5*basecbcl_q09_p
+lambda6*basecbcl_q11_p
+lambda7*basecbcl_q12_p
+lambda8*basecbcl_q13_p
+lambda9*basecbcl_q14_p
+lambda10*basecbcl_q15_p
+lambda11*basecbcl_q16_p
+lambda12*basecbcl_q17_p
+lambda13*basecbcl_q18_p
+lambda14*basecbcl_q19_p
+lambda15*basecbcl_q24_p
+lambda16*basecbcl_q26_p
+lambda17*basecbcl_q27_p
+lambda18*basecbcl_q04_p
+lambda19*basecbcl_q29_p
+lambda20*basecbcl_q30_p
+lambda21*basecbcl_q31_p
+lambda22*basecbcl_q51_p
+lambda23*basecbcl_q32_p
+lambda24*basecbcl_q33_p
+lambda25*basecbcl_q34_p
+lambda26*basecbcl_q52_p
+lambda27*basecbcl_q35_p
+lambda28*basecbcl_q36_p
+lambda29*basecbcl_q37_p
+lambda30*basecbcl_q38_p
+lambda31*basecbcl_q39_p
+lambda32*basecbcl_q56b_p
+lambda33*basecbcl_q41_p
+lambda34*basecbcl_q43_p
+lambda35*basecbcl_q56c_p
+lambda36*basecbcl_q46_p
+lambda37*basecbcl_q45_p
+lambda38*basecbcl_q47_p
+lambda39*basecbcl_q56h_p
+lambda40*basecbcl_q54_p
+lambda41*basecbcl_q58_p
+lambda42*basecbcl_q61_p
+lambda43*basecbcl_q62_p
+lambda44*basecbcl_q63_p
+lambda45*basecbcl_q64_p
+lambda46*basecbcl_q56d_p
+lambda47*basecbcl_q56e_p
+lambda48*basecbcl_q65_p
+lambda49*basecbcl_q66_p
+lambda50*basecbcl_q67_p
+lambda51*basecbcl_q68_p
+lambda52*basecbcl_q69_p
+lambda53*basecbcl_q71_p
+lambda54*basecbcl_q74_p
+lambda55*basecbcl_q76_p
+lambda56*basecbcl_q77_p
+lambda57*basecbcl_q79_p
+lambda58*basecbcl_q80_p
+lambda59*basecbcl_q56f_p
+lambda60*basecbcl_q83_p
+lambda61*basecbcl_q84_p
+lambda62*basecbcl_q85_p
+lambda63*basecbcl_q86_p
+lambda64*basecbcl_q87_p
+lambda65*basecbcl_q88_p
+lambda66*basecbcl_q89_p
+lambda67*basecbcl_q90_p
+lambda68*basecbcl_q91_p
+lambda69*basecbcl_q93_p
+lambda70*basecbcl_q94_p
+lambda71*basecbcl_q95_p
+lambda72*basecbcl_q96_p
+lambda73*basecbcl_q50_p
+lambda74*basecbcl_q75_p
+lambda75*basecbcl_q100_p
+lambda76*basecbcl_q102_p
+lambda77*basecbcl_q103_p
+lambda78*basecbcl_q104_p
+lambda79*basecbcl_q56g_p
+lambda80*basecbcl_q107_p
+lambda81*basecbcl_q109_p
+lambda82*basecbcl_q110_p
+lambda83*basecbcl_q111_p
+lambda84*basecbcl_q112_p
+lambda85*basecbcl_q13_p
+lambda86*basecbcl_q42_p
+lambda87*baseAttacksThreatens
+lambda88*baseDestroys
+lambda89*baseDisobeysrules
+lambda90*baseDistractedHyperactive
+lambda91*baseHallucinations
+lambda92*basePeerProblems
+lambda93*baseSexPlay
+lambda94*baseSteals
+lambda95*baseWeightproblems
twoPFactor=~
  twocbcl_q56a_p
+lambda1*twocbcl_q01_p 
+lambda2*twocbcl_q03_p
+lambda3*twocbcl_q06_p
+lambda4*twocbcl_q07_p
+lambda5*twocbcl_q09_p
+lambda6*twocbcl_q11_p
+lambda7*twocbcl_q12_p
+lambda8*twocbcl_q13_p
+lambda9*twocbcl_q14_p
+lambda10*twocbcl_q15_p
+lambda11*twocbcl_q16_p
+lambda12*twocbcl_q17_p
+lambda13*twocbcl_q18_p
+lambda14*twocbcl_q19_p
+lambda15*twocbcl_q24_p
+lambda16*twocbcl_q26_p
+lambda17*twocbcl_q27_p
+lambda18*twocbcl_q04_p
+lambda19*twocbcl_q29_p
+lambda20*twocbcl_q30_p
+lambda21*twocbcl_q31_p
+lambda22*twocbcl_q51_p
+lambda23*twocbcl_q32_p
+lambda24*twocbcl_q33_p
+lambda25*twocbcl_q34_p
+lambda26*twocbcl_q52_p
+lambda27*twocbcl_q35_p
+lambda28*twocbcl_q36_p
+lambda29*twocbcl_q37_p
+lambda30*twocbcl_q38_p
+lambda31*twocbcl_q39_p
+lambda32*twocbcl_q56b_p
+lambda33*twocbcl_q41_p
+lambda34*twocbcl_q43_p
+lambda35*twocbcl_q56c_p
+lambda36*twocbcl_q46_p
+lambda37*twocbcl_q45_p
+lambda38*twocbcl_q47_p
+lambda39*twocbcl_q56h_p
+lambda40*twocbcl_q54_p
+lambda41*twocbcl_q58_p
+lambda42*twocbcl_q61_p
+lambda43*twocbcl_q62_p
+lambda44*twocbcl_q63_p
+lambda45*twocbcl_q64_p
+lambda46*twocbcl_q56d_p
+lambda47*twocbcl_q56e_p
+lambda48*twocbcl_q65_p
+lambda49*twocbcl_q66_p
+lambda50*twocbcl_q67_p
+lambda51*twocbcl_q68_p
+lambda52*twocbcl_q69_p
+lambda53*twocbcl_q71_p
+lambda54*twocbcl_q74_p
+lambda55*twocbcl_q76_p
+lambda56*twocbcl_q77_p
+lambda57*twocbcl_q79_p
+lambda58*twocbcl_q80_p
+lambda59*twocbcl_q56f_p
+lambda60*twocbcl_q83_p
+lambda61*twocbcl_q84_p
+lambda62*twocbcl_q85_p
+lambda63*twocbcl_q86_p
+lambda64*twocbcl_q87_p
+lambda65*twocbcl_q88_p
+lambda66*twocbcl_q89_p
+lambda67*twocbcl_q90_p
+lambda68*twocbcl_q91_p
+lambda69*twocbcl_q93_p
+lambda70*twocbcl_q94_p
+lambda71*twocbcl_q95_p
+lambda72*twocbcl_q96_p
+lambda73*twocbcl_q50_p
+lambda74*twocbcl_q75_p
+lambda75*twocbcl_q100_p
+lambda76*twocbcl_q102_p
+lambda77*twocbcl_q103_p
+lambda78*twocbcl_q104_p
+lambda79*twocbcl_q56g_p
+lambda80*twocbcl_q107_p
+lambda81*twocbcl_q109_p
+lambda82*twocbcl_q110_p
+lambda83*twocbcl_q111_p
+lambda84*twocbcl_q112_p
+lambda85*twocbcl_q13_p
+lambda86*twocbcl_q42_p
+lambda87*twoAttacksThreatens
+lambda88*twoDestroys
+lambda89*twoDisobeysrules
+lambda90*twoDistractedHyperactive
+lambda91*twoHallucinations
+lambda92*twoPeerProblems
+lambda93*twoSexPlay
+lambda94*twoSteals
+lambda95*twoWeightproblems

#P Factor Variance constrained to 1 
basePFactor ~~ 1*basePFactor
twoPFactor ~~ 1*twoPFactor
'
pFactorMetricModel2 <- paste(pFactorMetric2, errorstructure2, sep = ' ', collapse = NULL)

pFactorMetric_fit2 <- cfa(pFactorMetricModel2,
                          data = cbclDF2)

print("PFACTOR METRIC MODEL AND CFA DONE")

###SCALAR
print("SCALAR MODEL 1")
pFactorScalar1 <- '
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
print("PFACTOR SCALAR MODEL 1 DONE")

pFactorscalarModel1 <- paste(pFactorMetric1, errorstructure1, pFactorScalar1, sep = ' ', collapse = NULL)

pFactorScalar_fit1 <- cfa(pFactorscalarModel1,
                   data = cbclDF1)
print("PFACTOR SCALAR CFA")


pFactorScalar2 <- '
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
print("P FACTOR SCALAR MODEL 2 DONE")
pFactorscalarModel2 <- paste(pFactorMetric2, errorstructure2, pFactorScalar2, sep = ' ', collapse = NULL)

pFactorScalar_fit2 <- cfa(pFactorscalarModel2,
                   data = cbclDF2)
print("P FACTOR SCALAR 2 CFA")


####RESIDUAL 

print("RESIDUAL MODEL 1")
pFactorResidual1<- '
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
print(" P FACTOR RESIDUAL MODEL 1 DONE")

pFactorResidualModel1 <- paste(pFactorMetric1, errorstructure1, pFactorScalar1, pFactorResidual1, sep = ' ', collapse = NULL)

pFactorresidual_fit1 <- cfa(pFactorResidualModel1,
                     data = cbclDF1)
print("P FACTOR RESIDUAL CFA")


pFactorResidual2<- '
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
print("PFACTOR RESIDUAL MODEL 2 DONE")

pFactorResidualModel2 <- paste(pFactorMetric2, errorstructure2, pFactorScalar2, pFactorResidual2, sep = ' ', collapse = NULL)

pFactorresidual_fit2 <- cfa(pFactorResidualModel2,
                            data = cbclDF2)
print("P FACTOR RESIDUAL CFA")


pfactorfitmeasures <- round(cbind(pFactorConfigural1=inspect(pFactorConfigural_fit1, 'fit.measures'),
                                  pFactorMetric1=inspect(pFactorMetric_fit1, 'fit.measures'),
                                  pFactorScalar1=inspect(pFactorScalar_fit1, 'fit.measures'),
                           pFactorResidual1=inspect(pFactorresidual_fit1, 'fit.measures'),
                           pFactorConfigural2=inspect(pFactorConfigural_fit2, 'fit.measures'),
                           pFactorMetric2=inspect(pFactorMetric_fit2, 'fit.measures'),
                           pFactorScalar2=inspect(pFactorScalar_fit2, 'fit.measures'),
                           pFactorResidual2=inspect(pFactorresidual_fit2, 'fit.measures')),3)
print(pfactorfitmeasures)