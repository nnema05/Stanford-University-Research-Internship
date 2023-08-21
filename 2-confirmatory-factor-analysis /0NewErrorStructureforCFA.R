
# ERROR STRUCTURE FOR LONGITUDNAL CONFIRMATORY FACTOR ANALYSIS
# A study called "Delineating and validating higher-order dimensions of psychopathology in the ABCD study" created a 5 factor structure of pyschopathology for us
# We want to test and verify this factor structure for our set of observed variables actually exists 
# This led to the use LONGITUDNAL CONFIRMATORY FACTOR ANALYSIS (CFA)
# For this ran a longitudinal CFA with an error structure, and four different models: configural, metric, scalar, and residual

# The below program this is running an error structure for longitudinal CFA
  # 

####STEP 0: INSTALL LIBRARIES 
library(sqldf)
library(psych)
library(lavaan)

# Error term co-variances constrained across waves
print("ERROR STRUCTURE")
errorstructure1 <- 
  'basecbcl_q01_p ~~ q01cov*onecbcl_q01_p
basecbcl_q03_p ~~ q03cov*onecbcl_q03_p
basecbcl_q04_p ~~ q04cov*onecbcl_q04_p
basecbcl_q05_p ~~ q05cov*onecbcl_q05_p
basecbcl_q06_p ~~ q06cov*onecbcl_q06_p
basecbcl_q07_p ~~ q07cov*onecbcl_q07_p
basecbcl_q09_p ~~ q09cov*onecbcl_q09_p
basecbcl_q11_p ~~ q11cov*onecbcl_q11_p
basecbcl_q12_p ~~ q12cov*onecbcl_q12_p
basecbcl_q13_p ~~ q13cov*onecbcl_q13_p
basecbcl_q14_p ~~ q14cov*onecbcl_q14_p
basecbcl_q15_p ~~ q15cov*onecbcl_q15_p
basecbcl_q16_p ~~ q16cov*onecbcl_q16_p
basecbcl_q17_p ~~ q17cov*onecbcl_q17_p
basecbcl_q18_p ~~ q18cov*onecbcl_q18_p
basecbcl_q19_p ~~ q19cov*onecbcl_q19_p
basecbcl_q24_p ~~ q24cov*onecbcl_q24_p
basecbcl_q26_p ~~ q26cov*onecbcl_q26_p
basecbcl_q27_p ~~ q27cov*onecbcl_q27_p
basecbcl_q29_p ~~ q29cov*onecbcl_q29_p
basecbcl_q30_p ~~ q30cov*onecbcl_q30_p
basecbcl_q31_p ~~ q31cov*onecbcl_q31_p
basecbcl_q32_p ~~ q32cov*onecbcl_q32_p
basecbcl_q33_p ~~ q33cov*onecbcl_q33_p
basecbcl_q34_p ~~ q34cov*onecbcl_q34_p
basecbcl_q35_p ~~ q35cov*onecbcl_q35_p
basecbcl_q36_p ~~ q36cov*onecbcl_q36_p
basecbcl_q37_p ~~ q37cov*onecbcl_q37_p
basecbcl_q38_p ~~ q38cov*onecbcl_q38_p
basecbcl_q39_p ~~ q39cov*onecbcl_q39_p
basecbcl_q41_p ~~ q41cov*onecbcl_q41_p
basecbcl_q42_p ~~ q42cov*onecbcl_q42_p
basecbcl_q43_p ~~ q43cov*onecbcl_q43_p
basecbcl_q44_p ~~ q44cov*onecbcl_q44_p
basecbcl_q45_p ~~ q45cov*onecbcl_q45_p
basecbcl_q46_p ~~ q46cov*onecbcl_q46_p
basecbcl_q47_p ~~ q47cov*onecbcl_q47_p
basecbcl_q49_p ~~ q49cov*onecbcl_q49_p
basecbcl_q50_p ~~ q50cov*onecbcl_q50_p
basecbcl_q51_p ~~ q51cov*onecbcl_q51_p
basecbcl_q52_p ~~ q52cov*onecbcl_q52_p
basecbcl_q54_p ~~ q54cov*onecbcl_q54_p
basecbcl_q56a_p ~~ q56acov*onecbcl_q56a_p
basecbcl_q56b_p ~~ q56bcov*onecbcl_q56b_p
basecbcl_q56c_p ~~ q56ccov*onecbcl_q56c_p
basecbcl_q56d_p ~~ q56dcov*onecbcl_q56d_p
basecbcl_q56e_p ~~ q56ecov*onecbcl_q56e_p
basecbcl_q56f_p ~~ q56fcov*onecbcl_q56f_p
basecbcl_q56g_p ~~ q56gcov*onecbcl_q56g_p
basecbcl_q56h_p ~~ q56hcov*onecbcl_q56h_p
basecbcl_q58_p ~~ q58cov*onecbcl_q58_p
basecbcl_q61_p ~~ q61cov*onecbcl_q61_p
basecbcl_q62_p ~~ q62cov*onecbcl_q62_p
basecbcl_q63_p ~~ q63cov*onecbcl_q63_p
basecbcl_q64_p ~~ q64cov*onecbcl_q64_p
basecbcl_q65_p ~~ q65cov*onecbcl_q65_p
basecbcl_q66_p ~~ q66cov*onecbcl_q66_p
basecbcl_q67_p ~~ q67cov*onecbcl_q67_p
basecbcl_q68_p ~~ q68cov*onecbcl_q68_p
basecbcl_q69_p ~~ q69cov*onecbcl_q69_p
basecbcl_q71_p ~~ q71cov*onecbcl_q71_p
basecbcl_q72_p ~~ q72cov*onecbcl_q72_p
basecbcl_q74_p ~~ q74cov*onecbcl_q74_p
basecbcl_q75_p ~~ q75cov*onecbcl_q75_p
basecbcl_q76_p ~~ q76cov*onecbcl_q76_p
basecbcl_q77_p ~~ q77cov*onecbcl_q77_p
basecbcl_q79_p ~~ q79cov*onecbcl_q79_p
basecbcl_q80_p ~~ q80cov*onecbcl_q80_p
basecbcl_q83_p ~~ q83cov*onecbcl_q83_p
basecbcl_q84_p ~~ q84cov*onecbcl_q84_p
basecbcl_q85_p ~~ q85cov*onecbcl_q85_p
basecbcl_q86_p ~~ q86cov*onecbcl_q86_p
basecbcl_q87_p ~~ q87cov*onecbcl_q87_p
basecbcl_q88_p ~~ q88cov*onecbcl_q88_p
basecbcl_q89_p ~~ q89cov*onecbcl_q89_p
basecbcl_q90_p ~~ q90cov*onecbcl_q90_p
basecbcl_q91_p ~~ q91cov*onecbcl_q91_p
basecbcl_q92_p ~~ q92cov*onecbcl_q92_p
basecbcl_q93_p ~~ q93cov*onecbcl_q93_p
basecbcl_q94_p ~~ q94cov*onecbcl_q94_p
basecbcl_q95_p ~~ q95cov*onecbcl_q95_p
basecbcl_q96_p ~~ q96cov*onecbcl_q96_p
basecbcl_q98_p ~~ q98cov*onecbcl_q98_p
basecbcl_q100_p ~~ q100cov*onecbcl_q100_p
basecbcl_q102_p ~~ q102cov*onecbcl_q102_p
basecbcl_q103_p ~~ q103cov*onecbcl_q103_p
basecbcl_q104_p ~~ q104cov*onecbcl_q104_p
basecbcl_q107_p ~~ q107cov*onecbcl_q107_p
basecbcl_q108_p ~~ q108cov*onecbcl_q108_p
basecbcl_q109_p ~~ q109cov*onecbcl_q109_p
basecbcl_q110_p ~~ q110cov*onecbcl_q110_p
basecbcl_q111_p ~~ q111cov*onecbcl_q111_p
basecbcl_q112_p ~~ q112cov*onecbcl_q112_p
baseAttacksThreatens ~~ q113cov*oneAttacksThreatens
baseDestroys ~~ q114cov*oneDestroys
baseDisobeysrules ~~ q115cov*oneDisobeysrules
baseSteals ~~ q116cov*oneSteals
basePeerProblems ~~ q117cov*onePeerProblems
baseDistractedHyperactive ~~ q118cov*oneDistractedHyperactive
baseHallucinations ~~ q119cov*oneHallucinations
baseSexPlay ~~ q120cov*oneSexPlay
baseWeightproblems ~~ q121cov*oneWeightproblems
'



errorstructure2 <- 
  'basecbcl_q01_p ~~ q01cov*twocbcl_q01_p
basecbcl_q03_p ~~ q03cov*twocbcl_q03_p
basecbcl_q04_p ~~ q04cov*twocbcl_q04_p
basecbcl_q05_p ~~ q05cov*twocbcl_q05_p
basecbcl_q06_p ~~ q06cov*twocbcl_q06_p
basecbcl_q07_p ~~ q07cov*twocbcl_q07_p
basecbcl_q09_p ~~ q09cov*twocbcl_q09_p
basecbcl_q11_p ~~ q11cov*twocbcl_q11_p
basecbcl_q12_p ~~ q12cov*twocbcl_q12_p
basecbcl_q13_p ~~ q13cov*twocbcl_q13_p
basecbcl_q14_p ~~ q14cov*twocbcl_q14_p
basecbcl_q15_p ~~ q15cov*twocbcl_q15_p
basecbcl_q16_p ~~ q16cov*twocbcl_q16_p
basecbcl_q17_p ~~ q17cov*twocbcl_q17_p
basecbcl_q18_p ~~ q18cov*twocbcl_q18_p
basecbcl_q19_p ~~ q19cov*twocbcl_q19_p
basecbcl_q24_p ~~ q24cov*twocbcl_q24_p
basecbcl_q26_p ~~ q26cov*twocbcl_q26_p
basecbcl_q27_p ~~ q27cov*twocbcl_q27_p
basecbcl_q29_p ~~ q29cov*twocbcl_q29_p
basecbcl_q30_p ~~ q30cov*twocbcl_q30_p
basecbcl_q31_p ~~ q31cov*twocbcl_q31_p
basecbcl_q32_p ~~ q32cov*twocbcl_q32_p
basecbcl_q33_p ~~ q33cov*twocbcl_q33_p
basecbcl_q34_p ~~ q34cov*twocbcl_q34_p
basecbcl_q35_p ~~ q35cov*twocbcl_q35_p
basecbcl_q36_p ~~ q36cov*twocbcl_q36_p
basecbcl_q37_p ~~ q37cov*twocbcl_q37_p
basecbcl_q38_p ~~ q38cov*twocbcl_q38_p
basecbcl_q39_p ~~ q39cov*twocbcl_q39_p
basecbcl_q41_p ~~ q41cov*twocbcl_q41_p
basecbcl_q42_p ~~ q42cov*twocbcl_q42_p
basecbcl_q43_p ~~ q43cov*twocbcl_q43_p
basecbcl_q44_p ~~ q44cov*twocbcl_q44_p
basecbcl_q45_p ~~ q45cov*twocbcl_q45_p
basecbcl_q46_p ~~ q46cov*twocbcl_q46_p
basecbcl_q47_p ~~ q47cov*twocbcl_q47_p
basecbcl_q49_p ~~ q49cov*twocbcl_q49_p
basecbcl_q50_p ~~ q50cov*twocbcl_q50_p
basecbcl_q51_p ~~ q51cov*twocbcl_q51_p
basecbcl_q52_p ~~ q52cov*twocbcl_q52_p
basecbcl_q54_p ~~ q54cov*twocbcl_q54_p
basecbcl_q56a_p ~~ q56acov*twocbcl_q56a_p
basecbcl_q56b_p ~~ q56bcov*twocbcl_q56b_p
basecbcl_q56c_p ~~ q56ccov*twocbcl_q56c_p
basecbcl_q56d_p ~~ q56dcov*twocbcl_q56d_p
basecbcl_q56e_p ~~ q56ecov*twocbcl_q56e_p
basecbcl_q56f_p ~~ q56fcov*twocbcl_q56f_p
basecbcl_q56g_p ~~ q56gcov*twocbcl_q56g_p
basecbcl_q56h_p ~~ q56hcov*twocbcl_q56h_p
basecbcl_q58_p ~~ q58cov*twocbcl_q58_p
basecbcl_q61_p ~~ q61cov*twocbcl_q61_p
basecbcl_q62_p ~~ q62cov*twocbcl_q62_p
basecbcl_q63_p ~~ q63cov*twocbcl_q63_p
basecbcl_q64_p ~~ q64cov*twocbcl_q64_p
basecbcl_q65_p ~~ q65cov*twocbcl_q65_p
basecbcl_q66_p ~~ q66cov*twocbcl_q66_p
basecbcl_q67_p ~~ q67cov*twocbcl_q67_p
basecbcl_q68_p ~~ q68cov*twocbcl_q68_p
basecbcl_q69_p ~~ q69cov*twocbcl_q69_p
basecbcl_q71_p ~~ q71cov*twocbcl_q71_p
basecbcl_q72_p ~~ q72cov*twocbcl_q72_p
basecbcl_q74_p ~~ q74cov*twocbcl_q74_p
basecbcl_q75_p ~~ q75cov*twocbcl_q75_p
basecbcl_q76_p ~~ q76cov*twocbcl_q76_p
basecbcl_q77_p ~~ q77cov*twocbcl_q77_p
basecbcl_q79_p ~~ q79cov*twocbcl_q79_p
basecbcl_q80_p ~~ q80cov*twocbcl_q80_p
basecbcl_q83_p ~~ q83cov*twocbcl_q83_p
basecbcl_q84_p ~~ q84cov*twocbcl_q84_p
basecbcl_q85_p ~~ q85cov*twocbcl_q85_p
basecbcl_q86_p ~~ q86cov*twocbcl_q86_p
basecbcl_q87_p ~~ q87cov*twocbcl_q87_p
basecbcl_q88_p ~~ q88cov*twocbcl_q88_p
basecbcl_q89_p ~~ q89cov*twocbcl_q89_p
basecbcl_q90_p ~~ q90cov*twocbcl_q90_p
basecbcl_q91_p ~~ q91cov*twocbcl_q91_p
basecbcl_q92_p ~~ q92cov*twocbcl_q92_p
basecbcl_q93_p ~~ q93cov*twocbcl_q93_p
basecbcl_q94_p ~~ q94cov*twocbcl_q94_p
basecbcl_q95_p ~~ q95cov*twocbcl_q95_p
basecbcl_q96_p ~~ q96cov*twocbcl_q96_p
basecbcl_q98_p ~~ q98cov*twocbcl_q98_p
basecbcl_q100_p ~~ q100cov*twocbcl_q100_p
basecbcl_q102_p ~~ q102cov*twocbcl_q102_p
basecbcl_q103_p ~~ q103cov*twocbcl_q103_p
basecbcl_q104_p ~~ q104cov*twocbcl_q104_p
basecbcl_q107_p ~~ q107cov*twocbcl_q107_p
basecbcl_q108_p ~~ q108cov*twocbcl_q108_p
basecbcl_q109_p ~~ q109cov*twocbcl_q109_p
basecbcl_q110_p ~~ q110cov*twocbcl_q110_p
basecbcl_q111_p ~~ q111cov*twocbcl_q111_p
basecbcl_q112_p ~~ q112cov*twocbcl_q112_p
baseAttacksThreatens ~~ q113cov*twoAttacksThreatens
baseDestroys ~~ q114cov*twoDestroys
baseDisobeysrules ~~ q115cov*twoDisobeysrules
baseSteals ~~ q116cov*twoSteals
basePeerProblems ~~ q117cov*twoPeerProblems
baseDistractedHyperactive ~~ q118cov*twoDistractedHyperactive
baseHallucinations ~~ q119cov*twoHallucinations
baseSexPlay ~~ q120cov*twoSexPlay
baseWeightproblems ~~ q121cov*twoWeightproblems
'
print("ERROR STRUCTURE DONE")