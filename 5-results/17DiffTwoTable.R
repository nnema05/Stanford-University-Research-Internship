
### Creating a final result of p-values table
## This program, using SQL, works to create a final table that will hold all p-values from regression called 17DiffTwoRegression.csv
  ## This p-values showing the association between phenotypes and Difference in TWO year and baseline scores 
  ###.. for factors: p-factor, internalizing factor (factor 1) and externalizing factor (factor 2)

####CREATING DATA FOR DIFF TWO

#####STEP 0: LIBRARIES
library(tidyverse)
library(sqldf)

######STEP 1: READ IN REGRESSION P-VALUE DATA
TwoNewRes <- read.csv("/PATH/16TwoNewRes.csv")
TwoNoKSADSRes <- read.csv("/PATH/13TwoNoKSADSRes.csv")
TwoKSADSRes <- read.csv("/PATH/14TwoKSADSRes.csv")
TwoDemoRes <- read.csv("/PATH/15TwoDemoRes.csv")

####STEP 2: SELECT FOR NEW PHENOTYPES 
##P-factor
pfact <- sqldf("select
header,phenotype,FDR_std_pvalue,std_beta
,CASE WHEN FDR_std_pvalue < 0.001  and std_beta >= 0.2  THEN std_beta_s||'**'||' ('||std_lower_ci||'-'||std_upper_ci||')' 
      WHEN FDR_std_pvalue < 0.001 OR std_beta >= 0.2  THEN std_beta_s||'*'||' ('||std_lower_ci||'-'||std_upper_ci||')'
 ELSE std_beta_s||' ('||std_lower_ci||'-'||std_upper_ci||')' END DiffTwop
from (
SELECT 
  CASE WHEN phenotype LIKE 'gender%' THEN 'Sex'
            WHEN phenotype LIKE 'race%' THEN 'Race/Ethnicity'
            WHEN phenotype LIKE 'educ%' THEN 'Highest parent education'
            WHEN phenotype LIKE 'marital%' THEN 'Parent marital status'
            WHEN phenotype LIKE 'income%' THEN 'Household income' END header
  , CASE WHEN phenotype=='gender_dummy' THEN 'Female'
            WHEN phenotype=='race_dummy_1' THEN 'Hispanic'
            WHEN phenotype=='race_dummy_2' THEN 'Black' 
            WHEN phenotype=='race_dummy_3' THEN 'Other' 
            WHEN phenotype=='race_dummy_4' THEN 'Asian' 
            WHEN phenotype=='educ_dummy_1' THEN 'College degree'
            WHEN phenotype=='educ_dummy_2' THEN 'Masters or Professional degree'
            WHEN phenotype=='educ_dummy_3' THEN 'Some college' 
            WHEN phenotype=='educ_dummy_4' THEN 'Associates or occupational'
            WHEN phenotype=='educ_dummy_5' THEN 'HS or equivalent'
            WHEN phenotype=='marital_dummy' THEN 'Unmarried'
            WHEN phenotype=='income_dummy_1' THEN '<$50 000'
            WHEN phenotype=='income_dummy_2' THEN '$50 000-$100 000' END phenotype 
  , cast(FDR_std_pvalue as double) FDR_std_pvalue
  ,ROUND(cast(std_beta as double),2) std_beta
   ,CAST(ROUND(std_beta, 2) AS STRING) std_beta_s
  ,CAST(ROUND(std_lower_ci, 2) AS STRING) as std_lower_ci
  ,CAST(ROUND(std_upper_ci,2) AS STRING) as std_upper_ci 
FROM TwoNewRes 
WHERE dependent in ('DiffTwoPFactor'))")

#Factor 1 
fact1 <- sqldf("select
header,phenotype,FDR_std_pvalue,std_beta
,CASE WHEN FDR_std_pvalue < 0.001  and std_beta >= 0.2  THEN std_beta_s||'**'||' ('||std_lower_ci||'-'||std_upper_ci||')' 
      WHEN FDR_std_pvalue < 0.001 OR std_beta >= 0.2  THEN std_beta_s||'*'||' ('||std_lower_ci||'-'||std_upper_ci||')'
 ELSE std_beta_s||' ('||std_lower_ci||'-'||std_upper_ci||')' END DiffTwoEXT
from (
SELECT 
  CASE WHEN phenotype LIKE 'gender%' THEN 'Sex'
            WHEN phenotype LIKE 'race%' THEN 'Race/Ethnicity'
            WHEN phenotype LIKE 'educ%' THEN 'Highest parent education'
            WHEN phenotype LIKE 'marital%' THEN 'Parent marital status'
            WHEN phenotype LIKE 'income%' THEN 'Household income' END header
  , CASE WHEN phenotype=='gender_dummy' THEN 'Female'
            WHEN phenotype=='race_dummy_1' THEN 'Hispanic'
            WHEN phenotype=='race_dummy_2' THEN 'Black' 
            WHEN phenotype=='race_dummy_3' THEN 'Other' 
            WHEN phenotype=='race_dummy_4' THEN 'Asian' 
            WHEN phenotype=='educ_dummy_1' THEN 'College degree'
            WHEN phenotype=='educ_dummy_2' THEN 'Masters or Professional degree'
            WHEN phenotype=='educ_dummy_3' THEN 'Some college' 
            WHEN phenotype=='educ_dummy_4' THEN 'Associates or occupational'
            WHEN phenotype=='educ_dummy_5' THEN 'HS or equivalent'
            WHEN phenotype=='marital_dummy' THEN 'Unmarried'
            WHEN phenotype=='income_dummy_1' THEN '<$50 000'
            WHEN phenotype=='income_dummy_2' THEN '$50 000-$100 000' END phenotype 
  , cast(FDR_std_pvalue as double) FDR_std_pvalue
  ,ROUND(cast(std_beta as double),2) std_beta
   ,CAST(ROUND(std_beta, 2) AS STRING) std_beta_s
  ,CAST(ROUND(std_lower_ci, 2) AS STRING) as std_lower_ci
  ,CAST(ROUND(std_upper_ci,2) AS STRING) as std_upper_ci  
FROM TwoNewRes 
WHERE dependent in ('DiffTwoFactor1'))")

#factor 2
fact2 <- sqldf("select
header,phenotype,FDR_std_pvalue,std_beta
,CASE WHEN FDR_std_pvalue < 0.001  and std_beta >= 0.2  THEN std_beta_s||'**'||' ('||std_lower_ci||'-'||std_upper_ci||')' 
      WHEN FDR_std_pvalue < 0.001 OR std_beta >= 0.2  THEN std_beta_s||'*'||' ('||std_lower_ci||'-'||std_upper_ci||')'
 ELSE std_beta_s||' ('||std_lower_ci||'-'||std_upper_ci||')' END DiffTwoINT
from (
SELECT 
  CASE WHEN phenotype LIKE 'gender%' THEN 'Sex'
            WHEN phenotype LIKE 'race%' THEN 'Race/Ethnicity'
            WHEN phenotype LIKE 'educ%' THEN 'Highest parent education'
            WHEN phenotype LIKE 'marital%' THEN 'Parent marital status'
            WHEN phenotype LIKE 'income%' THEN 'Household income' END header
  , CASE WHEN phenotype=='gender_dummy' THEN 'Female'
            WHEN phenotype=='race_dummy_1' THEN 'Hispanic'
            WHEN phenotype=='race_dummy_2' THEN 'Black' 
            WHEN phenotype=='race_dummy_3' THEN 'Other' 
            WHEN phenotype=='race_dummy_4' THEN 'Asian' 
            WHEN phenotype=='educ_dummy_1' THEN 'College degree'
            WHEN phenotype=='educ_dummy_2' THEN 'Masters or Professional degree'
            WHEN phenotype=='educ_dummy_3' THEN 'Some college' 
            WHEN phenotype=='educ_dummy_4' THEN 'Associates or occupational'
            WHEN phenotype=='educ_dummy_5' THEN 'HS or equivalent'
            WHEN phenotype=='marital_dummy' THEN 'Unmarried'
            WHEN phenotype=='income_dummy_1' THEN '<$50 000'
            WHEN phenotype=='income_dummy_2' THEN '$50 000-$100 000' END phenotype 
  , cast(FDR_std_pvalue as double) FDR_std_pvalue
  ,ROUND(cast(std_beta as double),2) std_beta
   ,CAST(ROUND(std_beta, 2) AS STRING) std_beta_s
  ,CAST(ROUND(std_lower_ci, 2) AS STRING) as std_lower_ci
  ,CAST(ROUND(std_upper_ci,2) AS STRING) as std_upper_ci 
FROM TwoNewRes 
WHERE dependent in ('DiffTwoFactor2'))")

NEWcompare <- merge(pfact, fact1, by = c("phenotype", "header"))
NEWcompare <- merge(NEWcompare, fact2, by = c("phenotype", "header"))
NEWcompare <- NEWcompare %>% select(header, phenotype, DiffTwop, DiffTwoINT, DiffTwoEXT)
NEWcompare <- NEWcompare %>% arrange(header)
write.csv(NEWcompare, "17NEWDiffTwo.csv")
            

#, CASE WHEN dependent=='twoPFactor' THEN 'DiffTwoPFactor'
#  WHEN dependent=='twoFactor1' THEN 'DiffTwoEXT'
#  WHEN dependent=='twoFactor2' THEN 'DiffTwoINT' END DiffTwop


#####STEP 3: SELECT FOR DEMO PHENOTYPES 
##P-factor
pfact <- sqldf("select
header, phenotype, FDR_std_pvalue, std_beta
 ,CASE WHEN FDR_std_pvalue < 0.001  and std_beta >= 0.2  THEN std_beta_s||'**'||' ('||std_lower_ci||'-'||std_upper_ci||')' 
      WHEN FDR_std_pvalue < 0.001 OR std_beta >= 0.2  THEN std_beta_s||'*'||' ('||std_lower_ci||'-'||std_upper_ci||')'
 ELSE std_beta_s||' ('||std_lower_ci||'-'||std_upper_ci||')' END DiffTwop
from (
SELECT 
    CASE WHEN UPPER(phenotype) LIKE 'fh%' THEN 'Household income'
         WHEN phenotype LIKE 'adi%' THEN 'Household income'  END header
  , CASE WHEN phenotype=='fh_parent_alcdrug_binary' THEN 'Family History of psychopathology'
            WHEN phenotype=='FHtotal' THEN 'Family History of substance use problems'
            WHEN phenotype=='adi_weightavg_kind' THEN 'Area deprivation index' END phenotype
  , cast(FDR_std_pvalue as double) FDR_std_pvalue
  ,ROUND(cast(std_beta as double),2) std_beta
   ,CAST(ROUND(std_beta, 2) AS STRING) std_beta_s
  ,CAST(ROUND(std_lower_ci, 2) AS STRING) as std_lower_ci
  ,CAST(ROUND(std_upper_ci,2) AS STRING) as std_upper_ci 
FROM TwoDemoRes 
WHERE dependent in ('DiffTwoPFactor'))")

#Factor 1 
fact1 <- sqldf("select
header, phenotype,FDR_std_pvalue,std_beta
,CASE WHEN FDR_std_pvalue < 0.001  and std_beta >= 0.2  THEN std_beta_s||'**'||' ('||std_lower_ci||'-'||std_upper_ci||')' 
      WHEN FDR_std_pvalue < 0.001 OR std_beta >= 0.2  THEN std_beta_s||'*'||' ('||std_lower_ci||'-'||std_upper_ci||')'
 ELSE std_beta_s||' ('||std_lower_ci||'-'||std_upper_ci||')' END DiffTwoEXT
from (
SELECT 
    CASE WHEN UPPER(phenotype) LIKE 'fh%' THEN 'Household income'
         WHEN phenotype LIKE 'adi%' THEN 'Household income'  END header
  , CASE WHEN phenotype=='fh_parent_alcdrug_binary' THEN 'Family History of psychopathology'
            WHEN phenotype=='FHtotal' THEN 'Family History of substance use problems'
            WHEN phenotype=='adi_weightavg_kind' THEN 'Area deprivation index' END phenotype
  , cast(FDR_std_pvalue as double) FDR_std_pvalue
  ,ROUND(cast(std_beta as double),2) std_beta
   ,CAST(ROUND(std_beta, 2) AS STRING) std_beta_s
  ,CAST(ROUND(std_lower_ci, 2) AS STRING) as std_lower_ci
  ,CAST(ROUND(std_upper_ci,2) AS STRING) as std_upper_ci 
FROM TwoDemoRes
WHERE dependent in ('DiffTwoFactor1'))")

#factor 2
fact2 <- sqldf("select
header, phenotype,FDR_std_pvalue,std_beta
,CASE WHEN FDR_std_pvalue < 0.001  and std_beta >= 0.2  THEN std_beta_s||'**'||' ('||std_lower_ci||'-'||std_upper_ci||')' 
      WHEN FDR_std_pvalue < 0.001 OR std_beta >= 0.2  THEN std_beta_s||'*'||' ('||std_lower_ci||'-'||std_upper_ci||')'
 ELSE std_beta_s||' ('||std_lower_ci||'-'||std_upper_ci||')' END DiffTwoINT
from (
SELECT 
    CASE WHEN UPPER(phenotype) LIKE 'fh%' THEN 'Household income'
         WHEN phenotype LIKE 'adi%' THEN 'Household income'  END header
  , CASE WHEN phenotype=='fh_parent_alcdrug_binary' THEN 'Family History of psychopathology'
            WHEN phenotype=='FHtotal' THEN 'Family History of substance use problems'
            WHEN phenotype=='adi_weightavg_kind' THEN 'Area deprivation index' END phenotype
  , cast(FDR_std_pvalue as double) FDR_std_pvalue
  ,ROUND(cast(std_beta as double),2) std_beta
   ,CAST(ROUND(std_beta, 2) AS STRING) std_beta_s
  ,CAST(ROUND(std_lower_ci, 2) AS STRING) as std_lower_ci
  ,CAST(ROUND(std_upper_ci,2) AS STRING) as std_upper_ci 
FROM TwoDemoRes
WHERE dependent in ('DiffTwoFactor2'))")

DEMOcompare <- merge(pfact, fact1, by = c("phenotype", "header"))
DEMOcompare <- merge(DEMOcompare, fact2, by = c("phenotype", "header"))
DEMOcompare <- DEMOcompare %>% select(header, phenotype, DiffTwop, DiffTwoINT, DiffTwoEXT)
DEMOcompare <- DEMOcompare %>% arrange(header)
write.csv(DEMOcompare, "17DEMODiffTwo.csv")



####STEP 4: SELECT FOR KSAD PHENOTYPES 
##P-factor
pfact <- sqldf("select
header,phenotype,FDR_std_pvalue,std_beta
,CASE WHEN FDR_std_pvalue < 0.001  and std_beta >= 0.2  THEN std_beta_s||'**'||' ('||std_lower_ci||'-'||std_upper_ci||')' 
      WHEN FDR_std_pvalue < 0.001 OR std_beta >= 0.2  THEN std_beta_s||'*'||' ('||std_lower_ci||'-'||std_upper_ci||')'
 ELSE std_beta_s||' ('||std_lower_ci||'-'||std_upper_ci||')' END DiffTwop
from (
SELECT 
  CASE WHEN UPPER(phenotype) LIKE 'ANY%' THEN 'KSAD'
  ELSE phenotype END header
  , CASE WHEN phenotype=='Any_DepDx' THEN 'Depression'
            WHEN phenotype=='ANY_SUISH' THEN 'Suicide/Self-harm behaviors'
            WHEN phenotype=='ANY_GAD' THEN 'Generalized Anxiety' 
            WHEN phenotype=='Any_SocAnx' THEN 'Social Anxiety' 
            WHEN phenotype=='Any_SepAnx' THEN 'Separation Anxiety' 
            WHEN phenotype=='Any_Phobia' THEN 'Specific Phobia'
            WHEN phenotype=='Any_PanicAgorDx' THEN 'Panic/Agoraphobia'
            WHEN phenotype=='Any_BipolarDx' THEN 'Bipolar' 
            WHEN phenotype=='Any_ADHD' THEN 'ADHD'
            WHEN phenotype=='Any_CD' THEN 'Conduct Disorder'
            WHEN phenotype=='Any_ODD' THEN 'ODD' END phenotype 
  , cast(FDR_std_pvalue as double) FDR_std_pvalue
  ,ROUND(cast(std_beta as double),2) std_beta
   ,CAST(ROUND(std_beta, 2) AS STRING) std_beta_s
  ,CAST(ROUND(std_lower_ci, 2) AS STRING) as std_lower_ci
  ,CAST(ROUND(std_upper_ci,2) AS STRING) as std_upper_ci  
FROM TwoKSADSRes 
WHERE dependent in ('DiffTwoPFactor'))")

#Factor 1 
fact1 <- sqldf("select
header,phenotype,FDR_std_pvalue,std_beta
,CASE WHEN FDR_std_pvalue < 0.001  and std_beta >= 0.2  THEN std_beta_s||'**'||' ('||std_lower_ci||'-'||std_upper_ci||')' 
      WHEN FDR_std_pvalue < 0.001 OR std_beta >= 0.2  THEN std_beta_s||'*'||' ('||std_lower_ci||'-'||std_upper_ci||')'
 ELSE std_beta_s||' ('||std_lower_ci||'-'||std_upper_ci||')' END DiffTwoEXT
from (
SELECT 
  CASE WHEN UPPER(phenotype) LIKE 'ANY%' THEN 'KSAD'
  ELSE phenotype END header
  , CASE WHEN phenotype=='Any_DepDx' THEN 'Depression'
            WHEN phenotype=='ANY_SUISH' THEN 'Suicide/Self-harm behaviors'
            WHEN phenotype=='ANY_GAD' THEN 'Generalized Anxiety' 
            WHEN phenotype=='Any_SocAnx' THEN 'Social Anxiety' 
            WHEN phenotype=='Any_SepAnx' THEN 'Seperation Anxiety' 
            WHEN phenotype=='Any_Phobia' THEN 'Specific Phobia'
            WHEN phenotype=='Any_PanicAgorDx' THEN 'Panic/Agoraphobia'
            WHEN phenotype=='Any_BipolarDx' THEN 'Bipolar' 
            WHEN phenotype=='Any_ADHD' THEN 'ADHD'
            WHEN phenotype=='Any_CD' THEN 'Conduct Disorder'
            WHEN phenotype=='Any_ODD' THEN 'ODD' END phenotype 
  , cast(FDR_std_pvalue as double) FDR_std_pvalue
  ,ROUND(cast(std_beta as double),2) std_beta
   ,CAST(ROUND(std_beta, 2) AS STRING) std_beta_s
  ,CAST(ROUND(std_lower_ci, 2) AS STRING) as std_lower_ci
  ,CAST(ROUND(std_upper_ci,2) AS STRING) as std_upper_ci 
FROM TwoKSADSRes
WHERE dependent in ('DiffTwoFactor1'))")

#factor 2
fact2 <- sqldf("select
header,phenotype,FDR_std_pvalue,std_beta
,CASE WHEN FDR_std_pvalue < 0.001  and std_beta >= 0.2  THEN std_beta_s||'**'||' ('||std_lower_ci||'-'||std_upper_ci||')' 
      WHEN FDR_std_pvalue < 0.001 OR std_beta >= 0.2  THEN std_beta_s||'*'||' ('||std_lower_ci||'-'||std_upper_ci||')'
 ELSE std_beta_s||' ('||std_lower_ci||'-'||std_upper_ci||')' END DiffTwoINT
from (
SELECT 
  CASE WHEN UPPER(phenotype) LIKE 'ANY%' THEN 'KSAD'
  ELSE phenotype END header
  , CASE WHEN phenotype=='Any_DepDx' THEN 'Depression'
            WHEN phenotype=='ANY_SUISH' THEN 'Suicide/Self-harm behaviors'
            WHEN phenotype=='ANY_GAD' THEN 'Generalized Anxiety' 
            WHEN phenotype=='Any_SocAnx' THEN 'Social Anxiety' 
            WHEN phenotype=='Any_SepAnx' THEN 'Seperation Anxiety' 
            WHEN phenotype=='Any_Phobia' THEN 'Specific Phobia'
            WHEN phenotype=='Any_PanicAgorDx' THEN 'Panic/Agoraphobia'
            WHEN phenotype=='Any_BipolarDx' THEN 'Bipolar' 
            WHEN phenotype=='Any_ADHD' THEN 'ADHD'
            WHEN phenotype=='Any_CD' THEN 'Conduct Disorder'
            WHEN phenotype=='Any_ODD' THEN 'ODD' END phenotype 
  , cast(FDR_std_pvalue as double) FDR_std_pvalue
  ,ROUND(cast(std_beta as double),2) std_beta
   ,CAST(ROUND(std_beta, 2) AS STRING) std_beta_s
  ,CAST(ROUND(std_lower_ci, 2) AS STRING) as std_lower_ci
  ,CAST(ROUND(std_upper_ci,2) AS STRING) as std_upper_ci  
FROM TwoKSADSRes 
WHERE dependent in ('DiffTwoFactor2'))")

KSADScompare <- merge(pfact, fact1, by = c("phenotype", "header"))
KSADScompare <- merge(KSADScompare, fact2, by = c("phenotype", "header"))
KSADScompare <- KSADScompare %>% select(header, phenotype, DiffTwop, DiffTwoINT, DiffTwoEXT)
KSADScompare <- KSADScompare %>% arrange(header)
write.csv(KSADScompare, "17KSADSDiffTwo.csv")



####STEP 5: SELECT FOR NOKSADS PHENOTYPES 
##P-factor
pfact <- sqldf("select
header,phenotype,FDR_std_pvalue,std_beta
,CASE WHEN FDR_std_pvalue < 0.001  and std_beta >= 0.2  THEN std_beta_s||'**'||' ('||std_lower_ci||'-'||std_upper_ci||')' 
      WHEN FDR_std_pvalue < 0.001 OR std_beta >= 0.2  THEN std_beta_s||'*'||' ('||std_lower_ci||'-'||std_upper_ci||')'
 ELSE std_beta_s||' ('||std_lower_ci||'-'||std_upper_ci||')' END DiffTwop
from (
SELECT 
  CASE WHEN phenotype LIKE 'pps%' THEN 'KSAD'
            WHEN phenotype LIKE 'bpm%' THEN 'Brief problems monitor'
            WHEN phenotype LIKE 'educ%' THEN 'Highest parent education'
            WHEN phenotype LIKE 'bis%' THEN 'BIS/BAS'
            WHEN phenotype LIKE 'upps%' THEN 'UPPS'
            WHEN phenotype LIKE 'srpf%' THEN 'School risk and protective factors'
            WHEN phenotype LIKE 'psb%' THEN 'Prosocial behavior'
            WHEN phenotype LIKE 'fes%' THEN 'Family environment: conflict'
            WHEN phenotype LIKE 'fes%' THEN 'Family environment: conflict' 
            WHEN phenotype LIKE 'pmq%' THEN 'Family environment: conflict' END header
  , CASE WHEN phenotype=='pps_y_ss_severity_score' THEN 'Prodromal questionnaire'
            WHEN phenotype=='bpm_t_scr_internal_t' THEN 'Internalizing'
            WHEN phenotype=='bpm_t_scr_external_t' THEN 'Externalizing' 
            WHEN phenotype=='bpm_t_scr_attention_t' THEN 'Attention' 
            WHEN phenotype=='bis_y_ss_bis_sum' THEN 'BIS' 
            WHEN phenotype=='bis_y_ss_bas_rr' THEN 'BAS- Reward Response'
            WHEN phenotype=='bis_y_ss_bas_drive' THEN 'BAS- Drive'
            WHEN phenotype=='bis_y_ss_bas_fs' THEN 'BAS- Fun seeking' 
            WHEN phenotype=='upps_y_ss_negative_urgency' THEN 'Negative Urgency'
            WHEN phenotype=='upps_y_ss_positive_urgency' THEN 'Positive Urgency'
            WHEN phenotype=='upps_y_ss_lack_of_planning' THEN 'Lack of planning'
            WHEN phenotype=='upps_y_ss_lack_of_perseverance' THEN 'Lack of perseverance'
            WHEN phenotype=='upps_y_ss_sensation_seeking' THEN 'Sensation seeking' 
            WHEN phenotype=='srpf_y_ss_ses' THEN 'School environment' 
            WHEN phenotype=='srpf_y_ss_iiss' THEN 'School involvement' 
            WHEN phenotype=='srpf_y_ss_dfs' THEN 'School disengagement' 
            WHEN phenotype=='psb_y_ss_mean' THEN 'Youth report' 
            WHEN phenotype=='psb_p_ss_mean' THEN 'Parent report'
            WHEN phenotype=='fes_y_ss_fc_pr' THEN 'Youth report' 
            WHEN phenotype=='fes_p_ss_fc_pr' THEN 'Parent report' 
            WHEN phenotype=='pmq_y_ss_mean' THEN 'Parental monitoring survey' END phenotype 
  , cast(FDR_std_pvalue as double) FDR_std_pvalue
  ,ROUND(cast(std_beta as double),2) std_beta
   ,CAST(ROUND(std_beta, 2) AS STRING) std_beta_s
  ,CAST(ROUND(std_lower_ci, 2) AS STRING) as std_lower_ci
  ,CAST(ROUND(std_upper_ci,2) AS STRING) as std_upper_ci 
FROM TwoNoKSADSRes
WHERE dependent in ('DiffTwoPFactor'))")

#Factor 1 
fact1 <- sqldf("select
header,phenotype,FDR_std_pvalue,std_beta
,CASE WHEN FDR_std_pvalue < 0.001  and std_beta >= 0.2  THEN std_beta_s||'**'||' ('||std_lower_ci||'-'||std_upper_ci||')' 
      WHEN FDR_std_pvalue < 0.001 OR std_beta >= 0.2  THEN std_beta_s||'*'||' ('||std_lower_ci||'-'||std_upper_ci||')'
 ELSE std_beta_s||' ('||std_lower_ci||'-'||std_upper_ci||')' END DiffTwoEXT
from (
SELECT 
  CASE WHEN phenotype LIKE 'pps%' THEN 'KSADS'
            WHEN phenotype LIKE 'bpm%' THEN 'Brief problems monitor'
            WHEN phenotype LIKE 'educ%' THEN 'Highest parent education'
            WHEN phenotype LIKE 'bis%' THEN 'BIS/BAS'
            WHEN phenotype LIKE 'upps%' THEN 'UPPS'
            WHEN phenotype LIKE 'srpf%' THEN 'School risk and protective factors'
            WHEN phenotype LIKE 'psb%' THEN 'Prosocial behavior'
            WHEN phenotype LIKE 'fes%' THEN 'Family environment: conflict'
            WHEN phenotype LIKE 'fes%' THEN 'Family environment: conflict' 
            WHEN phenotype LIKE 'pmq%' THEN 'Family environment: conflict' END header
  , CASE WHEN phenotype=='pps_y_ss_severity_score' THEN 'Prodromal questionnaire'
            WHEN phenotype=='bpm_t_scr_internal_t' THEN 'Internalizing'
            WHEN phenotype=='bpm_t_scr_external_t' THEN 'Externalizing' 
            WHEN phenotype=='bpm_t_scr_attention_t' THEN 'Attention' 
            WHEN phenotype=='bis_y_ss_bis_sum' THEN 'BIS' 
            WHEN phenotype=='bis_y_ss_bas_rr' THEN 'BAS- Reward Response'
            WHEN phenotype=='bis_y_ss_bas_drive' THEN 'BAS- Drive'
            WHEN phenotype=='bis_y_ss_bas_fs' THEN 'BAS- Fun seeking' 
            WHEN phenotype=='upps_y_ss_negative_urgency' THEN 'Negative Urgency'
            WHEN phenotype=='upps_y_ss_positive_urgency' THEN 'Positive Urgency'
            WHEN phenotype=='upps_y_ss_lack_of_planning' THEN 'Lack of planning'
            WHEN phenotype=='upps_y_ss_lack_of_perseverance' THEN 'Lack of perseverance'
            WHEN phenotype=='upps_y_ss_sensation_seeking' THEN 'Sensation seeking' 
            WHEN phenotype=='srpf_y_ss_ses' THEN 'School environment' 
            WHEN phenotype=='srpf_y_ss_iiss' THEN 'School involvement' 
            WHEN phenotype=='srpf_y_ss_dfs' THEN 'School disengagement' 
            WHEN phenotype=='psb_y_ss_mean' THEN 'Youth report' 
            WHEN phenotype=='psb_p_ss_mean' THEN 'Parent report'
            WHEN phenotype=='fes_y_ss_fc_pr' THEN 'Youth report' 
            WHEN phenotype=='fes_p_ss_fc_pr' THEN 'Parent report' 
            WHEN phenotype=='pmq_y_ss_mean' THEN 'Parental monitoring survey' END phenotype 
  , cast(FDR_std_pvalue as double) FDR_std_pvalue
  ,ROUND(cast(std_beta as double),2) std_beta
   ,CAST(ROUND(std_beta, 2) AS STRING) std_beta_s
  ,CAST(ROUND(std_lower_ci, 2) AS STRING) as std_lower_ci
  ,CAST(ROUND(std_upper_ci,2) AS STRING) as std_upper_ci 
FROM TwoNoKSADSRes
WHERE dependent in ('DiffTwoFactor1'))")

#factor 2
fact2 <- sqldf("select
header,phenotype,FDR_std_pvalue,std_beta
,CASE WHEN FDR_std_pvalue < 0.001  and std_beta >= 0.2  THEN std_beta_s||'**'||' ('||std_lower_ci||'-'||std_upper_ci||')' 
      WHEN FDR_std_pvalue < 0.001 OR std_beta >= 0.2  THEN std_beta_s||'*'||' ('||std_lower_ci||'-'||std_upper_ci||')'
 ELSE std_beta_s||' ('||std_lower_ci||'-'||std_upper_ci||')' END DiffTwoINT
from (
SELECT 
  CASE WHEN phenotype LIKE 'pps%' THEN 'KSADS'
            WHEN phenotype LIKE 'bpm%' THEN 'Brief problems monitor'
            WHEN phenotype LIKE 'educ%' THEN 'Highest parent education'
            WHEN phenotype LIKE 'bis%' THEN 'BIS/BAS'
            WHEN phenotype LIKE 'upps%' THEN 'UPPS'
            WHEN phenotype LIKE 'srpf%' THEN 'School risk and protective factors'
            WHEN phenotype LIKE 'psb%' THEN 'Prosocial behavior'
            WHEN phenotype LIKE 'fes%' THEN 'Family environment: conflict'
            WHEN phenotype LIKE 'fes%' THEN 'Family environment: conflict' 
            WHEN phenotype LIKE 'pmq%' THEN 'Family environment: conflict' END header
  , CASE WHEN phenotype=='pps_y_ss_severity_score' THEN 'Prodromal questionnaire'
            WHEN phenotype=='bpm_t_scr_internal_t' THEN 'Internalizing'
            WHEN phenotype=='bpm_t_scr_external_t' THEN 'Externalizing' 
            WHEN phenotype=='bpm_t_scr_attention_t' THEN 'Attention' 
            WHEN phenotype=='bis_y_ss_bis_sum' THEN 'BIS' 
            WHEN phenotype=='bis_y_ss_bas_rr' THEN 'BAS- Reward Response'
            WHEN phenotype=='bis_y_ss_bas_drive' THEN 'BAS- Drive'
            WHEN phenotype=='bis_y_ss_bas_fs' THEN 'BAS- Fun seeking' 
            WHEN phenotype=='upps_y_ss_negative_urgency' THEN 'Negative Urgency'
            WHEN phenotype=='upps_y_ss_positive_urgency' THEN 'Positive Urgency'
            WHEN phenotype=='upps_y_ss_lack_of_planning' THEN 'Lack of planning'
            WHEN phenotype=='upps_y_ss_lack_of_perseverance' THEN 'Lack of perseverance'
            WHEN phenotype=='upps_y_ss_sensation_seeking' THEN 'Sensation seeking' 
            WHEN phenotype=='srpf_y_ss_ses' THEN 'School environment' 
            WHEN phenotype=='srpf_y_ss_iiss' THEN 'School involvement' 
            WHEN phenotype=='srpf_y_ss_dfs' THEN 'School disengagement' 
            WHEN phenotype=='psb_y_ss_mean' THEN 'Youth report' 
            WHEN phenotype=='psb_p_ss_mean' THEN 'Parent report'
            WHEN phenotype=='fes_y_ss_fc_pr' THEN 'Youth report' 
            WHEN phenotype=='fes_p_ss_fc_pr' THEN 'Parent report' 
            WHEN phenotype=='pmq_y_ss_mean' THEN 'Parental monitoring survey' END phenotype 
  , cast(FDR_std_pvalue as double) FDR_std_pvalue
  ,ROUND(cast(std_beta as double),2) std_beta
   ,CAST(ROUND(std_beta, 2) AS STRING) std_beta_s
  ,CAST(ROUND(std_lower_ci, 2) AS STRING) as std_lower_ci
  ,CAST(ROUND(std_upper_ci,2) AS STRING) as std_upper_ci 
FROM TwoNoKSADSRes
WHERE dependent in ('DiffTwoFactor2'))")

NOKSADScompare <- merge(pfact, fact1, by = c("phenotype", "header"))
NOKSADScompare <- merge(NOKSADScompare, fact2, by = c("phenotype", "header"))
NOKSADScompare <- NOKSADScompare %>% select(header, phenotype, DiffTwop, DiffTwoINT, DiffTwoEXT)
NOKSADScompare <- NOKSADScompare %>% arrange(header)
write.csv(NOKSADScompare, "17NOKSADSDiffTwo.csv")

## STEP 6: CREATE A FINAL FILE THAT WILL HOLD ALL OF THE P-VALUES FOR ALL OF REGRESSION FOR DIFFERENCE IN TWO YEAR AND BASELINE
#cat 17NEWDiffTwo.csv 17DEMODiffTwo.csv 17KSADSDiffTwo.csv 17NOKSADSDiffTwo.csv > 17DiffTwoRegression.csv
#will become table that you adjust, bold, add headers etc.


