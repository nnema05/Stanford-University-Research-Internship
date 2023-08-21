
##########IGNORE FOR NOW


####STEP 0: INSTALL LIBRARIES
library(tidyverse)
library(psych)
library(GPArotation)
library(dplyr)
library(ggplot2)
library(corrplot) #plotting correlation matrices
library(lavaan)  #for fitting structural equation models
library(semPlot)  #for automatically making diagrams
library(sqldf)
library(writexl)
library(fastDummies)
library(recipes)
library(psychTools)
library(rmarkdown)
library(knitr)

#####STEP 1: READ DATA
#Read Script and upload


#abcd_bisbas01 <- read.delim("/PATH//abcd_bisbas01.txt") 
abcd_fes01 <- read.delim("/PATH/abcd_fes01.txt")
#abcd_ksad01.txt 
#abcd_ppdms01.txt
abcd_sscep01 <- read.delim("/PATH//sscep01.txt")  
#abcd_upps01.txt
#fhxp102.txt
pdem02 <- read.delim("/PATH//abcd_sscey01.txt/dem02.txt")
abcd_bpmt01 <- read.delim("/PATH//abcd_bpmt01.txt")
abcd_cbcls01 <- read.delim("/PATH//abcd_cbcls01.txt")
#abcd_fhxssp01.txt
abcd_mhy02 <- read.delim("/PATH//abcd_mhy02.txt") 
#abcd_psb01.txt
abcd_sscey01 <- read.delim("/PATH//abcd_sscey01.txt ") 
#acspsw03.txt 
#fhxp201.txt


####STEP 2: SELECT NEEDED MEASURES:
#abcd_bisbas01 <- abcd_bisbas01 %>% select()
abcd_fes01 <- abcd_fes01 %>% 
  select(subjectkey
         , sex)
#abcd_ksad01 <- abcd_ksad01 %>% select()
#abcd_ppdms01 <- abcd_ppdms01 %>% select()
abcd_sscep01 <- abcd_sscep01 %>% 
  select(subkectkey
         , Psb_p_ss_mean
         , fes_p_ss_fc_pr)
#abcd_upps01 <- abcd_upps01 %>% select()
#fhxp102 <- fhxp102.txt %>% select()
pdem02 <- pdem02 %>% 
  select(subjectkey
         , demo_prnt_income_v2
         , demo_prnt_marital_v2
         , demo_brthdat_v2)
abcd_bpmt01 <- abcd_bpmt01 %>% 
  select(subjectkey
         , bpm_t_scr_internal_t
         , bpm_t_scr_attention_t
         , bpm_t_scr_external_t)
abcd_cbcls01 <- abcd_cbcls01 %>% 
  select(subjectkey 
         , cbcl_scr_syn_anxdep_r
         , cbcl_scr_syn_withdep_r
         , cbcl_scr_syn_somatic_r
         , cbcl_scr_syn_social_r
         , cbcl_scr_syn_thought_r
         , cbcl_scr_syn_attention_r
         , cbcl_scr_syn_rulebreak_r
         , cbcl_scr_syn_aggressive_r)
#abcd_fhxssp01 <- abcd_fhxssp01 %>% select()
abcd_mhy02 <- abcd_mhy02 %>%
  select(subjectkey
         , pps_y_ss_severity_score 
         , bis_y_ss_bis_sum
         , bis_y_ss_bas_drive
         , bis_y_ss_bas_fs
         , upps_y_ss_negative_urgency
         , upps_y_ss_positive_urgency
         , upps_y_ss_lack_of_planning
         , upps_y_ss_lack_of_perseverance
         , upps_y_ss_sensation_seeking)
#abcd_psb01 <- abcd_psb01 %>% select()
abcd_sscey01 <- abcd_sscey01 %>% 
  select(subjectkey
    , srpf_y_ss_ses
    , srpf_y_ss_iiss
    , srpf_y_ss_dfs
    , psb_y_ss_mean
    , fes_y_ss_fc_pr
    , pmq_y_ss_mean)
#acspsw03 <- acspsw03 %>% select()
#fhxp201 <- fhxp201 %>% select()


####STEP 3: DIFF ONE COMBINE NEEDED COLUMNS FROM DIFFEENT FILES BASED ON SUBJECT KEY
#DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, abcd_bisbas01, by = "subjectkey",keep = FALSE)
DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, abcd_fes01, 
                                 by = c("basesubjectkey" = "subjectkey"),
                                 keep = FALSE)
#DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, abcd_ksad01,  by = c("basesubjectkey" = "subjectkey"),keep = FALSE)
#DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, abcd_ppdms01,  by = c("basesubjectkey" = "subjectkey"),keep = FALSE)
DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, abcd_sscep01, 
                                 by = c("basesubjectkey" = "subjectkey"),
                                 keep = FALSE)
#DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, abcd_upps01,  by = c("basesubjectkey" = "subjectkey"),keep = FALSE)
#DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, fhxp102,  by = c("basesubjectkey" = "subjectkey") ,keep = FALSE)
DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, pdem02, 
                                    by = c("basesubjectkey" = "subjectkey"),
                                 keep = FALSE)
DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, abcd_bpmt01, 
                                    by = c("basesubjectkey" = "subjectkey"),
                                 keep = FALSE)
DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, abcd_cbcls01, 
                                    by = c("basesubjectkey" = "subjectkey"),
                                 keep = FALSE)
#DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, abcd_fhxssp01, by = c("basesubjectkey" = "subjectkey"), keep = FALSE)
DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, abcd_mhy02, 
                                    by = c("basesubjectkey" = "subjectkey"),
                                 keep = FALSE)
#DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, abcd_psb01,  by = c("basesubjectkey" = "subjectkey") ,keep = FALSE)
DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, abcd_sscey01, 
                                    by = c("basesubjectkey" = "subjectkey"),
                                 keep = FALSE)
#DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, acspsw03, by = c("basesubjectkey" = "subjectkey")",keep = FALSE)
#DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, fhxp201,  by = c("basesubjectkey" = "subjectkey"),keep = FALSE)


####WRITE INTO CSV
write.csv(DiffOneScoresPhenotype, "DiffOneScoresPhenotype.csv")

####STEP 4: DIFF TWO COMBINE NEEDED COLUMNS FROM DIFFEENT FILES BASED ON SUBJECT KEY
#DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, abcd_bisbas01, by = c("basesubjectkey" = "subjectkey") ,keep = FALSE)
DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, abcd_fes01, 
                                    by = "subjectkey",
                                    keep = FALSE)
#DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, abcd_ksad01, by = c("basesubjectkey" = "subjectkey"),keep = FALSE)
#DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, abcd_ppdms01, by = c("basesubjectkey" = "subjectkey"),keep = FALSE)
DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, abcd_sscep01, 
                                    by = "subjectkey",
                                    keep = FALSE)
#DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, abcd_upps01, by = c("basesubjectkey" = "subjectkey"),keep = FALSE)
#DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, fhxp102, by = c("basesubjectkey" = "subjectkey"),keep = FALSE)
DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, pdem02, 
                                    by = c("basesubjectkey" = "subjectkey"),
                                    keep = FALSE)
DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, abcd_bpmt01, 
                                    by = c("basesubjectkey" = "subjectkey"),
                                    keep = FALSE)
DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, abcd_cbcls01, 
                                    by = c("basesubjectkey" = "subjectkey"),
                                    keep = FALSE)
#DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, abcd_fhxssp01, by = c("basesubjectkey" = "subjectkey"),keep = FALSE)
DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, abcd_mhy02, 
                                    by = c("basesubjectkey" = "subjectkey"),
                                    keep = FALSE)
#DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, abcd_psb01, by = c("basesubjectkey" = "subjectkey"),keep = FALSE)
DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, abcd_sscey01, 
                                    by = c("basesubjectkey" = "subjectkey"),
                                    keep = FALSE)
#DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, acspsw03, by = c("basesubjectkey" = "subjectkey"),keep = FALSE)
#DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, fhxp201, by = c("basesubjectkey" = "subjectkey"),keep = FALSE)


####WRITE INTO CSV
write.csv(DiffTwoScoresPhenotype, "DiffTwoScoresPhenotype.csv")