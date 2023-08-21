
## Creating predictor datasets for ALL of the predictors for regression
  ## Predictor datasets cover: Family History,  

### Baseline family history data
## This program is reading in data about family history (FH) from ABCD dataset
  ## Then it is used to to create a final dataset of all the averages of family history 
## This is being done for family history data offered at BASELINE!

####STEP 0: INSTALL LIBRARIES 
library(tidyverse)
library(psych)
library(GPArotation)
library(dplyr)
library(base)
library(naniar)

##### FAMILY HISTORY OF PSYCHOPATHOLOGY 
####### eight categories are depression, hospitalized, mania, nerves, 
####### professional, suicide, trouble, and visions.

####STEP 1: MERGE THE TWO NEEDED FILES FOR FAMILY HISTORY OF PSYCHOPATHOLOGY 
print("Reading data")
fhxp102 <- read.delim("/PATH//fhxp102_d.txt")
fhxp201 <- read.delim("/PATH//fhxp201_d.txt")

# join both files
fhxp <- full_join(fhxp102, fhxp201, by = "subjectkey", keep = FALSE)
#deal with Nas
fhxp <- fhxp %>%
  replace_with_na_all(condition = ~.x == 999)
#make numeric
fhxp[ , 10:1450] <- apply(fhxp[ , 10:1450], 2,
                            function(x) as.numeric(as.character(x))) 

write.csv(fhxp, "fhxp.csv")

fhxp <- fhxp %>% 
  filter(eventname == "baseline_year_1_arm_1")
print("Different timeline")


print("DATA READ AND MERGED ")


########## DEPRESSION
####STEP 2: SELECT NEEDED VARIABLES FOR FIRST DEGREE AND SECOND DEGREE REALTIVES
#first relative is mother, father, full siblings 
firstDep <- fhxp %>%
  select(fam_history_q6a_depression
         , fam_history_q6d_depression
         , q6k_full_sib_young1_depression
         , q6k_full_sib_young2_depression
         , q6k_full_sib_young3_depression
         , q6k_full_sib_young4_depression
         , q6k_full_sib_young5_depression
         , q6l_full_sib_old1_depression
         , q6l_full_sib_old2_depression
         , q6l_full_sib_old3_depression
         , q6l_full_sib_old4_depression
         , q6l_full_sib_old5_depression
         , q6m_full_sib_same1_depression
         , q6m_full_sib_same2_depression)
firstDep$firstDepSum <- rowSums(firstDep, na.rm = TRUE)
#firstDep <- cbind(firstDep, fhxp$subjectkey)
write.csv(firstDep, "firstDep.csv")
head(firstDep)
print("FIRST DEP DONE")

#second degre everyone else includng aunts, uncles, grandparents 
secondDep <- fhxp %>%
  select(fam_history_q6b_depression
         , fam_history_q6c_depression
         , fam_history_q6e_depression
         , fam_history_q6f_depression
         , q6g_pat_uncle1_depression
         , q6g_pat_uncle2_depression
         , q6g_pat_uncle3_depression
         , q6g_pat_uncle4_depression
         , q6g_pat_uncle5_depression
         , q6h_pat_aunt1_depression
         , q6h_pat_aunt2_depression
         , q6h_pat_aunt3_depression
         , q6h_pat_aunt4_depression
         , q6h_pat_aunt5_depression
         , q6i_mat_uncle1_depression
         , q6i_mat_uncle2_depression
         , q6i_mat_uncle3_depression
         , q6i_mat_uncle4_depression
         , q6i_mat_uncle5_depression
         , q6j_mat_aunt1_depression
         , q6j_mat_aunt2_depression
         , q6j_mat_aunt3_depression
         , q6j_mat_aunt4_depression
         , q6j_mat_aunt5_depression
         , q6m_half_sib_young1_depression
         , q6m_half_sib_young2_depression
         , q6m_half_sib_young3_depression
         , q6m_half_sib_young4_depression
         , q6m_half_sib_young5_depression
         , q6n_half_sib_old1_depression
         , q6n_half_sib_old2_depression
         , q6n_half_sib_old3_depression
         , q6n_half_sib_old4_depression
         , q6n_half_sib_old5_depression)
secondDep$secondDepSum <- rowSums(secondDep, na.rm = TRUE)
secondDep$secondDepWeightedSum <- .5*secondDep$secondDepSum
#secondDep <- cbind(secondDep, fhxp$subjectkey)
write.csv(secondDep, "secondDep.csv")
head(secondDep)
print("SECOND DEP DONE")


#####STEP 3: FOR EACH CATEGORY RUN FORUMULA
#first degree + .5(second degree)/total
##Create new data frame for both sum columns and then rowSum them
#Weighted score sum
depSum <- data.frame(firstDep$firstDepSum, secondDep$secondDepWeightedSum)
depSum$depSum <- rowSums(depSum, na.rm = TRUE)
#Total score sum
totalDepSum <- data.frame(firstDep$firstDepSum, secondDep$secondDepSum)
totalDepSum$totalDepSum <- rowSums(totalDepSum, na.rm = TRUE)
#divide weighted score by total score 
depSum$depSumFinal <- (depSum$depSum/totalDepSum$totalDepSum)
#Add subject key
depSum <- cbind(depSum, subjectkey = fhxp$subjectkey)
#head(depSum)
#join to orignal data fhxp
#fhxp <- left_join(fhxp, depSum, by = "subjectkey",keep = FALSE)
write.csv(depSum, "depSum.csv")
print("WEIGHTED SUM SCORE DONE")

#####STEP 4: READ INTO DIFF SCORES
#print("READ INTO DIFF SCORES")
#DiffOneScores <- read.csv("/PATH/DiffOneScores.csv")
#DiffTwoScores <- read.csv("/PATH/DiffTwoScores.csv")
#DiffOneScoresPhenotype <- left_join(DiffOneScores, depSum, by = c("basesubjectkey" = "fhxp$subjectkey"))
#DiffTwoScoresPhenotype <- left_join(DiffTwoScores, depSum, by = c("basesubjectkey" = "fhxp$subjectkey"))
#write.csv(DiffOneScoresPhenotype, "DiffOneScoresPhenotype.csv")
#write.csv(DiffTwoScoresPhenotype, "DiffTwoScoresPhenotype.csv")
#print("READ INTO DIFF SCORES DONE")


########HOSPITALIZED
####STEP 2: SELECT NEEDED VARIABLES FOR FIRST DEGREE AND SECOND DEGREE REALTIVES
#first relative is mother, father, full siblings 
firstHosp <- fhxp %>% 
  select(fam_history_q12d_hospitalized 
         , fam_history_q12d_hospitalized
         , q12k_full_sib_young1_hosp 
         , q12k_full_sib_young2_hosp 
         , q12k_full_sib_young3_hosp
         , q12k_full_sib_young4_hosp
         , q12k_full_sib_young5_hosp
         , q12l_full_sib_old1_hosp
         , q12l_full_sib_old2_hosp 
         , q12l_full_sib_old3_hosp 
         , q12l_full_sib_old4_hosp 
         , q12l_full_sib_old5_hosp
         , q12m_full_sib_same1_hosp 
         , q12m_full_sib_same2_hosp)
firstHosp$firstHospSum <- rowSums(firstHosp, na.rm = TRUE)
#firstHosp <- cbind(firstHosp, fhxp$subjectkey)
write.csv(firstHosp, "firstHosp.csv")
head(firstHosp)
print("FIRST HOSPITALIZED DONE")

#second degre everyone else includng aunts, uncles, grandparents 
secondHosp <- fhxp %>%
  select(fam_history_q12b_hospitalized 
         , fam_history_q12c_hospitalized 
         , fam_history_q12e_hospitalized
         , fam_history_q12f_hospitalized
         , q12g_pat_uncle1_hospitalized
         , q12g_pat_uncle2_hospitalized 
         , q12g_pat_uncle3_hospitalized
         , q12g_pat_uncle4_hospitalized 
         , q12g_pat_uncle5_hospitalized
         , q12h_pat_aunt1_hospitalized 
         , q12h_pat_aunt2_hospitalized 
         , q12h_pat_aunt3_hospitalized
         , q12h_pat_aunt4_hospitalized
         , q12h_pat_aunt5_hospitalized 
         , q12i_mat_uncle1_hospitalized
         , q12i_mat_uncle2_hospitalized
         , q12i_mat_uncle3_hospitalized
         , q12i_mat_uncle4_hospitalized 
         , q12i_mat_uncle5_hospitalized
         , q12j_mat_aunt1_hospitalized 
         , q12j_mat_aunt2_hospitalized
         , q12j_mat_aunt3_hospitalized 
         , q12j_mat_aunt4_hospitalized
         , q12j_mat_aunt5_hospitalized 
         , q12m_half_sib_young1_hosp 
         , q12m_half_sib_young2_hosp 
         , q12m_half_sib_young3_hosp 
         , q12m_half_sib_young4_hosp
         , q12m_half_sib_young5_hosp
         , q12n_half_sib_old1_hosp 
         , q12n_half_sib_old2_hosp 
         , q12n_half_sib_old3_hosp
         , q12n_half_sib_old4_hosp
         , q12n_half_sib_old5_hosp)
secondHosp$secondHospSum <- rowSums(secondHosp, na.rm = TRUE)
secondHosp$secondHospWeightedSum <- .5*secondHosp$secondHospSum
#secondHosp <- cbind(secondHosp, fhxp$subjectkey)
write.csv(secondHosp, "secondHosp.csv")
head(secondHosp)
print("SECOND HOSPITALIZAED DONE")

#####STEP 3: FOR EACH CATEGORY RUN FORUMULA
#first degree + .5(second degree)/total
##Create new data frame for both sum columns and then rowSum them
#Weighted score sum
hospSum <- data.frame(firstHosp$firstHospSum, secondHosp$secondHospWeightedSum)
hospSum$hospSum <- rowSums(hospSum, na.rm = TRUE)
#Total score sum
totalHospSum <- data.frame(firstHosp$firstHospSum, secondHosp$secondHospSum)
totalHospSum$totalHospSum <- rowSums(totalHospSum, na.rm = TRUE)
#divide weighted score by total score 
hospSum$hospSumFinal <- (hospSum$hospSum/totalHospSum$totalHospSum)
#Add subject key
hospSum <- cbind(hospSum, subjectkey = fhxp$subjectkey)
#head(hospSum)
#join to orignal data fhxp
#fhxp <- left_join(fhxp, hospSum, by = "subjectkey",keep = FALSE)
write.csv(hospSum, "hospSum.csv")
print("WEIGHTED SUM SCORE DONE")

#####STEP 4: READ INTO DIFF SCORES
#print("READ INTO DIFF SCORES")
#DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, hospSum, by = c("basesubjectkey" = "fhxp$subjectkey"))
#DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, hospSum, by = c("basesubjectkey" = "fhxp$subjectkey"))
#write.csv(DiffOneScoresPhenotype, "DiffOneScoresPhenotype.csv")
#write.csv(DiffTwoScoresPhenotype, "DiffTwoScoresPhenotype.csv")
#print("READ INTO DIFF SCORES DONE")




########MANIA
####STEP 2: SELECT NEEDED VARIABLES FOR FIRST DEGREE AND SECOND DEGREE REALTIVES
#first relative is mother, father, full siblings 
firstMania <- fhxp %>%
  select(fam_history_q7a_mania
         , fam_history_q7d_mania
         , q7k_full_sib_young1_mania
         , q7k_full_sib_young2_mania
         , q7k_full_sib_young3_mania
         , q7k_full_sib_young4_mania
         , q7k_full_sib_young5_mania
         , q7l_full_sib_old1_mania
         , q7l_full_sib_old2_mania
         , q7l_full_sib_old3_mania
         , q7l_full_sib_old4_mania
         , q7l_full_sib_old5_mania
         , q7m_full_sib_same1_mania
         , q7m_full_sib_same2_mania)
firstMania$firstManiaSum <- rowSums(firstMania, na.rm = TRUE)
#firstMania <- cbind(firstMania, fhxp$subjectkey)
write.csv(firstMania, "firstMania.csv")
head(firstMania)
print("FIRST MANIA DONE")

#second degre everyone else includng aunts, uncles, grandparents 
secondMania <- fhxp %>%
  select(fam_history_q7b_mania
         , fam_history_q7c_mania
         , fam_history_q7e_mania
         , fam_history_q7f_mania
         , q7g_pat_uncle1_mania
         , q7g_pat_uncle2_mania
         , q7g_pat_uncle3_mania
         , q7g_pat_uncle4_mania
         , q7g_pat_uncle5_mania
         , q7h_pat_aunt1_mania
         , q7h_pat_aunt2_mania
         , q7h_pat_aunt3_mania
         , q7h_pat_aunt4_mania
         , q7h_pat_aunt5_mania
         , q7i_mat_uncle1_mania
         , q7i_mat_uncle2_mania
         , q7i_mat_uncle3_mania
         , q7i_mat_uncle4_mania
         , q7i_mat_uncle5_mania
         , q7j_mat_aunt1_mania
         , q7j_mat_aunt2_mania
         , q7j_mat_aunt3_mania
         , q7j_mat_aunt4_mania
         , q7j_mat_aunt5_mania
         , q7m_half_sib_young1_mania
         , q7m_half_sib_young2_mania
         , q7m_half_sib_young3_mania
         , q7m_half_sib_young4_mania
         , q7m_half_sib_young5_mania
         , q7n_half_sib_old1_mania
         , q7n_half_sib_old2_mania
         , q7n_half_sib_old3_mania
         , q7n_half_sib_old4_mania
         , q7n_half_sib_old5_mania)
secondMania$secondManiaSum <- rowSums(secondMania, na.rm = TRUE)
secondMania$secondManiaWeightedSum <- .5*secondMania$secondManiaSum
#secondMania <- cbind(secondMania, fhxp$subjectkey)
write.csv(secondMania, "secondMania.csv")
head(secondMania)
print("SECOND MANIA DONE")

#####STEP 3: FOR EACH CATEGORY RUN FORUMULA
#first degree + .5(second degree)/total
##Create new data frame for both sum columns and then rowSum them
#Weighted score sum
maniaSum <- data.frame(firstMania$firstManiaSum, secondMania$secondManiaWeightedSum)
maniaSum$maniaSum <- rowSums(maniaSum, na.rm = TRUE)
#Total score sum
totalManiaSum <- data.frame(firstMania$firstManiaSum, secondMania$secondManiaSum)
totalManiaSum$totalManiaSum <- rowSums(totalManiaSum, na.rm = TRUE)
#divide weighted score by total score 
maniaSum$maniaSumFinal <- (maniaSum$maniaSum/totalManiaSum$totalManiaSum)
#Add subject key
maniaSum <- cbind(maniaSum, subjectkey = fhxp$subjectkey)
#head(maniaSum)
#join to orignal data fhxp
#fhxp <- left_join(fhxp, maniaSum, by = "subjectkey",keep = FALSE)
write.csv(maniaSum, "maniaSum.csv")
print("WEIGHTED SUM SCORE DONE")

#####STEP 4: READ INTO DIFF SCORES
#print("READ INTO DIFF SCORES")
#DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, maniaSum, by = c("basesubjectkey" = "fhxp$subjectkey"))
#DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, maniaSum, by = c("basesubjectkey" = "fhxp$subjectkey"))
#write.csv(DiffOneScoresPhenotype, "DiffOneScoresPhenotype.csv")
#write.csv(DiffTwoScoresPhenotype, "DiffTwoScoresPhenotype.csv")
#print("READ INTO DIFF SCORES DONE")


#########NERVES
####STEP 2: SELECT NEEDED VARIABLES FOR FIRST DEGREE AND SECOND DEGREE REALTIVES
#first relative is mother, father, full siblings 
firstNerves <- fhxp %>%
  select(fam_history_q10a_nerves
         , fam_history_q10d_nerves
         , q10k_full_sib_young1_nerves
         , q10k_full_sib_young2_nerves
         , q10k_full_sib_young3_nerves
         , q10k_full_sib_young4_nerves
         , q10k_full_sib_young5_nerves
         , q10l_full_sib_old1_nerves
         , q10l_full_sib_old2_nerves
         , q10l_full_sib_old3_nerves
         , q10l_full_sib_old4_nerves
         , q10l_full_sib_old5_nerves
         , q10m_full_sib_same1_nerves
         , q10m_full_sib_same2_nerves)
firstNerves$firstNervesSum <- rowSums(firstNerves, na.rm = TRUE)
#firstNerves <- cbind(firstNerves, fhxp$subjectkey)
write.csv(firstNerves, "firstNerves.csv")
head(firstNerves)
print("FIRST NERVES DONE")

#second degre everyone else includng aunts, uncles, grandparents 
secondNerves <- fhxp %>%
  select(fam_history_q10b_nerves
         , fam_history_q10c_nerves
         , fam_history_q10e_nerves
         , fam_history_q10f_nerves
         , q10g_pat_uncle1_nerves
         , q10g_pat_uncle2_nerves
         , q10g_pat_uncle3_nerves
         , q10g_pat_uncle4_nerves
         , q10g_pat_uncle5_nerves
         , q10h_pat_aunt1_nerves
         , q10h_pat_aunt2_nerves
         , q10h_pat_aunt3_nerves
         , q10h_pat_aunt4_nerves
         , q10h_pat_aunt5_nerves
         , q10i_mat_uncle1_nerves
         , q10i_mat_uncle2_nerves
         , q10i_mat_uncle3_nerves
         , q10i_mat_uncle4_nerves
         , q10i_mat_uncle5_nerves
         , q10j_mat_aunt1_nerves
         , q10j_mat_aunt2_nerves
         , q10j_mat_aunt3_nerves
         , q10j_mat_aunt4_nerves
         , q10j_mat_aunt5_nerves
         , q10m_half_sib_young1_nerves
         , q10m_half_sib_young2_nerves
         , q10m_half_sib_young3_nerves
         , q10m_half_sib_young4_nerves
         , q10m_half_sib_young5_nerves
         , q10n_half_sib_old1_nerves
         , q10n_half_sib_old2_nerves
         , q10n_half_sib_old3_nerves
         , q10n_half_sib_old4_nerves
         , q10n_half_sib_old5_nerves)
secondNerves$secondNervesSum <- rowSums(secondNerves, na.rm = TRUE)
secondNerves$secondNervesWeightedSum <- .5*secondNerves$secondNervesSum
#secondNerves <- cbind(secondNerves, fhxp$subjectkey)
write.csv(secondNerves, "secondNerves.csv")
head(secondNerves)
print("SECOND NERVES DONE")

#####STEP 3: FOR EACH CATEGORY RUN FORUMULA
#first degree + .5(second degree)/total
##Create new data frame for both sum columns and then rowSum them
#Weighted score sum
nervesSum <- data.frame(firstNerves$firstNervesSum, secondNerves$secondNervesWeightedSum)
nervesSum$nervesSum <- rowSums(nervesSum, na.rm = TRUE)
#Total score sum
totalNervesSum <- data.frame(firstNerves$firstNervesSum, secondNerves$secondNervesSum)
totalNervesSum$totalNervesSum <- rowSums(totalNervesSum, na.rm = TRUE)
#divide weighted score by total score 
nervesSum$nervesSumFinal <- (nervesSum$nervesSum/totalNervesSum$totalNervesSum)
#Add subject key
nervesSum <- cbind(nervesSum, subjectkey = fhxp$subjectkey)
#head(nervesSum)
#join to orignal data fhxp
#fhxp <- left_join(fhxp, nervesSum, by = "subjectkey",keep = FALSE)
write.csv(nervesSum, "nervesSum.csv")
print("WEIGHTED SUM SCORE DONE")

#####STEP 4: READ INTO DIFF SCORES
#print("READ INTO DIFF SCORES")
#DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, nervesSum, by = c("basesubjectkey" = "fhxp$subjectkey"))
#DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, nervesSum, by = c("basesubjectkey" = "fhxp$subjectkey"))
#write.csv(DiffOneScoresPhenotype, "DiffOneScoresPhenotype.csv")
#write.csv(DiffTwoScoresPhenotype, "DiffTwoScoresPhenotype.csv")
#print("READ INTO DIFF SCORES DONE")


########PROFESSIONAL
####STEP 2: SELECT NEEDED VARIABLES FOR FIRST DEGREE AND SECOND DEGREE REALTIVES
#first relative is mother, father, full siblings 
firstProf <- fhxp %>%
  select(fam_history_q11a_professional
         , fam_history_q11d_professional
         , q11k_full_sib_young1_prof
         , q11k_full_sib_young2_prof
         , q11k_full_sib_young3_prof
         , q11k_full_sib_young4_prof
         , q11k_full_sib_young5_prof
         , q11l_full_sib_old1_prof
         , q11l_full_sib_old2_prof
         , q11l_full_sib_old3_prof
         , q11l_full_sib_old4_prof
         , q11l_full_sib_old5_prof
         , q11m_full_sib_same1_prof
         , q11m_full_sib_same2_prof)
firstProf$firstProfSum <- rowSums(firstProf, na.rm = TRUE)
#firstProf <- cbind(firstProf, fhxp$subjectkey)
write.csv(firstProf, "firstProf.csv")
head(firstProf)
print("FIRST PROF DONE")

#second degre everyone else includng aunts, uncles, grandparents 
secondProf <- fhxp %>%
  select(fam_history_q11b_professional
         , fam_history_q11c_professional
         , fam_history_q11e_professional
         , fam_history_q11f_professional
         , q11g_pat_uncle1_professional
         , q11g_pat_uncle2_professional
         , q11g_pat_uncle3_professional
         , q11g_pat_uncle4_professional
         , q11g_pat_uncle5_professional
         , q11h_pat_aunt1_professional
         , q11h_pat_aunt2_professional
         , q11h_pat_aunt3_professional
         , q11h_pat_aunt4_professional
         , q11h_pat_aunt5_professional
         , q11i_mat_uncle1_professional
         , q11i_mat_uncle2_professional
         , q11i_mat_uncle3_professional
         , q11i_mat_uncle4_professional
         , q11i_mat_uncle5_professional
         , q11j_mat_aunt1_professional
         , q11j_mat_aunt2_professional
         , q11j_mat_aunt3_professional
         , q11j_mat_aunt4_professional
         , q11j_mat_aunt5_professional
         , q11m_half_sib_young1_prof
         , q11m_half_sib_young2_prof
         , q11m_half_sib_young3_prof
         , q11m_half_sib_young4_prof
         , q11m_half_sib_young5_prof
         , q11n_half_sib_old1_prof
         , q11n_half_sib_old2_prof
         , q11n_half_sib_old3_prof
         , q11n_half_sib_old4_prof
         , q11n_half_sib_old5_prof)
secondProf$secondProfSum <- rowSums(secondProf, na.rm = TRUE)
secondProf$secondProfWeightedSum <- .5*secondProf$secondProfSum
#secondProf <- cbind(secondProf, fhxp$subjectkey)
write.csv(secondProf, "secondProf.csv")
head(secondProf)
print("SECOND PROF DONE")

#####STEP 3: FOR EACH CATEGORY RUN FORUMULA
#first degree + .5(second degree)/total
##Create new data frame for both sum columns and then rowSum them
#Weighted score sum
profSum <- data.frame(firstProf$firstProfSum, secondProf$secondProfWeightedSum)
profSum$profSum <- rowSums(profSum, na.rm = TRUE)
#Total score sum
totalProfSum <- data.frame(firstProf$firstProfSum, secondProf$secondProfSum)
totalProfSum$totalProfSum <- rowSums(totalProfSum, na.rm = TRUE)
#divide weighted score by total score 
profSum$profSumFinal <- (profSum$profSum/totalProfSum$totalProfSum)
#Add subject key
profSum <- cbind(profSum, subjectkey = fhxp$subjectkey)
#head(profSum)
#join to orignal data fhxp
#fhxp <- left_join(fhxp, profSum, by = "subjectkey",keep = FALSE)
write.csv(profSum, "profSum.csv")
print("WEIGHTED SUM SCORE DONE")

#####STEP 4: READ INTO DIFF SCORES
#print("READ INTO DIFF SCORES")
#DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, nervesSum, by = c("basesubjectkey" = "fhxp$subjectkey"))
#DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, nervesSum, by = c("basesubjectkey" = "fhxp$subjectkey"))
#write.csv(DiffOneScoresPhenotype, "DiffOneScoresPhenotype.csv")
#write.csv(DiffTwoScoresPhenotype, "DiffTwoScoresPhenotype.csv")
#print("READ INTO DIFF SCORES DONE")



########SUICIDE
####STEP 2: SELECT NEEDED VARIABLES FOR FIRST DEGREE AND SECOND DEGREE REALTIVES
#first relative is mother, father, full siblings 
firstSuicide <- fhxp %>%
  select(fam_history_q13a_suicide
         , fam_history_q13d_suicide
         , q13k_full_sib_young1_suicide 
         , q13k_full_sib_young2_suicide 
         , q13k_full_sib_young3_suicide 
         , q13k_full_sib_young4_suicide 
         , q13k_full_sib_young5_suicide 
         , q13l_full_sib_old1_suicide 
         , q13l_full_sib_old2_suicide 
         , q13l_full_sib_old3_suicide
         , q13l_full_sib_old4_suicide 
         , q13l_full_sib_old5_suicide
         , q13m_full_sib_same1_suicide 
         , q13m_full_sib_same2_suicide)
firstSuicide$firstSuicideSum <- rowSums(firstSuicide, na.rm = TRUE)
#firstSuicide <- cbind(firstSuicide, fhxp$subjectkey)
write.csv(firstSuicide, "firstSuicide.csv")
head(firstSuicide)
print("FIRST SUICIDE DONE")

#second degre everyone else includng aunts, uncles, grandparents 
secondSuicide <- fhxp %>%
  select(fam_history_q13b_suicide 
         , fam_history_q13c_suicide
         , fam_history_q13e_suicide 
         , fam_history_q13f_suicide
         , q13g_pat_uncle1_suicide
         , q13g_pat_uncle2_suicide 
         , q13g_pat_uncle3_suicide 
         , q13g_pat_uncle4_suicide 
         , q13g_pat_uncle5_suicide 
         , q13h_pat_aunt1_suicide 
         , q13h_pat_aunt2_suicide
         , q13h_pat_aunt3_suicide 
         , q13h_pat_aunt4_suicide 
         , q13h_pat_aunt5_suicide 
         , q13i_mat_uncle1_suicide
         , q13i_mat_uncle2_suicide 
         , q13i_mat_uncle3_suicide
         , q13i_mat_uncle4_suicide 
         , q13i_mat_uncle5_suicide
         , q13j_mat_aunt1_suicide
         , q13j_mat_aunt2_suicide 
         , q13j_mat_aunt3_suicide 
         , q13j_mat_aunt4_suicide 
         , q13j_mat_aunt5_suicide 
         , q13m_half_sib_young1_suicide
         , q13m_half_sib_young2_suicide
         , q13m_half_sib_young3_suicide 
         , q13m_half_sib_young4_suicide
         , q13m_half_sib_young5_suicide 
         , q13n_half_sib_old1_suicide 
         , q13n_half_sib_old2_suicide 
         , q13n_half_sib_old3_suicide 
         , q13n_half_sib_old4_suicide
         , q13n_half_sib_old5_suicide)
secondSuicide$secondSuicideSum <- rowSums(secondSuicide, na.rm = TRUE)
secondSuicide$secondSuicideWeightedSum <- .5*secondSuicide$secondSuicideSum
#secondSuicide <- cbind(secondSuicide, fhxp$subjectkey)
write.csv(secondSuicide, "secondSuicide.csv")
head(secondSuicide)
print("SECOND SUICIDE DONE")

#####STEP 3: FOR EACH CATEGORY RUN FORUMULA
#first degree + .5(second degree)/total
##Create new data frame for both sum columns and then rowSum them
#Weighted score sum
suicideSum <- data.frame(firstSuicide$firstSuicideSum, secondSuicide$secondSuicideWeightedSum)
suicideSum$suicideSum <- rowSums(suicideSum, na.rm = TRUE)
#Total score sum
totalSuicideSum <- data.frame(firstSuicide$firstSuicideSum, secondSuicide$secondSuicideSum)
totalSuicideSum$totalSuicideSum <- rowSums(totalSuicideSum, na.rm = TRUE)
#divide weighted score by total score 
suicideSum$suicideSumFinal <- (suicideSum$suicideSum/totalSuicideSum$totalSuicideSum)
#Add subject key
suicideSum <- cbind(suicideSum, subjectkey = fhxp$subjectkey)
#head(suicideSum)
#join to orignal data fhxp
#fhxp <- left_join(fhxp, suicideSum, by = "subjectkey",keep = FALSE)
write.csv(suicideSum, "suicideSum.csv")
print("WEIGHTED SUM SCORE DONE")

#####STEP 4: READ INTO DIFF SCORES
#print("READ INTO DIFF SCORES")
#DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, suicideSum, by = c("basesubjectkey" = "fhxp$subjectkey"))
#DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, suicideSum, by = c("basesubjectkey" = "fhxp$subjectkey"))
#write.csv(DiffOneScoresPhenotype, "DiffOneScoresPhenotype.csv")
#write.csv(DiffTwoScoresPhenotype, "DiffTwoScoresPhenotype.csv")
#print("READ INTO DIFF SCORES DONE")


########TROUBLE
####STEP 2: SELECT NEEDED VARIABLES FOR FIRST DEGREE AND SECOND DEGREE REALTIVES
#first relative is mother, father, full siblings 
firstTrouble <- fhxp %>%
  select(fam_history_q9a_trouble
         , fam_history_q9d_trouble 
         , q9k_full_sib_young1_trouble
         , q9k_full_sib_young2_trouble
         , q9k_full_sib_young3_trouble
         , q9k_full_sib_young4_trouble
         , q9k_full_sib_young5_trouble
         , q9l_full_sib_old1_trouble 
         , q9l_full_sib_old2_trouble
         , q9l_full_sib_old3_trouble 
         , q9l_full_sib_old4_trouble 
         , q9l_full_sib_old5_trouble 
         , q9m_full_sib_same1_trouble
         , q9m_full_sib_same2_trouble)
firstTrouble$firstTroubleSum <- rowSums(firstTrouble, na.rm = TRUE)
#firstTrouble <- cbind(firstTrouble, fhxp$subjectkey)
write.csv(firstTrouble, "firstTrouble.csv")
head(firstTrouble)
print("FIRST TROUBLE DONE")

#second degre everyone else includng aunts, uncles, grandparents 
secondTrouble <- fhxp %>%
  select(fam_history_q9b_trouble 
         , fam_history_q9c_trouble 
         , fam_history_q9e_trouble 
         , fam_history_q9f_trouble 
         , q9g_pat_uncle1_trouble 
         , q9g_pat_uncle2_trouble
         , q9g_pat_uncle3_trouble
         , q9g_pat_uncle4_trouble 
         , q9g_pat_uncle5_trouble
         , q9h_pat_aunt1_trouble
         , q9h_pat_aunt2_trouble 
         , q9h_pat_aunt3_trouble
         , q9h_pat_aunt4_trouble
         , q9h_pat_aunt5_trouble 
         , q9i_mat_uncle1_trouble 
         , q9i_mat_uncle2_trouble 
         , q9i_mat_uncle3_trouble
         , q9i_mat_uncle4_trouble 
         , q9i_mat_uncle5_trouble
         , q9j_mat_aunt1_trouble
         , q9j_mat_aunt2_trouble 
         , q9j_mat_aunt3_trouble 
         , q9j_mat_aunt4_trouble 
         , q9j_mat_aunt5_trouble 
         , q9m_half_sib_young1_trouble 
         , q9m_half_sib_young2_trouble
         , q9m_half_sib_young3_trouble 
         , q9m_half_sib_young4_trouble 
         , q9m_half_sib_young5_trouble 
         , q9n_half_sib_old1_trouble
         , q9n_half_sib_old2_trouble 
         , q9n_half_sib_old3_trouble
         , q9n_half_sib_old4_trouble 
         , q9n_half_sib_old5_trouble)
secondTrouble$secondTroubleSum <- rowSums(secondTrouble, na.rm = TRUE)
secondTrouble$secondTroubleWeightedSum <- .5*secondTrouble$secondTroubleSum
#secondTrouble <- cbind(secondTrouble, fhxp$subjectkey)
write.csv(secondTrouble, "secondTrouble.csv")
head(secondTrouble)
print("SECOND TROUBLE DONE")

#####STEP 3: FOR EACH CATEGORY RUN FORUMULA
#first degree + .5(second degree)/total
##Create new data frame for both sum columns and then rowSum them
#Weighted score sum
troubleSum <- data.frame(firstTrouble$firstTroubleSum, secondTrouble$secondTroubleWeightedSum)
troubleSum$troubleSum <- rowSums(troubleSum, na.rm = TRUE)
#Total score sum
totalTroubleSum <- data.frame(firstTrouble$firstTroubleSum, secondTrouble$secondTroubleSum)
totalTroubleSum$totalTroubleSum <- rowSums(totalTroubleSum, na.rm = TRUE)
#divide weighted score by total score 
troubleSum$troubleSumFinal <- (troubleSum$troubleSum/totalTroubleSum$totalTroubleSum)
#Add subject key
troubleSum <- cbind(troubleSum, subjectkey = fhxp$subjectkey)
#head(troubleSum)
#join to orignal data fhxp
#fhxp <- left_join(fhxp, troubleSum, by = "subjectkey",keep = FALSE)
write.csv(troubleSum, "troubleSum.csv")
print("WEIGHTED SUM SCORE DONE")

#####STEP 4: READ INTO DIFF SCORES
#DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, troubleSum, by = c("basesubjectkey" = "fhxp$subjectkey"))
#DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, troubleSum, by = c("basesubjectkey" = "fhxp$subjectkey"))
#write.csv(DiffOneScoresPhenotype, "DiffOneScoresPhenotype.csv")
#write.csv(DiffTwoScoresPhenotype, "DiffTwoScoresPhenotype.csv")




########VISIONS
####STEP 2: SELECT NEEDED VARIABLES FOR FIRST DEGREE AND SECOND DEGREE REALTIVES
#first relative is mother, father, full siblings 
firstVision <- fhxp %>%
  select(fam_history_q8a_visions
         , fam_history_q8d_visions
         , q8k_full_sib_young1_visions
         , q8k_full_sib_young2_visions
         , q8k_full_sib_young3_visions
         , q8k_full_sib_young4_visions
         , q8k_full_sib_young5_visions
         , q8l_full_sib_old1_visions
         , q8l_full_sib_old2_visions
         , q8l_full_sib_old3_visions
         , q8l_full_sib_old4_visions
         , q8l_full_sib_old5_visions
         , q8m_full_sib_same1_visions
         , q8m_full_sib_same2_visions)
firstVision$firstVisionSum <- rowSums(firstVision, na.rm = TRUE)
#firstVision <- cbind(firstVision, fhxp$subjectkey)
write.csv(firstVision, "firstVision.csv")
head(firstVision)
print("FIRST VISION DONE")

#second degre everyone else includng aunts, uncles, grandparents 
secondVision <- fhxp %>%
  select(fam_history_q8b_visions
         , fam_history_q8c_visions
         , fam_history_q8e_visions
         , fam_history_q8f_visions
         , q8g_pat_uncle1_visions
         , q8g_pat_uncle2_visions
         , q8g_pat_uncle3_visions
         , q8g_pat_uncle4_visions
         , q8g_pat_uncle5_visions
         , q8h_pat_aunt1_visions
         , q8h_pat_aunt2_visions
         , q8h_pat_aunt3_visions
         , q8h_pat_aunt4_visions
         , q8h_pat_aunt5_visions
         , q8i_mat_uncle1_visions
         , q8i_pat_uncle2_visions
         , q8i_mat_uncle3_visions
         , q8i_mat_uncle4_visions
         , q8i_mat_uncle5_visions
         , q8j_mat_aunt1_visions
         , q8j_mat_aunt2_visions
         , q8j_mat_aunt3_visions
         , q8j_mat_aunt4_visions
         , q8j_mat_aunt5_visions
         , q8m_half_sib_young1_visions
         , q8m_half_sib_young2_visions
         , q8m_half_sib_young3_visions
         , q8m_half_sib_young4_visions
         , q8m_half_sib_young5_visions
         , q8n_half_sib_old1_visions
         , q8n_half_sib_old2_visions
         , q8n_half_sib_old3_visions
         , q8n_half_sib_old4_visions
         , q8n_half_sib_old5_visions)
secondVision$secondVisionSum <- rowSums(secondVision, na.rm = TRUE)
secondVision$secondVisionWeightedSum <- .5*secondVision$secondVisionSum
#secondVision <- cbind(secondVision, fhxp$subjectkey)
write.csv(secondVision, "secondVision.csv")
head(secondVision)
print("SECOND VISION DONE")

#####STEP 3: FOR EACH CATEGORY RUN FORUMULA
#first degree + .5(second degree)/total
##Create new data frame for both sum columns and then rowSum them
#Weighted score sum
visionSum <- data.frame(firstVision$firstVisionSum, secondVision$secondVisionWeightedSum)
visionSum$visionSum <- rowSums(visionSum, na.rm = TRUE)
#Total score sum
totalVisionSum <- data.frame(firstVision$firstVisionSum, secondVision$secondVisionSum)
totalVisionSum$totalVisionSum <- rowSums(totalVisionSum, na.rm = TRUE)
#divide weighted score by total score 
visionSum$visionSumFinal <- (visionSum$visionSum/totalVisionSum$totalVisionSum)
#Add subject key
visionSum <- cbind(visionSum, subjectkey = fhxp$subjectkey)
#head(profSum)
#join to orignal data fhxp
#fhxp <- left_join(fhxp, profSum, by = "subjectkey",keep = FALSE)
write.csv(visionSum, "visionSum.csv")
print("WEIGHTED SUM SCORE DONE")

#####STEP 4: READ INTO DIFF SCORES
#print("READ INTO DIFF SCORES")
#DiffOneScoresPhenotype <- left_join(DiffOneScoresPhenotype, visionSum, by = c("basesubjectkey" = "fhxp$subjectkey"))
#DiffTwoScoresPhenotype <- left_join(DiffTwoScoresPhenotype, visionSum, by = c("basesubjectkey" = "fhxp$subjectkey"))
#write.csv(DiffOneScoresPhenotype, "DiffOneScoresPhenotype.csv")
#write.csv(DiffTwoScoresPhenotype, "DiffTwoScoresPhenotype.csv")
#print("READ INTO DIFF SCORES DONE")

#####STEP 5: SUM SCORE FOR EACH CATEGORY 
#FHtotal <- cbind(depSum$depSumFinal, hospSum$hospSumFinal, maniaSum$maniaSumFinal
 #                , nervesSum$nervesSumFinal, profSum$profSumFinal, suicideSum$suicideSumFinal
 #                , troubleSum$troubleSumFinal, visionSum$visionSumFinal)
FHtotal <- left_join(depSum, hospSum, by = "subjectkey")
FHtotal <- left_join(FHtotal, maniaSum, by = "subjectkey")
FHtotal <- left_join(FHtotal, nervesSum, by = "subjectkey")
FHtotal <- left_join(FHtotal, profSum, by = "subjectkey")
FHtotal <- left_join(FHtotal, suicideSum, by = "subjectkey")
FHtotal <- left_join(FHtotal, troubleSum, by = "subjectkey")
FHtotal <- left_join(FHtotal, visionSum, by = "subjectkey")

FHtotal <- FHtotal %>% select(depSumFinal
         , hospSumFinal
         , maniaSumFinal
         , nervesSumFinal 
         , profSumFinal 
         , suicideSumFinal
         , troubleSumFinal
         , visionSumFinal)

#FHtotal <- as.data.frame(FHtotal) 
FHtotal$FHtotal <- rowSums(FHtotal, na.rm = TRUE)
FHtotal <- cbind(eventname = fhxp$eventname, subjectkey = fhxp$subjectkey, FHtotal)


#write into csv
write.csv(FHtotal, "BaseFHtotal.csv")

####STEP 6: JOIN TO DIFF SCORES
#DiffOneScores <- read.delim("/PATH/DiffOneScores.csv")
#DiffTwoScores <- read.delim("/PATH/DiffTwoScores.csv")
#DiffOneScoresPhenotype <- left_join(DiffOneScores, FHtotal,by = "subjectkey",keep = FALSE)
#DiffTwoScoresPhenotype <- left_join(DiffTwoScores, FHtotal,by = "subjectkey",keep = FALSE)