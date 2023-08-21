#Generate test data 

#######STEP 1: MAKE THE ORIGINAL CBCL_NM
#read og cbcl
cbcl_nm_og <- read.delim(file.choose())
ncol(cbcl_nm_og) #124
#3000 * 119 = 357000, 357000 observains, 1000 subjects, 3000 rows

#make new sample
samplecbclsample <- sample(x = 0:2, size = 357000, replace = TRUE)
#create sample data
samplecbclvector <- as.vector(samplecbclsample)
samplecbclMatrix <- matrix(samplecbclvector, nrow = 3000, ncol = 119, byrow = FALSE)
cbcl_nm <- as.data.frame(samplecbclMatrix)

colnames(cbcl_nm_og)
colnames(cbcl_nm) <- colnames(cbcl_nm_og[,5:123])
#add columns of subject keys and event name 
subjectkey <- c(1:1000, 1:1000, 1:1000)

eventnamebase <- sample(x ="baseline_year_1_arm_1",size = 1000, replace = TRUE)
eventname1 <- sample(x ="1_year_follow_up_y_arm_1",size = 1000, replace = TRUE)
eventname2 <- sample(x ="2_year_follow_up_y_arm_1",size = 1000, replace = TRUE)
eventname <- c(eventnamebase, eventname1, eventname2)
eventname

#Final orignal data, my own sampe
cbcl_nm <- cbind(subjectkey, eventname, cbcl_nm)


#######STEP 2: DELETE “Drinks alcohol without parents' approval”, “Sexual problems”, “Smokes, chews, or sniffs tobacco”, “Truancy, skips school”, “Uses drugs for non-medical purposes (don't include alcohol or tobacco)”.
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q02_p, cbcl_q73_p,
                                       cbcl_q99_p, cbcl_q101_p, 
                                       cbcl_q105_p))
#####STEP 3. AGGREGATE NEEDED ITEMS INTO COMPOSITES

##CONVERT character columns to numeric
#count number of columns 
ncol(cbcl_nm)

#Columns that must be numeric are 5:118 so those will become their own data.frame
columns.to.numeric <- c(3:116)
#view(columns.to.numeric)

#changes 5:118 (columns.to.numeric) to numeric data but keeps everything else as characters 
cbcl_nm[, columns.to.numeric] <- apply(cbcl_nm[ ,columns.to.numeric], 2,
                                        function(x) as.numeric(as.character(x))) 
#Remove comment below code if there is a need to troubleshoot 
#head(cbcl_nm)
#str(columns.to.numeric) #check what type of data
#str(cbcl_nm) # should have both characters and numeric data

##CREATE composites 

#Attacks/threatens (“Physically attacks people”, “Threatens people”)
#cbcl_nm$AttacksThreatens <- rowMeans(cbcl_nm[,c("cbcl_q57_p", "cbcl_q97_p")],na.rm = TRUE)
#cbcl_nm$AttacksThreatens <- pmax(cbcl_nm$"cbcl_q57_p", cbcl_nm$"cbcl_q97_p")
cbcl_nm$AttacksThreatens <- round(rowMeans(cbcl_nm[,c("cbcl_q57_p", "cbcl_q97_p")],na.rm = TRUE), 0)

#view(cbcl_nm$`Attacks/threatens`)

#Destroys (“Destroys his/her own things”, “Destroys things belonging to his/her family or others”, “Vandalism”)
#cbcl_nm$Destroys <- rowMeans(cbcl_nm[,c("cbcl_q20_p", "cbcl_q21_p", "cbcl_q106_p")],na.rm = TRUE)
#cbcl_nm$Destroys <- pmax(cbcl_nm$"cbcl_q20_p", cbcl_nm$"cbcl_q21_p", cbcl_nm$"cbcl_q106_p")
cbcl_nm$Destroys <- round(rowMeans(cbcl_nm[,c("cbcl_q20_p", "cbcl_q21_p", "cbcl_q106_p")],na.rm = TRUE), 0)

#Disobeys rules (“Disobedient at home”, “Disobedient at school”, “Breaks rules at home, school or elsewhere”)
#cbcl_nm$Disobeysrules <- rowMeans(cbcl_nm[,c("cbcl_q22_p", "cbcl_q23_p", "cbcl_q28_p")],na.rm = TRUE)
#cbcl_nm$Disobeysrules <- pmax(cbcl_nm$"cbcl_q22_p", cbcl_nm$"cbcl_q23_p", cbcl_nm$"cbcl_q28_p")
cbcl_nm$Disobeysrules <- round(rowMeans(cbcl_nm[,c("cbcl_q22_p", "cbcl_q23_p", "cbcl_q28_p")],na.rm = TRUE), 0)

#Steals (“Steals at home”, “Steals outside the home”)
#cbcl_nm$Steals <- rowMeans(cbcl_nm[,c("cbcl_q81_p", "cbcl_q82_p")],na.rm = TRUE)
#cbcl_nm$Steals <- pmax(cbcl_nm$"cbcl_q81_p", cbcl_nm$"cbcl_q82_p")
cbcl_nm$Steals <- round(rowMeans(cbcl_nm[,c("cbcl_q81_p", "cbcl_q82_p")],na.rm = TRUE), 0)

# Peer problems (“Doesn't get along with other kids”, “Not liked by other kids”)
#cbcl_nm$PeerProblems <- rowMeans(cbcl_nm[,c("cbcl_q25_p", "cbcl_q48_p")],na.rm = TRUE)
#cbcl_nm$PeerProblems <- pmax(cbcl_nm$"cbcl_q25_p", cbcl_nm$"cbcl_q48_p")
cbcl_nm$PeerProblems <- round(rowMeans(cbcl_nm[,c("cbcl_q25_p", "cbcl_q48_p")],na.rm = TRUE), 0)

#Distracted/Hyperactive (“Can't concentrate, can't pay attention for long”, “Inattentive or easily distracted”, “Can't sit still, restless, or hyperactive”)
#cbcl_nm$DistractedHyperactive <- rowMeans(cbcl_nm[,c("cbcl_q08_p", "cbcl_q78_p", "cbcl_q10_p")],na.rm = TRUE)
#cbcl_nm$DistractedHyperactive <-  pmax(cbcl_nm$"cbcl_q08_p", cbcl_nm$"cbcl_q78_p", cbcl_nm$"cbcl_q10_p")
cbcl_nm$DistractedHyperactive <- round(rowMeans(cbcl_nm[,c("cbcl_q08_p", "cbcl_q78_p", "cbcl_q10_p")],na.rm = TRUE), 0)

#Hallucinations (“Hears sound or voices that aren't there”, “Sees things that aren't there”)
#cbcl_nm$Hallucinations <- rowMeans(cbcl_nm[,c("cbcl_q40_p", "cbcl_q70_p")],na.rm = TRUE)
#cbcl_nm$Hallucinations <- pmax(cbcl_nm$"cbcl_q40_p", cbcl_nm$"cbcl_q70_p")
cbcl_nm$Hallucinations <- round(rowMeans(cbcl_nm[,c("cbcl_q40_p", "cbcl_q70_p")],na.rm = TRUE), 0)

#Sex play (“Plays with own sex parts in public”, “Plays with own sex parts too much”)
#cbcl_nm$SexPlay <- rowMeans(cbcl_nm[,c("cbcl_q59_p", "cbcl_q60_p")], na.rm = TRUE)
#cbcl_nm$SexPlay <- pmax(cbcl_nm$"cbcl_q59_p", cbcl_nm$"cbcl_q60_p")
cbcl_nm$SexPlay <- round(rowMeans(cbcl_nm[,c("cbcl_q59_p", "cbcl_q60_p")], na.rm = TRUE), 0)

#Weight problems (“Overeating”, “Overweight”)
#cbcl_nm$Weightproblems <- rowMeans(cbcl_nm[,c("cbcl_q53_p", "cbcl_q55_p")], na.rm = TRUE)
#cbcl_nm$Weightproblems <- pmax(cbcl_nm$"cbcl_q53_p", cbcl_nm$"cbcl_q55_p")
cbcl_nm$Weightproblems <- round(rowMeans(cbcl_nm[,c("cbcl_q53_p", "cbcl_q55_p")], na.rm = TRUE), 0)


#Remove Comment below code if there is a need to troubleshoot 
#ncol(cbcl_nm) #Check number of columns: 128  

##REMOVE the extra columns which were aggregated to composites 
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q57_p, cbcl_q97_p)) #attacks/threatens
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q20_p)) #destroys
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q21_p, cbcl_q106_p)) #destroys
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q22_p, cbcl_q23_p, cbcl_q28_p)) #Disobeys
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q81_p, cbcl_q82_p)) #steals 
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q25_p, cbcl_q48_p)) #peer problems
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q08_p, cbcl_q78_p, cbcl_q10_p)) #Distracted/Hyperactive
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q40_p, cbcl_q70_p)) #Hallucinations
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q59_p, cbcl_q60_p)) #Sex play 
cbcl_nm <- subset(cbcl_nm, select = -c(cbcl_q53_p, cbcl_q55_p)) #Weight problems 


#####STEP 4. SEPERATE INTO DIFFERENT TIMELINES (baseline_year_1_arm_1 , 1_year_follow_up_y_arm_1, 2_year_follow_up_y_arm_1 )

#baseline_year_1_arm_1
baselineData <- cbcl_nm %>% 
  filter(eventname == "baseline_year_1_arm_1")
colnames(baselineData) <- paste0("base", colnames(baselineData))
ncol(baselineData)
head(baselineData)

# 1_year_follow_up_y_arm_1
oneyearData <- cbcl_nm %>% 
  filter(eventname == "1_year_follow_up_y_arm_1")
colnames(oneyearData) <- paste0("one", colnames(oneyearData))
head(oneyearData)

#2_year_follow_up_y_arm_1
twoyearData <- cbcl_nm %>% 
  filter(eventname == "2_year_follow_up_y_arm_1")
colnames(twoyearData) <- paste0("two", colnames(twoyearData))
head(twoyearData)

#Make numeric parts 
baselineDataNumeric <- select(baselineData, c(3:103))
oneyearDataNumeric <- select(oneyearData, c(3:103))
twoyearDataNumeric <- select(twoyearData, c(3:103))












#MAKING CBCLDF1
cbcl_nm1 <- sample(x = 0:2, size = 960, replace = TRUE)
samplevector <- as.vector(samplecbcl1)

sampleMatrix <- matrix(samplevector, nrow = 5, ncol = 192, byrow = FALSE)
sampleDF1 <- as.data.frame(sampleMatrix)
colnames(sampleDF1) <- c("basecbcl_q56a_p"
                        , "basecbcl_q01_p"
                        , "basecbcl_q03_p"
                        , "basecbcl_q06_p"
                        , "basecbcl_q07_p"
                        , "basecbcl_q09_p"
                        , "basecbcl_q11_p"
                        , "basecbcl_q12_p"
                        , "basecbcl_q13_p"
                        , "basecbcl_q14_p"
                        , "basecbcl_q15_p"
                        , "basecbcl_q16_p"
                        , "basecbcl_q17_p"
                        , "basecbcl_q18_p"
                        , "basecbcl_q19_p"
                        , "basecbcl_q24_p"
                        , "basecbcl_q26_p"
                        , "basecbcl_q27_p"
                        , "basecbcl_q04_p"
                        , "basecbcl_q29_p"
                        , "basecbcl_q30_p"
                        , "basecbcl_q31_p"
                        , "basecbcl_q51_p"
                        , "basecbcl_q32_p"
                        , "basecbcl_q33_p"
                        , "basecbcl_q34_p"
                        , "basecbcl_q52_p"
                        , "basecbcl_q35_p"
                        , "basecbcl_q36_p"
                        , "basecbcl_q37_p"
                        , "basecbcl_q38_p"
                        , "basecbcl_q39_p"
                        , "basecbcl_q56b_p"
                        , "basecbcl_q41_p"
                        , "basecbcl_q43_p"
                        , "basecbcl_q56c_p"
                        , "basecbcl_q46_p"
                        , "basecbcl_q45_p"
                        , "basecbcl_q47_p"
                        , "basecbcl_q56h_p"
                        , "basecbcl_q54_p"
                        , "basecbcl_q58_p"
                        , "basecbcl_q61_p"
                        , "basecbcl_q62_p"
                        , "basecbcl_q63_p"
                        , "basecbcl_q64_p"
                        , "basecbcl_q56d_p"
                        , "basecbcl_q56e_p"
                        , "basecbcl_q65_p"
                        , "basecbcl_q66_p"
                        , "basecbcl_q67_p"
                        , "basecbcl_q68_p"
                        , "basecbcl_q69_p"
                        , "basecbcl_q71_p"
                        , "basecbcl_q74_p"
                        , "basecbcl_q76_p"
                        , "basecbcl_q77_p"
                        , "basecbcl_q79_p"
                        , "basecbcl_q80_p"
                        , "basecbcl_q56f_p"
                        , "basecbcl_q83_p"
                        , "basecbcl_q84_p"
                        , "basecbcl_q85_p"
                        , "basecbcl_q86_p"
                        , "basecbcl_q87_p"
                        , "basecbcl_q88_p"
                        , "basecbcl_q89_p"
                        , "basecbcl_q90_p"
                        , "basecbcl_q91_p"
                        , "basecbcl_q93_p"
                        , "basecbcl_q94_p"
                        , "basecbcl_q95_p"
                        , "basecbcl_q96_p"
                        , "basecbcl_q50_p"
                        , "basecbcl_q75_p"
                        , "basecbcl_q100_p"
                        , "basecbcl_q102_p"
                        , "basecbcl_q103_p"
                        , "basecbcl_q104_p"
                        , "basecbcl_q56g_p"
                        , "basecbcl_q107_p"
                        , "basecbcl_q109_p"
                        , "basecbcl_q110_p"
                        , "basecbcl_q111_p"
                        , "basecbcl_q112_p"
                        , "basecbcl_q13_p"
                        , "basecbcl_q42_p"
                        , "baseAttacksThreatens"
                        , "baseDestroys"
                        , "baseDisobeysrules"
                        , "baseDistractedHyperactive"
                        , "baseHallucinations"
                        , "basePeerProblems"
                        , "baseSexPlay"
                        , "baseSteals"
                        , "baseWeightproblems"
                        , "onecbcl_q56a_p"
                        , "onecbcl_q01_p"
                        , "onecbcl_q03_p"
                        , "onecbcl_q06_p"
                        , "onecbcl_q07_p"
                        , "onecbcl_q09_p"
                        , "onecbcl_q11p"
                        , "onecbcl_q12_p"
                        , "onecbcl_q13_p"
                        , "onecbcl_q14_p"
                        , "onecbcl_q15_p"
                        , "onecbcl_q16_p"
                        , "onecbcl_q17_p"
                        , "onecbcl_q18_p"
                        , "onecbcl_q19_p"
                        , "onecbcl_q24_p"
                        , "onecbcl_q26_p"
                        , "onecbcl_q27_p"
                        , "onecbcl_q04_p"
                        , "onecbcl_q29_p"
                        , "onecbcl_q30_p"
                        , "onecbcl_q31_p"
                        , "onecbcl_q51_p"
                        , "onecbcl_q32_p"
                        , "onecbcl_q33_p"
                        , "onecbcl_q34_p"
                        , "onecbcl_q52_p"
                        , "onecbcl_q35_p"
                        , "onecbcl_q36_p"
                        , "onecbcl_q37_p"
                        , "onecbcl_q38_p"
                        , "onecbcl_q39_p"
                        , "onecbcl_q56b_p"
                        , "onecbcl_q41_p"
                        , "onecbcl_q43_p"
                        , "onecbcl_q56c_p"
                        , "onecbcl_q46_p"
                        , "onecbcl_q45_p"
                        , "onecbcl_q47_p"
                        , "onecbcl_q56h_p"
                        , "onecbcl_q54_p"
                        , "onecbcl_q58_p"
                        , "onecbcl_q61_p"
                        , "onecbcl_q62_p"
                        , "onecbcl_q63_p"
                        , "onecbcl_q64_p"
                        , "onecbcl_q56d_p"
                        , "onecbcl_q56e_p"
                        , "onecbcl_q65_p"
                        , "onecbcl_q66_p"
                        , "onecbcl_q67_p"
                        , "onecbcl_q68_p"
                        , "onecbcl_q69_p"
                        , "onecbcl_q71_p"
                        , "onecbcl_q74_p"
                        , "onecbcl_q76_p"
                        , "onecbcl_q77_p"
                        , "onecbcl_q79_p"
                        , "onecbcl_q80_p"
                        , "onecbcl_q56f_p"
                        , "onecbcl_q83_p"
                        , "onecbcl_q84_p"
                        , "onecbcl_q85_p"
                        , "onecbcl_q86_p"
                        , "onecbcl_q87_p"
                        , "onecbcl_q88_p"
                        , "onecbcl_q89_p"
                        , "onecbcl_q90_p"
                        , "onecbcl_q91_p"
                        , "onecbcl_q93_p"
                        , "onecbcl_q94_p"
                        , "onecbcl_q95_p"
                        , "onecbcl_q96_p"
                        , "onecbcl_q50_p"
                        , "onecbcl_q75_p"
                        , "onecbcl_q100_p"
                        , "onecbcl_q102_p"
                        , "onecbcl_q103_p"
                        , "onecbcl_q104_p"
                        , "onecbcl_q56g_p"
                        , "onecbcl_q107_p"
                        , "onecbcl_q109_p"
                        , "onecbcl_q110_p"
                        , "onecbcl_q111_p"
                        , "onecbcl_q112_p"
                        , "onecbcl_q13_p"
                        , "onecbcl_q42_p"
                        , "oneAttacksThreatens"
                        , "oneDestroys"
                        , "oneDisobeysrules"
                        , "oneDistractedHyperactive"
                        , "oneHallucinations"
                        , "onePeerProblems"
                        , "oneSexPlay"
                        , "oneSteals"
                        , "oneWeightproblems")
library(writexl)
library(openxlsx)
write.xlsx(sampleDF, "samplecbclDF1.xlsx")

#read file
samplecbclDF1 <- read.xlsx(file.choose())
