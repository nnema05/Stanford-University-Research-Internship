
### TWO YEAR KSADS DATA
## This program is reading in KSADS data from ABCD dataset
## KSADS is a  diagnostic interview for children mental health symptoms
## KSADS data is used to create a final data set of symptom information organized in a meaningful way
## This is being done for KSADS data from TWO YEAR!


#####STEP 0: LOAD LIBRARIES
library(tidyverse)
library(psych)
library(GPArotation)
library(dplyr)
library(base)
library(naniar)


#####STEP 1: READ IN DATA
ksad <- read.delim("/PATH//abcd_ksad01.txt")
print("Read data")

####Seperate into timelines 
#2_year_follow_up_y_arm_1
ksad <- ksad %>% 
  filter(eventname == "2_year_follow_up_y_arm_1")


######STEP 2: Any_DepDx
#read in needed columns
Any_DepDx <- ksad %>% select(ksads_1_843_p 
                             , ksads_1_845_p
                             , ksads_1_840_p
                             , ksads_1_842_p
                             , ksads_1_847_p
                             , ksads_1_846_p
                             , ksads_1_172_p	
                             , ksads_1_168_p
                             , ksads_1_171_p		
                             , ksads_1_167_p
                             , ksads_1_3_p		
                             , ksads_1_4_p		
                             , ksads_1_156_p		
                             , ksads_1_163_p
                             , ksads_1_164_p		
                             , ksads_1_170_p		
                             , ksads_1_169_p		
                             , ksads_1_188_p
                             , ksads_1_187_p
                             , ksads_1_175_p
                             , ksads_1_176_p
                             , ksads_1_173_p
                             , ksads_1_174_p
                             , ksads_1_185_p
                             , ksads_1_186_p
                             , ksads_1_166_p
                             , ksads_1_181_p
                             , ksads_1_165_p
                             , ksads_1_182_p			
                             , ksads_1_1_p
                             , ksads_1_2_p		
                             , ksads_1_5_p	
                             , ksads_1_6_p		
                             , ksads_1_161_p
                             , ksads_1_162_p
                             , ksads_1_159_p
                             , ksads_1_160_p			
                             , ksads_1_183_p
                             , ksads_1_184_p
                             , ksads_1_180_p
                             , ksads_1_179_p	
                             , ksads_1_157_p	
                             , ksads_1_158_p
                             , ksads_1_177_p
                             , ksads_1_178_p)
print("Any_DepDX selected")

#take out 555 and 888 and replace with na
Any_DepDx <- Any_DepDx %>%
  replace_with_na_all(condition = ~.x == 555)
Any_DepDx <- Any_DepDx %>%
  replace_with_na_all(condition = ~.x == 888)
print("take out 555 and 888 and replace with na")

#make numeric
ncol(Any_DepDx)
Any_DepDx[ , 1:45] <- apply(Any_DepDx[ , 1:45], 2,
                                        function(x) as.numeric(as.character(x))) 
#str(Any_DepDx)
print("make numeric")

#take rowsums statment 
Any_DepDx$sum <- rowSums(Any_DepDx, na.rm = TRUE)
print("rowsums")

##set up ifelse 
Any_DepDx$Any_DepDx <- ifelse(Any_DepDx$sum > 0, 1, 0)
print("ifelse")

#Add to a data frame where all ksads phenotypes will go 
ksadsPhenotype <- data.frame(ksad$subjectkey, ksad$eventname, Any_DepDx$Any_DepDx)
print("add to data frame")
                             


######STEP 3: ANY_SUISH
#read in needed columns
ANY_SUISH <- ksad %>% select(
  ksads_23_946_p
  ,ksads_23_957_p
  ,ksads_23_954_p
  ,ksads_23_965_p
  ,ksads_23_956_p
  ,ksads_23_945_p
  ,ksads_23_950_p
  ,ksads_23_961_p
  ,ksads_23_947_p
  ,ksads_23_958_p
  ,ksads_23_948_p
  ,ksads_23_959_p
  ,ksads_23_949_p
  ,ksads_23_960_p
  ,ksads_23_952_p
  ,ksads_23_963_p
  ,ksads_23_951_p
  ,ksads_23_962_p
  ,ksads_23_953_p
  ,ksads_23_964_p
  ,ksads_23_813_p
  ,ksads_23_822_p
  ,ksads_23_144_p
  ,ksads_23_823_p
  ,ksads_23_814_p
  ,ksads_23_809_p
  ,ksads_23_818_p
  ,ksads_23_825_p
  ,ksads_23_149_p
  ,ksads_23_150_p
  ,ksads_23_812_p
  ,ksads_23_821_p
  ,ksads_23_810_p
  ,ksads_23_819_p
  ,ksads_23_147_p
  ,ksads_23_148_p
  ,ksads_23_811_p
  ,ksads_23_820_p
  ,ksads_23_815_p
  ,ksads_23_824_p)
print("Any_SUISH selected")

#take out 555 and 888 and replace with na
ANY_SUISH <- ANY_SUISH %>%
  replace_with_na_all(condition = ~.x == 555)
ANY_SUISH <- ANY_SUISH %>%
  replace_with_na_all(condition = ~.x == 888)
print("take out 555 and 888 and replace with na")

#make numeric
ncol(ANY_SUISH)
ANY_SUISH[ , 1:40] <- apply(ANY_SUISH[ , 1:40], 2,
                            function(x) as.numeric(as.character(x))) 
#str(ANY_SUISH)
print("make numeric")

#take rowsums statment 
ANY_SUISH$sum <- rowSums(ANY_SUISH, na.rm = TRUE)
print("rowsums")

##set up ifelse 
ANY_SUISH$ANY_SUISH <- ifelse(ANY_SUISH$sum > 0, 1, 0)
print("ifelse")

#Add to a data frame where all ksads phenotypes will go 
ksadsPhenotype <- cbind(ksadsPhenotype, ANY_SUISH$ANY_SUISH)
print("add to data frame")


######STEP 3: ANY_GAD
#read in needed columns
ANY_GAD <- ksad %>% select( 
  ksads_10_869_p
  ,ksads_10_870_p
  ,ksads_10_45_p
  ,ksads_10_46_p
  ,ksads_10_320_p
  ,ksads_10_321_p
  ,ksads_10_325_p
  ,ksads_10_324_p
  ,ksads_10_329_p
  ,ksads_10_328_p
  ,ksads_10_326_p
  ,ksads_10_327_p
  ,ksads_10_323_p
  ,ksads_10_47_p
  ,ksads_10_330_p
  ,ksads_10_322_p)
print("Any_GAD selected")

#take out 555 and 888 and replace with na
ANY_GAD <- ANY_GAD %>%
  replace_with_na_all(condition = ~.x == 555)
ANY_GAD <- ANY_GAD %>%
  replace_with_na_all(condition = ~.x == 888)
print("take out 555 and 888 and replace with na")

#make numeric
ncol(ANY_GAD)
ANY_GAD[ , 1:16] <- apply(ANY_GAD[ , 1:16], 2,
                            function(x) as.numeric(as.character(x))) 
#str(ANY_GAD)
print("make numeric")

#take rowsums statment 
ANY_GAD$sum <- rowSums(ANY_GAD, na.rm = TRUE)
print("rowsums")

##set up ifelse 
ANY_GAD$ANY_GAD <- ifelse(ANY_GAD$sum > 0, 1, 0)
print("ifelse")

#Add to a data frame where all ksads phenotypes will go 
ksadsPhenotype <- cbind(ksadsPhenotype, ANY_GAD$ANY_GAD)
print("add to data frame")


#######STEP 4: Any_SocAnx
#read in needed columns
Any_SocAnx <- ksad %>% select(
  ksads_8_864_p
  ,ksads_8_863_p
  ,ksads_8_309_p
  ,ksads_8_310_p
  ,ksads_8_29_p
  ,ksads_8_31_p
  ,ksads_8_313_p
  ,ksads_8_30_p
  ,ksads_8_312_p
  ,ksads_8_311_p
  ,ksads_8_308_p
  ,ksads_8_304_p
  ,ksads_8_307_p
  ,ksads_8_302_p
  ,ksads_8_303_p
  ,ksads_8_301_p)
print("Any_SocAnx selected")

#take out 555 and 888 and replace with na
Any_SocAnx <- Any_SocAnx %>%
  replace_with_na_all(condition = ~.x == 555)
Any_SocAnx <- Any_SocAnx %>%
  replace_with_na_all(condition = ~.x == 888)
print("take out 555 and 888 and replace with na")

#make numeric
ncol(Any_SocAnx)
Any_SocAnx[ , 1:16] <- apply(Any_SocAnx[ , 1:16], 2,
                          function(x) as.numeric(as.character(x))) 
#str(Any_SocAnx)
print("make numeric")

#take rowsums statment 
Any_SocAnx$sum <- rowSums(Any_SocAnx, na.rm = TRUE)
print("rowsums")

##set up ifelse 
Any_SocAnx$Any_SocAnx <- ifelse(Any_SocAnx$sum > 0, 1, 0)
print("ifelse")

#Add to a data frame where all ksads phenotypes will go 
ksadsPhenotype <- cbind(ksadsPhenotype, Any_SocAnx$Any_SocAnx)
print("add to data frame")


#######STEP 5: Any_SepAnx
#read in needed columns
Any_SepAnx <- ksad %>% select(
  ksads_7_861_p
  ,ksads_7_862_p
  ,ksads_7_300_p
  ,ksads_7_27_p
  ,ksads_7_26_p
  ,ksads_7_288_p
  ,ksads_7_287_p
  ,ksads_7_291_p
  ,ksads_7_292_p
  ,ksads_7_293_p
  ,ksads_7_294_p
  ,ksads_7_289_p
  ,ksads_7_290_p
  ,ksads_7_296_p
  ,ksads_7_295_p
  ,ksads_7_24_p
  ,ksads_7_25_p
  ,ksads_7_281_p
  ,ksads_7_282_p
  ,ksads_7_285_p
  ,ksads_7_286_p
  ,ksads_7_297_p
  ,ksads_7_299_p
  ,ksads_7_298_p
  ,ksads_7_284_p
  ,ksads_7_283_p)
print("Any_SepAnx selected")


#take out 555 and 888 and replace with na
Any_SepAnx <- Any_SepAnx %>%
  replace_with_na_all(condition = ~.x == 555)
Any_SepAnx <- Any_SepAnx %>%
  replace_with_na_all(condition = ~.x == 888)
print("take out 555 and 888 and replace with na")

#make numeric
ncol(Any_SepAnx)
Any_SepAnx[ , 1:26] <- apply(Any_SepAnx[ , 1:26], 2,
                             function(x) as.numeric(as.character(x))) 
#str(Any_SepAnx)
print("make numeric")

#take rowsums statment 
Any_SepAnx$sum <- rowSums(Any_SepAnx, na.rm = TRUE)
print("rowsums")

##set up ifelse 
Any_SepAnx$Any_SepAnx <- ifelse(Any_SepAnx$sum > 0, 1, 0)
print("ifelse")

#Add to a data frame where all ksads phenotypes will go 
ksadsPhenotype <- cbind(ksadsPhenotype, Any_SepAnx$Any_SepAnx)
print("add to data frame")

#######STEP 6: Any_Phobia
#read in needed columns
Any_Phobia <- ksad %>% select(
  ksads_9_868_p
  ,ksads_9_867_p
  ,ksads_9_37_p
  ,ksads_9_38_p
  ,ksads_9_41_p
  ,ksads_9_42_p
  ,ksads_9_43_p
  ,ksads_9_44_p
  ,ksads_9_40_p
  ,ksads_9_39_p
  ,ksads_9_34_p
  ,ksads_9_35_p
  ,ksads_9_36_p)
print("Any_Phobia selected")

#take out 555 and 888 and replace with na
Any_Phobia <- Any_Phobia %>%
  replace_with_na_all(condition = ~.x == 555)
Any_Phobia <- Any_Phobia %>%
  replace_with_na_all(condition = ~.x == 888)
print("take out 555 and 888 and replace with na")

#make numeric
ncol(Any_Phobia)
Any_Phobia[ , 1:13] <- apply(Any_Phobia[ , 1:13], 2,
                             function(x) as.numeric(as.character(x))) 
#str(Any_Phobia)
print("make numeric")

#take rowsums statment 
Any_Phobia$sum <- rowSums(Any_Phobia, na.rm = TRUE)
print("rowsums")

##set up ifelse 
Any_Phobia$Any_Phobia <- ifelse(Any_Phobia$sum > 0, 1, 0)
print("ifelse")

#Add to a data frame where all ksads phenotypes will go 
ksadsPhenotype <- cbind(ksadsPhenotype, Any_Phobia$Any_Phobia)
print("add to data frame")


#######STEP 7: Any_PanicAgorDx
#read in needed columns
Any_PanicAgorDx <- ksad %>% select(
  ksads_5_857_p
  ,ksads_5_858_p
  ,ksads_5_261_p
  ,ksads_5_262_p
  ,ksads_5_264_p
  ,ksads_5_263_p
  ,ksads_5_20_p
  ,ksads_5_21_p
  ,ksads_5_265_p
  ,ksads_5_266_p
  ,ksads_5_267_p
  ,ksads_5_268_p
  ,ksads_5_269_p
  ,ksads_5_270_p
  ,ksads_6_859_p
  ,ksads_6_860_p
  ,ksads_6_275_p
  ,ksads_6_274_p
  ,ksads_6_276_p
  ,ksads_6_278_p
  ,ksads_6_279_p
  ,ksads_6_22_p
  ,ksads_6_23_p
  ,ksads_6_277_p
  ,ksads_6_272_p
  ,ksads_6_273_p)
print("Any_PanicAgorDx selected")

#take out 555 and 888 and replace with na
Any_PanicAgorDx <- Any_PanicAgorDx %>%
  replace_with_na_all(condition = ~.x == 555)
Any_PanicAgorDx <- Any_PanicAgorDx %>%
  replace_with_na_all(condition = ~.x == 888)
print("take out 555 and 888 and replace with na")


#make numeric
ncol(Any_PanicAgorDx)
Any_PanicAgorDx[ , 1:26] <- apply(Any_PanicAgorDx[ , 1:26], 2,
                             function(x) as.numeric(as.character(x))) 
#str(Any_PanicAgorDx)
print("make numeric")

#take rowsums statment 
Any_PanicAgorDx$sum <- rowSums(Any_PanicAgorDx, na.rm = TRUE)
print("rowsums")

##set up ifelse 
Any_PanicAgorDx$Any_PanicAgorDx <- ifelse(Any_PanicAgorDx$sum > 0, 1, 0)
print("ifelse")

#Add to a data frame where all ksads phenotypes will go 
ksadsPhenotype <- cbind(ksadsPhenotype, Any_PanicAgorDx$Any_PanicAgorDx)
print("add to data frame")


######STEP 8: Any_BipolarDx
#read in needed columns
Any_BipolarDx <- ksad %>% select(
  ksads_2_837_p
  ,ksads_2_835_p
  ,ksads_2_836_p
  ,ksads_2_831_p
  ,ksads_2_832_p
  ,ksads_2_830_p
  ,ksads_2_833_p
  ,ksads_2_834_p
  ,ksads_2_839_p
  ,ksads_2_838_p
  ,ksads_2_217_p
  ,ksads_2_218_p
  ,ksads_2_216_p
  ,ksads_2_215_p
  ,ksads_2_207_p
  ,ksads_2_208_p)
print("Any_BipolarDx selected")

#take out 555 and 888 and replace with na
Any_BipolarDx <- Any_BipolarDx %>%
  replace_with_na_all(condition = ~.x == 555)
Any_BipolarDx <- Any_BipolarDx %>%
  replace_with_na_all(condition = ~.x == 888)
print("take out 555 and 888 and replace with na")

#make numeric
ncol(Any_BipolarDx)
Any_BipolarDx[ , 1:16] <- apply(Any_BipolarDx[ , 1:16], 2,
                                  function(x) as.numeric(as.character(x))) 
#str(Any_BipolarDx)
print("make numeric")

#take rowsums statment 
Any_BipolarDx$sum <- rowSums(Any_BipolarDx, na.rm = TRUE)
print("rowsums")


##set up ifelse 
Any_BipolarDx$Any_BipolarDx <- ifelse(Any_BipolarDx$sum > 0, 1, 0)
print("ifelse")

#Add to a data frame where all ksads phenotypes will go 
ksadsPhenotype <- cbind(ksadsPhenotype, Any_BipolarDx$Any_BipolarDx)
print("add to data frame")


######STEP 9:Any_ADHD
#read in needed columns
Any_ADHD <- ksad %>% select(
  ksads_14_856_p
  ,ksads_14_853_p
  ,ksads_14_854_p)
print("Any_ADHD selected")

#take out 555 and 888 and replace with na
Any_ADHD <- Any_ADHD %>%
  replace_with_na_all(condition = ~.x == 555)
Any_ADHD <- Any_ADHD %>%
  replace_with_na_all(condition = ~.x == 888)
print("take out 555 and 888 and replace with na")

#make numeric
ncol(Any_ADHD)
Any_ADHD[ , 1:3] <- apply(Any_ADHD[ , 1:3], 2,
                          function(x) as.numeric(as.character(x))) 
#str(Any_ADHD)
print("make numeric")

#take rowsums statment 
Any_ADHD$sum <- rowSums(Any_ADHD, na.rm = TRUE)
print("rowsums")

##set up ifelse 
Any_ADHD$Any_ADHD <- ifelse(Any_ADHD$sum > 0, 1, 0)
print("ifelse")

#Add to a data frame where all ksads phenotypes will go 
ksadsPhenotype <- cbind(ksadsPhenotype, Any_ADHD$Any_ADHD)
print("add to data frame")


######STEP 10: Any_CD
#read in needed columns
Any_CD <- ksad %>% select(
  ksads_16_900_p
  ,ksads_16_897_p
  ,ksads_16_899_p
  ,ksads_16_898_p)
print("Any_CD selected")

#take out 555 and 888 and replace with na
Any_CD <- Any_CD %>%
  replace_with_na_all(condition = ~.x == 555)
Any_CD <- Any_CD %>%
  replace_with_na_all(condition = ~.x == 888)
print("take out 555 and 888 and replace with na")

#make numeric
ncol(Any_CD)
Any_CD[ , 1:4] <- apply(Any_CD[ , 1:4], 2,
                        function(x) as.numeric(as.character(x))) 
#str(Any_CD)
print("make numeric")


#take rowsums statment 
Any_CD$sum <- rowSums(Any_CD, na.rm = TRUE)
print("rowsums")

##set up ifelse 
Any_CD$Any_CD <- ifelse(Any_CD$sum > 0, 1, 0)
print("ifelse")


#Add to a data frame where all ksads phenotypes will go 
ksadsPhenotype <- cbind(ksadsPhenotype, Any_CD$Any_CD)
print("add to data frame")


######STEP 11: Any_ODD
#read in needed columns
Any_ODD <- ksad %>% select(
  ksads_15_901_p
  ,ksads_15_902_p)
print("Any_ODD selected")

#take out 555 and 888 and replace with na
Any_ODD <- Any_ODD %>%
  replace_with_na_all(condition = ~.x == 555)
Any_ODD <- Any_ODD %>%  
  replace_with_na_all(condition = ~.x == 888)
print("take out 555 and 888 and replace with na")

#make numeric
ncol(Any_ODD)
Any_ODD[ , 1:2] <- apply(Any_ODD[ , 1:2], 2,
                         function(x) as.numeric(as.character(x))) 
#str(Any_ODD)
print("make numeric")

#take rowsums statment 
Any_ODD$sum <- rowSums(Any_ODD, na.rm = TRUE)
print("rowsums")

##set up ifelse 
Any_ODD$Any_ODD <- ifelse(Any_ODD$sum > 0, 1, 0)
print("ifelse")

#Add to a data frame where all ksads phenotypes will go 
ksadsPhenotype <- cbind(ksadsPhenotype, Any_ODD$Any_ODD)
print("add to data frame")


######STEP 12: WRITE INTO CSV
#add event name 
write.csv(ksadsPhenotype, "ksadsPhenotypeTwo.csv")
#JOIN WITH OTHER PHENOTYPES
print("write into csv")