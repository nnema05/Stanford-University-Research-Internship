
### BASELINE DATA OF FAMILY HISTORY OF ALCOHOL/DRUG USE
## This program is reading in data about family history of alcohol and drug use from ABCD dataset
## This data is used to create a final data set of this information organized in a meaningful way
## This is being done for Fanily history of alcohol and drug use data from BASELINE!


#####STEP 0: LOAD LIBRARIES
library(tidyverse)
library(psych)
library(GPArotation)
library(dplyr)
library(base)
library(naniar)

####STEP 1: MERGE THE TWO NEEDED FILES FOR FAMILY HISTORY OF PSYCHOPATHOLOGY 
print("READING DATA")
fhxp102 <- read.delim("/PATH//fhxp102_d.txt")
fhxp201 <- read.delim("/PATH//fhxp201_d.txt")

fhxp <- full_join(fhxp102, fhxp201, by = "subjectkey", keep = FALSE)
print("READ AND MERGED DATA")

#seperate into timeline
fhxp <- fhxp %>% 
  filter(eventname == "baseline_year_1_arm_1")
print("Different timeline")

#deal with Nas
fhxp <- fhxp %>%
  replace_with_na_all(condition = ~.x == 999)
print("NA REPLACED")

#make numeric
fhxp[ , 10:1450] <- apply(fhxp[ , 10:1450], 2,
                          function(x) as.numeric(as.character(x))) 
print("MADE NUMERIC")

write.csv(fhxp, "fhxp.csv")
print("WRITE INTO CSV")


#####STEP 2: SELECT NEEDED COLUMNS
fh_parent_alcdrug_binary <- fhxp %>% 
  select(famhx_4d_p___4
         , famhx_4d_p___6
         , famhx_4d_p___3
         , famhx4a_p___3
         , famhx4a_p___4
         , famhx4a_p___6
        # , fam_history_q5a_drugs___888
        # , fam_history_q5d_drugs___888
         , fam_history_q5a_drugs___3
         , fam_history_q5a_drugs___4
         , fam_history_q5a_drugs___6
         , fam_history_q5d_drugs___3
         , fam_history_q5d_drugs___4
         , fam_history_q5d_drugs___6)
print("SELECT NEEDED COLUMNS")


######STEP 3: ROWSUMS STATMENT TO SET UP FOR IF ELSE STATEMENT 
fh_parent_alcdrug_binary$sum <- rowSums(fh_parent_alcdrug_binary, na.rm = TRUE)
print("ROW SUMS")

#####STEP 4: IFELSE STATMENT 
##A threshold is established: parent says yes (1) to the above variables...
##...representing serious substance use problems

##If yes is said for any variable for any parent (d is mother a is father) then the... 
###...threshold is met
##If threshold is met complete column fh_parent_alcdrug_binary is coded as ...
##0 = neither parent met the threshold; 1 = one or more parents met the threshold

fh_parent_alcdrug_binary$fh_parent_alcdrug_binary <- ifelse(fh_parent_alcdrug_binary$sum > 0, 1, 0)
print("IFELSE")

######STEP 4: ADD SUBJECT KEY AND EVENT NAME
fh_parent_alcdrug_binary <- cbind(subjectkey = fhxp$subjectkey, eventname = fhxp$eventname, fh_parent_alcdrug_binary)

######STEP 5: WRITE INTO CSV
write.csv(fh_parent_alcdrug_binary, "fh_parent_alcdrug_binary.csv")
