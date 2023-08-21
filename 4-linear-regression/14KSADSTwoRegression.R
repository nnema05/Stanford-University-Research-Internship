
##### Regression and Prediction 
  ## Follows work of study "Differentiated nomological networks of internalizing, externalizing, and the general factor of psychopathology (' p factor') in emerging adolescence in the ABCD study"
## This program creates a final phenotype (predictor variables) file for KSDAS information 
## Then a linear regression model is run between these phenotypes and THE DIFFERENCE BEWTEEN TWO YEAR AND BASELINE p-factor and all 5 factor scores
## The resulting p-value is added to a final result file 

#####STEP 0: LIBRARIES 
library(lme4)
library(lmerTest)
library(lavaan)
library(lavaan.survey)
library(survey)
library(psych)
library(stringr)
library(tidyverse)
library(base)


#####STEP 1: READ DATA 
#Function to extract standardized coefficient estimates from lmer mixed-effects models
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

#Function to read abcd phenotype files
read.abcd = function(file,sep="\t",skip=1,cols=NULL,descriptions=FALSE,descriptions.only=FALSE) {
  headers = names(read.table(file,sep=sep,header=T,stringsAsFactors=F)[-1,])
  if (descriptions) {
    descrip = names(read.table(file,sep=sep,header=T,stringsAsFactors=T,skip=1, na.strings = c("NA", "", " ")))
  }
  data = read.table(file,sep=sep,header=T,stringsAsFactors=T,skip=skip, na.strings = c("NA", "", " "))
  names(data) = headers
  if (!is.null(cols)) {
    data = subset(data,select=cols)
  }
  if (descriptions) {
    if (descriptions.only) {
      temp = data[1,]
      temp[1,] = descrip
      temp
    } else {
      list(data=data,descrip=descrip)
    }
  } else {
    data
  }
}


#Read phenotype data and diff scores and combine
diffScores = read.csv("/PATH/DiffTwoScores.csv")
dat = read.csv("/PATH//finalphenotypes_all.csv", na.strings = c("NA", "", " "))
#select baseline
dat <- dat %>% 
  filter(eventname == "baseline_year_1_arm_1")
dat = merge(dat, diffScores, by.x = c("subjectkey"), by.y=c("basesubjectkey"))
dat <- subset(dat, select = -c(X.x)) 
dat <- subset(dat, select = -c(X.x)) 
dat <- subset(dat, select = -c(X.y)) 
dat <- subset(dat, select = -c(X.y.1)) 


######STEP 2: CONTROLS AND COVARAIATES
#rename values in variables as needed 
dat$RaceEthnicity <- case_when(
  dat$RaceEthnicity == 1 ~ "White",
  dat$RaceEthnicity == 2 ~ "Black",
  dat$RaceEthnicity == 3 ~ "Hispanic",
  dat$RaceEthnicity == 4 ~ "Asian", 
  dat$RaceEthnicity == 5 ~ "Other")
dat$HouseholdMaritalStatus <- case_when(
  dat$HouseholdMaritalStatus == 1 ~ "yes",
  dat$HouseholdMaritalStatus == 2 ~ "no",
  dat$HouseholdMaritalStatus == 3 ~ "no",
  dat$HouseholdMaritalStatus == 4 ~ "no", 
  dat$HouseholdMaritalStatus == 5 ~ "no",
  dat$HouseholdMaritalStatus == 6 ~ "no",
  dat$HouseholdMaritalStatus == 777 ~ NA_character_)
dat$HouseholdIncome <- case_when(
  dat$HouseholdIncome == 1 ~ "[<50K]",
  dat$HouseholdIncome == 2 ~ "[<50K]",
  dat$HouseholdIncome == 3 ~ "[<50K]",
  dat$HouseholdIncome == 4 ~ "[<50K]", 
  dat$HouseholdIncome == 5 ~ "[<50K]",
  dat$HouseholdIncome == 6 ~ "[<50K]",
  dat$HouseholdIncome == 7 ~ "[>=50K & <100K]",
  dat$HouseholdIncome == 8 ~ "[>=50K & <100K]",
  dat$HouseholdIncome == 9 ~ "[>=100K]",
  dat$HouseholdIncome == 10 ~ "[>=100K]",
  dat$HouseholdIncome == 777 ~ NA_character_,
  dat$HouseholdIncome == 999 ~ NA_character_)


# scale sample where to assist lmer convergence (where applicable)
#dat['acs_raked_propensity_score_scaled'] = dat$acs_raked_propensity_score/1000
# set reference levels for categorical variables
dat['gender'] = as.factor(dat[['gender']])
dat['RaceEthnicity'] = as.factor(dat[['RaceEthnicity']])
dat['pcgeduc_recode'] = as.factor(dat[['pcgeduc_recode']])
dat['HouseholdMaritalStatus'] = as.factor(dat[['HouseholdMaritalStatus']])
dat['HouseholdIncome'] = as.factor(dat[['HouseholdIncome']])
dat = within(dat, gender <- relevel(gender, ref = 'M'))
dat = within(dat, RaceEthnicity <- relevel(RaceEthnicity, ref = 'White'))
dat = within(dat, pcgeduc_recode <- relevel(pcgeduc_recode, ref =  "Less than Highschool'"))
dat = within(dat, HouseholdMaritalStatus <- relevel(HouseholdMaritalStatus, ref = 'yes'))
dat = within(dat, HouseholdIncome <- relevel(HouseholdIncome, ref = '[>=100K]'))


# controls and nesting structure for mixed-effects models
controls = c('C(gender)', 'C(RaceEthnicity)', 'C(pcgeduc_recode)',
             'C(HouseholdMaritalStatus)', 'C(HouseholdIncome)', 'Age')
nested_str = ' + (1| site_id_l/rel_family_id)'


# lavaan modelling needs categorical variables to be coded as dummy variables 
race_dummy = dummy.code(dat$RaceEthnicity, na.rm=TRUE)[, c("Hispanic", "Black", "Other", "Asian")]
covariates = cbind('gender_dummy' = dummy.code(dat$gender, na.rm=TRUE)[, c('F')],
                   'race_dummy_1' = race_dummy[, 1], 
                   'race_dummy_2' = race_dummy[, 2],
                   'race_dummy_3' = race_dummy[, 3],
                   'race_dummy_4' = race_dummy[, 4])
educ_dummy = dummy.code(dat$pcgeduc_recode, na.rm=TRUE)[, c("College Degree'", "Masters or Professional Degree'" 
                                                            , "Some college'", " 'Associates or Occupational Degree'"
                                                            , "High School Degree or Equivalent'")]
covariates = cbind(covariates, 
                   'educ_dummy_1' = educ_dummy[, 1],
                   'educ_dummy_2' = educ_dummy[, 2],
                   'educ_dummy_3' = educ_dummy[, 3],
                   'educ_dummy_4' = educ_dummy[, 4],
                   'educ_dummy_5' = educ_dummy[, 5])
covariates = cbind(covariates, 
                   'marital_dummy' = dummy.code(dat$HouseholdMaritalStatus, na.rm=TRUE)[, c('no')])
income_dummy = dummy.code(dat$HouseholdIncome, na.rm=TRUE)[, c('[<50K]', '[>=50K & <100K]')]
covariates = cbind(covariates, 
                   'income_dummy_1' = income_dummy[, 1],
                   'income_dummy_2' = income_dummy[, 2])
dat = cbind(dat, covariates)


# drop rows with missing values or duplicated subject-key's
dat = dat[!is.na(dat$rel_family_id), ]
for (covariate in colnames(covariates)) {
  dat = dat[!is.na(dat[, covariate]), ]
}
#dat = dat[!is.na(dat[, 'acs_raked_propensity_score_scaled']), ]
dat = dat[!duplicated(dat$subjectkey), ]
print(dim(dat))


######STEP 3: PHENOTYPES 
phenotypes_nonksads = c('pps_y_ss_severity_score', 'bpm_t_scr_attention_t', 'bpm_t_scr_internal_t'
                        , 'bpm_t_scr_external_t', 'bis_y_ss_bis_sum'
                        , 'bis_y_ss_bas_rr', 'bis_y_ss_bas_drive'
                        , 'bis_y_ss_bas_fs', 'upps_y_ss_negative_urgency'
                        , 'upps_y_ss_positive_urgency', 'upps_y_ss_lack_of_planning'
                        , 'upps_y_ss_lack_of_perseverance', 'upps_y_ss_sensation_seeking'
                        #, 'G_Dave'
                        , 'srpf_y_ss_ses', 'srpf_y_ss_iiss'
                        , 'srpf_y_ss_dfs', 'psb_y_ss_mean'
                        , 'psb_p_ss_mean', 'fes_p_ss_fc_pr', 'fes_y_ss_fc_pr'
                        , 'pmq_y_ss_mean')
phenotypes_ksads = c('Any_DepDx', 'ANY_SUISH', 'ANY_GAD', 
                     'Any_SocAnx', 'Any_SepAnx', 'Any_Phobia',
                     'Any_PanicAgorDx', 'Any_BipolarDx', 'Any_ADHD',
                     'Any_CD', 'Any_ODD')
phenotypes_demo = c('fh_parent_alcdrug_binary', 'FHtotal', 'adi_weightavg_kind')


#Prepare a dataframe
dat2 = data.frame(dat)
dat2['Zcbcl_scr_syn_internal_t'] = scale(dat$cbcl_scr_syn_internal_t)
dat2['Zcbcl_scr_syn_external_t'] = scale(dat$cbcl_scr_syn_external_t)
dat2['Any_DepDx'] = as.factor(dat[['Any_DepDx']])
dat2['ANY_SUISH'] = as.factor(dat[['ANY_SUISH']])
dat2['ANY_GAD'] = as.factor(dat[['ANY_GAD']])
dat2['Any_SocAnx'] = as.factor(dat[['Any_SocAnx']])
dat2['Any_SepAnx'] = as.factor(dat[['Any_SepAnx']])
dat2['Any_Phobia'] = as.factor(dat[['Any_Phobia']])
dat2['Any_PanicAgorDx'] = as.factor(dat[['Any_PanicAgorDx']])
dat2['Any_BipolarDx'] = as.factor(dat[['Any_BipolarDx']])
dat2['Any_ADHD'] = as.factor(dat[['Any_ADHD']])
dat2['Any_CD'] = as.factor(dat[['Any_CD']])
dat2['Any_ODD'] = as.factor(dat[['Any_ODD']])
dat2 = within(dat2, Any_DepDx <- relevel(Any_DepDx, ref = '0'))
dat2 = within(dat2, ANY_SUISH <- relevel(ANY_SUISH, ref = '0'))
dat2 = within(dat2, ANY_GAD <- relevel(ANY_GAD, ref = '0'))
dat2 = within(dat2, Any_SocAnx <- relevel(Any_SocAnx, ref = '0'))
dat2 = within(dat2, Any_SepAnx <- relevel(Any_SepAnx, ref = '0'))
dat2 = within(dat2, Any_Phobia <- relevel(Any_Phobia, ref = '0'))
dat2 = within(dat2, Any_PanicAgorDx <- relevel(Any_PanicAgorDx, ref = '0'))
dat2 = within(dat2, Any_BipolarDx <- relevel(Any_BipolarDx, ref = '0'))
dat2 = within(dat2, Any_ADHD <- relevel(Any_ADHD, ref = '0'))
dat2 = within(dat2, Any_CD <- relevel(Any_CD, ref = '0'))
dat2 = within(dat2, Any_ODD <- relevel(Any_ODD, ref = '0'))

# declare the phenotype set we're interested in modelling
# 'phenotypes' below can be any one of the three list of phenotypes defined above
phenotypes = phenotypes_ksads #phenotypes_nonksads  #phenotypes_demo

######STEP 4: DEPENDENTS 
dependents = c('DiffTwoPFactor', 'DiffTwoFactor1', 'DiffTwoFactor2', 
               'DiffTwoFactor3', 'DiffTwoFactor4', 'DiffTwoFactor5')

######STEP 5: LINEAR MODEL AND OUTPUT 
# column names of output (should not need to be changed)
default_res_names = c('dependent', 'phenotype', 'raw_beta', 'raw_pvalue', 'raw_lower_ci', 'raw_upper_ci', 'std_beta', 'std_pvalue', 'std_lower_ci', 'std_upper_ci')
n_done = 0
full_res = c()
fdr_raw_pvals = c()
fdr_std_pvals = c()
for (dependent in dependents) {
  dep_raw_pvals = c()
  dep_std_pvals = c()
  for (phenotype in phenotypes) {
    covariateDepFact <- case_when(
      dependent == 'DiffOnePFactor' ~ "basePFactor",
      dependent == 'DiffOneFactor1' ~ "baseFactor1", 
      dependent == 'DiffOneFactor2' ~ "baseFactor2",
      dependent == 'DiffOneFactor3' ~ "baseFactor3",
      dependent == 'DiffOneFactor4' ~ "baseFactor4",
      dependent == 'DiffOneFactor5' ~ "baseFactor5")
    dat2 = cbind(dat2, covariateDepFact)
    fmla = paste(dependent, ' ~ ', phenotype, ' + ', 
                 paste(controls[controls!='C(gender)'], collapse=' + '),
                 nested_str, sep='')
    model = lmer(formula=fmla, data=dat2, 
                 na.action='na.exclude') 
    #weights = acs_raked_propensity_score_scaled)
    std_model = stdCoef.merMod(model)
    pt = paste(phenotype, "1", sep = "")
    raw_se = coef(summary(model))[pt, 'Std. Error']
    std_se = std_model[phenotype, 'stdse']
    
    
    lavaan_raw_beta = fixef(model)[[pt]]
    lavaan_std_beta = std_model[phenotype, 'stdcoef']
    lavaan_pval_raw = coef(summary(model))[pt, 'Pr(>|t|)']
    lavaan_pval_std = coef(summary(model))[pt, 'Pr(>|t|)']
    lavaan_lowerci_raw = lavaan_raw_beta - (qnorm(0.975)*raw_se)
    lavaan_lowerci_std = lavaan_std_beta - (qnorm(0.975)*std_se)
    lavaan_upperci_raw = lavaan_raw_beta + (qnorm(0.975)*raw_se)
    lavaan_upperci_std = lavaan_std_beta + (qnorm(0.975)*std_se)
    res = cbind(dependent,
                phenotype,
                lavaan_raw_beta,
                lavaan_pval_raw,
                lavaan_lowerci_raw,
                lavaan_upperci_raw,
                lavaan_std_beta,
                lavaan_pval_std,
                lavaan_lowerci_std,
                lavaan_upperci_std)
    colnames(res) = default_res_names
    full_res = rbind(full_res, res)
    
    dep_raw_pvals = c(dep_raw_pvals, lavaan_pval_raw)
    dep_std_pvals = c(dep_std_pvals, lavaan_pval_std)
    n_done = n_done + 1
    print(paste('(', n_done, '/', length(phenotypes)*length(dependents), ')   Done ', phenotype, ' - ', dependent, '...', sep=''))
  }
  # adjust pvalues using FDR
  fdr_raw_pvals = c(fdr_raw_pvals, p.adjust(dep_raw_pvals, method='fdr'))
  fdr_std_pvals = c(fdr_std_pvals, p.adjust(dep_std_pvals, method='fdr'))
}
print(warnings())

colnames(full_res) = default_res_names
# include FDR adjust pvalues
#full_res <- full_res[-c(1), ]

# include FDR adjust pvalues
full_res <- cbind(full_res, FDR_raw_pvalue = fdr_raw_pvals)
full_res <- cbind(full_res, FDR_std_pvalue = fdr_std_pvals)
#full_res$FDR_raw_pvalue <- fdr_raw_pvals
#full_res$FDR_std_pvalue <- fdr_std_pvals
#full_res[, 'FDR_raw_pvalue'] = fdr_raw_pvals
#full_res[, 'FDR_std_pvalue'] = fdr_std_pvals


######STEP 6 :SAVE AS CSV
write.csv(full_res, file='14TwoKSADSRes.csv', quote=FALSE, row.names=FALSE)


