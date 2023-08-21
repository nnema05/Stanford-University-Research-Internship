#Testing regression 
mtcars
library(lme4)
library(lmerTest)
library(lavaan)
library(lavaan.survey)
library(survey)
library(psych)
library(stringr)

#Function to extract standardized coefficient estimates from lmer mixed-effects models
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

#sd -gets standard deviation 
#apply - Returns a vector or array or list of values obtained by applying a function to margins of an array or matrix.
#here SD is getting the componstns from object x, column and standrad deviaion
#getMe - Extract or Get Generalize Components from a Fitted Mixed Effects Model
#fixef Extract the estimates of the fixed-effects parameters from a fitted model.
#coef- coef is a generic function which extracts model coefficients from objects returned by modeling functions.
#Std. eror The standard error is a statistical term that measures the accuracy with which a sample distribution represents a population by using standard deviation. In statistics, a sample mean deviates from the actual mean of a population; this deviation is the standard error of the mean.

##Data 
mtcars
#Question, what can predict mpg (weight and cyl), control am
head(mtcars)
##Covariates and conrtols and random effects
#conrtols
controls = 'C(am)'
print(controls)

#covariates
mtcars['am'] = as.factor(mtcars[['am']])
am_dummy = dummy.code(mtcars$am, na.rm = TRUE)[, c("0", "1")]
covariates = cbind('am_dummy1' = am_dummy[,1], 
                   'am_dummy2' = am_dummy[,2])
mtcars2 = cbind(mtcars, covariates)

#random effects
nested_str = '+ (1| vs)'

##Predictors/phenotypess
phenotypes <- c('wt', 'cyl', 'am_dummy1')
print(phenotypes)

###Depedents 
dependents <- c('mpg')
print(dependents)

##linear model and output 
default_res_names = c('dependent', 'phenotype', 'raw_beta', 'raw_pvalue', 'raw_lower_ci', 'raw_upper_ci', 'std_beta', 'std_pvalue', 'std_lower_ci', 'std_upper_ci')
print(default_res_names)
n_done = 0
full_res = c()
fdr_raw_pvals = c()
fdr_std_pvals = c()
for (dependent in dependents) {
    print(dependent)
    dep_raw_pvals = c()
    dep_std_pvals = c()
    for (phenotype in phenotypes) {
      print(phenotype)
      fmla = paste(dependent, ' ~ ', phenotype, ' + ', 
                   paste(controls, collapse=' + '),
                   nested_str, sep='')
      model = lmer(formula=fmla, data=mtcars2, 
                   na.action='na.exclude')
      std_model = stdCoef.merMod(model)
      raw_se = coef(summary(model))[phenotype, 'Std. Error']
      std_se = std_model[phenotype, 'stdse']
      
      lavaan_raw_beta = fixef(model)[[phenotype]]
      lavaan_std_beta = std_model[phenotype, 'stdcoef']
      #lavaan_std_beta_controls = std_model[controls,'stdcoef']
      lavaan_pval_raw = coef(summary(model))[phenotype, 'Pr(>|t|)']
      lavaan_pval_std = coef(summary(model))[phenotype, 'Pr(>|t|)']
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
                  #lavaan_std_beta_controls,
                  lavaan_pval_std,
                  lavaan_lowerci_std,
                  lavaan_upperci_std)
      colnames(res) = default_res_names
      full_res = rbind(full_res, res)
      print(std_model)
      
      dep_raw_pvals = c(dep_raw_pvals, lavaan_pval_raw)
      dep_std_pvals = c(dep_std_pvals, lavaan_pval_std)
      n_done = n_done + 1
      print(paste('(', n_done, '/', length(phenotypes)*length(dependents), ')   Done ', phenotype, ' - ', dependent, '...', sep=''))
    }
    # adjust pvalues using FDR
    fdr_raw_pvals = c(fdr_raw_pvals, p.adjust(dep_raw_pvals, method='fdr'))
    fdr_std_pvals = c(fdr_std_pvals, p.adjust(dep_std_pvals, method='fdr'))
  }
  

colnames(full_res) = default_res_names
# include FDR adjust pvalues
full_res[, 'FDR_raw_pvalue'] = fdr_raw_pvals
full_res[, 'FDR_std_pvalue'] = fdr_std_pvals

write.csv(full_res, file = "testlmer.csv", quote=FALSE, row.names=FALSE)

newTest <- read.csv("testlmer.csv")
print(full_res)

column = c('high', )
mtcars3 

