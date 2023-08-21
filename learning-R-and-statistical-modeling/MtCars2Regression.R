mtcars


stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

phenotype = 'am_dummy1'
dependent = 'mpg'
controls = 'C(am)'

mtcars2=mtcars

race <-c("Hispanic", "Black", "Other", "Asian")

as.factor(race)

mtcars['am'] = as.factor(mtcars[['am']])

am_dummy = dummy.code(mtcars$am, na.rm = TRUE)[, c("0", "1")]
covariates = cbind('am_dummy1' = am_dummy[,1], 
                   'am_dummy2' = am_dummy[,2])
mtcars2 <- cbind(mtcars, covariates)
str(mtcars2)
nested_str = '+ (1| vs)'


default_res_names = c('dependent', 'phenotype', 'control' ,'raw_beta', 'raw_pvalue', 'raw_lower_ci', 'raw_upper_ci', 'std_beta', 'std_pvalue', 'std_lower_ci', 'std_upper_ci')
n_done = 0
full_res = c()
fdr_raw_pvals = c()
fdr_std_pvals = c()

fmla = paste(dependent, ' ~ ', phenotype, ' + ', 
             paste(controls, collapse= ' + '),
             nested_str, sep='')
model = lmer(formula=fmla, data= mtcars2, 
             na.action='na.exclude')
std_model = stdCoef.merMod(model)
raw_coef= coef(summary(model))
raw_coef[phenotype, 'Std. Error']

x<-coef(summary(model))
x <- as.data.frame(raw_coef)
controsl_val=row.names(x)
rn[3:]



raw_se_controls = coef(summary(model))[rn[3], 'Std. Error']

std_se = std_model[phenotype, 'stdse']
std_se_controls = std_model[rn[3], 'stdse']




lavaan_raw_beta = fixef(model)[[phenotype]]
lavaan_raw_beta_controls = fixef(model)[[controls]]
lavaan_std_beta = std_model[phenotype, 'stdcoef']
controls
lavaan_std_beta_controls = std_model[controls,'stdcoef']
lavaan_pval_raw = coef(summary(model))[phenotype, 'Pr(>|t|)']
lavaan_pval_std = coef(summary(model))[phenotype, 'Pr(>|t|)']
lavaan_pval_raw_controls = coef(summary(model))[rn[3], 'Pr(>|t|)']
lavaan_pval_std_controls = coef(summary(model))[rn[3], 'Pr(>|t|)']
lavaan_lowerci_raw = lavaan_raw_beta - (qnorm(0.975)*raw_se)
lavaan_lowerci_raw_controls= lavaan_raw_beta_controls - (qnorm(0.975)*raw_se1)
lavaan_lowerci_std = lavaan_std_beta - (qnorm(0.975)*std_se)
lavaan_lowerci_std_controls = lavaan_std_beta_controls - (qnorm(0.975)*std_se1)
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
res_controls = cbind(dependent,
            controls,
            lavaan_raw_beta,
            lavaan_pval_raw,
            lavaan_lowerci_raw,
            lavaan_upperci_raw,
            lavaan_std_beta_controls,
            lavaan_pval_std,
            lavaan_lowerci_std,
            lavaan_upperci_std)
colnames(res) = default_res_names
full_res = rbind(full_res, res,res_controls)
print(std_model)

dep_raw_pvals = c(dep_raw_pvals, lavaan_pval_raw)
dep_std_pvals = c(dep_std_pvals, lavaan_pval_std)
n_done = n_done + 1
print(paste('(', n_done, '/', length(phenotype)*length(dependent), ')   Done ', phenotype, ' - ', dependent, '...', sep=''))