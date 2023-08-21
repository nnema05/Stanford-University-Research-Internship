
#P-factor loading comparison 
PFactorComparison <- read.delim(file.choose(), sep = ",")
view(PFactorComparison)
cor.test(PFactorComparison$MyPFactorLoading, PFactorComparison$Dloading)
cor(PFactorComparison$MyPFactorLoading, PFactorComparison$Dloading)


#P-factor scores comparison 
baselineFa1 <- fa(r = baselineDataNumeric, 
                  nfactors = 1, 
                  fm = "ml",
                  residuals = TRUE) 
baselinefa1Scores <- baselineFa1$scores
#Cor.test with our EFA p-factor scores and the delineating scores
cor.test(baselinefa1Scores, ) #add delineating factor1scores


#5-factor score comparison 
baselineFa5 <- fa(r = baselineDataNumeric,
                  nfactors = 5,
                  fm = "ml",
                  rotate = "geominT",
                  residuals = TRUE)
baselinefa5scores <- baselineFa5$scores

#Cor.test with our EFA 5-factor scores and the delineating scores
cor.test(baselinefa1Scores, ) # add delineating 5 factor scores
