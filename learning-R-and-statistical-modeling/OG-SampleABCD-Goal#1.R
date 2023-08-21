#Sample ABCD - Goal #1
setwd("/Users/nandininema/Downloads/Sample ABCD Work")

#STEP 1.open data that has removed supplemental materials
library(tidyverse)
library(psych)
library(multicon) #need to install this package for composite function
updated.data <- read.csv("cbcl_nm_updated_1.csv")
head(updated.data)
ncol(updated.data) #119, (1:4 and 119 should not be changed to numeric)
view(updated.data) 
attach(updated.data)

#STEP 2. aggregate needed items into composites
is.data.frame(updated.data) #check if it is a data frame

#Attacks/threatens (“Physically attacks people”, “Threatens people”)
#removes first row to start making everything numeric
updated.data <- updated.data[2:29685,] 

#convert character to numeric

#Attach updated.data to the script 
attach(updated.data)

#counting number of columns 
ncol(updated.data)

#To continue, everything must be numeric
#Columns that must be numeric are 5:118 so those will become their own data.frame
columns.to.numeric <- c(5:118)
view(columns.to.numeric)

#changes 5:118 to numeric but keeps everything else as characters 
updated.data[ , columns.to.numeric] <- apply(updated.data[ ,columns.to.numeric], 2,
                                            function(x) as.numeric(as.character(x))) 
head(updated.data)
str(columns.to.numeric) #check what type of data it is numeric?
str(updated.data) # characters and numeric

#Manually delete “Drinks alcohol without parents' approval”, “Sexual problems”, “Smokes, chews, or sniffs tobacco”, “Truancy, skips school”, “Uses drugs for non-medical purposes (don't include alcohol or tobacco)”.

#create composite 
#Attacks/threatens (“Physically attacks people”, “Threatens people”)
updated.data$`Attacks/threatens` <- rowMeans(updated.data[,c("cbcl_q57_p", "cbcl_q97_p")],
                                           na.rm = TRUE)
view(updated.data$`Attacks/threatens`)

#Destroys (“Destroys his/her own things”, “Destroys things belonging to his/her family or others”, “Vandalism”)
updated.data$Destroys <- rowMeans(updated.data[,c("cbcl_q20_p", "cbcl_q21_p", "cbcl_q106_p")],
                                             na.rm = TRUE)
#Disobeys rules (“Disobedient at home”, “Disobedient at school”, “Breaks rules at home, school or elsewhere”)
updated.data$`Disobeys rules` <- rowMeans(updated.data[,c("cbcl_q22_p", "cbcl_q23_p", "cbcl_q28_p")],
                                  na.rm = TRUE)
#Steals (“Steals at home”, “Steals outside the home”)
updated.data$Steals <- rowMeans(updated.data[,c("cbcl_q81_p", "cbcl_q82_p")],
                                          na.rm = TRUE)
# Peer problems (“Doesn't get along with other kids”, “Not liked by other kids”)
updated.data$`Peer problems` <- rowMeans(updated.data[,c("cbcl_q25_p", "cbcl_q48_p")],
                                          na.rm = TRUE)
#Distracted/Hyperactive (“Can't concentrate, can't pay attention for long”, “Inattentive or easily distracted”, “Can't sit still, restless, or hyperactive”)
updated.data$`Distracted/Hyperactive` <- rowMeans(updated.data[,c("cbcl_q08_p", "cbcl_q78_p", "cbcl_q10_p")],
                                         na.rm = TRUE)
#Hallucinations (“Hears sound or voices that aren't there”, “Sees things that aren't there”)
updated.data$Hallucinations <- rowMeans(updated.data[,c("cbcl_q40_p", "cbcl_q70_p")],
                                                  na.rm = TRUE)
#Sex play (“Plays with own sex parts in public”, “Plays with own sex parts too much”)
updated.data$`Sex play` <- rowMeans(updated.data[,c("cbcl_q59_p", "cbcl_q60_p")],
                                        na.rm = TRUE)
#Weight problems (“Overeating”, “Overweight”)
updated.data$`Weight problems` <- rowMeans(updated.data[,c("cbcl_q53_p", "cbcl_q55_p")],
                                    na.rm = TRUE)

ncol(updated.data) #128  
#Remove aggregated columns
updated.data <- subset(updated.data, select = -c(cbcl_q57_p, cbcl_q97_p)) #attacks/threatens
updated.data <- subset(updated.data, select = -c(cbcl_q20_p)) #destroys
updated.data <- subset(updated.data, select = -c(cbcl_q21_p, cbcl_q106_p)) #destroys
updated.data <- subset(updated.data, select = -c(cbcl_q22_p, cbcl_q23_p, cbcl_q28_p)) #Disobeys
updated.data <- subset(updated.data, select = -c(cbcl_q81_p, cbcl_q82_p)) #steals 
updated.data <- subset(updated.data, select = -c(cbcl_q25_p, cbcl_q48_p)) #peer problems
updated.data <- subset(updated.data, select = -c(cbcl_q08_p, cbcl_q78_p, cbcl_q10_p)) #Distracted/Hyperactive
updated.data <- subset(updated.data, select = -c(cbcl_q40_p, cbcl_q70_p)) #Hallucinations
updated.data <- subset(updated.data, select = -c(cbcl_q59_p, cbcl_q60_p)) #Sex play 
updated.data <- subset(updated.data, select = -c(cbcl_q53_p, cbcl_q55_p)) #Weight problems 
ncol(updated.data)#107 - 5(subject key, interview age, sex, language, event name) = 102 

#STEP 3. Separate into different time lines (baseline_year_1_arm_1 , 1_year_follow_up_y_arm_1, 2_year_follow_up_y_arm_1 )
head(updated.data)

#baseline_year_1_arm_1
baseline.data <- updated.data %>% 
  filter(eventname == "baseline_year_1_arm_1")
view(baseline.data)
nrow(baseline.data)

# 1_year_follow_up_y_arm_1
oneyear.data <- updated.data %>% 
  filter(eventname == "1_year_follow_up_y_arm_1")
view(oneyear.data)
nrow(oneyear.data)

#2_year_follow_up_y_arm_1
twoyear.data <- updated.data %>% 
  filter(eventname == "2_year_follow_up_y_arm_1")
view(twoyear.data)
nrow(twoyear.data)

#STEP 4. EFA and hierarchical method 
#STEP 4a. Parallel analyses
baseline.paralell <- fa.parallel(baseline.data) #data must be numeric
str(baseline.data) #numeric and character
ncol(baseline.data) #107, numeric is 5:97, 99:107
baseline.data.numeric <- select(baseline.data, c(5:97, 99:107)) #removing all variables that are not numeric
str(baseline.data.numeric) 
head(baseline.data.numeric)
view(baseline.data.numeric) 

#running parallel analysis 
#use fa = pa because the article recommends finding principal components (pc) instead of principal factors (fa) 
baseline.parallel <- fa.parallel(baseline.data.numeric, fa = "pc") #ERROR Error in eigen(rx) : infinite or missing values in 'x'
plot()

#Test if code is working 
test <- read.csv("pca_gsp.csv")
test1 <- select(test, c(2:13))


testmatrix <- data.matrix(test1, rownames.force = NA)
fa.multi.diagram(fa.multi(testmatrix, nfactors = 2, nfact2 = 1, n.iter = 5))
fa.multi.diagram(fa.multi(testmatrix, nfactors = 3, nfact2 = 2, n.iter = 5))

testcor <- cor(test1)
testcor
testGoldberg <- bassAckward(r = testmatrix, nfactors = 5,
                            fm = "pca", rotate = "geominT", 
                            use = "pairwise",
                            items = TRUE,
                            sort = TRUE)
summary(testGoldberg)
itemsfa(testGoldberg)

summary(testGoldberg)
library(psychTools)
fa.lookup(testGoldberg,dictionary=psychTools::bfi.dictionary)


testGoldberg$loadings
fa.sort(testGoldberg, 102)
test.parallel <- fa.parallel(test1)

#STEP 4b PCA (delineating paper stated to empirically extract with PCA)
baseline.pca <- princomp(baseline.data.numeric) #ERROR Error in cov.wt(z) : 'x' must contain finite values only

#Test if code is working
test.pca <- princomp(test1)
plot(test.pca)

#Assumption: parallel analysis gave me 16 factors, pca gave me 5 based on delinating paper 

#STEP 4c: EFA for each factor, extract and rotate 
#FIRST FACTOR
#factanal method:
baseline.fa.1 <- factanal(baseline.data.numeric, 
                          factors = 1,
                          scores = "regression")
#Retry factor analysis to keep column names
baseline.fa.1 <- factanal(select(baseline.data, c(5:97, 99:107)), 
                          factors = 1,
                          scores = "regression")

#fa method: unsure as to what fm will equal 
baseline.fa.1 <- fa(baseline.data.numeric,
                    nfactors = 1,
                    fm = ? ,
                    scores = "regression")

#evaluate loading (need to be presence of >3 clear primary loading, greater then .1 for each factor)
#save scores for each successive number of components 
baseline.fa.1.scores <- baseline.fa.1$scores

#Test if code is working 

#use geominT which is an orthoganal rotation and the goldberg article uses an orthogonal rotation as well
#geomin because of delinating article
#THIS IS THE RIGHT FA METHOD
test.fa <- fa(r = test1, 
              nfactors = 4, 
              fm = "ml",
              rotate = "geominT",
              residuals = TRUE) 
loadings(test.fa)
#loadings correlate with variables (factors are ML1, ML2, ML4)
#h2 is how much of the variance is explained by the factors
#so age is largely explained, best explained is the h2 thing
#com term tells you how many variables contrubute to the variable, want 1


install.packages("GPArotation")
library("GPArotation")
test.fa <- factanal(test1, 
              factors = 3,
              scores = "regression",
              rotation = geominT) #doesnt have rotation 
test.fa
test.fa.scores <- test.fa$scores
typeof(test.fa.scores)
view(test)
#convert the matrix of test.fa.scores to a data frame
test.fa.scores.df <- as.data.frame(test.fa.scores)
#make dataframe of just states
test.df <- select(test, c(State))
#combine both data frames 
test1 <- cbind(test.df, test.fa.scores.df)
test2 <- cbind(test.df, test.fa.scores.df)

#OR
library(sqldf)
#inner join -SUBTRACTED
subtracted.test <- sqldf("SELECT State, test1.Factor1 as test1Factor1
                                      , test2.Factor1 as test2Factor1
                                      , test1.Factor1 - test2.Factor1 as diff1
                                      , test1.Factor2 as test1Factor2
                                      , test2.Factor2 as test2Factor2
                                      , test1.Factor2 - test2.Factor2 as diff2
                                      , test1.Factor3 as test1Factor3
                                      , test2.Factor3 as test2Factor3
                                      , test1.Factor3 - test2.Factor3 as diff3
                                      , test1.Factor4 as test1Factor4
                                      , test2.Factor4 as test2Factor4
                                      , test1.Factor4 - test2.Factor4 as diff4
                         FROM test1
                         OUTER JOIN test2 USING(State)")
#put in excel 
install.packages("writexl")
library("writexl")
write_xlsx(subtracted.test, "/Users/nandininema/Downloads/Sample ABCD Work/testresults.xlsx")




#TWO FACTORS (now rotate)
baseline.fa.2 <- factanal(baseline.data.numeric, 
                          factors = 2, 
                          rotation = "geomin",
                          scores = "regression")
baseline.fa.2.scores <- baseline.fa.2$scores

#THREE FACTORS 
baseline.fa.3 <- factanal(baseline.data.numeric, 
                          factors = 3, 
                          rotation = "geomin",
                          scores = "regression")
baseline.fa.3.scores <- baseline.fa.3$scores

#FOUR FACTORS 
baseline.fa.4 <- factanal(baseline.data.numeric, 
                          factors = 4, 
                          rotation = "geomin",
                          scores = "regression")
baseline.fa.4.scores <- baseline.fa.4$scores

#FIVE FACTORS
baseline.fa.5 <- factanal(baseline.data.numeric, 
                          factors = 5, 
                          rotation = "geomin",
                          scores = "regression")
baseline.fa.5.scores <- baseline.fa.5$scores

#SIX FACTORS  
baseline.fa.6 <- factanal(baseline.data.numeric, 
                          factors = 6, 
                          rotation = "geomin",
                          scores = "regression")
baseline.fa.6.scores <- baseline.fa.6$scores
#Assumption that at 6 factors and beyond it is no longer interpretable

#Names of 5 factors based on loadings: 

#STEP 5: CORRELATION ANALYSIS
#correlation between factor scores starting from the first factor at the top and down

#1 and 2 scores 
baseline.cor.1_2 <- cor.test(baseline.fa.1.scores, baseline.fa.2.scores, 
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

#2 and 3 scores
baseline.cor.2_3 <- cor.test(baseline.fa.2.scores, baseline.fa.3.scores,
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

#3 and 4 scores
baseline.cor.3_4 <- cor.test(baseline.fa.3.scores, baseline.fa.4.scores, 
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

#4 and 5 scores 
baseline.cor.4_5 <- cor.test(baseline.fa.4.scores, baseline.fa.5.scores, 
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

