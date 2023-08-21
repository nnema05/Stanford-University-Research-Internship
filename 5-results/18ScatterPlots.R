

#### SCATTERPLOTS
# This program creates scatter plots showing the relationship...
# ...between phenotypes and difference in one year and baseline factor scores
#... and between phenotypes and difference in two year and baseline factor scores


######STEP 0: LIBRARIES
library(ggplot2)
library(tidyverse)

#####STEP 1: READ AND COMBINE DATA
diffOneScores = read.csv("/PATH/DiffOneScores.csv")
diffTwoScores = read.csv("/PATH/DiffTwoScores.csv")
dat = read.csv("/PATH//finalphenotypes_all.csv", na.strings = c("NA", "", " "))
dat <- dat %>% 
  filter(eventname == "baseline_year_1_arm_1")
dat = merge(dat, diffOneScores, by.x = c("subjectkey"), by.y=c("basesubjectkey"))
dat <- subset(dat, select = -c(X.x)) 
dat <- subset(dat, select = -c(X.x)) 
dat <- subset(dat, select = -c(X.y)) 
dat <- subset(dat, select = -c(X.y.1))
dat = merge(dat, diffTwoScores, by.x = c("subjectkey"), by.y=c("basesubjectkey"))


#####STEP 2: CREATE SCATTER PLOTS 
#attach(mtcars)
#plot(wt, mpg, main="Scatterplot Example",
   #  xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
#attach(dat)


#######STEP 2: P-FACTOR SCATTER PLOTS
#####ONE YEAR
###One year P-factor and ADHD
jpeg("18pDiffOneADHD.jpg")
ggplot(dat, aes(x=Any_ADHD, y=DiffOnePFactor)) +
  geom_point() +
  ggtitle("Diff One year P-factor and ADHD") + 
  xlab("Baseline ADHD") +
  ylab("DiffOneP")
dev.off() 
#ggplot(dat, aes(x = Any_ADHD, y = DiffOnePFactor)) + 
 # geom_point(position = position_dodge(width = 0.4))
#plot <- plot(Any_BipolarDx, DiffOnePFactor, main="One year P-factor and Bipolar",
#      xlab="Bipolar", ylab="DiffOnep", pch=19)

###One year P-factor and Bipolar
jpeg("18pDiffOneBipolar.jpg")
ggplot(dat, aes(x=Any_BipolarDx, y=DiffOnePFactor)) +
  geom_point() +
  ggtitle("Diff One year P-factor and Bipolar") + 
  xlab("Baseline Bipolar") +
  ylab("DiffOneP")
dev.off() 

###One year P-factor and Generalized Anxiety
jpeg("18pDiffOneGAD.jpg")
ggplot(dat, aes(x=ANY_GAD, y=DiffOnePFactor)) +
  geom_point() +
  ggtitle("Diff One year P-factor and GAD") + 
  xlab("Baseline GAD") +
  ylab("DiffOneP")
dev.off() 

###One year P-factor and Oppositional defiant disorder
jpeg("18pDiffOneODD.jpg")
ggplot(dat, aes(x=Any_ODD, y=DiffOnePFactor)) +
  geom_point() +
  ggtitle("Diff One year P-factor and ODD") + 
  xlab("Baseline ODD") +
  ylab("DiffOneP")
dev.off() 

###One year P-factor and Social Anxiety
jpeg("18pDiffOneSocAnx.jpg")
ggplot(dat, aes(x=Any_SocAnx, y=DiffOnePFactor)) +
  geom_point() +
  ggtitle("Diff One year P-factor and Social Anxiety") + 
  xlab("Baseline Social Anxiety") +
  ylab("DiffOneP")
dev.off() 

###One year P-factor and Specific Phobia
jpeg("18pDiffOnePhobia.jpg")
ggplot(dat, aes(x=Any_Phobia, y=DiffOnePFactor)) +
  geom_point() +
  ggtitle("Diff One year P-factor and Specific Phobia") + 
  xlab("Baseline Specific Phobia") +
  ylab("DiffOneP")
dev.off() 

###One year P-factor and Prosocial 
jpeg("18pDiffOneProSocial.jpg")
ggplot(dat, aes(x=psb_p_ss_mean, y=DiffOnePFactor)) +
  geom_point() +
  ggtitle("Diff One year P-factor and Prosocial") + 
  xlab("Baseline Prosocial") +
  ylab("DiffOneP")
dev.off() 

#####TWO YEAR
###Two year P-factor and Prosocial 
jpeg("18pDiffTwoProSocial.jpg")
ggplot(dat, aes(x=psb_p_ss_mean, y=DiffTwoPFactor)) +
  geom_point() + 
  ggtitle("Diff Two year P-factor and Prosocial") + 
  xlab("Baseline Prosocial") +
  ylab("DiffTwoP")
dev.off() 

#######STEP 3: INT SCATTER PLOTS - FACTOR 2

#####ONE YEAR
###One year INT and Generalized Anxiety
jpeg("18INTDiffOneGAD.jpg")
ggplot(dat, aes(x=ANY_GAD, y=DiffOneFactor2)) +
  geom_point() +
  ggtitle("Diff One year INT and GAD") + 
  xlab("Baseline GAD") +
  ylab("DiffOneINT")
dev.off() 

###One year INT and Oppositional defiant disorder
jpeg("18INTDiffOneODD.jpg")
ggplot(dat, aes(x=Any_ODD, y=DiffOneFactor2)) +
  geom_point() +
  ggtitle("Diff One year INT and ODD") + 
  xlab("Baseline ODD") +
  ylab("DiffOneINT")
dev.off() 

###One year INT and Social Anxiety
jpeg("18INTDiffOneSocAnx.jpg")
ggplot(dat, aes(x=Any_SocAnx, y=DiffOneFactor2)) +
  geom_point() +
  ggtitle("Diff One year INT and Social Anxiety") + 
  xlab("Baseline Social Anxiety") +
  ylab("DiffOneINT")
dev.off() 

###One year INT and Separation Anxiety
jpeg("18INTDiffOneSepAnx.jpg")
ggplot(dat, aes(x=Any_SepAnx, y=DiffOneFactor2)) +
  geom_point() +
  ggtitle("Diff One year INT and Separation Anxiety") + 
  xlab("Baseline Separation Anxiety") +
  ylab("DiffOneINT")
dev.off() 

###One year INT and Specific Phobia
jpeg("18INTDiffOnePhobia.jpg")
ggplot(dat, aes(x=Any_Phobia, y=DiffOneFactor2)) +
  geom_point() +
  ggtitle("Diff One year INT and Specific Phobia") + 
  xlab("Baseline Specific Phobia") +
  ylab("DiffOneINT")
dev.off() 

#####TWO YEAR
##Not significantly associated with anything 

#######STEP 4: EXT SCATTER PLOTS - FACTOR 1

###One year EXT and ADHD
jpeg("18EXTDiffOneADHD.jpg")
ggplot(dat, aes(x=Any_ADHD, y=DiffOneFactor1)) +
  geom_point() +
  ggtitle("Diff One year EXT and ADHD") + 
  xlab("Baseline ADHD") +
  ylab("DiffOneEXT")
dev.off()

###One year EXT and Generalized Anxiety
jpeg("18EXTDiffOneGAD.jpg")
ggplot(dat, aes(x=ANY_GAD, y=DiffOneFactor1)) +
  geom_point() +
  ggtitle("Diff One year EXT and GAD") + 
  xlab("Baseline GAD") +
  ylab("DiffOneEXT")
dev.off() 

###One year EXT and Oppositional defiant disorder
jpeg("18EXTDiffOneODD.jpg")
ggplot(dat, aes(x=Any_ODD, y=DiffOneFactor1)) +
  geom_point() +
  ggtitle("Diff One year EXT and ODD") + 
  xlab("Baseline ODD") +
  ylab("DiffOneEXT")
dev.off() 

###One year EXT and Social Anxiety
jpeg("18EXTDiffOneSocAnx.jpg")
ggplot(dat, aes(x=Any_SocAnx, y=DiffOneFactor1)) +
  geom_point() +
  ggtitle("Diff One year EXT and Social Anxiety") + 
  xlab("Baseline Social Anxiety") +
  ylab("DiffOneEXT")
dev.off() 

###One year EXT and Specific Phobia
jpeg("18EXTDiffOnePhobia.jpg")
ggplot(dat, aes(x=Any_Phobia, y=DiffOneFactor1)) +
  geom_point() +
  ggtitle("Diff One year EXT and Specific Phobia") + 
  xlab("Baseline Specific Phobia") +
  ylab("DiffOneEXT")
dev.off() 

###One year EXT and Prosocial 
jpeg("18EXTDiffOneProSocial.jpg")
ggplot(dat, aes(x=psb_p_ss_mean, y=DiffOneFactor1)) +
  geom_point() + 
  ggtitle("Diff One year EXT and Prosocial") + 
  xlab("Baseline Prosocial") +
  ylab("DiffOneEXT")
dev.off() 


#####TWO YEAR
###Two year EXT and Prosocial 
jpeg("18EXTDiffTwoProSocial.jpg")
ggplot(dat, aes(x=psb_p_ss_mean, y=DiffTwoFactor1)) +
  geom_point() + 
  ggtitle("Diff Two year EXT and Prosocial") + 
  xlab("Baseline Prosocial") +
  ylab("DiffTwoEXT")
dev.off() 
