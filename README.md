# Stanford University Research Internship

Interning under Stanford University Professor of Behavioral Sciences, Dr. Kaustubh Supekar, I undertook a study  whose goal was understand development of mental health disorders by attempting to both examine and predict it. Specifically, we worked to see how children from ages 9-12 and their symptoms of mental disorders change over time and if this change can be predicted by a number of variables. These predictors include demographics, psychopathology, temperament, family history of substance use and psychopathology, school and family environment, and cognitive ability, using instruments based on youth-, parent-, and teacher-report, and behavioral task performance. 

## Folder explanation
All code files in order with numbers in front of it!  

**1-reading-in-data**  
These programs reads in and cleans up the Child Behavior Checklist data from the ABCD data set, that each subject in the ABCD study completes  

**2-confirmatory-factor-analysis**  
-The following programs in this folder complete Confirmatory Factor Analysis on the Child Behavior Checklist data  
-A study called "Delineating and validating higher-order dimensions of psychopathology in the ABCD study" created a 5 factor dimensional structure of pyschopathology for us  
-We wanted to first test and verify this factor structure for our set of observed variables in the ABCD data set actually exists  
-This led to the use LONGITUDNAL CONFIRMATORY FACTOR ANALYSIS (CFA)  
-For this, we ran a longitudinal CFA with an error structure, and four different models: configural, metric, scalar, and residual  
-We then figured out which model of CFA the best fit best based on conventional fit thresholds like CFI, TLI, RMSEA  
 - Configural was the best fit  
We used the CFA factor scores from the configural model and found the difference between these scores
 - difference between baseline scores and one year scores for each factor and p-factor
 - difference between baseline scores and two year scores for each factor and p-factor
 - Knowing these differences allows us to see what the change in mental health symptoms is between the baseline and the one year mark and two year mark  


**3-preparring-for-regression**  
-Now that we have the difference in scores between baseline and the one year mark and two year mark, we want to perform linear regression on these scores to see if any variables can be a significant predictor of the difference in scores  
-The following programs in this folder prepare for linear regression by grabbing possible predictors from the ABCD data set  
-These predictors include family history, the child's information for the K-SADS test (
Kiddie Schedule for Affective Disorders and Schizophrenia), family history with drug and alcohol use, the child's Area Deprivation Index (ADI) and the child's information from the American Community survey (ACS)  

**4-linear-regression**  
-The programs in this folder perform linear regression for
 -  the phenotypes and baseline data for all of the factor scores
 -  the phenotypes and the difference of one year and baseline data for all of the factor scores
 - the phenotypes and the difference of two year and baseline data for all of the factor scores  
-The resulting p-value to show the association between the phenotypes and factor scores is added to result files  


**5-results**  
-The programs in this folder put all the p-values in two easily readable csv's, one for the difference in one year and baseline data and one for the difference of two year and baseline data  

