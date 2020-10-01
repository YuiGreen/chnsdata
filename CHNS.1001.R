setwd("D:/CHNS")
library(haven)
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
library(dplyr)

# Baseline Characteristic

## DoB and gender
surveys_pub_12 <- read_sas("Master_ID_201908/surveys_pub_12.sas7bdat")
mast_pub_12 <- read_sas("Master_ID_201908/mast_pub_12.sas7bdat")
survey <- surveys_pub_12[,c('Idind', 'hhid', 'age', 'wave')]
mast <- mast_pub_12[,c('Idind', 'GENDER', 'WEST_DOB_Y')]
survey <- left_join(survey, mast, by = c('Idind' = 'Idind'))
base <- na.omit(survey)

## Education
educ_12 <- read_sas("Master_Educ_201804/educ_12.sas7bdat")
education <- educ_12[,c('IDind', "WAVE", "A12")]
colnames(education) <- c('IDind', 'WAVE', 'hiedu')
base <- left_join(base, education, by = c('Idind'='IDind','wave'='WAVE'))

## Total Energy
macronutrients <- read_sas("Master_Macronutrients_201410/c12diet.sas7bdat")
macnut <- macronutrients[,c('IDind', 'wave', 'd3kcal')]
base <- left_join(base, macnut, by = c('Idind'='IDind','wave'='wave'))

## Household Income Per Capita
hhinc_10 <- read_sas("Master_Constructed_Income_201804/hhinc_10.sas7bdat")
hhinc <- data.frame(cbind(hhinc_10$hhid, hhinc_10$WAVE, hhinc_10$hhincpc_cpi))
colnames(hhinc) <- c('IDhh', 'WAVE', 'income_inflated')
base <- left_join(base, hhinc, by = c('hhid'='IDhh', 'wave'='WAVE'))

## BMI derived from height and weight
pexam_pub_12 <- read_sas("Master_PE_PA_201908/pexam_pub_12.sas7bdat")
pexam_pub_12$U56[is.na(pexam_pub_12$U56)]<-9
htwt <- subset(pexam_pub_12,U56!="1",select=c(IDind,WAVE,HEIGHT,WEIGHT))
htwt$BMI <- htwt$WEIGHT/((htwt$HEIGHT*0.01)*(htwt$HEIGHT*0.01))
base <- left_join(base, htwt, by = c('Idind'='IDind','wave'='WAVE'))

## Smoking and alcohol
smokenalcohol <- subset(pexam_pub_12,select=c(IDind,WAVE,U25,U40))
colnames(smokenalcohol) <- c('IDind', 'WAVE', 'smoked', 'drank')
base <- left_join(base, smokenalcohol, by = c('Idind'='IDind','wave'='WAVE'))

## Physical Activity


## A summary on the base dataset
summary(base)
base <- filter(base, wave>=1997)

# Data Cleaning



# Food Intake

## Loading Food Intake Data
nutr3_00 <- read_sas("Master_Nutrition_201410/nutr3_00.sas7bdat")
head(nutr3_00)
nutr3_00$IDind <- as.character(nutr3_00$IDind)
head(nutr3_00)
food_intake <- data.frame(cbind(nutr3_00$IDind, nutr3_00$WAVE, nutr3_00$V39, nutr3_00$FOODCODE))
colnames(food_intake) <- c('IDind', 'WAVE', 'intake', 'foodcode')
food_intake <- filter(food_intake,WAVE>=1997)

## Conversion from `foodcode` to `foodgroup`
## Note: 2 coding systems are used after 1997



## Calculating Plant-based Diet Score (`PBDS`)



# Diabetes Diagnosis

## Loading Diabetes data
diabetes <- subset(pexam_pub_12,select=c(IDind, WAVE, U24A, U24B))
diabetes <- diabetes[which(!is.na(diabetes$U24A)),]
diabetes <- diabetes[which(diabetes$U24A!=9),]
diabetes <- diabetes[-which(diabetes$U24B<0),]
colnames(diabetes) <- c('IDind', 'WAVE', 'Diabetes', 'age_dia')

## Calculating year of diagnosis (`Year_dia`)
diabetes <- left_join(diabetes, base[,c('Idind', 'wave', 'WEST_DOB_Y')], by=c('IDind'='Idind', 'WAVE'='wave'))
diabetes$Year_dia <- diabetes$age_dia + diabetes$WEST_DOB_Y
summary(diabetes)

