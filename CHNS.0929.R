setwd("D:/CHNS")
library(haven)
library(dplyr)
library(ggplot2)

# Baseline Characteristic

## Import DoB and gender
surveys_pub_12 <- read_sas("Master_ID_201908/surveys_pub_12.sas7bdat")
mast_pub_12 <- read_sas("Master_ID_201908/mast_pub_12.sas7bdat")
survey <- surveys_pub_12[,c('Idind', 'hhid', 'age', 'wave')]
mast <- mast_pub_12[,c('Idind', 'GENDER', 'WEST_DOB_Y')]
survey <- left_join(survey, mast, by = c('Idind' = 'Idind'))
base <- na.omit(survey)

## Import education data
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

## Height and weight



# Import food intake data
nutr3_00 <- read_sas("Master_Nutrition_201410/nutr3_00.sas7bdat")
head(nutr3_00)
nutr3_00$IDind <- as.character(nutr3_00$IDind)
head(nutr3_00)
food_intake <- data.frame(cbind(nutr3_00$IDind, nutr3_00$WAVE, nutr3_00$V39, nutr3_00$FOODCODE))
colnames(food_intake) <- c('IDind', 'WAVE', 'intake', 'foodcode')


# Import diabetes data
pexam_pub_12 <- read_sas("Master_PE_PA_201908/pexam_pub_12.sas7bdat")
diabetes <- subset(pexam_pub_12,select=c(IDind, WAVE, U24A, U24B))
diabetes <- diabetes[which(!is.na(diabetes$U24A)),]
summary(diabetes)
diabetes <- diabetes[which(diabetes$U24A!=9),]
diabetes <- diabetes[-which(diabetes$U24B<0),]
hist(diabetes$U24B)
