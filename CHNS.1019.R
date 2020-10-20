setwd("D:/CHNS")
library(haven)
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)

#****************************Data Accessing and Prepossessing****************************

# Covariates

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
education <- education[which(education$hiedu<=6),]
base <- left_join(base, education, by = c('Idind'='IDind','wave'='WAVE'))

## Total Energy
macronutrients <- read_sas("Master_Macronutrients_201410/c12diet.sas7bdat")
macnut <- macronutrients[,c('IDind', 'wave', 'd3kcal')]
macnut <- macnut[which(macnut$d3kcal<9999),]
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
htwt <- htwt[which(htwt$HEIGHT>0),]
htwt <- htwt[which(htwt$WEIGHT>0),]
htwt$BMI <- htwt$WEIGHT/((htwt$HEIGHT*0.01)*(htwt$HEIGHT*0.01))
base <- left_join(base, htwt, by = c('Idind'='IDind','wave'='WAVE'))

## Smoking and alcohol
smokenalcohol <- subset(pexam_pub_12,select=c(IDind,WAVE,U25,U40))
colnames(smokenalcohol) <- c('IDind', 'WAVE', 'smoked', 'drank')
smokenalcohol <- smokenalcohol[which(smokenalcohol$smoked<=1),]
smokenalcohol <- smokenalcohol[which(smokenalcohol$drank<=1),]
base <- left_join(base, smokenalcohol, by = c('Idind'='IDind','wave'='WAVE'))

## Physical Activity
pact_12 <- read_sas("Master_PE_PA_201908/pact_12.sas7bdat")
pa1<-pact_12[,c('IDind', "WAVE", "U140","U140_MN","U141","U141_MN","U142","U142_MN","U128","U129_MN","U126A","U126B","U127A_MN","U127B_MN","U124","U125_MN","U325","U326_MN",
                "U145","U146_MN","U149","U329_MN","U330_MN","U147","U331_MN","U332_MN","U151","U333_MN","U334_MN","U153",
                "U335_MN","U336_MN","U155","U337_MN","U338_MN")]
timea_12 <- read_sas("Master_TimeUse_201804/timea_12.sas7bdat")
pa2<-timea_12[,c('IDind', "WAVE", "K2","K3","K4","K5","K6","K7","K7B","K7C")]
pa<-merge(pa1, pa2,by=c('Idind'='IDind','wave'='WAVE'))
pa$Mets<-NA
pa[is.na(pa)]<-0
attach(pa)
pa$Mets<-(U140_MN*2+U141_MN*4+U142_MN*6+U129_MN*3+U127A_MN*4+U127B_MN*4+U125_MN*1.5+U326_MN*1.5+U146_MN*4.5
+U329_MN*5+U330_MN*5+U331_MN*7.5+U332_MN*7.5+U333_MN*6+U334_MN*6+U335_MN*5+U336_MN*5+U337_MN*5+U338_MN*5
+2.3*K3+2.25*K5+2.15*K7+3*K7C)/60

## Plant-based Diet Indices - `PDIs_melt` from Diet.R
base <- left_join(base, PDIs_melt, by = c('Idind'='IDind','wave'='WAVE'))

# Diabetes

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

## Summary of the dataset and objects selection // needs revision
summary(base)
complete_data <- base[which(base$wave>=1997),]
baseline <- base[which(base$wave==1997),]

#****************************Data Analysis and Visualization****************************

# Baseline Charicteristics
