## Set up environment
setwd("D:/CHNS")
library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(ggplot2)


## Load Food Intake Data
nutr3_00 <- read_sas("Master_Nutrition_201410/nutr3_00.sas7bdat")
nutr3_00$IDind <- as.character(nutr3_00$IDind)
food_intake <- data.frame(cbind(nutr3_00$IDind, nutr3_00$WAVE, nutr3_00$T7_DF, nutr3_00$V40, nutr3_00$V39, nutr3_00$FOODCODE))
colnames(food_intake) <- c('IDind', 'WAVE', 'day', 'mealtime', 'intake', 'foodcode')
food_intake <- filter(food_intake,WAVE>=1997)


## Construct foodcode dictionary of FCT1991 named `foodcode_1991.csv`
# construct_fc91 <- function(){
#   grain_91 <- cbind(c(1001:1084, 21001:21029, 21032:21035, 21050:21104), 'grain')
#   whole_grain_91 <- cbind(c(1003, 1019, 1055, 1057, 1058, 1068:1071, 1084, 1075:1077, 25001:25009), 'whole_grain')
#   vegetable_oil_91 <- cbind(c(20001:20010, 20013, 20017, 20018, 20021), 'vegetable_oil')
#   fresh_fruit_91 <- cbind(c(6001:6016, 6018:6021, 6025:6033, 10001:10155), 'fresh_fruit')
#   vegetable_91 <- cbind(c(4001:4049, 5001:5079, 6017, 6022:6024, 7001:7014, 9001:9033), 'vegetable')
#   legume_91 <- cbind(c(2001:2078, 3001:3020, 21030, 21031), 'legume')
#   nut_91 <- cbind(c(11001:11030), 'nut')
#   tea_91 <- cbind(c(22006:22015), 'tea')
#   ref_grain_91 <- cbind(setdiff(grain_91[,1], whole_grain_91[,1]), 'ref_grain')
#   potato_91 <- cbind(c(4027:4032), 'potato')
#   sweet_91 <- cbind(c(24001:24022), 'sweet')
#   ssb_91 <- cbind(c(22001:22005, 22016:22044), 'ssb')
#   preserved_91 <- cbind(c(8001:8035), 'preserved')
#   animal_fat_91 <- cbind(c(20011, 20012, 20014, 20015, 20016, 20019, 20020, 14001), 'animal_fat')
#   meat_91 <- cbind(c(12001:12136, 13001:13061), 'meat')
#   fish_sea_91 <- cbind(c(17001:17071, 18001:18042), 'fish_sea')
#   egg_91 <- cbind(c(16001:16020), 'egg')
#   dairy_91 <- cbind(c(14002:14036), 'dairy')
#   foodcode_1991 <- rbind(whole_grain_91, vegetable_oil_91, fresh_fruit_91, vegetable_91, legume_91,
#                          nut_91, tea_91, ref_grain_91, potato_91, sweet_91, ssb_91, preserved_91, animal_fat_91,
#                          meat_91, fish_sea_91, egg_91, dairy_91)
#   colnames(foodcode_1991) <- c('foodcode', 'foodgroup')
#   foodcode_1991 <- data.frame(foodcode_1991)
#   write.csv(foodcode_1991, 'foodcode_1991.csv', row.names = FALSE)
# }

## Foodcode dictionary of FCT2002 was constructed in MS Excel named `foodcode_2002.csv`


## Load foodcode dictionaries
foodcode_1991 <- read_csv("foodcode_1991.csv", col_types = cols(foodcode = col_character()))
foodcode_2002 <- read_csv("foodcode_2002.csv", col_types = cols(foodcode = col_character()))


## Summarise individual intake by foodgroup
sum_intake <- function(intake_data, code_dict){
  intake_data <- left_join(intake_data, code_dict, by = 'foodcode')
  intake_data$intake  <- as.numeric(intake_data$intake)
  intake_sum <- na.omit(intake_data)  %>%
    group_by(IDind, WAVE, foodgroup) %>%
    summarise(intake=sum(intake))
}
food_intake_fct91 <- filter(food_intake, WAVE<2004)
food_intake_fct02 <- filter(food_intake, WAVE>=2004)
food_intake_fct91_sum <- sum_intake(food_intake_fct91, foodcode_1991)
food_intake_fct91_sum$intake <- food_intake_fct91_sum$intake*50 # Converting the unit to gram
food_intake_fct02_sum <- sum_intake(food_intake_fct02, foodcode_2002)
food_intake_sum <- rbind(food_intake_fct91_sum, food_intake_fct02_sum)
food_intake_sum <- spread(food_intake_sum, foodgroup, intake)
food_intake_sum[is.na(food_intake_sum)] <- 0
food_intake_sum <- melt(food_intake_sum, id.vars=c("IDind","WAVE"), variable.name="foodgroup", value.name="intake")
foodgroup <- factor(food_intake_sum$foodgroup)

## Calculate quintiles of each food group and join them to intake_sum
quintiles <- tapply(food_intake_sum$intake, foodgroup, function(x) {return(quantile(x,probs=c(0,0.2,0.4,0.6,0.8,1)))})
quintiles <- data.frame(matrix(unlist(quintiles), nrow=17, byrow=T),stringsAsFactors=FALSE)
quintiles$foodgroup <- c('animal_fat', 'dairy', 'egg', 'fish_sea', 'fresh_fruit', 'legume',
                         'meat', 'nut', 'potato', 'preserved', 'ref_grain', 'ssb', 'sweet',
                         'tea', 'vegetable', 'vegetable_oil', 'whole_grain')
food_intake_quin <- left_join(food_intake_sum, quintiles, by = "foodgroup")


## Calculating Plant-based Diet Indices
# healthy_plant <- c('tea', 'vegetable', 'vegetable_oil', 'whole_grain', 'nut', 'legume', 'fresh_fruit')
# unhealthy_plant <- c('potato', 'preserved', 'ref_grain', 'ssb', 'sweet')
# animal <- c('animal_fat', 'dairy', 'egg', 'fish_sea', 'meat')
food_intake_quin$score <- 1
food_intake_quin[which(food_intake_quin$intake>food_intake_quin$X2), 'score'] <- 2
food_intake_quin[which(food_intake_quin$intake>food_intake_quin$X3), 'score'] <- 3
food_intake_quin[which(food_intake_quin$intake>food_intake_quin$X4), 'score'] <- 4
food_intake_quin[which(food_intake_quin$intake>food_intake_quin$X5), 'score'] <- 5
food_intake_quin_short <- food_intake_quin[,c('IDind','WAVE','foodgroup', 'score')]

food_intake_spread <- spread(food_intake_quin_short, foodgroup, score)
food_intake_spread$heal_plant <- food_intake_spread$tea + food_intake_spread$vegetable + food_intake_spread$vegetable_oil + food_intake_spread$whole_grain + food_intake_spread$nut + food_intake_spread$legume + food_intake_spread$fresh_fruit
food_intake_spread$unheal_plant <- food_intake_spread$potato + food_intake_spread$preserved + food_intake_spread$ref_grain + food_intake_spread$ssb + food_intake_spread$sweet
food_intake_spread$animal <- food_intake_spread$animal_fat + food_intake_spread$dairy + food_intake_spread$egg + food_intake_spread$fish_sea + food_intake_spread$meat
food_intake_spread$PDI <- food_intake_spread$heal_plant+food_intake_spread$unheal_plant-food_intake_spread$animal+30
food_intake_spread$hPDI <- food_intake_spread$heal_plant-food_intake_spread$unheal_plant-food_intake_spread$animal+60
food_intake_spread$uPDI <- -food_intake_spread$heal_plant+food_intake_spread$unheal_plant-food_intake_spread$animal+72

PDIs <- food_intake_spread[, c('IDind', 'WAVE', 'PDI', 'hPDI', 'uPDI')]
summary(PDIs)
ggplot(PDIs, aes(x=factor(WAVE))) + geom_boxplot(aes(y=PDI))
# PDI <- dcast(PDIs[, c('IDind', 'WAVE', 'PDI')], IDind ~ WAVE)
# hPDI <- dcast(PDIs[, c('IDind', 'WAVE', 'hPDI')], IDind ~ WAVE)
# uPDI <- dcast(PDIs[, c('IDind', 'WAVE', 'uPDI')], IDind ~ WAVE)
