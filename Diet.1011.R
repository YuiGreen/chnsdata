setwd("D:/CHNS")
library(haven)
library(dplyr)
library(readr)


## Load Food Intake Data
nutr3_00 <- read_sas("Master_Nutrition_201410/nutr3_00.sas7bdat")
head(nutr3_00)
nutr3_00$IDind <- as.character(nutr3_00$IDind)
head(nutr3_00)
food_intake <- data.frame(cbind(nutr3_00$IDind, nutr3_00$WAVE, nutr3_00$T7_DF, nutr3_00$V40, nutr3_00$V39, nutr3_00$FOODCODE))
colnames(food_intake) <- c('IDind', 'WAVE', 'day', 'mealtime', 'intake', 'foodcode')
food_intake <- filter(food_intake,WAVE>=1997)

## Construct foodcode dictionary of FCT1991
construct_fc91 <- function(){
  grain_91 <- cbind(c(1001:1084, 21001:21029, 21032:21035, 21050:21104), 'grain')
  whole_grain_91 <- cbind(c(1003, 1019, 1055, 1057, 1058, 1068:1071, 1084, 1075:1077, 25001:25009), 'whole_grain')
  vegetable_oil_91 <- cbind(c(20001:20010, 20013, 20017, 20018, 20021), 'vegetable_oil')
  fresh_fruit_91 <- cbind(c(6001:6016, 6018:6021, 6025:6033, 10001:10155), 'fresh_fruit')
  vegetable_91 <- cbind(c(4001:4049, 5001:5079, 6017, 6022:6024, 7001:7014, 9001:9033), 'vegetable')
  legume_91 <- cbind(c(2001:2078, 3001:3020, 21030, 21031), 'legume')
  nut_91 <- cbind(c(11001:11030), 'nut')
  tea_91 <- cbind(c(22006:22015), 'tea')
  ref_grain_91 <- cbind(setdiff(grain_91[,1], whole_grain_91[,1]), 'ref_grain')
  potato_91 <- cbind(c(4027:4032), 'potato')
  sweet_91 <- cbind(c(24001:24022), 'sweet')
  ssb_91 <- cbind(c(22001:22005, 22016:22044), 'ssb')
  preserved_91 <- cbind(c(8001:8035), 'preserved')
  animal_fat_91 <- cbind(c(20011, 20012, 20014, 20015, 20016, 20019, 20020, 14001), 'ani_fat')
  meat_91 <- cbind(c(12001:12136, 13001:13061), 'meat')
  fish_sea_91 <- cbind(c(17001:17071, 18001:18042), 'fish_sea')
  egg_91 <- cbind(c(16001:16020), 'egg')
  dairy_91 <- cbind(c(14002:14036), 'dairy')
  foodcode_1991 <- rbind(whole_grain_91, vegetable_oil_91, fresh_fruit_91, vegetable_91, legume_91,
                         nut_91, tea_91, ref_grain_91, potato_91, sweet_91, ssb_91, preserved_91, animal_fat_91,
                         meat_91, fish_sea_91, egg_91, dairy_91)
  colnames(foodcode_1991) <- c('foodcode', 'foodgroup')
  foodcode_1991 <- data.frame(foodcode_1991)
  write.csv(foodcode_1991, 'foodcode_1991.csv')
}

## Group by foodgroup in FCT91
foodcode_1991 <- read_csv("foodcode_1991.csv", col_types = cols(foodcode = col_character()))
food_intake_fct91 <- filter(food_intake, WAVE<2004)
food_intake_fct91 <- left_join(food_intake_fct91, foodcode_1991, by = 'foodcode')
food_intake_fct91$intake <- as.numeric(food_intake_fct91$intake)
fct91_abnormal <- food_intake_fct91[food_intake_fct91$intake>10,]
write.csv(fct91_abnormal, 'fct91_abnormal.csv')
### needed cleaning
food_intake_fct91_sum <- na.omit(food_intake_fct91)  %>%
  group_by(IDind, WAVE, foodgroup) %>%
  ### the parament in group_by can be replaced to observe by mealtime/day/3days
  summarise(n = n(), intake=sum(intake))
foodgroup <- as.factor(food_intake_fct91_sum$foodgroup)
food_intake_fct91_sum$intake <- food_intake_fct91_sum$intake*50

## Foodcode dictionary of FCT2002 was constructed in MS Excel named `foodcode_2002.csv`

## Group by foodgroup in FCT02
foodcode_2002 <- read_csv("foodcode_2002.csv", col_types = cols(foodcode = col_character()))
food_intake_fct02 <- filter(food_intake, WAVE>=2004)
food_intake_fct02 <- left_join(food_intake_fct02, foodcode_2002, by = 'foodcode')
food_intake_fct02_sum <- na.omit(food_intake_fct02)  %>%
  group_by(IDind, WAVE, foodgroup) %>%
  ### the parament in group_by can be replaced to observe by mealtime/day/3days
  summarise(n = n(), intake=sum(intake))

food_intake_sum <- rbind(food_intake_fct91_sum, food_intake_fct02_sum)
tapply(food_intake_sum$intake, foodgroup, function(x) {return(quantile(x,probs=c(0,0.2,0.4,0.6,0.8,1)))})

## Calculating Plant-based Diet Index
