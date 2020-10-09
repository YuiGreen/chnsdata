setwd("D:/CHNS")
library(haven)
library(dplyr)


## Loading Food Intake Data
nutr3_00 <- read_sas("Master_Nutrition_201410/nutr3_00.sas7bdat")
head(nutr3_00)
nutr3_00$IDind <- as.character(nutr3_00$IDind)
head(nutr3_00)
food_intake <- data.frame(cbind(nutr3_00$IDind, nutr3_00$WAVE, nutr3_00$T7_DF, nutr3_00$V40, nutr3_00$V39, nutr3_00$FOODCODE))
colnames(food_intake) <- c('IDind', 'WAVE', 'day', 'mealtime', 'intake', 'foodcode')
food_intake <- filter(food_intake,WAVE>=1997)

## Constructing foodcode dictionary of FCT1997
grain_91 <- cbind(c(1001:1084), 'grain')
whole_grain_91 <- cbind(c(1003, 1019, 1055, 1057, 1058, 1068:1071, 1084, 1075:1077), 'whole_grain')
vegetable_oil_91 <- cbind(c(20001:20010, 20013, 20017, 20018, 20021), 'vegetable_oil')
fresh_fruit_91 <- cbind(c(6001:6016, 6018:6021, 6025:6033, 10001:10155), 'fresh_fruit')
vegetable_91 <- cbind(c(4001:4049, 5001:5079, 6017, 6022:6024, 7001:7014, 9001:9033), 'vegetable')
bean_91 <- cbind(c(2001:2078, 3001:3020), 'bean')
garlic_91 <- cbind(c(5018:5020), 'garlic')
nut_91 <- cbind(c(11001:11030), 'nut')
tea_91 <- cbind(c(22006:22015), 'tea')
ref_grain_91 <- cbind(setdiff(grain_91, whole_grain_91), 'ref_grain')
sugar_91 <- cbind(c(24001:24022), 'sugar')
salt_pre <- cbind(c(8001:8035), 'salt_pre')
animal_fat_91 <- cbind(c(20011, 20012, 20014, 20015, 20016, 20019, 20020, 14001), 'ani_fat')
meat_91 <- cbind(c(12001:12136, 13001:13061), 'meat')
fish_91 <- cbind(c(17001:17071), 'fish')
egg_91 <- cbind(c(16001:16020), 'egg')
milk_91 <- cbind(c(14002:14036), 'milk')

foodcode_1991 <- rbind(whole_grain_91, vegetable_oil_91, fresh_fruit_91, vegetable_91, bean_91,
                       garlic_91, nut_91, tea_91, ref_grain_91, sugar_91, salt_pre, animal_fat_91,
                       meat_91, fish_91, egg_91, milk_91)
colnames(foodcode_1991) <- c('foodcode', 'foodgroup')
foodcode_1991 <- data.frame(foodcode_1991)

## Group by foodgroup in FCT91
food_intake_fct91 <- filter(food_intake, WAVE<2004)
food_intake_fct91 <- left_join(food_intake_fct91, foodcode_1991, by = 'foodcode')
food_intake_fct91_sum <- food_intake_fct91  %>%
  group_by(IDind, WAVE, foodgroup) %>%
  summarise(n = n())



## Defining food groups in FCT02


## Constructing foodcode dictionary of FCT2002


## Group by foodgroup in FCT02
food_intake_fct02 <- filter(food_intake, WAVE>=2004)
food_intake_fct02 <- left_join(food_intake_fct02, foodcode_2002, by = 'foodcode')
food_intake_fct02_sum <- food_intake_fct02  %>%
  group_by(IDind, WAVE, foodgroup) %>%
  summarise(n = n())
  
## Group by foodgroup and split the data by FCT system version
