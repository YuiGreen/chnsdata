setwd("~/Documents/GreenR")
library(haven)
pexam<-read_sas("chnsdata/PE_PA/pexam_12.sas7bdat")
#p_test <- read_sas("chnsdata/PE_PA/pexam_12.sas7bdat",as.is=TRUE)
pexam$U56[is.na(pexam$U56)]<-9
htwt<-subset(pexam,U56!="1",select=c(IDind,WAVE,HEIGHT,WEIGHT))
library(dplyr)
count(pexam,U56="1")
#htwt<-subset(htwtw1989,WAVE!="1989")
summary(htwt)
mast<-read_sas("chnsdata/ID/mast_pub_12.sas7bdat")
survey<-read_sas("chnsdata/ID/surveys_pub_12.sas7bdat")
nutrition<-read_sas("chnsdata/Nutrition/nutr3_00.sas7bdat")
food<-subset(nutrition,select=c(IDind,WAVE,FOODCODE,V39,V39B,V42))

######test######
h1 <- htwt[1:100,]
h1$IDind <- as.character(h1$IDind)
g1 <- gender[1:100,]

a1 <- age[1:100,]

gender_age <- merge(gender,age,by=c("IDind"),all = TRUE)
gender_age$IDind <- as.character(gender_age$IDind)

all <- merge(htwt,gender_age,by=c("IDind","wave"),all = TRUE)
all$IDind <- as.character(all$IDind)
head(all)
################

head(food)
tail(food)

write.table(food,"food.txt",sep="\t")

gender<-subset(mast,select=c(Idind,GENDER,DOD_RPT,DOD_Y))
age<-subset(survey,select=c(Idind,age,wave))

#rename
install.packages("reshape")
library(reshape)
htwt<-rename(htwt,c(WAVE="wave",HEIGHT="height",WEIGHT="weight"))
gender<-rename(gender,c(Idind="IDind",GENDER="gender"))
age<-rename(age,c(Idind="IDind"))
subagehtwt<-merge(age,htwt,by=c("IDind","wave"),all = TRUE)
allwofood<-merge(subagehtwt,gender,by="IDind",all = TRUE)
#food<-rename(food,c(WAVE="wave"))
##all<merge(allwofood,food,by=c("IDind","wave"))

#####################roster#survival###############
write.table(gender,"genderdeath.txt",sep="\t")
summary(gender$DOD_Y)
death8911<-read.table("genderdeath.txt",header=TRUE,sep="\t")

roster<-read_sas("chnsdata/ID/rst_12.sas7bdat")
roster_s<-subset(roster,WAVE!="2015",select=c(IDind,WAVE))

###
myid.uni <- unique(roster_s$IDind)
a<-length(myid.uni)
last <- c()

############do not use loop if not necessary##########
for (i in 1:a) {
  temp<-subset(roster_s, IDind==myid.uni[i])
  if (dim(temp)[1] > 1) {
    last.temp<-temp[dim(temp)[1],]
  }
  else {
    last.temp<-temp
  }
  last<-rbind(last, last.temp)
}

last
tail(last)
#####################################

library(dplyr)
roster_s %>% colnames
roster_s %>% group_by(IDind) %>% filter(WAVE == max(WAVE))
roster_s %>% head
roster_s %>% group_by(IDind) %>% filter(WAVE == min(WAVE))
roster_s %>% filter(IDind == "111101005002")

tempRoster <- roster_s %>% filter(IDind %in% names(((roster_s$IDind %>% table) > 1)[((roster_s$IDind %>% table) > 1) == T]) )
tempRoster %>% group_by(IDind) %>% filter(WAVE == max(WAVE))
tempRoster %>% group_by(IDind) %>% filter(WAVE == min(WAVE))
lapply(1:10, function(s) s+1) %>% unlist

mydata %>% group_by(id, day) %>% filter(row_number(value) == 1)

############loop first not correct but not needed##############
first <- c()
for (i in 1:a) {
  temp<-subset(roster_s, IDind==myid.uni[i])
  if (dim(temp)[1] > 1) {
    first.temp<-temp[,dim(temp)[1]]
  }
  else {
    first.temp<-temp
  }
  first<-rbind(first, first.temp)
}

first
########################

########################
do.call("rbind", 
        by(df, INDICES=df$IDind, FUN=function(roster_s) roster_s[which.min(roster_s$WAVE), ]))

Ranks <- with(roster_s, ave(IDind, WAVE, FUN = function(x) 
  rank(WAVE, ties.method="first")))
first<-roster_s[Ranks == 1, ]
summary(Ranks)

library(dplyr)
first<-roster_s %>% group_by(IDind) %>% filter(row_number(WAVE) == 1)
tail(first)
head(first)
#####################################

roster_s <- roster_s[with(roster_s, do.call(order, list(IDind, WAVE))), ]
first2<-do.call(rbind, by(roster_s, list(roster_s$IDind, roster_s$WAVE), 
                         FUN=function(x) head(x, 1)))
summary(first2)

library(data.table)
attach(roster_s)
DT <- data.table(IDind, WAVE, key = "IDind")
first3<-DT[, n := rank(WAVE, ties.method="first"), by = key(DT)][n == 1]
summary(first3)
detach(roster_s)

death_gender<-read.table("genderdeath.txt",header=TRUE,sep="\t")
library(reshape)
last<-rename(last,c(WAVE="lastentry"))
survival1 <- merge(death_gender,last,by=c("IDind"))
write.table(survival1,"survival1.txt",sep="\t")
first<-rename(first,c(WAVE="firstentry"))

survival2<-merge(survival1,first,by=c("IDind"),all=TRUE)

########################################################
write.table(survival2,"survival2.txt",sep="\t")
attach(survival2)
summary(survival2$firstentry)
survival2$firstentry
#survival2$firstentry<-as.character(survival2$firstentry)
#str(df$firstentry)
library(dplyr)
#df <- df %>% 
###### as.Date(survival2$firstentry, format = "%m/%d/%Y")
  mutate(firstentry_format1 = as.Date(survival2$firstentry, format = "%m/%d/%Y"))


warnings()
tb_iris <- as_tibble(firstentry)
print(tb_iris)

detach(survival2)
################################################################
library(psych)
library(survival)
install.packages("gmodels")
library(gmodels)
library(stargazer)

survpre<-read.table("survival2 wtf.txt",header=TRUE,sep="\t")
##survwtf<-merge(b10rename,survpre,by=c("IDind"))
##surv<-merge(survwtf,food,by=c("IDind"))

fv<-merge(cum_vegie,cum_fruit,by=c("IDind","wave"))
##survfv<-merge(fv,survwtf,by=c("IDind","wave"))
##write.table(survfv,"survfv.txt",sep="\t")
#survfvnc<-merge(fv,survpre,by=c("IDind"))
survfvnc[!is.na(survfvnc) ,]
library(plyr)
count.1id.per.column <- ldply(survfvnc, function(c) sum(c=="111101005005"))
count.1id.per.column
write.table(survfvnc,"survfvnc.txt",sep="\t")
write.table(fv,"fv.txt",sep="\t")

fv <- cbind(fv, num_groups = c(1:91509))
fv$num_groups <- as.numeric(ave(fv$IDind, fv$IDind, FUN = length))

fv <- cbind(fv, meanv = c(1:91509), meanf = c(1:91509))
fv$meanv <- fv$cum_sum.x/fv$num_groups
fv$meanf <- fv$cum_sum.y/fv$num_groups

sfv <- fv[order(fv$IDind, -(fv$cum_sum.x)), ] #sort by id and reverse of abs(value)
sfv[!duplicated(sfv$IDind), ]              # take the first row within each id

library(data.table)

sfv <- as.data.table(fv)
setkey(sfv,"IDind")

sfv[J(unique(IDind)), list(meanv = meanv[which.max(meanv)])]


library(data.table)
sfv<-as.data.table(fv)[, .SD[which.max(meanv)], by=IDind]
survfvnc<-merge(sfv,survpre,by=c("IDind"))


#####################food processing############################
f97<-read.table("f97.txt",header=TRUE,sep="\t")
f97$code[is.na(f97$code)]<-"other"
write.table(f97,"f97v1.txt",sep="\t")

library(reshape2)
f97re<-read.table("f97clean.txt",header=TRUE,sep="\t")
f97pre<-subset(f97re,select=c(IDind,code,weight))
cast97<-cast(f97pre,IDind~code,sum)

f00re<-read.table("f00clean.txt",header=TRUE,sep="\t")
f00pre<-subset(f00re,select=c(IDind,code,weight))
cast00<-cast(f00pre,IDind~code,sum)

f04re<-read.table("f04clean.txt",header=TRUE,sep="\t")
f04pre<-subset(f04re,select=c(IDind,code,weight))
cast04<-cast(f04pre,IDind~code,sum)

f06re<-read.table("f06clean.txt",header=TRUE,sep="\t")
f06pre<-subset(f06re,select=c(IDind,code,weight))
cast06<-cast(f06pre,IDind~code,sum)

f09re<-read.table("f09clean.txt",header=TRUE,sep="\t")
f09pre<-subset(f09re,select=c(IDind,code,weight))
cast09<-cast(f09pre,IDind~code,sum)

f11re<-read.table("f11clean.txt",header=TRUE,sep="\t")
f11pre<-subset(f11re,select=c(IDind,code,weight))
cast11<-cast(f11pre,IDind~code,sum)

f89re<-read.table("f89clean.txt",header=TRUE,sep="\t")
f89pre<-subset(f89re,select=c(IDind,code,weight))
cast89<-cast(f89pre,IDind~code,sum)

f91re<-read.table("f91clean.txt",header=TRUE,sep="\t")
f91pre<-subset(f91re,select=c(IDind,code,weight))
cast91<-cast(f91pre,IDind~code,sum)

f93re<-read.table("f93clean.txt",header=TRUE,sep="\t")
f93pre<-subset(f93re,select=c(IDind,code,weight))
cast93<-cast(f93pre,IDind~code,sum)

cast00$vegie<-cast00$vegie*50
cast00$fruit<-cast00$fruit*50
cast00$other<-cast00$other*50

cast97$vegie<-cast97$vegie*50
cast97$fruit<-cast97$fruit*50
cast97$other<-cast97$other*50

cast93$vegie<-cast93$vegie*50
cast93$fruit<-cast93$fruit*50
cast93$other<-cast93$other*50

cast91$vegie<-cast91$vegie*50
cast91$fruit<-cast91$fruit*50
cast91$other<-cast91$other*50

cast89$vegie<-cast89$vegie*50
cast89$fruit<-cast89$fruit*50
cast89$other<-cast89$other*50

cast89$wave<-1989
cast91$wave<-1991
cast93$wave<-1993
cast97$wave<-1997
cast00$wave<-2000
cast04$wave<-2004
cast06$wave<-2006
cast09$wave<-2009
cast11$wave<-2011

cast89$vegie<-cast89$vegie/3
cast89$fruit<-cast89$fruit/3
cast89$other<-cast89$other/3
cast91$vegie<-cast91$vegie/3
cast91$fruit<-cast91$fruit/3
cast91$other<-cast91$other/3
cast93$vegie<-cast93$vegie/3
cast93$fruit<-cast93$fruit/3
cast93$other<-cast93$other/3
cast97$vegie<-cast97$vegie/3
cast97$fruit<-cast97$fruit/3
cast97$other<-cast97$other/3
cast00$vegie<-cast00$vegie/3
cast00$fruit<-cast00$fruit/3
cast00$other<-cast00$other/3
cast04$vegie<-cast04$vegie/3
cast04$fruit<-cast04$fruit/3
cast04$other<-cast04$other/3
cast06$vegie<-cast06$vegie/3
cast06$fruit<-cast06$fruit/3
cast06$other<-cast06$other/3
cast09$vegie<-cast09$vegie/3
cast09$fruit<-cast09$fruit/3
cast09$other<-cast09$other/3
cast11$vegie<-cast11$vegie/3
cast11$fruit<-cast11$fruit/3
cast11$other<-cast11$other/3

totalfood <- rbind(cast89,cast91,cast93,cast97,cast00,cast04,cast06,cast09,cast11)

sub_vegie<-subset(totalfood,select=c(IDind,wave,vegie))
sub_fruit<-subset(totalfood,select=c(IDind,wave,fruit))

##############try but wrong############
sub_subvegie<-sub_vegie[1:100,]
sub_subvegie$variable<-"cum"
castvegie<-cast(sub_vegie,IDind~vegie,cummean)
View(sub_vegie)
castsubvegie<-cast(sub_subvegie,IDind+wave~vegie+variable,GMCM:::cummean)
######################################

library(dplyr)
sub_vegie %>% colnames()
cum_vegie <- sub_vegie %>% group_by(IDind) %>% mutate(cum_sum = cumsum(vegie))
(sub_vegie %>% group_by(IDind) ) %>% View
sub_vegie[sub_vegie$IDind == "212103010005", ]
cum_vegie[cum_vegie$IDind == "212103010005", ]

sub_fruit %>% colnames()
cum_fruit <- sub_fruit %>% group_by(IDind) %>% mutate(cum_sum = cumsum(fruit))
(sub_fruit %>% group_by(IDind) ) %>% View
sub_fruit[sub_fruit$IDind == "212103010005", ]
cum_fruit[cum_fruit$IDind == "212103010005", ]


##################GMCM#################################
install.packages("GMCM")
library(GMCM)
?cummean

x <- sort(rnorm(100))
GMCM:::cummean(x)
######################################################

##############bmi################
bmi<-htwt
bmi$bmi<-NA
bmi$bmi<-bmi$weight/((bmi$height*0.01)*(bmi$height*0.01))
summary(bmi$bmi)
summary(bmi)
bmi[!is.na(bmi$bmi) ,]
###adultsbmi<-subset(bmi,age>=18 & age<=65)
summary(adultswithna)

adults<-na.omit(adultswithna)
summary(adults)



quantile(cum_vegie$cum_sum,c(0.2,0.4,0.6,0.8),na.rm=TRUE)
totalfood$vegiequin[totalfood$vegie<160] <- 1
totalfood$vegiequin[totalfood$vegie>=160&totalfood$vegie<240] <- 2
totalfood$vegiequin[totalfood$vegie>=240&totalfood$vegie<318.3333] <- 3
totalfood$vegiequin[totalfood$vegie>=318.3333&totalfood$vegie<433.3333] <- 4
totalfood$vegiequin[totalfood$vegie>=433.3333] <- 5

quantile(totalfood$fruit,c(0.2,0.4,0.6,0.8),na.rm=TRUE)
totalfood$fruitquin[totalfood$vegie<160] <- 1
totalfood$fruitquin[totalfood$vegie>=160&totalfood$vegie<240] <- 2
totalfood$fruitquin[totalfood$vegie>=240&totalfood$vegie<318.3333] <- 3
totalfood$fruitquin[totalfood$vegie>=318.3333&totalfood$vegie<433.3333] <- 4
totalfood$fruitquin[totalfood$vegie>=433.3333] <- 5

############################covariates#######################################
library(haven)
educ_12<-read_sas("chnsdata/Educ/educ_12.sas7bdat")
edu<-subset(educ_12,select=c(IDind,WAVE,A12))
write.table(edu,"edu.txt",sep="\t")
rm(edu)
edu<-read.table("edu.txt",header=TRUE,sep="\t")
hhincome_pre<-read_sas("chnsdata/Constructed_Income/hhinc_10.sas7bdat")
hhincome<-subset(hhincome_pre,select=c(hhid,WAVE,HHINC,hhsize,hhinc_pc))
maritalwresidence<-subset(roster,select=c(IDind,WAVE,A8,T2))
urban_11<-read_sas("chnsdata/UrbanIndex/urban_11.sas7bdat")
urbanindex<-subset(urban_11,select=c(COMMID,wave,index))
smokealcohol<-subset(pexam,select=c(IDind,WAVE,U25,U40,U41))
nutr_2<-read_sas("chnsdata/Nutrition/nutr2_00.sas7bdat")
pa<-subset(nutr_2,select=c(IDind,WAVE,V29))
pact<-read_sas("chnsdata/PE_PA/pact_12.sas7bdat")
sleep<-subset(pact,select=c(IDind,WAVE,U324))
macronutrients<-read_sas("chnsdata/Macronutrients/c12diet.sas7bdat")
calorie<-subset(macronutrients,select=c(IDind,wave,d3kcal,d3fat,d3protn,t5))
selfhealth<-subset(pexam,select=c(IDind,WAVE,U48A))
diseasehistory<-subset(pexam,select=c(IDind,WAVE,U22,U24A,U24J,U24L,U24W))

library(reshape)
diseasehistory<-rename(diseasehistory,c(WAVE="wave",U22="highbloodpre",U24A="diabetes",U24J="heartattack",U24L="stroke",U24W="cancer"))
write.table(diseasehistory,"diseasehistory.txt",sep="\t")
diseasehis<-read.table("diseasehis.txt",header=TRUE,sep="\t")

##############subset baseline data################

##################################################
covariates1<-merge(age,gender,by=c("IDind"),all=TRUE)
covariates2<-merge(covariates1,bmi,by=c("IDind","wave"),all=TRUE)
covariates3<-merge(covariates2,edu,by=c("IDind","wave"),all=TRUE)
idhdid<-subset(roster,select=c(IDind,hhid))
iincome<-merge(hhincome,idhdid,by=c("hhid"))
iavincome<-iincome
iavincome$avincome<-iavincome$HHINC/iavincome$hhsize
iavincome<-subset(iavincome,select=c("IDind","WAVE","avincome"))
write.table(iavincome,"iavincome.txt",sep="\t")
rm(iavincome)
iavincome<-read.table("iavincome.txt",header=TRUE,sep="\t")
covariates4<-merge(covariates3,iavincome,by=c("IDind","wave"),all=TRUE)
write.table(maritalwresidence,"maritalwresidence.txt",sep="\t")
rm(maritalwresidence)
maritalwresidence<-read.table("maritalwresidence.txt",header=TRUE,sep="\t")

memory.limit()
covariates5<-merge(covariates4,maritalwresidence,by=c("IDind","wave"))
install.packages("usethis")
library(usethis)
usethis::edit_r_environ()
##########################################################
final[complete.cases(final),]
na.omit(final)
final[complete.cases(final[,5:6]),]
na.omit()

########################each baseline data#########################################
write.table(first,"firstentry.txt",sep="\t")
firstentry<-read.table("firstentry.txt",header=TRUE,sep="\t")
colnames(firstentry)[2] <- "wave"
firstentry$wave <- firstentry$wave %>% as.numeric

agebase<-merge(age,firstentry,by=c("IDind","wave"))
cagebase<-na.omit(agebase)

bmibase<-merge(bmi,firstentry,by=c("IDind","wave"))
cbmibase<-na.omit(bmibase)

#edu$wave<-as.numeric(edu$wave)
firstentryedu<-firstentry
firstentryedu$IDind<-as.character(firstentry$IDind)
edubase<-merge(edu,firstentry,by=c("IDind","wave"))
edu<-rename(edu,c(A12="educ",WAVE="wave"))
cedubase<-na.omit(edubase)

rm(iavincbase)
iavincome<-rename(iavincome,c(WAVE="wave"))
iavincbase<-merge(iavincome,firstentry,by=c("IDind","wave"))
iavincbase<-unique(iavincbase)
ciavincbase<-na.omit(iavincbase)

maritalwresidence<-rename(maritalwresidence,c(WAVE="wave",A8="marital",T2="residence"))
maritalwresidencebase<-merge(maritalwresidence,firstentry,by=c("IDind","wave"))
cmaritalwresidencebase<-na.omit(maritalwresidencebase)

idcomless<-subset(survey,select=c(Idind,wave,commid))
idcom<-subset(roster,select=c(IDind,WAVE,COMMID))
write.table(idcom,"idcom.txt",sep="\t")
idcom<-read.table("idcom.txt",header=TRUE,sep="\t")
urbanindex<-rename(urbanindex,c(COMMID="commid"))
idcom<-rename(idcom,c(COMMID="commid",WAVE="wave"))
write.table(urbanindex,"urbanindex.txt",sep="\t")
urbanindex<-read.table("urbanindex.txt",header=TRUE,sep="\t")
uipro<-merge(idcom,urbanindex,by=c("commid","wave"))
urbanindexbase<-merge(uipro,firstentry,by=c("IDind","wave"))
curbanindexbase<-na.omit(urbanindexbase)

write.table(smokealcohol,"smokealcohol.txt",sep="\t")
smokealcohol<-read.table("smokealcohol.txt",header=TRUE,sep="\t")
smokealcohol<-rename(smokealcohol,c(WAVE="wave",U25="eversmoke",U40="drinklastyear"))
smokealcoholbase<-merge(smokealcohol,firstentry,by=c("IDind","wave"))
summary(smokealcoholbase)
smokealcoholbase$eversmoke[smokealcoholbase$eversmoke==9]<-0
smokealcoholbase$eversmoke[is.na(smokealcoholbase$eversmoke)]<-0
smokealcoholbase$drinklastyear[smokealcoholbase$drinklastyear==9]<-0
smokealcoholbase$drinklastyear[is.na(smokealcoholbase$drinklastyear)]<-0
smokedrink<-subset(smokealcohol,select=c("IDind","wave","eversmoke","drinklastyear"))
smokedrink<-rename(smokedrink,c(WAVE="wave",U25="eversmoke",U40="drinklastyear"))
smokedrinkbase<-merge(smokedrink,firstentry,by=c("IDind","wave"))
table(smokedrinkbase$eversmoke)
smokedrinkbase$eversmoke[smokedrinkbase$eversmoke==9]<-(-1)
smokedrinkbase$eversmoke[is.na(smokedrinkbase$eversmoke)]<-(-1)
smokedrinkbase$drinklastyear[is.na(smokedrinkbase$drinklastyear)]<-(-1)
smokedrinkbase$drinklastyear[smokedrinkbase$drinklastyear==9]<-(-1)
table(smokedrinkbase$drinklastyear)
csmokedrinkbase<-na.omit(smokedrinkbase)

write.table(sleep,"sleep.txt",sep="\t")
sleep<-read.table("sleep.txt",header=TRUE,sep="\t")
sleep<-rename(sleep,c(WAVE="wave",U324="sleep"))
sleepbase<-merge(sleep,firstentry,by=c("IDind","wave"))
table(sleepbase$bedtime)
sleepbase$sleep[sleepbase$sleep==(-9)]<-NA
sleepbase$sleep[is.na(sleepbase$sleep)]<-(-1)
csleepbase<-na.omit(sleepbase)

write.table(pa,"pa.txt",sep="\t")
pa<-read.table("pa.txt",header=TRUE,sep="\t")
pa<-rename(pa,c(WAVE="wave",V29="pa"))
pabase<-merge(pa,firstentry,by=c("IDind","wave"))
table(pabase$pa)
cpabase<-na.omit(pabase)

calorie$perkcal<-calorie$d3kcal/calorie$t5
write.table(calorie,"calorie.txt",sep="\t")
calorie<-read.table("calorie.txt",header=TRUE,sep="\t")
caloriebase<-merge(calorie,firstentry,by=c("IDind","wave"))
ccaloriebase<-na.omit(caloriebase)

write.table(selfhealth,"selfhealth.txt",sep="\t")
selfhealth<-read.table("selfhealth.txt",header=TRUE,sep="\t")
selfhealthbase<-merge(selfhealth,firstentry,by=c("IDind","wave"))
selfhealthbase
cselfhealthbase<-na.omit(selfhealthbase)

#######disease baseline#######
diseasebase<-merge(diseasehistory,firstentry,by=c("IDind","wave"))
summary(diseasebase)
write.table(diseasebase,"diseasebase.txt",sep="\t")

diseasebase$highbloodpre[diseasebase$highbloodpre==9]<-0
diseasebase$highbloodpre[is.na(diseasebase$highbloodpre)]<-0

diseasebase$diabetes[diseasebase$diabetes==9]<-0
diseasebase$diabetes[is.na(diseasebase$diabetes)]<-0

diseasebase$heartattack[diseasebase$heartattack==9]<-0
diseasebase$heartattack[is.na(diseasebase$heartattack)]<-0

diseasebase$stroke[diseasebase$stroke==9]<-0
diseasebase$stroke[is.na(diseasebase$stroke)]<-0

diseasebase$cancer[diseasebase$cancer==9]<-0
diseasebase$cancer[is.na(diseasebase$cancer)]<-0

#diseasebase$anydi<-diseasebase$highbloodpre+diseasebase$diabetes+diseasebase$heartattack+diseasebase$stroke
#nondisease<-subset(diseasebase,diseasebase$anydi==0,select=c("IDind","wave"))
#summary(nondisease)
diseasebase$cvdcancer<-diseasebase$heartattack+diseasebase$stroke+diseasebase$cancer
cvdcancer<-subset(diseasebase,diseasebase$cvdcancer>=1,select=c("IDind","wave"))
cnondisease<-na.omit(nondisease)

summary(bmibase)
summary(cbmibase)

############################form final dataset#######################
sex<-gender
cagebase #34524
cbmibase #22786
cedubase #24438
ciavincbase #172438
cmaritalwresidencebase #31225
curbanindexbase #35703
csmokedrinklbase #24629
csleepbase #10526
cpabase #31684
ccaloriebase #16385
##cselfhealthbase #6838
cnondisease #23071

#b1<-merge(cagebase,cbmibase,by=c("IDind","wave")) #22784
#b2<-merge(b1,cedubase,by=c("IDind","wave")) #15628
#b3<-merge(b2,ciavincbase,by=c("IDind","wave")) #69094
#b4<-merge(b3,cmaritalwresidencebase,by=c("IDind","wave")) #66332
#b5<-merge(b4,curbanindexbase,by=c("IDind","wave")) #66332
#b6<-merge(b5,csmokedrinkbase,by=c("IDind","wave")) #66332
#b7<-merge(b6,csleepbase,by=c("IDind","wave")) #16255
#b8<-merge(b7,cpabase,by=c("IDind","wave")) #15878
#b9<-merge(b8,ccaloriebase,by=c("IDind","wave")) #15693
#b10<-merge(b9,cnondisease,by=c("IDind","wave")) #13259

write.table(b10,"b10.txt",sep="\t")
b10rename<-read.table("b10.txt",header=TRUE,sep="\t")
#❤️#
#####rename survfv######
library("dplyr")
library("dplyr")
library("plyr")
survfv<-plyr::rename(survfv,c(edu12="edu",t5="householdnum"))

############################Variables class processing################################
survfv<-select(survfv,-edu11)
survfv<-select(survfv,-X)

#edu
survfv$edu[survfv$edu==0] <- 1
survfv$edu[survfv$edu==2] <- 1
survfv$edu[survfv$edu==3] <- 2
survfv$edu[survfv$edu==4] <- 2
survfv$edu[survfv$edu==5] <- 3
survfv$edu[survfv$edu==6] <- 3
summary(edu)

#marital
survfv$marital[survfv$marital==1] <- 0
survfv$marital[survfv$marital==3] <- 0
survfv$marital[survfv$marital==4] <- 0
survfv$marital[survfv$marital==5] <- 0
survfv$marital[survfv$marital==2] <- 1
summary(marital)

#obesityornot 18.5 24 28 
survfv$obs[survfv$bmi<=18.5] <- 1
survfv$obs[survfv$bmi>=18.5 & survfv$bmi<=24.0] <- 2
survfv$obs[survfv$bmi>=24.0 & survfv$bmi<=28.0] <- 3
survfv$obs[survfv$bmi>=28.0] <- 4

#pa
survfv$pa[survfv$pa==2] <- 1
survfv$pa[survfv$pa==3] <- 2
survfv$pa[survfv$pa==4] <- 3
survfv$pa[survfv$pa==5] <- 3
survfv$pa[survfv$pa==6] <- 1

#sleep
survfv$bedtime[survfv$bedtime<7] <- 1
survfv$bedtime[survfv$bedtime>=7 & survfv$bedtime<=9] <- 2
survfv$bedtime[survfv$bedtime>9] <- 3
survfv$bedtime[survfv$bedtime==1] <- 0
survfv$bedtime[survfv$bedtime==3] <- 0
survfv$bedtime[survfv$bedtime==2] <- 1

#urbanindex
survfv$urbanindex[survfv$urbanindex<7] <- 1
survfv$urbanindex[survfv$urbanindex>=7 & survfv$urbanindex<=9] <- 2
survfv$urbanindex[survfv$urbanindex>9] <- 3
survfv$urbanindex[survfv$urbanindex==3] <- 0
survfv$urbanindex[survfv$urbanindex==2] <- 1

#avincome
survfv$avincome[survfv$avincome<0] <- 0
quantile(survfv$avincome,c(0.2,0.4,0.6,0.8))
survfv$avincome[survfv$avincome<3180.000] <- 1
survfv$avincome[survfv$avincome>=3180.000&survfv$avincome<6557.345] <- 2
survfv$avincome[survfv$avincome>=6557.345&survfv$avincome<12000.000] <- 3
survfv$avincome[survfv$avincome>=12000.000&survfv$avincome<21155.622] <- 4
survfv$avincome[survfv$avincome>=21155.622] <- 5

# veg & fruit
quantile(survfv$cum_sum.x,c(0.2,0.4,0.6,0.8))
survfv$vegqu[survfv$cum_sum.x<153.3333] <- 1
survfv$vegqu[survfv$cum_sum.x>=153.3333&survfv$cum_sum.x<223.3333] <- 2
survfv$vegqu[survfv$cum_sum.x>=223.3333&survfv$cum_sum.x<300.0000] <- 3
survfv$vegqu[survfv$cum_sum.x>=300.0000&survfv$cum_sum.x<403.8667] <- 4
survfv$vegqu[survfv$cum_sum.x>=403.8667] <- 5

quantile(survfv$cum_sum.y,c(0.2,0.4,0.6,0.8),na.rm=TRUE)
survfv$fruitqu[survfv$cum_sum.y<0.00000] <- 1
survfv$fruitqu[survfv$cum_sum.y>=0.00000&survfv$cum_sum.y<0.00000] <- 2
survfv$fruitqu[survfv$cum_sum.y>=0.00000&survfv$cum_sum.y<16.66667] <- 3
survfv$fruitqu[survfv$cum_sum.y>=16.66667&survfv$cum_sum.y<120.00000] <- 4
survfv$fruitqu[survfv$cum_sum.y>=120.00000] <- 5

#data(survfv)
head(survfv)
table(survfv$gender)
library(survival)
with(survfv,Surv(time,event))
summary(with(survfv,Surv(time,event)~gender))
fit<-survfit(Surv(time,event)~gender,data=survfv)
print(fit)
summary(fit)

table(survfv$event)

install.packages("car",dependencies = TRUE)
install.packages("survival",dependencies = TRUE)
install.packages("flexsurv",dependencies = TRUE)
install.packages('KMsurv',dependencies = TRUE)
install.packages("e1071",dependencies = TRUE)
install.packages("rms",dependencies = TRUE)

###########################survival analysis###########################################
survfvnc
survsub<-subset(survfvnc,select=c(IDind,time,event,censor))
??survival
surv.combine <- Surv(survfvnc$time,survfvnc$event)
survival::Surv(survsub$time,survsub$event)
f1 <- survival::survfit(survival::Surv(survsub$time,survsub$event) ~ 1, data = survsub)
names(f1)
plot(survival::survfit(survival::Surv(survsub$time,survsub$event) ~ 1, data = survsub), mark.time = TRUE
,
     xlab = "Years", 
     ylab = "Overall survival probability")

survminer::ggsurvplot(
  fit = survival::survfit(survival::Surv(survsub$time,survsub$event) ~ 1, data =  survsub), 
  xlab = "Years", 
  ylab = "Overall survival probability")

survival::survfit(survival::Surv(survsub$time,survsub$event) ~ 1, data = survsub)

###############################################
install.packages("ranger")
install.packages("ggfortify")
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
data(survfv)
head(survfv)

survminer::ggsurvplot(
  fit = survival::survfit(survival::Surv(time, event) ~ 1, data = survsub), 
  xlab = "Years", 
  ylab = "Overall survival probability")
summary(survival::survfit(survival::Surv(time, event) ~ 1, data = survsub), 
        times = 5)
survival::survfit(survival::Surv(time, event) ~ 1, data = survsub)

survfv$time[survsub$event == 1] %>% 
  median


## Summary

#- Time-to-event data is common
#- Survival analysis techniques are required to account for censored data
#- The `survival` package provides tools for survival analysis, including the `Surv` and `survfit` functions
#- The `survminer` package allows for customization of Kaplan-Meier plots based on `ggplot2`
#- Between-group comparisons can be made with the log-rank test using `survival::survdiff`
#- Multiavariable Cox regression analysis can be accomplished using `survival::coxph` 

vegie
fruit
quantile(survfv$cum_sum.x,c(0.2,0.4,0.6,0.8))
survfv$vegie_cat[survfv$cum_sum.x<159.6667] <- 1
survfv$vegie_cat[survfv$cum_sum.x>=4.512329&survfv$cum_sum.x<233.3333] <- 2
survfv$vegie_cat[survfv$cum_sum.x>=233.3333&survfv$cum_sum.x<316.6667] <- 3
survfv$vegie_cat[survfv$cum_sum.x>=316.6667&survfv$cum_sum.x<410.0000] <- 4
survfv$vegie_cat[survfv$cum_sum.x>=410.0000] <- 5
install.packages("devtools")
devtools::install_github("zabore/ezfun")
library(ezfun)

survminer::ggsurvplot(
  fit = survival::survfit(survival::Surv(time, event) ~ vegie_cat, data = survsub), 
  xlab = "Years",
  ylab = "Overall survival probability",
  legend.title = "vegie",
  legend.labs = c("Quintile1", "Quintile2", "Quintile3","Quintile4","Quintile5"),
  break.x.by = 5, 
  ylim=c(0.9,1),
  xlim = c(0,10),
  censor = TRUE,
  risk.table = TRUE,
  risk.table.y.text = FALSE)

summary(survival::survfit(survival::Surv(time, event) ~ vegie_cat, data = survfv), 
        times = 5)

survival::survdiff(survival::Surv(survfv$time, survfv$event) ~ survfv$vegie_cat)
survival::coxph(survival::Surv(time, event) ~ factor(vegie_cat), data = survfv)
broom::tidy(survival::coxph(survival::Surv(time, event) ~ factor(vegie_cat), 
                            data = survfv))
exp(14.4)?

#####install.packages("Hmisc") 18-65
#####adultfruit: 300~500g/d & 3.0~5.0 servings/d
#####vegie: 200~350g/d & 2.0~3.5 servings/d

write.table(b10rename,"b10rename.txt",sep="\t")
survfvagegender<-merge(survfvnc,cagebase,by=c("IDind"))
survfvagegenderx<-merge(survfvnc, cagebase, by.survfvnc = IDind, by.cagebase = IDind)
table(survfvnc$event)

rainbow()

# fit a Kaplan−Meier and plot it
fit <- survfit(Surv(time, status) ~ x, data=aml) plot(fit)
# life table
cbind(fit$time,fit$n.risk ,fit$n.event,fit$surv)

## Fit a time transform model using current age 
coxph(Surv(time, status) ~ ph.ecog + tt(age), data=lung,
      tt = function(x,t ,...) pspline(x + t/365.25))

#merge(x, y, by.x = x, by.y = y)
#Arguments:
#-x: The origin data frame
#-y: The data frame to merge
#-by.x: The column used for merging in x data frame. Column x to merge on
#-by.y: The column used for merging in y data frame. Column y to merge on

summary(age)
adultage<-subset(agebase,age>=18)
sample<- merge(adultage,survfvnc,by=c("IDind"),all = FALSE)
summary(sample)
gsample<-subset(sample,select=c(IDind,age,meanv,meanf,gender,lastentry,firstentry,censor,event,time))        
summary(gsample)
plot(gsample$meanf)
nofruit<-subset(gsample,meanf==0)
gsample$fvcombine<-gsample$meanf+gsample$meanv
table(gsample$event)
table(healthysample$event)
objects(pattern='cvdcancer$IDind')
intersect(cvdcancer$IDind,gsample$IDind)
Reduce(intersect, list(cvdcancer$IDind,gsample$IDind))
uniqueid<-setdiff(gsample$IDind, cvdcancer$IDind)
noncvdcancerid<-as.data.frame(uniqueid)
noncvdcancerid<-rename(noncvdcancerid,c(uniqueid="IDind"))
healthysample<-merge(gsample,noncvdcancerid,by=c("IDind"),all = FALSE)
table(healthysample$event)
summary(healthysample)
healthysample<-na.omit(healthysample)
healthysample<-rename(healthysample,c(gender="sex"))
summary(gsample)
gsample<-na.omit(gsample)
###anti_join

b1<-merge(cbmibase,cedubase,by=c("IDind")) #18098
b2<-merge(b1,ciavincbase,by=c("IDind")) #17946
b3<-merge(b2,cmaritalwresidencebase,by=c("IDind")) #16775
b4<-merge(b3,curbanindexbase,by=c("IDind")) #16775
b5<-merge(b4,csmokedrinkbase,by=c("IDind")) #16775
b6<-merge(b5,csleepbase,by=c("IDind")) #16775
b7<-merge(b6,cpabase,by=c("IDind")) #16166
b8<-merge(b7,ccaloriebase,by=c("IDind")) #10808


allinclude<-merge(b8,healthysample,by=c("IDind"))

b1n<-merge(cbmibase,cedubase,by=c("IDind"), all=TRUE) #32713
b2n<-merge(b1n,ciavincbase,by=c("IDind"), all=TRUE) #35703
b3n<-merge(b2n,cmaritalwresidencebase,by=c("IDind"), all=TRUE) #32714
b4n<-merge(b3n,curbanindexbase,by=c("IDind"), all=TRUE) #35765
b5n<-merge(b4n,csmokedrinkbase,by=c("IDind"), all=TRUE) #35765
b6n<-merge(b5n,csleepbase,by=c("IDind"), all=TRUE) #35765
b7n<-merge(b6n,cpabase,by=c("IDind"), all=TRUE) #35765
b8n<-merge(b7n,ccaloriebase,by=c("IDind"), all=TRUE) #35765


allincluden<-merge(b8n,healthysample,by=c("IDind"),all=TRUE)


table(healthysample$event)
names(healthysample)


#######Datacamp########

library(survival)
install.packages("survminer")
library(survminer)
library(dplyr)

# allinclude #obesityornot 18.5 24 28 
summary(allinclude)
allinclude$bmicat[allinclude$bmi<=18.5] <- 1
allinclude$bmicat[allinclude$bmi>=18.5 & allinclude$bmi<=24.0] <- 2
allinclude$bmicat[allinclude$bmi>=24.0 & allinclude$bmi<=28.0] <- 3
allinclude$bmicat[allinclude$bmi>=28.0] <- 4
allinclude$bmicat2=factor(allinclude$bmicat,levels=c(1,2,3,4),labels=c("underweight","normal","overweight","obesity"))
#factor(allinclude$bmicat2,ordered=T,levels=c("underweight","normal","overweight","obesity"))

#edu
allinclude$educat[allinclude$educ==0] <- 1
allinclude$educat[allinclude$educ==1] <- 1
allinclude$educat[allinclude$educ==2] <- 1
allinclude$educat[allinclude$educ==3] <- 2
allinclude$educat[allinclude$educ==4] <- 2
allinclude$educat[allinclude$educ==5] <- 3
allinclude$educat[allinclude$educ==6] <- 3
allinclude$educat2=factor(allinclude$educat,levels=c(1,2,3),labels=c("primary or junior high school","high school or equal","college or above"))
summary(allinclude$educat2)

#residence
allinclude$residencecat2=factor(allinclude$residence,levels=c(1,2),labels=c("rural","urban"))

#marital
allinclude$maritalcat[allinclude$marital==1] <- 0
allinclude$maritalcat[allinclude$marital==3] <- 0
allinclude$maritalcat[allinclude$marital==4] <- 0
allinclude$maritalcat[allinclude$marital==5] <- 0
allinclude$maritalcat[allinclude$marital==2] <- 1
allinclude$maritalcat2=factor(allinclude$maritalcat,levels=c(0,1),labels=c("not married","married"))
summary(allinclude$maritalcat2)

#pa
allinclude$pacat[allinclude$pa==2] <- 1
allinclude$pacat[allinclude$pa==1] <- 1
allinclude$pacat[allinclude$pa==3] <- 2
allinclude$pacat[allinclude$pa==4] <- 3
allinclude$pacat[allinclude$pa==5] <- 3
allinclude$pacat[allinclude$pa==6] <- 1
allinclude$pacat2=factor(allinclude$pacat,levels=c(1,2,3),labels=c("light","moderate","heavy"),exclude=NULL)
factor(allinclude$pacat2,ordered=T,levels=c("light","moderate","heavy"))
summary(allinclude$pacat2)

#sleep
allinclude$sleepcat[allinclude$sleep<7] <- 1
allinclude$sleepcat[allinclude$sleep>=7 & allinclude$sleep<=9] <- 2
allinclude$sleepcat[allinclude$sleep>9] <- 3
#allinclude$sleepcat[allinclude$sleepcat==1] <- 0
#allinclude$sleepcat[allinclude$sleepcat==3] <- 0
#allinclude$sleepcat[allinclude$sleepcat==2] <- 1
allinclude$sleepcat2=factor(allinclude$sleepcat,levels=c(1,2,3),labels=c("less than 7 hours","7-9 hours","more than 9 hours"))
summary(allinclude$sleepcat2)

#smoke
allinclude$smokecat2=factor(allinclude$eversmoke,levels=c(-1,0,1),labels=c("not answered","no","yes"))
summary(allinclude$smokecat2)

#drink
allinclude$drinkcat2=factor(allinclude$drinklastyear,levels=c(-1,0,1),labels=c("not answered","no","yes"))

#avincome
allinclude$avincome[allinclude$avincome<0] <- 0
quantile(allinclude$avincome,c(0.25,0.5,0.75))
allinclude$incat[allinclude$avincome<2750.2] <- 1
allinclude$incat[allinclude$avincome>=2750.2&allinclude$avincome<6306.0] <- 2
allinclude$incat[allinclude$avincome>=6306.0&allinclude$avincome<15691.6] <- 3
allinclude$incat[allinclude$avincome>=15691.6] <- 4
allinclude$incat2=factor(allinclude$incat,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))

# veg & fruit
#veg5
quantile(allinclude$meanv,c(0.2,0.4,0.6,0.8))
allinclude$vegcat5[allinclude$meanv<175.0000] <- 1
allinclude$vegcat5[allinclude$meanv>=175.0000&allinclude$meanv<241.6667] <- 2
allinclude$vegcat5[allinclude$meanv>=241.6667&allinclude$meanv<306.6667] <- 3
allinclude$vegcat5[allinclude$meanv>=306.6667&allinclude$meanv<388.8889] <- 4
allinclude$vegcat5[allinclude$meanv>=388.8889] <- 5
allinclude$vegcat52=factor(allinclude$vegcat5,levels=c(1,2,3,4,5),labels=c("Q1","Q2","Q3","Q4","Q5"))
#veg4
quantile(allinclude$meanv,c(0.25,0.5,0.75))
allinclude$vegcat4[allinclude$meanv<193.3333] <- 1
allinclude$vegcat4[allinclude$meanv>=193.3333&allinclude$meanv<273.7083] <- 2
allinclude$vegcat4[allinclude$meanv>=273.7083&allinclude$meanv<365.9583] <- 3
allinclude$vegcat4[allinclude$meanv>=365.9583] <- 4
allinclude$vegcat42=factor(allinclude$vegcat4,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
#fru4
quantile(allinclude$meanf,c(0.25,0.5,0.75))
allinclude$frucat4[allinclude$meanf==0.00000] <- 1
subsetfru<-subset(allinclude, meanf>0,select=c(meanf))
quantile(subsetfru$meanf,c(0.25,0.5,0.75))
allinclude$frucat4[allinclude$meanf>0.00000&allinclude$meanf<60.0000] <- 2
allinclude$frucat4[allinclude$meanf>=60.0000&allinclude$meanf<133.3333] <- 3
allinclude$frucat4[allinclude$meanf>=133.3333] <- 4
allinclude$frucat42=factor(allinclude$frucat4,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))
#fru5
quantile(subsetfru$meanf,c(0.25,0.5,0.75))
allinclude$frucat5[allinclude$meanf==0.00000] <- 1
allinclude$frucat5[allinclude$meanf>0.00000&allinclude$meanf<46.66667] <- 2
allinclude$frucat5[allinclude$meanf>=46.66667&allinclude$meanf<91.66667] <- 3
allinclude$frucat5[allinclude$meanf>=91.66667&allinclude$meanf<166.66667] <- 4
allinclude$frucat5[allinclude$meanf>=166.66667] <- 5
allinclude$frucat52=factor(allinclude$frucat5,levels=c(1,2,3,4,5),labels=c("Q1","Q2","Q3","Q4","Q5"))
# fv combine5
quantile(allinclude$fvcombine,c(0.2,0.4,0.6,0.8))
allinclude$fvcat5[allinclude$fvcombine<213.3333] <- 1
allinclude$fvcat5[allinclude$fvcombine>=213.3333&allinclude$fvcombine<291.6667] <- 2
allinclude$fvcat5[allinclude$fvcombine>=291.6667&allinclude$fvcombine<369.9667] <- 3
allinclude$fvcat5[allinclude$fvcombine>=369.9667&allinclude$fvcombine<476.6667] <- 4
allinclude$fvcat5[allinclude$fvcombine>=476.6667] <- 5
allinclude$fvcat52=factor(allinclude$fvcat5,levels=c(1,2,3,4,5),labels=c("Q1","Q2","Q3","Q4","Q5"))
summary(allinclude$fvcat52)

# fv combine4
quantile(allinclude$fvcombine,c(0.25,0.5,0.75))
allinclude$fvcat4[allinclude$fvcombine<233.3333] <- 1
allinclude$fvcat4[allinclude$fvcombine>=233.3333&allinclude$fvcombine<331.6667] <- 2
allinclude$fvcat4[allinclude$fvcombine>=331.6667&allinclude$fvcombine<446.6667] <- 3
allinclude$fvcat4[allinclude$fvcombine>=446.6667] <- 4
allinclude$fvcat42=factor(allinclude$fvcat4,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))

write.table(allincluden,"allincluden.txt",sep="\t")
summary(allinclude)

library(plyr)
allinclude$fvgcat5 <- NULL
allinclude$fvgcat4 <- NULL
final<-na.omit(allinclude)
describe<-subset(final,select=c(bmi,index,perkcal,age,sex,censor,time,bmicat2,educat2,residencecat2,maritalcat2,pacat2,sleepcat2,smokecat2,drinkcat2,incat2,vegcat52,vegcat42,frucat52,frucat42,fvcat52,fvcat42))
library(Hmisc)
describe(describe)

describe$agecat<- NA
describe$agecat[describe$age>=18 & describe$age<=24] <- 1
describe$agecat[describe$age>=25 & describe$age<=34] <- 2
describe$agecat[describe$age>=35 & describe$age<=44] <- 3
describe$agecat[describe$age>=45 & describe$age<=54] <- 4
describe$agecat[describe$age>=55 & describe$age<=64] <- 5
describe$agecat[describe$age>=65] <- 6
describe$agecat=factor(describe$agecat,levels=c(1,2,3,4,5,6),labels=c("18-24","25-34","35-44","45-54","55-64","65+"))
describe$sex=factor(describe$sex,levels=c(1,2),labels=c("male","female"))

install.packages("tableone")

library(tableone)
CreateTableOne(data = describe)
CreateTableOne(data = describe,strata = "vegcat52")
CreateTableOne(data = describe,strata = "frucat52")
CreateTableOne(data = describe,strata = "fvcat52")

library(stargazer)
expsurv<-subset(allinclude,select=c(IDind,bmi,index,d3kcal,age,censor,time,bmicat2,educat2,maritalcat2,pacat2,sleepcat2,incat2,vegcat52,vegcat42,frucat42,residencecat2,drinkcat2,fvcat52,fvcat42))
summary(expsurv)

########################sample w/ age, sex, and fv
finalwoco<-healthysample
quantile(finalwoco$meanv,c(0.2,0.4,0.6,0.8))
finalwoco$vegcat5<-NA
finalwoco$vegcat5[finalwoco$meanv<199.0000] <- 1
finalwoco$vegcat5[finalwoco$meanv>=199.0000&finalwoco$meanv<265.9524] <- 2
finalwoco$vegcat5[finalwoco$meanv>=265.9524&finalwoco$meanv<326.6667] <- 3
finalwoco$vegcat5[finalwoco$meanv>=326.6667&finalwoco$meanv<408.3333] <- 4
finalwoco$vegcat5[finalwoco$meanv>=408.3333] <- 5
finalwoco$vegcat5=factor(finalwoco$vegcat5,levels=c(1,2,3,4,5),labels=c("Q1","Q2","Q3","Q4","Q5"))

quantile(subsetfinalwocofru$meanf,c(0.25,0.5,0.75))
subsetfinalwocofru<-subset(finalwoco, meanf>0,select=c(meanf))
finalwoco$frucat5[finalwoco$meanf==0.00000] <- 1
finalwoco$frucat5[finalwoco$meanf>0.00000&finalwoco$meanf<25.0000] <- 2
finalwoco$frucat5[finalwoco$meanf>=25.0000&finalwoco$meanf<60.0000] <- 3
finalwoco$frucat5[finalwoco$meanf>=60.0000&finalwoco$meanf<125.6389] <- 4
finalwoco$frucat5[finalwoco$meanf>=125.6389] <- 5
finalwoco$frucat5=factor(finalwoco$frucat5,levels=c(1,2,3,4,5),labels=c("Q1","Q2","Q3","Q4","Q5"))

quantile(finalwoco$fvcombine,c(0.2,0.4,0.6,0.8))
finalwoco$fvcat5 <- NA
finalwoco$fvcat5[finalwoco$fvcombine<225.0000] <- 1
finalwoco$fvcat5[finalwoco$fvcombine>=225.0000&finalwoco$fvcombine<299.7222] <- 2
finalwoco$fvcat5[finalwoco$fvcombine>=299.7222&finalwoco$fvcombine<366.6667] <- 3
finalwoco$fvcat5[finalwoco$fvcombine>=366.6667&finalwoco$fvcombine<460.8111] <- 4
finalwoco$fvcat5[finalwoco$fvcombine>=460.8111] <- 5
finalwoco$fvcat5=factor(finalwoco$fvcat5,levels=c(1,2,3,4,5),labels=c("Q1","Q2","Q3","Q4","Q5"))

quantile(finalwoco$meanv,c(0.25,0.5,0.75))
finalwoco$vegcat4[finalwoco$meanv<216.6667] <- 1
finalwoco$vegcat4[finalwoco$meanv>=216.6667&finalwoco$meanv<296.1905] <- 2
finalwoco$vegcat4[finalwoco$meanv>=296.1905&finalwoco$meanv<383.3333] <- 3
finalwoco$vegcat4[finalwoco$meanv>=383.3333] <- 4
finalwoco$vegcat4=factor(finalwoco$vegcat4,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))

subsetfinalwocofru<-subset(finalwoco, meanf>0,select=c(meanf))
quantile(subsetfinalwocofru$meanf,c(1/3,2/3))
finalwoco$frucat4[finalwoco$meanf==0.00000] <- 1
finalwoco$frucat4[finalwoco$meanf>0.00000&finalwoco$meanf<33.33333] <- 2
finalwoco$frucat4[finalwoco$meanf>=33.33333&finalwoco$meanf<100.00000] <- 3
finalwoco$frucat4[finalwoco$meanf>=100.00000] <- 4
finalwoco$frucat4=factor(finalwoco$frucat4,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))

quantile(finalwoco$fvcombine,c(0.25,0.5,0.75))
finalwoco$fvcat4[finalwoco$fvcombine<246.6667] <- 1
finalwoco$fvcat4[finalwoco$fvcombine>=246.6667&finalwoco$fvcombine<333.3333] <- 2
finalwoco$fvcat4[finalwoco$fvcombine>=333.3333&finalwoco$fvcombine<433.3333] <- 3
finalwoco$fvcat4[finalwoco$fvcombine>=433.3333] <- 4
finalwoco$fvcat4=factor(finalwoco$fvcat4,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))

finalwoco$agecat<- NA
finalwoco$agecat[finalwoco$age>=18 & finalwoco$age<=24] <- 1
finalwoco$agecat[finalwoco$age>=25 & finalwoco$age<=34] <- 2
finalwoco$agecat[finalwoco$age>=35 & finalwoco$age<=44] <- 3
finalwoco$agecat[finalwoco$age>=45 & finalwoco$age<=54] <- 4
finalwoco$agecat[finalwoco$age>=55 & finalwoco$age<=64] <- 5
finalwoco$agecat[finalwoco$age>=65] <- 6

finalwoco$agecat=factor(finalwoco$agecat,levels=c(1,2,3,4,5,6),labels=c("18-24","25-34","35-44","45-54","55-64","65+"))

finalwoco$sex=factor(finalwoco$sex,levels=c(1,2),labels=c("male","female"))

finalonly<- subset(finalwoco,select=c(censor,time,vegcat5,frucat5,fvcat5, vegcat4, frucat4, fvcat4))

#Divide data and change data labels
data(healthysample)
glimpse(healthysample)

summary(healthysample)

str(healthysample)
summary(healthysample)
healthysample$sex2=factor(healthysample$sex,levels=c(1,2),labels=c("M","F"))
healthysample$vegiequin2=factor(healthysample$vegiequin,levels=c(1,2,3,4,5),labels=c("Q1","Q2","Q3","Q4","Q"))

mean(healthysample$meanv)
quantile(healthysample$meanv,c(0.2,0.4,0.6,0.8),na.rm=TRUE)
healthysample$vquin5[healthysample$meanv<199] <- 1
healthysample$vquin5[healthysample$meanv>=199&healthysample$meanv<265.953] <- 2
healthysample$vquin5[healthysample$meanv>=265.953&healthysample$meanv<326.67] <- 3
healthysample$vquin5[healthysample$meanv>=326.67&healthysample$meanv<408.33] <- 4
healthysample$vquin5[healthysample$meanv>=408.33] <- 5
healthysample$vquin52=factor(healthysample$vquin5,levels=c(1,2,3,4,5),labels=c("Q1","Q2","Q3","Q4","Q5"))

mean(healthysample$meanv)
quantile(healthysample$meanv,c(0.25,0.5,0.75),na.rm=TRUE)
healthysample$vquar[healthysample$meanv<216.6667] <- 1
healthysample$vquar[healthysample$meanv>=216.6667&healthysample$meanv<296.1905] <- 2
healthysample$vquar[healthysample$meanv>=296.1905&healthysample$meanv<383.3333] <- 3
healthysample$vquar[healthysample$meanv>=383.3333] <- 4
healthysample$vquar2=factor(healthysample$vquar,levels=c(1,2,3,4),labels=c("Q1","Q2","Q3","Q4"))

mean(healthysample$meanf)
quantile(healthysample$meanf,c(0.25,0.5,0.75),na.rm=TRUE)
healthysample$fruitquar[healthysample$meanf<=0.00] <- 1
healthysample$fruitquar[healthysample$meanf>=0.00&healthysample$meanf<0.00] <- 2
healthysample$fruitquar[healthysample$meanf>=0.00&healthysample$meanf<50] <- 3
healthysample$fruitquar[healthysample$meanf>=50] <- 4

subvegqq<-subset(healthysample,select=c(IDind,age,censor,time,sex2,vquin52,vquar2))

glimpse(subvegqq)

# Data seems to be bimodal
hist(healthysample$meanf) 

ovarian <- ovarian %>% mutate(age_group = ifelse(age >=50, "old", "young"))
ovarian$age_group <- factor(ovarian$age_group)

######
glimpse(finalonly)
# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = finalonly$time, event = finalonly$censor)
surv_object 
##  [1]   59   115   156   421+  431   448+  464   475   477+  563   638 
## [12]  744+  769+  770+  803+  855+ 1040+ 1106+ 1129+ 1206+ 1227+  268 
## [23]  329   353   365   377+

fit1 <- survfit(surv_object ~ frucat5, data = finalonly)
summary(fit1)


ggsurvplot(fit1, data = finalonly, pval = TRUE)

# Examine prdictive value of residual disease status
fit2 <- survfit(surv_object ~ frucat5, data = finalonly)
ggsurvplot(fit2, data = finalonly, pval = TRUE)

# Fit a Cox proportional hazards model
fit.coxph <- coxph(surv_object ~ vegcat5 + frucat5, 
                   data = finalonly)
ggforest(fit.coxph, data = finalonly)

#2
surv_object <- Surv(time = finalwoco$time, event = finalwoco$censor)
surv_object 

fit2 <- survfit(surv_object ~ vegcat4, data = finalwoco)
summary(fit2)

ggsurvplot(fit2, data = finalwoco, pval = TRUE)

# Fit a Cox proportional hazards model
fit.coxph <- coxph(surv_object ~ vegcat4 + frucat4 + fvcat4 +sex + agecat, 
                   data = finalwoco)
ggforest(fit.coxph, data = finalwoco)


#3
surv_object <- Surv(time = describe$time, event = describe$censor)
surv_object 
##  [1]   59   115   156   421+  431   448+  464   475   477+  563   638 
## [12]  744+  769+  770+  803+  855+ 1040+ 1106+ 1129+ 1206+ 1227+  268 
## [23]  329   353   365   377+

fit3 <- survfit(surv_object ~ vegcat52, data = describe)
summary(fit3)


ggsurvplot(fit3, data = describe, pval = TRUE)

# Fit a Cox proportional hazards model
fit.coxph <- coxph(surv_object ~ vegcat42 + frucat42 + fvcat42 + sex + agecat +
                     bmicat2 + residencecat2 + maritalcat2 + pacat2 + sleepcat2 + 
                     smokecat2 + drinkcat2 + incat2, 
                   data = describe)
ggforest(fit.coxph, data = describe)

#####################
survminer::ggsurvplot(
  fit = survival::survfit(survival::Surv(time, event) ~ 1, data = survsub), 
  xlab = "Years", 
  ylab = "Overall survival probability")
summary(survival::survfit(survival::Surv(time, event) ~ 1, data = survsub), 
        times = 5)

survival::survfit(survival::Surv(time,censor) ~ 1, data = finalwoco)

table(finalwoco$censor)

survfv$time[finalwoco$censor == 1] %>% 
  median

install.packages("dashboard")
??dashboard

mean(finalwoco$meanv)
sd(finalwoco$meanv)
mean(finalwoco$meanf)
sd(finalwoco$meanf)
mean(finalwoco$fvcombine)
sd(finalwoco$fvcombine)


###zhu's code###
library(survival)
install.packages("survminer")
library(survminer)
fit<-survfit(Surv(surlm,all)~treat,data = X1112)
ggsurvplot(fit,data = X1112,pval = TRUE,conf.int = TRUE, 
           title = "Figure1. Survival Curves by Treatment", linetype = "strata",
           risk.table = TRUE, legend.title = "treatment",
           legend.labs = c("None","Chemotherapy", "Surgery", "Operative adjuvant chemotherapy", "Comprehensive therapy", "Others"), 
           risk.table.col="strata",surv.median.line="hv",ggtheme=theme_bw(),palette=c("green","pink","#00AFBB","#FC4E07", "#E7B800", "sky blue"))