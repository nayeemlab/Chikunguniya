library(dplyr)
library("FactoMineR")
library("factoextra")
library(extrafont)
library(ggplot2)
library(pastecs)
library(corrplot)
library(ppcor)
library(factoextra)
library(psych)
library(GPArotation)
library(Hmisc)
library(dplyr)
library(ape)
library(psych)
library(psychometric)

setwd('E:\\ResearchProject\\Sorowar Sir\\Chikunguniya')
ChikData <- read.csv("Data.csv")

#Age Category
ChikData$age_years
ChikData$age_years_group[ChikData$age_years <  15]  = 1
ChikData$age_years_group[ChikData$age_years >= 15 & ChikData$age_years <=  29]  = 2
ChikData$age_years_group[ChikData$age_years >= 30 & ChikData$age_years <= 59] = 3
ChikData$age_years_group[ChikData$age_years >= 60] = 4

ChikData$age_years_group <- factor(ChikData$age_years_group,levels=c(1,2,3,4),labels = c('<15','15-29','30-59','59+'))
ChikData$age_years_group

#Comorbidities
ChikData$comorbid_conditions___1
ChikData$comorbid_conditions___2
ChikData$comorbid_conditions___3
ChikData$comorbid_conditions___4
ChikData$comorbid_conditions___5
ChikData$comorbid_conditions___6
ChikData$comorbid_conditions___7
ChikData$comorbid_conditions___8
ChikData$comorbid <- ChikData$comorbid_conditions___1 + ChikData$comorbid_conditions___2 + ChikData$comorbid_conditions___3 + ChikData$comorbid_conditions___4 + 
  ChikData$comorbid_conditions___5 + ChikData$comorbid_conditions___6 + ChikData$comorbid_conditions___8
ChikData$comorbid
summary(ChikData$comorbid)

ChikData$comorbid_group[ChikData$comorbid <  1]  = 0
ChikData$comorbid_group[ChikData$comorbid >=  1]  = 1

ChikData$comorbid_group <- factor(ChikData$comorbid_group,levels=c(0,1),labels = c('No','Yes'))
ChikData$comorbid_group
summary(ChikData$comorbid_group)

#hypertesion
ChikData$hypertension <- factor(ChikData$comorbid_conditions___1,levels=c(0,1),labels = c('No','Yes'))
ChikData$hypertension 

#diabetes
ChikData$diabetes <- factor(ChikData$comorbid_conditions___2,levels=c(0,1),labels = c('No','Yes'))
ChikData$diabetes

#Stroke
ChikData$stroke <- factor(ChikData$comorbid_conditions___3,levels=c(0,1),labels = c('No','Yes'))
ChikData$stroke

#heart_disease
ChikData$heart_disease <- factor(ChikData$comorbid_conditions___4,levels=c(0,1),labels = c('No','Yes'))
ChikData$heart_disease

#chronic kidney disease
ChikData$ckd <- factor(ChikData$comorbid_conditions___5,levels=c(0,1),labels = c('No','Yes'))
ChikData$ckd

#COPD
ChikData$copd <- factor(ChikData$comorbid_conditions___6,levels=c(0,1),labels = c('No','Yes'))
ChikData$copd

#other
ChikData$other <- factor(ChikData$comorbid_conditions___8,levels=c(0,1),labels = c('No','Yes'))
ChikData$other

#Gender
ChikData$sex
ChikData$sex_group <- factor(ChikData$sex,levels=c(1,2),labels = c('Male','Female'))
ChikData$sex_group

#Hopitalization
ChikData$is_hospitalized
ChikData$is_hospitalized_group <- factor(ChikData$is_hospitalized,levels=c(0,1),labels = c('No','Yes'))
ChikData$is_hospitalized_group

#joint pain before fever
ChikData$is_joint_muscle_pain
ChikData$is_joint_muscle_pain_group <- factor(ChikData$is_joint_muscle_pain,levels=c(0,1),labels = c('No','Yes'))
ChikData$is_joint_muscle_pain_group

#Age Category
ChikData$arth_pain_rating
ChikData$arth_pain_rating_group[ChikData$arth_pain_rating <=  6]  = 1
ChikData$arth_pain_rating_group[ChikData$arth_pain_rating >= 7] = 2
ChikData$arth_pain_rating_group

ChikData$arth_pain_rating_group <- factor(ChikData$arth_pain_rating_group,levels=c(1,2),labels = c('Mild_Moderate','Severe'))
ChikData$arth_pain_rating_group

#joint pain days
ChikData$jointpain_days
ChikData$jointpain_days_new[ChikData$jointpain_days >  1]  = 1
ChikData$jointpain_days_new[ChikData$jointpain_days >  2]  = 2
ChikData$jointpain_days_new[ChikData$jointpain_days >  3]  = 3
ChikData$jointpain_days_new[ChikData$jointpain_days >  4]  = 4
ChikData$jointpain_days_new[ChikData$jointpain_days >  5]  = 5
ChikData$jointpain_days_new[ChikData$jointpain_days >  6]  = 6
ChikData$jointpain_days_new[ChikData$jointpain_days >  7]  = 7
ChikData$jointpain_days_new[ChikData$jointpain_days >  8]  = 8
ChikData$jointpain_days_new[ChikData$jointpain_days >  9]  = 9
ChikData$jointpain_days_new[ChikData$jointpain_days >  10]  = 10
ChikData$jointpain_days_new[ChikData$jointpain_days >  11]  = 11
ChikData$jointpain_days_new[ChikData$jointpain_days >  12]  = 12
ChikData$jointpain_days_new[ChikData$jointpain_days >  13]  = 13
ChikData$jointpain_days_new[ChikData$jointpain_days >=  14]  = 14
ChikData$jointpain_days_new

#sowlen_joint
ChikData$sowlen_joint
ChikData$sowlen_joint_group <- factor(ChikData$sowlen_joint,levels=c(0,1),labels = c('No','Yes'))
ChikData$sowlen_joint_group

#joint pain morning
ChikData$jointpain_morning
ChikData$jointpain_morning_group <- factor(ChikData$jointpain_morning,levels=c(0,1),labels = c('No','Yes'))
ChikData$jointpain_morning_group


#pain_location
ChikData$pain_location___1
ChikData$pain_location___2
ChikData$pain_location___3
ChikData$pain_location___4
ChikData$pain_location___5
ChikData$pain_location___6
ChikData$pain_location___7
ChikData$pain_location___8
ChikData$pain_location___9

ChikData$pain_location <- ChikData$pain_location___1 + ChikData$pain_location___2 + ChikData$pain_location___3 + ChikData$pain_location___4 + ChikData$pain_location___5 +
  ChikData$pain_location___6 + ChikData$pain_location___7 + ChikData$pain_location___8 + ChikData$pain_location___9
summary(ChikData$pain_location)

ChikData$pain_location_group[ChikData$pain_location <=  4]  = 1
ChikData$pain_location_group[ChikData$pain_location >=  5]  = 2

ChikData$pain_location_group <- factor(ChikData$pain_location_group,levels=c(1,2),labels = c('Oligoarthralgia','Polyarthralgia'))
ChikData$pain_location_group
summary(ChikData$pain_location_group)

#finger
ChikData$finger <- factor(ChikData$pain_location___1,levels=c(0,1),labels = c('No','Yes'))
ChikData$finger 

#wrist
ChikData$wrist <- factor(ChikData$pain_location___2,levels=c(0,1),labels = c('No','Yes'))
ChikData$wrist

#spine
ChikData$spine <- factor(ChikData$pain_location___3,levels=c(0,1),labels = c('No','Yes'))
ChikData$spine

#knee
ChikData$knee <- factor(ChikData$pain_location___4,levels=c(0,1),labels = c('No','Yes'))
ChikData$knee

#ankle
ChikData$ankle <- factor(ChikData$pain_location___5,levels=c(0,1),labels = c('No','Yes'))
ChikData$ankle

#feet
ChikData$feet <- factor(ChikData$pain_location___6,levels=c(0,1),labels = c('No','Yes'))
ChikData$feet

#shoulder
ChikData$shoulder <- factor(ChikData$pain_location___7,levels=c(0,1),labels = c('No','Yes'))
ChikData$shoulder


#sowlen_which_joint
ChikData$sowlen_which_joint___1
ChikData$sowlen_which_joint___2
ChikData$sowlen_which_joint___3
ChikData$sowlen_which_joint___4
ChikData$sowlen_which_joint___5

ChikData$sowlen_which_joint <- ChikData$sowlen_which_joint___1 + ChikData$sowlen_which_joint___2 + 
  ChikData$sowlen_which_joint___3 +ChikData$sowlen_which_joint___4 + ChikData$sowlen_which_joint___5
summary(ChikData$sowlen_which_joint)

#redish joint
ChikData$redish_joint_group <- factor(ChikData$redish_joint,levels=c(0,1),labels = c('No','Yes'))
ChikData$redish_joint_group

#Walking
ChikData$Walking <- factor(ChikData$jointpain_reason_list___1,levels=c(0,1),labels = c('No','Yes'))
ChikData$Walking

#sitting
ChikData$sitting <- factor(ChikData$jointpain_reason_list___2,levels=c(0,1),labels = c('No','Yes'))
ChikData$sitting

#standing up
ChikData$standing <- factor(ChikData$jointpain_reason_list___3,levels=c(0,1),labels = c('No','Yes'))
ChikData$standing

#is_rash
ChikData$skin_rash <- factor(ChikData$is_rash,levels=c(0,1),labels = c('No','Yes'))
ChikData$skin_rash

#is_itchy
ChikData$itching <- factor(ChikData$is_itchy,levels=c(0,1),labels = c('No','Yes'))
ChikData$itching

#is_headache
ChikData$headache <- factor(ChikData$is_headache,levels=c(0,1),labels = c('No','Yes'))
ChikData$headache

#is_muscle_ache
ChikData$myalgia <- factor(ChikData$is_muscle_ache,levels=c(0,1),labels = c('No','Yes'))
ChikData$myalgia

#is_red_eye
ChikData$red_eye <- factor(ChikData$is_red_eye,levels=c(0,1),labels = c('No','Yes'))
ChikData$red_eye

#is_eye_pain
ChikData$retroorbitalpain <- factor(ChikData$is_eye_pain,levels=c(0,1),labels = c('No','Yes'))
ChikData$retroorbitalpain

#body_water
ChikData$edema <- factor(ChikData$body_water,levels=c(0,1),labels = c('No','Yes'))
ChikData$edema




#Loss appetite 
ChikData$lossappetite  <- factor(ChikData$symptoms_when_fever___1,levels=c(0,1),labels = c('No','Yes'))
ChikData$lossappetite

#Nausea
ChikData$nausea <- factor(ChikData$symptoms_when_fever___2,levels=c(0,1),labels = c('No','Yes'))
ChikData$nausea

#Diarrhea
ChikData$diarrhea <- factor(ChikData$symptoms_when_fever___3,levels=c(0,1),labels = c('No','Yes'))
ChikData$diarrhea

#Abdominal cramp
ChikData$abdominalcramp <- factor(ChikData$symptoms_when_fever___4,levels=c(0,1),labels = c('No','Yes'))
ChikData$abdominalcramp

#Irregular bowel movement
ChikData$Irregularbowelmovement <- factor(ChikData$symptoms_when_fever___5,levels=c(0,1),labels = c('No','Yes'))
ChikData$Irregularbowelmovement



#chestpain
ChikData$chestpain <- factor(ChikData$other_symptom___1,levels=c(0,1),labels = c('No','Yes'))
ChikData$chestpain

#blurredvision
ChikData$blurredvision <- factor(ChikData$other_symptom___2,levels=c(0,1),labels = c('No','Yes'))
ChikData$blurredvision

#memoryloss
ChikData$memoryloss <- factor(ChikData$other_symptom___3,levels=c(0,1),labels = c('No','Yes'))
ChikData$memoryloss

#Pigmentation
ChikData$Pigmentation <- factor(ChikData$is_black_spot,levels=c(0,1),labels = c('No','Yes'))
ChikData$Pigmentation

#Oralulcer 
ChikData$Oralulcer  <- factor(ChikData$mouth_wound,levels=c(0,1),labels = c('No','Yes'))
ChikData$Oralulcer 

#Dropbloodpressure 
ChikData$Dropbloodpressure  <- factor(ChikData$history_pain,levels=c(0,1),labels = c('No','Yes'))
ChikData$Dropbloodpressure 

#Maculopapular_eruption 
ChikData$Maculopapular_eruption  <- factor(ChikData$is_large_rash,levels=c(0,1),labels = c('No','Yes'))
ChikData$Maculopapular_eruption 

#Bleeding manifestations 
ChikData$Bleeding_manifestations  <- factor(ChikData$is_bleeding,levels=c(0,1),labels = c('No','Yes'))
ChikData$Bleeding_manifestations


#Results

#Crosstab Comorbid and Age
c <- table(ChikData$age_years_group,ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)


#Crosstab Comorbid and sex
c <- table(ChikData$sex_group,ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)


#Crosstab Comorbid and hospitalize
c <- table(ChikData$is_hospitalized_group,ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Sex and Hypertension
c <- table(ChikData$hypertension ,ChikData$sex_group)
c
prop.table(c,2)*100
summary(c)
c <- table(ChikData$hypertension)
c
prop.table(c)*100


#Crosstab Sex and Diabetes
c <- table(ChikData$diabetes ,ChikData$sex_group)
c
prop.table(c,2)*100
summary(c)
c <- table(ChikData$diabetes)
c
prop.table(c)*100


#Crosstab Sex and stroke
c <- table(ChikData$stroke ,ChikData$sex_group)
c
prop.table(c,2)*100
summary(c)
test <- fisher.test(ChikData$stroke ,ChikData$sex_group)
test
c <- table(ChikData$stroke)
c
prop.table(c)*100

test <- fisher.test(ChikData$stroke ,ChikData$sex_group)
test


#Crosstab Sex and heart_disease
c <- table(ChikData$heart_disease ,ChikData$sex_group)
c
prop.table(c,2)*100
summary(c)
c <- table(ChikData$heart_disease)
c
prop.table(c)*100

#Crosstab Sex and ckd
c <- table(ChikData$ckd ,ChikData$sex_group)
c
prop.table(c,2)*100
summary(c)
test <- fisher.test(ChikData$ckd ,ChikData$sex_group)
test
c <- table(ChikData$ckd)
c
prop.table(c)*100


#Crosstab Sex and copd
c <- table(ChikData$copd ,ChikData$sex_group)
c
prop.table(c,2)*100
summary(c)
test <- fisher.test(ChikData$copd ,ChikData$sex_group)
test
c <- table(ChikData$copd)
c
prop.table(c)*100


#Crosstab Sex and other
c <- table(ChikData$other ,ChikData$sex_group)
c
prop.table(c,2)*100
summary(c)
c <- table(ChikData$other)
c
prop.table(c)*100



###############################################


#Crosstab muscle pain before fever and comorbid
c <- table(ChikData$is_joint_muscle_pain_group ,ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)


#Crosstab pain SEVERITY and comorbid
c <- table(ChikData$arth_pain_rating_group, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)


#Mean test join pain score and comorbid
t.test(ChikData$arth_pain_rating ~ ChikData$comorbid_group)

group_by(ChikData, comorbid_group) %>%
  summarise(
    count = n(),
    mean = mean(jointpain_days_new, na.rm = TRUE),
    sd = sd(jointpain_days_new, na.rm = TRUE)
  )


#Mean test join pain days and comorbid
t.test(ChikData$jointpain_days_new ~ ChikData$comorbid_group)

group_by(ChikData, comorbid_group) %>%
  summarise(
    count = n(),
    mean = mean(jointpain_days_new, na.rm = TRUE),
    sd = sd(jointpain_days_new, na.rm = TRUE)
  )

#Crosstab jointpain_morning and comorbid
c <- table(ChikData$jointpain_morning_group, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab sowlen_joint and comorbid
c <- table(ChikData$sowlen_joint_group, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab pain_location and comorbid
c <- table(ChikData$pain_location_group, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$finger, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$wrist, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$spine, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$knee, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$ankle, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$feet, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)


#Crosstab finger and comorbid
c <- table(ChikData$shoulder, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Mean test sowlen which joint and comorbid
t.test(ChikData$sowlen_which_joint ~ ChikData$comorbid_group)

group_by(ChikData, comorbid_group) %>%
  summarise(
    count = n(),
    mean = mean(sowlen_which_joint, na.rm = TRUE),
    sd = sd(sowlen_which_joint, na.rm = TRUE)
  )

#Crosstab redish_joint and comorbid
c <- table(ChikData$redish_joint_group, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab walking and comorbid
c <- table(ChikData$Walking, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab sitting and comorbid
c <- table(ChikData$sitting, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab standing and comorbid
c <- table(ChikData$standing, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab rash and comorbid
c <- table(ChikData$is_rash, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab itching and comorbid
c <- table(ChikData$itching, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab headache and comorbid
c <- table(ChikData$headache, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab myalgia ache and comorbid
c <- table(ChikData$myalgia, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab red eye and comorbid
c <- table(ChikData$red_eye, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab eye pain and comorbid
c <- table(ChikData$retroorbitalpain, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab body water and comorbid
c <- table(ChikData$edema, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Loss appetite and comorbid
c <- table(ChikData$lossappetite, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Nausea and comorbid
c <- table(ChikData$nausea, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Diarrhea and comorbid
c <- table(ChikData$diarrhea, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Abdominal cramp and comorbid
c <- table(ChikData$abdominalcramp, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Irregular bowel movement and comorbid
c <- table(ChikData$Irregularbowelmovement, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)


#Crosstab chest pain and comorbid
c <- table(ChikData$chestpain, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab blurredvision and comorbid
c <- table(ChikData$blurredvision, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab memoryloss and comorbid
c <- table(ChikData$memoryloss, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Pigmentation and comorbid
c <- table(ChikData$Pigmentation, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Oralulcer and comorbid
c <- table(ChikData$Oralulcer, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Dropbloodpressure and comorbid
c <- table(ChikData$Dropbloodpressure, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)


#Crosstab Maculopapular_eruption and comorbid
c <- table(ChikData$Maculopapular_eruption, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Bleeding_manifestations and comorbid
c <- table(ChikData$Bleeding_manifestations, ChikData$comorbid_group)
c
prop.table(c,2)*100
summary(c)


#Mean test sowlen which joint and comorbid
t.test(ChikData$days_missed_work ~ ChikData$comorbid_group)

group_by(ChikData, comorbid_group) %>%
  summarise(
    count = n(),
    mean = mean(days_missed_work, na.rm = TRUE),
    sd = sd(days_missed_work, na.rm = TRUE)
  )



###############################################################################


#Crosstab arth_pain_rating_group and Hypertension
c <- table(ChikData$hypertension ,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab arth_pain_rating_group and Diabetes
c <- table(ChikData$diabetes ,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)


#Crosstab Sex and stroke
c <- table(ChikData$stroke ,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)
test <- fisher.test(ChikData$stroke ,ChikData$arth_pain_rating_group)
test


#Crosstab arth_pain_rating_group and heart_disease
c <- table(ChikData$heart_disease ,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Sex and ckd
c <- table(ChikData$ckd ,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)
test <- fisher.test(ChikData$ckd ,ChikData$arth_pain_rating_group)
test


#Crosstab arth_pain_rating_group and copd
c <- table(ChikData$copd ,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)
test <- fisher.test(ChikData$copd ,ChikData$arth_pain_rating_group)
test


#Crosstab arth_pain_rating_group and other
c <- table(ChikData$other ,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)



######################################################################

#Crosstab comorbidity and arth_pain_rating_group
c <- table(ChikData$comorbid_group, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)


#Crosstab muscle pain before fever and arth_pain_rating_group
c <- table(ChikData$is_joint_muscle_pain_group ,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Mean test join pain score and comorbid
t.test(ChikData$arth_pain_rating ~ ChikData$arth_pain_rating_group)

group_by(ChikData, arth_pain_rating_group) %>%
  summarise(
    count = n(),
    mean = mean(jointpain_days_new, na.rm = TRUE),
    sd = sd(jointpain_days_new, na.rm = TRUE)
  )

#Mean test join pain days and arth_pain_rating_group
t.test(ChikData$jointpain_days_new ~ ChikData$arth_pain_rating_group)

group_by(ChikData, arth_pain_rating_group) %>%
  summarise(
    count = n(),
    mean = mean(jointpain_days_new, na.rm = TRUE),
    sd = sd(jointpain_days_new, na.rm = TRUE)
  )

#Crosstab jointpain_morning and arth_pain_rating_group
c <- table(ChikData$jointpain_morning_group, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab sowlen_joint and arth_pain_rating_group
c <- table(ChikData$sowlen_joint_group, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab pain_location and arth_pain_rating_group
c <- table(ChikData$pain_location_group, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)


#Crosstab finger and comorbid
c <- table(ChikData$finger, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$wrist, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$spine, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$knee, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$ankle, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$feet, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)


#Crosstab finger and comorbid
c <- table(ChikData$shoulder, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)



#Mean test sowlen which joint and arth_pain_rating_group
t.test(ChikData$sowlen_which_joint ~ ChikData$arth_pain_rating_group)

group_by(ChikData, arth_pain_rating_group) %>%
  summarise(
    count = n(),
    mean = mean(sowlen_which_joint, na.rm = TRUE),
    sd = sd(sowlen_which_joint, na.rm = TRUE)
  )

#Crosstab redish_joint and arth_pain_rating_group
c <- table(ChikData$redish_joint_group, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab walking and arth_pain_rating_group
c <- table(ChikData$Walking, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab sitting and arth_pain_rating_group
c <- table(ChikData$sitting, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab standing and arth_pain_rating_group
c <- table(ChikData$standing, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab rash and arth_pain_rating_group
c <- table(ChikData$skin_rash, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab itching and comorbid
c <- table(ChikData$itching, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab headache and  arth_pain_rating_group
c <- table(ChikData$headache, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab myalgia ache and arth_pain_rating_group
c <- table(ChikData$myalgia, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab red eye and arth_pain_rating_group
c <- table(ChikData$red_eye, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab eye pain and arth_pain_rating_group
c <- table(ChikData$retroorbitalpain, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab body water and arth_pain_rating_group
c <- table(ChikData$edema, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Loss appetite and arth_pain_rating_group
c <- table(ChikData$lossappetite, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Nausea and arth_pain_rating_group
c <- table(ChikData$nausea, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Diarrhea and arth_pain_rating_group
c <- table(ChikData$diarrhea, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Abdominal cramp and arth_pain_rating_group
c <- table(ChikData$abdominalcramp, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Irregular bowel movement and arth_pain_rating_group
c <- table(ChikData$Irregularbowelmovement, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)


#Crosstab chest pain and arth_pain_rating_group
c <- table(ChikData$chestpain, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab blurredvision and arth_pain_rating_group
c <- table(ChikData$blurredvision, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab memoryloss and arth_pain_rating_group
c <- table(ChikData$memoryloss, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Pigmentation and arth_pain_rating_group
c <- table(ChikData$Pigmentation, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Oralulcer and arth_pain_rating_group
c <- table(ChikData$Oralulcer, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Dropbloodpressure and arth_pain_rating_group
c <- table(ChikData$Dropbloodpressure, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)


#Crosstab Maculopapular_eruption and arth_pain_rating_group
c <- table(ChikData$Maculopapular_eruption, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Bleeding_manifestations and arth_pain_rating_group
c <- table(ChikData$Bleeding_manifestations, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)


#Mean test sowlen which joint and arth_pain_rating_group
t.test(ChikData$days_missed_work ~ ChikData$arth_pain_rating_group)

group_by(ChikData, arth_pain_rating_group) %>%
  summarise(
    count = n(),
    mean = mean(days_missed_work, na.rm = TRUE),
    sd = sd(days_missed_work, na.rm = TRUE)
  )


###############################################################################

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$comorbid_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$is_joint_muscle_pain_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$jointpain_morning_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$sowlen_joint_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$pain_location_group),ref = "Oligoarthralgia"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$finger),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$wrist),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$spine),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$knee),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$ankle),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$feet),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$shoulder),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$redish_joint_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$Walking),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$sitting),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$standing),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$skin_rash),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$itching),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$headache),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$myalgia),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$red_eye),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$retroorbitalpain),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$edema),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$lossappetite),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$nausea),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$diarrhea),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$abdominalcramp),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$Irregularbowelmovement),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$chestpain),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$blurredvision),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$memoryloss),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$Pigmentation),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$Oralulcer),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$Dropbloodpressure),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$Maculopapular_eruption),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$Bleeding_manifestations),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

#########################################################################################
#logistic odds ratio

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$jointpain_morning_group),ref = "No") +
                relevel(factor(ChikData$sowlen_joint_group),ref = "No") + relevel(factor(ChikData$pain_location_group),ref = "Oligoarthralgia") +
                relevel(factor(ChikData$finger),ref = "No") + relevel(factor(ChikData$wrist),ref = "No") +
                relevel(factor(ChikData$spine),ref = "No") + relevel(factor(ChikData$knee),ref = "No") +
                relevel(factor(ChikData$ankle),ref = "No") + relevel(factor(ChikData$feet),ref = "No") +
                relevel(factor(ChikData$shoulder),ref = "No") + relevel(factor(ChikData$redish_joint_group),ref = "No") +
                relevel(factor(ChikData$Walking),ref = "No") +  relevel(factor(ChikData$sitting),ref = "No") +
                relevel(factor(ChikData$standing),ref = "No") + 
                relevel(factor(ChikData$myalgia),ref = "No") + relevel(factor(ChikData$edema),ref = "No") +
                relevel(factor(ChikData$lossappetite),ref = "No") +
                relevel(factor(ChikData$abdominalcramp),ref = "No") + 
                relevel(factor(ChikData$Oralulcer),ref = "No") + relevel(factor(ChikData$Dropbloodpressure),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)

exp(cbind(coef(model), confint(model)))



###########################Hypertension###########################################


#Crosstab muscle pain before fever and arth_pain_rating_group
c <- table(ChikData$arth_pain_rating_group ,ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab muscle pain before fever and arth_pain_rating_group
c <- table(ChikData$is_joint_muscle_pain_group ,ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Mean test join pain score and comorbid
t.test(ChikData$arth_pain_rating ~ ChikData$hypertension)

group_by(ChikData, hypertension) %>%
  summarise(
    count = n(),
    mean = mean(arth_pain_rating, na.rm = TRUE),
    sd = sd(arth_pain_rating, na.rm = TRUE)
  )

#Mean test join pain days and arth_pain_rating_group
t.test(ChikData$jointpain_days_new ~ ChikData$hypertension)

#Mean test join pain days and arth_pain_rating_group
group_by(ChikData, hypertension) %>%
  summarise(
    count = n(),
    mean = mean(jointpain_days_new, na.rm = TRUE),
    sd = sd(jointpain_days_new, na.rm = TRUE)
  )

#Crosstab jointpain_morning and arth_pain_rating_group
c <- table(ChikData$jointpain_morning_group, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab sowlen_joint and arth_pain_rating_group
c <- table(ChikData$sowlen_joint_group, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab pain_location and arth_pain_rating_group
c <- table(ChikData$pain_location_group, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)


#Crosstab finger and comorbid
c <- table(ChikData$finger, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$wrist, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$spine, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$knee, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$ankle, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$feet, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)


#Crosstab finger and comorbid
c <- table(ChikData$shoulder, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)



#Mean test sowlen which joint and arth_pain_rating_group
t.test(ChikData$sowlen_which_joint ~ ChikData$hypertension)

group_by(ChikData, hypertension) %>%
  summarise(
    count = n(),
    mean = mean(sowlen_which_joint, na.rm = TRUE),
    sd = sd(sowlen_which_joint, na.rm = TRUE)
  )

#Crosstab redish_joint and arth_pain_rating_group
c <- table(ChikData$redish_joint_group, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab walking and arth_pain_rating_group
c <- table(ChikData$Walking, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab sitting and arth_pain_rating_group
c <- table(ChikData$sitting, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab standing and arth_pain_rating_group
c <- table(ChikData$standing, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab rash and arth_pain_rating_group
c <- table(ChikData$skin_rash, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab itching and comorbid
c <- table(ChikData$itching, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab headache and  arth_pain_rating_group
c <- table(ChikData$headache, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab myalgia ache and arth_pain_rating_group
c <- table(ChikData$myalgia, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab red eye and arth_pain_rating_group
c <- table(ChikData$red_eye, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab eye pain and arth_pain_rating_group
c <- table(ChikData$retroorbitalpain, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab body water and arth_pain_rating_group
c <- table(ChikData$edema, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab Loss appetite and arth_pain_rating_group
c <- table(ChikData$lossappetite, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab Nausea and arth_pain_rating_group
c <- table(ChikData$nausea, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab Diarrhea and arth_pain_rating_group
c <- table(ChikData$diarrhea, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab Abdominal cramp and arth_pain_rating_group
c <- table(ChikData$abdominalcramp, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab Irregular bowel movement and arth_pain_rating_group
c <- table(ChikData$Irregularbowelmovement, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)


#Crosstab chest pain and arth_pain_rating_group
c <- table(ChikData$chestpain, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab blurredvision and arth_pain_rating_group
c <- table(ChikData$blurredvision, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab memoryloss and arth_pain_rating_group
c <- table(ChikData$memoryloss, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab Pigmentation and arth_pain_rating_group
c <- table(ChikData$Pigmentation, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab Oralulcer and arth_pain_rating_group
c <- table(ChikData$Oralulcer, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab Dropbloodpressure and arth_pain_rating_group
c <- table(ChikData$Dropbloodpressure, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)


#Crosstab Maculopapular_eruption and arth_pain_rating_group
c <- table(ChikData$Maculopapular_eruption, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)

#Crosstab Bleeding_manifestations and arth_pain_rating_group
c <- table(ChikData$Bleeding_manifestations, ChikData$hypertension)
c
prop.table(c,2)*100
summary(c)


#Mean test sowlen which joint and arth_pain_rating_group
t.test(ChikData$days_missed_work ~ ChikData$hypertension)

group_by(ChikData, hypertension) %>%
  summarise(
    count = n(),
    mean = mean(days_missed_work, na.rm = TRUE),
    sd = sd(days_missed_work, na.rm = TRUE)
  )




###########################Diabetes###########################################

#Crosstab muscle pain before fever and arth_pain_rating_group
c <- table(ChikData$arth_pain_rating_group ,ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab muscle pain before fever and arth_pain_rating_group
c <- table(ChikData$is_joint_muscle_pain_group ,ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Mean test join pain score and comorbid
t.test(ChikData$arth_pain_rating ~ ChikData$diabetes)

group_by(ChikData, diabetes) %>%
  summarise(
    count = n(),
    mean = mean(arth_pain_rating, na.rm = TRUE),
    sd = sd(arth_pain_rating, na.rm = TRUE)
  )

#Mean test join pain days and arth_pain_rating_group
t.test(ChikData$jointpain_days_new ~ ChikData$diabetes)

#Mean test join pain days and arth_pain_rating_group
group_by(ChikData, diabetes) %>%
  summarise(
    count = n(),
    mean = mean(jointpain_days_new, na.rm = TRUE),
    sd = sd(jointpain_days_new, na.rm = TRUE)
  )

#Crosstab jointpain_morning and arth_pain_rating_group
c <- table(ChikData$jointpain_morning_group, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab sowlen_joint and arth_pain_rating_group
c <- table(ChikData$sowlen_joint_group, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab pain_location and arth_pain_rating_group
c <- table(ChikData$pain_location_group, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)


#Crosstab finger and comorbid
c <- table(ChikData$finger, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$wrist, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$spine, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$knee, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$ankle, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$feet, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)


#Crosstab finger and comorbid
c <- table(ChikData$shoulder, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)



#Mean test sowlen which joint and arth_pain_rating_group
t.test(ChikData$sowlen_which_joint ~ ChikData$diabetes)

group_by(ChikData, diabetes) %>%
  summarise(
    count = n(),
    mean = mean(sowlen_which_joint, na.rm = TRUE),
    sd = sd(sowlen_which_joint, na.rm = TRUE)
  )

#Crosstab redish_joint and arth_pain_rating_group
c <- table(ChikData$redish_joint_group, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab walking and arth_pain_rating_group
c <- table(ChikData$Walking, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab sitting and arth_pain_rating_group
c <- table(ChikData$sitting, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab standing and arth_pain_rating_group
c <- table(ChikData$standing, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab rash and arth_pain_rating_group
c <- table(ChikData$skin_rash, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab itching and comorbid
c <- table(ChikData$itching, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab headache and  arth_pain_rating_group
c <- table(ChikData$headache, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab myalgia ache and arth_pain_rating_group
c <- table(ChikData$myalgia, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab red eye and arth_pain_rating_group
c <- table(ChikData$red_eye, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab eye pain and arth_pain_rating_group
c <- table(ChikData$retroorbitalpain, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab body water and arth_pain_rating_group
c <- table(ChikData$edema, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab Loss appetite and arth_pain_rating_group
c <- table(ChikData$lossappetite, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab Nausea and arth_pain_rating_group
c <- table(ChikData$nausea, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab Diarrhea and arth_pain_rating_group
c <- table(ChikData$diarrhea, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab Abdominal cramp and arth_pain_rating_group
c <- table(ChikData$abdominalcramp, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab Irregular bowel movement and arth_pain_rating_group
c <- table(ChikData$Irregularbowelmovement, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)


#Crosstab chest pain and arth_pain_rating_group
c <- table(ChikData$chestpain, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab blurredvision and arth_pain_rating_group
c <- table(ChikData$blurredvision, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab memoryloss and arth_pain_rating_group
c <- table(ChikData$memoryloss, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab Pigmentation and arth_pain_rating_group
c <- table(ChikData$Pigmentation, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab Oralulcer and arth_pain_rating_group
c <- table(ChikData$Oralulcer, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab Dropbloodpressure and arth_pain_rating_group
c <- table(ChikData$Dropbloodpressure, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)


#Crosstab Maculopapular_eruption and arth_pain_rating_group
c <- table(ChikData$Maculopapular_eruption, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

#Crosstab Bleeding_manifestations and arth_pain_rating_group
c <- table(ChikData$Bleeding_manifestations, ChikData$diabetes)
c
prop.table(c,2)*100
summary(c)

test <- fisher.test(ChikData$Bleeding_manifestations, ChikData$diabetes)
test

#Mean test sowlen which joint and arth_pain_rating_group
t.test(ChikData$days_missed_work ~ ChikData$diabetes)

group_by(ChikData, diabetes) %>%
  summarise(
    count = n(),
    mean = mean(days_missed_work, na.rm = TRUE),
    sd = sd(days_missed_work, na.rm = TRUE)
  )

###########################Heart Disease###########################################

#Crosstab muscle pain before fever and arth_pain_rating_group
c <- table(ChikData$arth_pain_rating_group ,ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)


#Crosstab muscle pain before fever and arth_pain_rating_group
c <- table(ChikData$is_joint_muscle_pain_group ,ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Mean test join pain score and comorbid
t.test(ChikData$arth_pain_rating ~ ChikData$heart_disease)

group_by(ChikData, heart_disease) %>%
  summarise(
    count = n(),
    mean = mean(arth_pain_rating, na.rm = TRUE),
    sd = sd(arth_pain_rating, na.rm = TRUE)
  )

#Mean test join pain days and arth_pain_rating_group
t.test(ChikData$jointpain_days_new ~ ChikData$heart_disease)

#Mean test join pain days and arth_pain_rating_group
group_by(ChikData, heart_disease) %>%
  summarise(
    count = n(),
    mean = mean(jointpain_days_new, na.rm = TRUE),
    sd = sd(jointpain_days_new, na.rm = TRUE)
  )

#Crosstab jointpain_morning and arth_pain_rating_group
c <- table(ChikData$jointpain_morning_group, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab sowlen_joint and arth_pain_rating_group
c <- table(ChikData$sowlen_joint_group, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab pain_location and arth_pain_rating_group
c <- table(ChikData$pain_location_group, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)


#Crosstab finger and comorbid
c <- table(ChikData$finger, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$wrist, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$spine, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$knee, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$ankle, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab finger and comorbid
c <- table(ChikData$feet, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)


#Crosstab finger and comorbid
c <- table(ChikData$shoulder, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)



#Mean test sowlen which joint and arth_pain_rating_group
t.test(ChikData$sowlen_which_joint ~ ChikData$heart_disease)

group_by(ChikData, heart_disease) %>%
  summarise(
    count = n(),
    mean = mean(sowlen_which_joint, na.rm = TRUE),
    sd = sd(sowlen_which_joint, na.rm = TRUE)
  )

#Crosstab redish_joint and arth_pain_rating_group
c <- table(ChikData$redish_joint_group, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab walking and arth_pain_rating_group
c <- table(ChikData$Walking, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab sitting and arth_pain_rating_group
c <- table(ChikData$sitting, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab standing and arth_pain_rating_group
c <- table(ChikData$standing, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab rash and arth_pain_rating_group
c <- table(ChikData$skin_rash, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab itching and comorbid
c <- table(ChikData$itching, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab headache and  arth_pain_rating_group
c <- table(ChikData$headache, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab myalgia ache and arth_pain_rating_group
c <- table(ChikData$myalgia, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab red eye and arth_pain_rating_group
c <- table(ChikData$red_eye, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab eye pain and arth_pain_rating_group
c <- table(ChikData$retroorbitalpain, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab body water and arth_pain_rating_group
c <- table(ChikData$edema, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab Loss appetite and arth_pain_rating_group
c <- table(ChikData$lossappetite, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab Nausea and arth_pain_rating_group
c <- table(ChikData$nausea, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab Diarrhea and arth_pain_rating_group
c <- table(ChikData$diarrhea, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab Abdominal cramp and arth_pain_rating_group
c <- table(ChikData$abdominalcramp, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab Irregular bowel movement and arth_pain_rating_group
c <- table(ChikData$Irregularbowelmovement, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)


#Crosstab chest pain and arth_pain_rating_group
c <- table(ChikData$chestpain, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab blurredvision and arth_pain_rating_group
c <- table(ChikData$blurredvision, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab memoryloss and arth_pain_rating_group
c <- table(ChikData$memoryloss, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab Pigmentation and arth_pain_rating_group
c <- table(ChikData$Pigmentation, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab Oralulcer and arth_pain_rating_group
c <- table(ChikData$Oralulcer, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab Dropbloodpressure and arth_pain_rating_group
c <- table(ChikData$Dropbloodpressure, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)


#Crosstab Maculopapular_eruption and arth_pain_rating_group
c <- table(ChikData$Maculopapular_eruption, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

#Crosstab Bleeding_manifestations and arth_pain_rating_group
c <- table(ChikData$Bleeding_manifestations, ChikData$heart_disease)
c
prop.table(c,2)*100
summary(c)

test <- fisher.test(ChikData$Bleeding_manifestations, ChikData$heart_disease)
test

#Mean test sowlen which joint and arth_pain_rating_group
t.test(ChikData$days_missed_work ~ ChikData$heart_disease)

group_by(ChikData, heart_disease) %>%
  summarise(
    count = n(),
    mean = mean(days_missed_work, na.rm = TRUE),
    sd = sd(days_missed_work, na.rm = TRUE)
  )


#######################################Hypertension Logistic Model###################################

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$arth_pain_rating_group),ref = "Mild_Moderate"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$is_joint_muscle_pain_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$jointpain_morning_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$sowlen_joint_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$pain_location_group),ref = "Oligoarthralgia"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$finger),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$wrist),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$spine),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$knee),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$ankle),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$feet),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$shoulder),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$redish_joint_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$Walking),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$sitting),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$standing),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$skin_rash),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$itching),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$headache),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$myalgia),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$red_eye),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$retroorbitalpain),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$edema),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$lossappetite),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$nausea),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$diarrhea),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$abdominalcramp),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$Irregularbowelmovement),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$chestpain),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$blurredvision),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$memoryloss),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$Pigmentation),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$Oralulcer),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$Dropbloodpressure),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$Maculopapular_eruption),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$Bleeding_manifestations),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

#########################################################################################
#logistic odds ratio

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$jointpain_morning_group),ref = "No") +
                relevel(factor(ChikData$sowlen_joint_group),ref = "No") + relevel(factor(ChikData$pain_location_group),ref = "Oligoarthralgia") +
                relevel(factor(ChikData$finger),ref = "No") + relevel(factor(ChikData$wrist),ref = "No") +
                relevel(factor(ChikData$finger),ref = "No") + relevel(factor(ChikData$wrist),ref = "No") +
                relevel(factor(ChikData$feet),ref = "No") + relevel(factor(ChikData$shoulder),ref = "No") +
                relevel(factor(ChikData$shoulder),ref = "No") + relevel(factor(ChikData$sitting),ref = "No") +
                relevel(factor(ChikData$standing),ref = "No") + 
                relevel(factor(ChikData$skin_rash),ref = "No") + relevel(factor(ChikData$headache),ref = "No") +
                relevel(factor(ChikData$retroorbitalpain),ref = "No") +
                relevel(factor(ChikData$edema),ref = "No") + relevel(factor(ChikData$nausea),ref = "No") +
                relevel(factor(ChikData$diarrhea),ref = "No") +
                relevel(factor(ChikData$Dropbloodpressure),ref = "No") + relevel(factor(ChikData$Maculopapular_eruption),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)

exp(cbind(coef(model), confint(model)))



#######################################diabetes Logistic Model###################################

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$arth_pain_rating_group),ref = "Mild_Moderate"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$is_joint_muscle_pain_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$jointpain_morning_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$sowlen_joint_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$pain_location_group),ref = "Oligoarthralgia"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$finger),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$wrist),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$spine),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$knee),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$ankle),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$feet),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$shoulder),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$redish_joint_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$Walking),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$sitting),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$standing),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$skin_rash),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$itching),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$headache),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$myalgia),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$red_eye),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$retroorbitalpain),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$edema),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$lossappetite),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$nausea),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$diarrhea),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$abdominalcramp),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$Irregularbowelmovement),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$chestpain),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$blurredvision),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$memoryloss),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$Pigmentation),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$Oralulcer),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$Dropbloodpressure),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$Maculopapular_eruption),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$diabetes), ref = "No")~ relevel(factor(ChikData$Bleeding_manifestations),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

#########################################################################################
#logistic odds ratio

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$arth_pain_rating_group),ref = "Mild_Moderate") +
                relevel(factor(ChikData$jointpain_morning_group),ref = "No") +
                relevel(factor(ChikData$sowlen_joint_group),ref = "No") + relevel(factor(ChikData$pain_location_group),ref = "Oligoarthralgia") +
                relevel(factor(ChikData$finger),ref = "No") + relevel(factor(ChikData$knee),ref = "No") +
                relevel(factor(ChikData$feet),ref = "No") +
                relevel(factor(ChikData$shoulder),ref = "No") + relevel(factor(ChikData$sitting),ref = "No") +
                relevel(factor(ChikData$standing),ref = "No") + 
                relevel(factor(ChikData$skin_rash),ref = "No") + relevel(factor(ChikData$headache),ref = "No") +
                relevel(factor(ChikData$myalgia),ref = "No") +
                relevel(factor(ChikData$edema),ref = "No") + relevel(factor(ChikData$nausea),ref = "No") +
                relevel(factor(ChikData$blurredvision),ref = "No") +
                relevel(factor(ChikData$Dropbloodpressure),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)

round(exp(cbind(coef(model), confint(model))),2)




#######################################heart disease Logistic Model###################################

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$arth_pain_rating_group),ref = "Mild_Moderate"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$is_joint_muscle_pain_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$jointpain_morning_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$sowlen_joint_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$pain_location_group),ref = "Oligoarthralgia"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$finger),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$wrist),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$spine),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$knee),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$ankle),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$feet),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$shoulder),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$redish_joint_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$Walking),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$sitting),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$standing),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$skin_rash),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$itching),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$headache),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$myalgia),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$red_eye),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$retroorbitalpain),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$edema),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$lossappetite),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$nausea),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$diarrhea),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$abdominalcramp),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$Irregularbowelmovement),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$chestpain),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$blurredvision),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$memoryloss),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$Pigmentation),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$Oralulcer),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$Dropbloodpressure),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$Maculopapular_eruption),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$heart_disease), ref = "No")~ relevel(factor(ChikData$Bleeding_manifestations),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

#########################################################################################
#logistic odds ratio

model <- glm( relevel(factor(ChikData$hypertension), ref = "No")~ relevel(factor(ChikData$jointpain_morning_group),ref = "No") +
                relevel(factor(ChikData$sowlen_joint_group),ref = "No") + relevel(factor(ChikData$pain_location_group),ref = "Oligoarthralgia") +
                relevel(factor(ChikData$finger),ref = "No") + + relevel(factor(ChikData$wrist),ref = "No") +
                relevel(factor(ChikData$spine),ref = "No") + relevel(factor(ChikData$knee),ref = "No") +
                relevel(factor(ChikData$ankle),ref = "No") + relevel(factor(ChikData$feet),ref = "No") +
                relevel(factor(ChikData$shoulder),ref = "No") + relevel(factor(ChikData$redish_joint_group),ref = "No") +
                relevel(factor(ChikData$Walking),ref = "No") + relevel(factor(ChikData$sitting),ref = "No") +
                relevel(factor(ChikData$standing),ref = "No") + 
                relevel(factor(ChikData$skin_rash),ref = "No") + relevel(factor(ChikData$headache),ref = "No") +
                relevel(factor(ChikData$red_eye),ref = "No") +
                relevel(factor(ChikData$edema),ref = "No") + relevel(factor(ChikData$lossappetite),ref = "No") +
                relevel(factor(ChikData$chestpain),ref = "No") + relevel(factor(ChikData$Dropbloodpressure),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)

round(exp(cbind(coef(model), confint(model))),2)
