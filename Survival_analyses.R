library(plyr)
library(ggpubr)
library(survival)
library(survminer)
library(lubridate)
library(dplyr)
library(ggplot2)
library(coxme)
library(tidyverse) 

# set environment
rm(list = ls())
setwd("~/Documents/R_scripts/NACC/Psychosis/")

# load data: (MBI+ rolling vs No NPS until dementia dx) and arrange it
NACC_all <- read.csv( "clean_nacc_reformated4cox_noNPS_tilDemDX2.csv")
NACC <- NACC_all %>% arrange(naccid_number, followupdov)

# limit  analysis to 10 years - decided based on exploratory data analysis
NACC <- NACC %>% group_by(naccid_number) %>% 
  filter(followupdov <= 9.9) %>% ungroup

#---------------------------------------
# check for duplicated rows
duplicated_rows = NACC[duplicated(NACC), ] #0 duplicated rows

all.neg <- function(x) -1*abs(x)
all.pos <- function(x) +1*abs(x)
#---------------------------------------
# re-label categorical variables
# sex
NACC$SEX[NACC$SEX == 1] <- "Male"
NACC$SEX[NACC$SEX == 2] <- "Female"

# NIH derived race
NACC$RaceTag <- 0
NACC$RaceTag[NACC$NACCNIHR==1] <- "White"
NACC$RaceTag[NACC$NACCNIHR==2] <- "Black"
NACC$RaceTag[NACC$NACCNIHR==3] <- "Other"
NACC$RaceTag[NACC$NACCNIHR==4] <- "Other"
NACC$RaceTag[NACC$NACCNIHR==5] <- "Other"
NACC$RaceTag[NACC$NACCNIHR==6] <- "Other"
NACC$RaceTag[NACC$NACCNIHR==99] <- "Other"

# APOE-e4 status
NACC$ApoEe4_Status <- 0
NACC$ApoEe4_Status[NACC$NACCNE4S==0] <- "Noncarrier"
NACC$ApoEe4_Status[NACC$NACCNE4S != 0] <- "Carrier"

# cognitive diagnosis 
NACC$dxstatusBL[NACC$dxstatusBL==1]<- "NC"
NACC$dxstatusBL[NACC$dxstatusBL==2]<- "NC"
NACC$dxstatusBL[NACC$dxstatusBL==3]<- "MCI"
NACC$dxstatusBL = factor(NACC$dxstatusBL, levels = c("NC","MCI"))

# make concatenated variables to look at interaction later
NACC$Psych_Apoe4 = paste(NACC$Psych_grpBL, NACC$ApoEe4_Status, sep ="_")
NACC$Psych_Sex = paste(NACC$Psych_grpBL, NACC$SEX, sep ="_")
NACC$Psych_Race = paste(NACC$Psych_grpBL, NACC$RaceTag, sep ="_")
NACC$Psych_Cog = paste(NACC$Psych_grpBL, NACC$dxstatusBL, sep ="_")

#---------------------------------------
# categorize status based on diagnosis
NACC = NACC  %>% group_by(naccid_number) %>% 
  mutate(changed_diagnosis = case_when((length(unique(dxstatusdov)) > 1)  ~ "Worsened", 
                                       (length(unique(dxstatusdov)) <= 1) ~ "No_Change")) %>%
  mutate(changed_diagnosis = case_when((sum(diff(dxstatusdov)) < 0  && abs(sum(diff(dxstatusdov))) == sum(all.pos(diff(dxstatusdov))))  ~ "Improved", 
                                      (abs(sum(diff(dxstatusdov))) != sum(all.pos(diff(dxstatusdov)))) ~ "Complicated", 
                                      TRUE ~ changed_diagnosis)) 

#--------------Manipulating data to properly run cox regression---------------------
#Create a variable to label when diagnosis is dementia
NACC = NACC %>% mutate(Diagnosis_Dem = case_when(as.numeric(dxstatusdov) >3  ~ "1", 
                                                 as.numeric(dxstatusdov) <= 3 ~ "0"))

Worsened_diagnosis_participants = subset(NACC, changed_diagnosis == "Worsened" | changed_diagnosis == "Complicated")
Worsened_diagnosis_list = unique(Worsened_diagnosis_participants$naccid_number)

NACC$Diagnosis_end = 0
NACC$Diagnosis_end_visit = 999 #999: placeholder
NACC$Diagnosis_beyond_end = 0

# Labeling end points for cox regression (Diagnosis conversion)
# look for first instance of worsened status within the followup visits and mark that point as end point
for (p in Worsened_diagnosis_list) {
  for (visit in NACC[NACC$naccid_number == p,]$followupdov){
    if (nrow(NACC[NACC$naccid_number == p & NACC$followupdov == visit & NACC$Diagnosis_Dem == "1",]) != 0){
      if(NACC[NACC$naccid_number == p & NACC$followupdov == visit & NACC$Diagnosis_Dem == "1",]$followupdov == min(NACC[NACC$naccid_number == p & NACC$Diagnosis_Dem == "1",]$followupdov)){
        NACC[NACC$naccid_number == p & NACC$followupdov == visit,]$Diagnosis_end = 1
        NACC[NACC$naccid_number == p,]$Diagnosis_end_visit = visit 
      }
    }
  }
}
# if followup visit is beyond end visit (dementia diagnosis), then its a diagnosis_beyond_end case (to be removed afterwards)
for (p in Worsened_diagnosis_list) {
  for (visit in NACC[NACC$naccid_number == p,]$followupdov){
    if(NACC[NACC$naccid_number == p & NACC$followupdov == visit,]$followupdov > NACC[NACC$naccid_number == p & NACC$followupdov == visit,]$Diagnosis_end_visit){
      NACC[NACC$naccid_number == p & NACC$followupdov == visit,]$Diagnosis_beyond_end = 1 
    }
  }
}

# if there was no dementia event, the last followup visit is the final visit
NACC <- NACC %>% dplyr::mutate(Diagnosis_end_visit = case_when((Diagnosis_end_visit == 999) ~ last(followupdov), 
                                                               TRUE ~ Diagnosis_end_visit))

# leave out  visits after hitting end points (beyond end visit cases)
NACC_Diagnosis_Cox = subset(NACC, Diagnosis_beyond_end != 1)

#............... prepare variables (in their correct data type) for Cox model ............................
NACC_Diagnosis_Cox$ApoEe4_Status <- factor(NACC_Diagnosis_Cox$ApoEe4_Status, levels=c("Noncarrier","Carrier"))
NACC_Diagnosis_Cox$RaceTag <- factor(NACC_Diagnosis_Cox$RaceTag, levels=c("White","Black","Other"))
NACC_Diagnosis_Cox$SEX = factor(NACC_Diagnosis_Cox$SEX,levels=c("Male","Female"))
NACC_Diagnosis_Cox$Psych_grpBL = factor(NACC_Diagnosis_Cox$Psych_grpBL, levels = c("Psychosis-","Psychosis+"),
                                        labels = c("No NPS", "MBI Psychosis"))
NACC_Diagnosis_Cox$naccid_number = as.numeric (NACC_Diagnosis_Cox$naccid_number)
NACC_Diagnosis_Cox$newBL_Age = as.numeric (NACC_Diagnosis_Cox$newBL_Age)
NACC_Diagnosis_Cox$EDUC = as.numeric (NACC_Diagnosis_Cox$EDUC)
NACC_Diagnosis_Cox$dxstatusBL = factor(NACC_Diagnosis_Cox$dxstatusBL, levels = c("NC","MCI"))

# interaction variables:
NACC_Diagnosis_Cox$Psych_Apoe4 = factor(NACC_Diagnosis_Cox$Psych_Apoe4, levels = c("Psychosis-_Noncarrier","Psychosis+_Noncarrier",
                                                                                   "Psychosis-_Carrier","Psychosis+_Carrier"))
NACC_Diagnosis_Cox$Psych_Sex = factor(NACC_Diagnosis_Cox$Psych_Sex, levels = c("Psychosis-_Female","Psychosis+_Female",
                                                                               "Psychosis-_Male","Psychosis+_Male"))
NACC_Diagnosis_Cox$Psych_Race = factor(NACC_Diagnosis_Cox$Psych_Race, levels = c("Psychosis-_Black","Psychosis+_Black","Psychosis-_White","Psychosis+_White",
                                                                                 "Psychosis-_Other","Psychosis+_Other"))
NACC_Diagnosis_Cox$Psych_Cog = factor(NACC_Diagnosis_Cox$Psych_Cog, levels = c("Psychosis-_NC","Psychosis+_NC","Psychosis-_MCI","Psychosis+_MCI"))

# ...................prepare survival object..........................................
# Just keep the last visit, either  event visit (dementia) or last followup visit for cases with no event detected
Single_visit = NACC_Diagnosis_Cox %>% 
  group_by(naccid_number) %>%
  slice(which.max(followupdov))

# create survival object
surv_object_single <- Surv(time = Single_visit$Diagnosis_end_visit, event =Single_visit$Diagnosis_end)

#.....................KM survival curve...................................
fit_single <- survfit(surv_object_single ~ Psych_grpBL, data =Single_visit)
summary(fit_single)
ggsurvplot(fit_single, data = Single_visit, pval = TRUE, conf.int = TRUE, 
           risk.table.col = "strata", 
           risk.table = TRUE,
           palette = c("forestgreen", "dodgerblue3"),
           xlab = "Time in years", ylab = "Dementia-free survival probability",
           legend.title = "Strata",
           legend.labs = c("No NPS","MBI Psychosis"), 
           font.legend = c(12,"plain","black"))

# survival chance up to 5 years
summary(fit_single, times = 5)

#...............renaming labels for the final Cox forest plot......................
Single_visit$newBL_Age <- Single_visit$newBL_Age/10
names(Single_visit)[names(Single_visit)=="Psych_grpBL"] <- "NPS group"
names(Single_visit)[names(Single_visit)=="newBL_Age"] <- "Age"
names(Single_visit)[names(Single_visit)=="SEX"] <- "Sex"
names(Single_visit)[names(Single_visit)=="EDUC"] <- "Education"
names(Single_visit)[names(Single_visit)=="RaceTag"] <- "Race"
names(Single_visit)[names(Single_visit)=="ApoEe4_Status"] <- "APOE-e4 status"
names(Single_visit)[names(Single_visit)=="dxstatusBL"] <- "Cognitive diagnosis"
names(Single_visit)[names(Single_visit)=="Psych_Apoe4"] <- "Psych x APOE-e4"
names(Single_visit)[names(Single_visit)=="Psych_Sex"] <- "Psych x Sex"
names(Single_visit)[names(Single_visit)=="Psych_Cog"] <- "Psych x Cog"
names(Single_visit)[names(Single_visit)=="Psych_Race"] <- "Psych x Race"

# Cox model and forest plot
cx1 <- coxph(surv_object_single ~ `NPS group` + `Age` + `Sex` + `Education` + `Race` + `APOE-e4 status` + `Cognitive diagnosis` , data =Single_visit)
summary(cx1)
ggforest(fit.coxph, data = Single_visit, fontsize = 1) 

# interaction APOE-e4 status
cx1 <- coxph(surv_object_single ~ `NPS group` * `APOE-e4 status` +`Age` +`Sex` +`Education` +`Race` +`Cognitive diagnosis`,data =Single_visit)
summary(cx1)

# interaction sex
cx1 <- coxph(surv_object_single ~ `NPS group` * `Sex` +`Age` +`APOE-e4 status` +`Education` +`Race` +`Cognitive diagnosis`,data =Single_visit)
summary(cx1)

# interaction cog
cx1  <- coxph(surv_object_single ~ `NPS group` * `Cognitive diagnosis` +`Age` +`APOE-e4 status` +`Education` +`Race` +`Sex`,data =Single_visit)
summary(cx1)
cx1  <- coxph(surv_object_single ~ `Psych x Cog` +`Age` +`APOE-e4 status` +`Education` +`Race` +`Sex`,data =Single_visit)
ggforest(fit.coxph, data = Single_visit, fontsize = 1) 

# interaction race
cx1 <- coxph(surv_object_single ~ `NPS group` * `Race` +`Age` +`APOE-e4 status` +`Education` +`Cognitive diagnosis` +`Sex`,data =Single_visit)
summary(cx1)
cx1  <- coxph(surv_object_single ~ `Psych x Race` +`Age` +`APOE-e4 status` +`Education` +`Cognitive diagnosis` +`Sex`,data =Single_visit)
ggforest(fit.coxph, data = Single_visit, fontsize = 1) 