library(plyr)
library(dplyr)
library(tidyverse) 

# set environment
rm(list = ls())
setwd("~/Documents/R_scripts/NACC/Psychosis/")

# load data: MBI rolling, - No NPS at all visits prior to dementia dx
pers <- read.csv("clean_pers_grp_rolling_LG.csv")
neg <- read.csv("clean_neg_grp_LG_noNPS_tilDemDX.csv")
pers$Psych_grp <- "Psychosis+"
neg$Psych_grp <- "Psychosis-"
data_v1 <- full_join(pers,neg)

# arrange  data 
data_v1 <- data_v1 %>% arrange(naccid_number, visit_time)

# removing missing demographic data
missingDemog_list <- data_v1 %>% dplyr::select(NACCID,EDUC,NACCNE4S) %>%
  filter(EDUC == 99 | NACCNE4S == 9 ) %>% pull(NACCID)
data_v1 <- data_v1 %>% filter(!(NACCID %in% missingDemog_list)) 

# discard unwanted columns
data_v1 <- subset(data_v1, select = c(NACCID,VISITYR,VISITMO,VISITDAY,SEX,EDUC,NACCNIHR,NACCNINR,NACCNE4S,NACCAGE,newBL_Age,
                                      NACCUDSD,NACCAPSY,NACCBVFT,NACCLBDS,NACCALZD,NACCALZP,VASC,
                                      MBI_moti,MBI_affect,MBI_impulse, MBI_social,MBI_psychosis,MBI_total,
                                      naccid_number,visit_time,Psych_grp))
data_v1 <- data_v1 %>% arrange(naccid_number, visit_time)

# shaping baseline dataframe with updated col names for baseline visit
data_BL <- subset(data_v1, data_v1$visit_time == 0)
data_BL$dateofvisitBL <- as.Date(ISOdate(data_BL$VISITYR, data_BL$VISITMO, data_BL$VISITDAY))
data_BL$dxstatusBL <- data_BL$NACCUDSD
data_BL$AntiPsychBL <- data_BL$NACCAPSY
data_BL$BVFTBL <- data_BL$NACCBVFT
data_BL$LBDBL <- data_BL$NACCLBDS
data_BL$ALZBL <- data_BL$NACCALZD
data_BL$ALZPBL <- data_BL$NACCALZP
data_BL$VASCBL <- data_BL$VASC   
data_BL$MBImotiBL <- data_BL$MBI_moti 
data_BL$MBIaffctBL <- data_BL$MBI_affect
data_BL$MBIimpulsBL <- data_BL$MBI_impulse
data_BL$MBIpsychosBL <- data_BL$MBI_psychosis
data_BL$MBIsocBL <- data_BL$MBI_social
data_BL$MBItotBL <- data_BL$MBI_total
data_BL$Psych_grpBL <- data_BL$Psych_grp

data_BL_v1 <- dplyr::select(data_BL,NACCID,SEX,EDUC,NACCNIHR,NACCNINR,NACCNE4S,newBL_Age,dateofvisitBL,
                               dxstatusBL,AntiPsychBL,BVFTBL,LBDBL,ALZBL,ALZPBL,VASCBL,
                               MBImotiBL,MBIaffctBL,MBIimpulsBL,MBIpsychosBL,MBIsocBL,MBItotBL,Psych_grpBL,naccid_number)

# shaping followup visits dataframe with updated col names
data_FU <- subset(data_v1, data_v1$visit_time > 0)
data_FU$dateofvisit <- as.Date(ISOdate(data_FU$VISITYR, data_FU$VISITMO, data_FU$VISITDAY))
data_FU$followupdov <- data_FU$visit_time
data_FU$dxstatusdov <- data_FU$NACCUDSD
data_FU$AntiPsychdov <- data_FU$NACCAPSY
data_FU$BVFTdov <- data_FU$NACCBVFT
data_FU$LBDdov <- data_FU$NACCLBDS
data_FU$ALZdov <- data_FU$NACCALZD
data_FU$ALZPdov <- data_FU$NACCALZP
data_FU$VASCdov <- data_FU$VASC
data_FU$MBImotidov <- data_FU$MBI_moti 
data_FU$MBIaffctdov <- data_FU$MBI_affect
data_FU$MBIimpulsdov <- data_FU$MBI_impulse
data_FU$MBIpsychosdov <- data_FU$MBI_psychosis
data_FU$MBIsocdov <- data_FU$MBI_social
data_FU$MBItotdov <- data_FU$MBI_total

data_FU_final <- dplyr::select(data_FU,NACCID,SEX,EDUC,NACCNIHR,NACCNINR,NACCNE4S,NACCAGE,dateofvisit,followupdov,
                               dxstatusdov,AntiPsychdov,BVFTdov,LBDdov,ALZdov,ALZPdov,VASCdov,
                               MBImotidov,MBIaffctdov,MBIimpulsdov,MBIpsychosdov,MBIsocdov,MBItotdov,naccid_number)

# merge the two dataframes for baseline and followup visits
data_final <- merge(data_BL_v1,data_FU_final, by = c("NACCID","naccid_number","SEX","EDUC","NACCNIHR","NACCNINR","NACCNE4S"))

# back to BL dataframe, trying to add dov visits for BL (for a merge later on) 
data_BL$dateofvisit <- as.Date(ISOdate(data_BL$VISITYR, data_BL$VISITMO, data_BL$VISITDAY))
data_BL$NACCAGE <- data_BL$newBL_Age
data_BL$followupdov <- data_BL$visit_time
data_BL$dxstatusdov <- data_BL$NACCUDSD
data_BL$AntiPsychdov <- data_BL$NACCAPSY
data_BL$BVFTdov <- data_BL$NACCBVFT
data_BL$LBDdov <- data_BL$NACCLBDS
data_BL$ALZdov <- data_BL$NACCALZD
data_BL$ALZPdov <- data_BL$NACCALZP
data_BL$VASCdov <- data_BL$VASC
data_BL$MBImotidov <- data_BL$MBI_moti
data_BL$MBIaffctdov <- data_BL$MBI_affect
data_BL$MBIimpulsdov <- data_BL$MBI_impulse
data_BL$MBIpsychosdov <- data_BL$MBI_psychosis
data_BL$MBIsocdov <- data_BL$MBI_social
data_BL$MBItotdov <- data_BL$MBI_total

data_BL_final <- dplyr::select(data_BL, NACCID,naccid_number,SEX,EDUC,NACCNIHR,NACCNINR,NACCNE4S,newBL_Age,dateofvisitBL,
                               dxstatusBL,AntiPsychBL,BVFTBL,LBDBL,ALZBL,ALZPBL,VASCBL,
                               MBImotiBL,MBIaffctBL,MBIimpulsBL,MBIpsychosBL,MBIsocBL,MBItotBL,Psych_grpBL,
                               NACCAGE,dateofvisit,followupdov,
                               dxstatusdov,AntiPsychdov,BVFTdov,LBDdov,ALZdov,ALZPdov,VASCdov,
                               MBImotidov,MBIaffctdov,MBIimpulsdov,MBIpsychosdov,MBIsocdov,MBItotdov)
# bind and arrange
data_finalrw <- bind_rows(data_BL_final, data_final)
data_finalrw <- data_finalrw %>% arrange(naccid_number, followupdov)
# save file
write.csv(data_finalrw, "clean_nacc_reformated4cox_noNPS_tilDemDX2.csv", row.names = FALSE)