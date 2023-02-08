library(plyr)
library(tidyverse) 

# set environment
rm(list = ls())
setwd("~/Documents/R_scripts/NACC/Psychosis/")

NACC <- read.csv("preclean_nacc_basic_2021.csv")
NACC <- subset(NACC, select = -c(NPIQINF,NPIQINFX,NACCVNUM,NACCDAYS))
NACC <- NACC %>% arrange(naccid_number, NACCFDYS)

# assign psychosis status based on two visits, if symptomatic at 2 consecutive visits, its persistent
NACC <- NACC %>% group_by(naccid_number) %>% mutate(MBI_Status = case_when((MBI_psychosis > 0 & lag(MBI_psychosis) > 0) ~ "MBI_pers", 
                                                                           (MBI_psychosis == 0 &  lag(MBI_psychosis) > 0)~ "MBI_trans",
                                                                           (MBI_psychosis > 0 &  lag(MBI_psychosis) == 0)~ "MBI_emrg",
                                                                           (MBI_psychosis == 0 &  lag(MBI_psychosis) == 0)~ "MBI-")) %>% ungroup()

#...........................(1) MBI PSYCHOSIS GROUP - ROLLING BASELINE......................................
NACCp <- NACC %>% mutate(pers_MBI = case_when(as.character(MBI_Status) == "MBI_pers"  ~ "1", 
                                               as.character(MBI_Status) == "MBI_trans" ~ "0",
                                               as.character(MBI_Status) == "MBI_emrg" ~ "0", 
                                               as.character(MBI_Status) == "MBI-" ~ "0"))
NACCp = NACCp %>% filter(pers_MBI == "0" | pers_MBI == "1")
# remove rows until first instance of pers MBI
Pers_grp <- NACCp %>% group_by(naccid_number) %>% filter(cumsum(pers_MBI == "1") > 0) %>% ungroup()

# filter out those with dementia at baseline
Pers_grp <- Pers_grp %>% group_by(naccid_number) %>% 
  mutate(noDem_BL = ifelse(first(NACCUDSD <= 3), 1, 0)) %>% filter(noDem_BL==1) %>% ungroup()

# adjust baseline visit time back to 0, and baseline age to age at new baseline 
Pers_grp <- Pers_grp %>% group_by(naccid_number) %>% 
  mutate(visit_time = NACCFDYS - first(NACCFDYS), newBL_Age = first(NACCAGE)) %>% ungroup()

# remove IDs with missing followup visits for LG analysis
no_followup_IDs <- Pers_grp %>% group_by(NACCID) %>%
  mutate(n_vis = n()) %>% filter(n_vis == 1) %>% pull(NACCID)
Pers_grp <- Pers_grp %>% filter(!(NACCID %in% no_followup_IDs)) 

# final file for MBI-psychosis
Pers_grp_final_LG <- subset(Pers_grp, select = -c(PACKET,pers_MBI,noDem_BL))
write.csv(Pers_grp_final_LG, "clean_pers_grp_rolling_LG.csv", row.names = FALSE)

#...............................(2) No NPS GROUP WITH NO NPS TIL DEMENTIA DX.....................................
# filter out visits after the first instance of dementia dx
NACCn<- NACC %>% group_by(naccid_number) %>% 
  mutate(has_dementia = ifelse(NACCUDSD > 3, 1, 0),
         has_dementia_cum = cumsum(has_dementia),
         MBI_total_cum = cumsum(MBI_total)) %>%
  filter(has_dementia_cum <=1) %>% ungroup()

# check for NPS til dementia diagnosis
NACCn <- NACCn %>% group_by(naccid_number) %>% mutate(neg_MBI = case_when((has_dementia_cum<1 & MBI_total_cum == 0)  ~ "1", 
                                                                          (has_dementia_cum<1 & MBI_total_cum>0)  ~ "0",
                                                                          (has_dementia_cum>=1 & lag(MBI_total_cum==0)) ~ "1",
                                                                          (has_dementia_cum>=1 & lag(MBI_total_cum>=1)) ~ "0")) %>% ungroup()
neg_grp = NACCn %>% filter(is.na(MBI_Status) == FALSE)
# take only those with no NPS at all visits til dementia dx
neg_grp = neg_grp %>% group_by(naccid_number) %>% 
  mutate(neg_MBI_final = ifelse(all(neg_MBI==1)==TRUE, "1", "0")) %>% filter(neg_MBI_final == "1") %>% ungroup()

# filter out those with dementia at baseline
neg_grp <- neg_grp %>% group_by(naccid_number) %>% 
  mutate(noDem_BL = ifelse(first(NACCUDSD <= 3), 1, 0)) %>% filter(noDem_BL== 1) %>% ungroup()

# adjust baseline visit time back to 0, and baseline age to age at new baseline 
neg_grp <- neg_grp %>% group_by(naccid_number) %>% 
  mutate(visit_time = NACCFDYS - first(NACCFDYS), newBL_Age = first(NACCAGE)) %>% ungroup()

# remove IDs with missing followup visits for LG analysis
no_followup_IDs <- neg_grp %>% group_by(NACCID) %>%
  mutate(n_vis = n()) %>% filter(n_vis == 1) %>% pull(NACCID)
neg_grp <- neg_grp %>% filter(!(NACCID %in% no_followup_IDs)) 

# final file for No NPS
Neg_grp_final_LG_noNPS <- subset(neg_grp, select = -c(PACKET,neg_MBI,noDem_BL))
write.csv(Neg_grp_final_LG_noNPS, "clean_neg_grp_LG_noNPS_tilDemDX.csv", row.names = FALSE)
