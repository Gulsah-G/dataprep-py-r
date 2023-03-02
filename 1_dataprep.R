
#-- Setup --#
# Empty environment.
rm(list=ls())

# Libraries.
library(dplyr)

# Folder path.
fpath <- "..."



#-- Load data --#
# Data.
data0 <- haven::read_spss(file.path(fpath,"Raw","public-use/prgushp1_puf (2012-2014).sav"))
data0 %>% head() %>% View()

# Counts.
for (i in c('C_Q07','YEARLYINCPR','AGEG10LFS_T','RACETHN_4CAT','EDCAT6','GENDER_R','J_Q04A','REGION_US')){
  data0 %>% count(.[i]) %>% print()
}



#-- Data Pre-processing --#
df1 <- data0 %>% as.data.frame() %>% 
  
  # Filter age groups
  filter(AGEG10LFS_T %in% c(2,3,4)) %>%   
  
  # Remove if NA
  filter(
    !is.na(YEARLYINCPR),
    !is.na(AGEG10LFS_T),
    !is.na(RACETHN_4CAT),
    !is.na(EDCAT6),
    !is.na(GENDER_R),
    !is.na(J_Q04A),
    !is.na(REGION_US)
  ) %>% 
  
  # Create new variables
  # INCOME
  ##Low and High income
  mutate(LowIncome = ifelse(YEARLYINCPR %in% c(1,2), 1, 0)) %>% 
  mutate(HighIncome = ifelse(YEARLYINCPR %in% c(5,6), 1, 0)) %>% 
  mutate(YEARLYINCPR_R = case_when(YEARLYINCPR %in% c(1,2) ~ "Q1", 
                                   YEARLYINCPR==3 ~ "Q2",
                                   YEARLYINCPR==4 ~ "Q3", 
                                   YEARLYINCPR %in% c(5,6) ~ "Q4")) %>% 
  
  # AGE
  mutate(AGEG10LFS_T = case_when(AGEG10LFS_T==2 ~ "2534",
                                 AGEG10LFS_T==3 ~ "3544",
                                 AGEG10LFS_T==4 ~ "4554")) %>% 
  mutate(AGEG10LFS_T = factor(AGEG10LFS_T, levels = c("2534", "3544", "4554"))) %>% 
  
  # RACETHN
  mutate(RACETHN_4CAT = case_when(RACETHN_4CAT==1 ~ "Hispanic",
                                  RACETHN_4CAT==2 ~ "White",
                                  RACETHN_4CAT==3 ~ "Black",
                                  RACETHN_4CAT==6 ~ "Other")) %>% 
  mutate(RACETHN_4CAT = factor(RACETHN_4CAT, levels = c("White", "Black", "Hispanic", "Other"))) %>% 
  
  # EDCAT
  mutate(EDCAT3 = case_when(EDCAT6 %in% c(1,2) ~ "Secondary",
                            EDCAT6==3 ~ "Non-tertiary",
                            EDCAT6 %in% c(4,5,6) ~ "Tertiary")) %>%
  mutate(EDCAT3 = factor(EDCAT3, levels = c("Secondary","Non-tertiary","Tertiary"))) %>% 
  
  ##Gender and FEMALE
  mutate(FEMALE = case_when(GENDER_R==2 ~ 1, 
                            GENDER_R==1 ~ 0)) %>% 
  mutate(GENDER_R = case_when(GENDER_R==1 ~ "Male",
                              GENDER_R==2 ~ "Female")) %>% 
  mutate(GENDER_R = factor(GENDER_R, levels = c("Male","Female"))) %>% 
  
  # born in the country
  mutate(US_born = case_when(J_Q04A==1 ~ 1, 
                             J_Q04A==2 ~ 0)) %>% 
  
  # region US
  mutate(REGION_US = case_when(REGION_US==1 ~ "Northeast", 
                               REGION_US==2 ~ "Midwest",
                               REGION_US==3 ~ "South", 
                               REGION_US==4 ~ "West")) %>% 
  mutate(REGION_US = factor(REGION_US, levels = c("Northeast","Midwest","South","West"))) %>% 
  
  # composites for cognitive skills
  mutate(STLITNUM1 = as.numeric(scale(PVLIT1 + PVNUM1)),
         STLITNUM2 = as.numeric(scale(PVLIT2 + PVNUM2)),
         STLITNUM3 = as.numeric(scale(PVLIT3 + PVNUM3)),
         STLITNUM4 = as.numeric(scale(PVLIT4 + PVNUM4)),
         STLITNUM5 = as.numeric(scale(PVLIT5 + PVNUM5)),
         STLITNUM6 = as.numeric(scale(PVLIT6 + PVNUM6)),
         STLITNUM7 = as.numeric(scale(PVLIT7 + PVNUM7)),
         STLITNUM8 = as.numeric(scale(PVLIT8 + PVNUM8)),
         STLITNUM9 = as.numeric(scale(PVLIT9 + PVNUM9)),
         STLITNUM10 = as.numeric(scale(PVLIT10 + PVNUM10))) %>% 

  # remove outliers
  group_by(GENDER_R,RACETHN_4CAT,EDCAT3) %>%
  mutate(q1 = quantile(EARNMTHBONUSUS_C, probs = 0.25, na.rm = T),
         q3 = quantile(EARNMTHBONUSUS_C, probs = 0.75, na.rm = T),
         iqr = q3 - q1,
         lower = q1 - 4 * iqr,
         upper = q3 + 4 * iqr,
         EARNMTHBONUSUS_C_outlier = case_when(
           EARNMTHBONUSUS_C > upper ~ "high",
           EARNMTHBONUSUS_C < lower ~ "low",
           is.na(EARNMTHBONUSUS_C) ~ "missing",
           TRUE ~ "normal")
  ) %>%
  ungroup() %>%
  filter(EARNMTHBONUSUS_C_outlier != "high") %>%
  select(-q1, -q3, -iqr, -lower, -upper, -EARNMTHBONUSUS_C_outlier) %>%
  
  # Merge groups: Other-Non-tertiary cell is relatively small; recode them as Other-Tertiary
  mutate(EDCAT3 = ifelse(RACETHN_4CAT=="Other" & EDCAT3=="Non-tertiary", "Tertiary", as.character(EDCAT3))) %>% 
  
  # replace NA EARNMTHBONUS and EARNMTHBONUSUS_C with the mean incomes for YEARLYINCPR
  group_by(YEARLYINCPR) %>%
  mutate(EARNMTHBONUSUS_C_mean = mean(EARNMTHBONUSUS_C, na.rm=T)) %>% 
  ungroup() %>%
  mutate(EARNMTHBONUSUS_C = ifelse(is.na(EARNMTHBONUSUS_C)==T, EARNMTHBONUSUS_C_mean, EARNMTHBONUSUS_C)) %>% 
  
  # select variables
  select(LowIncome,HighIncome,FEMALE, 
         C_Q07,YEARLYINCPR,GENDER_R,AGEG10LFS_T,RACETHN_4CAT,EDCAT6,EDCAT3,
         C_Q09_C,ISCOSKIL4,EARNMTHBONUSUS_C,EARNMTHBONUSPPPUS_C,D_Q18A_T,
         J_Q04A,US_born,REGION_US,
         LITSTATUS:PVNUM10,STLITNUM1:STLITNUM10,
         VEMETHOD,VENREPS, SPFWT0:SPFWT80) %>% 
  # rename
  as.data.frame()



#-- Export cleaned data --#
readr::write_csv(df1, file = file.path(fpath, "Cleaned","Data_v1_r.csv"))


