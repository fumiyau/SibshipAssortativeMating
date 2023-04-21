#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Table 1: Forces of attraction, by sibship combination and decade
# Table 2: Availability Ratios, by Sex, Sibship Pairing, and Decade
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

rm(list = ls())

library(tidyverse) 
library(haven)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Set Parameters ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
FN_DF_YearAgeSib_obs <- "02_Output/DF_YearAgeSib_obs.rds"
OutDir         <- "02_Output/"

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# set dataframes ----
## year, age, and sibship ----
DF_YearAgeSib_obs <- readRDS(FN_DF_YearAgeSib_obs)

## year and sibship (age omitted) ----
DF_YearSib_obs <-   # obs: observed
  DF_YearAgeSib_obs %>% 
  group_by(Year10y, SibStrF, SibStrM) %>% 
  mutate(TotNofMar = sum(NofMar)) %>% 
  summarize(NofMar      = mean(TotNofMar),
            Pop_atRiskF = mean(Pop_atRiskF),
            Pop_atRiskM = mean(Pop_atRiskM)) %>% 
  ungroup() %>% 
  mutate(AR_F = Pop_atRiskM / (Pop_atRiskM + Pop_atRiskF),
         AR_M = Pop_atRiskF / (Pop_atRiskM + Pop_atRiskF),
         AR_T = (Pop_atRiskM * Pop_atRiskF) / (Pop_atRiskM + Pop_atRiskF),
         FA   = NofMar / AR_T)

# figures shown in Table 1 and Table 2 ----
## forces of attraction (FOA) ----
FOA <- DF_YearSib_obs %>% 
       pivot_wider(id_cols = c(Year10y, SibStrF),
                   names_from = SibStrM,
                   values_from = FA,
                   names_prefix = "SibStrM:")

## availability ratio (AR) ----
### women ----
AR_F <-
  DF_YearSib_obs %>% 
  pivot_wider(id_cols = c(Year10y, SibStrF),
              names_from = SibStrM,
              values_from = AR_F,
              names_prefix = "SibStrM:")

### men ----
AR_M <-
  DF_YearSib_obs %>% 
  pivot_wider(id_cols = c(Year10y, SibStrF),
              names_from = SibStrM,
              values_from = AR_M,
              names_prefix = "SibStrM:")

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Save Data ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
saveRDS(FOA,  file = paste0(OutDir, "FOA.rds"))
saveRDS(AR_F, file = paste0(OutDir, "AR_F.rds"))
saveRDS(AR_M, file = paste0(OutDir, "AR_M.rds"))