#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# 01_APSFMR_obs_CF.R
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

rm(list = ls())

library(tidyverse) 
library(haven)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Set Parameters ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
FN_TidyData_HM <- "00_TidyData_HM/TidyData_HM.csv"
OutDir         <- "02_Output/"

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
TidyData_HM_raw <- read_csv(FN_TidyData_HM)

DF_YearAgeSib_obs <-
  TidyData_HM_raw %>% 
  mutate(SibStrF = fct_relevel(SibStrF,
                               "Only child",
                               "Eldest daughter w/o brothers",
                               "Younger daughter w/o brothers",
                               "Daughter w/ brothers"),
 
         SibStrM = fct_relevel(SibStrM,
                               "Only child",
                               "Eldest son w/o brothers",
                               "Eldest son w/ brothers",
                               "Younger son")) %>% 
    mutate(AR_F = Pop_atRiskM / (Pop_atRiskM + Pop_atRiskF),
      　   AR_M = Pop_atRiskF / (Pop_atRiskM + Pop_atRiskF),
           AR_T = (Pop_atRiskM * Pop_atRiskF) / (Pop_atRiskM + Pop_atRiskF),
           FA   = NofMar / AR_T)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Observed first marriage hazards/rates----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## age-pairing-specific first marriage hazard (APSFMH) ----  
APSFMH_obs <- 
  DF_YearAgeSib_obs %>% 
  mutate(APSFMH_F_obs = NofMar / Pop_atRiskF,
         APSFMH_M_obs = NofMar / Pop_atRiskM) %>% 
  select(Year10y, CAgeF5y, CAgeM5y, SibStrF, SibStrM, APSFMH_F_obs, APSFMH_M_obs)

## pairing-specific total first marriage hazards (TFMHs) ----
PSTFMH_obs_temp <- 
  APSFMH_obs %>% 
  group_by(Year10y, SibStrF, SibStrM) %>% 
  mutate(PSTFMH_F_obs = sum(5 * APSFMH_F_obs, na.rm = TRUE),
         PSTFMH_M_obs = sum(5 * APSFMH_M_obs, na.rm = TRUE)) %>%
  # proportions of each APSFMH to PSTFMH
  mutate(PropAPSFMH_F_obs = (APSFMH_F_obs / PSTFMH_F_obs) * 5,
         PropAPSFMH_M_obs = (APSFMH_M_obs / PSTFMH_M_obs) * 5) %>% 
  ungroup()

PropAPSFMH_obs <- 
  PSTFMH_obs_temp %>% 
  select(Year10y, CAgeF5y, CAgeM5y, SibStrF, SibStrM, PropAPSFMH_F_obs, PropAPSFMH_M_obs)

PSTFMH_obs <- 
  PSTFMH_obs_temp %>% 
  group_by(Year10y, SibStrF, SibStrM) %>% 
  summarize(PSTFMH_F_obs = mean(PSTFMH_F_obs),
            PSTFMH_M_obs = mean(PSTFMH_M_obs)) %>% 
  ungroup()

## total first marriage hazards/rates (TFMH/TFMR) by sibship composition (observed) ----
### female ----
TFMH_TFMR_F_obs <-
  PSTFMH_obs %>%
  group_by(Year10y, SibStrF) %>% 
  summarize(TFMH_F_obs = sum(PSTFMH_F_obs)) %>% 
  ungroup() %>% 
  mutate(TFMR_F_obs = 1-(1/exp(TFMH_F_obs)))

### male ----
TFMH_TFMR_M_obs <-
  PSTFMH_obs %>%
  group_by(Year10y, SibStrM) %>% 
  summarize(TFMH_M_obs = sum(PSTFMH_M_obs)) %>% 
  ungroup() %>% 
  mutate(TFMR_M_obs = 1-(1/exp(TFMH_M_obs)))

## pairing-specific TFMR ----
# (with the assumption that the proportion of PSTFMH to TFMH is equal to the proportion of PSTFMR to TFMR)
PSTFMR_obs <- 
  PSTFMH_obs %>% 
  right_join(TFMH_TFMR_F_obs, by = c("Year10y", "SibStrF")) %>% 
  right_join(TFMH_TFMR_M_obs, by = c("Year10y", "SibStrM")) %>% 
  # Proportions of each PSTFMH to TFMH
  mutate(PropPSTFMH_F_obs = PSTFMH_F_obs / TFMH_F_obs, 
         PropPSTFMH_M_obs = PSTFMH_M_obs / TFMH_M_obs) %>% 
  mutate(PSTFMR_F_obs = TFMR_F_obs * PropPSTFMH_F_obs,
         PSTFMR_M_obs = TFMR_M_obs * PropPSTFMH_M_obs) %>% 
  select(Year10y, SibStrF, SibStrM, PSTFMR_F_obs, PSTFMR_M_obs)

## age-pairing-specific marriage rate ----
# (with the assumption that the proportion of APSFMH to PSTFMH is equal to the proportion of APSFMR to PSTFMR)
APSFMR_obs <- 
  PropAPSFMH_obs %>% 
  right_join(PSTFMR_obs, by = c("Year10y", "SibStrF", "SibStrM")) %>% 
  mutate(APSFMR_F_obs = (PSTFMR_F_obs * PropAPSFMH_F_obs) / 5,
         APSFMR_M_obs = (PSTFMR_M_obs * PropAPSFMH_M_obs) / 5) %>% 
  select(Year10y, CAgeF5y, CAgeM5y, SibStrF, SibStrM, APSFMR_F_obs, APSFMR_M_obs)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Counter-factual first marriage hazards/rates ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
DF_YearAgeSib_CF <- 
  DF_YearAgeSib_obs %>% 
  mutate(MidYear10y = case_when(Year10y == "1980-1989" ~ 1985,
                                Year10y == "1990-1999" ~ 1995,
                                Year10y == "2000-2009" ~ 2005)) %>% 
  select(MidYear10y, CAgeF5y, CAgeM5y, SibStrF, SibStrM, AR_F, AR_M, FA) %>% 
  pivot_wider(id_cols = c(CAgeF5y, CAgeM5y, SibStrF, SibStrM),
              names_from = MidYear10y,
              values_from =  c(AR_F, AR_M, FA)) 

## age-pairing-specific first marriage hazard ----
APSFMH_CF <- 
  DF_YearAgeSib_CF %>% 
  # FA fixed
  mutate(APSFMH_CFconsFA_F1995 = FA_1985 * AR_F_1995,
	       APSFMH_CFconsFA_M1995 = FA_1985 * AR_M_1995,
         APSFMH_CFconsFA_F2005 = FA_1985 * AR_F_2005,
	       APSFMH_CFconsFA_M2005 = FA_1985 * AR_M_2005) %>% 
  # AR fixed
  mutate(APSFMH_CFconsAR_F1995 = FA_1995 * AR_F_1985,
	       APSFMH_CFconsAR_M1995 = FA_1995 * AR_M_1985,
	       APSFMH_CFconsAR_F2005 = FA_2005 * AR_F_1985,
	       APSFMH_CFconsAR_M2005 = FA_2005 * AR_M_1985)

## pairing-specific TFMH ----
PSTFMH_CF_temp <- 
  APSFMH_CF %>% 
  group_by(SibStrF, SibStrM) %>% 
  # FA fixed
  mutate(PSTFMH_CFconsFA_F1995 = sum(5 * APSFMH_CFconsFA_F1995, na.rm = TRUE),
         PSTFMH_CFconsFA_M1995 = sum(5 * APSFMH_CFconsFA_M1995, na.rm = TRUE),
         PSTFMH_CFconsFA_F2005 = sum(5 * APSFMH_CFconsFA_F2005, na.rm = TRUE),
         PSTFMH_CFconsFA_M2005 = sum(5 * APSFMH_CFconsFA_M2005, na.rm = TRUE)) %>% 
  # AR fixed
  mutate(PSTFMH_CFconsAR_F1995 = sum(5 * APSFMH_CFconsAR_F1995, na.rm = TRUE),
         PSTFMH_CFconsAR_M1995 = sum(5 * APSFMH_CFconsAR_M1995, na.rm = TRUE),
         PSTFMH_CFconsAR_F2005 = sum(5 * APSFMH_CFconsAR_F2005, na.rm = TRUE),
         PSTFMH_CFconsAR_M2005 = sum(5 * APSFMH_CFconsAR_M2005, na.rm = TRUE)) %>% 
  ungroup()

PSTFMH_CF <- 
  PSTFMH_CF_temp %>% 
  select(SibStrF, SibStrM, starts_with("PSTFMH")) %>% 
  group_by(SibStrF, SibStrM) %>% 
  summarize_all(mean) %>% 
  ungroup()

# proportions of each APSFMH to PSTFMH
PropAPSFMH_CF <- 
  PSTFMH_CF_temp %>% 
  # FA fixed
  mutate(PropAPSFMH_CFconsFA_F1995 = (APSFMH_CFconsFA_F1995 / PSTFMH_CFconsFA_F1995) * 5,
         PropAPSFMH_CFconsFA_F2005 = (APSFMH_CFconsFA_F2005 / PSTFMH_CFconsFA_F2005) * 5,
         PropAPSFMH_CFconsFA_M1995 = (APSFMH_CFconsFA_M1995 / PSTFMH_CFconsFA_M1995) * 5,
         PropAPSFMH_CFconsFA_M2005 = (APSFMH_CFconsFA_M2005 / PSTFMH_CFconsFA_M2005) * 5) %>%
  # AR fixed
  mutate(PropAPSFMH_CFconsAR_F1995 = (APSFMH_CFconsAR_F1995 / PSTFMH_CFconsAR_F1995) * 5,
         PropAPSFMH_CFconsAR_F2005 = (APSFMH_CFconsAR_F2005 / PSTFMH_CFconsAR_F2005) * 5, 
         PropAPSFMH_CFconsAR_M1995 = (APSFMH_CFconsAR_M1995 / PSTFMH_CFconsAR_M1995) * 5,
         PropAPSFMH_CFconsAR_M2005 = (APSFMH_CFconsAR_M2005 / PSTFMH_CFconsAR_M2005) * 5) %>% 
  select(CAgeF5y, CAgeM5y, SibStrF, SibStrM, starts_with("PropAPSFMH"))


## TFMH and TFMR by sibship composition ----
### female ----
TFMH_TFMR_F_CF <- 
  PSTFMH_CF %>% 
  group_by(SibStrF) %>% 
  summarize(
    # FA fixed
    TFMH_CFconsFA_F1995 = sum(PSTFMH_CFconsFA_F1995),
    TFMH_CFconsFA_F2005 = sum(PSTFMH_CFconsFA_F2005),
  
    # AR fixed
    TFMH_CFconsAR_F1995 = sum(PSTFMH_CFconsAR_F1995),
    TFMH_CFconsAR_F2005 = sum(PSTFMH_CFconsAR_F2005)
  ) %>% 
  ungroup() %>% 
  # Convert TFMH to TFMR by sibship composition
	# FA fixed
	mutate(TFMR_CFconsFA_F1995  = 1-(1/exp(TFMH_CFconsFA_F1995)), 
	       TFMR_CFconsFA_F2005  = 1-(1/exp(TFMH_CFconsFA_F2005))) %>%  
	# AR fixed
	mutate(TFMR_CFconsAR_F1995  = 1-(1/exp(TFMH_CFconsAR_F1995)), 
	       TFMR_CFconsAR_F2005  = 1-(1/exp(TFMH_CFconsAR_F2005)))

### male ----
TFMH_TFMR_M_CF <- 
  PSTFMH_CF %>% 
  group_by(SibStrM) %>% 
  summarize(
    # FA fixed
    TFMH_CFconsFA_M1995 = sum(PSTFMH_CFconsFA_M1995),
    TFMH_CFconsFA_M2005 = sum(PSTFMH_CFconsFA_M2005),
  
    # AR fixed
    TFMH_CFconsAR_M1995 = sum(PSTFMH_CFconsAR_M1995),
    TFMH_CFconsAR_M2005 = sum(PSTFMH_CFconsAR_M2005)
  ) %>% 
  ungroup() %>% 
  # Convert TFMH to TFMR by sibship composition
	# FA fixed
	mutate(TFMR_CFconsFA_M1995  = 1-(1/exp(TFMH_CFconsFA_M1995)), 
	       TFMR_CFconsFA_M2005  = 1-(1/exp(TFMH_CFconsFA_M2005))) %>%  
	# AR fixed
	mutate(TFMR_CFconsAR_M1995  = 1-(1/exp(TFMH_CFconsAR_M1995)), 
	       TFMR_CFconsAR_M2005  = 1-(1/exp(TFMH_CFconsAR_M2005)))

## pairing-specific TFMR ----
# (with the assumption that the proportion of PSTFMH to TFMH is equal to the proportion of PSTFMR to TFMR)
PSTFMR_CF <- 
  PSTFMH_CF %>% 
  right_join(TFMH_TFMR_F_CF, by = "SibStrF") %>% 
  right_join(TFMH_TFMR_M_CF, by = "SibStrM") %>% 
	# FA fixed
  # Proportions of each PSTFMH to TFMH
  mutate(PropPSTFMH_CFconsFA_F1995 = PSTFMH_CFconsFA_F1995 / TFMH_CFconsFA_F1995,
         PropPSTFMH_CFconsFA_F2005 = PSTFMH_CFconsFA_F2005 / TFMH_CFconsFA_F2005,
         PropPSTFMH_CFconsFA_M1995 = PSTFMH_CFconsFA_M1995 / TFMH_CFconsFA_M1995,
         PropPSTFMH_CFconsFA_M2005 = PSTFMH_CFconsFA_M2005 / TFMH_CFconsFA_M2005) %>% 
  # calculate pairing-specific TFMR
  mutate(PSTFMR_CFconsFA_F1995 = TFMR_CFconsFA_F1995 * PropPSTFMH_CFconsFA_F1995,
	       PSTFMR_CFconsFA_M1995 = TFMR_CFconsFA_M1995 * PropPSTFMH_CFconsFA_M1995,
	       PSTFMR_CFconsFA_F2005 = TFMR_CFconsFA_F2005 * PropPSTFMH_CFconsFA_F2005,
	       PSTFMR_CFconsFA_M2005 = TFMR_CFconsFA_M2005 * PropPSTFMH_CFconsFA_M2005) %>%
  # AR fixed
  # Proportions of each PSTFMH to TFMH 
  mutate(PropPSTFMH_CFconsAR_F1995 = PSTFMH_CFconsAR_F1995 / TFMH_CFconsAR_F1995,
         PropPSTFMH_CFconsAR_F2005 = PSTFMH_CFconsAR_F2005 / TFMH_CFconsAR_F2005,
         PropPSTFMH_CFconsAR_M1995 = PSTFMH_CFconsAR_M1995 / TFMH_CFconsAR_M1995,
         PropPSTFMH_CFconsAR_M2005 = PSTFMH_CFconsAR_M2005 / TFMH_CFconsAR_M2005) %>% 
  # calculate pairing-specific TFMR
  mutate(PSTFMR_CFconsAR_F1995 = TFMR_CFconsAR_F1995 * PropPSTFMH_CFconsAR_F1995,
	       PSTFMR_CFconsAR_M1995 = TFMR_CFconsAR_M1995 * PropPSTFMH_CFconsAR_M1995,
	       PSTFMR_CFconsAR_F2005 = TFMR_CFconsAR_F2005 * PropPSTFMH_CFconsAR_F2005,
	       PSTFMR_CFconsAR_M2005 = TFMR_CFconsAR_M2005 * PropPSTFMH_CFconsAR_M2005)

  
## age-pairing-specific marriage rate ----
# (with the assumption that the proportion of APSFMH to PSTFMH is equal to the proportion of APSFMR to PSTFMR)
APSFMR_CF <- 
  PropAPSFMH_CF %>% 
  right_join(PSTFMR_CF, by = c("SibStrF", "SibStrM")) %>% 
  # FA fixed
  mutate(APSFMR_CFconsFA_F1995 = (PSTFMR_CFconsFA_F1995 * PropAPSFMH_CFconsFA_F1995) / 5,
	       APSFMR_CFconsFA_M1995 = (PSTFMR_CFconsFA_M1995 * PropAPSFMH_CFconsFA_M1995) / 5,
	       APSFMR_CFconsFA_F2005 = (PSTFMR_CFconsFA_F2005 * PropAPSFMH_CFconsFA_F2005) / 5,
	       APSFMR_CFconsFA_M2005 = (PSTFMR_CFconsFA_M2005 * PropAPSFMH_CFconsFA_M2005) / 5) %>% 
  # AR fixed
  mutate(APSFMR_CFconsAR_F1995 = (PSTFMR_CFconsAR_F1995 * PropAPSFMH_CFconsAR_F1995) / 5,
	       APSFMR_CFconsAR_M1995 = (PSTFMR_CFconsAR_M1995 * PropAPSFMH_CFconsAR_M1995) / 5,
	       APSFMR_CFconsAR_F2005 = (PSTFMR_CFconsAR_F2005 * PropAPSFMH_CFconsAR_F2005) / 5,
	       APSFMR_CFconsAR_M2005 = (PSTFMR_CFconsAR_M2005 * PropAPSFMH_CFconsAR_M2005) / 5) %>% 
  select(CAgeF5y, CAgeM5y, SibStrF, SibStrM, starts_with("APSFMR")) %>% 
  arrange(SibStrF, SibStrM, CAgeF5y, CAgeM5y)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Save Data ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
saveRDS(APSFMR_obs, file = paste0(OutDir, "APSFMR_obs.rds"))
saveRDS(APSFMR_CF,  file = paste0(OutDir, "APSFMR_CF.rds"))

saveRDS(DF_YearAgeSib_obs, file = paste0(OutDir, "DF_YearAgeSib_obs.rds"))