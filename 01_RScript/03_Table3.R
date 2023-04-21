#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Table 3: Contribution of each pairing to observed and counterfactual differences in TFMR
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

rm(list = ls())

library(tidyverse) 

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Set Parameters ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
FN_DF_YearAgeSib_obs <- "02_Output/DF_YearAgeSib_obs.rds"

FN_APSFMR_obs <- "02_Output/APSFMR_obs.rds"
FN_APSFMR_CF  <- "02_Output/APSFMR_CF.rds"

OutDir <- "02_Output/"

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Set dataframes ----
## actual ----
DF_YearAgeSib_obs_raw <- readRDS(FN_DF_YearAgeSib_obs)
APSFMR_obs            <- readRDS(FN_APSFMR_obs)

DF_Obs <- 
  DF_YearAgeSib_obs_raw %>% 
  right_join(APSFMR_obs, by = c("Year10y", "CAgeF5y", "CAgeM5y", "SibStrF", "SibStrM")) %>% 
  mutate(Year10y = case_when(Year10y == "1980-1989" ~ "1980s",
                             Year10y == "1990-1999" ~ "1990s",
                             Year10y == "2000-2009" ~ "2000s"),
         
         CAgeF5y = as_factor(CAgeF5y),
         CAgeM5y = as_factor(CAgeM5y)) %>% 
  select(Year10y, CAgeF5y, CAgeM5y, SibStrF, SibStrM, Pop_atRiskF, Pop_atRiskM, APSFMR_F_obs, APSFMR_M_obs)

## counter factuals ----
DF_CF <- readRDS(FN_APSFMR_CF)

# DF_CF <- 
#   DF_CF_raw %>% 
#   mutate(CAgeF5y = as_factor(CAgeF5y),
#          CAgeM5y = as_factor(CAgeM5y),
#          
#          SibStrF = case_when(SibStrF == 1 ~ "Only child",
#                              SibStrF == 2 ~ "Eldest w/o brothers",
#                              SibStrF == 3 ~ "Not eldest w/o brothers",
#                              SibStrF == 4 ~ "Daughters w/ brothers"),
#          
#          SibStrF = factor(SibStrF,levels = c("Only child",
#                                              "Eldest w/o brothers",
#                                              "Not eldest w/o brothers",
#                                              "Daughters w/ brothers")),
#          
#          SibStrM = as.factor(case_when(SibStrM == 1 ~ "Only child",
#                                        SibStrM == 2 ~ "Eldest w/o brothers",
#                                        SibStrM == 3 ~ "Eldest w/ brothers",
#                                        SibStrM == 4 ~ "Not eldest")),
# 
#          SibStrM = factor(SibStrM, levels = c("Only child",
#                                               "Eldest w/o brothers",
#                                               "Eldest w/ brothers",
#                                               "Not eldest")))

# sibship distributions ----
PropSibStrM <- 
  DF_Obs %>% 
  select(Year10y, CAgeM5y, SibStrM, SibStrF, Pop_atRiskM) %>% 
  group_by(Year10y, CAgeM5y, SibStrM, SibStrF) %>%   
  summarize(Pop_atRiskM = mean(Pop_atRiskM)) %>% 
  group_by(Year10y, CAgeM5y, SibStrF) %>% 
  mutate(PropSibStrM = Pop_atRiskM / sum(Pop_atRiskM))

PropSibStrM1985 <- 
  PropSibStrM %>% 
  filter(Year10y == "1980s") %>% 
  ungroup() %>% 
  select(CAgeM5y, SibStrM, SibStrF, PropSibStrM)

PropSibStrF <- 
  DF_Obs %>% 
  select(Year10y, CAgeF5y, SibStrF, SibStrM, Pop_atRiskF) %>% 
  group_by(Year10y, CAgeF5y, SibStrF, SibStrM) %>%   
  summarize(Pop_atRiskF = mean(Pop_atRiskF)) %>% 
  group_by(Year10y, CAgeF5y, SibStrM) %>% 
  mutate(PropSibStrF = Pop_atRiskF / sum(Pop_atRiskF))

PropSibStrF1985 <- 
  PropSibStrF %>% 
  filter(Year10y == "1980s") %>% 
  ungroup() %>% 
  select(CAgeF5y, SibStrM, SibStrF, PropSibStrF)

# counterfactual age-pairing-specific first marriage rates (APSFMRs)----
## AR constant ----
DF_CFconsAR_Men <- 
  DF_CF %>% 
  select(CAgeM5y, SibStrM, SibStrF, APSFMR_CFconsAR_M1995, APSFMR_CFconsAR_M2005) %>% 
  pivot_longer(APSFMR_CFconsAR_M1995:APSFMR_CFconsAR_M2005, 
               names_to     = "Year10y", 
               names_prefix = "APSFMR_CFconsAR_M",
               values_to    = "APSFMR_CFconsAR_M") %>% 
  mutate(Year10y = as.factor(if_else(Year10y == "1995", "1990s", "2000s")))

DF_CFconsAR_Women <- 
  DF_CF %>% 
  select(CAgeF5y, SibStrM, SibStrF, APSFMR_CFconsAR_F1995, APSFMR_CFconsAR_F2005) %>% 
  pivot_longer(APSFMR_CFconsAR_F1995:APSFMR_CFconsAR_F2005, 
               names_to     = "Year10y", 
               names_prefix = "APSFMR_CFconsAR_F",
               values_to    = "APSFMR_CFconsAR_F") %>% 
  mutate(Year10y = as.factor(if_else(Year10y == "1995", "1990s", "2000s")))

## FA constant ----
DF_CFconsFA_Men <-
  DF_CF %>% 
  select(CAgeM5y, SibStrM, SibStrF, APSFMR_CFconsFA_M1995, APSFMR_CFconsFA_M2005) %>% 
  pivot_longer(APSFMR_CFconsFA_M1995:APSFMR_CFconsFA_M2005, 
               names_to     = "Year10y", 
               names_prefix = "APSFMR_CFconsFA_M",
               values_to    = "APSFMR_CFconsFA_M") %>% 
  mutate(Year10y = as.factor(if_else(Year10y == "1995", "1990s", "2000s")))
  
DF_CFconsFA_Women <- 
  DF_CF %>% 
  select(CAgeF5y, SibStrM, SibStrF, APSFMR_CFconsFA_F1995, APSFMR_CFconsFA_F2005) %>% 
  pivot_longer(APSFMR_CFconsFA_F1995:APSFMR_CFconsFA_F2005, 
               names_to     = "Year10y", 
               names_prefix = "APSFMR_CFconsFA_F",
               values_to    = "APSFMR_CFconsFA_F") %>% 
  mutate(Year10y = as.factor(if_else(Year10y == "1995", "1990s", "2000s")))  

# pairing-specific TFMRs ----
## Observed ----
### sum APSFMR across the opposite sex's ages ----
APSFMR_M_obs <- 
  DF_Obs %>% 
  group_by(Year10y, SibStrM, SibStrF, CAgeM5y) %>% 
  summarize(APSFMR = sum(APSFMR_M_obs, na.rm = TRUE)) %>%
  ungroup()

APSFMR_F_obs <- 
  DF_Obs %>% 
  group_by(Year10y, SibStrM, SibStrF, CAgeF5y) %>% 
  summarize(APSFMR = sum(APSFMR_F_obs, na.rm = TRUE)) %>% 
  ungroup()

### APSFMRs weighted with sibship proportions ----
WtAPSFMR_M_obs <-
  APSFMR_M_obs %>%
  inner_join(PropSibStrM, by = c("Year10y", "CAgeM5y", "SibStrM", "SibStrF")) %>%
  mutate(WtAPSFMR = APSFMR * PropSibStrM) %>%
  group_by(Year10y) %>%
  mutate(CumWtAPSFMR = WtAPSFMR * 5) %>% 
  ungroup() %>% 
  mutate(Cont_Obs = CumWtAPSFMR) %>% 
  select(Year10y, SibStrM, SibStrF, CAgeM5y, Cont_Obs) 

WtAPSFMR_F_obs <-
  APSFMR_F_obs %>%
  inner_join(PropSibStrF, by = c("Year10y", "CAgeF5y", "SibStrM", "SibStrF")) %>%
  mutate(WtAPSFMR = APSFMR * PropSibStrF) %>%
  group_by(Year10y) %>%
  mutate(CumWtAPSFMR = WtAPSFMR * 5) %>% 
  ungroup() %>% 
  mutate(Cont_Obs = CumWtAPSFMR) %>% 
  select(Year10y, SibStrM, SibStrF, CAgeF5y, Cont_Obs) 

## Counter-factual: FA constant ----
### sum APSFMR across the opposite sex's ages ----
APSFMR_CFconsFA_Men <- 
  DF_CFconsFA_Men %>% 
  group_by(Year10y, SibStrM, SibStrF, CAgeM5y) %>% 
  summarize(APSFMR_consFA = sum(APSFMR_CFconsFA_M, na.rm = TRUE)) 

APSFMR_CFconsFA_Women <- 
  DF_CFconsFA_Women %>% 
  group_by(Year10y, SibStrM, SibStrF, CAgeF5y) %>% 
  summarize(APSFMR_consFA = sum(APSFMR_CFconsFA_F, na.rm = TRUE)) 

### APSFMRs weighted with sibship proportions ----
WtAPSFMR_M_CFconsFA <- 
  APSFMR_CFconsFA_Men %>% 
  left_join(PropSibStrM, by = c("Year10y", "CAgeM5y", "SibStrM", "SibStrF")) %>% 
  mutate(WtAPSFMR_consFA    = APSFMR_consFA * PropSibStrM) %>% 
  mutate(CumWtAPSFMR_consFA = WtAPSFMR_consFA * 5) %>% 
  ungroup() %>% 
  mutate(Cont_FA = CumWtAPSFMR_consFA) %>% 
  select(Year10y,SibStrM,SibStrF,CAgeM5y,Cont_FA)

WtAPSFMR_F_CFconsFA <- 
  APSFMR_CFconsFA_Women %>% 
  left_join(PropSibStrF, by = c("Year10y", "CAgeF5y", "SibStrM", "SibStrF")) %>% 
  mutate(WtAPSFMR_consFA    = APSFMR_consFA * PropSibStrF) %>% 
  mutate(CumWtAPSFMR_consFA = WtAPSFMR_consFA * 5) %>% 
  ungroup() %>% 
  mutate(Cont_FA = CumWtAPSFMR_consFA) %>% 
  select(Year10y, SibStrM, SibStrF, CAgeF5y, Cont_FA)

## Counter-factual: AR constant ----
### sum APSFMR across the opposite sex's ages ----
APSFMR_CFconsAR_Men <- 
  DF_CFconsAR_Men %>% 
  group_by(Year10y, SibStrM, SibStrF, CAgeM5y) %>% 
  summarize(APSFMR_consAR = sum(APSFMR_CFconsAR_M, na.rm = TRUE)) 

APSFMR_CFconsAR_Women <- 
  DF_CFconsAR_Women %>% 
  group_by(Year10y, SibStrM, SibStrF, CAgeF5y) %>% 
  summarize(APSFMR_consAR = sum(APSFMR_CFconsAR_F, na.rm = TRUE)) 

### APSFMRs weighted with sibship proportions ----
WtAPSFMR_M_CFconsAR <- 
  APSFMR_CFconsAR_Men %>% 
  left_join(PropSibStrM1985, by = c("CAgeM5y", "SibStrM", "SibStrF")) %>% 
  mutate(WtAPSFMR_consAR = APSFMR_consAR * PropSibStrM) %>% 
  group_by(Year10y, SibStrM, SibStrF, CAgeM5y) %>% 
  mutate(CumWtAPSFMR_consAR = WtAPSFMR_consAR * 5) %>% 
  ungroup() %>% 
  mutate(Cont_AR = CumWtAPSFMR_consAR) %>% 
  select(Year10y, SibStrM, SibStrF, CAgeM5y, Cont_AR)

WtAPSFMR_F_CFconsAR <- 
  APSFMR_CFconsAR_Women %>% 
  left_join(PropSibStrF1985, by = c("CAgeF5y", "SibStrM", "SibStrF")) %>% 
  mutate(WtAPSFMR_consAR = APSFMR_consAR * PropSibStrF) %>% 
  group_by(Year10y, SibStrM, SibStrF, CAgeF5y) %>% 
  mutate(CumWtAPSFMR_consAR = WtAPSFMR_consAR * 5) %>% 
  ungroup() %>% 
  mutate(Cont_AR = CumWtAPSFMR_consAR) %>% 
  select(Year10y, SibStrM, SibStrF, CAgeF5y, Cont_AR)

# Absolute contribution of each sibship ----
## by age and pairing ----
WtAPSFMR_M_obs_CF <- 
  WtAPSFMR_M_obs %>% 
  left_join(WtAPSFMR_M_CFconsFA, by = c("Year10y", "SibStrM", "SibStrF", "CAgeM5y")) %>% 
  left_join(WtAPSFMR_M_CFconsAR, by = c("Year10y", "SibStrM", "SibStrF", "CAgeM5y")) %>% 
  ungroup()

WtAPSFMR_F_obs_CF <- 
  WtAPSFMR_F_obs %>% 
  left_join(WtAPSFMR_F_CFconsFA, by = c("Year10y", "SibStrM", "SibStrF", "CAgeF5y")) %>% 
  left_join(WtAPSFMR_F_CFconsAR, by = c("Year10y", "SibStrM", "SibStrF", "CAgeF5y")) %>% 
  ungroup() 

## by pairing ----
WtPSFMR_M_obs_CF <- 
  WtAPSFMR_M_obs_CF %>% 
  group_by(Year10y, SibStrM, SibStrF) %>% 
  summarise(Cont_Obs = sum(Cont_Obs),
            Cont_FA  = sum(Cont_FA),
            Cont_AR  = sum(Cont_AR))

WtPSFMR_F_obs_CF <-  
  WtAPSFMR_F_obs_CF %>% 
  group_by(Year10y, SibStrM, SibStrF) %>% 
  summarise(Cont_Obs = sum(Cont_Obs),
            Cont_FA  = sum(Cont_FA),
            Cont_AR  = sum(Cont_AR))

## by sibship status ----
SSFMR_M_obs_CF <-   # SS: sibship specific
  WtPSFMR_M_obs_CF %>%
  group_by(Year10y, SibStrM) %>% 
  summarize(Cont_Obs = sum(Cont_Obs),
            Cont_FA  = sum(Cont_FA),
            Cont_AR  = sum(Cont_AR)) %>% 
  ungroup()

SSFMR_F_obs_CF <-   # SS: sibship specific
  WtPSFMR_F_obs_CF %>%
  group_by(Year10y, SibStrF) %>% 
  summarize(Cont_Obs = sum(Cont_Obs),
            Cont_FA  = sum(Cont_FA),
            Cont_AR  = sum(Cont_AR)) %>% 
  ungroup()

# TFMR for the whole population ----
TFMR_M_obs_CF <-
  SSFMR_M_obs_CF %>% 
  group_by(Year10y) %>% 
  summarize(TFMR_obs    = sum(Cont_Obs),
            TFMR_consFA = sum(Cont_FA),
            TFMR_consAR = sum(Cont_AR))

TFMR_F_obs_CF <-
  SSFMR_F_obs_CF %>% 
  group_by(Year10y) %>% 
  summarize(TFMR_obs    = sum(Cont_Obs),
            TFMR_consFA = sum(Cont_FA),
            TFMR_consAR = sum(Cont_AR))

# question 1 - what is contribution of each pairing to overall decline in TFMR? ----
WtPSFMR_M_obs_CF %>% 
  select(Year10y, SibStrF, SibStrM, Cont_Obs) %>% 
  filter(Year10y %in% c("1980s", "2000s")) %>% 
  pivot_wider(id_cols = c(SibStrF, SibStrM),
              names_from  = Year10y,
              values_from = Cont_Obs,
              names_prefix = "Cont_Obs_") %>% 
  mutate(DifCont_obs = Cont_Obs_1980s - Cont_Obs_2000s)

WtPSFMR_F_obs_CF %>% 
  select(Year10y, SibStrF, SibStrM, Cont_Obs) %>% 
  filter(Year10y %in% c("1980s", "2000s")) %>% 
  pivot_wider(id_cols = c(SibStrF, SibStrM),
              names_from  = Year10y,
              values_from = Cont_Obs,
              names_prefix = "Cont_Obs_") %>% 
  mutate(DifCont_obs = Cont_Obs_1980s - Cont_Obs_2000s)


# question 2 - what is contribution of each pairing to FOA constant change in TFMR ----
# this can be seen as the contribution of change in AR to decline in marriage
PairingCont_to_TFMR_consFA_M <- 
  WtPSFMR_M_obs_CF %>% 
  select(Year10y, SibStrF, SibStrM, Cont_Obs, Cont_FA) %>% 
  filter(Year10y %in% c("2000s")) %>% 
  pivot_wider(id_cols = c(SibStrF, SibStrM),
              names_from  = Year10y,
              values_from = c(Cont_Obs, Cont_FA)) %>% 
  mutate(DifCont = Cont_FA_2000s - Cont_Obs_2000s) %>% 
  select(SibStrF, SibStrF, DifCont) %>% 
  pivot_wider(id_cols = SibStrF,
              names_from  = SibStrM,
              values_from = DifCont)

PairingCont_to_TFMR_consFA_F <- 
  WtPSFMR_F_obs_CF %>% 
  select(Year10y, SibStrF, SibStrM, Cont_Obs, Cont_FA) %>% 
  filter(Year10y %in% c("2000s")) %>% 
  pivot_wider(id_cols = c(SibStrF, SibStrM),
              names_from  = Year10y,
              values_from = c(Cont_Obs, Cont_FA)) %>% 
  mutate(DifCont = Cont_FA_2000s - Cont_Obs_2000s) %>% 
  select(SibStrF, SibStrF, DifCont) %>% 
  pivot_wider(id_cols = SibStrF,
              names_from  = SibStrM,
              values_from = DifCont)

# question 3 - what is contribution of each group to AR constant change in TFMR ----
# this can be seen as the contribution of change in FOA to decline in marriage
PairingCont_to_TFMR_consAR_M <- 
  WtPSFMR_M_obs_CF %>% 
  select(Year10y, SibStrF, SibStrM, Cont_Obs, Cont_AR) %>% 
  filter(Year10y %in% c("2000s")) %>% 
  pivot_wider(id_cols = c(SibStrF, SibStrM),
              names_from  = Year10y,
              values_from = c(Cont_Obs, Cont_AR)) %>% 
  mutate(DifCont = Cont_AR_2000s - Cont_Obs_2000s) %>% 
  select(SibStrF, SibStrF, DifCont) %>% 
  pivot_wider(id_cols = SibStrF,
              names_from  = SibStrM,
              values_from = DifCont)

PairingCont_to_TFMR_consAR_F <- 
  WtPSFMR_F_obs_CF %>% 
  select(Year10y, SibStrF, SibStrM, Cont_Obs, Cont_AR) %>% 
  filter(Year10y %in% c("2000s")) %>% 
  pivot_wider(id_cols = c(SibStrF, SibStrM),
              names_from  = Year10y,
              values_from = c(Cont_Obs, Cont_AR)) %>% 
  mutate(DifCont = Cont_AR_2000s - Cont_Obs_2000s) %>% 
  select(SibStrF, SibStrF, DifCont) %>% 
  pivot_wider(id_cols = SibStrF,
              names_from  = SibStrM,
              values_from = DifCont)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Save data ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
PairingCont_to_TFMR <- vector("list", 4)
names(PairingCont_to_TFMR) <-  
  c("PairingCont_to_TFMR_consFA_M", "PairingCont_to_TFMR_consFA_F",
    "PairingCont_to_TFMR_consAR_M", "PairingCont_to_TFMR_consAR_F")

PairingCont_to_TFMR$PairingCont_to_TFMR_consFA_M <- PairingCont_to_TFMR_consFA_M
PairingCont_to_TFMR$PairingCont_to_TFMR_consFA_F <- PairingCont_to_TFMR_consFA_F
PairingCont_to_TFMR$PairingCont_to_TFMR_consAR_M <- PairingCont_to_TFMR_consAR_M
PairingCont_to_TFMR$PairingCont_to_TFMR_consAR_F <- PairingCont_to_TFMR_consAR_F

saveRDS(PairingCont_to_TFMR, file = paste0(OutDir, "PairingCont_to_TFMR.rds"))