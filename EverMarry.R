#===============================================================================
# 2020/06/25
# Harmonic mean models ever marry
# Sibship assortative mating
# Fumiya Uchikoshi, uchikoshi@princeton.edu
#===============================================================================

library(gdata)
library(tidyverse) 
library(dplyr)
library(ggthemes)
library(viridis)
library(RColorBrewer)
library(ggsci)
library(haven)

######################################################################
# Set directory
######################################################################
setwd("/Users/fumiyau/Dropbox (Princeton)/49.Sibship_Assortative_Mating/9.Uchikoshi/")

######################################################################
# Ever marry
######################################################################

#### #### ####
## Observed ##
#### #### ####

df1 <- read_dta("1_Data/2_HM_Model/Hazard-Model/02_Output/02_HM/Sib4Cat/PSTFMH_PSTFMR_obs.dta") %>%
  dplyr::select(-PSTFMH_F_obs,-PSTFMH_M_obs) %>% #drop marriage hazards
  filter(Year5yRnd == 1 | Year5yRnd == 6) %>%
  pivot_longer(
    cols = ends_with("_obs"),
    names_to = "type",
    values_to = "rate",
    values_drop_na = TRUE
  ) %>% 
  arrange(type,Year5yRnd, SibStrF, SibStrM) %>% 
  mutate(sex = c(rep("F",32),rep("M",32)),
         Year5yRnd = if_else(Year5yRnd==1,"Obs_1985","Obs_2010")) %>% 
  dplyr::select(-type)

#### #### ####
## CF value ##
#### #### ####
df2 <- read_dta("1_Data/2_HM_Model/Hazard-Model/02_Output/02_HM/Sib4Cat/PSTFMH_CF.dta") %>%
  pivot_longer(
    cols = starts_with("PSTFMH_"),
    names_to = "sex",
    names_prefix = "PSTFMH_CF_",
    values_to = "rate",
    values_drop_na = TRUE
  ) %>% 
  arrange(Year5yRnd, sex, SibStrF, SibStrF) %>% 
  mutate(Year5yRnd = "CF_2010",
         rate=1-(1/exp(rate)))

df <-  bind_rows(df1,df2) %>% 
  mutate(SibStrF=case_when(
    SibStrF == 1 ~ "Only child",
    SibStrF == 2 ~ "Eldest, no brothers",
    SibStrF == 3 ~ "Not eldest, no brothers",
    SibStrF == 4 ~ "Daughter, brothers"),
    SibStrF=factor(SibStrF,levels=c("Only child","Eldest, no brothers","Not eldest, no brothers","Daughter, brothers")),
    SibStrM=case_when(
      SibStrM == 1 ~ "Only child",
      SibStrM == 2 ~ "Eldest, no brothers",
      SibStrM == 3 ~ "Eldest, brothers",
      SibStrM == 4 ~ "Not eldest"),
    SibStrM=factor(SibStrM,levels=c("Only child","Eldest, no brothers","Eldest, brothers","Not eldest")),
    Year5yRnd=factor(Year5yRnd, levels=c("Obs_1985","Obs_2010","CF_2010")))

######################################################################
# Data viz
######################################################################

df %>% filter(sex=="F") %>% 
  ggplot(aes(x=SibStrM, y=rate, fill=Year5yRnd,group=Year5yRnd)) +facet_wrap(~SibStrF)+
  geom_bar(stat="identity", position="dodge")+xlab("")+ylab("")+theme_few() +scale_fill_brewer(palette="Blues") +
  theme(legend.title=element_blank(), legend.position = "bottom")+ylim(0,1)+theme(axis.text.x=element_text(angle=20,vjust=1,hjust=1))
ggsave(height=6,width=9,dpi=200, filename="Figures/6.Comparison/PSTFMR-ObsCF-Female-4cat.pdf",  family = "Helvetica")

df %>% filter(sex=="M") %>% 
  ggplot(aes(x=SibStrF, y=rate, fill=Year5yRnd,group=Year5yRnd)) +facet_wrap(~SibStrM)+
  geom_bar(stat="identity", position="dodge")+xlab("")+ylab("")+theme_few() +scale_fill_brewer(palette="Blues") +
  theme(legend.title=element_blank(), legend.position = "bottom")+ylim(0,1)+theme(axis.text.x=element_text(angle=20,vjust=1,hjust=1))
ggsave(height=6,width=9,dpi=200, filename="Figures/6.Comparison/PSTFMR-ObsCF-Male-4cat.pdf",  family = "Helvetica")


