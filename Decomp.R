#===============================================================================
# 2020/06/15
# Harmonic mean models counterfactual decomposition
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
# Four categories
######################################################################
df1 <- read_dta("1_Data/2_HM_Model/Hazard-Model/02_Output/02_HM/Sib4Cat/DecompPSTMFR.dta") %>% 
  dplyr::select(SibStrF, SibStrM, ContAR_F, ContAR_M) %>% 
  gather( "Female", "Cont", 3:4) %>% 
  mutate(Female=case_when(
    Female == "ContAR_F" ~ 1,
    Female == "ContAR_M" ~ 0),
    AR = 1)

df2 <- read_dta("1_Data/2_HM_Model/Hazard-Model/02_Output/02_HM/Sib4Cat/DecompPSTMFR.dta") %>% 
  dplyr::select(SibStrF, SibStrM, ContFA_F, ContFA_M) %>% 
  gather( "Female", "Cont", 3:4) %>% 
  mutate(Female=case_when(
    Female == "ContFA_F" ~ 1,
    Female == "ContFA_M" ~ 0),
    AR = 0)

df <- rbind(df1,df2) %>% 
  mutate(SibStrF=case_when(
    SibStrF == 1 ~ "Only child",
    SibStrF == 2 ~ "Eldest, no brothers",
    SibStrF == 3 ~ "Not eldest, no brothers",
    SibStrF == 4 ~ "Daughter, brothers"
  ),
  SibStrF=factor(SibStrF,levels=c("Only child","Eldest, no brothers","Not eldest, no brothers","Daughter, brothers")),
  SibStrM=case_when(
    SibStrM == 1 ~ "Only child",
    SibStrM == 2 ~ "Eldest, no brothers",
    SibStrM == 3 ~ "Eldest, brothers",
    SibStrM == 4 ~ "Not eldest"
  ),
  SibStrM=factor(SibStrM,levels=c("Only child","Eldest, no brothers","Eldest, brothers","Not eldest")),
  AR=case_when(
    AR == 0 ~ "Force of Attraction",
    AR == 1 ~ "Availability Ratio"
  )) 
df1 <- df %>% 
  filter(Female == 1)
df2 <- df %>% 
  filter(Female == 0)

######################################################################
# Data viz
######################################################################

ggplot(df1, aes(x=SibStrM, y=Cont, fill=AR,group=AR)) + 
  geom_bar(stat="identity")+xlab("")+ylab("")+theme_few() +facet_wrap(~SibStrF)+scale_fill_brewer(palette="Paired") +
  theme(legend.title=element_blank())+ylim(-0.35,0.1)+theme(axis.text.x=element_text(angle=60,vjust=1,hjust=1))
ggsave(height=6,width=8,dpi=200, filename="Figures/4.Decomp/DecompFemaleAR5-CFAR-4cat.pdf",  family = "Helvetica")

ggplot(df2, aes(x=SibStrF, y=Cont, fill=AR,group=AR)) + 
  geom_bar(stat="identity")+xlab("")+ylab("")+theme_few() +facet_wrap(~SibStrM) + scale_fill_brewer(palette="Paired") +
  theme(legend.title=element_blank())+ylim(-0.35,0.1)+theme(axis.text.x=element_text(angle=60,vjust=1,hjust=1))
ggsave(height=6,width=8,dpi=200, filename="Figures/4.Decomp/DecompMaleAR5-CFAR-4cat.pdf",  family = "Helvetica")