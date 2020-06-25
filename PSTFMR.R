#===============================================================================
# 2020/06/15
# Pairing specific marriage rates
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
setwd("/Users/fumiyau/Dropbox (Princeton)/49.Sibship_Assortative_Mating/")

######################################################################
# Pairing specific TFMRs
######################################################################

df <- read_dta("1_Data/2_HM_Model/Hazard-Model/02_Output/02_HM/Sib4Cat/PSTFMH_PSTFMR_obs.dta") %>% 
  mutate(Year5yRnd=case_when(
    Year5yRnd == 1 ~ 1985,
    Year5yRnd == 2 ~ 1990,
    Year5yRnd == 3 ~ 1995,
    Year5yRnd == 4 ~ 2000,
    Year5yRnd == 5 ~ 2005,
    Year5yRnd == 6 ~ 2010
  ),
  SibStrF=case_when(
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
  SibStrM=factor(SibStrM,levels=c("Only child","Eldest, no brothers","Eldest, brothers","Not eldest"))
  ) 

dfx <- df %>% 
  filter(Year5yRnd == 1985 | Year5yRnd == 2010) %>% 
  mutate(Year5yRnd=as.character(Year5yRnd))

df3 <- df %>% 
  filter(Year5yRnd == 1985 | Year5yRnd == 2010) 

######################################################################
# Data viz
######################################################################

ggplot(dfx, aes(x=SibStrM, y=PSTFMR_F_obs, fill=Year5yRnd,group=Year5yRnd)) + 
  geom_bar(stat="identity", position="dodge")+facet_wrap(~SibStrF)+xlab("")+ylab("")+theme_few() +scale_fill_brewer(palette="Paired") +
  theme(legend.title=element_blank())+ylim(0,0.8)+theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
ggsave(height=6,width=9,dpi=200, filename="9.Uchikoshi/Figures/3.PSTFMR/PSTFMR-Female-4cat.pdf",  family = "Helvetica")

ggplot(dfx, aes(x=SibStrF, y=PSTFMR_M_obs, fill=Year5yRnd,group=Year5yRnd)) + 
  geom_bar(stat="identity", position="dodge")+facet_wrap(~SibStrM)+xlab("")+ylab("")+theme_few() +scale_fill_brewer(palette="Paired") +
  theme(legend.title=element_blank())+ylim(0,0.8)+theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
ggsave(height=6,width=9,dpi=200, filename="9.Uchikoshi/Figures/3.PSTFMR/PSTFMR-Male-4cat.pdf",  family = "Helvetica")