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

df1 <- read_dta("1_Data/2_HM_Model/Hazard-Model/02_Output/02_HM/Sib4Cat/PSTFMH_PSTFMR_obs.dta") %>% 
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

dfx <- df1 %>% 
  filter(Year5yRnd == 1985 | Year5yRnd == 2010) %>% 
  mutate(Year5yRnd=as.character(Year5yRnd))

df3 <- df1 %>% 
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



######################################################################
# Comparison
######################################################################

df1f <- df1 %>% 
  dplyr::select(Year5yRnd, SibStrF,SibStrM, PSTFMR_F_obs) %>% 
  mutate(type="tempo adjusted")

df1m <- df1 %>% 
  dplyr::select(Year5yRnd, SibStrF,SibStrM, PSTFMR_M_obs) %>% 
  mutate(type="tempo adjusted")


df2 <- read_dta("9.Uchikoshi/1_Data/2_HM_Model/Rate-Model/02_Output/02_HM/Sib4Cat/PSTFMR_obs.dta") %>% 
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


df2f <- df2 %>% 
  dplyr::select(Year5yRnd, SibStrF,SibStrM, PSTFMR_F_obs) %>% 
  mutate(type="crude")

df2m <- df2 %>% 
  dplyr::select(Year5yRnd, SibStrF,SibStrM, PSTFMR_M_obs) %>% 
  mutate(type="crude")

dff <- bind_rows(df1f,df2f)
dfm <- bind_rows(df1m,df2m)

plot_dff <- ggplot(dff, mapping = aes(x=Year5yRnd,y=PSTFMR_F_obs,group=type,color=type,shape=type))+
  geom_point()+geom_line(aes(linetype=type))+ylab("")+theme_few() + ylim(0.0,1) + facet_wrap(SibStrF~SibStrM)+
  scale_x_continuous(breaks = seq(1985,2010,5)) + xlab("") +
  theme(legend.title=element_blank(), legend.position = "bottom") +
  scale_colour_manual(values=cbp1)
ggsave(plot_dff, height=7.5,width=10,dpi=200, filename="9.Uchikoshi/Figures/3.PSTFMR/PSTFMR-Female-4cat-comp.pdf",  family = "Helvetica")

plot_dfm <- ggplot(dfm, mapping = aes(x=Year5yRnd,y=PSTFMR_M_obs,group=type,color=type,shape=type))+
  geom_point()+geom_line(aes(linetype=type))+ylab("")+theme_few() + ylim(0.0,1) + facet_wrap(SibStrM~SibStrF)+
  scale_x_continuous(breaks = seq(1985,2010,5)) + xlab("") +
  theme(legend.title=element_blank(), legend.position = "bottom") +
  scale_colour_manual(values=cbp1)
ggsave(plot_dfm, height=7.5,width=10,dpi=200, filename="9.Uchikoshi/Figures/3.PSTFMR/PSTFMR-Male-4cat-comp.pdf",  family = "Helvetica")
