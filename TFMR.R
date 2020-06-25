#===============================================================================
# 2020/06/15
# Total first marriage rates
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
library(egg)
library(haven)

######################################################################
# Assign colors
######################################################################
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

######################################################################
# Set directory
######################################################################
setwd("/Users/fumiyau/Dropbox (Princeton)/49.Sibship_Assortative_Mating/")

######################################################################
# Female TFMR
######################################################################
df1 <- read_dta("1_Data/2_HM_Model/Hazard-Model/02_Output/02_HM/Sib4Cat/TFMH_TFMR_F_obs.dta") %>% 
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
  SibStrF=factor(SibStrF,levels=c("Only child","Eldest, no brothers","Not eldest, no brothers","Daughter, brothers"))
  )

######################################################################
# Male TFMR
######################################################################
df2 <- read_dta("1_Data/2_HM_Model/Hazard-Model/02_Output/02_HM/Sib4Cat/TFMH_TFMR_M_obs.dta") %>% 
  mutate(Year5yRnd=case_when(
    Year5yRnd == 1 ~ 1985,
    Year5yRnd == 2 ~ 1990,
    Year5yRnd == 3 ~ 1995,
    Year5yRnd == 4 ~ 2000,
    Year5yRnd == 5 ~ 2005,
    Year5yRnd == 6 ~ 2010
  ),
  SibStrM=case_when(
    SibStrM == 1 ~ "Only child",
    SibStrM == 2 ~ "Eldest, no brothers",
    SibStrM == 3 ~ "Eldest, brothers",
    SibStrM == 4 ~ "Not eldest"
  ),
  SibStrM=factor(SibStrM,levels=c("Only child","Eldest, no brothers","Eldest, brothers","Not eldest"))
  )

######################################################################
# Data viz
######################################################################
plot_df1 <- ggplot(df1, mapping = aes(x=Year5yRnd,y=TFMR_F_obs,group=SibStrF,color=SibStrF,shape=SibStrF))+
  geom_point()+geom_line()+ylab("")+theme_few() + ylim(0.5,1) +
  scale_x_continuous(breaks = seq(1985,2010,5)) + xlab("") +
  theme(legend.title=element_blank(), legend.position = c(0.3, 0.2)) +
  scale_colour_manual(values=cbp1)
ggsave(plot_df1, height=4.5,width=6,dpi=200, filename="9.Uchikoshi/Figures/2.TFMR/TFMR-Female-4cat.pdf",  family = "Helvetica")

plot_df2 <- ggplot(df2, mapping = aes(x=Year5yRnd,y=TFMR_M_obs,group=SibStrM,color=SibStrM,shape=SibStrM))+
  geom_point()+geom_line()+ylab("")+theme_few() + ylim(0.5,1) +
  scale_x_continuous(breaks = seq(1985,2010,5)) + xlab("") +
  theme(legend.title=element_blank(), legend.position = c(0.3, 0.2))+
  scale_colour_manual(values=cbp1)
ggsave(plot_df2,height=4.5,width=6,dpi=200, filename="9.Uchikoshi/Figures/2.TFMR/TFMR-Male-4cat.pdf",  family = "Helvetica")

plot<-   ggarrange(plot_df1, plot_df2,
                   labels = c("A","B"),
                   ncol = 2, nrow = 1)
ggsave(plot,height=4.5,width=9,dpi=200, filename="9.Uchikoshi/Figures/2.TFMR/TFMR-Comb-4cat.pdf",  family = "Helvetica")
