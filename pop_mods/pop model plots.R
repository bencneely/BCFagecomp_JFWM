#--------------------------------------------------------------
#Ben Neely
#09/25/2023
#Model population response for different regulations
#--------------------------------------------------------------

## Clear R
cat("\014")  
rm(list=ls())

## Install and load packages
## Checks if package is installed, installs if not, activates for current session
if("FSA" %in% rownames(installed.packages()) == FALSE) {install.packages("FSA")}
library(FSA)

if("rio" %in% rownames(installed.packages()) == FALSE) {install.packages("rio")}
library(rio)

if("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")}
library(patchwork)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

## Set ggplot theme
pubtheme=theme_classic()+
  theme(panel.grid=element_blank(), 
        panel.background=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(fill="transparent"),
        axis.title=element_text(size=22,color="black",face="bold"),
        axis.text=element_text(size=18,color="black"),
        #axis.text.x=element_text(angle=90,hjust=0.5,vjust=0.5),
        legend.text=element_text(size=14),
        legend.background=element_rect(fill="transparent"),
        legend.position=c(0.98,0),
        legend.justification=c(1,0),
        legend.spacing=unit(0.8,"cm"),
        legend.key.width=unit(1.2,"cm"))
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/JFWM analysis/pop models")

## Read in population model data with import and assign reg type to a factor
dat=import("popmoddat_01092024.csv")%>%
  mutate(structure=factor(structure))

## Set up color map
colmap=c("#440154","#31688E","#35B779","#FDE725")

################################################################################
################################################################################
################################################################################
## Graph YPR and SPR for each structure

################################################################################
################################################################################
## YPR plots
yprdat=filter(dat,metric=="yield")

################################################################################
## YPR
## 381 mm minimum length limit
min15_ypr=ggplot(subset(yprdat,reg=="min15"))+
  geom_line(aes(x=exp,y=estimate,color=structure),linewidth=1.2)+
  scale_color_manual(values=colmap,
                     labels=c("Articulating process","Articulating surface",
                              "Basal recess","Lapillus otolith"),
                        name="")+
  scale_x_continuous(breaks=seq(0,0.4,0.05),
                     labels=scales::percent,
                     name="")+
  scale_y_continuous(breaks=seq(0,1,0.1),
                     name="Yield per recruit (kg)")+
  coord_cartesian(xlim=c(0,0.41),
                  ylim=c(0.19,1.01),
                  expand=F)+
  annotate("text",x=0.21,y=0.375,label="381 mm MLL",hjust=0,vjust=0,size=7)+
  pubtheme

## 762 mm maximum length limit
max30_ypr=ggplot(subset(yprdat,reg=="max30"))+
  geom_line(aes(x=exp,y=estimate,color=structure),linewidth=1.2)+
  scale_color_manual(values=colmap,
                     labels=c("Articulating process","Articulating surface",
                              "Basal recess","Lapillus otolith"),
                     name="")+
  scale_x_continuous(breaks=seq(0,0.4,0.05),
                     labels=scales::percent,
                     name="")+
  scale_y_continuous(breaks=seq(0,1,0.1),
                     name="")+
  coord_cartesian(xlim=c(0,0.41),
                  ylim=c(0.09,0.63),
                  expand=F)+
  annotate("text",x=0.21,y=0.215,label="762 mm maximum",hjust=0,vjust=0,size=7)+
  pubtheme

## Combine YPR plots for plotting via patchwork
yprout=min15_ypr|max30_ypr

################################################################################
################################################################################
## SPR plots
sprdat=filter(dat,metric=="spr")

################################################################################
## SPR
## 381 mm minimum length limit
min15_spr=ggplot(subset(sprdat,reg=="min15"))+
  geom_line(aes(x=exp,y=estimate,color=structure),linewidth=1.2)+
  scale_color_manual(values=colmap,
                     labels=c("Articulating process","Articulating surface",
                              "Basal recess","Lapillus otolith"),
                     name="")+
  scale_x_continuous(breaks=seq(0,0.4,0.05),
                     labels=scales::percent,
                     name="Exploitation")+
  scale_y_continuous(breaks=seq(0,0.8,0.1),
                     labels=scales::percent,
                     name="Spawning potential ratio")+
  coord_cartesian(xlim=c(0,0.41),
                  ylim=c(0,0.88),
                  expand=F)+
  geom_hline(yintercept=0.3,linetype="dashed",color="black",linewidth=1.2)+
  annotate("text",x=0.215,y=0.86,label="381 mm MLL",hjust=0,vjust=1,size=7)+
  pubtheme+
  theme(legend.position=c(1,0.97),
        legend.justification=c(1,1))

## 762 mm maximum length limit
max30_spr=ggplot(subset(sprdat,reg=="max30"))+
  geom_line(aes(x=exp,y=estimate,color=structure),linewidth=1.2)+
  scale_color_manual(values=colmap,
                     labels=c("Articulating process","Articulating surface",
                              "Basal recess","Lapillus otolith"),
                     name="")+
  scale_x_continuous(breaks=seq(0,0.4,0.05),
                     labels=scales::percent,
                     name="Exploitation")+
  scale_y_continuous(breaks=seq(0,0.8,0.1),
                     labels=scales::percent,
                     name="")+
  coord_cartesian(xlim=c(0,0.41),
                  ylim=c(0,0.88),
                  expand=F)+
  geom_hline(yintercept=0.3,linetype="dashed",color="black",linewidth=1.2)+
  annotate("text",x=0.215,y=0.86,label="762 mm maximum",hjust=0,vjust=1,size=7)+
  pubtheme+
  theme(legend.position=c(1,0.97),
        legend.justification=c(1,1))

## Combine SPR plots for plotting via patchwork
sprout=min15_spr|max30_spr

################################################################################
################################################################################
## combine all plots for a look
modplots=yprout/sprout
ggsave(plot=modplots,"popmods.png",width=12,height=12,units="in",bg="white")
