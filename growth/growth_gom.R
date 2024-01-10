#---------------------------------------------------------------------------------------
#Ben Neely
#01/09/2024
#Fit Gompertz growth models to ages derived from each structure
#---------------------------------------------------------------------------------------

## Clear R
cat("\014")  
rm(list=ls())

## Set seed for reproducibility
set.seed(109)

## http://derekogle.com/IFAR/supplements/growth/OtherGrowthFuns.html

## Install and load packages
## Checks if package is installed, installs if not, activates for current session
if("FSA" %in% rownames(installed.packages()) == FALSE) {install.packages("FSA")}
library(FSA)

if("rio" %in% rownames(installed.packages()) == FALSE) {install.packages("rio")}
library(rio)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

if("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")}
library(patchwork)

if("nlstools" %in% rownames(installed.packages()) == FALSE) {install.packages("nlstools")}
library(nlstools)

if("car" %in% rownames(installed.packages()) == FALSE) {install.packages("car")}
library(car)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/JFWM analysis/")

## Read in data with import
dat=import("allaged.csv")

## Set up plotting theme
pubtheme=theme_classic()+
  theme(panel.border=element_rect(color="black",fill=NA),
        axis.title=element_text(size=22,color="black",face="bold"),
        axis.text=element_text(size=18,color="black"),
        axis.text.x=element_text(angle=0,vjust=0.5),
        legend.position=c(0,1),
        legend.justification=c(0,1),
        legend.text=element_text(size=14),
        legend.title=element_blank(),
        legend.background=element_rect(fill="transparent"),
        legend.key.width=unit(3,"cm"))

## Set up functions to estimate length up to max observed age
artproc_ages=seq(0,max(subset(dat,struc=="artproc")$age),by=0.05)
artproc_gompred=function(x) predict(x,data.frame(age=artproc_ages))

artsurf_ages=seq(0,max(subset(dat,struc=="artsurf")$age),by=0.05)
artsurf_gompred=function(x) predict(x,data.frame(age=artsurf_ages))

basal_ages=seq(0,max(subset(dat,struc=="basal")$age),by=0.05)
basal_gompred=function(x) predict(x,data.frame(age=basal_ages))

otolith_ages=seq(0,max(subset(dat,struc=="otolith")$age),by=0.05)
otolith_gompred=function(x) predict(x,data.frame(age=otolith_ages))

## Define growth model function and set starting values
gom=GompertzFuns()
gom_sv=list(gi=0.1,ti=6)

## Find Linf for each population
linf=max(dat$cmgroup)

## Set up color map
colmap=c("#440154","#31688E","#35B779","#FDE725")

############################################################################################
############################################################################################
## artproc Gompertz growth model with fixed Linf
## Fit the model and calculate confidence intervals
artproc_gom_fit=nls(cmgroup~gom(age,linf,gi,ti),data=subset(dat,struc=="artproc"),start=gom_sv)
artproc_gom_boot=nlsBoot(artproc_gom_fit,niter=400)
(artproc_gom_parms=cbind(ests=coef(artproc_gom_fit),confint(artproc_gom_boot))%>%
    rbind(linf=linf))

## Use model to predict length at age for plotting
artproc_predboot=Boot(artproc_gom_fit,f=artproc_gompred)
artproc_preds=data.frame(artproc_ages,
                         artproc_gompred(artproc_gom_fit),
                         confint(artproc_predboot),
                         "artproc")
names(artproc_preds)=c("age","tl","lci95","uci95","struc")

## Equation for plot label
artproc_lab=paste0("Articulating~process:TL==",1170,"~group('(',e^{-e^{",0.126,"~(age","-",5.201,")}},')')")

############################################################################################
## artsurf Gompertz growth model with fixed Linf
## Fit the model and calculate confidence intervals
artsurf_gom_fit=nls(cmgroup~gom(age,linf,gi,ti),data=subset(dat,struc=="artsurf"),start=gom_sv)
artsurf_gom_boot=nlsBoot(artsurf_gom_fit,niter=400)
(artsurf_gom_parms=cbind(ests=coef(artsurf_gom_fit),confint(artsurf_gom_boot))%>%
    rbind(linf=linf))

## Use model to predict length at age for plotting
artsurf_predboot=Boot(artsurf_gom_fit,f=artsurf_gompred)
artsurf_preds=data.frame(artsurf_ages,
                         artsurf_gompred(artsurf_gom_fit),
                         confint(artsurf_predboot),
                         "artsurf")
names(artsurf_preds)=c("age","tl","lci95","uci95","struc")

## Equation for plot label
artsurf_lab=paste0("Articulating~surface:TL==",1170,"~group('(',e^{-e^{",0.115,"~(age","-",6.119,")}},')')")

############################################################################################
## basal Gompertz growth model with fixed Linf
## Fit the model and calculate confidence intervals
basal_gom_fit=nls(cmgroup~gom(age,linf,gi,ti),data=subset(dat,struc=="basal"),start=gom_sv)
basal_gom_boot=nlsBoot(basal_gom_fit,niter=400)
(basal_gom_parms=cbind(ests=coef(basal_gom_fit),confint(basal_gom_boot))%>%
    rbind(linf=linf))

## Use model to predict length at age for plotting
basal_predboot=Boot(basal_gom_fit,f=basal_gompred)
basal_preds=data.frame(basal_ages,
                       basal_gompred(basal_gom_fit),
                       confint(basal_predboot),
                       "basal")
names(basal_preds)=c("age","tl","lci95","uci95","struc")

## Equation for plot label
basal_lab=paste0("Basal~recess:TL==",1170,"~group('(',e^{-e^{",0.118,"~(age","-",5.911,")}},')')")

############################################################################################
## otolith Gompertz growth model with fixed Linf
## Fit the model and calculate confidence intervals
otolith_gom_fit=nls(cmgroup~gom(age,linf,gi,ti),data=subset(dat,struc=="otolith"),start=gom_sv)
otolith_gom_boot=nlsBoot(otolith_gom_fit,niter=400)
(otolith_gom_parms=cbind(ests=coef(otolith_gom_fit),confint(otolith_gom_boot))%>%
    rbind(linf=linf))

## Use model to predict length at age for plotting
otolith_predboot=Boot(otolith_gom_fit,f=otolith_gompred)
otolith_preds=data.frame(otolith_ages,
                         otolith_gompred(otolith_gom_fit),
                         confint(otolith_predboot),
                         "otolith")
names(otolith_preds)=c("age","tl","lci95","uci95","struc")

## Equation for plot label
otolith_lab=paste0("Lapillus~otolith:TL==",1170,"~group('(',e^{-e^{",0.096,"~(age","-",6.537,")}},')')")


############################################################################################
############################################################################################
## Combine data for plotting Gompertz growth models
plotdat=bind_rows(artproc_preds,artsurf_preds,basal_preds,otolith_preds)

## Plot growth
ggplot(plotdat)+
  geom_ribbon(aes(x=age,ymin=lci95,ymax=uci95,fill=struc),alpha=0.5)+
  geom_line(aes(x=age,y=tl,color=struc),linewidth=0.9)+
  scale_fill_manual(values=colmap,
                     labels=c("Articulating process","Articulating surface",
                              "Basal recess","Lapillus otolith"),
                     name="")+
  scale_color_manual(values=colmap,
                     labels=c("Articulating process","Articulating surface",
                              "Basal recess","Lapillus otolith"),
                     name="")+
  scale_x_continuous(breaks=seq(0,19,1),
                     name="Estimated age")+
  scale_y_continuous(breaks=seq(0,1000,100),
                     name="Total length (mm)")+
  coord_cartesian(xlim=c(0,19.3),
                  ylim=c(0,1010),
                  expand=F)+
  annotate("text",x=8,y=230,label=artproc_lab,hjust=0,vjust=0,size=5,parse=T)+
  annotate("text",x=8,y=170,label=artsurf_lab,hjust=0,vjust=0,size=5,parse=T)+
  annotate("text",x=8,y=110,label=basal_lab,hjust=0,vjust=0,size=5,parse=T)+
  annotate("text",x=8,y=50,label=otolith_lab,hjust=0,vjust=0,size=5,parse=T)+
  pubtheme

## Save growth model plots
ggsave(plot=last_plot(),"growth/Gompertz growth mods.png",height=6,width=8,units="in")
