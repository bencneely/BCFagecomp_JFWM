#---------------------------------------------------------------------------------------
#Ben Neely
#01/09/2024
#Calculate annual mortality from catch curve analysis
#---------------------------------------------------------------------------------------

## Clear R
cat("\014")  
rm(list=ls())

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

if("car" %in% rownames(installed.packages()) == FALSE) {install.packages("car")}
library(car)

## Set seed for reproducibility
set.seed=109
## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/JFWM analysis/")

## Read in data with import
dat=import("allaged.csv")

## Format so that we have catch by age
dat1=dat%>%
  group_by(struc,age)%>%
  summarize(n=n()+1)%>%
  ungroup()%>%
  mutate(ln_n=log(n))

## Set up plotting theme
pubtheme=theme_classic()+
  theme(panel.border=element_rect(color="black",fill=NA),
        axis.title=element_text(size=22,color="black",face="bold"),
        axis.text=element_text(size=18,color="black"),
        axis.text.x=element_text(angle=0,vjust=0.5),
        legend.position="none",
        plot.margin=margin(0,0,0,0,"cm"))

################################################################################
################################################################################
## Calculate annual mortality for each "population"

################################################################################
################################################################################
## Articulating process
## Minimum and maximum ages
artproc_min=min(subset(dat1,struc=="artproc")$age)
artproc_max=max(subset(dat1,struc=="artproc")$age)

## Create artproc data set
## Add n=1 (i.e., n+0) for all ages not observed
artproc_dat=filter(dat1,struc=="artproc")%>%
  complete(age=artproc_min:artproc_max,
           fill=list(struc="artproc",
                     n=1,
                     ln_n=log(1)))

## Look at data to see what age descending limb of the catch curve starts
head(artproc_dat,n=10)
## Looks like it starts descending at age-2

## Create catch curve
## Page 210 in Ogle 2016 IFAR
artproc_cc=catchCurve(n~age,artproc_dat,
                      ages2use=2:artproc_max,
                      weighted=T)
artproc_cc_out=cbind(summary(artproc_cc),confint(artproc_cc))

## Save estimates for plot labels
artproc_z=paste0("Z = ",round(artproc_cc_out[1,1],3))
artproc_a_pct=scales::label_percent()(round(artproc_cc_out[2,1],2)/100)
artproc_a=paste0("A = ",artproc_a_pct)

################################################################################
## Plot catch curve
## Format data to show used and unused ages
artproc_plotdat=artproc_dat%>%
  mutate(used=case_when(age %in% 2:artproc_max ~ 1,
                        TRUE ~ 0))

## Create plot
artproc_plot=ggplot()+
  geom_point(data=artproc_plotdat,aes(x=age,y=ln_n,shape=factor(used)),size=4)+
  scale_shape_manual(values=c(1,19))+
  geom_smooth(data=subset(artproc_plotdat,age>=2),
              aes(x=age,y=ln_n,
                  weight=ln_n),
              color="black",
              method='lm',
              formula=y~x,
              se=F)+
  scale_y_continuous(limits=c(-0.1,8.1),
                     breaks=seq(0,8,1),
                     labels=seq(0,8,1),
                     expand=c(0,0),
                     name="log(Catch)")+
  scale_x_continuous(limits=c(0,19.5),
                     breaks=seq(0,18,2),
                     labels=seq(0,18,2),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=19,y=8,label="Articulating process",hjust=1,vjust=1,size=8)+
  annotate("text",x=13,y=7.4,label=artproc_z,hjust=0,vjust=1,size=8)+
  annotate("text",x=13,y=6.9,label=artproc_a,hjust=0,vjust=1,size=8)+
  pubtheme

################################################################################
################################################################################
## Articulating surface
## Minimum and maximum ages
artsurf_min=min(subset(dat1,struc=="artsurf")$age)
artsurf_max=max(subset(dat1,struc=="artsurf")$age)

## Create artsurf data set
## Add n=1 (i.e., n+0) for all ages not observed
artsurf_dat=filter(dat1,struc=="artsurf")%>%
  complete(age=artsurf_min:artsurf_max,
           fill=list(struc="artsurf",
                     n=1,
                     ln_n=log(1)))

## Look at data to see what age descending limb of the catch curve starts
head(artsurf_dat,n=10)
## Looks like it starts descending at age-2

## Create catch curve
## Page 210 in Ogle 2016 IFAR
artsurf_cc=catchCurve(n~age,artsurf_dat,
                      ages2use=2:artsurf_max,
                      weighted=T)
artsurf_cc_out=cbind(summary(artsurf_cc),confint(artsurf_cc))

## Save estimates for plot labels
artsurf_z=paste0("Z = ",round(artsurf_cc_out[1,1],3))
artsurf_a_pct=scales::label_percent()(round(artsurf_cc_out[2,1],2)/100)
artsurf_a=paste0("A = ",artsurf_a_pct)

################################################################################
## Plot catch curve
## Format data to show used and unused ages
artsurf_plotdat=artsurf_dat%>%
  mutate(used=case_when(age %in% 2:artsurf_max ~ 1,
                        TRUE ~ 0))

## Create plot
artsurf_plot=ggplot()+
  geom_point(data=artsurf_plotdat,aes(x=age,y=ln_n,shape=factor(used)),size=4)+
  scale_shape_manual(values=c(19))+
  geom_smooth(data=subset(artsurf_plotdat,age>=2),
              aes(x=age,y=ln_n,
                  weight=ln_n),
              color="black",
              method='lm',
              formula=y~x,
              se=F)+
  scale_y_continuous(limits=c(-0.1,8.1),
                     breaks=seq(0,8,1),
                     labels=seq(0,8,1),
                     expand=c(0,0),
                     name="")+
  scale_x_continuous(limits=c(0,19.5),
                     breaks=seq(0,18,2),
                     labels=seq(0,18,2),
                     expand=c(0,0),
                     name="")+
  annotate("text",x=19,y=8,label="Articulating surface",hjust=1,vjust=1,size=8)+
  annotate("text",x=13,y=7.4,label=artsurf_z,hjust=0,vjust=1,size=8)+
  annotate("text",x=13,y=6.9,label=artsurf_a,hjust=0,vjust=1,size=8)+
  pubtheme

################################################################################
################################################################################
## Basal recess
## Minimum and maximum ages
basal_min=min(subset(dat1,struc=="basal")$age)
basal_max=max(subset(dat1,struc=="basal")$age)

## Create basal data set
## Add n=1 (i.e., n+0) for all ages not observed
basal_dat=filter(dat1,struc=="basal")%>%
  complete(age=basal_min:basal_max,
           fill=list(struc="basal",
                     n=1,
                     ln_n=log(1)))

## Look at data to see what age descending limb of the catch curve starts
head(basal_dat,n=10)
## Looks like it starts descending at age-2

## Create catch curve
## Page 210 in Ogle 2016 IFAR
basal_cc=catchCurve(n~age,basal_dat,
                      ages2use=2:basal_max,
                      weighted=T)
basal_cc_out=cbind(summary(basal_cc),confint(basal_cc))

## Save estimates for plot labels
basal_z=paste0("Z = ",round(basal_cc_out[1,1],3))
basal_a_pct=scales::label_percent()(round(basal_cc_out[2,1],2)/100)
basal_a=paste0("A = ",basal_a_pct)

################################################################################
## Plot catch curve
## Format data to show used and unused ages
basal_plotdat=basal_dat%>%
  mutate(used=case_when(age %in% 2:basal_max ~ 1,
                        TRUE ~ 0))

## Create plot
basal_plot=ggplot()+
  geom_point(data=basal_plotdat,aes(x=age,y=ln_n,shape=factor(used)),size=4)+
  scale_shape_manual(values=c(19))+
  geom_smooth(data=subset(basal_plotdat,age>=2),
              aes(x=age,y=ln_n,
                  weight=ln_n),
              color="black",
              method='lm',
              formula=y~x,
              se=F)+
  scale_y_continuous(limits=c(-0.1,8.1),
                     breaks=seq(0,8,1),
                     labels=seq(0,8,1),
                     expand=c(0,0),
                     name="log(Catch)")+
  scale_x_continuous(limits=c(0,19.5),
                     breaks=seq(0,18,2),
                     labels=seq(0,18,2),
                     expand=c(0,0),
                     name="Estimated age")+
  annotate("text",x=19,y=8,label="Basal recess",hjust=1,vjust=1,size=8)+
  annotate("text",x=13,y=7.4,label=basal_z,hjust=0,vjust=1,size=8)+
  annotate("text",x=13,y=6.9,label=basal_a,hjust=0,vjust=1,size=8)+
  pubtheme

################################################################################
################################################################################
## Lapillus otolith
## Minimum and maximum ages
otolith_min=min(subset(dat1,struc=="otolith")$age)
otolith_max=max(subset(dat1,struc=="otolith")$age)

## Create otolith data set
## Add n=1 (i.e., n+0) for all ages not observed
otolith_dat=filter(dat1,struc=="otolith")%>%
  complete(age=otolith_min:otolith_max,
           fill=list(struc="otolith",
                     n=1,
                     ln_n=log(1)))

## Look at data to see what age descending limb of the catch curve starts
head(otolith_dat,n=10)
## Looks like it starts descending at age-2

## Create catch curve
## Page 210 in Ogle 2016 IFAR
otolith_cc=catchCurve(n~age,otolith_dat,
                      ages2use=2:otolith_max,
                      weighted=T)
otolith_cc_out=cbind(summary(otolith_cc),confint(otolith_cc))

## Save estimates for plot labels
otolith_z=paste0("Z = ",round(otolith_cc_out[1,1],3))
otolith_a_pct=scales::label_percent()(round(otolith_cc_out[2,1],2)/100)
otolith_a=paste0("A = ",otolith_a_pct)

################################################################################
## Plot catch curve
## Format data to show used and unused ages
otolith_plotdat=otolith_dat%>%
  mutate(used=case_when(age %in% 2:otolith_max ~ 1,
                        TRUE ~ 0))

## Create plot
otolith_plot=ggplot()+
  geom_point(data=otolith_plotdat,aes(x=age,y=ln_n,shape=factor(used)),size=4)+
  scale_shape_manual(values=c(1,19))+
  geom_smooth(data=subset(otolith_plotdat,age>=2),
              aes(x=age,y=ln_n,
                  weight=ln_n),
              color="black",
              method='lm',
              formula=y~x,
              se=F)+
  scale_y_continuous(limits=c(-0.1,8.1),
                     breaks=seq(0,8,1),
                     labels=seq(0,8,1),
                     expand=c(0,0),
                     name="")+
  scale_x_continuous(limits=c(0,19.5),
                     breaks=seq(0,18,2),
                     labels=seq(0,18,2),
                     expand=c(0,0),
                     name="Estimated age")+
  annotate("text",x=19,y=8,label="Lapillus otolith",hjust=1,vjust=1,size=8)+
  annotate("text",x=13,y=7.4,label=otolith_z,hjust=0,vjust=1,size=8)+
  annotate("text",x=13,y=6.9,label=otolith_a,hjust=0,vjust=1,size=8)+
  pubtheme

################################################################################
################################################################################
## Combine plots and export
out=(artproc_plot|artsurf_plot)/(basal_plot|otolith_plot)
ggsave(plot=out,"mortality/mortality.png",width=10,height=10,units="in",bg="white")

############################################################################################
############################################################################################
## Dummy variable regression to see if mortality slopes differ
## Ogle IFAR page 210

## Add structure to each data frame
ap_dvr=artproc_plotdat%>%
  select(age,ln_n,struc)

as_dvr=artsurf_plotdat%>%
  select(age,ln_n,struc)

br_dvr=basal_plotdat%>%
  select(age,ln_n,struc)

lo_dvr=otolith_plotdat%>%
  select(age,ln_n,struc)

## Combine into single data frame
dvr_dat=bind_rows(ap_dvr,as_dvr,br_dvr,lo_dvr)%>%
  filter(age>=2)

## Fit regression
lm_cc=lm(ln_n~age*struc,data=dvr_dat)
Anova(lm_cc)