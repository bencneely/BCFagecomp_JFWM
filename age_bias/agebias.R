#---------------------------------------------------------------
#Jeff Koch
#01/09/2024
#Analysis for BCF age structure comparisons
#---------------------------------------------------------------

#Clear R
cat("\014")
rm(list=ls())

## Set seed for reproducibility
set.seed(109)

#Install packages
library(rio)
library(tidyverse)
library(FSA)

#Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/JFWM analysis/age bias/")

#Read in data with import
dat=import("abdat.csv")

#calculate agreement and CV with agePrecision function
ap.procsurf=agePrecision(~artsurf+artproc,data=dat)
  A=summary(ap.procsurf,what="precision")%>%
    mutate(comparison="Process vs Surface")

ap.procbasal=agePrecision(~basal+artproc,data=dat)
  B=summary(ap.procbasal,what="precision")%>%
    mutate(comparison="Process vs Basal")
  
ap.procotolith=agePrecision(~otolith+artproc,data=dat)
  C=summary(ap.procotolith,what="precision")%>%
    mutate(comparison="Process vs Otolith")
  
ap.surfbasal=agePrecision(~basal+artsurf,data=dat)
  D=summary(ap.surfbasal,what="precision")%>%
    mutate(comparison="Surface vs Basal")
  
ap.surfotolith=agePrecision(~otolith+artsurf,data=dat)
  E=summary(ap.surfotolith,what="precision")%>%
    mutate(comparison="Surface vs Otolith")
  
ap.basalotolith=agePrecision(~otolith+basal,data=dat)
  G=summary(ap.basalotolith,what="precision")%>%
    mutate(comparison="Basal vs Otolith")

#bind those together and export  
all=rbind(A,B,C,D,E,G)
#export(all,"allprecision.csv")

#create age bias plots
procsurf=ageBias(artsurf~artproc,data=dat,nref.lab="Articulating Surface Age",ref.lab="Articulating Process Age")
png("procsurf.png",width=5,height=5,units="in",res=600)
plotAB(procsurf)
procsurf1=plotAB(procsurf,col.CIsig="gray60",col.agree="black",xlim=c(0,20),ylim=c(0,20),yaxt="n")
axis(2,las=2)
text(x=0.5, y=18.5,"CV=15.4\nPA=21.3",adj=0,cex=1.2)
dev.off()

procbasal=ageBias(basal~artproc,data=dat,nref.lab="Basal Section Age",ref.lab="Articulating Process Age")
png("procbasal.png",width=5,height=5,units="in",res=600)
plotAB(procbasal)
procbasal1=plotAB(procbasal,col.CIsig="gray60",col.agree="black",xlim=c(0,20),ylim=c(0,20),yaxt="n")
axis(2,las=2)
text(x=0.5, y=18.5,"CV=12.6\nPA=27.6",adj=0,cex=1.2)
dev.off()

procotolith=ageBias(otolith~artproc,data=dat,nref.lab="Otolith Section Age",ref.lab="Articulating Process Age")
png("procotolith.png",width=5,height=5,units="in",res=600)
plotAB(procotolith)
procotolith1=plotAB(procotolith,col.CIsig="gray60",col.agree="black",xlim=c(0,20),ylim=c(0,20),yaxt="n")
axis(2,las=2)
text(x=0.5, y=18.5,"CV=16.1\nPA=25.9",adj=0,cex=1.2)
dev.off()

surfbasal=ageBias(basal~artsurf,data=dat,nref.lab="Basal Section Age",ref.lab="Articulating Surface Age")
png("surfbasal.png",width=5,height=5,units="in",res=600)
plotAB(surfbasal)
surfbasal1=plotAB(surfbasal,col.CIsig="gray60",col.agree="black",xlim=c(0,20),ylim=c(0,20),yaxt="n")
axis(2,las=2)
text(x=0.5, y=18.5,"CV=6.0\nPA=50.0",adj=0,cex=1.2)
dev.off()

surfotolith=ageBias(otolith~artsurf,data=dat,nref.lab="Otolith Section Age",ref.lab="Articulating Surface Age")
png("surfotolith.png",width=5,height=5,units="in",res=600)
plotAB(surfotolith)
surfotolith1=plotAB(surfotolith,col.CIsig="gray60",col.agree="black",xlim=c(0,20),ylim=c(0,20),yaxt="n")
axis(2,las=2)
text(x=0.5, y=18.5,"CV=9.7\nPA=37.6",adj=0,cex=1.2)
dev.off()

basalotolith=ageBias(otolith~basal,data=dat,nref.lab="Otolith Section Age",ref.lab="Basal Section Age")
png("basalotolith.png",width=5,height=5,units="in",res=600)
plotAB(basalotolith)
basalotolith1=plotAB(basalotolith,col.CIsig="gray60",col.agree="black",xlim=c(0,20),ylim=c(0,20),yaxt="n")
axis(2,las=2)
text(x=0.5, y=18.5,"CV=10.7\nPA=38.1",adj=0,cex=1.2)
dev.off()
