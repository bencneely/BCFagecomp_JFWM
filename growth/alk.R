#---------------------------------------------------------------------------------------
#Ben Neely
#01/09/2024
#Create ALK for MILR BCF using four structures and assign ages to sample
#---------------------------------------------------------------------------------------

## Clear R
cat("\014")  
rm(list=ls())

## Set seed for reproducibility
set.seed(109)

## Install and load packages
## Checks if package is installed, installs if not, activates for current session
if("FSA" %in% rownames(installed.packages()) == FALSE) {install.packages("FSA")}
library(FSA)

if("rio" %in% rownames(installed.packages()) == FALSE) {install.packages("rio")}
library(rio)

if("nnet" %in% rownames(installed.packages()) == FALSE) {install.packages("nnet")}
library(nnet)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/JFWM analysis/")

## Read in data with import
aged=import("growth/agedfish.csv")%>%
  mutate(cmgroup=lencat(tl,10))

unaged=import("growth/unagedfish.csv")%>%
  mutate(age=as.numeric(NA))

## Separate out ages by structure
aged_artproc=filter(aged,structure=="artproc")
aged_artsurf=filter(aged,structure=="artsurf")
aged_basal=filter(aged,structure=="basal")
aged_otolith=filter(aged,structure=="otolith")

############################################################################################
############################################################################################
## Create age-length key from artproc data using multinomial logistic regression model
artproc_mlr=multinom(age~cmgroup,data=aged_artproc)

## Predict probability that a fish in a given cm group is a certain age
lens=seq(100,1200,10)
artproc_alk=predict(artproc_mlr,data.frame(cmgroup=lens),type="probs")
row.names(artproc_alk)=lens
round(artproc_alk,3)

## Apply ALK to unaged data set and combine with aged fish
artproc_modage=alkIndivAge(artproc_alk,age~cmgroup,data=unaged)
artproc_out=bind_rows(artproc_modage,aged_artproc)%>%
  select(id,cmgroup,age)%>%
  mutate(struc="artproc")%>%
  drop_na()

############################################################################################
############################################################################################
## Create age-length key from artsurf data using multinomial logistic regression model
artsurf_mlr=multinom(age~cmgroup,data=aged_artsurf)

## Predict probability that a fish in a given cm group is a certain age
lens=seq(100,1200,10)
artsurf_alk=predict(artsurf_mlr,data.frame(cmgroup=lens),type="probs")
row.names(artsurf_alk)=lens
round(artsurf_alk,3)

## Apply ALK to unaged data set and combine with aged fish
artsurf_modage=alkIndivAge(artsurf_alk,age~cmgroup,data=unaged)
artsurf_out=bind_rows(artsurf_modage,aged_artsurf)%>%
  select(id,cmgroup,age)%>%
  mutate(struc="artsurf")%>%
  drop_na()

############################################################################################
############################################################################################
## Create age-length key from basal data using multinomial logistic regression model
basal_mlr=multinom(age~cmgroup,data=aged_basal)

## Predict probability that a fish in a given cm group is a certain age
lens=seq(100,1200,10)
basal_alk=predict(basal_mlr,data.frame(cmgroup=lens),type="probs")
row.names(basal_alk)=lens
round(basal_alk,3)

## Apply ALK to unaged data set and combine with aged fish
basal_modage=alkIndivAge(basal_alk,age~cmgroup,data=unaged)
basal_out=bind_rows(basal_modage,aged_basal)%>%
  select(id,cmgroup,age)%>%
  mutate(struc="basal")%>%
  drop_na()

############################################################################################
############################################################################################
## Create age-length key from otolith data using multinomial logistic regression model
otolith_mlr=multinom(age~cmgroup,data=aged_otolith)

## Predict probability that a fish in a given cm group is a certain age
lens=seq(100,1200,10)
otolith_alk=predict(otolith_mlr,data.frame(cmgroup=lens),type="probs")
row.names(otolith_alk)=lens
round(otolith_alk,3)

## Apply ALK to unaged data set and combine with aged fish
otolith_modage=alkIndivAge(otolith_alk,age~cmgroup,data=unaged)
otolith_out=bind_rows(otolith_modage,aged_otolith)%>%
  select(id,cmgroup,age)%>%
  mutate(struc="otolith")%>%
  drop_na()

############################################################################################
############################################################################################
## Combine and export data
out=bind_rows(artproc_out,artsurf_out,basal_out,otolith_out)
export(out,"allaged.csv")
