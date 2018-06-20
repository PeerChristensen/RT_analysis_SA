# HOW TO SUBSET DATA WITH INDIVIDUAL RT CUT-OFFS

####### LOAD PACKAGES AND DATA ###################################################
library(tidyverse)
library(lme4)
library(lmerTest)
library(viridis)
library(janitor)
library(Rmisc)
library(jtools)
library(effects)
library(sjPlot)
library(ggridges)
library(gridExtra)
library(retimes)
library(data.table)

df = read_csv("RT_task2_CLEAN")

limits = df %>%
  filter(accuracy == "1") %>%
  ddply(.(participant), summarise, m = mean(RT), limit = mean(RT) + (2.5*sd(RT)))

new = df %>% 
  filter(accuracy == "1") %>%
  group_by(participant) %>%
  inner_join(limits, by = "participant") #%>%
  #select(-Gender , -RTinv, - age, -RTlag, -logRT, -trialCount)

nrow(df)
nrow(new)
  