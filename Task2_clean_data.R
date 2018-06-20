# IMPLICIT ASSOCIATIONS RT TASK: SCRIPT FOR DATA CLEANING
# PEER CHRISTENSEN
# DATA FROM STELLENBOSCH, MAY 2018

####### LOAD PACKAGES ###################################################
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

###### LOAD DATA ####################################################
files = list.files("RT_logs_stellenbosch",pattern="*.csv",full.names = T)

df = files %>%
  map(read_csv2) %>%
  reduce(rbind)

####### ADD / CHANGE VARIABLES ##################################################
df$RT = as.numeric(df$RT) * 1000
df$participant = factor(df$participant)
df$condition = factor(df$condition)
df$sound=factor(df$sound)
df$logRT=log(df$RT)
df$RTinv=1/df$RT
d_table=data.table(df)
d_table[ , RTlag := shift(RT)]
df=as.tibble(d_table)
df = df %>% 
  group_by(participant) %>% 
  dplyr::mutate(trialCount = row_number(),
                accuracy = ifelse((sound == "high" & key == "k") |
                                    (sound == "low" & key == "s"), 1, 0),
                congruent = factor(ifelse(((visual=="high" | visual =="small")  & sound=="high") |
                                            ((visual=="low" | visual =="big") & sound=="low"),T,F)),
                item = paste0(visual,sound))

# several participants appear to have reversed their key presses for high/low 
# upon inspection of overall accuracy
prop.table(table(df$accuracy,df$participant),2)
reversed = c("F24","F31","F34","F35","F39","F43","M31","M36","M42","M44")
df$accuracy[df$participant %in% reversed] = recode(df$accuracy[df$participant %in% reversed],"0"="1","1"="0") %>%
  as.numeric()

####### INSPECT DISTRIBUTIONS / REMOVE OUTLIERs : RT ###############################
distDens = ggplot(df,aes(RT)) + 
  geom_density() +
  facet_wrap(~condition)

distBox = ggplot(df,aes(x = condition, y=RT)) + 
  geom_jitter(alpha=.3) + 
  geom_boxplot(alpha =.5) 

grid.arrange(distDens,distBox,ncol=2)
# given the task, some RT values are unlikely.
# zooming in between 0-5000 msec
# It seems reasonable to set an absolute upper RT cut-off at 5000 msec, lower at 200 msec

distDensZoom = ggplot(df,aes(RT)) + 
  geom_density() +
  geom_vline(xintercept = 3000, colour = "blue", linetype = "dashed", size=1) +
  facet_wrap(~condition) +
  coord_cartesian(xlim = c(0,5000))

distBoxZoom = ggplot(df,aes(x=condition,y=RT)) + 
  geom_jitter(alpha=.3) + geom_boxplot(alpha =.5) + 
  geom_hline(yintercept = 3000, colour = "blue", linetype = "dashed", size = 1) +
  coord_cartesian(ylim = c(0,5000)) 

grid.arrange(distDensZoom,distBoxZoom,ncol=2)

#  data points before removing outliers
before = nrow(df) 
before

limits = df %>%
  filter(accuracy == "1") %>%
  ddply(.(participant), summarise, m = mean(RT), limit = mean(RT) + (2.5*sd(RT)))
limits

df = df %>% 
  filter(RT>200, RT<5000) %>%
  group_by(participant) %>%
  inner_join(limits, by = "participant") %>%
  filter(RT <= limit)


after = nrow(df)
after
n_removed = before-after
n_removed 
pct_removed = 100-(after/before *100)
pct_removed
# 3.4% exclusion

####### INSPECT DISTRIBUTIONS / REMOVE OUTLIERs : ACCURACY ###############################

# overall accuracy per participant
overall_accuracy = df %>% 
  tabyl(accuracy,participant) %>% 
  adorn_percentages("col") %>%
  filter(accuracy==1) %>% gather()

accuracyDens = overall_accuracy %>% 
  ggplot(aes(x=value)) + geom_density()
accuracyBox = overall_accuracy%>% 
  ggplot(aes(y=value)) + geom_boxplot()

grid.arrange(accuracyDens,accuracyBox, ncol = 2)

#What should be the lowest possible accuracy?
df=df %>% filter(!participant %in% c("F41")) # chance , # <.8 also "F37","M33","M37"
df=droplevels(df)

after2 = nrow(df)
n_removed = before - after2
n_removed
total_pct_removed = 100-(after2/before* 100)
total_pct_removed 
# 6.63 percent of data removed

##### WRITE NEW DATA FILE ###############################################
write_csv(df, "RT_task2_CLEAN")
