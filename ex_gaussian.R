# ANALYSIS OF IMPLICIT ASSOCIATIONS RT TASK - exgaussian
# PEER CHRISTENSEN
# STELLENBOSCH, MAY 2018

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

##########################################################
#load and prepare data

files = list.files("RT_logs_stellenbosch",pattern="*.csv",full.names = T)

df = files %>%
  map(read_csv2) %>%
  reduce(rbind)

#sort by accuracy
df$RT = as.numeric(df$RT)
df$participant = factor(df$participant)
df$condition = factor(df$condition)
df$sound=factor(df$sound)
df$logRT=log(df$RT)
df$RTinv=1/df$RT

df = df %>% 
  filter(RT>=.2, RT<2) %>%
  #filter(RT < mean(df$RT) + 2.5*sd(df$RT)) %>%
  mutate(RT = RT * 1000,
         accuracy = ifelse((sound == "high" & key == "k") |
                             (sound == "low" & key == "s"), 1, 0),
         congruent = factor(ifelse(((visual=="high" | visual =="small")  & sound=="high") |
                                     ((visual=="low" | visual =="big") & sound=="low"),T,F)))


#several participants appear to have reversed their key presses for high/low 
prop.table(table(df$accuracy,df$participant),2)

reversed=c("F24","F31","F34","F35","F39","F43","M31","M36","M42","M44")
df$reversed = ifelse(df$participant %in% reversed,T,F)

df$accuracy[as.character(df$participant) %in% reversed] = recode(df$accuracy[as.character(df$participant) %in% reversed],"0"="1","1"="0") %>% as.numeric()

#mean accuracy distribution
acc = df %>% tabyl(accuracy,participant) %>% adorn_percentages("col") %>%
  filter(accuracy==1) 
acc = data.frame(t(acc))
acc

a1 = acc %>% ggplot(aes(y=t.acc.)) + geom_boxplot()
a2 = acc %>% ggplot(aes(x=t.acc.)) + geom_density()
grid.arrange(a1,a2, ncol = 2)

#What should be the lowest possible accuracy?
df=df %>% filter(!participant %in% c("F41")) # chance , # <.8 also "F37","M33","M37"
df=droplevels(df)

##########################################################
#overall accuracy
df %>% tabyl(accuracy)
#by condition and congruency
df %>% tabyl(accuracy,congruent,condition) %>% adorn_percentages("col")
df %>% summarySE(measurevar="accuracy",groupvars=c("condition","congruent"))

df %>% ggplot(aes(x = sound, y = accuracy, fill=congruent)) +
  stat_summary(geom = "bar", alpha=.8, fun.data = mean_cl_normal,colour="black",size=0.2,position='dodge', width=0.75) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_normal, width = 0.2,position= position_dodge(0.75)) +
  facet_wrap(~condition) +
  scale_fill_viridis(option="D",discrete=T,begin=0.1,end=1) +
  theme_apa(legend.use.title =T, legend.pos="bottom") +
  coord_cartesian(ylim=0.75,1)

fit1 =glmer(factor(accuracy) ~ sound*condition*congruent+(1|participant),data=df,family="binomial")
summary(fit1)

#accuracy + RT
df %>% ggplot(aes(x=factor(accuracy),y=RT,fill=factor(accuracy))) +
  geom_boxplot(aes(group=accuracy),alpha=.9) +
  facet_grid(sound~condition+congruent) +
  scale_fill_viridis(option="D",discrete=T,begin=.1) +
  theme_apa(legend.use.title =T)

##########################################################
#reaction time

df= df %>% filter(accuracy==1)
#overview
react = df %>% 
  group_by(condition,congruent,sound,participant) %>%
  dplyr::summarise(n=n(),
                   mean = mean(RT),
                   sd = sd(RT),
                   mu=mexgauss(RT)[1],
                   sigma = mexgauss(RT)[2],
                   tau = mexgauss(RT)[3])
react

params = react %>% 
  select(-n,-mean,-sd) %>% 
  gather(key = "parameter", value = "value", 5:7)

params %>% 
  ggplot(aes(x=value,fill=parameter)) + 
  geom_density() +
  scale_fill_viridis_d(option="A",alpha=.7) +
  facet_wrap(~condition)

params %>% 
  ggplot(aes(x=value,fill=parameter)) + 
  geom_density() +
  scale_fill_viridis_d(option="A",alpha=.7) +
  facet_wrap(~congruent)

params %>% 
  ggplot(aes(x=value,fill=parameter)) + 
  geom_density() +
  scale_fill_viridis_d(option="A",alpha=.7) +
  facet_wrap(~sound)

param=params[params$parameter=="mu",]
m=lmer(value~sound+condition+congruent+(1|participant),data=param)
summary(m)
plot_model(m, type="diag")
               