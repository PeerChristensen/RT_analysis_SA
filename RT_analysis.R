# ANALYSIS OF IMPLICIT ASSOCIATIONS RT TASK
# PEER CHRISTENSEN
# STELLENBOSCH, MAY 2018

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

########## ACCURACY ################################################
#overall accuracy
df %>% tabyl(accuracy)
#by condition and congruency
df %>% summarySE(measurevar="accuracy",groupvars=c("condition","congruent"))

df %>% ggplot(aes(x = sound, y = accuracy, fill=congruent)) +
  stat_summary(geom = "bar", alpha=.8, fun.data = mean_cl_normal,colour="black",size=0.2,position='dodge', width=0.75) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_normal, width = 0.2,position= position_dodge(0.75)) +
  facet_wrap(~condition) +
  scale_fill_viridis(option="D",discrete=T,begin=0.1,end=1) +
  theme_apa(legend.use.title =T, legend.pos="bottom") 

fit1 =glmer(factor(accuracy) ~ sound+condition+congruent+
              trialCount+
              (1+condition|participant) + (1|item),data=df,family="binomial")
summary(fit1)

# accuracy ~ RT
df %>% ggplot(aes(x=accuracy,y=RT,fill=factor(accuracy))) +
  geom_boxplot(aes(group=accuracy),alpha=.9) +
  facet_grid(sound~condition+congruent) +
  scale_fill_viridis(option="D",discrete=T,begin=.1) +
  theme_apa(legend.use.title =T)

####### REACTION TIME #######################################

df= df %>% filter(accuracy==1)
#overview
react = df %>% 
  group_by(condition,congruent,sound) %>%
  dplyr::summarise(n=n(),
                   median = median(RT),
                   mean = mean(RT),
                   sd = sd(RT),
                   mu=mexgauss(RT)[1],
                   sigma = mexgauss(RT)[2],
                   tau = mexgauss(RT)[3])
react

soundReact = df %>%
  group_by(sound,condition) %>%
  dplyr::summarise(n=n(),
                   median = median(RT),
                   mean = mean(RT),
                   sd = sd(RT),
                   mu=mexgauss(RT)[1],
                   sigma = mexgauss(RT)[2],
                   tau = mexgauss(RT)[3])
soundReact

df %>% summarySE(measurevar="RT",groupvars=c("condition","congruent"))

df %>%
  ggplot(aes(x = sound, y = RT, fill=congruent)) +
  stat_summary(geom = "bar",alpha=.8, fun.data = mean_cl_boot,colour="black",size=0.2,position='dodge', width=0.75) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width = 0.2,position= position_dodge(0.75)) +
  facet_wrap(~condition) +
  scale_fill_viridis(option="D",discrete=T,begin=0.1,end=1) +
  coord_cartesian(ylim=c(200,900)) +
  theme_apa(legend.use.title =T, legend.pos="bottom")

#RT smooths by condition, congruency and block
RTs2 = df %>% group_by(condition,block,sound) %>% dplyr::summarise(mean = mean(RT))
RTs2 %>% ggplot(aes(x=block,y=mean,fill=condition)) + 
  geom_jitter() + geom_smooth(colour="snow",alpha=.6) + 
  facet_grid(sound~condition) +
  theme_apa(legend.use.title =T, legend.pos="bottom") +
  scale_fill_viridis_d()

#boxplot: RT ~ condition
df %>%
  ggplot(aes(x=condition,y=RT,fill=condition)) +
  geom_jitter(alpha=.5) + 
  geom_boxplot(alpha=.6,colour="snow") +
  theme_apa() +
  scale_fill_viridis_d() +
  theme(legend.position = "none")

##boxplot: RT ~ congruency + sound + condition
df %>%
  ggplot(aes(x=congruent,y=RT,fill=sound)) +
  geom_jitter(alpha=.5) +
  geom_boxplot(alpha=.6,colour="snow") +
  scale_fill_viridis_d() +
  facet_wrap(~condition) + 
  theme_apa(legend.use.title =T, legend.pos="bottom") 

# distributions
df %>% ggplot(aes(x=RT, fill=sound)) +
  geom_density(alpha=.5) +
  scale_fill_viridis_d() +
  facet_grid(congruent~condition) +
  #facet_grid(~reversed) +
  theme_apa(legend.use.title =T, legend.pos = "bottom") 

#test
m0=lmer(RT~1+(1|participant),data=df)
m1_a=lmer(RTinv~sound+(1|participant) + (1|item),data=df)
summary(m1_a) # slower RT when sound is "low"

m1_b=lmer(RT~condition+(1|participant),data=df) #ns
summary(m1_b)
m1_c=lmer(RT~congruent+(1|participant),data=df) #ns
summary(m1_c)

anova(m0,m1_a) #m1_a best

m2_a = lmer(RT~sound+condition+(1|participant),data=df) 
summary(m2_a)
anova(m1_a,m2_a) #m1 best

m2_b = lmer(RT~sound+congruent+(1|participant),data=df)
anova(m1_a,m2_b) #m1 best

m3_a = lmer(RT~sound*congruent+(1|participant),data=df) #ns
m3_b = lmer(RT~sound*condition+(1|participant),data=df) #ns

m4 = lmer(RT~sound*congruent*condition+(1|participant),data=df)
summary(m4)
anova(m1_a,m4) #m1 slightly better, p = 0.05047

plot_model(m4, type = "int")

#diagnostic plots
plot_model(m1_a, type = "diag") #output looks identical to m1_a

dodge <- position_dodge(width=0.1)  
df %>% summarySE(measurevar="RT",groupvars=c("condition","congruent","sound")) %>%
  ggplot() +
  aes(x = congruent, y = RT, color = sound) +
  geom_line(aes(group = sound),size=1,position = dodge) +
  geom_point(position = dodge) +
  geom_errorbar(aes(ymin=RT-ci, ymax = RT+ci),width=.3,position = dodge) +
  facet_grid(~condition) +
  scale_color_viridis(discrete=T,begin=.2,end=1) +
  coord_cartesian(ylim=c(600, 800)) +
  theme_apa(legend.use.title =T)

dodge <- position_dodge(width=0.1)  
df %>% summarySE(measurevar="RT",groupvars=c("condition","congruent","sound")) %>%
  ggplot() +
  aes(x = condition, y = RT, color = sound) +
  geom_line(aes(group = sound),size=1,position = dodge) +
  geom_point(position = dodge) +
  geom_errorbar(aes(ymin=RT-ci, ymax = RT+ci),width=.3,position = dodge) +
  facet_grid(~congruent) +
  scale_color_viridis(discrete=T,begin=.2,end=1) +
  coord_cartesian(ylim=c(600, 800)) +
  theme_apa(legend.use.title =T)

# per participant distribution by condition and sound
df %>% ggplot(aes(x = RT, y = reorder(participant,RT),fill=sound)) +
  geom_density_ridges(rel_min_height = 0.005,size=0.2) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_apa(legend.use.title =T) +
  scale_fill_viridis(discrete=T,begin=.1,end=1, alpha=.6) +
  facet_grid(~condition)

#by block - sound
df %>% ggplot(aes(x = RT, y = reorder(block,-as.numeric(block)),fill=sound)) +
  geom_density_ridges(rel_min_height = 0.005,size=0.2) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_apa(legend.use.title =T) +
  scale_fill_viridis(discrete=T,begin=.1,end=1, alpha=.6) +
  facet_grid(~condition)

#by block - congruency
df %>% ggplot(aes(x = RT, y = reorder(block,-as.numeric(block)),fill=congruent)) +
  geom_density_ridges(rel_min_height = 0.005,size=0.2) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_apa(legend.use.title =T) +
  scale_fill_viridis(discrete=T,begin=.1,end=1, alpha=.6) +
  facet_grid(~condition)

#by trial
df %>% ggplot(aes(x = RT, y = reorder(factor(trial),-trial),fill=sound)) +
  geom_density_ridges(rel_min_height = 0.005,size=0.2) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_apa(legend.use.title =T) +
  scale_fill_viridis_d(begin=.1,end=1, alpha=.6) +
  facet_grid(~condition)


# UNUSED
#df=tibble()

#for (file in files){
#  p = read_csv2(file)
#  df = rbind(df,p)
#}



