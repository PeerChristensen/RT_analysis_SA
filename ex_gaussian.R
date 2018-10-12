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
library(grid)
library(gamlss)

df <- read_csv("RT_Task2_TRIMMED")
# ##########################################################
# #overall accuracy
# df %>% tabyl(accuracy)
# #by condition and congruency
# df %>% tabyl(accuracy,congruent,condition) %>% adorn_percentages("col")
# df %>% summarySE(measurevar="accuracy",groupvars=c("condition","congruent"))
# 
# df %>% ggplot(aes(x = sound, y = accuracy, fill=congruent)) +
#   stat_summary(geom = "bar", alpha=.8, fun.data = mean_cl_normal,colour="black",size=0.2,position='dodge', width=0.75) +
#   stat_summary(geom = "errorbar", fun.data = mean_cl_normal, width = 0.2,position= position_dodge(0.75)) +
#   facet_wrap(~condition) +
#   scale_fill_viridis(option="D",discrete=T,begin=0.1,end=1) +
#   theme_apa(legend.use.title =T, legend.pos="bottom") +
#   coord_cartesian(ylim=0.75,1)
# 
# fit1 =glmer(factor(accuracy) ~ sound*condition*congruent+(1|participant),data=df,family="binomial")
# summary(fit1)
# 
# #accuracy + RT
# df %>% ggplot(aes(x=factor(accuracy),y=RT,fill=factor(accuracy))) +
#   geom_boxplot(aes(group=accuracy),alpha=.9) +
#   facet_grid(sound~condition+congruent) +
#   scale_fill_viridis(option="D",discrete=T,begin=.1) +
#   theme_apa(legend.use.title =T)

##########################################################
#reaction time

#overview
summary <- df %>% 
  group_by(language,condition,congruent,sound,participant) %>%
  dplyr::summarise(n=n(),
                   mean = mean(RT),
                   sd = sd(RT),
                   mu=mexgauss(RT)[1],
                   sigma = mexgauss(RT)[2],
                   tau = mexgauss(RT)[3])
summary

params <- summary %>% 
  select(-n,-mean,-sd) %>% 
  gather(key = "parameter", value = "value", 6:8)

params %>% 
  ggplot(aes(x=value,fill=parameter)) + 
  geom_density(alpha=.7) +
  scale_fill_stellenbosch() +
  facet_grid(language~condition + congruent + sound)

# fit data to ex-gaussian distribution

# all participants
df %>% ggplot(aes(RT)) + 
  geom_density()

# individual participants
af_density <- df %>% 
  filter(language=="Af") %>%
  ggplot(aes(x=RT, y = reorder(participant,RT), fill = language)) +
  geom_density_ridges(alpha=.7,rel_min_height = 0.005, show.legend = F,size=0.2) +
  scale_fill_stellenbosch("main",reverse =T) +
  xlim(c=0,2000) +
  labs(y ="Participant\n") +
  theme_ridges() +
  ggtitle("Afrikaans")

xh_density <- df %>% 
  filter(language=="Xh") %>% 
  ggplot(aes(x=RT, y = reorder(participant,RT),fill=language)) +
  geom_density_ridges(alpha=.7,rel_min_height = 0.005,show.legend=F,size=0.2) +
  scale_fill_stellenbosch("wine") +
  xlim(c=0,2000) +
  labs(y =NULL) +
  theme_ridges() +
  ggtitle("Xhosa")

grid.arrange(af_density,xh_density,ncol=2,
             top = grid::textGrob("by-participant RT distributions",
                                  gp=gpar(fontsize=20)))

distr <- df %>% 
  group_by(participant) %>%
  dplyr::summarise(n=n(),
                   mean = mean(RT),
                   sd = sd(RT),
                   mu=mexgauss(RT)[1],
                   sigma = mexgauss(RT)[2],
                   tau = mexgauss(RT)[3])

list_rand_rt = list()
for (i in seq(1:nrow(distr))) {
  
  n     = as_vector(distr[i,2])
  mu    = as_vector(distr[i,5])
  sigma = as_vector(distr[i,6])
  nu    = as_vector(distr[i,7]) # tau
  
  ls    = list(rexGAUS(n=n,mu=mu,sigma=sigma,nu=nu))
  list_rand_rt[i] = ls
}

d <- data.frame(participant = unique(df$participant),
                p_number    = row_number(unique(df$participant)))

df %<>% full_join(d)

list_real_rt <- df %>% 
  dplyr::select(p_number,RT) %>%
  split(.$p_number)

ks_tests = list()
for (i in unique(df$p_number)){
  
  ks_tests[[i]] = data.frame(
    real_rt = as_vector(list_real_rt[[i]][2]),
    sim_rt = as_vector(list_rand_rt[i]))
}

ks_results = data.frame()
for (i in unique(d$p_number)) {
  participant = d$participant[i]
  x = ks_tests[[i]]$real_rt
  y = ks_tests[[i]]$sim_rt
  
  result = ks.test(x,y)$p.value
  ks_results = rbind(ks_results,cbind(participant,result))
}

exgauss_fit <- ks_results %>% 
  ggplot(aes(x=participant,y=result)) + 
  geom_point() + 
  geom_hline(yintercept = 0.05,
             colour = stellenbosch_colours[5]) +
  ylim(0,1)

# fit to normal distribution

list_rand_rt_norm = list()
for (i in seq(1:nrow(distr))) {
  
  n     = as_vector(distr[i,2])
  mean    = as_vector(distr[i,3])
  sd = as_vector(distr[i,4])
  
  ls    = list(rnorm(n=n,mean=mean,sd=sd))
  list_rand_rt_norm[i] = ls
}

ks_tests_norm = list()
for (i in unique(df$p_number)){
  
  ks_tests_norm[[i]] = data.frame(
    real_rt = as_vector(list_real_rt[[i]][2]),
    sim_rt = as_vector(list_rand_rt_norm[i]))
}

ks_results_norm = data.frame()
for (i in unique(d$p_number)) {
  participant = d$participant[i]
  x = ks_tests_norm[[i]]$real_rt
  y = ks_tests_norm[[i]]$sim_rt
  
  result = ks.test(x,y)$p.value
  ks_results_norm = rbind(ks_results_norm,cbind(participant,result))
}

norm_fit <- ks_results_norm %>% 
  ggplot(aes(participant,y=result)) + 
  geom_point() + 
  geom_hline(yintercept = 0.05,
             colour = stellenbosch_colours[5]) +
  ylim(0,1)

grid.arrange(exgauss_fit,norm_fit,ncol=2)

# regression models

param=params[params$parameter=="tau",]
m=lm(value~language+congruent,data=param)
summary(m)

plot_model(m, type="diag")
               