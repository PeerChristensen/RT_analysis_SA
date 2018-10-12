# IMPLICIT ASSOCIATIONS RT TASK: EX_GAUSSIAN ANALYSIS
# PEER CHRISTENSEN
# DATA FROM STELLENBOSCH, MAY 2018
# OCTOBER 2018

# VISUALISE EX-GAUSSIAN DISTRIBUTIONS
# TEST GOODNESS OF FIT
# MODEL

# -------------------------------------------
####### LOAD PACKAGES AND DATA ##############

library(tidyverse)
library(ggridges)
library(stellenbosch)
library(retimes)
library(gamlss)
library(gridExtra)
library(grid)
library(sjPlot)
library(magrittr)

df <- read_csv("datafile_task2.csv")

theme_set(theme_bw())

# -------------------------------------------
####### ESTIMATE AND VISUALISE PARAMETERS ###

params <- df %>% 
  group_by(language,condition,congruent,participant,sound) %>%
  dplyr::summarise(n        = n(),
                   mean     = mean(RT),
                   sd       = sd(RT),
                   coef_var = sd(RT)/mean(RT),
                   mu       = mexgauss(RT)[1],
                   sigma    = mexgauss(RT)[2],
                   tau      = mexgauss(RT)[3]) %>%
  ungroup()

params_ex <- params                     %>%
  dplyr::select(-n,-mean,-sd,-coef_var) %>%
  gather(key = "parameter", value = "value", 6:8)

params_ex %>% 
  ggplot(aes(x=value,fill=parameter)) + 
  geom_density(alpha=.7) +
  scale_fill_stellenbosch() +
  facet_grid(language~condition + congruent)

# -------------------------------------------
####### VISUALISE RT DISTRIBUTIONS ##########

af_density <- df         %>% 
  filter(language=="Afrikaans") %>%
  ggplot(aes(x=RT, y = reorder(participant,RT), fill = language)) +
  geom_density_ridges(alpha          = .7,
                      rel_min_height = 0.005, 
                      show.legend    = F,
                      size           = 0.2) +
  scale_fill_stellenbosch("main",reverse =T) +
  xlim(c = 0,2000) +
  labs(y = "Participant\n") +
  ggtitle("Afrikaans")

xh_density <- df %>% 
  filter(language=="Xhosa") %>% 
  ggplot(aes(x=RT, y = reorder(participant,RT),fill=language)) +
  geom_density_ridges(alpha          = .7,
                      rel_min_height = 0.005,
                      show.legend    = F,
                      size           = 0.2) +
  scale_fill_stellenbosch("wine") +
  xlim(c = 0,2000) +
  labs(y = NULL) +
  ggtitle("Xhosa")

grid.arrange(af_density,xh_density,ncol=2,
             top = grid::textGrob("by-participant RT distributions",
                                  gp=gpar(fontsize=20)))

# -------------------------------------------
####### FIT DATA TO DISTRIBUTIONS ###########

### 1. EX_GAUSSIAN

distr <- df %>% 
  group_by(participant) %>%
  dplyr::summarise(n     = n(),
                   mean  = mean(RT),
                   sd    = sd(RT),
                   mu    = mexgauss(RT)[1],
                   sigma = mexgauss(RT)[2],
                   tau   = mexgauss(RT)[3])

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

list_real_rt <- df           %>% 
  dplyr::select(p_number,RT) %>%
  split(.$p_number)

ks_tests = list()
for (i in unique(df$p_number)){
  
  ks_tests[[i]] = data.frame(
    real_rt     = as_vector(list_real_rt[[i]][2]),
    sim_rt      = as_vector(list_rand_rt[i]))
}

ks_results = data.frame()
for (i in unique(d$p_number)) {
  participant = d$participant[i]
  x           = ks_tests[[i]]$real_rt
  y           = ks_tests[[i]]$sim_rt
  
  `P-values` = ks.test(x,y)$p.value
  ks_results = rbind(ks_results,cbind(participant,`P-values`))
}

exgauss_fit <- ks_results %>% 
  ggplot(aes(x=participant,y=`P-values`)) + 
  geom_point() + 
  geom_hline(yintercept = 0.05,
             colour = stellenbosch_colours[5]) +
  ylim(0,1) +
  ggtitle("K-S tests for Ex-Gaussian distributions")

### 2. NORMAL DISTRIBUTION

list_rand_rt_norm = list()
for (i in seq(1:nrow(distr))) {
  
  n    = as_vector(distr[i,2])
  mean = as_vector(distr[i,3])
  sd   = as_vector(distr[i,4])
  
  ls   = list(rnorm(n=n,mean=mean,sd=sd))
  list_rand_rt_norm[i] = ls
}

ks_tests_norm = list()
for (i in unique(df$p_number)){
  
  ks_tests_norm[[i]] = data.frame(
    real_rt = as_vector(list_real_rt[[i]][2]),
    sim_rt  = as_vector(list_rand_rt_norm[i]))
}

ks_results_norm = data.frame()
for (i in unique(d$p_number)) {
  participant = d$participant[i]
  x = ks_tests_norm[[i]]$real_rt
  y = ks_tests_norm[[i]]$sim_rt
  
  `P-values`      = ks.test(x,y)$p.value
  ks_results_norm = rbind(ks_results_norm,cbind(participant,`P-values`))
}

norm_fit <- ks_results_norm %>% 
  ggplot(aes(participant,y=`P-values`)) + 
  geom_point() + 
  geom_hline(yintercept = 0.05,
             colour = stellenbosch_colours[5]) +
  ylim(0,1) +
  labs(y=NULL) +
  ggtitle("K-S tests for normal distributions")

### 3. COMPARE FITS

grid.arrange(exgauss_fit,norm_fit,ncol=2)

# -------------------------------------------
######### BOXPLOTS ##########################

cols = unname(stellenbosch_colours[c("green","shiraz","khaki")])

params_ex %>% ggplot(aes(x=parameter,y=value,fill=language)) + 
  geom_jitter(alpha = .6) +
  geom_boxplot(alpha         = .8,
               outlier.shape = NA,
               colour        = cols[3]) +
  scale_fill_manual(values   = c(cols[1],cols[2])) +
  facet_grid(condition~congruent)

# -------------------------------------------
######### DENSITY PLOTS #####################
params_ex %>%
  ggplot(aes(x=value, y = language,fill=parameter)) +
  geom_density_ridges(alpha          = .7,
                      rel_min_height = 0.005, 
                      show.legend    = T,
                      size           = 0.2) +
  scale_fill_stellenbosch() +
  facet_grid(condition~congruent)

# -------------------------------------------
######### INTERACTION PLOTS #################

dodge <- position_dodge(width=0.1)

mu_plot <- params %>% 
  Rmisc::summarySE(measurevar="mu",groupvars=c("language","condition","congruent")) %>%
  ggplot(aes(x = congruent, y = mu, color = language)) +
  geom_line(aes(group = language),
            size      = 1,
            position  = dodge) +
  geom_point(position = dodge) +
  geom_errorbar(aes(ymin = mu-ci,ymax = mu+ci),
                width    =.3,
                position = dodge) +
  facet_grid(~condition) +
  scale_colour_manual(values=c(cols[1],cols[2])) +
  ylim(c(0,625)) +
  guides(colour=F)

tau_plot <- params %>% 
  Rmisc::summarySE(measurevar="tau",groupvars=c("language","condition","congruent")) %>%
  ggplot(aes(x = congruent, y = tau, color = language)) +
  geom_line(aes(group = language),
            size      = 1,
            position  = dodge) +
  geom_point(position = dodge) +
  geom_errorbar(aes(ymin = tau-ci,ymax = tau+ci),
                width    =.3,
                position = dodge) +
  facet_grid(~condition) +
  scale_colour_manual(values=c(cols[1],cols[2])) +
  ylim(c(0,350)) +
  guides(colour=F)

sigma_plot <- params %>% Rmisc::summarySE(measurevar="sigma",groupvars=c("language","condition","congruent")) %>%
  ggplot(aes(x = congruent, y = sigma, color = language)) +
  geom_line(aes(group = language),
            size      = 1,
            position  = dodge) +
  geom_point(position = dodge) +
  geom_errorbar(aes(ymin = sigma-ci,ymax = sigma+ci),
                width    = .3,
                position = dodge) +
  facet_grid(~condition) +
  scale_colour_manual(values=c(cols[1],cols[2])) +
  ylim(c(0,150)) +
  theme(legend.position="bottom")

grid.arrange(mu_plot,tau_plot,sigma_plot,nrow=2)

# -------------------------------------------
####### REGRESSION MODELS ###################

### MU
mu_mod_0 = lm(mu ~ 1,data=params)
summary(mu_mod_0)

mu_mod_1 = lm(mu ~ language,data=params)
summary(mu_mod_1)

anova(mu_mod_0,mu_mod_1)

mu_mod_2 = aov(mu ~ language+condition,data=params)
summary(mu_mod_2)

anova(mu_mod_1,mu_mod_2)

mu_mod_3 = aov(mu ~ language+congruent,data=params)
summary(mu_mod_3)

anova(mu_mod_1,mu_mod_3)

mu_mod_4 = aov(mu ~ language+language:condition:congruent,data=params)
summary(mu_mod_4)

anova(mu_mod_1,mu_mod_4)

mu_mod_full = lm(mu ~ language*condition*congruent,data=params)
summary(mu_mod_full)

#Xhosa has lower mu

# check model assumptions
plot_model(mu_mod_full, type="slope")
plot_model(mu_mod_full, type="diag")

### TAU
tau_mod_0 = lm(tau ~ 1,data=params)
summary(tau_mod_0)

tau_mod_1 = aov(tau ~ language,data=params)
summary(tau_mod_1)

anova(tau_mod_0,tau_mod_1)

tau_mod_2 = aov(tau ~ language+condition,data=params)
summary(tau_mod_2)

anova(tau_mod_1,tau_mod_2)

tau_mod_3 = aov(tau ~ language+congruent,data=params)
summary(tau_mod_3)

anova(tau_mod_1,tau_mod_3)

tau_mod_4 = aov(tau ~ language+language:condition:congruent,data=params)
summary(tau_mod_4)

tau_mod_full = lm(lm(tau ~ language*condition*congruent,data=params))
summary(tau_mod_full)

anova(tau_mod_1,tau_mod_4)

#Xhosa has larger tau

# check model assumptions
plot_model(tau_mod_full, type="slope")
plot_model(tau_mod_full, type="diag")

### CONFLICT EFFECT

conflict_df <- df %>%
  group_by(language,condition,participant) %>%
  dplyr::summarise(m_incongruent = mean(RT[congruent==FALSE]),
                   m_congruent   = mean(RT[congruent==TRUE]),
                   conflict      = m_incongruent - m_congruent)

conflict_mod_full = lm(lm(conflict ~ language*condition,data=conflict_df))
summary(conflict_mod_full)

### COEFFICIENT OF VARIABILITY

coef_var_mod_full = lm(lm(coef_var ~ language*condition*congruent,data=params))
summary(coef_var_mod_full)


