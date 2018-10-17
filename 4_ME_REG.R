# IMPLICIT ASSOCIATIONS RT TASK: MIXED-EFFECTS REGRESSION ANALYSIS
# PEER CHRISTENSEN
# DATA FROM STELLENBOSCH, MAY 2018
# OCTOBER 2018

# VISUALISE TRANSFORMATIONS AND TEST GOODNESS OF FIT
# MODEL

# -------------------------------------------
####### LOAD PACKAGES AND DATA ##############

library(tidyverse)
library(stellenbosch)
library(gridExtra)
library(grid)
library(sjPlot)
library(magrittr)
library(lme4)
library(lmerTest)
library(broom)
library(stellenbosch)
library(itsadug)

df <- read_csv("datafile_task2.csv")

theme_set(theme_bw())

# -------------------------------------------
####### COMPARE TRANSFORMATIONS #############

### CALCULATE SKEW, KURTOSIS, NORMALITY

df                        %>% 
  select(RT,logRT, RTinv) %>% 
  psych::describe() 

df %>% 
  select(RT,logRT, RTinv) %>% 
  sample_n(5000)          %>%
  pastecs::stat.desc(norm =T) %>%
  round(3)

# The values of skew and kurtosis should be zero in a normal distribution.
# skew.2SE and kurt.2SE are the skew and kurtosis value divided by 2 SEs. SIgn. if greater than 1

### INSPECT QQPLOTS

RT_cor    = qqnorm(df$RT,plot=F)
RTinv_cor = qqnorm(df$RTinv,plot=F)
RTlog_cor = qqnorm(df$logRT,plot=F)

RT_plot <- df %>% 
  ggplot(aes(sample=RT))+
  stat_qq() +
  stat_qq_line(colour = stellenbosch_colours["shiraz"]) +
  ggtitle("RT") +
  annotate("text",
           x     = -3,
           y     = max(df$RT),
           size  = 8,
           label = paste("italic(r) ==",
                         round(cor(RT_cor$x,RT_cor$y),3)),parse=T)

RTinv_plot <- df  %>%
  ggplot(aes(sample=RTinv))+
  stat_qq() +
  stat_qq_line(colour = stellenbosch_colours["shiraz"]) +
  ggtitle("Inverse Gaussian transform") +
  annotate("text",
           x     = -3,
           y     = max(df$RTinv),
           size  = 8,
           label = paste("italic(r) ==",
                         round(cor(RTinv_cor$x,RTinv_cor$y),3)),parse=T)

RTlog_plot <- df %>% 
  ggplot(aes(sample=logRT))+
  stat_qq() +
  stat_qq_line(colour = stellenbosch_colours["shiraz"]) +
  ggtitle("Log-Normal transform") +
  annotate("text",
           x     = -3,
           y     = max(df$logRT),
           size  = 8,
           label = paste("italic(r) ==",
                         round(cor(RTlog_cor$x,RTlog_cor$y),3)),parse=T)

grid.arrange(RT_plot,RTinv_plot,RTlog_plot,ncol=3)

### BY PARTICIPANT

df %>%
  ggplot(aes(sample=RTinv,colour=language))+
  stat_qq() +
  facet_wrap(~participant) +
  stat_qq_line(colour="black") +
  scale_colour_stellenbosch() +
  ggtitle("b") +
  ggtitle("Participant QQ Plots")

### WHICH PARTICIPANTS DEVIATE FROM NORMALITY?

df %>% 
  group_by(participant) %>% 
  summarise(p = round(shapiro.test(RTinv)$p.value,5)) %>% 
  filter(p < .05)

# -------------------------------------------
####### REGRESSION MODELS ###################

### RANDOM INTERCEPTS

m1_a = lmer(RTinv ~ language*condition*congruent + (1|participant) + (1|item), data = df)
summary(m1_a)

library(robustlmm)
m1_a_rob = rlmer(RTinv ~ language + (1|participant) + (1|item),method="DASvar", data = df)
summary(m1__rob)

m1_au=augment(m1_a)

# MODEL CRITICISM

# R-squared
cor(m1_au$RTinv,m1_au$.fitted) ^2

# Diagnostic plots
plot_model(m1_a,type="diag")

# Remove residuals > 2.5. sd
df2 = df[abs(scale(m1_au$.resid)) < 2.5, ]

# refit model
m1_b = lmer(RTinv~language*condition*congruent + (1|participant) + (1|item), data = df2)
summary(m1_b)

# Check new model
m1_bu=augment(m1_b)

# R-squared
cor(m1_bu$RTinv,m1_bu$.fitted) ^2

plot_model(m1_b,type="diag")[[1]]

### RANDOM INTERCEPTS AND SLOPES

m2_a = lmer(RTinv~language*condition*congruent + (1+condition+congruent|participant) + (1|item), data = df)

summary(m2_a)

# MODEL CRITICISM

m2_au=augment(m2_a)

cor(m2_au$RTinv,m2_au$.fitted)^2

plot_model(m2_a,type="diag")

df3 = df[abs(scale(m2_au$.resid)) < 2.5, ]

m2_b = lmer(RTinv~language*condition*congruent +(1+condition+congruent|participant) + (1|item), data = df3)
summary(m2_b)
m2_bu=augment(m2_b)

cor(m2_bu$RTinv,m2_bu$.fitted)^2

plot_model(m2_b,type="diag")

# INCLUDE TRIAL AND LAG t-1 ???

acf_n_plots(df$RTinv,
         split_by=list(Subject=df$participant, Trial=df$trial),
         n=25)

# no compelling evidence for autocorrelation at lag t-1

df$RTinv_lag = (1/df$RTlag)*1000

m3_a = lmer(RTinv~language*condition*congruent + trial + RTinv_lag + (1+condition+congruent|participant) + (1|item), data = df)

summary(m3_a)

# MODEL CRITICISM

m3_au=augment(m3_a)

cor(m3_au$RTinv,m3_au$.fitted)^2

plot_model(m3_a,type="diag")

df4 = df[abs(scale(m3_au$.resid)) < 2.5, ]

m3_b = lmer(RTinv~language*condition*congruent + trial + RTinv_lag + (1+condition+congruent|participant) + (1|item), data = df4)
summary(m3_b)
m3_bu=augment(m3_b)

cor(m3_bu$RTinv,m3_bu$.fitted)^2

plot_model(m3_b,type="diag")

  