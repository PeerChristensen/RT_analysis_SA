# IMPLICIT ASSOCIATIONS RT TASK: EXPLORATORY ANALYSIS
# PEER CHRISTENSEN
# DATA FROM STELLENBOSCH, MAY 2018
# OCTOBER 2018

# BOXPLOTS
# DENSITY PLOTS
# INTERACTION PLOTS

# -------------------------------------------
####### LOAD PACKAGES AND DATA ##############

library(tidyverse)
library(ggridges)
library(stellenbosch)
library(gridExtra)
library(grid)

df <- read_csv("datafile_task2.csv")

theme_set(theme_bw())

# -------------------------------------------
######### BOXPLOTS ##########################

cols = unname(stellenbosch_colours[c("green","shiraz","khaki")])

df %>% ggplot(aes(x=language,y=RT,fill=language)) + 
  geom_jitter(alpha = .3) +
  geom_boxplot(outlier.shape = NA,
               colour        = cols[3]) +
  scale_fill_manual(values   = c(cols[1],cols[2])) +
  facet_grid(condition~congruent)

# -------------------------------------------
######### DENSITY PLOTS #####################

df %>% 
  ggplot(aes(x=RT,fill=language)) + 
  geom_density(alpha = .6) +
  scale_fill_manual(values=c(cols[1],cols[2])) +
  facet_grid(condition~congruent) +
  xlim(c(0,2000))

# -------------------------------------------
######### INTERACTION PLOT ##################

dodge <- position_dodge(width=0.1)

df %>% 
  Rmisc::summarySE(measurevar="RT",groupvars=c("language","condition","congruent")) %>%
  ggplot(aes(x = congruent, y = RT, color = language)) +
  geom_line(aes(group = language),
            size      = 1,
            position  = dodge) +
  geom_point(position = dodge) +
  geom_errorbar(aes(ymin = RT-ci,ymax = RT+ci),
                width    =.3,
                position = dodge) +
  facet_grid(~condition) +
  scale_colour_manual(values=c(cols[1],cols[2])) +
  coord_cartesian(ylim=c(0, 800))

# -------------------------------------------
######### CONFLICT EFFECT ###################

conflict_df <- df %>%
  group_by(language,condition,participant) %>%
  dplyr::summarise(m_incongruent = mean(RT[congruent==FALSE]),
                   m_congruent   = mean(RT[congruent==TRUE]),
                   conflict      = m_incongruent - m_congruent)

### BOXPLOTS
conflict_df %>% ggplot(aes(x=language,y=conflict,fill=language)) + 
  geom_jitter(alpha = .7) +
  geom_boxplot(alpha         = .8,
               outlier.shape = NA,
               colour        = cols[3]) +
  scale_fill_manual(values   = c(cols[1],cols[2])) +
  facet_grid(~condition)

### DENSITY PLOTS
conflict_df %>% ggplot(aes(x=conflict,fill=language)) + 
  geom_density(alpha = .6) +
  scale_fill_manual(values=c(cols[1],cols[2])) +
  facet_grid(~condition) 

### INTERACTION PLOTS
conflict_df %>% 
  Rmisc::summarySE(measurevar="conflict", groupvars = c("language","condition")) %>%
  ggplot(aes(x = condition, y = conflict, color = language)) +
  geom_line(aes(group = language),
            size      = 1,
            position  = dodge) +
  geom_point(position = dodge) +
  geom_errorbar(aes(ymin = conflict-ci,ymax = conflict+ci),
                width    =.3,
                position = dodge) +
  scale_colour_manual(values=c(cols[1],cols[2])) +
  facet_grid(~language) +
  guides(colour=FALSE)

# -------------------------------------------
######### COEFFICIENT OF VARIABILITY ########

coef_var_df <- df %>% 
  group_by(language,condition,congruent,participant) %>%
  dplyr::summarise(n        = n(),
                   mean     = mean(RT),
                   sd       = sd(RT),
                   coef_var = sd(RT)/mean(RT))

### BOXPLOTS
coef_var_df %>% ggplot(aes(x=language,y=coef_var,fill=language)) + 
  geom_jitter(alpha = .7) +
  geom_boxplot(alpha         = .8,
               outlier.shape = NA,
               colour        = cols[3]) +
  scale_fill_manual(values   = c(cols[1],cols[2])) +
  facet_grid(~condition+congruent)

### DENSITY PLOTS
coef_var_df %>% ggplot(aes(x=coef_var,fill=language)) + 
  geom_density(alpha = .6) +
  scale_fill_manual(values=c(cols[1],cols[2])) +
  facet_grid(~condition) 

### INTERACTION PLOTS
coef_var_df %>% 
  Rmisc::summarySE(measurevar="coef_var", groupvars = c("language","condition")) %>%
  ggplot(aes(x = condition, y = coef_var, color = language)) +
  geom_line(aes(group = language),
            size      = 1,
            position  = dodge) +
  geom_point(position = dodge) +
  geom_errorbar(aes(ymin = coef_var-ci,ymax = coef_var+ci),
                width    =.3,
                position = dodge) +
  scale_colour_manual(values=c(cols[1],cols[2])) +
  facet_grid(~language) +
  guides(colour=FALSE)
  
# -------------------------------------------
######### RT BY TRIAL #######################

# WITHIN BLOCKS
RT_within <- df %>% 
  group_by(language,participant,trial) %>%
  dplyr::summarise(m=mean(RT))         %>%
  ggplot(aes(x=trial,y=m)) +
  geom_line(aes(group=participant),
            size=.1) +
  facet_grid(language~.) +
  geom_smooth(colour      = cols[3],
              size        = 1,
              aes(fill    = language),
              show.legend = F) +
  ggtitle("RT within blocks") 

# OVERALL
RT_overall <- df %>% 
  group_by(language,participant,trialCount) %>%
  dplyr::summarise(m=mean(RT))              %>%
  ggplot(aes(x=trialCount,y=m)) +
  geom_line(aes(group=participant),size=.1) +
  facet_grid(language~.) +
  geom_smooth(colour      = cols[3],
              size        = 1,
              aes(fill    = language),
              show.legend = F) +
  ggtitle("RT overall")

grid.arrange(RT_within,RT_overall,
              top = grid::textGrob("RT by trial",
                                   gp=gpar(fontsize=20)))

