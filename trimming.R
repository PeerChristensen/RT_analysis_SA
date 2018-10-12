# ANALYSIS OF IMPLICIT ASSOCIATIONS RT TASK:
# TRIMMING

# PEER CHRISTENSEN
# STELLENBOSCH, MAY 2018

# 1. set upper threshold at 5000 ms
# 2. remove "bad" participants (poor accuracy)
# 3. set accuracy threshold at .85
# 4. set individual RT limits at 3 sd above mean

# For robust methods analysis (steps 1-3) use "robust" data
# For central tendency and ex-gaussian analyses steps (1-4), use "TRIMMED" data


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
library(magrittr)

df = read_csv("RT_task2_CLEAN")

# distribution
distDens = ggplot(df,aes(RT)) + 
  geom_density() +
  facet_wrap(~condition)

distBox = ggplot(df,aes(x = condition, y=RT)) + 
  geom_jitter(alpha=.3) + 
  geom_boxplot(alpha =.5) 

grid.arrange(distDens,distBox,ncol=2)

# zoom in, set line at 5000 ms
distDensZoom = ggplot(df,aes(RT)) + 
  geom_density() +
  geom_vline(xintercept = 5000, colour = "blue", linetype = "dashed", size=1) +
  facet_wrap(~condition) +
  coord_cartesian(xlim = c(0,10000))

distBoxZoom = ggplot(df,aes(x=condition,y=RT)) + 
  geom_jitter(alpha=.3) + geom_boxplot(alpha =.5) + 
  geom_hline(yintercept = 5000, colour = "blue", linetype = "dashed", size = 1) +
  coord_cartesian(ylim = c(0,10000)) 

grid.arrange(distDensZoom,distBoxZoom,ncol=2)

# discard data above 5000 ms threshold

# n observations in original data
df %<>% filter(!is.na(RT))
before <- nrow(df)

df %<>% filter(RT < 5000)

after <- nrow(df)

diff <- before - after
diff/before*100 # diff is 31, % of orig: 0.15

# look at distribution

distDens2 = ggplot(df,aes(RT)) + 
  geom_density() +
  facet_wrap(~condition)

distBox2 = ggplot(df,aes(x = condition, y=RT)) + 
  geom_jitter(alpha=.3) + 
  geom_boxplot(alpha =.5) 

grid.arrange(distDens2,distBox2,ncol=2)

# remove "bad" participants (poor accuracy)

overall_accuracy = df %>% 
  tabyl(accuracy,participant) %>% 
  adorn_percentages("col") %>%
  filter(accuracy==1) %>% 
  gather() %>%
  filter(key != "accuracy") %>%
  inner_join(df, by = c("key" = "participant")) %>%
  select(key, value)    

accuracyDens = overall_accuracy %>% 
  ggplot(aes(x=value)) + geom_density(alpha = .6)
accuracyBox = overall_accuracy%>% 
  ggplot(aes(y=value)) + geom_boxplot()

grid.arrange(accuracyDens,accuracyBox, ncol = 2)

# we can set the threshold at .85
remove_participants <- overall_accuracy %>%
  filter(value < .85) %>% 
  select(key) %>% 
  unique()

df %<>% filter(!participant %in% remove_participants$key,
               accuracy==1)

after2 <- nrow(df)
diff <- before - after2
diff/before*100 # diff is 2476, % of orig: 12.1

# remove Af speakers due to one hand answers/closed eyes etc.
remove_af <- c("ARF9","ARF28","ARF29","ARF31")
df %<>% filter(!participant %in% remove_af)

# write csv

write_csv(df, "RT_task2_TRIMMED_robust")

# individual cut-off

limits <- df %>%
  filter(accuracy == "1") %>%
  ddply(.(participant), summarise, m = mean(RT), limit = mean(RT) + (3*sd(RT)))

df %<>% 
  #filter(accuracy == "1") %>%
  group_by(participant) %>%
  inner_join(limits, by = "participant") %>%
  filter(RT <= limit)

after3 <- nrow(df)
diff <- before - after3
diff/before*100 # diff is 2852, % of orig: 14

write_csv(df, "RT_task2_TRIMMED")

