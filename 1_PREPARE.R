# IMPLICIT ASSOCIATIONS RT TASK: PREPARE DATA
# PEER CHRISTENSEN
# DATA FROM STELLENBOSCH, MAY 2018
# OCTOBER 2018

# 1 Clean and add variables
# 2 filter (accuracy, bad participants)
# 3 Inspect overall RT
# 4 set absolute thresholds
# 5 write CSV (robust)
# 6 set individual thresholds
# 7 write CSV

# -------------------------------------------
####### LOAD PACKAGES AND DATA ##############

library(tidyverse)
library(magrittr)
library(janitor)
library(gridExtra)
library(stellenbosch)
library(data.table)

files <- list.files("RT_logs_stellenbosch",pattern="*.csv",full.names = T)

df    <- files   %>%
  map(read_csv2) %>%
  reduce(rbind)

theme_set(theme_bw())

# -------------------------------------------
####### CHANGE VARIABLES ####################

df$language[df$language == "Afrkaans"] <- "Afrikaans"
df$language[df$language == "Xh"] <- "Xhosa"

df$RT          <- as.numeric(df$RT) * 1000
df$participant <- factor(df$participant)
df$condition   <- factor(df$condition)
df$sound       <- factor(df$sound)

# -------------------------------------------
####### ADD VARIABLES #######################

df$logRT <- log(df$RT)
df$RTinv <- (1/df$RT) * 1000
d_table  <- data.table(df)
d_table[ , RTlag := shift(RT)]
df       <- as.tibble(d_table)
df$RTlag[df$trial == 1] <- NA 

df %<>% 
  dplyr::group_by(participant) %>% 
  mutate(
    trialCount = row_number(),
    accuracy   = ifelse((sound == "high" & key == "k") |
                        (sound == "low" & key == "s"), 1, 0),
    congruent  = factor(ifelse(((visual=="high" | visual =="small")  & sound=="high") |
                               ((visual=="low" | visual =="big") & sound=="low"),T,F)),
    item       = paste0(visual,sound))

# several participants appear to have reversed their key presses for high/low 
# upon inspection of overall accuracy
prop.table(table(df$accuracy,df$participant),2)

reversed <- c("F24","F31","F34","F35","F39","F43","M31","M36","M42","M44")
df$accuracy[df$participant %in% reversed] <- recode(df$accuracy[df$participant %in% reversed],"0"="1","1"="0") %>%
  as.numeric()

# -------------------------------------------
####### FILTER ##############################

### 1. remove participants due to one hand answers/closed eyes etc.

remove_af <- c("ARF9","ARF28","ARF29","ARF31")

df %<>% 
  filter(!participant %in% remove_af) %>%
  droplevels()

### 2. remove "bad" participants (poor accuracy)

overall_accuracy <- df        %>% 
  tabyl(accuracy,participant) %>% 
  adorn_percentages("col")    %>%
  filter(accuracy == 1)       %>% 
  gather()                    %>%
  filter(key != "accuracy")   %>%
  inner_join(df, by = c("key" = "participant")) %>%
  dplyr::select(key, value)   %>%
  filter(!duplicated(key,value))

overall_accuracy %>% 
  ggplot(aes(y=value)) + 
  geom_boxplot(fill = unname(stellenbosch_colours["khaki"]))
  

# we can set the accuracy threshold at .85
# Another method (Abutalebi et al.) is to set threshold at lower group CI
# this would exclude 4 afrikaans speakers, see EXTRA

remove_participants <- overall_accuracy %>%
  filter(value < .85) %>% 
  unique()            

df %<>% filter(!participant %in% remove_participants$key,
               accuracy==1)

### 3. set upper RT threshold

# distribution
ggplot(df,aes(RT)) + 
  geom_density(fill = unname(stellenbosch_colours["khaki"]))

# zoom in
distr_curve <- ggplot(df,aes(RT)) + 
  geom_density(fill = unname(stellenbosch_colours["khaki"])) +
  geom_vline(xintercept = 5000, 
             colour     = stellenbosch_colours["green"], 
             linetype   = "dashed", size=1) +
  coord_cartesian(xlim  = c(0,10000))

distr_box <- ggplot(df,aes(x="",y=RT)) + 
  geom_jitter(alpha   = .2) +
  geom_boxplot(alpha  = .5,
               fill   = unname(stellenbosch_colours["shiraz"]),
               colour = unname(stellenbosch_colours["khaki"]),
               outlier.shape = NA) +
  geom_hline(yintercept = 5000,
             colour     = stellenbosch_colours["green"], 
             linetype   = "dashed", size = 1) +
  coord_cartesian(ylim  = c(0,10000)) 

grid.arrange(distr_curve,distr_box,ncol=2)

# -------------------------------------------
####### CSV FILE FOR ROBUST ANALYSIS ########

df_robust <- df
write_csv(df_robust,"datafile_task2_robust.csv")

# -------------------------------------------
####### SET INDIVIDUAL RT THRESHOLDS ########

limits <- df %>%
  filter(accuracy == "1") %>%
  group_by(participant)   %>%
  summarise(m = mean(RT),
            upper_lim = mean(RT) + (3*sd(RT)))

df %<>% 
  #filter(accuracy == "1") %>%
  group_by(participant) %>%
  inner_join(limits, by = "participant") %>%
  filter(RT <= upper_lim)

# -------------------------------------------
####### CSV FILE FOR ROBUST ANALYSIS ########

write_csv(df, "datafile_task2.csv")

