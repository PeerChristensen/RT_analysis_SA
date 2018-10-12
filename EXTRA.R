
library(tidyverse)
library(ggpairs)
library(retimes)


params %>% 
  filter(language=="Afrikaans") %>% 
  dplyr::select(mean,sd,mu,sigma,tau,coef_var) %>%
  ggpairs()

params %>% 
  filter(language=="Xhosa") %>% 
  dplyr::select(mean,sd,mu,sigma,tau,coef_var) %>%
  ggpairs()

# conflict,mu,tau

conflict_corr <- df %>%
  group_by(language,condition,participant) %>%
  dplyr::summarise(m_incongruent = mean(RT[congruent==FALSE]),
                   m_congruent   = mean(RT[congruent==TRUE]),
                   conflict      = m_incongruent - m_congruent,
                   mu            = mexgauss(RT)[1],
                   tau           = mexgauss(RT)[3]) %>%
  ungroup()

conflict_corr %>% 
  filter(language=="Afrikaans") %>% 
  dplyr::select(mu,tau,conflict) %>%
  ggpairs()

conflict_corr %>% 
  filter(language=="Xhosa") %>% 
  dplyr::select(mu,tau,conflict) %>%
  ggpairs()


# set threshold accuracy at lower CI
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
df$RTinv <- 1/df$RT
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

overall_accuracy <- df[df$language=="Afrikaans",]        %>% 
  droplevels() %>%
  tabyl(accuracy,participant) %>% 
  adorn_percentages("col")    %>%
  filter(accuracy == 1)       %>% 
  gather()                    %>%
  filter(key != "accuracy")   %>%
  inner_join(df, by = c("key" = "participant")) %>%
  dplyr::select(key, value)   %>%
  filter(!duplicated(key,value))

Publish::ci.mean(overall_accuracy$value)

remove_participants <- overall_accuracy %>%
  filter(value < .93) %>% 
  unique()            


overall_accuracy <- df[df$language=="Xhosa",]        %>% 
  droplevels() %>%
  tabyl(accuracy,participant) %>% 
  adorn_percentages("col")    %>%
  filter(accuracy == 1)       %>% 
  gather()                    %>%
  filter(key != "accuracy")   %>%
  inner_join(df, by = c("key" = "participant")) %>%
  dplyr::select(key, value)   %>%
  filter(!duplicated(key,value))

Publish::ci.mean(overall_accuracy$value)


# STEPWISE

library(MASS)
# Fit the full model 
full.model <- lm(tau ~ language*condition*congruent, data = params)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "backward", 
                      trace = FALSE)
summary(step.model)


