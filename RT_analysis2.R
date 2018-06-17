#selecting RT data transformation

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
library(data.table)

files = list.files("RT_logs_stellenbosch",pattern="*.csv",full.names = T)

df = files %>%
  map(read_csv2) %>%
  reduce(rbind)

df$RT = as.numeric(df$RT)*1000
df = df %>% filter(RT>=200, RT<=944)
d_table=data.table(df)
d_table[ , RTlag := shift(RT)]
df=as.tibble(d_table)
df$participant = factor(df$participant)
df$condition = factor(df$condition)
df$sound=factor(df$sound)
df$logRT=log(df$RT)
df$RTinv=-1000/df$RT
df = df %>% 
  group_by(participant) %>% 
  dplyr::mutate(id = row_number())

df = df %>% 
  #filter(RT>=200,RT<=1343.442) %>%
  mutate(accuracy = ifelse((sound == "high" & key == "k") |
                             (sound == "low" & key == "s"), 1, 0),
         congruent = factor(ifelse(((visual=="high" | visual =="small")  & sound=="high") |
                                     ((visual=="low" | visual =="big") & sound=="low"),T,F)),
         item = paste0(df$visual,df$sound))


df = df %>% filter(accuracy==1)

m0=lmer(RT~1+trial+RTlag+(1|participant),data=df)
m1_a=lmer(RTinv~sound+congruent+condition+block+trial+RTlag+id+
            (1|participant)+
            (1|item),data=df)
summary(m1_a) # slower RT when sound is "low"

m1_b=lmer(RTinv~condition+trial+RTlag
          +(1|participant),data=df) #ns
summary(m1_b)
m1_c=lmer(RTinv~+block+trial+RTlag+(1+condition+congruent+sound|participant),data=df) #ns
summary(m1_c)

anova(m0,m1_a) #m1_a best

m2_a = lmer(RT~sound+condition+trial+RTlag+(1|participant),data=df) 
summary(m2_a)
anova(m1_a,m2_a) #m1 best

m2_b = lmer(RT~sound+congruent+trial+RTlag+(1|participant),data=df)
summary(m2_b)

m_full = lmer(RT~sound+condition+congruent+RTlag+block+trial+(1+condition+congruent+sound|participant),data=df)
summary(m_full)

anova(m1_a,m2_b) #m1 best

m3_a = lmer(RT~sound*congruent+trial+RTlag+(1|participant),data=df) 
summary(m3_a)

m3_b = lmer(RT~sound*condition+trial+RTlag+(1|participant),data=df) 
summary(m3_b)


m4 = lmer(RT~sound*congruent*condition+RTlag+trial+(1|participant),data=df)
summary(m4)
anova(m1_a,m4) #m1 slightly better, p = 0.05047

#R^2
cor(fitted(m1_a), df$RT[-1])^2


f = function(dfr) return(shapiro.test(sample(dfr$RT)$p.value,5000))
p = as.vector(by(df, df$participant, f))
names(p) = levels(df$participant)
names(p[p < 0.05])

df

df3 = df[abs(scale(resid(m_full))) < 2.5, ]

qqnorm(df$RTinv)
qqline(df$RTinv)

plot_model(m_full, type = "int")

#diagnostic plots
plot_model(m_full, type = "diag") #output looks identical to m1_a


