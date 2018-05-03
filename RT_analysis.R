library(tidyverse)
library(lme4)
library(lmerTest)
##########################################################
#load and prepare

files = list.files("RT_logs",pattern="csv",full.names = T)
df=tibble()

for (file in files){
  p = read_csv2(file)
  df = rbind(df,p)
}

df$RT = as.numeric(df$RT)
df$participant = factor(df$participant)
df$condition = factor(df$condition)

df = df %>% 
  filter(RT>=.2, RT<=.65) %>%
  mutate(RT = RT * 1000,
         correct = factor(ifelse((sound == "high" & key == "k") |
                           (sound == "low" & key == "s"), "yes", "no")),
         congruent = factor(ifelse(((visual=="high" | visual =="small")  & sound=="high") |
                              ((visual=="low" | visual =="big") & sound=="low"),T,F)))

##########################################################
#accuracy

table(df$correct)
prop.table(table(df$correct))
#by condition
table(df$correct,df$condition)
prop.table(table(df$correct,df$condition),2)
#model
fit1 =glmer(correct ~ condition+congruent+(1|participant),data=df,family="binomial")
summary(fit1)

##########################################################
#reaction time

react = df %>% 
  filter(correct=="yes") %>%
  group_by(condition,congruent) %>%
  summarise(n=n(),mean = mean(RT), sd = sd(RT))
react 

#boxplot: RT ~ condition
df %>%
  ggplot(aes(x=condition,y=RT)) +
  geom_boxplot()

#boxplot: RT ~ congruency + condition
df %>%
  ggplot(aes(x=congruent,y=RT,fill=congruent)) +
  geom_boxplot() +
  #geom_violin() +
  #geom_point(aes(y=mean(RT)),shape=23, fill="white", color="black", size=5)
  facet_wrap(~condition)

##boxplot: RT ~ congruency + pitch + condition
df %>%
  ggplot(aes(x=congruent,y=RT,fill=sound)) +
  geom_boxplot() +
  #geom_point(aes(y=mean(RT)),shape=23, fill="white", color="darkred", size=5)
  #geom_violin() +
  facet_wrap(~condition)

#
ggplot(df, aes(x=RT, fill=congruent)) + geom_density(alpha=.3) +
  facet_wrap(~condition)

#test
fit2 =lmer(RT ~ condition+congruent+ (1|participant),data=df)
summary(fit2)
