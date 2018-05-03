

library(tidyverse)
library(lme4)
library(lmerTest)

files = list.files("Choice_logs",pattern="csv",full.names = T)
df=tibble()

for (file in files){
  p = read_csv2(file)
  df = rbind(df,p)
}

df = df %>%
  mutate(target = ifelse((voice =="low" & (left =="low" | left=="big") & (right =="low" | right=="big")) |
                  (voice =="high" & (left =="high" | left=="small") & (right =="high" | right=="small")),T,F)) %>%
  mutate(choiceCongruent = ifelse(((choiceName =="high" | choiceName =="small")  & voice=="high") |
                              ((choiceName=="low" | choiceName =="big") & voice=="low"),T,F)) %>%
  mutate(choiceDimension = ifelse(choiceName =="high" | choiceName =="low","height","size")) %>%
  filter(target==T)

table(df$choiceDimension)
