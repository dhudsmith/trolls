library(dplyr)
library(ggplot2)
library(reshape2)

setwd('~/Code/trolls/')

# read in the data
df_ira <- read.csv('data/watson_ira_1000.csv')
df_control <- read.csv('data/watson_media_1000.csv')

# Clean and combine
df_ira_clean <- 
  df_ira %>%
  select(sentiment_score, anger, disgust, fear, joy, sadness) %>%
  mutate(
    source = 'ira'
  )

df_control_clean <- 
  df_control %>%
  select(sentiment_score, anger, disgust, fear, joy, sadness) %>%
  mutate(
    source = 'control'
  )

df_clean <- rbind(df_ira_clean, df_control_clean) %>% select(-sentiment_score)

# reshape the data for easier analysis
df_clean_long <-
  df_clean %>%
  melt(id.vars=c('source'))

## make pictures!
df_clean_long %>%
  ggplot(aes(x=value, color=source)) +
  facet_grid(rows='variable', scales='free_x') +
  geom_density()
