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

# add a logit column for emotions variables
df_clean_long <-
  df_clean_long %>%
  mutate(
    value_logit = gtools::logit(value),
    value_logit = ifelse(value_logit < -100,-100, value_logit),
    value_logit = ifelse(value_logit > 100,100, value_logit)
  )

## make pictures!

# emotion scores
df_clean_long %>%
  ggplot(aes(x=value, color=source)) +
  facet_grid(rows='variable', scales='free_x') +
  geom_density()

# logit transformed emotions
df_clean_long %>%
  ggplot(aes(x=value_logit, color=source)) +
  facet_grid(rows='variable', scales='free_x') +
  geom_density() +
  scale_x_continuous(limits=c(-7,4))


## t-tests

#
get_t_test_results <- function(df_emotion){
  t.test(value_logit ~ source, df_emotion, na.action = na.omit)
}

df_clean_long %>% filter(variable=='anger' & !is.na(value_logit)) %>% get_t_test_results()
df_clean_long %>% filter(variable=='disgust' & !is.na(value_logit)) %>% get_t_test_results()
df_clean_long %>% filter(variable=='fear' & !is.na(value_logit)) %>% get_t_test_results()
df_clean_long %>% filter(variable=='joy' & !is.na(value_logit)) %>% get_t_test_results()
df_clean_long %>% filter(variable=='sadness' & !is.na(value_logit)) %>% get_t_test_results()



