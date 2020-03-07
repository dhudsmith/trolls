library(readr)
library(reshape2)
library(dplyr)
library(lubridate)
library(ggplot2)

# read in the giant dataset of tweets with sentiment and emotion
df <- readr::read_csv('~/Code/trolls/data/2020-02-26_tweets-and-nlu-results_complete-set.zip')

# reshape for plotting
df_long <- 
  df %>%
  melt(id.vars = c('tweet_id', 'is_ira', 'date', 'year', 'month', 'twitter_account', 'text', 'stratify'),
       measure.vars = c('sentiment_score', 'emotion_anger', 'emotion_disgust', 'emotion_fear', 'emotion_joy', 'emotion_sadness')) %>%
  mutate(
    variable = factor(variable,
                      levels = c('sentiment_score', 'emotion_anger', 'emotion_disgust', 'emotion_fear', 'emotion_joy', 'emotion_sadness'),
                      labels = c('Sentiment', 'Anger', 'Disgust', 'Fear', 'Joy', 'Sadness')),
    is_ira = factor(is_ira,
                    levels = c(0,1),
                    labels = c('Control', 'IRA'))
  ) 

# number of posts through time
df_long %>%
  group_by(is_ira, date = as.Date(date)) %>%
  dplyr::summarise(
    count = n()
  ) %>%
  ggplot(aes(x=date, y=count, color=is_ira)) +
  geom_point() +
  geom_smooth(span=0.25) +
  facet_grid(vars(is_ira), scales='free_y') + 
  labs(x='Date', y='Number of Posts (daily)', title='Number of posts through time')

# average sentiment through time
df_long %>%
  filter(variable=='Sentiment') %>%
  group_by(date = as.Date(date)) %>%
  dplyr::summarise(
    mean_sentiment_score = mean(value, na.rm=T)
  ) %>%
  ggplot(aes(x=date, y=mean_sentiment_score)) +
  geom_point() +
  geom_smooth(span=0.25) +
  labs(x='Date', y='Mean Sentiment Score (daily)', title='Sentiment score through time') +
  ylim(-0.25,0.25)

# average sentiment through time by ira
df_long %>%
  filter(variable=='Sentiment') %>%
  group_by(is_ira, date = as.Date(date)) %>%
  dplyr::summarise(
    mean_sentiment_score = mean(value, na.rm=T)
  ) %>%
  ggplot(aes(x=date, y=mean_sentiment_score, color=is_ira)) +
  geom_point() +
  geom_smooth(span=0.25) +
  labs(x='Date', y='Mean Sentiment Score (daily)', title='Sentiment score through time') +
  ylim(-0.25,0.1)

# average emotion through time
df_long %>%
  filter(variable!='Sentiment') %>%
  group_by(variable, date = as.Date(date)) %>%
  dplyr::summarise(
    mean_emotion = mean(value, na.rm=T)
  ) %>%
  ggplot(aes(x=date, y=mean_emotion)) +
  geom_point(alpha=0.3) +
  geom_smooth(span=0.25) +
  facet_grid(vars(variable)) + 
  labs(x='Date', y='Mean Emotion Score (daily)', title='Sentiment score through time')

# average emotion through time by IRA
df_long %>%
  filter(variable!='Sentiment') %>%
  group_by(is_ira, variable, date = as.Date(date)) %>%
  dplyr::summarise(
    mean_emotion = mean(value, na.rm=T)
  ) %>%
  ggplot(aes(x=date, y=mean_emotion, color=is_ira)) +
  geom_point(alpha=0.3) +
  geom_smooth(span=0.25) +
  facet_grid(vars(variable), scales='free_y') + 
  labs(x='Date', y='Mean Emotion Score (daily)', title='Sentiment score through time')
  
