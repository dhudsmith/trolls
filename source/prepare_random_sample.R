##
## Preliminary

# libraries
library(plyr)
library(ggplot2)

# directories
proj_dir <- '~/Code/trolls/'
data_dir <- paste0(proj_dir, 'data/' )

# load in the data
df_ira <- readr::read_csv(paste0(data_dir, 'NewsFeeds.csv'))

news_controls_names <- list.files(paste0(data_dir, 'Media Controls/'), full.names = TRUE)
#--> 14 csv files

df_news_controls <- ldply(news_controls_names, 
  function(file){
    return(readr::read_csv(file))
})

##
## Get the random samples
num <- 1000
df_ira_random <- dplyr::sample_n(df_ira, num)
df_nc_random <- dplyr::sample_n(df_news_controls, num)

##
## Save the relevant datasets

readr::write_csv(df_news_controls, paste0(data_dir, "media_controls_combined.csv"))
readr::write_csv(df_ira_random, paste0(data_dir, 'ira_random_',num, '.csv'))
readr::write_csv(df_nc_random, paste0(data_dir, 'media_controls_random_', num, '.csv'))
