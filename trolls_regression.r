# Trolls regression
# C Ehrett 2020-02
# !diagnostics off

rm(list=ls())

# Helper functions
logit_transform <- function(p){
  # This function performs a logit transform after shrinking a
  # [0,1] input to [epsilon,1-epsilon] for tiny epsilon.
  # The logit transform is to remove the boundaries of the input,
  # so that it is not confined to [0,1]. Thus it more closely
  # approximates the normal.
  # The shrinking is because logit(0) and logit(1) give -Inf, Inf.
  epsilon <- .Machine$double.eps
  shrunkp <- p * (1-2*epsilon) + epsilon
  return(log(shrunkp/(1-shrunkp)))
}
sentiment01 <- function(s) {(s+1)/2} # Put sent. score on 0-1 scale

# Load in the data
dat <- read.csv('./2020-02-26_tweets-and-nlu-results_complete-set.csv',
  colClasses=c("tweet_id"="character"))
head(dat)


# What's the language of all these rows with no emotion?
dat$language[is.na(dat$emotion_anger)]
# What's the nlu_status of all these rows with no emotion?
dat$nlu_status[is.na(dat$emotion_anger)]
# Some fail, but some pass. Hmm. Why are there NAs here when they pass NLU?
# Let's look at some of the tweets.
head(dat$text[is.na(dat$emotion_anger)&dat$nlu_status=='pass'])
tail(dat$text[is.na(dat$emotion_anger)&dat$nlu_status=='pass'])
# There is no obvious reason why these tweets lack emotion output.
# How many of them are in English?
sum(dat$language=='en'&is.na(dat$emotion_anger)&dat$nlu_status=='pass')
# What percentage of total tweets lack emotion score?
sum(is.na(dat$emotion_anger))/dim(dat)[1]
# Something to keep in mind.
# For now let's just nuke them, there are not very many.
dat <- na.omit(dat)

# Save some working memory by getting rid of columns we don't need
names(dat)
keepers <- c("tweet_id","is_ira","stratify","sentiment_score",
             "emotion_anger","emotion_disgust","emotion_fear",
             "emotion_joy","emotion_sadness")
dat <- dat[,keepers]


# Look for duplicates
dupes.idx <- duplicated(dat$tweet_id) | 
               duplicated(dat$tweet_id,fromLast = TRUE)
dupes <- dat[dupes.idx,]
dim(dupes)
# Get rows that are unique up to stratify label
dupes.unique <- unique(dupes[,(names(dupes)!="stratify")])
# So there are duplicates. We need to take that into account for the
# stratify dummy columns.
rm(dupes)
rm(dupes.unique)
rm(dupes.idx)


# One-hot encoding for stratify column
save.image()
library(dummies)
# Save the stratify labels and tweet_id
dat.stratify <- dat[,c('tweet_id','stratify')]
saveRDS(dat.stratify,'./dat_stratify.rds')
rm(dat.stratify)

# Create all the dummy columns. This takes a while
dat <- dummy.data.frame(dat,names="stratify",sep="_",verbose=TRUE)

# Aggregate stratify columns by sum, grouped by tweet_id
library(dplyr)

dat <- dat %>% group_by(tweet_id,is_ira,sentiment_score,
                             emotion_anger,emotion_disgust,emotion_fear,
                             emotion_joy,emotion_sadness) %>%
  summarise_at( names(dat)[grepl("stratify",names(dat))] ,sum)

# Earlier attempts at analysis ran afoul of what was eventually found to be 
# a duplicate stratify label. Existence of duplicate proven here:
ds226 <- dat$tweet_id[dat$stratify_226==1]
ds240 <- dat$tweet_id[dat$stratify_240==1]
all(ds226==ds240) # TRUE
rm(ds226,ds240)

# To correct for the duplicate stratify label, we simply delete one of
# the two columns in question.
dat <- dat[,!names(dat)=='stratify_240']

# Get stratify variable for each tweet_id. Since some are in multiple stratify
# groups, pick first for each.
dat.stratify <- dat.stratify %>% group_by(tweet_id) %>%
  summarise_at( "stratify" , first)
# Make sure we didn't pick up any 240's:
sum(dat.stratify$stratify==240)

# # Re-run the group_by statement with 240
# dat2 <- dat %>% group_by(tweet_id,is_ira,sentiment_score,
#                         emotion_anger,emotion_disgust,emotion_fear,
#                         emotion_joy,emotion_sadness) %>%
#   summarise_at( names(dat)[grepl("stratify",names(dat))] ,sum)


# Remove tweet_id column, we don't need it any more
dat$tweet_id<-NULL

# Convert sentiment score to logit scale
dat$sentiment_score <- 
  logit_transform(sentiment01(dat$sentiment_score))

# Convert each emotion score to logit scale
dat$emotion_anger <- logit_transform(dat$emotion_anger)
dat$emotion_disgust <- logit_transform(dat$emotion_disgust)
dat$emotion_fear <- logit_transform(dat$emotion_fear)
dat$emotion_joy <- logit_transform(dat$emotion_joy)
dat$emotion_sadness <- logit_transform(dat$emotion_sadness)
head(as.data.frame(dat))


library(tidyr)
library(broom)

# Take a look at the logit-transformed dependent variables
mean(dat$sentiment_score)
sd(dat$sentiment_score)
mean(dat$emotion_anger)
sd(dat$emotion_anger)

# # Standardize non-dummy vars
# # This is currently commented out because we decided not to use it.
# ndvars <- names(dat)[!grepl("stratify",names(dat))]
# ndvars <- ndvars[!(ndvars %in% c("score_type","is_ira"))]
# # Check for NAs again
# data.frame(ndvars,apply(dat[,ndvars],2,function(x) sum(is.na(x))))
# # And for Infs
# data.frame(ndvars,apply(dat[,ndvars],2,function(x) sum(is.infinite(x))))
# # Save the data before applying standardization
# saveRDS(dat,'R/Trolls/data_read_to_standardize.rds')
# for (var in ndvars) {
#   varmean <- mean(dat[,var])
#   varsd <- sd(dat[,var])
#   dat[,var] <- (dat[,var]-varmean)/varsd
# }

# Perform the analysis
targets <- c("sentiment_score","emotion_anger","emotion_disgust",
             "emotion_fear","emotion_joy","emotion_sadness")
covariates <- c(names(dat)[grep('stratify',names(dat))],"is_ira")
# Can use the following covariates if you don't want stratify
# covariates <- "is_ira"

# Save just before analysis
save.image('temp.RData')

library(biglm)
library(lmtest)
source('../Downloads/bigcluster_sandwich.R')
for (target in targets) {
  reg_formula = paste0(target,' ~ ',paste(covariates,collapse='+'))
  cat(paste(paste(rep('=',40),collapse = ""),
            "\nREGRESSION RESULTS FOR ",target,":\n",
            paste(rep('=',40),collapse = ""),sep=""))
  # Fit model. Too big, have to do it in chunks.
  maxrows <- 100000
  mod <- biglm(data = dat[1:maxrows,c(target,covariates)],
               as.formula(reg_formula))
  for (i in 2:floor(dim(dat)[1]/maxrows) ) {
    mod <- update(mod, dat[ ((i-1)*maxrows+1):(i*maxrows) ,
                            c(target,covariates)])
  }
  mod <- update(mod, dat[(i*maxrows+1):(dim(dat)[1]),
                         c(target,covariates)])
  
  # Now get vcov matric for cluster robust se's:
  bigvcov <- BigCluster(mod, as.data.frame(dat.stratify), 
                        dataFrame=as.data.frame(
                          dat[,c(target,covariates)]), sumMethod = "slow")
    # Print it
    print(tidy(mod)[tidy(mod)$term=='is_ira',])
    
    # Get Bonferroni correction of is_ira p-values
    pval <- coeftest(mod,bigvcov)['is_ira','Pr(>|z|)']
    bfcpval <- p.adjust(pval,"bonferroni",6)
    # Print adjusted p-values
    cat(paste("\nBONFERRONI CORRECTED p-value for is_ira: ",
              signif(bfcpval,digits = 3),"\n",sep=""))
  
  
}


#scr
save.image()
source('../Downloads/bigcluster_sandwich.R')
bigvcov <- BigCluster(mod, as.data.frame(dat.stratify), 
                      dataFrame=as.data.frame(dat[,c(target,covariates)]), sumMethod = "slow")

library(lmtest)
test <- coeftest(mod,bigvcov)
print(test[300:382,])
test['is_ira','Pr(>|z|)']

class(as.data.frame(dat[,c(target,covariates)])) 
class(as.data.frame(dat[,c(target,covariates)]))
dim(as.data.frame(dat[,c(target,covariates)]))
dim(as.data.frame(dat.stratify$stratify))
colnames(dat)

# Saving old biglm version here (pre-cluster robust se)
for (target in targets) {
  reg_formula = paste0(target,' ~ ',paste(covariates,collapse='+'))
  cat(paste("\nREGRESSION RESULTS FOR ",target,":\n",sep=""))
  # Fit model. Too big, have to do it in chunks.
  maxrows <- 100000
  mod <- biglm(data = dat[1:maxrows,c(target,covariates)],
               as.formula(reg_formula))
  for (i in 2:floor(dim(dat)[1]/maxrows) ) {
    mod <- update(mod, dat[ ((i-1)*maxrows+1):(i*maxrows) ,
                            c(target,covariates)])
  }
  mod <- update(mod, dat[(i*maxrows+1):(dim(dat)[1]),
                         c(target,covariates)])
  
  # Print it
  print(tidy(mod)[as.logical(tidy(mod)[1]=='is_ira'),])
  # Get Bonferroni correction of is_ira p-values
  pval <- summary(mod)[2][1]$mat[dim(summary(mod)[2][1]$mat)[1],5]
  bfcpval <- p.adjust(pval,"bonferroni",6)
  # Print adjusted p-values
  cat(paste("\nBONFERRONI CORRECTED p-value for is_ira: ",
            signif(bfcpval,digits = 3),"\n",sep=""))
}

# scr see if lm version works
for (target in targets) {
  reg_formula = paste0(target,' ~ ',paste(covariates,collapse='+'))
  cat(paste("\nREGRESSION RESULTS FOR ",target,":\n",sep=""))
  # Fit model. 
  
  mod <- lm(data = dat[,c(target,covariates)],
            as.formula(reg_formula))
  
  # Print it
  print(tidy(mod)[as.logical(tidy(mod)[1]=='is_ira'),])
  # Get Bonferroni correction of is_ira p-values
  pval <- summary(mod)$coefficients['is_ira',4]
  bfcpval <- p.adjust(pval,"bonferroni",6)
  # Print adjusted p-values
  cat(paste("\nBONFERRONI CORRECTED p-value for is_ira: ",
            signif(bfcpval,digits = 3),"\n",sep=""))
}

# scr see if lm.cluster version works
for (target in targets) {
  reg_formula = paste0(target,' ~ ',paste(covariates,collapse='+'))
  cat(paste("\nREGRESSION RESULTS FOR ",target,":\n",sep=""))
  # Fit model. 
  
  mod <- miceadds::lm.cluster(data = dat[,c(target,covariates)],
            as.formula(reg_formula),dat.stratify)
  
  # Print it
  print(tidy(mod)[as.logical(tidy(mod)[1]=='is_ira'),])
  # Get Bonferroni correction of is_ira p-values
  pval <- summary(mod)$coefficients['is_ira',4]
  bfcpval <- p.adjust(pval,"bonferroni",6)
  # Print adjusted p-values
  cat(paste("\nBONFERRONI CORRECTED p-value for is_ira: ",
            signif(bfcpval,digits = 3),"\n",sep=""))
}

# scr get csv of tweet_ids for tweets without emotion but that did pass nlu
df1 <- dat[dat$nlu_status=='pass'&is.na(dat$emotion_anger),c('tweet_id','text')]
write.csv(df1,'emotionless_tweets.csv')
