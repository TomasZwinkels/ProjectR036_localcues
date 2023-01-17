##########################################################################
##########################################################################
###                 Tweets and Local Cues Analyis                      ###
###                           including                                ### 
### Reliability Check of Geographic Dictionaries Application to Tweets ###
##########################################################################
##########################################################################

# Authors: Oliver Huwyler and Tomas Turner-Zwinkels

# change the language and date formatting to English if it is not already
Sys.setenv(LANG = "EN")
Sys.setlocale("LC_TIME", "English") # key, without this conversion to POSIXct does not work
Sys.getlocale(category = "LC_ALL")

# Set working directory
#setwd("C:/Users/Oliver/Desktop/Paper - Twitter Geographic Cues")
setwd("I:/Netzlaufwerk UNIBAS 2021-08-29/Analyses/Paper - Twitter Geographic Cues")
setwd("E:/Netzlaufwerk UNIBAS 2021-08-29/Analyses/Paper - Twitter Geographic Cues")
setwd("D:/Netzlaufwerk UNIBAS 2021-08-29/Analyses/Paper - Twitter Geographic Cues")
setwd("F:/PolCa/Analysis/R/ProjectR036_localcues")

# Install packages if necessary
if('rgdal' %in% rownames(installed.packages()) == FALSE) {install.packages('rgdal')}
if('rgeos' %in% rownames(installed.packages()) == FALSE) {install.packages('rgeos')}
if('sp' %in% rownames(installed.packages()) == FALSE) {install.packages('sp')}
if('tidyverse' %in% rownames(installed.packages()) == FALSE) {install.packages('tidyverse')}
if('sqldf' %in% rownames(installed.packages()) == FALSE) {install.packages('sqldf')}
if('plyr' %in% rownames(installed.packages()) == FALSE) {install.packages('plyr')}

if('readtext' %in% rownames(installed.packages()) == FALSE) {install.packages('readtext')}
if('quanteda' %in% rownames(installed.packages()) == FALSE) {install.packages('quanteda')}
if('readr' %in% rownames(installed.packages()) == FALSE) {install.packages('readr')}
if('formatR' %in% rownames(installed.packages()) == FALSE) {install.packages('formatR')}
if('Hmisc' %in% rownames(installed.packages()) == FALSE) {install.packages('Hmisc')}
if('XML' %in% rownames(installed.packages()) == FALSE) {install.packages('XML')}
if('mgsub' %in% rownames(installed.packages()) == FALSE) {install.packages('mgsub')}
if('readxl' %in% rownames(installed.packages()) == FALSE) {install.packages('readxl')}
if('purrr' %in% rownames(installed.packages()) == FALSE) {install.packages('purrr')}
if('xlsx' %in% rownames(installed.packages()) == FALSE) {install.packages('xlsx')}
if('ggplot2' %in% rownames(installed.packages()) == FALSE) {install.packages('ggplot2')}
if('cowplot' %in% rownames(installed.packages()) == FALSE) {install.packages('cowplot')}
if('openxlsx' %in% rownames(installed.packages()) == FALSE) {install.packages('openxlsx')}
if('foreign' %in% rownames(installed.packages()) == FALSE) {install.packages('foreign')}
if('stringr' %in% rownames(installed.packages()) == FALSE) {install.packages('stringr')}
if('stringi' %in% rownames(installed.packages()) == FALSE) {install.packages('stringi')}
if('dplyr' %in% rownames(installed.packages()) == FALSE) {install.packages('dplyr')}
if('data.table' %in% rownames(installed.packages()) == FALSE) {install.packages('data.table')}
if('lubridate' %in% rownames(installed.packages()) == FALSE) {install.packages('lubridate')}
if('tidyr' %in% rownames(installed.packages()) == FALSE) {install.packages('tidyr')}
if('scales' %in% rownames(installed.packages()) == FALSE) {install.packages('scales')}
if('lme4' %in% rownames(installed.packages()) == FALSE) {install.packages('lme4')}
if('stargazer' %in% rownames(installed.packages()) == FALSE) {install.packages('stargazer')}
if('sjPlot' %in% rownames(installed.packages()) == FALSE) {install.packages('sjPlot')}
if('sjstats' %in% rownames(installed.packages()) == FALSE) {install.packages('sjstats')}
if('ggpubr' %in% rownames(installed.packages()) == FALSE) {install.packages('ggpubr')}
if('dotwhisker' %in% rownames(installed.packages()) == FALSE) {install.packages('dotwhisker')}
if('zoo' %in% rownames(installed.packages()) == FALSE) {install.packages('zoo')}
if('gtools' %in% rownames(installed.packages()) == FALSE) {install.packages('gtools')}

if('car' %in% rownames(installed.packages()) == FALSE) {install.packages('car')}



# Load packages
library(rgdal)
library(rgeos)
library(sp)
library(tidyverse)
library(sqldf)
library(plyr)

library(readtext)
library(quanteda)
library(readr)
library(formatR)
library(Hmisc)
library(XML)
library(mgsub)
library(readxl)
library(purrr)
library(xlsx)
library(ggplot2)
library(cowplot)
library(openxlsx)
library(foreign)
library(stringr)
library(stringi)
library(dplyr)
library(data.table)
library(lubridate)
library(tidyr)
library(scales)
library(lme4)
library(stargazer)
library(sjPlot)
library(sjstats)
library(ggpubr)
library(dotwhisker)
library(zoo)
library(gtools)

library(car)

####################
# Custom functions #
####################

substrRight <- function(x, n)
{
  substr(x, nchar(x)-n+1, nchar(x))
}	


# Function for reordering column positions
# Source: https://stackoverflow.com/questions/5620885/how-does-one-reorder-columns-in-a-data-frame

##arrange df vars by position
##'vars' must be a named vector, e.g. c("var.name"=1)
arrange.vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}


#########################
# Load Coded Tweet Data #
#########################


# CH
CH_TWEE <- read_xlsx("./TWEETS/CHTWEETS_Summary_2020_03_04_final_version.xlsx", sheet = "Tweets")
CH_HITS <- read_xlsx("./TWEETS/CHTWEETS_Summary_2020_03_04_final_version.xlsx", sheet = "Hits")

TWEE_CH_TWEE <- data.frame(CH_TWEE)
TWEE_CH_HITS <- data.frame(CH_HITS)
head(TWEE_CH_TWEE)
head(TWEE_CH_HITS)

colnames(TWEE_CH_HITS)[which(names(TWEE_CH_HITS) == "False.Positive?")] <- "false_positive"
colnames(TWEE_CH_HITS)[which(names(TWEE_CH_HITS) == "False.Positive.")] <- "false_positive"
TWEE_CH_HITS$false_positive <- trimws(TWEE_CH_HITS$false_positive)

# DE

# 2017
DE_TWEE_2017 <- read_xlsx("./TWEETS/DETWEETS2017_Summary_2020-04-14_final_version.xlsx", sheet = "Tweets")
DE_HITS_2017 <- read_xlsx("./TWEETS/DETWEETS2017_Summary_2020-04-14_final_version.xlsx", sheet = "Hits")

# 2013
DE_TWEE_2013 <- read_xlsx("./TWEETS/DETWEETS2013_Summary_2020-04-08_final_version.xlsx", sheet = "Tweets")
DE_HITS_2013 <- read_xlsx("./TWEETS/DETWEETS2013_Summary_2020-04-08_final_version.xlsx", sheet = "Hits")

# 2009
DE_TWEE_2009 <- read_xlsx("./TWEETS/DETWEETS2009_Summary_final_version.xlsx", sheet = "Tweets")
DE_HITS_2009 <- read_xlsx("./TWEETS/DETWEETS2009_Summary_final_version.xlsx", sheet = "Hits")

# 2005
DE_TWEE_2005 <- read_xlsx("./TWEETS/DETWEETS2005_Summary_final_version.xlsx", sheet = "Tweets")
DE_HITS_2005 <- read_xlsx("./TWEETS/DETWEETS2005_Summary_final_version.xlsx", sheet = "Hits")


## Load the random sample coded by Jana (750 Tweets per country)
#RS_CH_TWEE <- read_xlsx("./TWEETS/Manual_Checks/Tweets_Random_Sample_2020-18-05.xlsx", sheet = "CHTWEETS")
#RS_DE_TWEE <- read_xlsx("./TWEETS/Manual_Checks/Tweets_Random_Sample_2020-18-05.xlsx", sheet = "DETWEETS")

# Update 13/02/2022: Version of Tweets_Random_Sample_2022-02-13.xlsx contains a classification of false-negatives
RS_TWEE <- read_xlsx("./TWEETS/Manual_Checks/Tweets_Random_Sample_2022-02-13.xlsx", sheet = "TweetSample")


#################
# Load PCC Data #
#################

# Load all PCC data, except 'modules': see codebook at https://www.overleaf.com/read/fhykbgcjsmdn


## Data on politicians

# Load POLI - politician level data: basic not time varying biographical information like gender
POLI = read.csv("./pcc_csvs/data_2022-03-28_1554/POLI.csv", header = TRUE, sep = ";")

# Some fixes to make our lives easier below
POLI$twitter_screen_name <- as.character(POLI$twitter_screen_name)
POLI$twitter_screen_name[which(nchar(POLI$twitter_screen_name) == 0)] <- NA
POLI$twitter_id <- as.character(POLI$twitter_id)
POLI$twitter_id[which(nchar(POLI$twitter_id) == 0)] <- NA

#	summary(POLI)
names(POLI)
head(POLI)
nrow(POLI)

# Load RESE - resume entries; so (political) jobs, interest group ties, eppisodes in parliaments, educational episodes e.t.s.
RESE = read.csv("./pcc_csvs/data_2022-03-28_1554/RESE.csv", header = TRUE, sep = ";")
#	summary(RESE)
names(RESE)
head(RESE)

# Load MEME - membership eppisodes; what parties where polticians a member of when
MEME = read.csv("./pcc_csvs/data_2022-03-28_1554/MEME.csv", header = TRUE, sep = ";")
#	summary(MEME)
names(MEME)
head(MEME)
nrow(MEME)

# Load ELEN - election list entries; where where politicians on what election lists?
ELEN = read.csv("./pcc_csvs/data_2022-03-28_1554/ELEN.csv", header = TRUE, sep = ";")
summary(ELEN)
names(ELEN)
head(ELEN)
nrow(ELEN)

## about institutions and other (institutional) contexts

# Load PARL - all information on the level of parliaments, for example their first day in session
PARL = read.csv("./pcc_csvs/data_2022-03-28_1554/PARL.csv", header = TRUE, sep = ";")
#	summary(PARL)
names(PARL)
head(PARL)
nrow(PARL)

# Load PART, all information on parties; connects to ParlGov when applicable
PART = read.csv("./pcc_csvs/data_2022-03-28_1554/PART.csv", header = TRUE, sep = ";")
#	summary(PART)
names(PART)
head(PART)
nrow(PART)

# Load ELDI, information on election districts, connects to CLEA when applicable
ELDI = read.csv("./pcc_csvs/data_2022-03-28_1554/ELDI.csv", header = TRUE, sep = ";")
#	summary(ELDI)
names(ELDI)
head(ELDI)
nrow(ELDI)

### and two other election related data-frames

# Load ELEC, information on the level of elections, for example the date on which the election took place
ELEC = read.csv("./pcc_csvs/data_2022-03-28_1554/ELEC.csv", header = TRUE, sep = ";")
#		summary(ELEC)
names(ELEC)
head(ELEC)
nrow(ELEC)

# Load ELLI, information on the level of election lists, for example the party_id (see PART) that produced this list
ELLI = read.csv("./pcc_csvs/data_2022-03-28_1554/ELLI.csv", header = TRUE, sep = ";")
#		summary(ELLI)
names(ELLI)
head(ELLI)
nrow(ELLI)


#################################
# Load Additional Election Data #
#################################

# 1) Panachage statistics for the Swiss National Council
PANACH <- read_excel("Data/Panaschierstatistik/Output/Nationalrat_Panaschierstimmen_2007-19_2022-06-07_1312.xlsx")

# 2) Election results for Swiss Council of States (Upper Chamber)
SRELEC <- read_excel("Data/Ständeratskandidierende/su-d-17.02.03.02.xlsx")
SRELEC$`1. Wahlgang Wahltag` <- as.Date(as.numeric(SRELEC$`1. Wahlgang Wahltag`), origin = "1899-12-30")
SRELEC$`2. Wahlgang Wahltag` <- as.Date(as.numeric(SRELEC$`2. Wahlgang Wahltag`), origin = "1899-12-30")

# 3) Candidates for the Swiss National Council 2019
NRCAND19 <- read_excel("Data/KandidierendeNR2019/sd-t-17.02-NRW2019-kandidierende-APPENDIX.xlsx")



#######################################
# Prepare Coded Tweets: Complete Data #
#######################################

# What Tomas implemented:
# For each tweet figure out if their are <any> local cues this is the case when:
# - tweets_local_cues contains a value
# - when the tweetnumber district match does not occur in the list of false positives
# - an MPs its constituency is the same as the local cue found

# Note from the instructions:
# 1) In the sheet called HITS: Create unique entries per Tweet number (i.e. the variable "docname") where 
#    the information on local cues (variable "False Positive?") is either yes or no, and not both.
# 2) In the sheet called HITS: Get rid of the element "text" added to the numbers in the column "docname".
# 3) To classify Tweets: Use TWEETS$tweetnumb and HITS$docname for matching purposes.

####
# 1) Create dataframe IDs
CH_TWEE$df_id <- "CH"
CH_HITS$df_id <- "CH"

DE_TWEE_2005$df_id <- "DE_2005"
DE_HITS_2005$df_id <- "DE_2005"
DE_TWEE_2009$df_id <- "DE_2009"
DE_HITS_2009$df_id <- "DE_2009"
DE_TWEE_2013$df_id <- "DE_2013"
DE_HITS_2013$df_id <- "DE_2013"
DE_TWEE_2017$df_id <- "DE_2017"
DE_HITS_2017$df_id <- "DE_2017"


# 2) Combine TWEE and HIT dataframes
dplyr::bind_rows(CH_TWEE, DE_TWEE_2005, DE_TWEE_2009, DE_TWEE_2013, DE_TWEE_2017) -> TWEETSCODED
dplyr::bind_rows(CH_HITS, DE_HITS_2005, DE_HITS_2009, DE_HITS_2013, DE_HITS_2017) -> HITS

# 3) Reduce the HITS df to single Tweets
colnames(HITS)[match("False Positive?",colnames(HITS))] <- "false_positive" # rename to false_positive
HITSRED <- sqldf("SELECT df_id, tweetnumb, group_concat(false_positive) 'false_positive' from HITS group by tweetnumb, df_id")

# 4) Remove duplicates within the false_positive string
# Note: An entry only qualifies as a false-positive if there is only a yes value
HITSRED$false_positive <- sapply(strsplit(str_squish(HITSRED$false_positive), ","), function(x) paste(sort(unique(rle(x)$values)), collapse=","))

# 5) Add the information on false_positives to TWEETSCODED
TWEETSCODED <- sqldf("SELECT TWEETSCODED.*, HITSRED.false_positive AS false_positive
                 FROM TWEETSCODED LEFT JOIN HITSRED
                 ON
                 (HITSRED.tweetnumb = TWEETSCODED.tweetnumb)
                 AND
                 (HITSRED.df_id = TWEETSCODED.df_id)
                 ")


# Update 23/03/2022: Tomas discovered that some Tweets got erroneously duplicated when information on candidacy of the MP
# where added. Concatenate information on the level of the tweet_id and then remove duplicate information within strings.
TWEETSCODED$DistrictMatch <- gsub("\\|", ";", TWEETSCODED$DistrictMatch)
TWEETSCODED$WKR_in_LAND <- gsub("\\|", ";", TWEETSCODED$WKR_in_LAND)
TWEETSCODED$RegionMatch <- gsub("\\|", ";", TWEETSCODED$RegionMatch)


TWEETSCODED <- sqldf("SELECT df_id, pers_id, country, twitter_user_id, tweet_id, tweet_timestamp, tweet_timestampR,text, retweet_count, favorite_count,
                    media_urls, links, tweet_type, related_id, related_text, related_timestamp, related_user_id, reply_to, related_media_urls, related_links,
                    test_original,
                    group_concat(canton,';') 'canton', group_concat(tweetnumb,';') 'tweetnumb',
                    group_concat(tweets_local_cues,';') 'tweets_local_cues', group_concat(DistrictMatch,';') 'DistrictMatch',
                    group_concat(candidacies,';') 'candidacies', group_concat(mandate,';') 'mandate',
                    group_concat(WKR_NUMMER,';') 'WKR_NUMMER', group_concat(WKR_NAME,';') 'WKR_NAME',
                    group_concat(WKR_LAND,';') 'WKR_LAND', group_concat(LISTE_LAND,';') 'LISTE_LAND',
                    group_concat(LAND_NR,';') 'LAND_NR', group_concat(WKR_in_LAND,';') 'WKR_in_LAND',
                    group_concat(RegionMatch,';') 'RegionMatch', group_concat(false_positive,';') 'false_positive'
                    from TWEETSCODED group by pers_id, country, twitter_user_id, tweet_id, tweet_timestamp, tweet_timestampR, df_id")

TWEETSCODED$tweet_id <- as.character(TWEETSCODED$tweet_id)

# 4) Remove duplicates in concatenated strings
TWEETSCODED$canton <- sapply(strsplit(str_squish(TWEETSCODED$canton), ";"), function(x) paste(sort(unique(rle(x)$values)), collapse=";"))
TWEETSCODED$tweetnumb <- sapply(strsplit(str_squish(TWEETSCODED$tweetnumb), ";"), function(x) paste(sort(unique(rle(x)$values)), collapse=";"))
TWEETSCODED$tweets_local_cues <- sapply(strsplit(str_squish(TWEETSCODED$tweets_local_cues), ";"), function(x) paste(sort(unique(rle(x)$values)), collapse=";"))
TWEETSCODED$DistrictMatch <- sapply(strsplit(str_squish(TWEETSCODED$DistrictMatch), ";"), function(x) paste(sort(unique(rle(x)$values)), collapse=";"))
TWEETSCODED$candidacies <- sapply(strsplit(str_squish(TWEETSCODED$candidacies), ";"), function(x) paste(sort(unique(rle(x)$values)), collapse=";"))
TWEETSCODED$mandate <- sapply(strsplit(str_squish(TWEETSCODED$mandate), ";"), function(x) paste(sort(unique(rle(x)$values)), collapse=";"))
TWEETSCODED$WKR_NUMMER <- sapply(strsplit(str_squish(TWEETSCODED$WKR_NUMMER), ";"), function(x) paste(sort(unique(rle(x)$values)), collapse=";"))
TWEETSCODED$WKR_NAME <- sapply(strsplit(str_squish(TWEETSCODED$WKR_NAME), ";"), function(x) paste(sort(unique(rle(x)$values)), collapse=";"))
TWEETSCODED$WKR_LAND <- sapply(strsplit(str_squish(TWEETSCODED$WKR_LAND), ";"), function(x) paste(sort(unique(rle(x)$values)), collapse=";"))
TWEETSCODED$LISTE_LAND <- sapply(strsplit(str_squish(TWEETSCODED$LISTE_LAND), ";"), function(x) paste(sort(unique(rle(x)$values)), collapse=";"))
TWEETSCODED$LAND_NR <- sapply(strsplit(str_squish(TWEETSCODED$LAND_NR), ";"), function(x) paste(sort(unique(rle(x)$values)), collapse=";"))
TWEETSCODED$WKR_in_LAND <- sapply(strsplit(str_squish(TWEETSCODED$WKR_in_LAND), ";"), function(x) paste(sort(unique(rle(x)$values)), collapse=";"))
TWEETSCODED$RegionMatch <- sapply(strsplit(str_squish(TWEETSCODED$RegionMatch), ";"), function(x) paste(sort(unique(rle(x)$values)), collapse=";"))
TWEETSCODED$false_positive <- sapply(strsplit(str_squish(TWEETSCODED$false_positive), ";"), function(x) paste(sort(unique(rle(x)$values)), collapse=";"))


# 5) Remove semicolons at the beginning of strings
TWEETSCODED$canton<-gsub("^;", "",TWEETSCODED$canton)
TWEETSCODED$tweetnumb<-gsub("^;", "",TWEETSCODED$tweetnumb)
TWEETSCODED$tweets_local_cues<-gsub("^;", "",TWEETSCODED$tweets_local_cues)
TWEETSCODED$DistrictMatch<-gsub("^;", "",TWEETSCODED$DistrictMatch)
TWEETSCODED$candidacies<-gsub("^;", "",TWEETSCODED$candidacies)
TWEETSCODED$mandate<-gsub("^;", "",TWEETSCODED$mandate)
TWEETSCODED$WKR_NUMMER<-gsub("^;", "",TWEETSCODED$WKR_NUMMER)
TWEETSCODED$WKR_NAME<-gsub("^;", "",TWEETSCODED$WKR_NAME)
TWEETSCODED$WKR_LAND<-gsub("^;", "",TWEETSCODED$WKR_LAND)
TWEETSCODED$LISTE_LAND<-gsub("^;", "",TWEETSCODED$LISTE_LAND)
TWEETSCODED$LAND_NR<-gsub("^;", "",TWEETSCODED$LAND_NR)
TWEETSCODED$WKR_in_LAND<-gsub("^;", "",TWEETSCODED$WKR_in_LAND)
TWEETSCODED$RegionMatch<-gsub("^;", "",TWEETSCODED$RegionMatch)
TWEETSCODED$false_positive<-gsub("^;", "",TWEETSCODED$false_positive)




#######################################
# Prepare Coded Tweets: Random Sample #
#######################################

# 1) Combine the two random sample dfs
#RS_CH_TWEE$country <- "CH"
#RS_DE_TWEE$country <- "DE"

#dplyr::bind_rows(RS_CH_TWEE, RS_DE_TWEE) -> RS_TWEE

#RS_TWEE$row_id <- seq(nrow(RS_TWEE))

# 2) Add information on the result from automated local cues coding

#RS_TWEE <- sqldf("SELECT RS_TWEE.*, TWEETSCODED.false_positive AS false_positive
#                 FROM RS_TWEE LEFT JOIN TWEETSCODED
#                 ON
#                 (TWEETSCODED.pers_id = RS_TWEE.pers_id)
#                 AND
#                 (TWEETSCODED.text = RS_TWEE.text)
#                 ")

# 3) Keep only unique rows
#RS_TWEE <- sqldf("SELECT DISTINCT RS_TWEE.*
#                        FROM RS_TWEE
#                  ")

# 4) Find the one row that has been duplicated and remove it
#ROWCOUNT <- as.data.frame(table(RS_TWEE$row_id))
#rm(ROWCOUNT)

#RS_TWEE <- RS_TWEE[-which(grepl("^Ob das so klug war Peter Tauber", RS_TWEE$text) & RS_TWEE$pers_id == "DE_Miazga_Corinna_1983" & is.na(RS_TWEE$false_positive)),]



#################################
# Descriptive Statistics Tables #
#################################

## All data (TWEETSCODED)
# 1a) Preparation for count
TWEETSCODED[(TWEETSCODED$false_positive)=="",]$false_positive <- "negative" # no local cues found with dictionary approach

# b) Create an overarching tweetislocalcue variable
TWEETSCODED$tweetislocalcue <- ""
TWEETSCODED[(TWEETSCODED$false_positive)=="negative",]$tweetislocalcue <- FALSE # no local cues found with dictionary approach
TWEETSCODED[TWEETSCODED$false_positive=="No,Yes",]$tweetislocalcue <-  TRUE
TWEETSCODED[TWEETSCODED$false_positive=="No",]$tweetislocalcue <-  TRUE
TWEETSCODED[TWEETSCODED$false_positive=="Yes",]$tweetislocalcue <-  FALSE


# 2) Count
TOT_TWEET_COUNT <- sqldf("SELECT false_positive AS local_cues, country, COUNT(false_positive) as count
                          from TWEETSCODED GROUP BY false_positive, country")

# 3) Merge the No,Yes and No category (as those hits that contain a "no" are true hits, i.e. true positives)
TOT_TWEET_COUNT[TOT_TWEET_COUNT$local_cues=="No,Yes",]$local_cues <-  "true positive"
TOT_TWEET_COUNT[TOT_TWEET_COUNT$local_cues=="No",]$local_cues <-  "true positive"
TOT_TWEET_COUNT[TOT_TWEET_COUNT$local_cues=="Yes",]$local_cues <-  "false positive"


# 4) Aggregate
TOT_TWEET_COUNT <- as.data.frame(aggregate(TOT_TWEET_COUNT$count, by=list(country=TOT_TWEET_COUNT$country, local_cues=TOT_TWEET_COUNT$local_cues), FUN=sum))
colnames(TOT_TWEET_COUNT)[match("x",colnames(TOT_TWEET_COUNT))] <- "count" # rename to count



### Added 17/02/2022 during a meeting with Tomas
# An alternative count by dataframe instead of country (to compare with Tomas' local cues measure from his script R036.R)
# 1) Count
TOT_TWEET_COUNT_ALT <- sqldf("SELECT false_positive AS local_cues, df_id, COUNT(false_positive) as count
                          from TWEETSCODED GROUP BY false_positive, df_id")

# 2) Merge the No,Yes and No category (as those hits that contain a "no" are true hits, i.e. true positives)
TOT_TWEET_COUNT_ALT[TOT_TWEET_COUNT_ALT$local_cues=="No,Yes",]$local_cues <-  "true positive"
TOT_TWEET_COUNT_ALT[TOT_TWEET_COUNT_ALT$local_cues=="No",]$local_cues <-  "true positive"
TOT_TWEET_COUNT_ALT[TOT_TWEET_COUNT_ALT$local_cues=="Yes",]$local_cues <-  "false positive"

# 3) Aggregate
TOT_TWEET_COUNT_ALT <- as.data.frame(aggregate(TOT_TWEET_COUNT_ALT$count, by=list(df_id=TOT_TWEET_COUNT_ALT$df_id, local_cues=TOT_TWEET_COUNT_ALT$local_cues), FUN=sum))
colnames(TOT_TWEET_COUNT_ALT)[match("x",colnames(TOT_TWEET_COUNT_ALT))] <- "count" # rename to count


## Random sample (RS_TWEE)
# 1) Preparation for count
# a) NAs to negative for information from the automatic matching part
colnames(RS_TWEE)[match("false_positive",colnames(RS_TWEE))] <- "local_cue_dict" # rename to false_positive
RS_TWEE[is.na(RS_TWEE$local_cue_dict)==T,]$local_cue_dict <- "negative" # no local cues found with dictionary approach

# b) Recode other automatically assigned codes
RS_TWEE[RS_TWEE$local_cue_dict=="No,Yes",]$local_cue_dict <-  "positive"
RS_TWEE[RS_TWEE$local_cue_dict=="No",]$local_cue_dict <-  "positive"
RS_TWEE[RS_TWEE$local_cue_dict=="Yes",]$local_cue_dict <-  "negative"

# c) Create a new variable that combines hand-coded and dictionary-coded information on local cues
RS_TWEE$local_cues <- ""
RS_TWEE[RS_TWEE$local_cue_dict=="negative" & RS_TWEE$LocalCue == "No",]$local_cues <- "true negative" 
RS_TWEE[RS_TWEE$local_cue_dict=="negative" & RS_TWEE$LocalCue == "Yes",]$local_cues <- "false negative" 
RS_TWEE[RS_TWEE$local_cue_dict=="positive" & RS_TWEE$LocalCue == "Yes",]$local_cues <- "true positive" 
RS_TWEE[RS_TWEE$local_cue_dict=="positive" & RS_TWEE$LocalCue == "No",]$local_cues <- "false positive" 


# 3) Export RS_TWEE
#completefilename <- "./False_Positive_Checks/Manual_Checks/Tweets_Random_Sample_2022-02-13.xlsx"
#write.xlsx(RS_TWEE, file=completefilename, sheetName="TweetSample", row.names=FALSE)

# 4) Count
# a) Local cues in general
RS_TWEET_COUNT <- sqldf("SELECT local_cues, country, COUNT(local_cues) as count
                          from RS_TWEE GROUP BY local_cues, country")

# b) Categories for false-negatives
RS_FAL_NEG <- sqldf("SELECT reasoning_category, country, COUNT(reasoning_category) as count
                          from RS_TWEE GROUP BY reasoning_category, local_cues, country")


# 5) Reduce RS_FAL_NEG
RS_FAL_NEG[grepl("link", RS_FAL_NEG$reasoning_category),]$reasoning_category <- "cue in link or picture"

RS_FAL_NEG <- as.data.frame(aggregate(RS_FAL_NEG$count, by=list(country=RS_FAL_NEG$country, reasoning_category=RS_FAL_NEG$reasoning_category), FUN=sum))
colnames(RS_FAL_NEG)[match("x",colnames(RS_FAL_NEG))] <- "count" # rename to count


###########
# Figures #
###########


# Plotting has to be done for: TOT_TWEET_COUNT, RS_TWEET_COUNT, and RS_FAL_NEG




#######################
### TOT_TWEET_COUNT ###
#######################

# 1) Calculate percentages
TOT_TWEET_COUNT$pct <- ""

for (i in 1:nrow(TOT_TWEET_COUNT[TOT_TWEET_COUNT$country == "DE",]))
{
  TOT_TWEET_COUNT[TOT_TWEET_COUNT$country == "DE",]$pct[i] <- (TOT_TWEET_COUNT[TOT_TWEET_COUNT$country == "DE",]$count[i] / sum(TOT_TWEET_COUNT[TOT_TWEET_COUNT$country == "DE",]$count))*100
}


for (i in 1:nrow(TOT_TWEET_COUNT[TOT_TWEET_COUNT$country == "CH",]))
{
  TOT_TWEET_COUNT[TOT_TWEET_COUNT$country == "CH",]$pct[i] <- (TOT_TWEET_COUNT[TOT_TWEET_COUNT$country == "CH",]$count[i] / sum(TOT_TWEET_COUNT[TOT_TWEET_COUNT$country == "CH",]$count))*100
}

TOT_TWEET_COUNT$pct <- as.numeric(TOT_TWEET_COUNT$pct)


# 2) Create factor variables
TOT_TWEET_COUNT$local_cues <- factor(TOT_TWEET_COUNT$local_cues, 
                                     levels = c("negative", "true positive", "false positive"))
TOT_TWEET_COUNT$local_cues <- factor(TOT_TWEET_COUNT$local_cues, levels=rev(levels(TOT_TWEET_COUNT$local_cues))) # Reverse the order of the factor level


TOT_TWEET_COUNT[TOT_TWEET_COUNT$country == "DE",]


# 3) Plot the stacked bar plot manually by IGs for tactics
# Note: All the negatives
alltweets <- ggplot(TOT_TWEET_COUNT[-which(TOT_TWEET_COUNT$local_cues == "negative"),], aes(fill=local_cues, y=pct, x=country)) + 
  geom_bar(position="stack", stat="identity", colour="black") +
  scale_fill_manual(values =  c('#d7191c',
                                '#2c7bb6'), guide = guide_legend(reverse = TRUE)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.title.y=element_blank(),
        axis.text.y = element_text(size = 14, colour="black"), # Increase x axis label font size
        axis.title.x = element_text(size = 14, colour="black"), # Increase y axis title font size
        axis.text.x = element_text(size = 12, colour = "black"), # Increase x axis label font size
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey80"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey80")) +
  scale_y_continuous("Tweets (%)", position = "right") + 
  scale_x_discrete(position = "bottom", labels=c("CH" = "Switzerland", "DE" = "Germany"))+
  coord_flip()

alltweets



# Obtain the plot without the legend
alltweets_noleg <- alltweets + theme(legend.position = "none")

# Obtain the legend separately
legend <- cowplot::get_legend(alltweets)

# Plot with cowplot
cowplot::plot_grid(alltweets_noleg, legend, nrow = 2, rel_heights = c(0.7, 0.2))
cowplot::plot_grid(alltweets_noleg, legend, nrow = 2, rel_heights = c(0.7, 0.2)) -> alltweets2


# Export the figure
completefilename <- paste("./Export/Figures/AllTweets_Fal-Tru_Positives_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
#tiff(completefilename,height = 6, width = 14.85, units = "cm", compression = "lzw", res = 1200)
png(completefilename,height = 6, width = 14.85, units = "cm", res = 1200)
alltweets2
dev.off() 




######################
### RS_TWEET_COUNT ###
######################

# 1) Calculate percentages
RS_TWEET_COUNT$pct <- ""

for (i in 1:nrow(RS_TWEET_COUNT[RS_TWEET_COUNT$country == "DE",]))
{
  RS_TWEET_COUNT[RS_TWEET_COUNT$country == "DE",]$pct[i] <- (RS_TWEET_COUNT[RS_TWEET_COUNT$country == "DE",]$count[i] / sum(RS_TWEET_COUNT[RS_TWEET_COUNT$country == "DE",]$count))*100
}


for (i in 1:nrow(RS_TWEET_COUNT[RS_TWEET_COUNT$country == "CH",]))
{
  RS_TWEET_COUNT[RS_TWEET_COUNT$country == "CH",]$pct[i] <- (RS_TWEET_COUNT[RS_TWEET_COUNT$country == "CH",]$count[i] / sum(RS_TWEET_COUNT[RS_TWEET_COUNT$country == "CH",]$count))*100
}

RS_TWEET_COUNT$pct <- as.numeric(RS_TWEET_COUNT$pct)


# 2) Create factor variables
RS_TWEET_COUNT$local_cues <- factor(RS_TWEET_COUNT$local_cues, 
                                    levels = c("true positive", "true negative", "false positive", "false negative"))
RS_TWEET_COUNT$local_cues <- factor(RS_TWEET_COUNT$local_cues, levels=rev(levels(RS_TWEET_COUNT$local_cues))) # Reverse the order of the factor level


RS_TWEET_COUNT[RS_TWEET_COUNT$country == "DE",]


# 3) Plot the stacked bar plot manually by IGs for tactics
# Note: All the negatives
sampletweets_classify <- ggplot(RS_TWEET_COUNT, aes(fill=local_cues, y=pct, x=country)) + 
  geom_bar(position="stack", stat="identity", colour="black") +
  # scale_fill_hue(guide = guide_legend(reverse = TRUE)) + # Flip the order of the legend
    scale_fill_manual(values =  c('#d7191c',
                                  '#fdae61',
                                  '#abd9e9',
                                  '#2c7bb6'), guide = guide_legend(reverse = TRUE)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        #legend.position = c(0, 0.2),
        #legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        axis.title.y=element_blank(),
        axis.text.y = element_text(size = 14, colour="black"), # Increase x axis label font size
        axis.title.x = element_text(size = 14, colour="black"), # Increase y axis title font size
        axis.text.x = element_text(size = 12, colour = "black"), # Increase x axis label font size
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey80"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey80")) +
  scale_y_continuous("Tweets (%)", position = "right") + 
  scale_x_discrete(position = "bottom", labels=c("CH" = "Switzerland", "DE" = "Germany"))+
  coord_flip()

sampletweets_classify

# Obtain the plot without the legend
sampletweets_classify_noleg <- sampletweets_classify + theme(legend.position = "none")

# Obtain the legend separately
legend <- cowplot::get_legend(sampletweets_classify)

# Plot with cowplot
cowplot::plot_grid(sampletweets_classify_noleg, legend, nrow = 2, rel_heights = c(0.7, 0.2))
cowplot::plot_grid(sampletweets_classify_noleg, legend, nrow = 2, rel_heights = c(0.7, 0.2)) -> sampletweets_classify2

# Export the figure
completefilename <- paste("./Export/Figures/SampleTweets_Classification_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
#tiff(completefilename,height = 6, width = 14.85, units = "cm", compression = "lzw", res = 1200)
png(completefilename,height = 6, width = 14.85, units = "cm", res = 1200)
sampletweets_classify2
dev.off() 






##################
### RS_FAL_NEG ###
##################

# 1) Calculate percentages
RS_FAL_NEG$pct <- ""

for (i in 1:nrow(RS_FAL_NEG[RS_FAL_NEG$country == "DE",]))
{
  RS_FAL_NEG[RS_FAL_NEG$country == "DE",]$pct[i] <- (RS_FAL_NEG[RS_FAL_NEG$country == "DE",]$count[i] / sum(RS_FAL_NEG[RS_FAL_NEG$country == "DE",]$count))*100
}


for (i in 1:nrow(RS_FAL_NEG[RS_FAL_NEG$country == "CH",]))
{
  RS_FAL_NEG[RS_FAL_NEG$country == "CH",]$pct[i] <- (RS_FAL_NEG[RS_FAL_NEG$country == "CH",]$count[i] / sum(RS_FAL_NEG[RS_FAL_NEG$country == "CH",]$count))*100
}

RS_FAL_NEG$pct <- as.numeric(RS_FAL_NEG$pct)


# 2) Create factor variables
RS_FAL_NEG$reasoning_category <- factor(RS_FAL_NEG$reasoning_category, 
                                        levels = c("geographic constituency", "geographical location", "building or facility",
                                                   "individual", "organization", "event", "cue in link or picture"))
RS_FAL_NEG$reasoning_category <- factor(RS_FAL_NEG$reasoning_category, levels=rev(levels(RS_FAL_NEG$reasoning_category))) # Reverse the order of the factor level


RS_FAL_NEG[RS_FAL_NEG$country == "DE",]


# 3) Plot the stacked bar plot manually by IGs for tactics
# Note: All the negatives
sampletweets_falneg <- ggplot(RS_FAL_NEG, aes(fill=reasoning_category, y=pct, x=country)) + 
  geom_bar(position="stack", stat="identity", colour="black") +
  # scale_fill_hue(guide = guide_legend(reverse = TRUE)) + # Flip the order of the legend
  scale_fill_manual(values =  c('#2166ac',
                                '#762a83',
                                '#af8dc3',
                                '#e7d4e8',
                                '#d9f0d3',
                                '#7fbf7b',
                                '#1b7837'),
                    guide = guide_legend(reverse = TRUE,
                                         nrow = 3, byrow = TRUE)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        #legend.position = c(0, 0.2),
        #legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        axis.title.y=element_blank(),
        axis.text.y = element_text(size = 14, colour="black"), # Increase x axis label font size
        axis.title.x = element_text(size = 14, colour="black"), # Increase y axis title font size
        axis.text.x = element_text(size = 12, colour = "black"), # Increase x axis label font size
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey80"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey80")) +
  scale_y_continuous("Tweets (%)", position = "right") + 
  scale_x_discrete(position = "bottom", labels=c("CH" = "Switzerland", "DE" = "Germany"))+
  coord_flip()

sampletweets_falneg

# Obtain the plot without the legend
sampletweets_falneg_noleg <- sampletweets_falneg + theme(legend.position = "none")

# Obtain the legend separately
legend <- cowplot::get_legend(sampletweets_falneg)

# Plot with cowplot
cowplot::plot_grid(sampletweets_falneg_noleg, legend, nrow = 2, rel_heights = c(0.3, 0.2))
cowplot::plot_grid(sampletweets_falneg_noleg, legend, nrow = 2, rel_heights = c(0.3, 0.2)) -> sampletweets_falneg2

# Export the figure
completefilename <- paste("./Export/Figures/SampleTweets_FalNeg_Categories_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
#tiff(completefilename,height = 8, width = 14.85, units = "cm", compression = "lzw", res = 1200)
png(completefilename,height = 8, width = 14.85, units = "cm", res = 1200)
sampletweets_falneg2
dev.off() 



#################
#################
# Main Analysis #
#################
#################

# Note: This part is taken in part from Tomas' script R036.R and adapted.


###################################
# MP-Month Data: Tenure & Chamber #
###################################

# For these data, we will rely on the RESE data frame.

# 1) We reduce RESE to only include information on parliamentary membership on the national level - also country filter is here!
RESERED <- RESE[which(RESE$pf_geolevel == "NT" & 
                        (RESE$pf_instdomain == "LE" | RESE$pf_instdomain == "LE-LH") &
                        RESE$pf_orglevel == "T3" & 
                        is.na(RESE$pf_policy_area) & 
                        RESE$pf_position == "01" &
                        (RESE$country == "CH" | RESE$country == "DE") 
),]

# b) Now, we further reduce it to only include MPs in TWEETSCODED
# Note: Relevant are those MPs who are in our TWEETSCODED sample.
relevantmps <- as.character(unique(TWEETSCODED$pers_id))

# b) Keep only relevant MPs in RESERED
RESERED <- RESERED[RESERED$pers_id %in% relevantmps, ]


# 2) Expand [[rcen]] entries from 05aug2019[[rcen]] to 01.12.2019 (the last day of the 2015-9 parliament)
RESERED[which(RESERED$res_entry_end=="05aug2019[[rcen]]"),]$res_entry_end <-  "01dec2019"


# 3) Make the dates readable for R
# a) Remove information on left ("[[lcen]]") and right censoring ("[[rcen]]")
RESERED$res_entry_start <- gsub("[[lcen]]", "", RESERED$res_entry_start, fixed = TRUE)
RESERED$res_entry_end <- gsub("[[rcen]]", "", RESERED$res_entry_end, fixed = TRUE)

# b) Transform dates to R format
RESERED$res_entry_start <- as.Date(RESERED$res_entry_start, format=c("%d%b%Y"))
RESERED$res_entry_end <- as.Date(RESERED$res_entry_end, format=c("%d%b%Y"))

# c) Remove rows that are (partly) NA for start and/or end date.
# Note: This is no problem because tenure information is complete on the day level.
# a) ... NAs on res_entry_start or res_entry_end
RESERED <- RESERED[which(!is.na(RESERED$res_entry_start)),]
RESERED <- RESERED[which(!is.na(RESERED$res_entry_end)),]

# Let's extract what chamber we're looking at: National Council (NR) or Council of States (SR)
RESERED$chamber <- as.factor(str_extract(RESERED$parliament_id, "(?<!^)(NR|SR|BT|TK|SE|DE)"))

head(RESERED)

# 4) Transpose these data to long format
# Note: The function breaks if the start date is after the end date
RESEREDLONG <- setDT(RESERED)[ , list(pers_id = pers_id, chamber = chamber,
               day = seq(as.Date(res_entry_start), as.Date(res_entry_end), by = "day")), by = 1:nrow(RESERED)]

RESEREDLONG$nrow <- NULL # drop nrow
head(RESEREDLONG)

# Get rid of duplicated rows
RESEREDLONG <- RESEREDLONG[!duplicated(RESEREDLONG),]

# 5) Order the data first according to "pers_id" and then to "day"
RESEREDLONG <-  with(RESEREDLONG, RESEREDLONG[order(pers_id, day) , ])

# 6) Now, we calculate tenure on a daily level
# To that end, we simply count (number) rows
RESEREDLONG$daysinparl <- ave(as.character(RESEREDLONG$pers_id), as.character(RESEREDLONG$pers_id), FUN = seq_along)

# 7) Calculate tenure
RESEREDLONG$tenure  <- as.numeric(RESEREDLONG$daysinparl)/365.2422 #365.2422 = Average duration of year

# 8) Remove all the numbers after the dot (no rounding)
RESEREDLONG$tenure  <- round(RESEREDLONG$tenure,digits=2)

# 9) Now, we prepare RESEREDLONG for aggregation on the MP-month level
# To that end, we generate a month variable like we did before.
RESEREDLONG$month <- str_extract(as.character(RESEREDLONG$day), "[0-9]{4}-[0-9]{2}") # [0-9]{4}-[0-9]{2} matches 4 digits followed by a hyphen and 2 digits

# Obtain the mode for tenure for every MP-month in the RESEREDLONG
# a) Get tenure mode by groups (pers_id, month) 

# custom function from Oliver
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

RESEREDMONTH <- aggregate(RESEREDLONG$tenure, by = list(pers_id = RESEREDLONG$pers_id, month = RESEREDLONG$month), getmode)
colnames(RESEREDMONTH)[match("x",colnames(RESEREDMONTH))] <- "tenure" # rename variable x to tenure

# b) Add the mode for the chamber (note: MPs could be in the upper chamber in CH)
RESEREDMONTH2 <- aggregate(RESEREDLONG$chamber, by = list(pers_id = RESEREDLONG$pers_id, month = RESEREDLONG$month), getmode)
colnames(RESEREDMONTH2)[match("x",colnames(RESEREDMONTH2))] <- "chamber" # rename variable x to chamber

# 10) Add the chamber to RESEREDMONTH
RESEREDMONTH <- sqldf("SELECT RESEREDMONTH.*, RESEREDMONTH2.chamber
                  FROM RESEREDMONTH LEFT JOIN RESEREDMONTH2
                  ON
                  ( RESEREDMONTH.pers_id = RESEREDMONTH2.pers_id )
                  AND
                  ( RESEREDMONTH.month = RESEREDMONTH2.month )
                  ")

rm(RESEREDMONTH2)


############################################
# MP-Month Data: Periods not in Parliament #
############################################

# 1) Take the unique months starting in 2009
months <- unique(RESEREDMONTH$month)

# 2) Retain only years > 2007
months <- months[which(as.numeric(str_extract(months, "[0-9]{4}")) > 2007)]

# 3) Generate permutations
COUNTERFACT <- expand.grid(relevantmps, months)
colnames(COUNTERFACT) <- c("pers_id", "month")

# 4) Add RESEREDMONTH to COUNTERFACT
# Note: The new RESEREDMONTH has the same length as COUNTERFACT
RESEREDMONTH <- sqldf("SELECT COUNTERFACT.*, RESEREDMONTH.tenure, RESEREDMONTH.chamber
                  FROM COUNTERFACT LEFT JOIN RESEREDMONTH
                  ON
                  ( RESEREDMONTH.pers_id = COUNTERFACT.pers_id )
                  AND
                  ( RESEREDMONTH.month = COUNTERFACT.month )
                  ")

rm(COUNTERFACT)

# 5) Fill empty row with information that the politician was not in office
RESEREDMONTH[which(is.na(RESEREDMONTH$chamber)),]$chamber <- "none"

# 6) Copy down tenure where it's missing (i.e. the MP is not in parliament)
RESEREDMONTH <- RESEREDMONTH[with(RESEREDMONTH, order(pers_id, month)), ]

RESEREDMONTH %>%
  group_by(pers_id) %>%
  mutate_all(funs(na.locf(., na.rm = FALSE))) -> RESEREDMONTH

# 7) Set the remaining tenure scores to 0
RESEREDMONTH[which(is.na(RESEREDMONTH$tenure)),]$tenure <- 0



#################################
# MP-Month Data: Gender & Party #
#################################

# 1) Add gender to RESEREDMONTH
RESEREDMONTH <- sqldf("SELECT RESEREDMONTH.*, POLI.gender
                    FROM RESEREDMONTH LEFT JOIN POLI
                    ON
                    ( RESEREDMONTH.pers_id = POLI.pers_id )
                    ")


# For the party variable, we will rely on the MEME data frame.

# 1) We reduce MEME to only relevant politicians.
# Note: Tomas calls this MEMEBU (MEME build up)
MEMERED <- MEME[MEME$pers_id %in% relevantmps, ]


# 2) Transform dates in the PCC format to R dates
# a) Replace missing end dates with the end of the 2015 legislative period (CH)
# Note: Only CH MPs have missing values
MEMERED[which(is.na(MEMERED$memep_enddate)),]$memep_enddate <-  "01dec2019"


# b) For more information on date formats: https://www.r-bloggers.com/date-formats-in-r/
MEMERED$memep_startdate <- as.Date(MEMERED$memep_startdate, format=c("%d%b%Y")) # start dates
MEMERED$memep_enddate <- as.Date(MEMERED$memep_enddate, format=c("%d%b%Y")) # end dates

# 3) Remove rows that refer only to an election date.
# Note: These entries have only a start date in the Swiss case.
MEMERED <- MEMERED[which(!is.na(MEMERED$memep_enddate)),]


# 4) Transpose MEMERED to long format
# Note: 1) This is needed for aggregation purposes since MPs might switch parties during an observation month.
#       2) This also helps remove duplicate episodes, and detect overlapping episodes.

# Note: The function breaks if the start date is after the end date
MEMEREDLONG <- setDT(MEMERED)[ , list(pers_id = pers_id, party_id = party_id,
                                    day = seq(as.Date(memep_startdate), as.Date(memep_enddate), by = "day")), by = 1:nrow(MEMERED)]

MEMEREDLONG$nrow <- NULL # drop nrow

# 5) Get rid of duplicated rows
MEMEREDLONG <- MEMEREDLONG[!duplicated(MEMEREDLONG),]

# 6)  Order the data first according to "pers_id" and then to "day"
MEMEREDLONG <-  with(MEMEREDLONG, MEMEREDLONG[order(pers_id, day) , ])

# 7) Now, we prepare MEMEREDLONG for aggregation on the MP-month level
# To that end, we generate a month variable like we did before.
MEMEREDLONG$month <- str_extract(as.character(MEMEREDLONG$day), "[0-9]{4}-[0-9]{2}") # [0-9]{4}-[0-9]{2} matches 4 digits followed by a hyphen and 2 digits

# 8) Obtain the mode for party membership for every MP-month in the MEMEREDLONG
# a) Define a function that gets us the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode

# b) Get the party_id mode by groups (pers_id, month)
# Note: This is a decision we take. It's an alternative to taking the first day of the month etc.
MEMEREDMONTH <- aggregate(MEMEREDLONG$party_id, by = list(pers_id = MEMEREDLONG$pers_id, month = MEMEREDLONG$month), getmode)
colnames(MEMEREDMONTH)[match("x",colnames(MEMEREDMONTH))] <- "party_id" # rename variable x to party_id


# 9) Finally, add party_id to RESEREDMONTH
RESEREDMONTH <- sqldf("SELECT RESEREDMONTH.*, MEMEREDMONTH.party_id
                    FROM RESEREDMONTH LEFT JOIN MEMEREDMONTH
                       ON
                       ( RESEREDMONTH.pers_id = MEMEREDMONTH.pers_id )
                       AND
                       ( RESEREDMONTH.month = MEMEREDMONTH.month )
                       ")

# 10) Delete some data frames to save space
gc()
rm(MEMERED, MEMEREDLONG, MEMEREDMONTH)
gc()


# 11) Missingness
# a) Add some missing cases manually:
RESEREDMONTH[which(RESEREDMONTH$pers_id == "CH_Bregy_Philipp_1978"),]$party_id <- "CH_CVP|PDC_RE-VS"
RESEREDMONTH[which(RESEREDMONTH$pers_id == "CH_Egger_Mike_1992"),]$party_id <- "CH_SVP|UDC_RE-SG"
RESEREDMONTH[which(RESEREDMONTH$pers_id == "CH_Marti_Samira_1994"),]$party_id <- "CH_SP|PS_RE-BL"
RESEREDMONTH[which(RESEREDMONTH$pers_id == "CH_Rochat_Nicolas_1982"),]$party_id <- "CH_SP|PS_RE-VD"



# b) Use national-level party_ids
RESEREDMONTH$party_id_national <- str_replace(RESEREDMONTH$party_id,"RE-[A-Z]{2}$","NT") #create a label for national parties (relevant for Switzerland)

# c) Copy down party_ids where they are missing.
RESEREDMONTH <- RESEREDMONTH[with(RESEREDMONTH, order(pers_id, month)), ]

RESEREDMONTH %>%
  group_by(pers_id) %>%
  mutate_all(funs(na.locf(., na.rm = FALSE))) -> RESEREDMONTH

# d) Copy party_ids upwards where they are missing
RESEREDMONTH <- RESEREDMONTH[with(RESEREDMONTH, order(pers_id, rev(month))), ]

RESEREDMONTH %>%
  group_by(pers_id) %>%
  mutate_all(funs(na.locf(., na.rm = FALSE))) -> RESEREDMONTH

RESEREDMONTH <- RESEREDMONTH[with(RESEREDMONTH, order(pers_id, month)), ]


# 12) Extract canton (for Swiss MPs)
RESEREDMONTH$canton <- str_extract(RESEREDMONTH$party_id, "[A-Z]{2}$")

# 13) Extract country
RESEREDMONTH$country <- str_extract(RESEREDMONTH$pers_id, "^[A-Z]{2}")

# 14) Set canton to NA if country is Germany
RESEREDMONTH[which(RESEREDMONTH$country == "DE"),]$canton <- NA


################################
# MP-Month Data: parliament_id #
################################

# 1) We reduce PARL to only relevant parliaments
PARLRED <- PARL[which(grepl("^(CH|DE)_NT", PARL$parliament_id)),]


# 2) Transform dates in the PCC format to R dates
# a) Replace end dates of the 2015 legislative period (CH) and 2017 legislative period (DE)
#    with their actual values.
PARLRED[which(PARLRED$parliament_id == "DE_NT-BT_2017"),]$leg_period_end <-  "26oct2021"
PARLRED[which(PARLRED$parliament_id == "CH_NT-SR_2015"),]$leg_period_end <-  "01dec2019"
PARLRED[which(PARLRED$parliament_id == "CH_NT-NR_2015"),]$leg_period_end <-  "01dec2019"




# b) For more information on date formats: https://www.r-bloggers.com/date-formats-in-r/
PARLRED$leg_period_start <- as.Date(PARLRED$leg_period_start, format=c("%d%b%Y")) # start dates
PARLRED$leg_period_end <- as.Date(PARLRED$leg_period_end, format=c("%d%b%Y")) # end dates

# 3) Remove rows that refer only to an election date.
# Note: These entries have only a start date in the Swiss case.
PARLRED <- PARLRED[which(!is.na(PARLRED$leg_period_end)),]


# 4) Transpose PARLRED to long format
# Note: 1) This is needed for aggregation purposes since MPs might switch parties during an observation month.
#       2) This also helps remove duplicate episodes, and detect overlapping episodes.

# Note: The function breaks if the start date is after the end date
PARLREDLONG <- setDT(PARLRED)[ , list(parliament_id = parliament_id, assembly_abb = assembly_abb,
                                      day = seq(as.Date(leg_period_start), as.Date(leg_period_end), by = "day")), by = 1:nrow(PARLRED)]

PARLREDLONG$nrow <- NULL # drop nrow

# 5) Get rid of duplicated rows
PARLREDLONG <- PARLREDLONG[!duplicated(PARLREDLONG),]

# 6)  Order the data first according to "pers_id" and then to "day"
PARLREDLONG <-  with(PARLREDLONG, PARLREDLONG[order(parliament_id, day) , ])

# 7) Now, we prepare PARLREDLONG for aggregation on the MP-month level
# To that end, we generate a month variable like we did before.
PARLREDLONG$month <- str_extract(as.character(PARLREDLONG$day), "[0-9]{4}-[0-9]{2}") # [0-9]{4}-[0-9]{2} matches 4 digits followed by a hyphen and 2 digits

# 8) Obtain the mode for party membership for every MP-month in the PARLREDLONG
# a) Define a function that gets us the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode

# b) Get the party_id mode by groups (pers_id, month)
# Note: This is a decision we take. It's an alternative to taking the first day of the month etc.
PARLREDMONTH <- aggregate(PARLREDLONG$parliament_id,  by = list(assembly_abb = PARLREDLONG$assembly_abb, month = PARLREDLONG$month), getmode)
colnames(PARLREDMONTH)[match("x",colnames(PARLREDMONTH))] <- "parliament_id" # rename variable x to party_id


# 9) Finally, add party_id to RESEREDMONTH
#PARLREDMONTH$chamber <- as.factor(str_extract(PARLREDMONTH$parliament_id, "(?<!^)(NR|SR|BT|TK|SE|DE)"))

RESEREDMONTH <- sqldf("SELECT RESEREDMONTH.*, PARLREDMONTH.parliament_id
                    FROM RESEREDMONTH LEFT JOIN PARLREDMONTH
                       ON
                       ( RESEREDMONTH.chamber = PARLREDMONTH.assembly_abb )
                       AND
                       ( RESEREDMONTH.month = PARLREDMONTH.month )
                       ")

# 10) Delete some data frames to save space
gc()
rm(PARLRED,PARLREDLONG, PARLREDMONTH)
gc()


################################
# MP-Month Data: Election date #
################################

# 1) Create a dataframe with election dates per district & chamber

# 2) St?nderat
# a) Create the base data
CHSRELEC <- subset(SRELEC, select = c("Kanton", "1. Wahlgang Wahltag"))
CHSRELEC <- sqldf("SELECT DISTINCT CHSRELEC.*
                       FROM CHSRELEC
                  ")

# b) Remove entries that are not cantons
CHSRELEC <- CHSRELEC[which(grepl("^[A-Z]", CHSRELEC$Kanton)),]

# c) ... or empty
CHSRELEC <- CHSRELEC[which(is.na(CHSRELEC$`1. Wahlgang Wahltag`)==F),]

# d) Add information on the country and the chamber
CHSRELEC$country <- "CH"
CHSRELEC$chamber <- "SR"
colnames(CHSRELEC)[match("Kanton",colnames(CHSRELEC))] <- "district" 
colnames(CHSRELEC)[match("1. Wahlgang Wahltag",colnames(CHSRELEC))] <- "election_date"


# e) Create a canton column
canton <- c('Aargau','Argau','Appenzell I.Rh.','Appenzell A.Rh.','Bern','Basel-Landschaft','Basel-Stadt','Freiburg','Genf','Glarus','Graub?nden','Jura','Luzern','Neuenburg','Nidwalden','Obwalden','St. Gallen','Schaffhausen','Solothurn','Schwyz','Thurgau','Tessin','Uri','Waadt','Wallis','Zug','Z?rich')
canton_abb <- c('AG','AG','AI','AR','BE','BL','BS','FR','GE','GL','GR','JU','LU','NE','NW','OW','SG','SH','SO','SZ','TG','TI','UR','VD','VS','ZG','ZH')

CHSRELEC$district <- mgsub(CHSRELEC$district, canton, canton_abb)

# f) Indicate whether this is entails list or district candidacy
CHSRELEC$candidacy <- ""
CHSRELEC[which(grepl("Proporzsystem",CHSRELEC$district)),]$candidacy <- "L"
CHSRELEC[which(grepl("Proporzsystem",CHSRELEC$district)==F),]$candidacy <- "D"
CHSRELEC$district <- gsub(" \\(Proporzsystem\\)", "", CHSRELEC$district)

# g) Create parliament_id
CHSRELEC$parliament_id <- paste("CH_", CHSRELEC$chamber, "-NR_", str_extract(as.character(CHSRELEC$election_date),"[0-9]{4}"), sep="")

# h) Add an additional electoral period
CHSRELEC23<- CHSRELEC[which(grepl("CH_SR-NR_2011",CHSRELEC$parliament_id)),]
CHSRELEC23[which(grepl("AI",CHSRELEC23$district)),]$election_date <- "2023-04-30" # Landsgemeinde
CHSRELEC23[which(grepl("2011-",CHSRELEC23$election_date)),]$election_date <- "2023-10-22"
CHSRELEC23$parliament_id <- "CH_NT-SR_2023" 

CHSRELEC <- smartbind(CHSRELEC, CHSRELEC23)
rm(CHSRELEC23)

# 2) Nationalrat and Bundestag
# a) Create the base data
NRBTELEC <- subset(ELEC, select = c("parliament_id", "election_date"))
NRBTELEC <- sqldf("SELECT DISTINCT NRBTELEC.*
                       FROM NRBTELEC
                  ")

# b) Reduce data
# Remove regional elections
# Keep only national elections after 2007
NRBTELEC <- NRBTELEC[which(grepl("CH_NT-NR_2011|CH_NT-NR_2015|DE_NT-BT_2009|DE_NT-BT_2013|DE_NT-BT_2017", NRBTELEC$parliament_id)),]

# c) Add later elections manually
NRBTELEC[nrow(NRBTELEC) + 1,]$parliament_id <- "CH_NT-NR_2019"
NRBTELEC[nrow(NRBTELEC),]$election_date <- "20oct2019"

NRBTELEC[nrow(NRBTELEC) + 1,]$parliament_id <- "CH_NT-NR_2023"
NRBTELEC[nrow(NRBTELEC),]$election_date <- "22oct2023"

NRBTELEC[nrow(NRBTELEC) + 1,]$parliament_id <- "DE_NT-BT_2021"
NRBTELEC[nrow(NRBTELEC),]$election_date <- "26sep2021"


# d) Create combinations
NRBTELEC2 <- expand.grid(NRBTELEC$parliament_id,unique(canton_abb))
colnames(NRBTELEC2) <- c("parliament_id", "district")
NRBTELEC2[which(grepl("^DE_",NRBTELEC2$parliament_id)),]$district <- ""
NRBTELEC2 <- sqldf("SELECT DISTINCT NRBTELEC2.*
                       FROM NRBTELEC2
                  ")


# e) Add election date back
NRBTELEC2 <- sqldf("SELECT NRBTELEC2.*, NRBTELEC.election_date
                    FROM NRBTELEC2 LEFT JOIN NRBTELEC
                       ON
                       ( NRBTELEC.parliament_id = NRBTELEC2.parliament_id )
                       ")


# g) Create a chamber variable
NRBTELEC2$chamber <- as.factor(str_extract(NRBTELEC2$parliament_id, "(?<!^)(NR|SR|BT|TK|SE|DE)"))


# h) Create a candidacy variable
NRBTELEC2$candidacy <- "L"
NRBTELEC2[which(grepl("UR|OW|NW|AI|AR|GL", NRBTELEC2$district)),]$candidacy <- "D"
NRBTELEC2[which(grepl("BT", NRBTELEC2$parliament_id)),]$candidacy <- ""

# i) Create a country variable
NRBTELEC2$country <- str_extract(NRBTELEC2$parliament_id, "^(CH|DE)")

# j) Transform date
NRBTELEC2$election_date <- as.Date(NRBTELEC2$election_date, format=c("%d%b%Y"))

# 3) Combine data
ELECT <- smartbind(NRBTELEC2, CHSRELEC)

# 4) Create a month variable
ELECT$month <- str_extract(as.character(ELECT$election_date), "[0-9]{4}-[0-9]{2}") # [0-9]{4}-[0-9]{2} matches 4 digits followed by a hyphen and 2 digits



# 5) Delete some data frames to save space
gc()
rm(NRBTELEC, NRBTELEC2, CHSRELEC)
gc()



##############################
# MP-Month Data: Candidacies #
##############################

# 1) Find out MPs' candidacies
CANDIDACIES <- ELEN
CANDIDACIES <- CANDIDACIES[which(grepl("^(CH|DE)_NT", CANDIDACIES$elec_entry_id)),]

# 2) Extract election dates
CANDIDACIES$election_date <- str_extract(CANDIDACIES$list_id, "[0-9]{2}[a-z]{3}[0-9]{4}")
CANDIDACIES$election_date <- as.Date(CANDIDACIES$election_date, format=c("%d%b%Y"))


# 3) Extract chamber
CANDIDACIES$chamber <- as.factor(str_extract(CANDIDACIES$list_id, "(?<!^)(NR|SR|BT|TK|SE|DE)"))

# 4) Extract parliament_id
CANDIDACIES$parliament_id <- str_extract(CANDIDACIES$list_id, "(CH|DE)_NT-(NR|SR|BT)_[0-9]{4}")

# 5) Extract district
CANDIDACIES$district <- str_extract(CANDIDACIES$list_id, "(?<!^)__(.*)__[0-9]")
CANDIDACIES$district <- gsub("__[0-9]","", CANDIDACIES$district)
CANDIDACIES$district <- gsub("__","", CANDIDACIES$district)

# 6) Delete district for Germany
CANDIDACIES[which(grepl("BT", CANDIDACIES$parliament_id)),]$district <- ""

# 7) Remove entries before 2008
CANDIDACIES <- CANDIDACIES[which(CANDIDACIES$election_date > as.Date('2007-12-31')),]

# 8) Create a district variable
canton <- c('Aargau','Appenzell-A-RH','Appenzell-I-RH','Basel-Landschaft','Basel-Stadt','Bern-Berne','Fribourg-Freiburg','Geneve','Glarus','Graubuenden','Graubuenden-Grigioni-Grischun','Jura','Luzern','Neuchatel','Nidwalden','Obwalden','Schaffhausen','Schwyz','Solothurn','St-Gallen','Thurgau','Ticino','Uri','Valais','Valais-Wallis','Vaud','Vaud8oct2015','Zuerich','Zug')
canton_abb <- c('AG','AR','AI','BL','BS','BE','FR','GE','GL','GR','GR','JU','LU','NE','NW','OW','SH','SZ','SO','SG','TG','TI','UR','VS','VS','VD','VD','ZH','ZG')
CANDIDACIES$district <- mgsub(CANDIDACIES$district, canton, canton_abb)

# 9) Subsetting
CANDIDACIES <- subset(CANDIDACIES, select = c("parliament_id", "chamber", "district" ,"election_date", "candidature_type", "pers_id","elected_bundesblatt"))




# 10) Add SR 2019
SRELEC2 <- SRELEC

# a) Create a district variable
canton <- c('Aargau','Argau','Appenzell I.Rh.','Appenzell A.Rh.','Bern','Basel-Landschaft','Basel-Stadt','Freiburg','Genf','Glarus','Graub?nden','Jura','Luzern','Neuenburg','Nidwalden','Obwalden','St. Gallen','Schaffhausen','Solothurn','Schwyz','Thurgau','Tessin','Uri','Waadt','Wallis','Zug','Z?rich')
canton_abb <- c('AG','AG','AI','AR','BE','BL','BS','FR','GE','GL','GR','JU','LU','NE','NW','OW','SG','SH','SO','SZ','TG','TI','UR','VD','VS','ZG','ZH')

SRELEC2$district <- mgsub(SRELEC2$Kanton, canton, canton_abb)

SRELEC2$district <- gsub(" \\((Proporzsystem|Stille Wahl|Landsgemeinde)\\)", "", SRELEC2$district) # Remove content in brackets

# b) Create parliament_id
SRELEC2$parliament_id <- paste("CH_NT-SR_", SRELEC2$Jahr, sep="")

# c) Chamber
SRELEC2$chamber <- "SR"

# d) Rename variables
colnames(SRELEC2)[match("gewaehlt_numeric",colnames(SRELEC2))] <- "elected_bundesblatt" # rename to false_positive
colnames(SRELEC2)[match("1. Wahlgang Wahltag",colnames(SRELEC2))] <- "election_date" # rename to false_positive

# e) Keep only rows with a pers_id
SRELEC2 <- SRELEC2[which(grepl("^CH", SRELEC2$pers_id)),]

# f) Reduce the number of variables
SRELEC2 <- subset(SRELEC2, select = c("parliament_id", "chamber", "district" ,"election_date", "pers_id","elected_bundesblatt"))

# g) Reduce data to only CH_NT-SR_2019
SRELEC2 <- SRELEC2[which(SRELEC2$parliament_id == "CH_NT-SR_2019"),]

# h) Merge data
CANDIDACIES <- smartbind(data.frame(CANDIDACIES), data.frame(SRELEC2))

rm(SRELEC2)


# 11) Add NR 2019 data
NRCAND192 <- NRCAND19

# a) Create parliament_id
NRCAND192$parliament_id <- paste("CH_NT-NR_", NRCAND192$wahl_jahr, sep="")

# b) Chamber
NRCAND192$chamber <- "NR"

# c) Rename variables
colnames(NRCAND192)[match("flag_gewaehlt",colnames(NRCAND192))] <- "elected_bundesblatt" # rename to false_positive
NRCAND192$election_date <- "2019-10-20"

# d) Keep only rows with a pers_id
NRCAND192 <- NRCAND192[which(grepl("^CH", NRCAND192$pers_id)),]

# e) Reduce the number of variables
NRCAND192 <- subset(NRCAND192, select = c("parliament_id", "chamber", "district" ,"election_date", "pers_id","elected_bundesblatt"))

# f) Merge data
CANDIDACIES <- smartbind(data.frame(CANDIDACIES), data.frame(NRCAND192))

rm(NRCAND192)


# 12) Add candidacy from ELECT to CANDIDACIES (necessary for Switzerland)
CANDIDACIES <- sqldf("SELECT CANDIDACIES.*, ELECT.candidacy
                    FROM CANDIDACIES LEFT JOIN ELECT
                       ON
                       ( CANDIDACIES.election_date = ELECT.election_date )
                       AND
                       ( CANDIDACIES.chamber = ELECT.chamber )
                       AND
                       ( CANDIDACIES.district = ELECT.district )
                       ")


# 13) Merge candicacy and candidature_type
CANDIDACIES[which(grepl("^$",CANDIDACIES$candidature_type)),]$candidature_type <- CANDIDACIES[which(grepl("^$",CANDIDACIES$candidature_type)),]$candidacy

CANDIDACIES[which(is.na(CANDIDACIES$candidature_type)),]$candidature_type <- CANDIDACIES[which(is.na(CANDIDACIES$candidature_type)),]$candidacy


# 14) Store all candidature_types for the same election per pers_id in the same cell
CANDIDACIES2 <- sqldf("SELECT district, election_date, pers_id,
                    group_concat(candidature_type,';') 'candidature_types'
                    from CANDIDACIES group by pers_id, election_date, district")


# 15) Remove duplicates in concatenated strings
CANDIDACIES2$candidature_types <- sapply(strsplit(str_squish(CANDIDACIES2$candidature_types), ";"), function(x) paste(sort(unique(rle(x)$values),decreasing = TRUE), collapse=""))
CANDIDACIES2$candidature_types <- sapply(strsplit(str_squish(CANDIDACIES2$candidature_types), ""), function(x) paste(sort(unique(rle(x)$values),decreasing = TRUE), collapse=""))



# 16) Create a month variable
CANDIDACIES2$month <- str_extract(as.character(CANDIDACIES2$election_date), "[0-9]{4}-[0-9]{2}") # [0-9]{4}-[0-9]{2} matches 4 digits followed by a hyphen and 2 digits

# 17) Remove empty candidature rows (second rounds of SR elections or silent elections)
CANDIDACIES2 <- CANDIDACIES2[which(grepl("^$", CANDIDACIES2$candidature_types)==F),]

# 18) Add CANDIDACIES2 to RESEREDMONTH
RESEREDMONTH <- sqldf("SELECT RESEREDMONTH.*, CANDIDACIES2.election_date, CANDIDACIES2.candidature_types
                    FROM RESEREDMONTH LEFT JOIN CANDIDACIES2
                       ON
                       ( CANDIDACIES2.pers_id = RESEREDMONTH.pers_id )
                       AND
                       ( CANDIDACIES2.month = RESEREDMONTH.month )
                       ")

# 19) Set NAs on parliament_id to none
RESEREDMONTH[is.na(RESEREDMONTH$parliament_id)==T,]$parliament_id <- "none"



####################################################################
# MP-Month Data: Panachage Data (for Swiss MPs running for the NR) #
####################################################################

# 4 relevant variables:
# stimmen_total: total number of votes
# stimmen_eigene_liste: votes from own list
# stimmen_fremde_listen: votes from other lists (this and and 2nd variable add up to the total number of votes)
# stimmen_eigene_liste_veraendert: votes from own list but changed by the voter 

# 1) Generate parliament_id
PANACH$parliament_id <- paste("CH_NT-NR_", PANACH$jahr, sep="")

# 2) Add the election_date to PANACH
PANACH$election_date <- ""

PANACH[which(PANACH$jahr == "2019"),]$election_date <- "2019-10-20"
PANACH[which(PANACH$jahr == "2015"),]$election_date <- "2015-10-18"
PANACH[which(PANACH$jahr == "2011"),]$election_date <- "2011-10-23"
PANACH[which(PANACH$jahr == "2007"),]$election_date <- "2007-10-21"


# 3) Find out which candidates where pre-cumulated (Vorkumulierung) by their party, i.e. already added twice to the pre-printed election list.
# a) Create a year-canton-list identifier
PANACH$list_id <- paste(PANACH$jahr,"_",PANACH$kanton_bezeichnung_kurz,"_", PANACH$liste_bezeichnung, sep="")

# b) Retain only list_id and stimmen_eigene_liste_unveraendert
PRECUMULATION <- subset(PANACH, select = c("list_id", "stimmen_eigene_liste_unveraendert", "kandidat_nummer"))

# c) Drop kandidat_nummer = 99 rows
PRECUMULATION <- PRECUMULATION[which(PRECUMULATION$kandidat_nummer != 99),]
PRECUMULATION$kandidat_nummer <- NULL

# d) Fix 2019_VS_Grüne Oberwallis (differing number of inedited list votes)
PRECUMULATION[which(PRECUMULATION$list_id == "2019_VS_Grüne Oberwallis"),]$stimmen_eigene_liste_unveraendert <- 809

# e) Retain only unique rows
PRECUMULATION <- sqldf("SELECT DISTINCT PRECUMULATION.*
                        FROM PRECUMULATION
                  ")

# f) Obtain the maximum and minimum per list_id
PRECUMULATIONMAX <- sqldf("select max(stimmen_eigene_liste_unveraendert) AS stimmen_eigene_liste_unveraendert,list_id from PRECUMULATION group by list_id")
PRECUMULATIONMAX$value <- "max"

PRECUMULATIONMIN <- sqldf("select min(stimmen_eigene_liste_unveraendert) AS stimmen_eigene_liste_unveraendert,list_id from PRECUMULATION group by list_id")
PRECUMULATIONMIN$value <- "min"

# e) Combine PRECUMULATIONMAX and PRECUMULATIONMIN
PRECUMULATION <- smartbind(PRECUMULATIONMAX, PRECUMULATIONMIN)
rm(PRECUMULATIONMAX, PRECUMULATIONMIN)

# f) Reduce the number of rows
PRECUMULATION <- sqldf("SELECT list_id, stimmen_eigene_liste_unveraendert, group_concat(value,';') 'value'
                    from PRECUMULATION group by list_id, stimmen_eigene_liste_unveraendert")

# g) Recode the information on how many votes politicians obtained
PRECUMULATION$represented_on_list <- NA
PRECUMULATION[which(PRECUMULATION$value == "max;min"),]$represented_on_list <- "everyone equally represented on list"
PRECUMULATION[which(PRECUMULATION$value == "max"),]$represented_on_list <- "twice on list while others once"
PRECUMULATION[which(PRECUMULATION$value == "min"),]$represented_on_list <- "once on list while others twice"

# h) Add the information back to PANACH
PANACH <- sqldf("SELECT PANACH.*, PRECUMULATION.represented_on_list
                    FROM PANACH LEFT JOIN PRECUMULATION
                       ON
                       ( PRECUMULATION.list_id = PANACH.list_id )
                       AND
                       ( PRECUMULATION.stimmen_eigene_liste_unveraendert = PANACH.stimmen_eigene_liste_unveraendert )
                       ")

rm(PRECUMULATION)

# 4) Merge PANACH with RESEREDMONTH
RESEREDMONTH <- sqldf("SELECT RESEREDMONTH.*, PANACH.stimmen_total AS votes_total, PANACH.stimmen_eigene_liste AS votes_own_list, PANACH.stimmen_fremde_liste AS votes_other_lists, PANACH.stimmen_eigene_liste_veraendert AS votes_own_list_changed, PANACH.represented_on_list
                    FROM RESEREDMONTH LEFT JOIN PANACH
                       ON
                       ( PANACH.pers_id = RESEREDMONTH.pers_id )
                       AND
                       ( PANACH.election_date = RESEREDMONTH.election_date )
                       ")


# 4) Calculate the share of panachage votes
RESEREDMONTH$panachage_vote_pct <- (RESEREDMONTH$votes_other_lists / RESEREDMONTH$votes_total) *100

# 5) Calculate the share of votes obtained from changed (own) lists
RESEREDMONTH$cumulation_vote_pct <- (RESEREDMONTH$votes_own_list_changed / RESEREDMONTH$votes_total) *100


###############################
# MP-Month Data: Tweet Counts #
###############################

# 1) Create a month variable in TWEETSCODED
TWEETSCODED$month <- str_extract(as.character(TWEETSCODED$tweet_timestampR), "^[0-9]{4}-[0-9]{2}")

# 2) Count the total number of Tweets
TWEEMO <- sqldf("SELECT pers_id, month, count(pers_id) as 'total_nr_of_tweets'
						 FROM TWEETSCODED
						 GROUP BY pers_id,month
						")





# 3) Count the number of Tweets with local cues
TWEELC <- TWEETSCODED[which(TWEETSCODED$tweetislocalcue==T),]


LCTWEEMO <- sqldf("SELECT pers_id, month, count(pers_id) as 'nr_of_tweets_with_localcue'
						 FROM TWEELC
						 GROUP BY pers_id,month
						")

rm(TWEELC)

# 4) Add the two counts to RESEREDMONTH
RESEREDMONTH <- sqldf("SELECT RESEREDMONTH.*, TWEEMO.total_nr_of_tweets
              					 FROM RESEREDMONTH LEFT JOIN TWEEMO
              					 ON
              					 RESEREDMONTH.pers_id = TWEEMO.pers_id
              					 AND
              					 RESEREDMONTH.month = TWEEMO.month
              					")

RESEREDMONTH <- sqldf("SELECT RESEREDMONTH.*, LCTWEEMO.nr_of_tweets_with_localcue
            					 FROM RESEREDMONTH LEFT JOIN LCTWEEMO
            					 ON
            					 RESEREDMONTH.pers_id = LCTWEEMO.pers_id
            					 AND
            					 RESEREDMONTH.month = LCTWEEMO.month
            					")

rm(TWEEMO, LCTWEEMO)

# 5) Add zeroes to nr_of_tweets_with_localcue
RESEREDMONTH[which(RESEREDMONTH$total_nr_of_tweets > 0 & is.na(RESEREDMONTH$nr_of_tweets_with_localcue)),]$nr_of_tweets_with_localcue <- 0


# 6) Add zeroes to months where the politician had used Twitter before but wasn't using it in a later month anymore.
# a) Retain only rows where Tweets were sent
RESEREDMONTHRED <- RESEREDMONTH[which(is.na(RESEREDMONTH$total_nr_of_tweets)==FALSE),]

# b) Find the first row for each group
RESEREDMONTHRED <- RESEREDMONTHRED[!duplicated(RESEREDMONTHRED$pers_id),]

# c) Add a zero column to RESEREDMONTHRED
RESEREDMONTHRED$zero <- 0

# d) Add this column to RESEREDMONTH
RESEREDMONTH <- sqldf("SELECT RESEREDMONTH.*, RESEREDMONTHRED.zero
            					 FROM RESEREDMONTH LEFT JOIN RESEREDMONTHRED
            					 ON
            					 RESEREDMONTH.pers_id = RESEREDMONTHRED.pers_id
            					 AND
            					 RESEREDMONTH.month = RESEREDMONTHRED.month
            					")

rm(RESEREDMONTHRED)

# e) Drag the values in the zero column of RESEREDMONTH down
RESEREDMONTH %>% group_by(pers_id) %>% fill(zero) -> RESEREDMONTH

# f) Copy the 0 to the total_nr_of_tweets and nr_of_tweets_with_localcue column when it's empty
RESEREDMONTH[which(is.na(RESEREDMONTH$total_nr_of_tweets)==TRUE),]$total_nr_of_tweets <- RESEREDMONTH[which(is.na(RESEREDMONTH$total_nr_of_tweets)==TRUE),]$zero
RESEREDMONTH[which(is.na(RESEREDMONTH$nr_of_tweets_with_localcue)==TRUE),]$nr_of_tweets_with_localcue <- RESEREDMONTH[which(is.na(RESEREDMONTH$nr_of_tweets_with_localcue)==TRUE),]$zero

RESEREDMONTH$zero <- NULL

# g) Share of TWEETS with local cues
RESEREDMONTH$pct_of_tweets_with_localcue <- (RESEREDMONTH$nr_of_tweets_with_localcue / RESEREDMONTH$total_nr_of_tweets)*100

RESEREDMONTH[which(RESEREDMONTH$pct_of_tweets_with_localcue == "NaN"),]$pct_of_tweets_with_localcue <- 0


##################################
# MP-Month Data: Campaign Season #
##################################

# Create a campaign season counter

# 1) Flip the df around
RESEREDMONTH <- RESEREDMONTH[with(RESEREDMONTH, order(pers_id, rev(month))), ]

# 2) Create a new group variable
# a) Copy the election_date variable
RESEREDMONTH$helper_column <- RESEREDMONTH$election_date

# b) Fill the helper_column
RESEREDMONTH %>% group_by(pers_id) %>% fill(helper_column) -> RESEREDMONTH


# 3) Add a counter by two groups
RESEREDMONTH %>%
  group_by(pers_id, helper_column) %>%
  mutate(nr_months_before_elec = row_number()) -> RESEREDMONTH


# 4) Set nr_months_before_elec to zero if helper_column is NA
RESEREDMONTH[which(is.na(RESEREDMONTH$helper_column)==TRUE),]$nr_months_before_elec <- NA

# 5) Delete helper_column and restore the row order
# RESEREDMONTH$helper_column <- NULL
RESEREDMONTH <- RESEREDMONTH[with(RESEREDMONTH, order(pers_id, month)), ]

# 6) Add a campaign season indicator
RESEREDMONTH$campaign_season <- ifelse(RESEREDMONTH$nr_months_before_elec <= 6,"yes","no")




################
### Ideology ###
################

# Get information on party positions
# Read in ParlGov_Parties.csv
IDEOLOGY = read_excel("Data/parlgov.xlsx")

# 1) Create party_idshort in RESEREDMONTH
RESEREDMONTH$party_idshort <- substr(RESEREDMONTH$party_id,1,nchar(as.character(RESEREDMONTH$party_id))-6)

# 2) Add information from IDEOLOGY based on party_idshort where RESEREDMONTH.party_idshort is contained
# in the array of party_idshorts of IDEOLOGY.pcc_party_id
RESEREDMONTH <- sqldf("SELECT RESEREDMONTH.*, IDEOLOGY.left_right, IDEOLOGY.state_market
                 FROM RESEREDMONTH LEFT JOIN IDEOLOGY
                 ON IDEOLOGY.pcc_party_id LIKE '%'||RESEREDMONTH.party_idshort||'%'")


# 3) Create an extremeness score (note: left_right ranges from 0-10 [https://www.parlgov.org/data/codebook.pdf])
RESEREDMONTH$extreme <- NA
RESEREDMONTH[which(RESEREDMONTH$left_right >5),]$extreme <- RESEREDMONTH[which(RESEREDMONTH$left_right >5),]$left_right-5 #right-wing
RESEREDMONTH[which(RESEREDMONTH$left_right <5),]$extreme <- 5-RESEREDMONTH[which(RESEREDMONTH$left_right <5),]$left_right # left-wing

# 4) Drop party_idshort
RESEREDMONTH$party_idshort <- NULL


####################################
# Dataframe for Panachage Analysis #
####################################

# 1) Reduce RESEREDMONTH to only CH entries at the election date of politicians running for the NR who do use Twitter
CHPANACH <- RESEREDMONTH[which(is.na(RESEREDMONTH$election_date)==F & RESEREDMONTH$country == "CH" & is.na(RESEREDMONTH$total_nr_of_tweets)==F & grepl("L", RESEREDMONTH$candidature_types)),]

# 2) Subset the RESEREDMONTH to campaign seasons
CAMPSEAS <- RESEREDMONTH[which(RESEREDMONTH$campaign_season == "yes" & RESEREDMONTH$country == "CH"),]

# 3) Count the Tweets during campaign season
# a) Set NAs to 0
CAMPSEAS[which(is.na(CAMPSEAS$total_nr_of_tweets)),]$total_nr_of_tweets <- 0
CAMPSEAS[which(is.na(CAMPSEAS$nr_of_tweets_with_localcue)),]$nr_of_tweets_with_localcue <- 0

# b) Count Tweets during campaign season
CAMPSEASCOUNT1 <- aggregate(CAMPSEAS$total_nr_of_tweets, by=list(pers_id=CAMPSEAS$pers_id, election_date = CAMPSEAS$helper_column), FUN=sum)
colnames(CAMPSEASCOUNT1)[match("x",colnames(CAMPSEASCOUNT1))] <- "total_nr_of_tweets_campaign" # rename


CAMPSEASCOUNT2 <- aggregate(CAMPSEAS$nr_of_tweets_with_localcue, by=list(pers_id=CAMPSEAS$pers_id, election_date = CAMPSEAS$helper_column), FUN=sum)
colnames(CAMPSEASCOUNT2)[match("x",colnames(CAMPSEASCOUNT2))] <- "nr_of_tweets_with_localcue_campaign" # rename

# c) Add these counts to CHPANACH
CHPANACH <- sqldf("SELECT CHPANACH.*, CAMPSEASCOUNT1.total_nr_of_tweets_campaign
            					 FROM CHPANACH LEFT JOIN CAMPSEASCOUNT1
            					 ON
            					 CHPANACH.pers_id = CAMPSEASCOUNT1.pers_id
            					 AND
            					 CHPANACH.election_date = CAMPSEASCOUNT1.election_date
            					")

CHPANACH <- sqldf("SELECT CHPANACH.*, CAMPSEASCOUNT2.nr_of_tweets_with_localcue_campaign
            					 FROM CHPANACH LEFT JOIN CAMPSEASCOUNT2
            					 ON
            					 CHPANACH.pers_id = CAMPSEASCOUNT2.pers_id
            					 AND
            					 CHPANACH.election_date = CAMPSEASCOUNT2.election_date
            					")

rm(CAMPSEASCOUNT1, CAMPSEASCOUNT2)

# 4) Calculate the percentage of local cues
CHPANACH$pct_of_tweets_with_localcue_campaign <- (CHPANACH$nr_of_tweets_with_localcue_campaign / CHPANACH$total_nr_of_tweets_campaign)*100
CHPANACH[which(CHPANACH$pct_of_tweets_with_localcue_campaign == "NaN"),]$pct_of_tweets_with_localcue_campaign <- 0


# 5) Rename the other Tweet count variables
colnames(CHPANACH)[match("nr_of_tweets_with_localcue",colnames(CHPANACH))] <- "nr_of_tweets_with_localcue_elecmonth" # rename
colnames(CHPANACH)[match("total_nr_of_tweets",colnames(CHPANACH))] <- "total_nr_of_tweets_elecmonth" # rename
colnames(CHPANACH)[match("pct_of_tweets_with_localcue",colnames(CHPANACH))] <- "pct_of_tweets_with_localcue_elecmonth" # rename

# 6) Generate an incumbency measure
CHPANACH$incumbent <- ifelse(CHPANACH$chamber <= "none",0,1)





######################
# Panachage Analysis #
######################

# 1) Some variable preparation
# a) Define base levels
CHPANACH$gender <- as.factor(CHPANACH$gender)
CHPANACH <- within(CHPANACH, gender <- relevel(gender, ref = "m"))

CHPANACH$party_id_national <- as.factor(CHPANACH$party_id_national)
CHPANACH <- within(CHPANACH, party_id_national <- relevel(party_id_national, ref = "CH_CVP|PDC_NT"))

# b) Create mean-centred variables
CHPANACH$nr_of_tweets_with_localcue_campaign_mc <- scale(CHPANACH$nr_of_tweets_with_localcue_campaign, center = TRUE, scale = FALSE)
CHPANACH$total_nr_of_tweets_campaign_mc <- scale(CHPANACH$total_nr_of_tweets_campaign, center = TRUE, scale = FALSE)
CHPANACH$tenure_mc <- scale(CHPANACH$tenure, center = TRUE, scale = FALSE)
CHPANACH$votes_total_mc <- scale(CHPANACH$votes_total, center = TRUE, scale = FALSE)

# 2) Run some models...
# An empty model:
linmod.empty <- lmer(panachage_vote_pct ~ 1 +
                       (1 | election_date) + (1 | pers_id) + (1 | canton),
                     data=CHPANACH)

summary(linmod.empty)

# ICC:
# Note: The ICC can be interpreted as "the proportion of the variance 
# explained by the grouping structure in the population" (Hox 2002: 15).
performance::icc(linmod.empty)


# A model with only Tweets as the IV
linmod1a <- lmer(panachage_vote_pct ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                  incumbent + tenure_mc + candidature_types + gender + total_votes_mc + party_id_national +
                  (1 | election_date)  + (1 | pers_id) + (1|canton),
                data=CHPANACH)
summary(linmod1a)
confint(linmod1a)


linmod1b <- lmer(panachage_vote_pct ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                   incumbent + tenure_mc + candidature_types + gender + votes_total_mc + party_id_national + canton + election_date
                   + (1 | pers_id),
                 data=CHPANACH)
summary(linmod1b)
confint(linmod1b)
tab_model(linmod1b)

anova(linmod1a,linmod1b) # canton-fixed effects work better

# Alternative with the absolute number of panachage votes (i.e. votes from other lists)
linmod2 <- lmer(votes_other_lists ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                  incumbent + tenure_mc + candidature_types + gender + votes_total_mc + party_id_national + canton + election_date
                + (1 | pers_id),
                data=CHPANACH)
summary(linmod2)
confint(linmod2)
tab_model(linmod2)


# Cumulation:
# Alternative model with only votes from ones own list counted
linmod3a <- lmer(votes_own_list_changed  ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                  incumbent + tenure_mc + candidature_types + gender + votes_total_mc + party_id_national + canton + election_date
                + (1 | pers_id),
                data=CHPANACH)
summary(linmod3a)
confint(linmod3a)
tab_model(linmod3a)


# Note: CHPANACH$cumulation_vote_pct <- (CHPANACH$votes_own_list_changed / CHPANACH$votes_total) *100
linmod3b <- lmer(cumulation_vote_pct  ~ nr_of_tweets_with_localcue_campaign_mc + total_nr_of_tweets_campaign_mc +
                   incumbent + tenure_mc + candidature_types + gender + votes_total_mc + party_id_national + canton + election_date
                 + (1 | pers_id),
                 data=CHPANACH)
summary(linmod3b)
confint(linmod3b)
tab_model(linmod3b)




# Check for multicollinearity
vif(linmod1)
vif(linmod2)


# Export with stargazer
stargazer(linmod3a,linmod3b,linmod2,linmod1b, title="The effect of Tweets with local cues on preference votes",
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Panachage votes",
          dep.var.labels   = c("Votes own list changed (abs.)", "Votes own list changed (%)","Votes other lists (abs.)", "Votes other lists (%)"),
          omit = c("party_id","canton", "election_date"),
          omit.labels = c("Party","Canton", "Election date"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("Tweets with local cues", "Total Tweets", "Incumbent MP",
                               "Tenure", "Additional Upper House candidacy", "Female", "Total votes",
                               "Constant")
          )



