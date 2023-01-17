############################################
############################################
###                                      ###
### Local Cues Paper 					 ###
###                                      ###
############################################
############################################


# Prepared by Tomas Turner-Zwinkels


###############
# Preparation #
###############

# Change the language and date formatting to English if it is not already
	Sys.setenv(LANG = "EN")
	Sys.setlocale("LC_TIME", "English") # key, without this conversion to POSIXct date format does not work correctly
	Sys.getlocale(category = "LC_ALL")

# Set working directory
	setwd("F:/PolCa/Analysis/R/ProjectR036_localcues")
	getwd()

# Note: script requires PCC data as well as coded tweets data to be present in sub-folders

############
# Packages #
############

# Install packages if necessary
	if("openxlsx" %in% rownames(installed.packages()) == FALSE) {install.packages("openxlsx")}
	if("foreign" %in% rownames(installed.packages()) == FALSE) {install.packages("foreign")}
	if("sqldf" %in% rownames(installed.packages()) == FALSE) {install.packages("sqldf")}
	if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}
	if("stringi" %in% rownames(installed.packages()) == FALSE) {install.packages("stringi")}
	if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
	if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}
	if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
	if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
	if("tidyr" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyr")}
	if("cowplot" %in% rownames(installed.packages()) == FALSE) {install.packages("cowplot")}
	if("lme4" %in% rownames(installed.packages()) == FALSE) {install.packages("lme4")}
	if("stargazer" %in% rownames(installed.packages()) == FALSE) {install.packages("stargazer")}
	if("sjPlot" %in% rownames(installed.packages()) == FALSE) {install.packages("sjPlot")}
	if("sjstats" %in% rownames(installed.packages()) == FALSE) {install.packages("sjPlot")}
	if("ggpubr" %in% rownames(installed.packages()) == FALSE) {install.packages("sjPlot")}
	if("dotwhisker" %in% rownames(installed.packages()) == FALSE) {install.packages("dotwhisker")}
	if("psych" %in% rownames(installed.packages()) == FALSE) {install.packages("psych")}

# Load packages
	library(openxlsx)
	library(foreign)
	library(sqldf)
	library(stringr)
	library(stringi)
	library(dplyr)
	library(data.table)
	library(lubridate)
	library(ggplot2)
	library(tidyr)
	library(scales)
	library(cowplot)
	library(lme4)
	library(stargazer)
	library(sjPlot)
	library(sjstats)
	library(ggpubr)
	library(dotwhisker)
	library(psych)

substrRight <- function(x, n)
	{
		substr(x, nchar(x)-n+1, nchar(x))
	}	

#################

# Load all PCC data, except 'modules': see codebook at https://www.overleaf.com/read/fhykbgcjsmdn

#################

 ## about politicians

	# Load POLI - politician level data: basic not time varying biographical information like gender
	POLI = read.csv("./PCC/POLI.csv", header = TRUE, sep = ";")
	
	# some fixes to make our lives easier below
		POLI$twitter_screen_name <- as.character(POLI$twitter_screen_name)
		POLI$twitter_screen_name[which(nchar(POLI$twitter_screen_name) == 0)] <- NA
		POLI$twitter_id <- as.character(POLI$twitter_id)
		POLI$twitter_id[which(nchar(POLI$twitter_id) == 0)] <- NA

#	summary(POLI)
	names(POLI)
	head(POLI)
	nrow(POLI)

	# Load RESE - resume entries; so (political) jobs, interest group ties, eppisodes in parliaments, educational episodes e.t.s.
	RESE = read.csv("./PCC/RESE.csv", header = TRUE, sep = ";")
#	summary(RESE)
	names(RESE)
	head(RESE)

	# Load MEME - membership eppisodes; what parties where polticians a member of when
	MEME = read.csv("./PCC/MEME.csv", header = TRUE, sep = ";")
#	summary(MEME)
	names(MEME)
	head(MEME)
	nrow(MEME)
	
	# Load ELEN - election list entries; where where politicians on what election lists?
	ELEN = read.csv("./PCC/ELEN.csv", header = TRUE, sep = ";")
	summary(ELEN)
	names(ELEN)
	head(ELEN)
	nrow(ELEN)

 ## about institutions and other (institutional) contexts

	# Load PARL - all information on the level of parliaments, for example their first day in session
	PARL = read.csv("./PCC/PARL.csv", header = TRUE, sep = ";")
#	summary(PARL)
	names(PARL)
	head(PARL)
	nrow(PARL)

	# Load PART, all information on parties; connects to ParlGov when applicable
	PART = read.csv("./PCC/PART.csv", header = TRUE, sep = ";")
#	summary(PART)
	names(PART)
	head(PART)
	nrow(PART)

	# Load ELDI, information on election districts, connects to CLEA when applicable
	ELDI = read.csv("./PCC/ELDI.csv", header = TRUE, sep = ";")
#	summary(ELDI)
	names(ELDI)
	head(ELDI)
	nrow(ELDI)
	
	### and two other election related data-frames
	
		# Load ELEC, information on the level of elections, for example the date on which the election took place
		ELEC = read.csv("./PCC/ELEC.csv", header = TRUE, sep = ";")
#		summary(ELEC)
		names(ELEC)
		head(ELEC)
		nrow(ELEC)
		
		# Load ELLI, information on the level of election lists, for example the party_id (see PART) that produced this list
		ELLI = read.csv("./PCC/ELLI.csv", header = TRUE, sep = ";")
#		summary(ELLI)
		names(ELLI)
		head(ELLI)
		nrow(ELLI)
	
#################

# Load all tweet data from the digital lives project

#################

#################

# Oliver also made a script to find out if a tweet was a local que and got a slightly different result, here I am loading his data so I can compare the difference below.

	# PLEASE NOTE THAT THIS	needs to be loaded first because Oliver uses some of the same filesnames as I do and I need to overwrite dataframes otherwise my script does not work
# load("Reliability_Check_2022-02-17_1407.RData") 3 not done by default because there are a lot of dataframes in here I do not need

ls()

#################


	# CH
		CH_TWEE <- read.xlsx("./TWEETS/CHTWEETS_Summary_2020_03_04_final_version.xlsx", sheet = 1)
		CH_HITS <- read.xlsx("./TWEETS/CHTWEETS_Summary_2020_03_04_final_version.xlsx", sheet = 2)
		
		TWEE_CH_TWEE <- data.frame(CH_TWEE)
		TWEE_CH_HITS <- data.frame(CH_HITS)
		head(TWEE_CH_TWEE)
		head(TWEE_CH_HITS)

		colnames(TWEE_CH_HITS)[which(names(TWEE_CH_HITS) == "False.Positive?")] <- "false_positive"
		colnames(TWEE_CH_HITS)[which(names(TWEE_CH_HITS) == "False.Positive.")] <- "false_positive"
		TWEE_CH_HITS$false_positive <- trimws(TWEE_CH_HITS$false_positive)
		
	# DE
	
		# 2017
			DE_TWEE_2017 <- read.xlsx("./TWEETS/DETWEETS2017_Summary_2020-04-14_final_version.xlsx", sheet = 1)
			DE_HITS_2017 <- read.xlsx("./TWEETS/DETWEETS2017_Summary_2020-04-14_final_version.xlsx", sheet = 2)
			
		# 2013
			DE_TWEE_2013 <- read.xlsx("./TWEETS/DETWEETS2013_Summary_2020-04-08_final_version.xlsx", sheet = 1)
			DE_HITS_2013 <- read.xlsx("./TWEETS/DETWEETS2013_Summary_2020-04-08_final_version.xlsx", sheet = 2)
			
		# 2009
			DE_TWEE_2009 <- read.xlsx("./TWEETS/DETWEETS2009_Summary_final_version.xlsx", sheet = 1)
			DE_HITS_2009 <- read.xlsx("./TWEETS/DETWEETS2009_Summary_final_version.xlsx", sheet = 2)
			
		# 2005
			DE_TWEE_2005 <- read.xlsx("./TWEETS/DETWEETS2005_Summary_final_version.xlsx", sheet = 1)
			DE_HITS_2005 <- read.xlsx("./TWEETS/DETWEETS2005_Summary_final_version.xlsx", sheet = 2)
			

#################

# Load data from Stoffel & Sieberer - electoral prospects data in DE - this with the idea that it is in particular those MPs that need the votes that will do this.
# see https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/T1Q93A

EPP <- read.dta("bundestag_reelection_prospects.dta") # note on 09/02/2022 --> trows an error now about the dataversion, this needs to be looked at.
head(EPP)

# maybe this setup does work?
EPP <- read.foreign("bundestag_reelection_prospects.dta", "dta", package = "stata")

#################

# Oliver also made a script to find out if a tweet was a local que and got a slightly different result, here I am loading his data so I can compare the difference below.

# load("Reliability_Check_2022-02-17_1407.RData") 3 not done by default because there are a lot of dataframes in here I do not need
ls()

#################

#################

# For each tweet figure out if their are <any> local cues this is the case when:
# - tweets_local_cues contains a value
# - when the tweetnumber district match does not occur in the last of false positives
# - an MPs its constituency is the same as the local que found

#################

	TWEETSLOC <- DE_TWEE_2017
    HITSLOC <- DE_HITS_2017
	
	getupdatedwastweetlocal <- function(TWEETSLOC,HITSLOC)
	{
	# input: 
		# 1) one data-frame with tweets 
		# 2) one data-frame with hits

	# some cleaning
		colnames(HITSLOC)[which(names(HITSLOC) == "False.Positive?")] <- "false_positive"
		colnames(HITSLOC)[which(names(HITSLOC) == "False.Positive.")] <- "false_positive"
		HITSLOC$false_positive <- trimws(HITSLOC$false_positive)

	# make a subset with the false positives (this is just helpful later on, the TWEETSLOC sheet is the one that forms the basis here!)
		HITS_FP <- HITSLOC[which(HITSLOC$false_positive == "Yes"),]

	# when there are multiple local cues, split them accross columns
		
		# implement the split
		longest <- max(str_count(TWEETSLOC$tweets_local_cues,";"),na.rm=T) 
		LC <- data.frame(str_split_fixed(TWEETSLOC$tweets_local_cues,";",longest+1)) # what happens here when there is only one!
		head(LC)
		
		TWEETSLOC <- cbind(TWEETSLOC,LC)
		head(TWEETSLOC)

	# get rid of all of the false positives in this data in a loop 
		# (so note how, to safe time, we loop through the false positive here and remove them, instead of checking for every row if..
		# it is a false positive
	
		pb <- txtProgressBar(min = 1, max = nrow(HITS_FP), style = 3)
		for(i in 1:nrow(HITS_FP))
		{
			mytweetnumber <- HITS_FP$tweetnumb[i]
			mydistrictmatch <- HITS_FP$DistrictMatch[i] 
		
			# find the tweet this concerns
			ROW <- TWEETSLOC[which(TWEETSLOC$tweetnumb == mytweetnumber),]
			rowoffset <- which(TWEETSLOC$tweetnumb == mytweetnumber)
			
			# find the column it concerns, right so this is KEY, it is not the tweet that is deleted here, it is one specific match that is set to blank!
			X1offset <- (which(as.vector(ROW[,match("X1",colnames(ROW)):ncol(ROW)]) == mydistrictmatch) - 1) + match("X1",colnames(TWEETSLOC))
			
			# set this value to empty
			TWEETSLOC[rowoffset,X1offset] <- ""
		
			setTxtProgressBar(pb, i)
		}
		close(pb)
		
	
	# collapse the column with all local cues to a single variable again, what happens here however to the Swiss case? Same story, can also be multiple hits
		TWEETSLOC <- data.table(TWEETSLOC)
		TWEETSLOC$tweets_local_cues_red <- unite(TWEETSLOC[,match("X1",colnames(TWEETSLOC)):ncol(TWEETSLOC)],"tweets_local_cues_red",sep=";") # right, so here it is all simplified again, makes sense
		
		# get rid of unnessary seperators (WHY?!)
		# TWEETSLOC$tweets_local_cues_red <- str_extract(TWEETSLOC$tweets_local_cues_red,"([A-Z]){2}((;([A-Z]{2}))){0,19}")

	# was this tweet a local cue for this MP?!
	
		resvec <- vector()
		pb <- txtProgressBar(min = 1, max = nrow(TWEETSLOC), style = 3)
		for(j in 1:nrow(TWEETSLOC))
		{
			if(TWEETSLOC$country[j] == "CH") # is the country CH, then check for a canton match?
			{
				resvec[j] <-  like(TWEETSLOC$tweets_local_cues_red[j],paste("(;|^)",TWEETSLOC$canton[j],"(;|$)",sep=""))
			}
			
			# is country DE, then check for a region or district match?
			if(TWEETSLOC$country[j] == "DE") # is the country DE, then check if their are local district matches as well .
			{
				resvec[j] <-  like(TWEETSLOC$tweets_local_cues_red[j], paste("(;|^)",TWEETSLOC$WKR_NAME[j],"(;|$)",sep="")) |
								length(
										intersect(
												unlist(split(as.character(TWEETSLOC$WKR_in_LAND[j]), "\\|")),
												unlist(strsplit(as.character(TWEETSLOC$tweets_local_cues_red[j]), ";"))
												 ))>0 
											     
				
			}
		
		setTxtProgressBar(pb, j)
		}
		close(pb)
	return(resvec)
	}
	table(resvec)
	
	TWEE_CH_TWEE$tweetislocalque <- getupdatedwastweetlocal(TWEE_CH_TWEE,TWEE_CH_HITS) # this has been tested against the version that ran by itself and it all looked good!
	table(TWEE_CH_TWEE$tweetislocalque)
	sum(table(TWEE_CH_TWEE$tweetislocalque))
	
	# investigating some potential issues with the function above that Oliver pointed out on the 17th of Feb
	table(TWEETSLOC$tweets_local_cues_red) 
	table(TWEETSLOC$canton)
	
	# testing the behavior of %like% 
	veca <- c("berlin-ost","ost berlin","ost berlin ost","berlin","paris")
	
	veca %like% "berlin" # ok, so this does go wrong indeed!, This has been fixed above now I think!
	veca %like% paste("(;|^)","berlin","(;|$)",sep="") 
	like(veca,paste("(;|^)","berlin","(;|$)",sep=""))
	
	# DE
	
		# DE 2017 
		DE_TWEE_2017$tweetislocalque <- getupdatedwastweetlocal(DE_TWEE_2017,DE_HITS_2017)
		table(DE_TWEE_2017$tweetislocalque)
		sum(table(DE_TWEE_2017$tweetislocalque))
		
		# DE 2013
		DE_TWEE_2013$tweetislocalque <- getupdatedwastweetlocal(DE_TWEE_2013,DE_HITS_2013)
		
		# DE 2009
		DE_TWEE_2009$tweetislocalque <- getupdatedwastweetlocal(DE_TWEE_2009,DE_HITS_2009)
		
		# DE 2005
		DE_TWEE_2005$tweetislocalque <- getupdatedwastweetlocal(DE_TWEE_2005,DE_HITS_2005)
	
	# somewhere around here this can be ran for each data-set with a rbind or something?!

	TWEE_CH_TWEE$candidacies <- rep("L",nrow(TWEE_CH_TWEE))

	TWEE <- rbind(	as.data.table(TWEE_CH_TWEE[c("pers_id","country","tweet_timestamp","text","tweetislocalque","candidacies")]),
				as.data.table(DE_TWEE_2017[c("pers_id","country","tweet_timestamp","text","tweetislocalque","candidacies")]),
				as.data.table(DE_TWEE_2013[c("pers_id","country","tweet_timestamp","text","tweetislocalque","candidacies")]),
				as.data.table(DE_TWEE_2009[c("pers_id","country","tweet_timestamp","text","tweetislocalque","candidacies")]),
				as.data.table(DE_TWEE_2005[c("pers_id","country","tweet_timestamp","text","tweetislocalque","candidacies")])
			 )
	head(TWEE)
	tail(TWEE)
	
	table(TWEE$candidacies)
	table(TWEE$country)
	
		# comparing what I have with what Oliver made
		
			# Merge the No,Yes and No category (as those hits that contain a "no" are true hits, i.e. true positives) <-- code from Oliver '. I don’t recode this variable in my script, so to make the classification obvious, I suggest running this additional bit of code:'

				table(TWEETSCODED$false_positive) # the variable Oliver mentioned in his email was not correct (it did not exist, I think he should have suggested me to use the variable named 'false_positive')
				TWEETSCODED[TWEETSCODED$false_positive=="No,Yes",]$false_positive <-  "true positive"
				TWEETSCODED[TWEETSCODED$false_positive=="No",]$false_positive <-  "true positive"
				TWEETSCODED[TWEETSCODED$false_positive=="Yes",]$false_positive <-  "false positive"
				table(TWEETSCODED$tweets_local_cues)
				table(TWEETSCODED$false_positive)
			
		head(TWEETSCODED)
		names(TWEETSCODED)
		nrow(TWEETSCODED)
	
	# so the totals match here
	table(TWEE_CH_TWEE$tweetislocalque)
	TOT_TWEET_COUNT_ALT
	
	# lets see if these aggregates from Oliver indeed come from the 'false_positives' variable
	table(TWEETSCODED$country,TWEETSCODED$false_positive) # not a match! I end up with a higher number here for CH! -- so Oliver needs to check this!
	
	# to compare Oliver and my count we need to match on tweet_id
	
		# for the swiss case
			TWEETSCODED_OLIVER_CH <- TWEETSCODED[which(TWEETSCODED$country == "CH"),]
			nrow(TWEETSCODED_OLIVER_CH)
			nrow(TWEE_CH_TWEE)
			
			NEW <- sqldf("SELECT TWEE_CH_TWEE.pers_id, TWEE_CH_TWEE.country, TWEE_CH_TWEE.tweet_id, TWEE_CH_TWEE.tweetislocalque, TWEE_CH_TWEE.tweets_local_cues, TWEE_CH_TWEE.text, TWEE_CH_TWEE.canton, TWEETSCODED.false_positive
						  FROM TWEE_CH_TWEE LEFT JOIN TWEETSCODED
						  ON 
						  TWEE_CH_TWEE.tweet_id = TWEETSCODED.tweet_id
						")
			nrow(NEW) # so, one row more... why? 
			length(unique(TWEE_CH_TWEE$tweet_id))
			length(unique(TWEETSCODED_OLIVER_CH$tweet_id))
			
			NEW$tweet_id_char <- as.character(NEW$tweet_id)
			
			head(NEW)
			table(NEW$tweetislocalque,NEW$false_positive)
			
			# now, who are these 347 cases?
				CH_ERR <- NEW[which(NEW$tweetislocalque == FALSE & NEW$false_positive == "true positive"),]
				head(CH_ERR)
		
		# for the German cohorts
		
			# difference between me and Oliver descriptively
			# 2005
				table(DE_TWEE_2005$tweetislocalque) # I count 275
				TOT_TWEET_COUNT_ALT # Oliver counts 357<MORE -- (note that the number is higher below because of the duplicated cases!)
				
				TWEETSCODED_OLIVER_DE2005 <- TWEETSCODED[which(TWEETSCODED$df_id == "DE_2005"),]
				nrow(TWEETSCODED_OLIVER_DE2005)
				nrow(DE_TWEE_2005)
				
				NEW_DE2005 <- sqldf("SELECT DE_TWEE_2005.pers_id, DE_TWEE_2005.country, DE_TWEE_2005.tweet_id, DE_TWEE_2005.tweetislocalque, DE_TWEE_2005.tweets_local_cues, DE_TWEE_2005.text, DE_TWEE_2005.WKR_NAME, TWEETSCODED.false_positive
							  FROM DE_TWEE_2005 LEFT JOIN TWEETSCODED
							  ON 
							  DE_TWEE_2005.tweet_id = TWEETSCODED.tweet_id
							")
				nrow(NEW_DE2005) # so we won a couple of cases here for some weird reason... rouding issues in excel again?
				length(unique(DE_TWEE_2005$tweet_id)) # so yes indeed, the tweets IDs are not unique!! 
				length(unique(TWEETSCODED_OLIVER_DE2005$tweet_id))
				
				DUBS <- DE_TWEE_2005[which(duplicated(DE_TWEE_2005$tweet_id)),]
				DUBS <- DUBS[order(DUBS$tweet_id),]
				nrow(DUBS)
				head(DUBS)
				tail(DUBS)
				
				# inspect some
				DE_TWEE_2005[which(DE_TWEE_2005$tweet_id==1387652418),] # this is actually also the exaxt same tweet?
				DE_TWEE_2005[which(DE_TWEE_2005$tweet_id==2612056748),] # this is actually also the exaxt same tweet?
				
				NEW_DE2005$tweet_id_char <- as.character(NEW_DE2005$tweet_id)
				head(NEW_DE2005)
				
				# how many mismatches?!
				table(NEW_DE2005$tweetislocalque,NEW_DE2005$false_positive) # so same thing, Oliver catches mores
				
				# lets inspect these mismatches
				DE2005_ERR <- NEW_DE2005[which(NEW_DE2005$tweetislocalque == FALSE & NEW_DE2005$false_positive == "true positive"),]
				head(DE2005_ERR)
				nrow(DE2005_ERR)
				
				# what do I see?! -- 'tweets local cues' clearly gives a hit, howevever! Is the MP its own district among these?
				
			# 2009
				table(DE_TWEE_2009$tweetislocalque) # I count 12087
				TOT_TWEET_COUNT_ALT # Oliver counts 15635<MORE
			# 2013
				table(DE_TWEE_2013$tweetislocalque) # I count 25060
				TOT_TWEET_COUNT_ALT # Oliver counts 32001<MORE

#################

# now this information is prepared, lets build up our 'empty' data-frame so that we can merge this information into it in a moment

	# here, the unit of analysis is MP months: lets use Oliver his script for this as inspiration here!
	# the nice adventage of that is that we also get his nice tenure measure already
	
#################


	# For this variable, we will rely on the RESE data frame.

		# We reduce RESE to only include information on parliamentary membership on the national level - also country filter is here!
		RESERED <- RESE[which(RESE$pf_geolevel == "NT" & 
								 (RESE$pf_instdomain == "LE" | RESE$pf_instdomain == "LE-LH") &
								 RESE$pf_orglevel == "T3" & 
								 is.na(RESE$pf_policy_area) & 
								 RESE$pf_position == "01" &
								 (RESE$country == "CH" | RESE$country == "DE") 
								),]
		nrow(RESE)
		nrow(RESERED)
		head(RESERED)

		# Make the dates readable for R
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
			RESERED$chamber <- as.factor(str_extract(RESERED$parliament_id, "NR|SR|BT|TK|SE|DE"))

			head(RESERED)

		# Following Oliver, I now transpose this data to long format
		
		# Note: The function breaks if the start date is after the end date
		RESEREDLONG <- setDT(RESERED)[ , list(pers_id = pers_id, chamber = chamber,
											day = seq(as.Date(res_entry_start), as.Date(res_entry_end), by = "day")), by = 1:nrow(RESERED)]

		RESEREDLONG$nrow <- NULL # drop nrow
		head(RESEREDLONG)

		# Get rid of duplicated rows
		RESEREDLONG <- RESEREDLONG[!duplicated(RESEREDLONG),]

		# Order the data first according to "pers_id" and then to "day"
		RESEREDLONG <-  with(RESEREDLONG, RESEREDLONG[order(pers_id, day) , ])

		# Now, we calculate tenure on a daily level
		# To that end, we simply count (number) rows
		RESEREDLONG$daysinparl <- ave(as.character(RESEREDLONG$pers_id), as.character(RESEREDLONG$pers_id), FUN = seq_along)

		# Calculate tenure
		RESEREDLONG$tenure  <- as.numeric(RESEREDLONG$daysinparl)/365.2422 #365.2422 = Average duration of year

		# Remove all the numbers after the dot (no rounding)
		RESEREDLONG$tenure  <- round(RESEREDLONG$tenure,digits=2)

		# Now, we prepare RESEREDLONG for aggregation on the MP-month level
		# To that end, we generate a month variable like we did before.
		
		RESEREDLONG$monthnew <- format(as.Date(RESEREDLONG$day, format = "%Y-%m-%d"), "%Y-%m")
		
		# older slower version
			# RESEREDLONG$month <- str_extract(as.character(RESEREDLONG$day), "[0-9]{4}-[0-9]{2}") # [0-9]{4}-[0-9]{2} matches 4 digits followed by a hyphen and 2 digits

		# Obtain the mode for tenure for every MP-month in the RESEREDLONG
		# a) Get tenure mode by groups (pers_id, month) 
		
			# custom function from Oliver
			getmode <- function(v) {
			  uniqv <- unique(v)
			  uniqv[which.max(tabulate(match(v, uniqv)))]
			}
					
		RESEREDMONTH <- aggregate(RESEREDLONG$tenure, by = list(pers_id = RESEREDLONG$pers_id, month = RESEREDLONG$month), getmode)
		colnames(RESEREDMONTH)[match("x",colnames(RESEREDMONTH))] <- "tenure" # rename variable x to tenure

		RESEREDMONTH[1:20,]

#################

# both data-frames are now prepared: lets merge them together
	
#################

	# getting the tweets to have the correct date format
		
		TWEE$tweet_timestamp <- as.character(TWEE$tweet_timestamp)
		format.str <- "%a %b %d %H:%M:%S %z %Y"
		TWEE <- as.data.frame(TWEE)
		TWEE$tweet_timestamp_posix <- as.POSIXct(strptime(TWEE[,"tweet_timestamp"], format.str, tz = "GMT"), tz = "GMT")
		head(TWEE)
		
	# let's start with reducing the TWEE data to monthly sums
		TWEE$yr_month <- format(as.Date(TWEE$tweet_timestamp_posix), "%Y-%m")
		head(TWEE)
		
		TWEEMO <- sqldf("SELECT pers_id, yr_month, count(pers_id) as 'total_nr_of_tweets'
						 FROM TWEE
						 GROUP BY pers_id,yr_month
						")

		head(TWEEMO)
		
		
		TWEELQ <- TWEE[which(TWEE$tweetislocalque),]
		table(TWEE$tweetislocalque)
		nrow(TWEELQ)
		
		LQTWEEMO <- sqldf("SELECT pers_id, yr_month, count(pers_id) as 'nr_of_tweets_with_localque'
						 FROM TWEELQ
						 GROUP BY pers_id,yr_month
						")

		head(LQTWEEMO)


		# inspect
		TWEEMO[which(TWEEMO$pers_id == "CH_Graf_Maya_1962"),]
		LQTWEEMO[which(LQTWEEMO$pers_id == "CH_Graf_Maya_1962"),] # looks good!
		

	##
	# merging it all together
	##

		head(RESEREDMONTH)
		head(TWEEMO)
		
		table(RESEREDMONTH$pers_id %in% TWEEMO$pers_id) # lots of people in RESERED with no tweets
		table(TWEEMO$pers_id %in% RESEREDMONTH$pers_id) # no people in TWEEMO that are not in RESERED
		RESEREDMONTH$country <- substr(RESEREDMONTH$pers_id,0,2)
		table(RESEREDMONTH$country)

		DT <- sqldf("SELECT RESEREDMONTH.*, TWEEMO.total_nr_of_tweets
					 FROM RESEREDMONTH LEFT JOIN TWEEMO
					 ON
					 RESEREDMONTH.pers_id = TWEEMO.pers_id
					 AND
					 RESEREDMONTH.month = TWEEMO.yr_month
					")
		tail(DT)
		
		DT <- sqldf("SELECT DT.*, LQTWEEMO.nr_of_tweets_with_localque
					 FROM DT LEFT JOIN LQTWEEMO
					 ON
					 DT.pers_id = LQTWEEMO.pers_id
					 AND
					 DT.month = LQTWEEMO.yr_month
					")
		tail(DT)
		nrow(DT)
		
		DT$country <- substr(DT$pers_id,0,2)
		table(DT$country)
		head(DT)

	# if this returns NA for nr_of_tweets_with_localque, while the total number of tweets is > 0 set local to 0!
	summary(DT$nr_of_tweets_with_localque) # so this tells us that the far majority of MP month combos comes with no tweets at all, does that make sense?
	DT$nr_of_tweets_with_localque <- ifelse(is.na(DT$nr_of_tweets_with_localque) & DT$total_nr_of_tweets > 0, 0, DT$nr_of_tweets_with_localque)
	summary(DT$nr_of_tweets_with_localque)

	# a very basic descriptive
	DT$percentage_local_indvlevel <- DT$nr_of_tweets_with_localque / DT$total_nr_of_tweets
	head(DT)
	tail(DT)
	summary(DT$percentage_local_indvlevel)
	hist(DT$percentage_local_indvlevel)

#################

# starting with some analysis

#################


###
## H1: Swiss and German MPs use more local cues during the campaign season.
###


	## get the total over time
	
	# first, we need to make the date-ranges rele
	
		TWT <- sqldf("SELECT month, country, SUM(total_nr_of_tweets) as 'total_sum', SUM(nr_of_tweets_with_localque) as 'lq_sum'
						 FROM DT
						 GROUP BY month, country
						")
		head(TWT)
		tail(TWT)
		nrow(TWT)
		table(TWT$country)
		
			TWT$timest <- as.POSIXct(paste(TWT$month,"-01 00:00",sep=""))
				
	# merge in the relevant parliaments
		PARL_RED <- PARL[which((PARL$country_abb == "CH"|PARL$country_abb == "DE") & PARL$level == "NT" & (PARL$assembly_abb == "NR"|PARL$assembly_abb == "BT")),]
		nrow(PARL_RED)
		PARL_RED$leg_period_start_asdate  <- as.POSIXct(as.character(PARL_RED$leg_period_start),format=c("%d%b%Y"))
		PARL_RED$leg_period_end_asdate  <- as.POSIXct(as.character(PARL_RED$leg_period_end),format=c("%d%b%Y"))

	TWT <- sqldf("SELECT TWT.*, PARL_RED.parliament_id, PARL_RED.leg_period_start_asdate, PARL_RED.leg_period_end_asdate
				  FROM TWT LEFT JOIN PARL_RED
				  ON
				  TWT.timest >= PARL_RED.leg_period_start_asdate
				  AND
				  TWT.timest <= PARL_RED.leg_period_end_asdate
				  AND
				  TWT.country = PARL_RED.country_abb
				 ")
	
	# calculate a percentage
	TWT$pers_loc <- (TWT$lq_sum / TWT$total_sum)*100
	summary(TWT$pers_loc)
	
	# remove observations based on less then 20 observations
	hist(TWT$total_sum)
	TWT <- TWT[which(TWT$total_sum > 150),]
	nrow(TWT)
	
	# warping these leg period start dates to the first day of the month
	year(TWT$leg_period_start_asdate)
	month(TWT$leg_period_start_asdate)
	
	
	TWT$leg_period_start_firstmonthday <- as.POSIXct(paste(year(TWT$leg_period_start_asdate),"-",month(TWT$leg_period_start_asdate),"-01 00:00",sep=""))

	ggplot(TWT, aes(y=total_sum,x=timest, colour="Total sum")) +
		  geom_line(size=1) +
		  geom_line(aes(y=lq_sum,x=timest,colour="Number of tweets with local cue"),size=1) +
		  geom_line(aes(y = pers_loc*1000, colour = "Percentage of tweet with local cue"),size=2) +
		  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Relative number of local cues [%]")) +
		  scale_x_datetime(limits = c(as.POSIXct("2009-01-01 00:00:00 GMT"),as.POSIXct("2019-05-31 23:59:59 GMT"))) +
		  geom_vline(aes(xintercept=TWT$leg_period_start_firstmonthday), linetype=4, colour="darkgreen",size=1.2) +
		  facet_grid(country ~ .) + # facet_grid(country ~ .)
		  theme_pubr(base_size =24) +
		  scale_colour_manual(values = c("darkred", "black", "darkblue")) +
		  geom_rect(data=TWT, aes(xmin=leg_period_start_firstmonthday- months(6), xmax=leg_period_start_firstmonthday, ymin=1, ymax=Inf),alpha=0.007,fill="darkgreen")
	
	#### and a regression model
	
	
		TWT$country <- factor(TWT$country,levels=c("DE","CH"))
	
	## empty model
	
		# the model
			m_empty 		<- lmer(pers_loc~ 1 +
								(1 | country)
								,data=TWT)
			summary(m_empty)
			stargazer(m_empty,type="text",intercept.bottom=FALSE)
	
	## add a lineair time-trend
	
		# the var prep
			TWT$year <- year(TWT$timest)
			medyear <- median(TWT$year)
			medyear
			TWT$year_cent <- TWT$year - medyear
			summary(TWT$year_cent)
			table(TWT$year_cent)
			
		# the model
		
			# general time trend
			m_time_gen 		<- lmer(pers_loc~ year_cent +
								(1 | country)
								,data=TWT)
			summary(m_time_gen)
			stargazer(m_empty,m_time_gen,type="text",intercept.bottom=FALSE)		
			plot_model(m_time_gen)
						
			# a country specific time-trend
			m_time_country 	<- lmer(pers_loc~ year_cent +
								(year_cent | country) 
								,data=TWT)			
			summary(m_time_country)
			ranef(m_time_country)
			se(m_time_country)
			
			
			attr(ranef(m_time_country, condVar = TRUE)[[1]], "postVar") # estimated standard errors are the values on the bottom right
			anova(m_time_gen,m_time_country) # yes, does fit much better.
			
			# some interpretation and inspections
			ranef(m_time_country)
			attr(ranef(m_time_country, condVar = TRUE)[[1]], "postVar") # estimated standard errors are the values on the bottom right
			plot_model(m_time_country,type = "re",se=TRUE)
			hist(TWT$pers_loc)
			
			
	## and add a model with a campaign season effect
		
		# the var prep
		
			# lets order TWT by month, just for easier inspection
			TWT <- TWT[order(TWT$country, TWT$timest),]
			TWT
		
			# number of months before the election - 
			TWT$NRMonthsBeforeElection <- round(as.numeric((TWT$leg_period_end_asdate - TWT$timest) /30),0) # taken from https://stackoverflow.com/questions/25369817/how-do-i-use-the-lubridate-package-to-calculate-the-number-of-months-between-two
		
			# and the core dummy
			TWT$campaign_season <- ifelse(TWT$NRMonthsBeforeElection <= 6,"yes","no")
		
		# the next step for the model
		
			# model with campaign season as fixed effect
				m_time_cs 	<- lmer(pers_loc~ year_cent +
									campaign_season +
									(year_cent | country) 
									,data=TWT)			
				summary(m_time_cs)
				ranef(m_time_cs)
				se(m_time_cs)
				plot_model(m_time_cs)
				plot_model(m_time_cs,type="re")
				
				# some interpretation and inspections
				mean(TWT$pers_loc)
				sd(TWT$pers_loc) # so about half a standard deviation more tweeting
				aggregate(data=TWT,pers_loc~campaign_season,mean) # so from roughly 8.5% to roughly 10.5%
				aggregate(data=TWT,pers_loc~campaign_season+country,mean) # descriptive difference is quite simular accross countries
	
			# model with campaign season as random effect as well
				m_time_cs_c 	<- lmer(pers_loc~ year_cent +
									(campaign_season | country) +
									(year_cent | country) 
									,data=TWT)			
				summary(m_time_cs_c)
				ranef(m_time_cs_c) # effects seem quite simular in size
				anova(m_time_cs,m_time_cs_c) # indeed, NOT a better model fit
	
	
	## plotting the effects
		plot_model(m_time_cs,se=TRUE)
		plot_model(m_time_cs,type = "re",se=TRUE)
	
	# stargazer output (needs quite some manual work as well still)
	
	stargazer(m_empty,
			  m_time_country,
			  m_time_cs,
			  type="text",
			  intercept.bottom=FALSE,
			  star.cutoffs = c(0.05, 0.01, 0.001))
	
	

###
## H3: Swiss and German MPs use more local cues when their tenure in the national parliament is low
###

		DT$pers_loc <- (DT$nr_of_tweets_with_localque / DT$total_nr_of_tweets)*100
		hist(DT$pers_loc)
		table(is.na(DT$pers_loc)) # lots of MP month combos ofcourse in which nobody tweets!
		
		hist(DT$pers_loc)
		
		ggplot(DT, aes(tenure, pers_loc)) +
		geom_point(color="darkgrey") + 
		geom_jitter(height=1,width=1) +
		geom_smooth(method = lm,size=1.5) +
		xlab("Parliamentary tenure in years") +
		ylab("Percentage of tweets with local cue") +
		facet_grid(country ~ .) + 
		scale_x_continuous(trans="sqrt") +
		scale_y_continuous(trans="sqrt",limits=c(0,100)) +
		theme_pubr(base_size =24)
		
		
		
	# lets do a regression model
		
		## for a starter, lets remove all of the observations for which we do not have any tweets of MPS
		nrow(DT)
		DT <- DT[which(!is.na(DT$total_nr_of_tweets)),]
		nrow(DT)
		
		# prepare DT as well
		head(DT)
		DT$timest <- as.POSIXct(paste(DT$month,"-01 00:00",sep=""))
		
		DT$country <- factor(DT$country,levels=c("DE","CH"))
		
		## empty model
		
			nrofsucces <- DT$nr_of_tweets_with_localque
			nrpffailures <- DT$total_nr_of_tweets - DT$nr_of_tweets_with_localque
			BinomialResponseMatrix <- as.matrix(cbind(nrofsucces,nrpffailures))
			head(BinomialResponseMatrix)
	
	
			# the model
				m_mp_empty 		<- glmer(BinomialResponseMatrix~ 1 + # m_mp_empty 		<- lmer(pers_loc~ 1 +
									(1 | country) +
									(1 | pers_id)
									,data=DT, family= binomial) # ,data=DT)
				summary(m_mp_empty)
				stargazer(m_mp_empty,type="text",intercept.bottom=FALSE)
				
				par(mfrow=2:1)
				hist(predict(m_mp_empty,type="response"),xlim=c(0,1),breaks=20) 
				hist(DT$pers_loc/100,xlim=c(0,1),breaks=40) # so we can see that the  model is not good at predicting the zero... 
			
		## controlling for time

			# the var prep
			DT$year <- year(DT$timest)
			medyear <- median(DT$year)
			medyear
			DT$year_cent <- DT$year - medyear
			summary(DT$year_cent)
			table(DT$year_cent)
			
		# the model
		
			# general time trend
				m_mp_time_gen 		<- glmer(BinomialResponseMatrix~ year_cent + # lmer(pers_loc~ year_cent +
									(1 | country) +
									(1 | pers_id)
									,data=DT, family= binomial) # ,data=DT)
				summary(m_mp_time_gen)
				stargazer(m_mp_empty,m_mp_time_gen,type="text",intercept.bottom=FALSE)
		
						
			# a country specific time-trend < this is the better model.
				m_mp_time_country 	<- glmer(BinomialResponseMatrix~ year_cent + # pers_loc~ year_cent +
									(year_cent | country) +
									(1 | pers_id)
									,data=DT, family= binomial) #	,data=DT)
				summary(m_mp_time_country)
				stargazer(m_mp_empty,m_mp_time_country,type="text",intercept.bottom=FALSE)
				
				ranef(m_mp_time_country)
				se(m_mp_time_country)
				anova(m_mp_time_gen,m_mp_time_country) # yes, clearly better
				
				par(mfrow=2:1)
				hist(predict(m_mp_time_country,type="response"),xlim=c(0,1),breaks=20) 
				hist(DT$pers_loc/100,xlim=c(0,1),breaks=40) 
				dev.off()
	
		## add age!
		
			# the var prep
				
				DT$age <- as.numeric(DT$year) - as.numeric(substrRight(DT$pers_id,4))
				head(DT)
				
				median(DT$age)
				
				DT$age_cent <- DT$age - median(DT$age)
				hist(DT$age_cent)
	
			# and the model
				# general
				m_mp_age 	<- glmer(BinomialResponseMatrix~ year_cent + # pers_loc~ year_cent +
									age_cent +
									(year_cent | country) +
									(1 | pers_id)
									,data=DT, family= binomial) #	,data=DT)
				summary(m_mp_age)
	
	
		## and a parliamentary tenure measure
			
				# var prep
				head(DT)
				
				hist(DT$tenure)
				mean(DT$tenure)
				DT$tenure_cent <- scale(DT$tenure, center = TRUE, scale = FALSE)
				hist(DT$tenure_cent)
			
				# general
				m_mp_tenure 	<- glmer(BinomialResponseMatrix~ year_cent + # pers_loc~ year_cent +
									age_cent +
									tenure_cent +
									(year_cent | country) +
									(1 | pers_id)
									,data=DT, family= binomial) #	,data=DT)
				summary(m_mp_tenure) # < this is the better model
				stargazer(m_mp_empty,m_mp_time_country,m_mp_tenure,type="text",intercept.bottom=FALSE)
				ranef(m_mp_tenure)
				se(m_mp_tenure)
						
				# country specific #
		#		m_mp_tenure_country <- glmer(BinomialResponseMatrix~ year_cent + # pers_loc~ year_cent +
		#									age_cent +
		#									tenure_cent +
		#									(tenure_cent | country) +
		#									(year_cent | country) +
		#									(1 | pers_id)
		#									,data=DT, family= binomial) #	,data=DT)
		#		summary(m_mp_tenure_country)
		#		anova(m_mp_tenure,m_mp_tenure_country) # NOT better
		#		stargazer(m_mp_empty,m_mp_time_country,m_mp_tenure,m_mp_tenure_country,type="text",intercept.bottom=FALSE)
	
		# add a campaign season effect here at the MP/month level.
		
			# first merge in relevant parliament level information
			nrow(DT)
			DT <- sqldf("SELECT DT.pers_id, DT.month, DT.tenure, DT.country, DT.total_nr_of_tweets, DT.nr_of_tweets_with_localque, DT.percentage_local_indvlevel, DT.pers_loc, DT.timest, DT.year, DT.year_cent, PARL_RED.parliament_id, PARL_RED.leg_period_start_asdate, PARL_RED.leg_period_end_asdate
				  FROM DT LEFT JOIN PARL_RED
				  ON (
				  DT.timest >= PARL_RED.leg_period_start_asdate
				  AND
				  DT.timest <= PARL_RED.leg_period_end_asdate
				  AND
				  DT.country = PARL_RED.country_abb)
				 ")
			nrow(DT)
			summary(DT$leg_period_start_asdate)
		
			# variable generation taken from above
			# number of months before the election - 
			DT$NRMonthsBeforeElection <- round(as.numeric((DT$leg_period_end_asdate - DT$timest) /30),0) 
			head(DT)
			summary(DT$NRMonthsBeforeElection)
			
			# and as the key dummy
			DT$campaign_season <- ifelse(DT$NRMonthsBeforeElection <= 6,"yes","no")
			table(DT$campaign_season)
			table(is.na(DT$campaign_season))
	
		# add this to the model
				m_mp_campaign_season  <- glmer(BinomialResponseMatrix~ year_cent + # pers_loc~ year_cent +
									age_cent +
									tenure_cent +
									campaign_season +
									(year_cent | country) +
									(1 | pers_id)
									,data=DT, family= binomial) #	,data=DT)
				summary(m_mp_campaign_season) # < this is the better model
				stargazer(m_mp_empty,m_mp_time_country,m_mp_tenure,m_mp_campaign_season,type="text",intercept.bottom=FALSE)
			#	ranef(m_mp_campaign_season)
			#	se(m_mp_campaign_season)
	
	
		# intpretation of effect sizes e.t.c.
		
			# for tenure
				meantenure <- mean(DT$tenure)
				tenuresd <- sd(DT$tenure)
				meantenure + (2*tenuresd)
				lowtenure = 1
				lowtenurecentvalue <- lowtenure - meantenure
		
				fix <- fixef(m_mp_tenure)
				fix
				
				exp(fix[1]+(fix[4]*lowtenurecentvalue))/(1+exp(fix[1]+(fix[4]*lowtenurecentvalue)))*100 # roughly 8.62 with low tenure
				
				exp(fix[1])/(1+exp(fix[1]))*100 										# roughly 7.69 at average tenure (7 years)
				
				exp(fix[1]+(fix[4]*tenuresd*2))/(1+exp(fix[1]+(fix[4]*tenuresd*2)))*100 # roughly 6.14 with tenure 2 SD above mean (18 years) 
	
			# for the campaign season effect
				fix2 <- fixef(m_mp_campaign_season)
				fix2
				
				exp(fix2[1])/(1+exp(fix2[1]))*100 # so about 5.5% indeed?
				
				exp(fix2[1]+fix2[5])/(1+exp(fix2[1]+fix2[5]))*100 # so would be about 7%, this increase is very simular to the one that came out of the country/month level analysis.
	

		# some simple (partially) copy/paste able stargazer output - RANDOM SLOPES NEED TO BE DONE MANUALLU!
		
			stargazer(m_mp_empty,
					  m_mp_time_country,
					  m_mp_tenure,
					  type="text",
					  intercept.bottom=FALSE,
					  star.cutoffs = c(0.05, 0.01, 0.001))

			plot_model(m_mp_tenure)
	
###
## H2: Swiss and German MPs use more local cues when the electoral system offers incentives to cultivate a personal vote.
###	

 # see country comparisons above!
 
	aggregate(TWT$pers_loc~TWT$country,data=TWT,mean) # percentage is much higher in Germany then in Switserland

# candidacy type

	# lets get a ELEN with a parliament ID in it
		
		ELENBU <- sqldf("SELECT ELEN.*, ELLI.parliament_id
						 FROM ELEN LEFT JOIN ELLI
						 ON
						 ELEN.list_id = ELLI.list_id
						")
		head(ELENBU)
		
		nrow(DT)
		
		nrow(DT)
		DT <- DT[which(!is.na(DT$total_nr_of_tweets)),]
		nrow(DT)
		
		DT$timest <- as.POSIXct(paste(DT$month,"-01 00:00",sep=""))
		DT <- sqldf("SELECT DT.*, PARL_RED.parliament_id, PARL_RED.leg_period_start_asdate, PARL_RED.leg_period_end_asdate
					  FROM DT LEFT JOIN PARL_RED
					  ON
					  DT.timest >= PARL_RED.leg_period_start_asdate
					  AND
					  DT.timest <= PARL_RED.leg_period_end_asdate
					  AND
					  DT.country = PARL_RED.country_abb
					 ")
		head(DT)
		tail(DT)
		nrow(DT)
		
		table(is.na(DT$parliament_id))
		# DT[which(is.na(DT$parliament_id)),] # not cases we are using!
		head(TEMP[which(is.na(TEMP$parliament_id) & TEMP$timest > as.POSIXct("2009-06-01 00:00:00 GMT")),])
	
	# merge them
		TEMP <- sqldf("	SELECT DT.*, ELENBU.candidature_type, ELENBU.candidate_votes
						FROM DT LEFT JOIN ELENBU
						ON 
						DT.pers_id = ELENBU.pers_id
						AND
						DT.parliament_id = ELENBU.parliament_id
		
		
					 ")
		nrow(TEMP)
		#fix later > note that for German MP with double candidatures I now double the observations!
		head(TEMP)
		
		# set all swiss cases to candidature_type : list
		table(TEMP$candidature_type)
		TEMP$candidature_type <- ifelse(TEMP$country == "CH","L",TEMP$candidature_type)
		table(TEMP$candidature_type)
		
		# so, for who is candidacy type missing here?!
		tail(TEMP[which(is.na(TEMP$candidature_type)),])
		
		
	# plot
	
		# aggregate
		TWT2 <- sqldf("SELECT month, country, candidature_type, 
					   SUM(total_nr_of_tweets) as 'total_sum', 
					   SUM(nr_of_tweets_with_localque) as 'lq_sum', 
					   MIN(timest) as 'timest', 
					   MIN(leg_period_start_asdate) as 'leg_period_start_asdate',
					   MIN(leg_period_end_asdate) as 'leg_period_end_asdate'
						 FROM TEMP
						 GROUP BY month, country, candidature_type
						")
		head(TWT2)
		tail(TWT2)
		nrow(TWT2)
		
		TWT2$timest <- as.POSIXct(TWT2$timest)
		TWT2$leg_period_start_asdate <- as.POSIXct(TWT2$leg_period_start_asdate)
		
		TWT2$pers_loc <- (TWT2$lq_sum / TWT2$total_sum)*100
		summary(TWT2$pers_loc)
		
		TWT2$country_plus_type <- paste(TWT2$country,TWT2$candidature_type)
		
		aggregate(pers_loc~country_plus_type,data=TWT2,mean)
		
		# so what is the hypothesises order here? # from converation with Stefanie and Natalie
		
			# [[ x ]] higher means stronger incentives
				# german list candidacies [[1]]
				# german mixed candidacies [[2]]
				# german disctrict candidacies [[3]]
				# swiss candidacies [[4]]
	
	####	
	# take the regression models from above!
	####
	
	
	TWT2$country <- factor(TWT2$country,levels=c("DE","CH"))
	
	## empty model
	
		# the model
			m2_empty 		<- lmer(pers_loc~ 1 +
								(1 | country)
								,data=TWT2)
			summary(m2_empty)
			stargazer(m2_empty,type="text",intercept.bottom=FALSE)
	
	## time	
		
		# var prep
		
			# the var prep
			TWT2$year <- year(TWT2$timest)
			medyear <- 2014
			medyear
			TWT2$year_cent <- TWT2$year - medyear
			summary(TWT2$year_cent)
			table(TWT2$year_cent)
		
		# the model
		
			# a time-trend
			m2_time	<- lmer(pers_loc~ year_cent +
								(1 | country) 
								,data=TWT2)			
			summary(m2_time)
			stargazer(m2_empty,m2_time,type="text",intercept.bottom=FALSE)
			
			ranef(m2_time)
			se(m2_time)
			
		
			# a country specific time-trend
			m2_time_country 	<- lmer(pers_loc~ year_cent +
								(year_cent | country) 
								,data=TWT2)			
			summary(m2_time_country)
			stargazer(m2_empty,m2_time_country,type="text",intercept.bottom=FALSE)
			
			anova(m2_time,m2_time_country)
			
			ranef(m2_time_country)
			se(m2_time_country)
		
	## and the candidacy type
			
			# variable prep
			table(TWT2$country_plus_type)
			TWT2$country_plus_type <- factor(TWT2$country_plus_type, level=c("DE L","DE LD","DE D","CH L"))
			
			# and the model
			m2_candidate_type <- lmer(pers_loc~year_cent +
									country_plus_type +
									(year_cent | country) 
									,data=TWT2)
								
			summary(m2_candidate_type)
			stargazer(m2_empty,m2_time_country,m2_candidate_type,type="text",intercept.bottom=FALSE)
			
			ranef(m2_candidate_type)
			se(m2_candidate_type)
	
	## adding campaign season here as well, as well as the interaction
	
		# the var prep
		
			# number of months before the election - 
			TWT2$NRMonthsBeforeElection <- round(as.numeric((TWT2$leg_period_end_asdate - TWT2$timest) /30),0) # taken from https://stackoverflow.com/questions/25369817/how-do-i-use-the-lubridate-package-to-calculate-the-number-of-months-between-two
		
			nrow(TWT2)
			head(TWT2)
		
			# and a dummy
			TWT2$campaign_season <- ifelse(TWT2$NRMonthsBeforeElection <= 6,"yes","no")
			TWT2
			table(TWT2$campaign_season)
		
		# the next step for the model
		
			# model with campaign season as fixed effect
				m2_time_type_cs  <- lmer(pers_loc~year_cent +
									country_plus_type * campaign_season +
									(year_cent | country) 
									,data=TWT2)	
				summary(m2_time_type_cs)
				stargazer(m2_empty,m2_time_country,m2_candidate_type,m2_time_type_cs,type="text",intercept.bottom=FALSE)
				
				ranef(m2_time_type_cs)
				se(m2_time_type_cs)
				
				plot_model(m2_time_type_cs)
				plot_model(m2_time_type_cs,type="re")
				plot_model(m2_time_type_cs)
	

	# some simple (partially) copy/paste able stargazer output - random effects needs to be done manually!
		
			stargazer(m2_empty,
					  m2_time_country,
					  m2_candidate_type,
					  m2_time_type_cs,
					  type="text",
					  intercept.bottom=FALSE,
					  star.cutoffs = c(0.05, 0.01, 0.001))

			summary(m2_time_type_cs)

#######		
#### safeness of seats?
#######

	
		TEMP$pers_loc <- (TEMP$nr_of_tweets_with_localque / TEMP$total_nr_of_tweets)*100
		hist(TEMP$pers_loc)
		table(is.na(TEMP$pers_loc)) # lots of MP month combos ofcourse in which nobody tweets!
		
		summary(TEMP$candidate_votes)
		TEMP$candidate_votes <- as.numeric(TEMP$candidate_votes)
		summary(TEMP$candidate_votes)
		
		head(TEMP)
		ggplot(TEMP, aes(candidate_votes, pers_loc)) +
		geom_point() + 
		geom_smooth(method = lm) +
		xlab("NR of votes in last election") +
		ylab("Percentage of tweets with a local cue") +
		ylim(c(0,25)) + 
		theme_pubr(base_size =24) +
		facet_grid(country ~ .)





# Building up a parliamentary eppisode data-frame, with politicians as often as they occur in parliaments and their state (e.g. party membership, tenure e.t.c.) at the start of each parliament

#################
	
	########
	#### we start with PARL and select the parliaments we are interested in
	########
	
		# lets see how many parliaments there are currently in the data
			nrow(PARL)
	
		# we can choose between the following countries
			table(PARL$country_abb)
		# and the following 'levels' (e.g. national, local e.t.c): see codebook!
			table(PARL$level)
	
		# lets select german national parliaments
			PARLBU <- PARL[which(PARL$country_abb == "DE" & PARL$level == "NT"),]

			# how many where there of these?
			nrow(PARLBU)
			
			# lets inspect these, to see what variables we have for them
			PARLBU
			
		# right, we can see some dates..  let's use this to focus on parliaments within a certain date range
		
			# for this we first need to get the human readable dates from PCC into the internal R-date format, the most computational efficient way to do this is like this, from the package 'lubridate'
			
				PARLBU$leg_period_start_asdate <- as.Date(fast_strptime(as.character(PARLBU$leg_period_start),"%d%b%Y")) 
				PARLBU$leg_period_end_asdate <- as.Date(fast_strptime(as.character(PARLBU$leg_period_end),"%d%b%Y")) 
	
				# lets inspect if these dates where indeed added correctly
				head(PARLBU)

			# now, lets focus on parliaments that started after the start of 1950 and before the end of 1980
			
				# define the start-cutoff and the end cutoff
				startcutoff <- as.Date(fast_strptime("01jan1950","%d%b%Y"))
				endcutoff <- as.Date(fast_strptime("31dec1980","%d%b%Y"))
			
				# and do the reduction
				PARLBU <- PARLBU[which(	PARLBU$leg_period_start_asdate >= startcutoff & 
										PARLBU$leg_period_end_asdate <= endcutoff
										
										),]
			
				# lets inspect the result of this
				PARLBU
		
		
	########
	#### we know use the resume entries (RESE) to see who (POLI) where in these parliaments
	########
	
		# this is how many resume entries we currently have per country_abb
			table(RESE$country)
	
		# we again want to focus on Germany
			nrow(RESE)
			RESEBU <- RESE[which(RESE$country == "DE"),]
			nrow(RESEBU)
						
		# get the dates in RESE in the right format
		
			# PCC data contains markers of 'left censoring' [[lcen]] and right censoring [[rcen]] when we only know for sure that a date what 'at least from this date onward' or 'at least until this date' respectively, the script user on purpose needs to explicitly deal with this censoring, otherwise the conversion to R-dates will be unsuccessfull.
			
				RESEBU$res_entry_start <- gsub("[[lcen]]", "", RESEBU$res_entry_start, fixed = TRUE)
				RESEBU$res_entry_end <- gsub("[[rcen]]", "", RESEBU$res_entry_end, fixed = TRUE)
			
			# conversion to the internal R date format
				RESEBU$res_entry_start_asdate <- as.Date(fast_strptime(as.character(RESEBU$res_entry_start),"%d%b%Y")) 
				RESEBU$res_entry_end_asdate <- as.Date(fast_strptime(as.character(RESEBU$res_entry_end),"%d%b%Y")) 
	
		# as we could see above, RESE contains a lot of resume entries, we are however at this stage, only interested in the ones about membership to the german bundestag
			# these entries have the following 'political function codes' (see the codebook)
				# pf_geo_level = NT
				# pf_instdomain	= LE and 'LE-LH' if there is a lower and upper house
				# pf_orglevel = T3
				# pf_policy_area = NA
				# pf_position = 01
				
			# so lets select these
				nrow(RESEBU)
				RESEBU <- RESEBU[which(
									RESEBU$pf_geolevel == "NT" & 
									(RESEBU$pf_instdomain == "LE" | RESEBU$pf_instdomain == "LE-LH") &  # '|' signifies an OR here.
									RESEBU$pf_orglevel == "T3" & 
									is.na(RESEBU$pf_policy_area) & 
									RESEBU$pf_position == "01" 
										),]
				# let's inspect the result
				nrow(RESEBU)
				head(RESEBU)
				table(RESEBU$country)				

				
		# now we use a SQL query with a so called 'left join' to add all of the German MPS that where in parliament 
			
			POPA <-	sqldf("SELECT PARLBU.parliament_id, PARLBU.leg_period_start_asdate, RESEBU.pers_id, RESEBU.res_entry_start_asdate, RESEBU.country
							FROM PARLBU LEFT JOIN RESEBU
							ON
								PARLBU.country_abb = RESEBU.country
								AND
								PARLBU.leg_period_start_asdate >= RESEBU.res_entry_start_asdate
								AND
								PARLBU.leg_period_start_asdate <= RESEBU.res_entry_end_asdate
						  ")
				  
			# 'unpacking' this SQL query
				# "SELECT PARLBU.parliament_id, RESEBU.pers_id" e.t.c. 
					 # meaning >>#  give me these two variables from these two data-frames - please not how SQL uses dots ('.') to seperate the variable (parliament_id) from its data-frame (PARLBU) and not '$' as is common in R.
					
				# "FROM PARLBU LEFT JOIN RESEBU"
					# meaning >># use the rows in PARLBU as the basis, and add as many rows from RESEBU to PARLBU as you can find matches.  Note: If you use WHERE instead of ON in the SQL query, rows in PARLBU get dropped if no matches in RESEBU can be found
					
				# "ON
				#				PARLBU.country_abb = RESEBU.country
				#				AND
				#				PARLBU.leg_period_start_asdate <= RESEBU.res_entry_start_asdate
				#				AND
				#				PARLBU.leg_period_start_asdate <= RESEBU.res_entry_end_asdate" > 
					# meaning >># match on country, and on dates: get all entries from RESEBU that started before or on the first day of each parliament and ended after (or on that day itself). So basically: pick a point on a time-line and give me all episodes that 'touch' this dot.
			
			# get rid of possible duplicates (pers_id parliament id combos) - in an ideal world these would not occur, but our experience thought us that in reality there is almost always a couple, no matter how hard we tried to avoid this. Please note that, for t
				
				# we make a string that contains person_parliament combinations
				POPA$fake_parl_episode_id <- paste(POPA$pers_id,POPA$parliament_id,sep="__")
				
				# how many duplicates are there? (number of TRUE indicates number of duplicates)
				table(duplicated(POPA$fake_parl_episode_id))
				
				# we run the reduction(nevertheless)
				nrow(POPA)
				POPABU <- POPA[which(!duplicated(POPA$fake_parl_episode_id)),]
				nrow(POPABU)
			
			# inspecting the result (which has the basic format we specified above: politicians at their first day in parliament).
			
				# looking at a snippit of the raw data - looking at the first twenty lines
				POPABU[1:20,]
				
				
				# some cross tabs to see if the numbers make sense (check the totals you see here with official sources!).
				table(POPABU$parliament_id)
	
	########
	#### having the basis structure of the data, let's enrich it with with some more information
	########	
	
		####
		## adding static individual level characteristics from POLI
		####
		
			# we again use LEFT JOIN, we write the results to a temporary data-frame so we can check if we do not accidentally have to many matches e.t.c.
			
				# first, lets have a look at the information we can found on politicians in POLI
					# look at top rows
					head(POLI)
				
					# how many from what country?
					
						# get country from primarty identifier (pers_id) by selecting its left two characters
						POLI$country <- substr(POLI$pers_id,0,2)
						
						# how many politicians are there in the data-frame from each country?, answer: plenty
						table(POLI$country)
						
			
			# running the query and checking what it does to our numbers of rows properly
			
				# checking the numbers before, at this point we have all of the people we need (not more, and not less!) in our data, so we don't want to loose or gain any rows in the data anymore!
					nrow(POPABU)
					
				# run the query
					TEMP <- sqldf("SELECT POPABU.pers_id, POPABU.parliament_id, POPABU.leg_period_start_asdate, POLI.first_name, POLI.last_name, POLI.gender, POLI.birth_date
								   FROM POPABU LEFT JOIN POLI
								   ON 
								   POPABU.pers_id = POLI.pers_id
								 ")
					nrow(TEMP)			
				
				# only continue / use this if we are happy with the numbers
					if(nrow(POPABU) == nrow(TEMP))
					{
						POPABU <- TEMP
					}
					
		
				# so, what does our data now look like?
					POPABU[0:10,]
			
					# inspect our result, lets see how many women their where in each parliament at this first day
					
						# first we need to aggregate (using 'tidyverse' solution)
							FORPLOT <- as.data.frame(POPABU %>% count(parliament_id,gender))
							
						# and we need the data when the parliament started
							# the .* below means 'everthing from FORPLOT')
							FORPLOT <- sqldf("SELECT FORPLOT.*, PARLBU.leg_period_start_asdate
											  FROM FORPLOT LEFT JOIN PARLBU
											  ON 
											  FORPLOT.parliament_id = PARLBU.parliament_id
											")
						# and country again as well (in case we need it at some point)
							FORPLOT$country <- substr(FORPLOT$parliament_id,0,2)
											
						# inspect the results (top 10 rows)
							FORPLOT[0:10,]
							
						# plot the result
							ggplot(data=FORPLOT, aes(x = leg_period_start_asdate, y = n, colour=gender)) +
							geom_line() +
							ylim(0,700) +
							ylab("Number of men and women") +
							facet_grid(country ~ .)
		
		####
		## adding party membership (MEME) and some party level information (PART)
		####		
		
		
			##
			# who was in what party when?
			##
		
			# what we are going to do here is very simular to what with did with POLI, with the one complication that we now need to take dates into account as well! (because people can switch parties!)
			
				# inspect MEME, look at somebody who switched
				MEME[which(MEME$pers_id == "NL_Wilders_Geert_1963"),]
				# we can see that he occurs as often in MEME as he was a member of different parties, and that these eppisodes are marked with dates, we thus needs these dates to know who was a member when
				
			# before proceeding, we need to again decide what to do with right and left censoring and transform things to the internal R format
				MEME$memep_startdate <- gsub("[[lcen]]", "", MEME$memep_startdate, fixed = TRUE)
				MEME$memep_enddate <- gsub("[[rcen]]", "", MEME$memep_enddate, fixed = TRUE)
				MEME$memep_startdate_asdate <- as.Date(fast_strptime(as.character(MEME$memep_startdate),"%d%b%Y")) 
				MEME$memep_enddate_asdate <- as.Date(fast_strptime(as.character(MEME$memep_enddate),"%d%b%Y")) 
			
				#inspect
				head(MEME)
			
			# now let's run the query, same setup we did above (pick a point on a time-line (entry date to parliament) and give me all episodes (in MEME) that 'touch' this dot, for this person(!)).
			
				nrow(POPABU)
			
				TEMP <- sqldf("SELECT POPABU.*, MEME.party_id
								   FROM POPABU LEFT JOIN MEME
								   ON 
								   POPABU.pers_id = MEME.pers_id
								   AND
									POPABU.leg_period_start_asdate >= MEME.memep_startdate_asdate
									AND
									POPABU.leg_period_start_asdate <= MEME.memep_enddate_asdate
								 ")
					nrow(TEMP)
				
				# only continue / use this if we are happy with the numbers
					if(nrow(POPABU) == nrow(TEMP))
					{
						POPABU <- TEMP
					}	
			
			# inspecting the results

				# top of data
				head(POPABU)
				
				# do the numbers make sense? (we use 'droplevels' here, because party_id is a factor and otherwise the table returns a lot of empty strings).
				table(droplevels(POPABU$party_id))
				
				# and graph them over time
				
						# aggreate to desired level using 'tidyverse' solution)
							FORPLOT2 <- as.data.frame(POPABU %>% count(parliament_id,party_id))
							
						# and we need the data when the parliament started
							# the .* below means 'everthing from FORPLOT2')
							FORPLOT2 <- sqldf("SELECT FORPLOT2.*, PARLBU.leg_period_start_asdate
											  FROM FORPLOT2 LEFT JOIN PARLBU
											  ON 
											  FORPLOT2.parliament_id = PARLBU.parliament_id
											")
						# and country again as well (in case we need it at some point)
							FORPLOT2$country <- substr(FORPLOT2$parliament_id,0,2)
											
						# inspect the results (top 10 rows)
							FORPLOT2[0:10,]
							
						# plot the result
							ggplot(data=FORPLOT2, aes(fill=party_id,x = leg_period_start_asdate, y = n)) +
							geom_bar(position="stack", stat="identity") +
							facet_grid(country ~ .) +
							ylab("Frequency")
		
			##
			# get in some additional party characteristics, like the party its party_id
			##
			
				# PART contains the party level information
			
				TEMP <- sqldf("SELECT POPABU.*, PART.party_abb, PART.party_parlgov_id
								   FROM POPABU LEFT JOIN PART
								   ON 
								   POPABU.party_id = PART.party_id
								 ")
				nrow(TEMP)
				nrow(POPABU)
				
				# only continue / use this if we are happy with the numbers
					if(nrow(POPABU) == nrow(TEMP))
					{
						POPABU <- TEMP
					}	
				
				# inspect
				POPABU[1:10,]
		
#######
## and an example from RESE: committee function before this parliamentary term?
#######		
		
				nrow(RESE)
			## please note that RESE contains all the resume entries, there are a lot of these, 
			# so we really need to be quite specific in what we are looking for.

				# Angela Merkel for example currently has about 40 rows in RESE
				RESE[which(RESE$pers_id == "DE_Merkel_Angela_1954"),]
				
				# lets for now focus on committee functions
				nrow(RESE)
				RESEBU <- RESE[which(RESE$pf_orglevel == "T2-CO"),]
				nrow(RESEBU)

			##			
			# Here, the dates need some decisions again, we apply to fixes for every row currently still in RESEBU
			##
			
				# left and right censoring
				RESEBU$res_entry_start <- as.character(RESEBU$res_entry_start)
				RESEBU$res_entry_end <- as.character(RESEBU$res_entry_end)
					
				RESEBU$res_entry_start <- gsub("[[lcen]]", "", RESEBU$res_entry_start, fixed = TRUE)
				RESEBU$res_entry_end <- gsub("[[rcen]]", "", RESEBU$res_entry_end, fixed = TRUE)
					
				# if we are missing a day, we pick midway the month
					
					# to see if we are making progress indeed
					table(nchar(RESEBU$res_entry_start))
					table(nchar(RESEBU$res_entry_end))
					
					RESEBU$res_entry_start <- ifelse(nchar(RESEBU$res_entry_start) == 7, paste0("15",RESEBU$res_entry_start),RESEBU$res_entry_start)
					RESEBU$res_entry_end <- ifelse(nchar(RESEBU$res_entry_end) == 7, paste0("15",RESEBU$res_entry_end),RESEBU$res_entry_end)

					table(nchar(RESEBU$res_entry_start))
					table(nchar(RESEBU$res_entry_end))

				# if we only have a year, we pick the 15th of June that year
					RESEBU$res_entry_start <- ifelse(nchar(RESEBU$res_entry_start) == 4, paste0("15jun",RESEBU$res_entry_start),RESEBU$res_entry_start)
					RESEBU$res_entry_end <- ifelse(nchar(RESEBU$res_entry_end) == 4, paste0("15jun",RESEBU$res_entry_end),RESEBU$res_entry_end)
						
						table(nchar(RESEBU$res_entry_start)) # we can see a handfull of issues with the dates are left we should go into the source data for and fix!
						table(nchar(RESEBU$res_entry_end))

					# we can lookup these problem cases like this; nice thing about 'pipeline' setup here: as soon as we have fixed these cases we only need to press the button!
							
						RESEBU[which(nchar(RESEBU$res_entry_start)==6),]
				
				# transform the dates that we have to internal R-dates
					RESEBU$res_entry_start_asdate <- as.Date(fast_strptime(as.character(RESEBU$res_entry_start),"%d%b%Y")) 
					RESEBU$res_entry_end_asdate <- as.Date(fast_strptime(as.character(RESEBU$res_entry_end),"%d%b%Y"))
				
					# what does Merkel look like now?
					RESEBU[which(RESEBU$pers_id == "DE_Merkel_Angela_1954"),]
			
		# now the dates are 'fixed' (except manual fixes in the source data that are still pending!) , lets get for each person, the last function they had before entering
			
			# for this, we first need a 'helper' data-frame, that merges in all 'past or' / 'active' party functions for this individual -- this multiplies to rows substantively... again, think time-lines! 
				
				# POPABU.leg_period_start_asdate > RESEBU.res_entry_end_asdate 
					# signifies:
					# >>> committee starts >>>> committee ends >
					# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>parliament starts
						
				# POPABU.leg_period_start_asdate >= RESEBU.res_entry_start_asdate
				# AND
				# POPABU.leg_period_start_asdate < RESEBU.res_entry_end_asdate
				# signifies:
					# >>> committee starts >>>>>>>>>>>>>>>>>>>>>>>>>>>> committee ends >
					# >>>>>>>>>>>>>>>>>>>>>>>>>>>parliament starts>>>>>>>>>>>>>>>>>>>>>>>>>>	
				
				
				TEMP2 <- sqldf("SELECT POPABU.*, RESEBU.res_entry_id, RESEBU.res_entry_raw,RESEBU.pf_orglevel,RESEBU.res_entry_start_asdate, RESEBU.res_entry_end_asdate
								   FROM POPABU LEFT JOIN RESEBU
								   ON 
								   POPABU.pers_id = RESEBU.pers_id
								   AND
								   (
										POPABU.leg_period_start_asdate > RESEBU.res_entry_end_asdate
										OR
										(
											POPABU.leg_period_start_asdate >= RESEBU.res_entry_start_asdate
											AND
											POPABU.leg_period_start_asdate < RESEBU.res_entry_end_asdate
										)
									)
								 ")
								 
				nrow(TEMP2)
				TEMP2[10:20,]
				
				table(is.na(TEMP2$res_entry_id))
				
			# to get the 'last function' we need to calculate the difference between the start date to this parliament and select the lowest value
				
				TEMP2$timesincelastfunction <- TEMP2$res_entry_end_asdate - TEMP2$leg_period_start_asdate
				TEMP2[0:200,]
				
			# focus on things in the past
				nrow(TEMP2)
				TEMP2 <- TEMP2[which(TEMP2$res_entry_start_asdate < TEMP2$leg_period_start_asdate),]
				nrow(TEMP2)
					
				# check
				table(round((as.numeric(TEMP2$timesincelastfunction)/365),0))
			
			# select the value closest to the entry date to parliament
				TEMP3 <- sqldf("SELECT 	  	TEMP2.pers_id,TEMP2.parliament_id,TEMP2.leg_period_start_asdate, TEMP2.res_entry_raw,TEMP2.pf_orglevel, MAX(timesincelastfunction)
									FROM TEMP2
									GROUP BY pers_id, parliament_id
								   ")
				nrow(TEMP3)
				TEMP3[1:10,]
					
				# merge ths into our 'master data frame'
				
				TEMP <- sqldf("SELECT POPABU.*, TEMP3.res_entry_raw, TEMP3.pf_orglevel
								   FROM POPABU LEFT JOIN TEMP3
								   ON 
								   POPABU.pers_id = TEMP3.pers_id
								   AND
								   POPABU.parliament_id = TEMP3.parliament_id
								 ")
					nrow(TEMP)
					nrow(POPABU)
				
				# only continue / use this if we are happy with the numbers
					if(nrow(POPABU) == nrow(TEMP))
					{
						POPABU <- TEMP
					}	
				
				# inspect
				POPABU[1:20,]		
				
				
#################

# Excercises 

#################
	
	# exercise A # adjust the script above (around line 180) to select a set of more recent parliaments
	
	# exercise B # adjust the script above to (around line 160 and line 210) select the desired data from both Switserland (CH), Germany (DE) and the Netherlands (NL)
		# Hint 1: you will need to add the correct country filter twice
		
		# Hint 2: adding additional data might bring to light new issues in the underlying data. Start with a fresh R instance to make sure that you are not incidentally working with 'old' data at some point. After adding new data, don't run the script as a whole but run it 'line by line' so you can deal with new issues if they come up. 
		
		# Hint 3: In Switserland party membership is in MEME is coded for the regional party. This means that the graph around line 500 has to many levels. If you have time you can add the 'mother_party_id' to the query on PART and move the graph down so you can use the national parties IDs as well in Switserland for the frequency counts.
		
	# exercise C # adjust the script above (around line 320) to also include the twitter screen name and twitter ID from POLI. Inspect for how many politicians we have this information and how this information is distributed accross countries. 
		# Hint 1: use table(is.na(...))
		
		# Hint 2: to be able to do this you will also need to generate or keep the country variable!
	
	# exercise D: adjust the script above to look for the last none-political function in the Netherlands MPS had before they entered parliament (use the codebook - page 12 or 14 - to find out what variable you need to filter on to look at none-political functions!) - if you have time-left, make an graphic that shows how much data currently already marked as 'prof' we actually have in what countries over time
	
	

	
