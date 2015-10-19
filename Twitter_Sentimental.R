#######
#Name: Twitter Sentiment Analysis
#Description: Sentiment Analysis of English Premier League match between Chelsea vs Aston Villa on 10/17/2015
#######

# Clearing environment objects
rm(list=ls())

#Install Packages
installed.packages("twitteR")
installed.packages("ROAuth")
installed.packages("grid")
installed.packages("ggplot2")
installed.packages("plyr")
installed.packages("gridExtra")

#Import Packages
library("twitteR")
library("ROAuth")
library("ggplot2")
library("plyr")
library("gridExtra")
library("grid")

#Load Twitter Authentication
load("twitter authentication.Rdata")

#Consumer_Key of your twitter account
consumer_key <- "P6SuHMpwZ3dD4p8RNA1XV0mXI"

#Consumer_Secret key of your twitter account
consumer_secret<- "NOFstXVHZ3bH3U4u0E5xwuFG0VSitSFT9JtBqLHhq15xrXz44v"

#Handshaking with twitter
setup_twitter_oauth(consumer_key, consumer_secret, access_token=NULL, access_secret=NULL)

#Search for the string, provide no of tweets to fetch and create dataframe
search.string <- "#AVFC"
no.of.tweets <- 250
tweets <- searchTwitter(search.string, lang="en", n=no.of.tweets, since="2015-10-17", until = "2015-10-18")
avfc.df <- twListToDF(tweets)

#Search for the string, provide no of tweets to fetch and create dataframe
search.string <- "#CFC"
no.of.tweets <- 250
tweets <- searchTwitter(search.string, lang="en", n=no.of.tweets, since="2015-10-17", until = "2015-10-18")
cfc.df <- twListToDF(tweets)

# Cleansing 1st team tweets for processing

# Replace blank space ("rt")
cfc.df$text <- gsub("RT", "", cfc.df$text)

# Replace @UserName
cfc.df$text <- gsub("@\\w+", "", cfc.df$text)

# Remove punctuation
cfc.df$text <- gsub("[[:punct:]]", "", cfc.df$text)

# Remove links
cfc.df$text <- gsub("http\\w+", "", cfc.df$text)

# Remove tabs
cfc.df$text <- gsub("[ |\t]{2,}", "", cfc.df$text)

# Remove blank spaces at the beginning
cfc.df$text <- gsub("^ ", "", cfc.df$text)

# Remove blank spaces at the end
cfc.df$text <- gsub(" $", "", cfc.df$text)

# Remove unnecessary spaces (white spaces, tabs etc)
cfc.df$text = gsub("[ \t]{2,}", "", cfc.df$text)
cfc.df$text = gsub("^\\s+|\\s+$", "", cfc.df$text)

# Remove retweet entities from the stored tweets (text)
cfc.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", cfc.df$text)

# Remove all "@people"
cfc.df$text = gsub("@\\w+", "", cfc.df$text)

# Remove all the punctuation
cfc.df$text = gsub("[[:punct:]]", "", cfc.df$text)

# Remove numbers, we need only text for analytics
cfc.df$text = gsub("[[:digit:]]", "", cfc.df$text)

# Remove NAs, if any exists, from cfc.df$text (the collected and refined text in analysis)
cfc.df$text = cfc.df$text[!is.na(cfc.df$text)]

# Remove names (column headings) from the text, as we do not want them in the sentiment analysis
names(cfc.df$text) = NULL

# Replace \n
cfc.df$text <- gsub("[\n]", "", cfc.df$text)

# Convert all text to lower case
cfc.df$text <- tolower(cfc.df$text)

# Cleansing 2nd team tweets for processing

# Replace blank space ("rt")
avfc.df$text <- gsub("RT", "", avfc.df$text)

# Replace @UserName
avfc.df$text <- gsub("@\\w+", "", avfc.df$text)

# Remove punctuation
avfc.df$text <- gsub("[[:punct:]]", "", avfc.df$text)

# Remove links
avfc.df$text <- gsub("http\\w+", "", avfc.df$text)

# Remove tabs
avfc.df$text <- gsub("[ |\t]{2,}", "", avfc.df$text)

# Remove blank spaces at the beginning
avfc.df$text <- gsub("^ ", "", avfc.df$text)

# Remove blank spaces at the end
avfc.df$text <- gsub(" $", "", avfc.df$text)

# Remove unnecessary spaces (white spaces, tabs etc)
avfc.df$text = gsub("[ \t]{2,}", "", avfc.df$text)
avfc.df$text = gsub("^\\s+|\\s+$", "", avfc.df$text)

# Remove retweet entities from the stored tweets (text)
avfc.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", avfc.df$text)

# Remove all "@people"
avfc.df$text = gsub("@\\w+", "", avfc.df$text)

# Remove all the punctuation
avfc.df$text = gsub("[[:punct:]]", "", avfc.df$text)

# Remove numbers, we need only text for analytics
avfc.df$text = gsub("[[:digit:]]", "", avfc.df$text)

# Remove NAs, if any exists, from avfc.df$text (the collected and refined text in analysis)
avfc.df$text = avfc.df$text[!is.na(avfc.df$text)]

# Remove names (column headings) from the text, as we do not want them in the sentiment analysis
names(avfc.df$text) = NULL

# Replace \n
avfc.df$text <- gsub("[\n]", "", avfc.df$text)

# Convert all text to lower case
avfc.df$text <- tolower(avfc.df$text)

#Load the ScoreSentiment function that contains specific sentiment scoring algorithm
source("ScoreSentiment.R")

# Load positive and negative lexicon files used to score individual words
pos = scan(file="positive-words.txt",what="charcter", comment.char=";")
neg = scan(file="negative-words.txt",what="charcter", comment.char=";")

# Score all the tweets for each team using score.sentiment function
Chelsea <- score.sentiment(cfc.df$text, pos,neg)
AVilla <- score.sentiment(avfc.df$text, pos,neg)

# Change format of timestamp to CST
Chelsea$created <- format(cfc.df$created,tz="America/Chicago")
AVilla$created <- format(avfc.df$created,tz="America/Chicago")

# Group by hour, minutes
Chelsea$hour <- as.POSIXlt(cfc.df$created)$hour
Chelsea$min <- as.POSIXlt(cfc.df$created)$min
AVilla$hour <- as.POSIXlt(avfc.df$created)$hour
AVilla$min <- as.POSIXlt(avfc.df$created)$min

# Summary
Chelsea.summary <- ddply(Chelsea, c("hour","min"), summarise, N = length(score), avg = mean(score))
AVilla.summary <- ddply(AVilla, c("hour","min"), summarise, N = length(score), avg = mean(score))
Chelsea.summary$created <-as.POSIXct(factor(paste0(as.character(Chelsea.summary$hour),':',as.character(Chelsea.summary$min))) , format="%H:%M")
AVilla.summary$created <-as.POSIXct(factor(paste0(as.character(AVilla.summary$hour),':',as.character(AVilla.summary$min))) , format="%H:%M")

# Plot by time and average score
plot.Chelsea <- ggplot(Chelsea.summary, aes(x=created, y=avg))+ geom_line(color='blue')+ scale_x_datetime(limits = c(as.POSIXct(strptime("2015-10-19 23:20", "%Y-%m-%d %H:%M")), as.POSIXct(strptime("2015-10-19 23:59", "%Y-%m-%d %H:%M")))) + labs(title = "Chelsea", x = "Time", y = "Average Sentiment Score") + ylim(-1, 2) + theme_bw()
plot.AVilla <- ggplot(AVilla.summary, aes(x=created, y=avg))+ geom_line(color='red')+ scale_x_datetime(limits = c(as.POSIXct(strptime("2015-10-19 22:40", "%Y-%m-%d %H:%M")), as.POSIXct(strptime("2015-10-19 23:59", "%Y-%m-%d %H:%M")))) + labs(title = "AVilla", x = "Time", y = "Average Sentiment Score") + ylim(-1, 2) + theme_bw()

# Create legend for grid
legendtable <- data.frame(Time = c('23:20', '23:38', '23:42', '23:59'), Event = c("Game Begins","Half Time", "Second Half Begins","Final Whistle"))

legend <- tableGrob(legendtable, rows = NULL, theme=ttheme_default(colhead=list(fg_params = list(parse=TRUE))))

# Arrange plots on grid
grid.arrange(plot.Chelsea, plot.AVilla, nrow=2,as.table=TRUE,legend=legend,heights=c(3,1)) 