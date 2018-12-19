install.packages("ROAuth")
library(ROAuth)
install.packages("twitteR")
library(twitteR)
install.packages("RCurl")
library(RCurl)

#Get Key and secret info from Twitter Dev account
key <- "yF6xCs6PFczQQlogBoA99i7A5"
secret <- "PD1KppOIpGQSsEgibfffvZbueXjMbffaA0jtDLWLlpWQeCg59U"
access_token <- "133165281-HEAsRQLIdMb3GqAZMNUssqxKPE6jJX8wMBc2GhbX"
access_secret <- "teMpJHC7MDtZhxmsW58F2wiFYHT0Apx2ylUnxsRAV4Tpt"

#Establishing Connection
setup_twitter_oauth(key, secret, access_token, access_secret)
1
#Checking Connection
searchTwitter("#AHA2018")

#Start with scraping
?userTimeline

userTimeline("@gauthamvinod")

?searchTwitteR

udemytweets <- searchTwitter("#AHA18", n=2000, lang = "en")

class(udemytweets)
length(udemytweets)
head(udemytweets)


install.packages("tm")
install.packages("corpus")
library(tm)

?sapply
?gettext

#Converting Tweets into character
udemylist <- sapply(udemytweets, function(x) x$getText())

# use the corpus function
# a corpus is the text body consisting of all the text including the meta info
udemycorpus <- VCorpus(VectorSource(udemylist)) 

?tm_map
#Remove characters functions
udemycorpus<-tm_map(udemycorpus, function(x) gsub("\\p{So}|\\p{Cn}", "", x, perl = TRUE))
udemycorpus<- tm_map(udemycorpus, function(x) gsub("[^[:alnum:]///' ]", "", x))

#Converting to lowercase
udemycorpus <- tm_map(udemycorpus, tolower)

#Removing Punctuations
udemycorpus <- tm_map(udemycorpus, removePunctuation)

#Remove numbers
udemycorpus <- tm_map(udemycorpus, removeNumbers)

#Removing stop words - meaningless words
?stopwords
udemycorpus <- tm_map(udemycorpus, function(x) removeWords(x,stopwords()))

#Other transfromations that you can do
?getTransformations

#Converting corpus to plain text which wordcloud can use
udemycorpus <- tm_map(udemycorpus,PlainTextDocument)

#Install wordcloud package
install.packages("wordcloud")
library(wordcloud)
install.packages("SnowballC")
library(SnowballC)
install.packages("RColorBrewer")
library(RColorBrewer)

?wordcloud

col <- brewer.pal(n =6, name = "Dark2")
wordcloud(udemycorpus, min.freq = 4, scale=c(5,1), rot.per = 0.5,
          max.words = 45, random.order = F, random.color = T, colors = col)

#Converting into a document matrix
ahatdm <- TermDocumentMatrix(udemycorpus)

ahatdm

#Finding frequent terms
findFreqTerms(ahatdm, lowfreq = 100)

#Finding Association
findAssocs(ahatdm, "amarin", 0.35)

#Remove sparse terms
ahatdm2 <- removeSparseTerms(ahatdm, sparse = 0.9)

#Scale the data
ahascale <- scale(ahatdm2)

#Calculate distance matrix with Euclidean distance
ahadist <- dist(ahascale, method = "euclidean")

#Hierarchical Clustering
ahafit <- hclust(ahadist)

#Visualize clustering
plot(ahafit)

#To get n number of clusters
cutree(ahafit, k=5)

#To color the clusters and plot them
rect.hclust(ahafit, k=6, border=2)


############ Working with gsub function  ####################
mystring <- "This is a string!"
mystring

#Changing the cases
toupper("Graphs and Histograms")

tolower("Graphs and Histograms")

#Splitting the string into single character values
strsplit("Graphs and Histograms", NULL)

#Splitting the string after space
strsplit("Graphs and Histograms"," ")


teststring <- c("my teststring to explain how substitution with my R Base works", 
                "another teststring for My example of gsub and sub")
teststring

# gsub/sub structure - sub replaces only first instance per string
?gsub

gsub("my","OUR", teststring)

sub("my", "oUR", teststring)

gsub("my", "OUR", teststring, ignore.case = T)

#Working with numbers
numberstring <- c("3445 is GReater than 23 - @???!§$",
                  "Tom coded 11 Java scrips and 23 Python scripts")
numberstring

#Deleting numbers
gsub("\\d","",numberstring)

#Deleting everything except numbers
gsub("\\D","",numberstring)

#Deleting spaces
gsub("\\s","",numberstring)

#Exchanging specific letters with Q
gsub("[iot]","Q",numberstring)

#Removing Punctuation
gsub("[[:punct:]]","",numberstring)

#Removing everything except graphical characters
gsub("[^[:graph:]]","",numberstring)


#Package stringer
install.packages("stringr")
library(stringr)

teststring <- c("my teststring to explain how substitution with my R Base works", 
                "another teststring for My example of gsub and sub")


numberstring <- c("3445 is GReater than 23 - @???!§$",
                  "Tom coded 11 Java scrips and 23 Python scripts")


# adding strings together
str_c(c(numberstring, teststring), sep="")


# we can count the occurences of a specific symbol in an element
str_count(numberstring, "3")


# we can locate the first and last position of a symbol in a given string
str_locate_all(numberstring, "3")


# replacement similar to sub - first occurence
str_replace(numberstring, "\\d", "")


# and gsub - all occurences
str_replace_all(numberstring, "\\d", "")


################Sentiment Analysis###########################

#Reading positive and negative lexicons
pos <- readLines(file.choose())
neg <- readLines(file.choose())

library(plyr)
library(stringr)
#Writing a score.sentiment function 

# function score.sentiment - this is how the whole function is written
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation - using global substitute
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits
                   sentence = gsub('\\d+', '', sentence)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

# Tweets for AHA competitors

ahatweets <- searchTwitter("#AHA18", n=2000, lang = "en")
esctweets <- searchTwitter("#ESCCongress", n=2000, lang = "en")
acctweets <- searchTwitter("@ACCinTouch", n=2000, lang = "en")
crftweets <- searchTwitter("#TCT2018", n=2000, lang = "en")

#get text
aha_txt <- sapply(ahatweets, function(x) x$getText())
esc_txt <- sapply(esctweets, function(x) x$getText())
acc_txt <- sapply(acctweets, function(x) x$getText())
crf_txt <- sapply(crftweets, function(x) x$getText())

#Length of each country's tweets
nd <- c(length(aha_txt), length(esc_txt), length(acc_txt), length(crf_txt))

#Join Texts
conference <- c(aha_txt, esc_txt, acc_txt, crf_txt)

scores <- score.sentiment(conference, pos, neg, .progress = "text")

#Adding variables to the data frame
scores$conf_name <- factor(rep(c("aha","esc","acc","crf"),nd))
scores$very.pos <- as.numeric(scores$score >= 2)
scores$very.neg <- as.numeric(scores$score <= -2)

numpos <- sum(scores$very.pos)
numneg <- sum(scores$very.neg)

#Global Score
global_score <- round(100 * numpos/ (numpos + numneg))

head(scores)

tail(scores)

boxplot(score~conf_name, data = scores)

library(lattice)

histogram(data=scores, ~score|conf_name, main="Sentiment Analysis of 4 Conferences", col=c("red", "grey"),
          xlab="", sub="Sentiment Score")
