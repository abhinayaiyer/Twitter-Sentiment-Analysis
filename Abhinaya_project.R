

install.packages("twitteR")
install.packages("stringr")
install.packages("plyr")

library(stringr)
library(twitteR)
library(plyr)


api_key<- "GP90VZhZe4Ud0kzXzXGGxsjr2"
api_secret <- "JUr6cKf7EnFfI8zqFVBFKQJ4iEYx6Y03rnfFbAx174aJYgO9kV"
access_token <- "3302893622-K5oJ6OlZOLiSZiDvtJFWZ4c7m1OvDrSYLmnvnco"
access_token_secret <- "4tQ2ECOIEo0uOOFY4ilSJ4a9QgQTyLRWMWsBUgYndxqyl"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

z<-searchTwitter("heretocreate", n=10000, lang="en")
w<-searchTwitter("mygirls", n=10000, lang="en")

length(w)
length(z)

heretocreate <- sapply(z, function(x) x$getText())
mygirls <- sapply(w, function(x) x$getText())

#write.csv(w,'mygrils_tweets.csv')

catch.error = function(x)
{
# let us create a missing value for test purpose
y = NA
# Try to catch that error (NA) we just created
catch_error = tryCatch(tolower(x), error=function(e) e)
# if not an error
if (!inherits(catch_error, "error"))
y = tolower(x)
# check result if error exists, otherwise the function works fine.
return(y)
}

cleanTweets<- function(tweet){
# Clean the tweet for sentiment analysis
#  remove html links, which are not required for sentiment analysis
tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
# First we will remove retweet entities from  the stored tweets (text)
tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
# Then remove all "#Hashtag"
tweet = gsub("#\\w+", " ", tweet)
# Then remove all "@people"
tweet = gsub("@\\w+", " ", tweet)
# Then remove all the punctuation
tweet = gsub("[[:punct:]]", " ", tweet)
# Then remove numbers, we need only text for analytics
tweet = gsub("[[:digit:]]", " ", tweet)
# finally, we remove unnecessary spaces (white spaces, tabs etc)
tweet = gsub("[ \t]{2,}", " ", tweet)
tweet = gsub("^\\s+|\\s+$", "", tweet)
# if anything else, you feel, should be removed, you can.  For example "slang words" etc using the above function and methods.
# Next we'll convert all the word in lower case.  This makes uniform pattern.
tweet = catch.error(tweet)
tweet
}

cleanTweetsAndRemoveNAs<- function(Tweets) {
TweetsCleaned = sapply(Tweets, cleanTweets)
# Remove the "NA" tweets from this tweet list
TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
names(TweetsCleaned) = NULL
# Remove the repetitive tweets from this tweet list
TweetsCleaned = unique(TweetsCleaned)
TweetsCleaned
}

MyGirlsCleaned = cleanTweetsAndRemoveNAs(mygirls)
HereToCreateCleaned=cleanTweetsAndRemoveNAs(heretocreate)

length(MyGirlsCleaned)
length(HereToCreateCleaned)


neg.words = scan("negative-words.txt", what="character", comment.char=";")
pos.words = scan("positive-words.txt", what="character", comment.char=";")

neg.words = c(neg.words, 'wtf','smh')
pos.words = c(pos.words, 'lol')

getSentimentScore = function(sentences, words.positive,  words.negative,
.progress='none')
{
require(plyr)
require(stringr)
scores = laply(sentences,  function(sentence, words.positive, words.negative) {
# Let first remove the Digit, Punctuation character and Control characters:
sentence = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '',  gsub('\\d+', '', sentence)))
# Then lets convert all to lower sentence case:
sentence = tolower(sentence)
# Now lets split each sentence by the space delimiter
words = unlist(str_split(sentence, '\\s+'))
# Get the boolean match of each words with the positive & negative opinion-lexicon
pos.matches = !is.na(match(words, words.positive))
neg.matches = !is.na(match(words, words.negative))
# Now get the score as total positive sentiment minus the total negatives
score = sum(pos.matches) - sum(neg.matches)
return(score)
}, words.positive, words.negative, .progress=.progress )
# Return a data frame with respective sentence and the score
return(data.frame(text=sentences, score=scores)) }


MyGirlsResult = getSentimentScore(MyGirlsCleaned, pos.words ,  neg.words)

HereToCreateResult = getSentimentScore(HereToCreateCleaned, pos.words ,  neg.words)

class(MyGirlsResult)
head(MyGirlsResult)
head(HereToCreateResult)
names(MyGirlsResult)

par(mfrow=c(1,2))#we can partition the screen into parts to show multiple graps for understanding

hist(MyGirlsResult$score)
hist(HereToCreateResult$score)

write.csv(MyGirlsResult, "mygirls.csv")
write.csv(HereToCreateResult, "heartocreate.csv")


### Estimating emotions using Navye Bayes####

install.packages("Rstem")
install.packages("C:/Users/mano/Downloads/sentiment_0.2.tar.gz", repos = NULL, type = "source")
require(devtools)
require(sentiment)
ls("package:sentiment")
library(devtools)
library(sentiment)
# classify_emotion function returns an object of class data frame
#  with seven columns (anger, disgust, fear, joy, sadness, surprise,  #
# best_fit) and one row for each document:

HereToCreateClassEmo = classify_emotion(HereToCreateCleaned,  algorithm="bayes", prior=1.0)
MyGirlsClassEmo = classify_emotion(MyGirlsCleaned,  algorithm="bayes", prior=1.0)

head(MyGirlsClassEmo,10)
head(HereToCreateClassEmo,10)
MygirlsEmotion = MyGirlsClassEmo[,7]
HeretocreateEmotion = HereToCreateClassEmo[,7]


head(MygirlsEmotion,10)
head(HeretocreateEmotion,10)
head(MygirlsEmotion,50)
head(HeretocreateEmotion,50)
MyGirlsClassPol = classify_polarity(MyGirlsCleaned,  algorithm="bayes")
HereToCreateClassPol = classify_polarity(HereToCreateCleaned,  algorithm="bayes")


head(MyGirlsClassPol,10)
head(HereToCreateClassPol,10)

MygirlsSentimentDataFrame = data.frame(text=MyGirlsCleaned,  emotion=MygirlsEmotion, polarity=MyGirlsClassPol, stringsAsFactors=FALSE)
HeretocreateSentimentDataFrame = data.frame(text=HereToCreateCleaned,  emotion=HeretocreateEmotion, polarity=HereToCreateClassPol, stringsAsFactors=FALSE)

MygirlsSentimentDataFrame = within(MygirlsSentimentDataFrame, emotion <-  factor(emotion, levels=names(sort(table(emotion),  decreasing=TRUE))))
HeretocreateSentimentDataFrame = within(HeretocreateSentimentDataFrame, emotion <-  factor(emotion, levels=names(sort(table(emotion),  decreasing=TRUE))))

plotSentiments1<- function (sentiment_dataframe,title) {
library(ggplot2)
ggplot(sentiment_dataframe, aes(x=emotion)) +
geom_bar(aes(y=..count.., fill=emotion)) +
scale_fill_brewer(palette="Dark2") +
ggtitle(title) +
theme(legend.position='right') + ylab('Number of Tweets') +
xlab('Emotion Categories')
}

par(mfrow=c(1,2))
plotSentiments1(MygirlsSentimentDataFrame, 'Sentiment Analysis of  Tweets on Twitter about MyGirls')
plotSentiments1(HeretocreateSentimentDataFrame, 'Sentiment Analysis of  Tweets on Twitter about HereToCreate')


removeCustomeWords <- function (TweetsCleaned) {
  for(i in 1:length(TweetsCleaned)){
    TweetsCleaned[i] <- tryCatch({
      TweetsCleaned[i] = removeWords(TweetsCleaned[i],
                                     c(stopwords("english"), "care", "guys", "can", "dis", "didn",
                                       "guy" ,"booked", "plz"))
      TweetsCleaned[i]
    }, error=function(cond) {
      TweetsCleaned[i]
    }, warning=function(cond) {
      TweetsCleaned[i]
    })
  }
  return(TweetsCleaned)
}


getWordCloud <- function
(sentiment_dataframe, TweetsCleaned, Emotion) {
  emos = levels(factor(sentiment_dataframe$emotion))
  n_emos = length(emos)
  emo.docs = rep("", n_emos)
  TweetsCleaned = removeCustomeWords(TweetsCleaned)
  for (i in 1:n_emos){
    emo.docs[i] = paste(TweetsCleaned[Emotion ==
                                        emos[i]], collapse=" ")
  }
  corpus = Corpus(VectorSource(emo.docs))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  colnames(tdm) = emos
  require(wordcloud)
  suppressWarnings(comparison.cloud(tdm, colors =
                                      brewer.pal(n_emos, "Dark2"), scale = c(3,.5), random.order =
                                      FALSE, title.size = 1.5))
}

par(mfrow=c(1,2))
getWordCloud(MygirlsSentimentDataFrame, MyGirlsCleaned,
             MygirlsEmotion)
getWordCloud(HeretocreateSentimentDataFrame,HereToCreateCleaned,
             HeretocreateEmotion)
