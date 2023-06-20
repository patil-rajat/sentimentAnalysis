# Read THE CSV file


apple <- read.csv(file.choose(), header = T)
str(apple)
first_line <- readLines(csv_file, n=1)

#checking the encode may show error in dif devices
#csv_file <- "paste\path\here\with.csv"
#data <- read.csv(csv_file, fileEncoding = "UTF-8")
#encoding <- Encoding(data)
#print(encoding)

# Building corpus.   'corpus  = collection of documents'
library(tm)       #tm : text mining , unstructured text into structured by analysing
Encoding(apple$text)
corpus <- iconv(apple$text, to = "utf-8") #used to convert given character vectors between encodings.
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#Data cleaning
corpus <- tm_map(corpus, tolower)    #converts in lowercase
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation) #remove punctuationss
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)  #remove integers
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english')) #remove all common words 
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))  #removes row appl
cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'stocks', 
                   replacement = 'stock')

cleanset <- tm_map(cleanset, stripWhitespace)  #removal space created after removal of punctn, int  
inspect(cleanset[1:5])

# Term document matrix.   
# tweets is unstructured data: to do further analysis tdm is used to convert into a structured data that is rows and columns
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm) #Convert that into matrix
tdm[1:10, 1:20] #(rows,columns)

# Bar plot
w <- rowSums(tdm) # 
w <- subset(w, w>=25)
barplot(w,
        las = 2,
        col = rainbow(50))

# Word cloud   (image representation acc. to usage freq)
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)     #repeatibility
wordcloud(words = names(w),
          freq = w,
          max.words = 150,      #no.of words
          random.order = F,
          min.freq = 5,     #any word that atleast 5 times repeated
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),     # size of the font
          rot.per = 0.7)

library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

letterCloud(w,
            word = "apple",
            size=1)

# Sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read file
apple <- read.csv(file.choose(), header = T)
tweets <- iconv(apple$text, to = 'utf-8-mac')

# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]
get_nrc_sentiment('delay')

#plotting the bar graph for getting the sentiment scores for 
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Apple Tweets')



