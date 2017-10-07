# Install
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# Select your own file
text <- readLines(file.choose())

# Read the text file from internet
filePath <- "<select your own url>"
text <- readLines(filePath)

# Load the data as a corpus
docs <- Corpus(VectorSource(text))

# Inspect the document
inspect(docs)

# Replacing special characters from text
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Building document term matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Building word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Explore frequent terms and their associations
findFreqTerms(dtm, lowfreq = 4)

# The frequency table of words
head(d, 10)

# Plot word frequencies
barplot(d[1:10,]$freq, las = 3, names.arg = d[1:10,]$word,
        col ="purple", main ="Most frequent words",
        ylab = "Word frequencies")