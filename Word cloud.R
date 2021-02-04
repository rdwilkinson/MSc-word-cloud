# Following: https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a

# Packages
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("wordcloud2")
install.packages("tm") # Turn data into a corpus
install.packages("textreadr") # Open Word files

library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(textreadr)
library(tidyverse)

# Load text data
MSD1 <- read_docx("Texts/MSD1.docx")
MSD2 <- read_docx("Texts/MSD2.docx")
TCM1 <- read_docx("Texts/TCM1.docx")
TCM2 <- read_docx("Texts/TCM2.docx")
Diss <- read_docx("Texts/Dissertation.docx")

  
# Create a corpus  
docs <- VCorpus(VectorSource(c(MSD1, MSD2, TCM1, TCM2, Diss)))

for(j in seq(docs)) # https://stackoverflow.com/questions/32225770/r-tm-removewords-function-not-removing-words
{
  docs[[j]] <- gsub("[[:punct:]]", " ", docs[[j]])
  docs[[j]] <- gsub("Latin America", "Latin-America", docs[[j]]) # So don't get split
}

# Clean corpus (using tm library)
docs <- docs %>%
  tm_map(removeNumbers) %>%
  #tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(tolower) %>%
  tm_map(removeWords, stopwords("SMART")) %>%
  tm_map(removeWords, c("tiri"))

# Get word frequency
docs <- tm_map(docs, PlainTextDocument) # https://stackoverflow.com/questions/32225770/r-tm-removewords-function-not-removing-words
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)


# Singularise where possible
df.single <- df
newList <- data.frame(word=character(), 
                      freq=character(),
                      otherMatches=character())

for (i in 1:200){
  #cat(paste0("\n", df.single[i, 1], "\n"))
  #cat(paste0(df.single[i, 2], "\n"))
  
  searchTerm <- df.single[i, 1]
  searchTermFreq <- df.single[i, 2]
  matches <- df.single$word[grepl(paste("^", df.single[i, 1], "s{1}", sep = ""), df.single$word)]
  matchFreq <- df.single$freq[grepl(paste("^", df.single[i, 1], "s{1}", sep = ""), df.single$word)]
  
  #print(matches)
  #print(matchFreq)
  
  if (length(grep(paste("^", df.single[i, 1], "s{1}", sep = ""), df.single$word)) == 0) {
    newList <- rbind(newList, c(searchTerm, searchTermFreq, ""))
  }
  else {
    aggregateSum <- sum(searchTermFreq, matchFreq) # Sum match frequencies
    
    # Add this to newList
    newList <- rbind(newList, c(searchTerm, aggregateSum, matches))
    
    # Remove all summed terms
    for (k in 1:length(matches)) {
      df.single <- df.single[!(grepl(matches[k], df.single[, 1])),]
    }
    
    
    
    # for (j in 1:length(matches)) { # Cycle through each match
    #   sumFreq <- sumFreq + as.integer(matchFreq[[j]])
    #   print(paste0("Sum: ", matchFreq))
    # }
    # keep root word, total frequencies, remove matching terms
  }
}


df444<-data.frame("hi","bye")

which()

grepl("level*", df.single$word)


for(i in 1:nrow(df.single)){
  print(df.single[i, 1])
  
  if(df.single[i, 1])
}

# Generate wordcloud
wordcloud2(data=df, size=1.6, color='random-dark')
wordcloud(words = df$word, 
          freq = df$freq, 
          min.freq = 1,
          max.words=200, 
          random.order=FALSE, 
          rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))
