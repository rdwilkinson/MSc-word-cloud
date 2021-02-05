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
library(kableExtra) # Output tables

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
  docs[[j]] <- gsub("SWD", "satisfaction with democracy", docs[[j]])
  docs[[j]] <- gsub("TIRI", "trust in representative institutions", docs[[j]])
  docs[[j]] <- gsub("PCA", "principal components analysis", docs[[j]])
  docs[[j]] <- gsub("LCA", "latent class analysis", docs[[j]])
  docs[[j]] <- gsub("countries", "country", docs[[j]])
  docs[[j]] <- gsub("Latin America+", "LatinAmerica", docs[[j]]) # So don't get split
}

# Clean corpus (using tm library)
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(textclean::replace_non_ascii) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(tolower) %>%
  tm_map(removeWords, stopwords("SMART")) %>%
  tm_map(removeWords, c("tiri", "figure", "table", "finally", "clas", "level", "levels"))
   


# 
# 
# dotRemover <- function(x) gsub(x, pattern = "(\"|'|\\.\\.\\.)", replacement = "")
# ### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 
# docs <- tm_map(docs, content_transformer(dotRemover))  #??
# 
# docs<-tm_map(docs, 
#              content_transformer(function(x) gsub(x, 
#                                                   pattern = "(\"|'|\\.\\.\\.)", 
#                                                   replacement = "")))
#   

# Get word frequency
docs <- tm_map(docs, PlainTextDocument) # https://stackoverflow.com/questions/32225770/r-tm-removewords-function-not-removing-words
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)



#df$word <- gsub(pattern = "(\"|'|\\.\\.\\.)", "", df$word)
#df

# Eliminate words less than 3 letters
df$word <- gsub('\\b\\w{1,3}\\b', '', df$word)
df <- subset(df, word != "")

# Singularise where possible

# Reorder dataframe alphabetically to get shorter forms first
# And keep copy
df.single <- df[order(df$word),]

df.single <- cbind(df.single, u=1:nrow(df.single))

row.names(df.single) <- NULL

kable(df.single) %>%
  kableExtra::kable_styling()
#df.single <- df.single[1:300,] # Experiment with shorter df

newList <- data.frame(word=character(), 
                      freq=character(),
                      otherMatches=character())

newDfnrow <- nrow(df.single) # Initially set number of rows

for (i in 1:newDfnrow){
  # Reorder
  #df.single <- df.single[order(df.single$word),]
  
  print(paste(i, " / ", newDfnrow, " = ", round(i / newDfnrow * 100, 1), "%", sep = "")) # Print current position and total rows following eliminations

  searchTerm <- df.single[i, 1]
  searchTermFreq <- df.single[i, 2]
  
  tempData <- df.single[grepl(paste("^", df.single[i, 1], "(an|s|es|ed|er|ing|ment|ship|al|ly)*$", sep = ""), df.single$word), ]
  matches <- tempData[, 1]
  #print(matches)
  matchFreq <- tempData[, 2]
  
  if (length(matches) == 1) {
    #if (df.single[i, 3] != "") {
      newList <- rbind(newList, c(searchTerm, df.single[i, 2], "0"))
      #print(df.single[i, 1])
      
      # Flag as used
      #df.single[i, 1] <- ""
      df.single$freq[df.single$word == searchTerm] <- 0
    #}
  }
  else if (length(matches) > 1) {
    #if (df.single[i, 3] != "") {
      aggregateSum <- sum(matchFreq) # Sum match frequencies
      
      # Add this to newList
      newList <- rbind(newList, c(searchTerm, aggregateSum, paste(matches, collapse = ", ")))
      
      # Remove all summed terms
      for (k in 1:length(matches)) {
        
        df.single$freq[df.single$word == matches[k]] <- 0
        
        #df.single <- df.single[!(grepl(matches[k], df.single[, 1])),]
        
        #Add flag to matches
        #df.single[(grepl(matches[k], df.single[, 1])),] <- ""
        
        #newDfnrow <- nrow(df.single) # Update number of rows for loop
      #}
    }
  }
}

df.single


# Reorder by frequency
newList[, 2] <- as.numeric(as.character(newList[, 2]))
newListS <- newList[order(-newList[, 2]),]

# Remove NAs
newListT <- subset(newListS, newListS[,1] != TRUE)

# Show merged frequency table
kable(newListS) %>%
  kableExtra::kable_styling()

kable(newListS[!is.na(newListS[,1]),]) %>%
  kableExtra::kable_styling()

# Generate wordcloud
set.seed(1234)
wordcloud2(data=newListS, size=1, color='random-dark')
wordcloud(words = df$word, 
          freq = df$freq, 
          min.freq = 1,
          max.words=200, 
          random.order=FALSE, 
          rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))


string <- c("research", "researcher", "researchers", "researcherx")

string2 <- c("participant", "participants", 
             "participate", "participated", 
             "participating", "participative",
             "participation", "participatory")

hg <- string2[grepl("^participation(an|s|es|ed|er|ing|ment|ship|al|ly)*$", string2)]
hg


tempData <- df.single[grepl(paste("^", "participation", "(an|s|es|ed|er|ing|ment|ship|al|ly)*$", sep = ""), df.single$word), ]
matches <- tempData[, 1]
print(matches)
matchFreq <- tempData[, 2]

df.single[,2][df.single$word == "trust"] <- 0
