# Word cloud for LinkedIn banner
# R. D. Wilkinson
# 04-05/02/2021

# Imports MSc submission files (docx), cleans text (stopwords, short words, etc.),
# does some basic stemming and then outputs wordcloud that is saved via webpage 

# Following various tutorials. 
# Began with: https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a


# Packages -----------
# install.packages("wordcloud") # Wordcloud package
# install.packages("wordcloud2")
# install.packages("RColorBrewer") # Gradient palette
# install.packages("tm") # Turn data into a corpus
# install.packages("textreadr") # Open Word files
# install.packages("webshot") # Packages for exporting wordcloud
# install.packages("htmlwidgets")
# webshot::install_phantomjs()

library(wordcloud) 
library(wordcloud2)
library(RColorBrewer)
library(tm)
library(textreadr)
library(tidyverse) # Piping
library(kableExtra) # Output tables
library(webshot)
library(htmlwidgets)

# Load text data ---------
MSD1 <- read_docx("Texts/MSD1.docx")
MSD2 <- read_docx("Texts/MSD2.docx")
TCM1 <- read_docx("Texts/TCM1.docx")
TCM2 <- read_docx("Texts/TCM2.docx")
Diss <- read_docx("Texts/Dissertation.docx")

  
# Create a corpus ---------
# Create a corpus called 'docs' from the source files
docs <- VCorpus(VectorSource(c(MSD1, MSD2, TCM1, TCM2, Diss)))

# Remove some punctuation, expand common acronyms, avoid noun groups getting split
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

# Clean corpus (using tm library) -----------
# Use textclean::replace_non_ascii to convert curly quotation marks into
# characters we can remove

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(textclean::replace_non_ascii) %>% #https://stackoverflow.com/questions/47173557/text-mining-r-package-regex-to-handle-replace-smart-curly-quotes
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(tolower) %>%
  tm_map(removeWords, stopwords("SMART")) %>%
  tm_map(removeWords, c("tiri", "figure", 
                        "table", "finally", 
                        "clas", "level", 
                        "levels", "variable",
                        "variables", "high",
                        "great", "greater",
                        "higher", "analy", 
                        "based", "using",
                        "found"))

# Get initial word frequency ----------
docs <- tm_map(docs, PlainTextDocument) # https://stackoverflow.com/questions/32225770/r-tm-removewords-function-not-removing-words
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Eliminate short words (<= 3 letters) -----------
df$word <- gsub('\\b\\w{1,3}\\b', '', df$word) # https://stackoverflow.com/questions/33226616/how-to-remove-words-of-specific-length-in-a-string-in-r
df <- subset(df, word != "")

# Basic, custom stemming ----------

# Reorder dataframe alphabetically to get shorter forms first
# And keep copy
df.single <- df[order(df$word),]
row.names(df.single) <- NULL

# Initial look at output
kable(df.single) %>% # https://stackoverflow.com/questions/22863771/is-it-possible-to-view-an-html-table-in-the-viewer-pane
  kableExtra::kable_styling()

# Create newlist for "stemmed" terms
newList <- data.frame(word=character(), 
                      freq=character(),
                      otherMatches=character())

# Shorthand for nb of rows to loop through
newDfnrow <- nrow(df.single) 

# Loop through each row and look for similar terms using 'grepl'
# If no other matches, add term to list and set frequency to 0
# If several matches, sum respective frequencies before setting them to 0
# Frequencies set to 0 so that matches not counted > once.

for (i in 1:newDfnrow){
  # Progress print
  print(paste(i, " / ", newDfnrow, " = ", round(i / newDfnrow * 100, 1), "%", sep = "")) # Print current position and total rows following eliminations

  # Shorthands
  searchTerm <- df.single[i, 1]
  searchTermFreq <- df.single[i, 2]
  
  # Look for matches and store rows in 'tempData'
  tempData <- df.single[grepl(paste("^", df.single[i, 1], "(an|s|es|ed|er|ing|ment|ship|al|ly)*$", sep = ""), df.single$word), ]
  matches <- tempData[, 1]
  matchFreq <- tempData[, 2]
  
  # No other matches (only term itself)
  if (length(matches) == 1) {
    
      # Add term to new list. No other matches
      newList <- rbind(newList, c(searchTerm, df.single[i, 2], "0"))
      
      # Set term's frequency to 0
      df.single$freq[df.single$word == searchTerm] <- 0
  }
  
  # Term itself + at least 1 other match
  else if (length(matches) > 1) {
    
    # Sum match frequencies (includes search term itself)
    aggregateSum <- sum(matchFreq) 
      
    # Add terms and sum to newList
    newList <- rbind(newList, c(searchTerm, aggregateSum, paste(matches, collapse = ", ")))
      
    # Set matches' frequencies to 0 by cycling through them
    for (k in 1:length(matches)) {
      
      df.single$freq[df.single$word == matches[k]] <- 0
    }
  }
}

## Reorder newLList by word frequency
newList[, 2] <- as.numeric(as.character(newList[, 2]))
newListS <- newList[order(-newList[, 2]),]

# Show merged frequency table
kable(newListS) %>%
  kableExtra::kable_styling()

# Generate wordcloud ---------
# Create custom colour palette
colorVec = colorRampPalette(c("black", "sky blue")) # https://stackoverflow.com/questions/13353213/gradient-of-n-colors-ranging-from-color-1-and-color-2
colorVec <- colorVec(15)

# Generate wordcloud
set.seed(1234)
wdc <- wordcloud2(data = newListS, # https://cran.r-project.org/web/packages/wordcloud2/wordcloud2.pdf
                  size = 1.05, 
                  color = colorVec,
                  fontFamily = "Caveat",
                  shuffle = TRUE,
                  rotateRatio = 0.4, 
                  shape = 'circle', 
                  ellipticity = 0.65,
                  widgetsize = NULL, 
                  figPath = NULL, 
                  hoverFunction = NULL)

# Export wordcloud via webpage --------
saveWidget(wdc,"4.html", selfcontained = F) # https://stackoverflow.com/questions/51759893/how-to-save-the-wordcloud-in-r
webshot::webshot("4.html","4.png",
                 vwidth = 1584, # LinkedIn banner dimensions
                 vheight = 396, 
                 delay =15) 