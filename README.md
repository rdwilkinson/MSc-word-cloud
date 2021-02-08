# Wordcloud for LinkedIn banner
04-05/02/2021

# What does the script do?
Creates a wordcloud PNG from my MSc in Social Research (2019-20) submissions (approx. 50,000 words).

* Imports MSc submission files (docx)
* Cleans text (stopwords, short words, etc.)
* Does some (very) basic lemmatising (custom loop)
* Outputs wordcloud that is saved as a PNG file via a webpage 

# Final banner PNG
![LinkedIn banner PNG](https://github.com/rdwilkinson/MSc-word-cloud/blob/master/Blue,%20Caveat.png?raw=true)

# What resources does it use?
* Followed various online tutorials to use the wordcloud2 package in R. Began with: https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a, but did look at others too. 
* Used many answers to Stackexchange questions (URLs are quoted in the script). 
* For the stemming loop, relied heavily on this page (https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html) to write the correct regex.
* The final wordcloud uses the attractive 'Caveat' font: https://fonts.google.com/specimen/Caveat?category=Handwriting&preview.text_type=custom#license

Thank you!
