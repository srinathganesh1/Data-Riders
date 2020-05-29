#Calories Wordcloud----
setwd('D:/Data Science/Dr Vinod online classes/Mini project')
getwd()
cal <- readLines('Calories.txt')
head(cal)

cal <- paste(cal,collapse = " ")
head(cal)

clean1 <- tolower(cal)
clean2 <- gsub(pattern = "\\W",replacement = " ",clean1)
clean3 <- gsub(pattern = "\\d",replacement = " ",clean2)
library(tm)
clean4 <- removeWords(clean3,stopwords())
clean5 <- gsub(pattern = "\\b[A-z]\\b{1}",replacement = " ",clean4)
clean6 <- stripWhitespace(clean5)
head(clean6)
clean7 <- removeWords(clean6,"â")
clean7 <- removeWords(clean7,"a")
clean7 <- stripWhitespace(clean7)
clean7 <- strsplit(clean7," ")
head(clean7)

freq <- table(clean7)
head(freq)
freq1 <- cbind(word=names(freq),f=as.numeric(freq))
head(freq1)
dim(freq1)
str(freq1)
write.csv(freq1,"frequency in calories.csv")

library(wordcloud)
library(wordcloud2)

unlistcal <- unlist(clean7)
wordcloud(unlistcal,min.freq = 10)

library(dplyr)
caldf <- read.csv("D:/Data Science/Dr Vinod online classes/Mini project/frequency in calories.csv")
head(caldf)
caldf <- caldf[,-2]
caldf$f <- as.numeric(cal$f)
str(caldf)
caldf <- filter(caldf,freq>8)
dim(caldf)

wordcloud2(caldf, shape='square', backgroundColor='black')
wordcloud2(caldf, figPath='D:/Data Science/Dr Vinod online classes/Mini project//Apple.jpg',size = 1)
letterCloud(caldf,word = "R")

