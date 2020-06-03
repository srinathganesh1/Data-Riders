#----setting Working diretory -----#
setwd("D:/Imarticus/R Learning/Vinodth Sir Docment/EDA")
# ----Readline from documen
readLines("Health.txt")
#--- Lets cub all liness ino one 

clean1 <-readLines("Health.txt")

clean1_pasted <- paste(clean1," ")
clean1_pasted

#-------Step2:Cleaning of daa----#
#Lower case#
clean2 <-  tolower(clean1_pasted)
head(clean2)

#----step3:Remove the punction------ #
clean3 <- gsub(pattern="\\W",replace=" ",clean2)
head(clean3)


#----step4: Remove the digits----#
clean4 <- gsub(pattern="\\d", replace="",clean3)

#-----step 5: Remove the stopwrds----#

library('tm')

stopwords()


clean5 <- removeWords(clean4,stopwords())


#----step 6: Single Letter ----#

clean6<- gsub(pattern="\\b[A-z]\\b{1}",replace="",clean5)

#----step7 : removewhitespace

clean7 <- stripWhitespace(clean6)
head(clean7)

#-----step 8: Spit the word------#

#we have a chunk of lines , and we are
#looking for ounting of words
#if ou remeber we have joined the various lines and made a chunk 
#so we a split aindividual word and madd a spae between the spitter 


clean8 <- strsplit(clean7," ")


clean8

#---Frequency word -------#

word_freq1<- table(clean8)

head(word_freq1)


word_freq2 <-cbind(names(word_freq1),as.integer(word_freq1))
head(word_freq2)

write.csv(word_freq2,"Word frequeny.csv")




# library
library(wordcloud2) 

# have a look to the example dataset
head(word_freq1)# library

# Gives a proposed palette
wordcloud2(word_freq1, size=1.6, color='random-dark')

# or a vector of colors. vector must be same length than input data
wordcloud2(word_freq1, size=1.6, color=rep_len( c("green","blue"), nrow(demoFreq) ) )

# Change the background color
wordcloud2(word_freq1, size=1.6, color='random-light', backgroundColor="black")

# Change the shape:
wordcloud2(word_freq1, size = 0.7, shape = 'star')



# Change the shape using your image
wordcloud2(word_freq1, figPath = "iphone.png", size = 1.5, color = "skyblue", backgroundColor="black")

wordcloud2(word_freq1, figPath = "health.png", size = 1.5, color = "skyblue", backgroundColor="black")

wordcloud2(word_freq1, figPath = "heart.png", size = 1.5, color=rep_len( c("orange","red"),nrow(word_freq1)), backgroundColor="#FFFFFF")

wordcloud2(word_freq1, figPath = "heart.png", size = 1.5, color=rep_len( c("orange","red"),nrow(word_freq1)), backgroundColor="black")

