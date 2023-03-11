--------------Packages-------------- 

install.packages("dplyr")
# usage: to create tibble(), work with gutenburg package, etc. 

install.packages("tidytext")
# for tokenization with unnest_tokens(),etc.

install.packages("janeaustenr")
# Novels of Jane Austen 

install.packages("stringr")
# to work with strings 

install.packages("getwiki")
# to get the text of a Wikipedia article 

install.packages("ggplot2")
# to visualize the results of text analysis, for example making bar charts of word counts

install.packages("gutenbergr")
#download books from Guetenburg Project

install.packages("textdata")
# to get_sentiment()

install.packages("wordcloud")
# to create a word cloud for text 

install.packages("tm")
# required to use wordcloud package  

install.packages("topicmodels")
# to get Asscoiated Press data

# load the packages 
library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(getwiki)
library(ggplot2)
library(gutenbergr)
library(wordcloud)
library(tm)
library(topicmodels)
----------------------Terminology----------------
                       

#character vector 
text <- c("A disaster is a serious problem",
          "occurring over a short or long period of time",
          "that causes widespread human, material, economic or environmental loss",
          "which exceeds the ability of the affected community or society to cope using its own resources.")
str(text)
#text is a typical character vector

#Mining text from the internet

  # Wikipedia articles 
get_wiki("Disaster")
disaster_tx <- get_wiki("Disaster")  
# We can then change this character to data frame, tokenize it,
#  remove its stop word, etc.  


  # findings books id from Gutenberg project 
gutenberg_works() %>% 
  filter(title == "Great Disasters and Horrors in the World's History") 
# id 51246 documentary 



#download disaster related books form Gutenberg project 
disaster_books <- gutenberg_download(c(51246,6716,64567, 15158))


# Tidy Text package

#Change character vector to data frame or tibble
text_df <- tibble(line= 1:4, text = text)



-----------------Tokenization------
  #tokenization at the word level (e.g., unigrams)
text_df %>% 
  unnest_tokens(disaster_words,text)
#unnest_tokens(the name of a a new column, input)


 #for packages like coreNLP, cleanNLP, sentimentr we should tokenize
 # at the sentence level

disaster_sentences <- tibble(text=disaster_tx) %>% 
  unnest_tokens(sentences, text, token = "sentences")
  #surprisingly this has produced only one sentence 

#Let's take a look at one sentence 
disaster_sentences$sentences[4]
#element four has more than one sentence! 


#Stop Words

#a list of 1149 stop words likes a, an, the, etc. from different lexicons
stop_words  # the command just shows the list 

data("stop_words")  # it seems the command adds stop words to your environment

anti_join(stop_words)

#Practice
disaster_tx <- get_wiki("Disaster")


disaster_df %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(n,word)) +
  geom_col()
  
#observation: The wiki entry was 608 words after removing stop words became 369 words

#Simple Sentiment Analysis
  
get_sentiments("afinn")

get_sentiments("bing")

get_sentiments("nrc")

#storing sentiment somewhere 
bing_thing <- get_sentiments("bing")


#counting positive and negative words in nrc
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("negative","positive")) %>% 
  count(sentiment)



#selecting words related to fear from nrc sentiment lexicon
NRC_fear <- get_sentiments("nrc") %>% 
  filter(sentiment == "fear")



#counting positive and negative words in disaster entry of wikipedia
disaster_tx <- get_wiki("Disaster")

disaster_df <- tibble(text = disaster_tx)

disaster_tidy <- disaster_df %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)

disaster_tidy %>% 
  inner_join(bing_thing) %>% 
  group_by(sentiment) %>% 
  count()


#calculating net sentiment of disaster entry of Wikipedia using AFINN sentiment  
AFINN <- get_sentiments("afinn")

disaster_tidy %>% 
  inner_join(get_sentiments("afinn")) %>% 
  summarise(affect = sum(value))
#general sentiment of disaster entry in Wikipedia is -97


#another task for sentiment analysis
#the proportion of negative words in a text 

#Word Clouds
  
wordcloud(disaster_tidy)

disaster_tidy %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 10))


#word frequency
#tf-idf analysis: the frequency of a word adjusted for how rarely it is used
#inverse frequency is useful to see common words within a specific context 
  
#n-grams
  
disaster_df %>% 
  unnest_tokens(bigrams, text, token = "ngrams", n = 2) %>% 
  count(bigrams)

#working with Document Term Matrix
  

data("AssociatedPress", package = "topicmodels")

AssociatedPress

APTerms <- Terms(AssociatedPress)  
str(APTerms)
tail(APTerms)

#AssociatedPress is an example of DTM: Document Term Matrix 
# we can change it to tidy data

AP_td <- tidy(AssociatedPress)








