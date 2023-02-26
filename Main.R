library(tm)
library(topicmodels)
library(SnowballC)
library(tidytext)
library(ggplot2)
library(dplyr)
library(purrr)
library(ggpubr)
library(tidyr)
library(syuzhet)

rm(list = ls())
cat("\014")

setwd("D:/università/progetti/Machine Learning and Evolutionary Robotics")
getwd()
graphics.off()

#load dataset
letters.csv = read.csv2("carteggio.svevo3.csv", fileEncoding = "UTF-8")

#extract corpus from dataset
letters.corpus = Corpus(VectorSource(letters.csv[, 12]))

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

# corpus_distr = letters.csv %>% group_by(corpus) %>% count() %>% arrange(-n) %>% ungroup()
# corpus_distr[4, 1] = "Altri"
# corpus_distr[4, 2] = colSums(corpus_distr[4:45,2])
# corpus_distr = corpus_distr[1:4,]
# corpus_distr = corpus_distr %>% arrange(-n)
# 
# 
# # corpus_distr %>% ggplot(aes(n, reorder(corpus, n))) +
# #   geom_col(show.legend = FALSE, alpha=.6, width=.5, fill = factor(corpus_distr$n))
# 
# langs_distr = letters.csv %>%
#   group_by(mainLanguage) %>%
#   count() %>%
#   arrange(-n) %>%
#   ungroup() %>%
#   rename("Numero_Lettere" = "n", "Lingua_Principale" = "mainLanguage")
# 
# pplist = list(langs_distr %>% 
#   mutate(Lingua_Principale = reorder(Lingua_Principale, Numero_Lettere)) %>%
#   ggplot(aes(Numero_Lettere, y="", fill = Lingua_Principale)) +
#   geom_bar(stat = "identity", show.legend = TRUE, alpha=.6, width=1) +
#   theme(axis.text.x = element_text(size = 8)),
#   corpus_distr %>%
#   rename("Numero_Lettere" = "n", "Raccolta" = "corpus") %>%
#   mutate(Raccolta = reorder(Raccolta, Numero_Lettere)) %>%
#   ggplot(aes(Numero_Lettere, y = "", fill = Raccolta)) +
#   geom_bar(stat = "identity", show.legend = TRUE, alpha=.6, width=1)+
#   theme(axis.text.x = element_text(size = 8))
# )
# 
# ggarrange(plotlist = pplist, nrow = 2)
# # testdual = corpus_distr %>% mutate(lang = langs_distr$mainLanguage, m = langs_distr$n)
# testdual = corpus_distr
# testdual[5:8, ] = langs_distr
# testdual = testdual %>% mutate(type = NA)
# testdual$type[1:4] = 1
# testdual$type[5:8] = 2
# 
# testdual = testdual %>% mutate(corpus = factor(corpus), type = factor(type, levels = c("1", "2"), ordered = TRUE))
# 
# # testdual %>% ggplot(aes(n, y="", fill = corpus)) +
# #   geom_bar(stat = "identity", show.legend = TRUE, alpha=.6, width=1)+
# #   facet_wrap(~type, nrow = 2)




#------------------------------------------------------------------------- Parameters: --------------------------------------------------------------------------------------------


#Parameters for language selection
param.lang3 <- factor(c("ITA", "FRE", "GER", "ENG", "WHOLE"))
param.langC <- factor(c("italian", "french", "german", "english", "WHOLE"))
param.lang = 1

#LDA parameters
#testing
# kt = 5
# nwords = 10
# LDAseed = 15000
#ita (15, 10, 15000)
kt = 4
nwords = 15
LDAseed = 10211

#Parameters for sentiment analysis
param2.lang = 1


#------------------------------------------------------------------- Preprocessing function: --------------------------------------------------------------------------------------------


preprocessor <- function(corp, toLowerCase, removeSW, removeCustomSW, removeP, lang, doStemming)
{
  #to lowercase
  if(toLowerCase)
  {
    corp = tm_map(corp, content_transformer(tolower))
  }
  
  #remove stopwords of all languages from the entirety of the corpus
  if(removeSW)
  {
    corp = tm_map(corp, removeWords, paste0("(?i)", stopwords("it")))
    corp = tm_map(corp, removeWords, paste0("(?i)", stopwords("fr")))
    corp = tm_map(corp, removeWords, paste0("(?i)", stopwords("de")))
    corp = tm_map(corp, removeWords, paste0("(?i)", stopwords("en")))
  }
  
  #custom stopwords
  if(removeCustomSW)
  {
    stopwords.custom = c('schmitz', "signor", 'signore', 'signora', 'mano', "mani", 'ettore', 'lettera', "lettere", 'parola', 'fare',
                         'cosa', "cose", "cosi", "così", "coso", "tanto", "tanti", "poi", "quando", "prima", "molto",
                         "caro", "cara", "carissimo", "carissima", "carissimi", "sua", "suo", "suoi")
    # stopwords.custom = c("tanto", "tanti", "poi", "quando", "prima", "molto", "ettore", "schmitz", "cosa", "cose", "cosi", "così", "coso", "caro", "cara",
    #                      "carissimo", "carissima", "carissimi", "sua", "suo", "suoi", "abbraccio", "essere", "oggi", "ancora", "qualche",
    #                      "sempre", "due", "ella", "devotissimo", "signor", "signore", "signora", "bene", "dopo", "fare", "tempo", "poco", "ora", "altro",
    #                      "moglie", "livia", "lettera", "lettere", "troppo", "ogni", "tutta", "meno", "senza", "invece", "pare", "ore",
    #                      "né", "ogni", "aver", "ciò", "già", "essa", "mano", "parola")
    corp = tm_map(corp, removeWords, paste0("(?i)", stopwords.custom))
  }
  
  #remove punctuation
  if(removeP)
  {
    removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
    corp = tm_map(corp, content_transformer(removeNumPunct))
  }
  
  #select the corpus based on the language and then stemming it
  if(param.lang3[lang] != "WHOLE")
  {
    corp = corp[indexes]
    
    #stemming the partial corpus
    if(doStemming)
    {
      corp = tm_map(corp, stemDocument, language = param.langC[lang])
    }
  }else
  {
    #Stemming the whole corpus
    if(doStemming)
    {
      corp = tm_map(corp, stemDocument, language = "italian")
      corp = tm_map(corp, stemDocument, language = "french")
      corp = tm_map(corp, stemDocument, language = "german")
      corp = tm_map(corp, stemDocument, language = "english")
    }
  }
  
  #return preprocessed corpus
  corp
}


#------------------------------------------------------------------ Topic modelling function: --------------------------------------------------------------------------------------------


doLDA <- function(x, kt, nw, LDAseed)
{
  #corpus to dtm conversion
  letters.DTM = DocumentTermMatrix(x)
  
  #compute LDA on the given corpus and parameters to obtain the LDA model
  lda.result = LDA(letters.DTM, kt, control = list(seed = LDAseed))
  
  #processing the LDA model for representation by truncating the nw most frequent terms associated to each topic
  lda_topics <- tidy(lda.result, matrix = "beta")
  
  lda_top_terms <- lda_topics %>%
    group_by(topic) %>%
    slice_max(n = nw, order_by = beta, with_ties = FALSE) %>%
    ungroup()
  
  #divide the terms by topics into a list
  list_by_topic = NULL
  list_by_topic = c(1:kt) %>% map(function(x) {
    append(list_by_topic, c(lda_top_terms[lda_top_terms$topic == x, ]))
  })
  
  #create a list of plot for later combining them
  plot_list = list()
  
  plot_list <- c(1:kt) %>% map(function(x) {
    pl <- as.data.frame(list_by_topic[x]) %>%
      group_by(topic) %>%
      mutate(term = reorder(term, beta)) %>%
      ungroup() %>%
      ggplot(aes(beta, term, colour = c("red", "yellow", "green", "blue")[x])) +
      geom_col(show.legend = FALSE, alpha=.7, width=.5) #+
      # scale_x_continuous(name = "", limits = c(0, 1.0)) +
      # scale_y_discrete(name = "") #+
      # facet_wrap(~ topic, scales = "free")
    
    pl
  })
  
  #combine plots
  figure = ggarrange(plotlist = plot_list, ncol = 4, nrow = 1#, labels = paste0("Topic ", c(1:kt)),
                     # font.label = list(size = 10, face = "plain", color = "black"), vjust = 0, hjust = 0
                     )
  
  #output
  list(figure, lda.result, letters.DTM)
}


#----------------------------------------------------------------------- Topic learning: --------------------------------------------------------------------------------------------


#define utility variable to store which indexes of the corpus we are working on
if(param.lang3[param.lang] != "WHOLE")
{
  indexes = grep(param.lang3[param.lang], letters.csv[, 11])
}else
{
  indexes = c(1:count(letters.csv)[[1]])
}

letters.corpus2 = preprocessor(corp = letters.corpus, toLowerCase = TRUE, removeSW = TRUE, removeCustomSW = TRUE,
                               removeP = TRUE, lang = param.lang, doStemming = FALSE)

#preprocess the corpus for topic discovery
letters.corpus = preprocessor(corp = letters.corpus, toLowerCase = TRUE, removeSW = TRUE, removeCustomSW = TRUE,
                              removeP = TRUE, lang = param.lang, doStemming = TRUE)

#obtaining the topic distributions
discoveredTopics = doLDA(letters.corpus, kt, nwords, LDAseed)

test = tidy(discoveredTopics[[2]], matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(n = nwords, order_by = beta, with_ties = FALSE) %>%
  ungroup()

test[, 2]= stemCompletion(test$term, letters.corpus2, type = "shortest")

test = test %>% mutate(term = factor(term), topic = factor(topic, levels = c("1", "2", "3", "4"), ordered = TRUE))

test %>%
  # group_by(topic) %>%
  # mutate(term = reorder(term, beta)) %>%
  # ungroup() %>%
  ggplot(aes(reorder_within(term, beta, topic), beta)) +
  geom_col(show.legend = FALSE, alpha=.6, width=.5, fill = factor(test$topic)) +
  scale_x_reordered(name = "Termini") +
# scale_x_continuous(name = "", limits = c(0, 1.0)) +
# scale_y_discrete(name = "") #+
  facet_wrap(facets = ~ topic, scales = "free_y", nrow = 1, labeller = as_labeller(
                                                              c(`1` = "Topic 1: Lavoro", `2` = "Topic 2: Quotidianità",
                                                                `3` = "Topic 3: Sentimenti", `4` = "Topic 4: Famiglia"))) +
  coord_flip() +
  scale_y_continuous(name = "Probabilità") +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9))

#obtaining the resulting plot of LDA
figure.LDAtopics = discoveredTopics[[1]]
figure.LDAtopics

#--------------------------------------------------------------- Discover year/topic correlation: --------------------------------------------------------------------------------------------


lda_documents <- tidy(discoveredTopics[[2]], matrix = "gamma")

#creating a joined table
letters.expanded.topic = letters.csv[indexes, ] 

#we need to rearrange the indexes of the dataframe if we aren't operating on the whole corpus!
letters.expanded.topic[ , 1] = c(1:length(indexes))
letters.expanded.topic = merge(letters.expanded.topic[, ], lda_documents, by.x = "n", by.y = "document")

#we discard the columns we don't need to plot
topic_by_documents = letters.expanded.topic %>%
  mutate(n = NULL, corpus = NULL, index = NULL, date = NULL, sender = NULL, senderLocation = NULL, 
         recipient = NULL, recipientLocation = NULL, languages = NULL, mainLanguage = NULL, text = NULL)

#we create a new dataframe that contains the grouped information by year and topic of the average gamma
topic_by_documents.mean = aggregate(gamma ~ year + topic, topic_by_documents, mean)

#draw plot
figure.yearTopic = topic_by_documents.mean %>%
  ggplot(aes(year, gamma)) +
  geom_line(show.legend = FALSE, colour = factor(topic_by_documents.mean$topic)) +
  geom_point(size = .8, show.legend = FALSE, colour = factor(topic_by_documents.mean$topic)) +
  geom_area(alpha = 0.1, show.legend = FALSE, fill = topic_by_documents.mean$topic) +
  scale_x_continuous(name = "Anno", breaks = c(1885, 1890, 1900, 1910, 1920, 1930)) +
  scale_y_continuous(name = "Probabilità media")+
  theme(axis.text.x = element_text(angle = 0, size = 8), axis.text.y = element_text(size = 8), panel.spacing.x = unit(1, "line")) +
  facet_wrap(~ topic, scales = "free_x", labeller = as_labeller(c(`1` = "Lavoro", `2` = "Quotidianità", `3` = "Sentimenti", `4` = "Famiglia")))

figure.yearTopic


#------------------------------------------------------------------ People/topic association: --------------------------------------------------------------------------------------------


#create an extended table which contains the pair sender/recipient with the gamma values associated to each topic
topic_per_people = letters.expanded.topic %>%
  mutate(recipient = paste(recipient,  ", ", sender), n = NULL, corpus = NULL, index = NULL, date = NULL, year =NULL, senderLocation = NULL,
         recipientLocation = NULL, languages = NULL, mainLanguage = NULL, text = NULL, sender = NULL)

#average the values of gamma for the same people on the same topic
topic_per_people = aggregate(gamma ~ recipient + topic, data = topic_per_people, FUN = mean)

#obtain a list of all the possible senders/recipients
people = unique(c(letters.expanded.topic[, 6], unlist(strsplit(unique(as.character(letters.expanded.topic[, 8])), ", "))))
#remove from the list "Ettore Schmitz" because he is associated to all the documents in the corpus
people = people[-grep("Ettore Schmitz", people)]

#create a dataframe to contain all the interesting data
topic_per_person = merge(people, c(1:kt), by.x = NULL, by.y = NULL) %>%
  mutate(gamma_mean = NA) %>%
  rename("name" = "x", "topic" = "y")

#fill the values for the gamma_mean column
for(i in 1:count(topic_per_person)[[1]])
{
  topic_per_person[i, 3] = colMeans(topic_per_people[grep(topic_per_person[i, 1], topic_per_people$recipient), ] %>%
                                    filter(topic == topic_per_person[i, 2]) %>%
                                    mutate(recipient = NULL, topic = NULL))
}

#plotting the results
plot_list = list()

plot_list <- c(1:kt) %>% map(function(x) {
  pl <- topic_per_person[topic_per_person$topic == x, ] %>%
    slice_max(n = 5, order_by = gamma_mean, with_ties = TRUE) %>%
    mutate(name = reorder(name, gamma_mean)) %>%
    ggplot(aes(gamma_mean, name)) +
    geom_col(show.legend = FALSE, alpha=.7, width=.5) +
    facet_wrap(~ topic, scales = "free_y")
  
  pl
})

figure.peopleTopic = ggarrange(plotlist = plot_list)
figure.peopleTopic

topPp = topic_per_person 

topPp = topPp %>% mutate(nletters = 0)
for(i in 1:(nrow(topPp))){
  topPp[i, 4] = length(grep(topPp[i, 1], letters.csv[, "sender"])) + length(grep(topPp[i, 1], letters.csv[, "recipient"]))
}

topPp[topPp$name == "Marieanne Crémieux Comnène", "name"] = "Marieanne Crémieux C."
topPp[topPp$name == "Letizia Veneziani Schmitz", "name"] = "Letizia Veneziani S."
topPp[topPp$name == "Casa editrice Fratelli Treves", "name"] = "Casa ed. F.lli Treves"

topPp =  topPp[topPp$nletters >= 2, ] %>%
  mutate(name = factor(name), topic = factor(topic, levels = c("1", "2", "3", "4"), ordered = TRUE)) %>%
  group_by(topic) %>%
  slice_max(n = 5, order_by = gamma_mean, with_ties = TRUE) %>%
  ungroup()

topPp  %>%
  ggplot(aes(reorder_within(name, gamma_mean, topic), gamma_mean)) +
  geom_col(show.legend = FALSE, alpha=.6, width=.5, fill = factor(topPp$topic)) +
  scale_x_reordered(name = "Nome") +
  facet_wrap(facets = ~ topic, scales = "free_y", nrow = 2, labeller = as_labeller(
    c(`1` = "Lavoro", `2` = "Quotidianità", `3` = "Sentimenti", `4` = "Famiglia"))) +
  coord_flip() +
  scale_y_continuous(name = "Probabilità media") +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9))




#-------------------------------------------------------------------- Sentiment analysis: --------------------------------------------------------------------------------------------


#reset the corpus for sentiment analysis
letters.corpus = Corpus(VectorSource(letters.csv[, 12]))

#preprocessing the corpus for sentiment analysis
letters.corpus = preprocessor(corp = letters.corpus, toLowerCase = TRUE, removeSW = TRUE, removeCustomSW = FALSE,
                                               removeP = TRUE, lang = param2.lang, doStemming = FALSE)

#putting back the corpus in the dataframe and expanding it with sentiment analysis variables setted to 0
letters.expanded.emot = letters.csv[indexes, ] %>%
  mutate(text = data.frame(sapply(letters.corpus, identity), stringsAsFactors = FALSE), n = NULL, corpus = NULL, index = NULL,
         date = NULL, mainLanguage = NULL, senderLocation = NULL, recipientLocation = NULL, anger = 0, anticipation = 0, disgust = 0,
         fear = 0, joy = 0, sadness = 0, surprise = 0, trust = 0, negative = 0, positive = 0)
  
#Do sentiment analysis for each language
for(i in 1:4)
{
  #for each language select the letters that contain that language
  lang.index = grep(param.lang3[i], letters.expanded.emot$languages)
  
  #store the partial sentiment analysis in a dataframe
  sentimentanalysis = get_nrc_sentiment(letters.expanded.emot$text[lang.index, ], language = param.langC[i])
  
  #add the values of the partial sentiment analysis to the according row of the data frame
  letters.expanded.emot[lang.index, ] = letters.expanded.emot[lang.index, ] %>%
    mutate(anger = anger + sentimentanalysis$anger, anticipation = anticipation + sentimentanalysis$anticipation,
           disgust = disgust + sentimentanalysis$disgust, fear = fear + sentimentanalysis$fear,
           joy = joy + sentimentanalysis$joy, sadness = sadness + sentimentanalysis$sadness,
           surprise = surprise + sentimentanalysis$surprise, trust = trust + sentimentanalysis$trust,
           negative = negative + sentimentanalysis$negative, positive = positive + sentimentanalysis$positive)
}

#normalize each emotional row, does it make sense? The letters have very different lengths, so it might be a good idea to cope with
#that. Still, by doing this we lose the information on how strong is the emotion on a particular letter.
#A maybe good approach would be to divide the number of words in the considered document

wordcount = stringi::stri_count(letters.expanded.emot[, 5][[1]], regex="\\S+")

letters.expanded.emot = letters.expanded.emot %>%
  mutate(anger = anger/wordcount, anticipation = anticipation/wordcount, disgust = disgust/wordcount, fear = fear/wordcount,
         joy = joy/wordcount, sadness = sadness/wordcount, surprise = surprise/wordcount, trust = trust/wordcount,
         negative = negative/wordcount, positive = positive/wordcount) %>%
  rename("rabbia" = "anger", "anticipazione" = "anticipation", "disgusto" = "disgust", "paura" = "fear",
         "gioia" = "joy", "tristezza" = "sadness", "sorpresa" = "surprise", "fiducia" = "trust",
         "negatività" = "negative", "positività" = "positive")

#plot the overall emotions of the corpus
overallEmo = colMeans(letters.expanded.emot[, 6:15])
overallEmo = data.frame(count = overallEmo, emotion = names(overallEmo)) 

ordercolor = as.data.frame(overallEmo[, 2]) %>% mutate(n = 0, color = "red") %>% rename("emotion" = "overallEmo[, 2]")

ordercolor[ordercolor$emotion == "gioia", 2] = 3
ordercolor[ordercolor$emotion == "fiducia", 2] = 5
ordercolor[ordercolor$emotion == "paura", 2] = 7
ordercolor[ordercolor$emotion == "sorpresa", 2] = 2
ordercolor[ordercolor$emotion == "tristezza", 2] = 6
ordercolor[ordercolor$emotion == "disgusto", 2] = 8
ordercolor[ordercolor$emotion == "rabbia", 2] = 1
ordercolor[ordercolor$emotion == "anticipazione", 2] = 4
ordercolor[ordercolor$emotion == "positività", 2] = 9
ordercolor[ordercolor$emotion == "negatività", 2] = 10

ordercolor[ordercolor$emotion == "gioia", 3] = "yellow"
ordercolor[ordercolor$emotion == "fiducia", 3] = "cyan"
ordercolor[ordercolor$emotion == "paura", 3] = "purple"
ordercolor[ordercolor$emotion == "sorpresa", 3] = "orange"
ordercolor[ordercolor$emotion == "tristezza", 3] = "blue"
ordercolor[ordercolor$emotion == "disgusto", 3] = "brown"
ordercolor[ordercolor$emotion == "rabbia", 3] = "red"
ordercolor[ordercolor$emotion == "anticipazione", 3] = "green"
ordercolor[ordercolor$emotion == "positività", 3] = "whitesmoke"
ordercolor[ordercolor$emotion == "negatività", 3] = "darkslategrey"

overallEmo = merge(overallEmo, ordercolor, by.x = "emotion", by.y = "emotion")
overallEmo[overallEmo$emotion == "anticipazione", "emotion"] = "anticip."

figure.overallEmo = overallEmo %>%
  mutate(emotion = reorder(emotion, n)) %>%
  ggplot(aes(emotion, count)) +
  geom_col(show.legend = FALSE, alpha=.2, width=.5, fill = factor(overallEmo$color), color = "darkslategrey") +
  scale_y_continuous(name = "Punteggio medio") +
  scale_x_discrete(name = "Emozione") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 9))

figure.overallEmo

#---------------------------------------------------------------- Emotions to topic and by year: --------------------------------------------------------------------------------------------


#Let's start with the emotions for each topic. To do that we want to perform the sentiment analysis on the word distribution of each topic.
#However, given the nature of the ncr emotional predictor, we can only predict from a text and not from just a collection of words.
#So, we'll take advantage of the already known topic probability gamma and the already computed emotions to calculate how each topic
#is associated to each emotion

#select the columns of the sentiment analysis and add an index for merging
letters.expanded = letters.expanded.emot[, 6:15] %>%
  mutate(n = 1:count(letters.expanded.emot)[[1]])

#merging the topic and emotion tables and computing the product between the gamma probability and the emotional indexes
letters.expanded = merge(letters.expanded.topic, letters.expanded, by.x = "n", by.y = "n") %>%
  mutate(rabbia = rabbia*gamma, anticipazione = anticipazione*gamma, disgusto = disgusto*gamma, paura = paura*gamma,
         gioia = gioia*gamma, tristezza = tristezza*gamma, sorpresa = sorpresa*gamma, fiducia = fiducia*gamma,
         negatività = negatività*gamma, positività = positività*gamma)

#compute the mean by topics
topic_emotions = aggregate(. ~ topic, data = letters.expanded[, c(13, 15:24)], mean) %>% rename("anticipaz." = "anticipazione")

ordercolor[ordercolor$emotion == "anticipazione", "emotion"] = "anticipaz."

topic_emotions = topic_emotions %>%
  pivot_longer(cols = c("rabbia", "anticipaz.", "disgusto", "paura", "gioia", "tristezza", "sorpresa", "fiducia", "negatività", "positività"),
               names_to = "emotion") %>%
  merge(ordercolor, by.x = "emotion", by.y = "emotion") %>%
  group_by(topic) %>%
  mutate(emotion = reorder(emotion, n)) %>%
  ungroup() %>%
  mutate(emotion = factor(emotion), color = factor(color, levels = c("blue", "brown", "cyan", "darkslategrey", "green",
                                                                     "orange", "purple", "red", "whitesmoke", "yellow"), ordered = TRUE))
ordercolor[ordercolor$emotion == "anticipaz.", "emotion"] = "anticipazione"

#draw the appropriate plot
figure.topicEmotion =  topic_emotions %>%
  group_by(topic) %>%
  mutate(emotion = reorder(emotion, n), color = reorder(color, n)) %>%
  ungroup() %>%
  ggplot(aes(value, emotion)) + 
  geom_col(show.legend = FALSE, alpha=0, width=.5, fill = "white", color = "darkslategrey") +
  facet_wrap(~ topic, scales = "fixed", labeller = as_labeller(
    c(`1` = "Lavoro", `2` = "Quotidianità", `3` = "Sentimenti", `4` = "Famiglia"))) +
  scale_y_discrete(name = "Emozione") +
  scale_x_continuous(name = "Punteggio medio") +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9))

figure.topicEmotion

#Now we want to get a representation on how the emotions change during the years. 
emotions_per_year = aggregate(. ~ year, data = letters.expanded.emot[, c(1, 6:15)], mean)%>%
  pivot_longer(cols = c("rabbia", "anticipazione", "disgusto", "paura", "gioia", "tristezza", "sorpresa", "fiducia", "negatività", "positività"),
               names_to = "emotion")

figure.yearEmotions = emotions_per_year %>%
  ggscatter(x = "year", y = "value", size = 1) +
  geom_line(aes(y = value)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.8, vjust = 0.8, size = 8)) + 
  facet_wrap(~ emotion, scales = "free_x")

figure.yearEmotions

emotions_per_year = emotions_per_year %>% merge(ordercolor, by.x = "emotion", by.y = "emotion") 
emotions_per_year[emotions_per_year$emotion == "positività", "color"] = "slategrey"
emotions_per_year[emotions_per_year$emotion == "negatività", "color"] = "black" 
  
emotions_per_year = emotions_per_year[((emotions_per_year$emotion == "positività") | (emotions_per_year$emotion == "negatività") |
                                        (emotions_per_year$emotion == "tristezza")), ]
emotions_per_year[emotions_per_year$emotion == "positività", "n"] = 1 

emotions_per_year %>% 
  arrange(n) %>%
  ggplot(aes(year, value)) +
  geom_line(show.legend = FALSE, colour = "darkslategrey") +
  geom_point(size = .8, show.legend = FALSE, colour = "darkslategrey") +
  geom_area(alpha = 0.1, show.legend = FALSE, fill = emotions_per_year$color) +
  scale_x_continuous(name = "Anno", breaks = c(1885, 1890, 1900, 1910, 1920, 1930)) +
  scale_y_continuous(name = "Punteggio medio")+
  theme(axis.text.x = element_text(angle = 0, size = 8), axis.text.y = element_text(size = 8), panel.spacing.x = unit(1, "line")) +
  facet_wrap(~ emotion, scales = "fixed", nrow = 5)


#---------------------------------------------------------------------- Emotions per person: --------------------------------------------------------------------------------------------


#create an extended table which contains the pair sender/recipient with the gamma values associated to each topic
emotions_per_people = letters.expanded.emot %>%
  mutate(recipient = paste(recipient,  ", ", sender), text = NULL, year = NULL, languages = NULL, sender = NULL)

#average the values of gamma for the same people on the same topic
emotions_per_people = aggregate(. ~ recipient, data = emotions_per_people, FUN = mean)

#create a dataframe to contain all the interesting data
emotions_per_person = as.data.frame(people) %>%
  mutate(rabbia_media = NA, anticipazione_media = NA, disgusto_media = NA, paura_media = NA,
         gioia_media = NA, tristezza_media = NA, sorpresa_media = NA, fiducia_media = NA,
         negatività_media = NA, positività_media = NA) %>%
  rename("name" = "people")

#fill the values for the emotion columns
for(i in 1:count(emotions_per_person)[[1]])
{
  emotions_per_person[i, 2:11] = colMeans(emotions_per_people[grep(emotions_per_person[i, 1], emotions_per_people$recipient), ] %>% 
                                            mutate(recipient = NULL))
}

emotions_per_person = emotions_per_person %>%
  pivot_longer(cols = c("rabbia_media", "anticipazione_media", "disgusto_media", "paura_media", "gioia_media", "tristezza_media",
                        "sorpresa_media", "fiducia_media", "negatività_media", "positività_media"), names_to = "emotion")

top_person_per_emotion = emotions_per_person %>%
  group_by(emotion) %>%
  slice_max(n = 3, order_by = value, with_ties = TRUE) %>%
  ungroup()

figure.emotionPeople = top_person_per_emotion %>%
  ggplot(aes(value, name)) +
  geom_col(show.legend = FALSE, alpha=.7, width=.5) +
  facet_wrap(~ emotion, scales = "free")

figure.emotionPeople

top_person_per_emotion  %>%
  ggplot(aes(reorder_within(name, value, emotion), value)) +
  geom_col(show.legend = FALSE, alpha=.6, width=.5) +
  scale_x_reordered(name = "Nome") +
  facet_wrap(facets = ~ emotion, scales = "free_y") +
  coord_flip() +
  scale_y_continuous(name = "Punteggio medio") +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9))

topPe = emotions_per_person 

topPe = topPe %>% mutate(nletters = 0)
for(i in 1:(nrow(topPe))){
  topPe[i, 4] = length(grep(topPe[i, 1], letters.csv[, "sender"])) + length(grep(topPe[i, 1], letters.csv[, "recipient"]))
}

topPe[topPe$name == "Marieanne Crémieux Comnène", "name"] = "Marieanne Crémieux C."
topPe[topPe$name == "Letizia Veneziani Schmitz", "name"] = "Letizia Veneziani S."
topPe[topPe$name == "Casa editrice Fratelli Treves", "name"] = "Casa ed. F.lli Treves"

topPe = topPe[topPe$nletters >= 2, ] %>%
  mutate(name = factor(name), emotion = factor(emotion, levels = c("rabbia_media", "anticipazione_media", "disgusto_media", "paura_media", "gioia_media", "tristezza_media",
                                                                   "sorpresa_media", "fiducia_media", "negatività_media", "positività_media"), ordered = TRUE)) %>%
  group_by(emotion) %>%
  slice_max(n = 5, order_by = value, with_ties = TRUE) %>%
  ungroup()

topPe = topPe[topPe$emotion == "positività_media" | topPe$emotion == "negatività_media", ]

topPe %>%
  ggplot(aes(reorder_within(name, value, emotion), value)) +
  geom_col(show.legend = FALSE, alpha=0, width=.5, colour = "darkslategrey") +
  scale_x_reordered(name = "Nome") +
  facet_wrap(facets = ~ emotion, scales = "free_y", nrow = 2) +
  coord_flip() +
  scale_y_continuous(name = "Punteggio medio") +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9))



#------------------------------------------------------------------------ Plot output: --------------------------------------------------------------------------------------------


show(figure.LDAtopics)
show(figure.yearTopic)
show(figure.peopleTopic)
show(figure.overallEmo)
show(figure.topicEmotion)
show(figure.yearEmotions)
show(figure.emotionPeople)

newdir.path = paste(c(as.character(kt), as.character(nwords), as.character(LDAseed), as.character(param.langC[param.lang])), collapse = " ")
i = 1

while(dir.exists(newdir.path))
{
  i = i + 1
  newdir.path = paste(c(as.character(kt), as.character(nwords), as.character(LDAseed), as.character(param.langC[param.lang]), "v", as.character(i)), collapse = " ")
}

dir.create(newdir.path)
setwd(newdir.path)
getwd()

save.image(file = "environment.RData")
ggsave(filename = "Topic overview.png", plot = figure.LDAtopics, device = "png", width = 325.12, height = 182.88, units = "mm")
ggsave(filename = "Topics in the years.png", plot = figure.yearTopic, device = "png", width = 325.12, height = 182.88, units = "mm")
ggsave(filename = "Topics with people.png", plot = figure.peopleTopic, device = "png", width = 325.12, height = 182.88, units = "mm")
ggsave(filename = "Emotion overview.png", plot = figure.overallEmo, device = "png", width = 325.12, height = 182.88, units = "mm")
ggsave(filename = "Topic emotions.png", plot = figure.topicEmotion, device = "png", width = 325.12, height = 182.88, units = "mm")
ggsave(filename = "Emotions in the years.png", plot = figure.yearEmotions, device = "png", width = 325.12, height = 182.88, units = "mm")
ggsave(filename = "Emotions towards people.png", plot = figure.emotionPeople, device = "png", width = 325.12, height = 182.88, units = "mm")

setwd("D:/università/progetti/Machine Learning and Evolutionary Robotics")

