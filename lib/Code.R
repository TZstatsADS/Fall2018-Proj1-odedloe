# Loading all the necessary packages
library(tm)
library(tidytext)
library(tidyverse)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library("maps")
library("maptools")
library("ggmap")
library(plotrix)
library(scales)
library(wordcloud2)
library(gridExtra)
library(ngram)
 

########################
# constructing hm_data #
########################

urlfile<-'https://raw.githubusercontent.com/rit-public/HappyDB/master/happydb/data/cleaned_hm.csv'
hm_data <- read_csv(urlfile)
corpus <- VCorpus(VectorSource(hm_data$cleaned_hm))%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)%>%
  tm_map(removeWords, character(0))%>%
  tm_map(stripWhitespace)

stemmed <- tm_map(corpus, stemDocument) %>%
  tidy() %>%
  select(text)

dict <- tidy(corpus) %>%
  select(text) %>%
  unnest_tokens(dictionary, text)

data("stop_words")

word <- c("happy","ago","yesterday","lot","today","months","month",
          "happier","happiest","last","week","past")

stop_words <- stop_words %>%
  bind_rows(mutate(tibble(word), lexicon = "updated"))

completed <- stemmed %>%
  mutate(id = row_number()) %>%
  unnest_tokens(stems, text) %>%
  bind_cols(dict) %>%
  anti_join(stop_words, by = c("dictionary" = "word"))

completed <- completed %>%
  group_by(stems) %>%
  count(dictionary) %>%
  mutate(word = dictionary[which.max(n)]) %>%
  ungroup() %>%
  select(stems, word) %>%
  distinct() %>%
  right_join(completed) %>%
  select(-stems)

completed <- completed %>%
  group_by(id) %>%
  summarise(text = str_c(word, collapse = " ")) %>%
  ungroup()

hm_data <- hm_data %>%
  mutate(id = row_number()) %>%
  inner_join(completed)

write_csv(hm_data, "processed_moments.csv")

urlfile<-'https://raw.githubusercontent.com/rit-public/HappyDB/master/happydb/data/demographic.csv'
demo_data <- read.csv(urlfile)

hm_data <- hm_data %>%
  inner_join(demo_data, by = "wid") %>%
  select(wid,
         original_hm,
         gender, 
         marital, 
         parenthood,
         reflection_period,
         age, 
         country, 
         ground_truth_category, 
         text) %>%
  mutate(count = sapply(hm_data$text, wordcount)) %>%
  filter(gender %in% c("m", "f")) %>%
  filter(marital %in% c("single", "married")) %>%
  filter(parenthood %in% c("n", "y")) %>%
  filter(reflection_period %in% c("24h", "3m")) %>%
  mutate(reflection_period = fct_recode(reflection_period, 
                                        months_3 = "3m", hours_24 = "24h"))

#################################
# constructing the tf-idf scores#
#################################

hm_words <- hm_data %>%
  unnest_tokens(word, text) %>%
  count(wid,word,gender,country,sort = TRUE) %>%
  ungroup()

total_words <- hm_words %>% 
  group_by(wid) %>% 
  summarize(total = sum(n))

hm_words <- left_join(hm_words, total_words)

hm_words <- hm_words %>%
  bind_tf_idf(word, wid, n)

hm_words <- hm_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

hm_words.imp <- hm_words[1:length(unique(hm_data$wid)),]

country.sub <- matrix(NA,nrow = length(unique(hm_words.imp$country)), ncol = 10)
for(i in 1:dim(country.sub)[1]){
  country.sub[i,] <- names(sort(table(hm_words.imp$word[hm_words.imp$country==unique(hm_words.imp$country)[2]]), decreasing = T)[1:10])
}

###############################################
# constructing the bargraphs of USA and India #
###############################################

ind <- sort(table(hm_words.imp$word[hm_words.imp$country==unique(hm_words.imp$country)[1]]), decreasing = T)[1:10]
usa <- sort(table(hm_words.imp$word[hm_words.imp$country==unique(hm_words.imp$country)[2]]), decreasing = T)[1:10]

plot_ly(
  x = as.numeric(usa),
  y = names(usa), 
  name = "USA most common happy moments",
  type = "bar"
)

plot_ly(
  x = as.numeric(ind),
  y = names(ind), 
  main = "India most common happy moments",
  type = "bar"
)

##############################################
# constructing the map of 5 chosen countries #
##############################################

ind.1 <- names(sort(table(hm_words.imp$word[hm_words.imp$country==unique(hm_words.imp$country)[1]]), decreasing = T)[1])
usa.1 <- names(sort(table(hm_words.imp$word[hm_words.imp$country==unique(hm_words.imp$country)[2]]), decreasing = T)[1])
uga.1 <- names(sort(table(hm_words.imp$word[hm_words.imp$country==unique(hm_words.imp$country)[8]]), decreasing = T)[1])
aus.1 <- names(sort(table(hm_words.imp$word[hm_words.imp$country==unique(hm_words.imp$country)[2]]), decreasing = T)[1])
bra.1 <- names(sort(table(hm_words.imp$word[hm_words.imp$country==unique(hm_words.imp$country)[43]]), decreasing = T)[1])

par(mfrow = c(1,1))
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
legend(-150,0,legend = c("India","USA","Uganda","Australia","Brazil"), fill = c(1,4,2,5,3), cex = 0.75)
y <- c(25,41.2565, 1.3733, -20, -10)
x <- c(80,-95.9345, 32.2903, 133.7751, -50)
text(x,y, c(ind.1, usa.1, uga.1, aus.1, bra.1), col = c(1,4,2,5,3), cex = 0.5)

usa.data <- hm_words.imp[hm_words.imp$country=="USA",]
female <- sort(table(usa.data$word[usa.data$gender=="f"]), decreasing = T)[1:10]
male <- sort(table(usa.data$word[usa.data$gender=="m"]), decreasing = T)[1:10]

##########################################################
# constructing the pie charts of women and men in the US #
##########################################################

par(mfrow = c(1,2), mar = c(0,0,0,0))
labels.female <- names(female)
labels.female <- paste(labels.female,",", sep = "")
pct.female <- round(100*as.vector(female)/sum(as.vector(female)))
labels.female <- paste(labels.female, pct.female)
labels.female <- paste(labels.female, "%", sep = "")
pie3D(female, labels = labels.female, labelcex = 0.8, main = "Female")

labels.male <- names(male)
labels.male <- paste(labels.male,",", sep = "")
pct.male <- round(100*as.vector(male)/sum(as.vector(male)))
labels.male <- paste(labels.male, pct.male)
labels.male <- paste(labels.male, "%", sep = "")
pie3D(male, labels = labels.male, labelcex = 0.8, main = "Male")
