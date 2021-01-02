library(rlist)
library(pipeR)
library(dplyr)
library(tidytext)
library(dplyr)
library(stringr)
library(textdata)
library(wordcloud)
library(tibble)
library(datetime)
library(lubridate)
library(zoo)
#Reading GitHub Comments
repos <- "https://api.github.com/repos/torvalds/linux/comments?per_page=100&page=%d" %>>%
  sprintf(1:20) %>>%
  list.load("json") %>>%
  list.ungroup

my_columns = c("body", "created_at")
#Cleaning out unwanted data
name_Of_repos <- vapply(repos, "[[", "", "body")
date_of_comments <- vapply(repos, "[[", "", "created_at")
name_Of_repos

#Converting to Data Frame
dates_df <- tibble(line= 1:1172, date = date_of_comments)

text_df <- tibble(line = 1:1172, text = name_Of_repos)

new_df <- merge(dates_df,text_df)


text_df

count(text_df)

single_word <- text_df %>%
  unnest_tokens(word, text)
data(stop_words)

single_word <- single_word %>%
  anti_join(stop_words)

single_word %>%
  count(word, sort = TRUE)

text_df 


library(ggplot2)

single_word %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

# Using NRC lexicon
get_sentiments("nrc")


comments_positive <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")
  

emotion_in_commits <- single_word %>% 
  inner_join(comments_positive) %>%
  count(word, sort=TRUE)

# Using Bing and graph sentiments
library(tidyr)

comment_sentiments <- single_word %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = line %/% 10, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)

ggplot(comment_sentiments, aes(index, sentiment)) +
  geom_col(show.legend = FALSE)


# Attempting to graph number of commits for the timeframe

new_df <- merge(dates_df,text_df)

single_word_with_date <- new_df %>%
  unnest_tokens(word, text)

data(stop_words)

single_word_with_date <- single_word_with_date %>%
  anti_join(stop_words)

single_word_with_date %>%
  count(word, sort = TRUE)

library(datetime)

single_word_with_date$date <- substr(single_word_with_date$date,1,nchar(single_word_with_date$date)-10)

single_word_with_date$date <- as.Date(single_word_with_date$date , format = "%Y-%m-%d")

single_word_with_date$date <- as.yearqtr(single_word_with_date$date, format = "%Y-%m-%d")

comment_sentiments_with_date <- single_word_with_date %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = date %/% yearqtr(1) , sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

head(single_word_with_date)

ggplot(comment_sentiments_with_date, aes(index, sentiment)) +
  geom_col(show.legend = FALSE)

# Attempting different sentiment analysis
  






repos %>>%
  list.table(fork)

repos %>>% 
  list.filter(!is.null(language)) %>>%
  list.table(language) %>>%
  list.sort(-.)

repos %>>%
  list.table(language, fork)

repos %>>%
  list.filter(!fork, language == "Java") %>>%
  list.names(name) %>>%
  list.mapv(stargazers_count) %>>%
  list.sort(-.) %>>%
  list.take(10) %>>%
  print %>>%
  barplot(main = "Hadley's top 10 R repos with most stargazers")

repos %>>%
  list.filter(has_issues, !fork, language == "R") %>>%
  list.names(name) %>>%
  list.mapv(open_issues) %>>%
  list.sort(-.) %>>%
  list.take(10) %>>%
  print %>>%
  barplot(main = "Hadley's top 10 R repos with most open issues")