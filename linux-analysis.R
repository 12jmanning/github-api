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
library(tidyverse)
library(forecast)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)

#Reading GitHub Comments
repos <- "https://api.github.com/repos/torvalds/linux/comments?per_page=100&page=%d" %>>%
  sprintf(1:50) %>>%
  list.load("json") %>>%
  list.ungroup

my_columns = c("body", "created_at")

#Cleaning out unwanted data
name_Of_repos <- vapply(repos, "[[", "", "body")
date_of_comments <- vapply(repos, "[[", "", "created_at")
name_Of_repos

#Converting to Data Frame
dates_df <- tibble(line= 1:1175, date = date_of_comments)

text_df <- tibble(line = 1:1175, text = name_Of_repos)

new_df <- merge(dates_df,text_df)


count(text_df)

single_word <- text_df %>%
  unnest_tokens(word, text)
data(stop_words)

single_word <- single_word %>%
  anti_join(stop_words)

single_word %>%
  count(word, sort = TRUE)

wordcloud_df <- single_word %>%
  count(word, sort = TRUE)

wordcloud(words = wordcloud_df$word, freq = wordcloud_df$n, min.freq = 1,  max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

library(ggplot2)

single_word %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() + 
  labs(y = NULL) + ylab(label ="Words in Comments") + xlab(label ="Frequency") + ggtitle(label = "Most common words in commit comments (data is cleaned)")



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
  geom_col(show.legend = FALSE) + ylab(label = "Sentiment Score Agregatted by 10 Comments") + xlab(label = "Batched Comments by 10") + ggtitle(label = "Sentiment Analysis using Bing Lexicon on commit comments on Linux Repo ")

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
  count(index = date , sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

head(single_word_with_date)

ggplot(comment_sentiments_with_date, aes(index, sentiment)) +
  geom_col(show.legend = FALSE) + ylab(label = "Sentiment Score Agregatted by Quarter") + xlab(label = "Batched Comments by Quarter") + ggtitle(label = "Sentiment Analysis using Bing Lexicon on commit comments on Linux Repo ")

# Weekly commit count for past 52 weeks

weekly_commit_count <- "https://api.github.com/repos/torvalds/linux/stats/participation" %>>%
  list.load("json") %>>%
  list.ungroup

total_weekly_commit_count <- weekly_commit_count[1:52]
plot(total_weekly_commit_count, type="l", main = "Total No. Of Commits in Linux in Past year", xlab = "Week Number from today back a year", ylab="No. of Commits")

torvalds_weekly_commit_count <- weekly_commit_count[53:104]
plot(torvalds_weekly_commit_count, type="l", main = "Total No. Of Commits in Linux by Torvalds in Past year", xlab = "Week Number from today back a year", ylab="No. of Commits")


# Breaking the sentiment analysis down by week

single_word_by_week <- new_df %>%
  unnest_tokens(word, text)

data(stop_words)

single_word_by_week <- single_word_by_week %>%
  anti_join(stop_words)

single_word_by_week %>%
  count(word, sort = TRUE)

library(datetime)

single_word_by_week$date <- substr(single_word_by_week$date,1,nchar(single_word_by_week$date)-10)


single_word_by_week$date <- as.Date(single_word_by_week$date , format = "%Y-%m-%d")


single_word_by_week <- single_word_by_week %>% filter(single_word_by_week$date >= (Sys.Date()-years(1)))

comment_sentiments_with_date_by_week <- single_word_by_week %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = date , sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

head(single_word_by_week)

ggplot(comment_sentiments_with_date_by_week, aes(index, sentiment)) +
  geom_col(show.legend = FALSE) + ggtitle(label ="Analysis of comments in past year (no comments since march)") + xlab(label ="2020")



