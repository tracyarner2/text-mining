
library(forcats)
library(dplyr)
library(readr)
library(tidyr)
library(rtweet)
library(writexl)
library(readxl)
library(tidytext)
library(textdata)
library(ggplot2)
library(textdata)
library(scales)


#read in data files
ngss_tweets <- read_xlsx("lab-2/data/ngss_tweets.xlsx")
ccss_tweets <- read_xlsx("lab-2/data/csss_tweets.xlsx")

#filtering for English only Tweets
ngss_text <- filter(ngss_tweets, lang == "en")
ccss_text <- filter(ccss_tweets, lang == "en")

#add column to identify standard category
ngss_text <- mutate(ngss_text, standards = "NGSS")
ccss_text <- mutate(ccss_text, standards = "CCSS")

#reorder the columns
ngss_text <- relocate(ngss_text, standards)
ccss_text <- relocate(ccss_text, standards)

#select desired variables
ngss_text <- select(ngss_text, standards, screen_name, created_at, text)
ccss_text <- select(ccss_text, standards, screen_name, created_at, text)

#combine datasets
tweets <- bind_rows(ngss_text, ccss_text)

#tokenize data
tweet_tokens <-
  tweets %>%
  unnest_tokens(output = word,
                input = text,
                token = "tweets")

#remove stop words and remove amp
tidy_tweets <-
  tweet_tokens %>%
  anti_join(stop_words, by = "word")%>%
  filter(!word == 'amp')

#count the most common words
count(tidy_tweets, word, sort = T)

#get sentiments from AFINN, nrc, bing, loughran
afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")
loughran <- get_sentiments("loughran")

#join sentiments by word
sentiment_afinn <- inner_join(tidy_tweets, afinn, by = "word")
sentiment_bing <- inner_join(tidy_tweets, bing, by = "word")
sentiment_nrc <- inner_join(tidy_tweets, nrc, by = "word")
sentiment_loughran <- inner_join(tidy_tweets, loughran, by = "word")

#view tweets by standard X time
tweets %>%
  group_by(standards) %>%
  ts_plot(by = "days")

#produce summaries
summary_bing <- count(sentiment_bing, sentiment, sort = TRUE)
summary_nrc <- count(sentiment_nrc, sentiment, sort = TRUE)
summary_afinn <- count(sentiment_afinn, sentiment, sort = TRUE)
summary_loughran <- count(sentiment_loughran, sentiment, sort = TRUE)


#revise dataframes
summary_afinn2 <- sentiment_afinn %>%
  group_by(standards) %>%
  filter(value != 0) %>%
  mutate(sentiment = if_else(value < 0, "negative", "positive")) %>%
  count(sentiment, sort = TRUE) %>%
  mutate(method = "afinn")

summary_bing2 <- sentiment_bing %>%
  group_by(standards) %>%
  count(sentiment, sort = TRUE) %>%
  mutate(method = "bing")

summary_nrc2 <- sentiment_nrc %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  group_by(standards) %>%
  count(sentiment, sort = TRUE) %>%
  mutate(method = "nrc")

summary_loughran2 <- sentiment_loughran %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  group_by(standards) %>%
  count(sentiment, sort = TRUE) %>%
  mutate(method = "loughran")


#view summaries
summary_bing2
summary_nrc2
summary_afinn2
summary_loughran2

#join summaries

summary_sentiment <- bind_rows(summary_afinn2,
                              summary_bing2,
                              summary_nrc2,
                              summary_loughran2) %>%
  arrange(method, standards) %>%
  relocate(method)
summary_sentiment
#create totals and then join summary and total counts to prepare percentages
total_counts <- summary_sentiment %>%
  group_by(method, standards) %>%
  summarise(total = sum(n))

sentiment_counts <- left_join(summary_sentiment, total_counts)

sentiment_percents <- sentiment_counts %>%
  mutate(percent = n/total * 100)

sentiment_percents

#visualization bar chart
sentiment_percents %>%
  ggplot(aes(x = standards, y = percent, fill=sentiment)) +
  geom_bar(width = .8, stat = "identity") +
  facet_wrap(~method, ncol = 1) +
  coord_flip() +
  labs(title = "Public Sentiment on Twitter",
       subtitle = "The Common Core & Next Gen Science Standards",
       x = "State Standards",
       y = "Percentage of Words")

#Small multiples visualization with green stacked
sentiment_percents %>%
ggplot(aes(standards, percent, fill = percent)) +
  theme_minimal() +
  scale_fill_distiller(palette = "Greens") +
  labs(title="Comparison of positive sentiment in NGSS and CCSS Tweets") +
geom_col(show.legend = FALSE) +
facet_wrap(~method, ncol = 2, scales = "free")

#Small multiples visualization with green not stacked


ggplot(sentiment_percents, aes(standards, percent, fill=sentiment)) +
  geom_bar(stat='identity', position = "dodge") +
  theme(legend.position="none", plot.title=element_text(hjust=0.5)) +
  labs(title="Comparison of positive sentiment in NGSS and CCSS Tweets",
       x="Standards",
       y="Percent") +
  facet_wrap(~method, ncol = 2, scales = "free")


