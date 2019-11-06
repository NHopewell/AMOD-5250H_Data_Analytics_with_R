

library(rtweet)

# create token
create_token(
      app = "nick_rtweet_app",
      consumer_key = "luQHNvoCA9YC0028BczJuq3Ef",
      consumer_secret = "iQTdYnn96OGTTsVZb9KQ1XaidH2odex8vwGXWycyUTjNEVzzjM", 
      access_token = "1011282348028186628-kYF7MgFvHJWVW2phJVW0xH4qIH4983",
      access_secret = "7ML5vff2UpidgTXoqZMWibtITQt2hhtr39yEpTd5FekrW")


# search hashtag
rt <- search_tweets("#MondayMotivation", n = 2000, include_rts = FALSE)
rt <- rt %>%
      mutate(method = "API")



# streaming
st <- stream_tweets(
      "#MondayMotivation",
      timeout = 60 * 60 * 3,
      file_name = "motivationtweets.json", 
      parse = FALSE
)

st <- parse_stream("motivationtweets.json")

st <- st %>%
      mutate(method = "stream")


# comabine into one
all_tweets <- inner_join(rt, st, by = "user_id")



library(tidytext)
library(tidyverse)
library(stringr)
library(wordcloud)

# relevant text analaysis (this should include cleaning, tokenizing, frequencies, and a couple of interesting graphs)
# sorry but I am exhausted and I am just going to do what you did in class. I also have 9 script files for when I did this for Sri
# so I feel confident I can do it for the most part. 


token.pattern <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
clean.pattern = "(http[^ ]*)|(www\\.[^ ]*)|&amp;|&lt;|&gt;|RT|[[:cntrl:]]|\\'|\\!|\\,|\\?|\\.|\\:"
hashtag.pattern <- "#[[:alnum:]]+"
handle.pattern <- "@[[:alnum:]]+"
handh.pattern <- "(@[[:alnum:]]+)|(#[[:alnum:]]+)"




# jsut test and retweens
tweets <- all_tweets %>% 
      select(text.x, retweet_count.x, text.y, retweet_count.y)

# clean tweera
clean.tweets <- tweets %>% 
      mutate(text.x = iconv(text.x, "latin1", "ASCII", ""),
             text.y = iconv(text.y, "latin1", "ASCII", "")) %>%
      mutate(text.x = str_replace_all(text.x, clean.pattern, ""),
             text.x = str_replace_all(text.x, handle.pattern, ""),
             text.x = str_replace_all(text.x, hashtag.pattern, ""),
             text.y = str_replace_all(text.y, clean.pattern, ""),
             text.y = str_replace_all(text.y, handle.pattern, ""),
             text.y = str_replace_all(text.y, hashtag.pattern, "")) %>%
      mutate(text.x = tolower(text.x),
             text.y = tolower(text.y))



# unnest tokens and filter out stop words
all_tweets <- clean.tweets %>% 
      unnest_tokens(word.x, text.x, token = "regex", pattern = token.pattern) %>%
      unnest_tokens(word.y, text.y, token = "regex", pattern = token.pattern) %>%
      filter(!word.x %in% stop_words$word,
             str_detect(word.x, "[a-z]")) %>%
      filter(!word.y %in% stop_words$word,
             str_detect(word.y, "[a-z]"))


# frequencies
word_freq.x <- all_tweets %>% 
      count(word.x, sort = TRUE) 

word_freq.y <- all_tweets %>%
      count(word.y, sort = TRUE)



# top 15 words for each

head(word_freq.x, 15)
head(word_freq.y, 15)


colors <- c("grey80", "darkgoldenrod1", "tomato")
colors2 <- colorRampPalette(brewer.pal(9,"Blues"))(32)[seq(8,32,6)]

# word cloud for searched tweets 
wordcloud(word_freq.x$word.x, 
          word_freq.x$n, 
          scale = c(3,.5), 
          max.words = 100, 
          random.order = FALSE, 
          random.color = FALSE, 
          colors = colors)

# word cloud for streamed tweets
wordcloud(word_freq.y$word.y, 
          word_freq.y$n, 
          scale = c(3,.5), 
          max.words = 100, 
          random.order = FALSE, 
          random.color = FALSE, 
          colors = colors2)



# frequncies for historic data
ggplot(word_freq.x[1:10, ], aes(x = reorder(word.x, -n), y = n)) +
      geom_bar(stat = "identity", fill = "dodgerblue", colour = "darkgrey") +
      labs(title = "#MotivationMonday: Top ten most frequent words (historic data)",
           x = "",
           y = "count\n") +
      theme_minimal()


# frequencies for streamed data
ggplot(word_freq.y[1:10, ], aes(x = reorder(word.y, -n), y = n)) +
      geom_bar(stat = "identity", fill = "tomato", colour = "darkgrey") +
      labs(title = "#MotivationMonday: Top ten most frequent words (streamed data)",
           x = "",
           y = "count\n") +
      theme_minimal()


