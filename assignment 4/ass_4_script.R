

library(rtweet)

# create token
create_token(
      app = "nick_rtweet_app",
      consumer_key = "luQHNvoCA9YC0028BczJuq3Ef",
      consumer_secret = "iQTdYnn96OGTTsVZb9KQ1XaidH2odex8vwGXWycyUTjNEVzzjM", 
      access_token = "1011282348028186628-kYF7MgFvHJWVW2phJVW0xH4qIH4983",
      access_secret = "7ML5vff2UpidgTXoqZMWibtITQt2hhtr39yEpTd5FekrW")


# search hashtage
rt <- search_tweets("#MondayMotivation", n = 2000, include_rts = FALSE)



rt <- data.frame(rt)
head(rt)



# streaming
st <- stream_tweets(
      "#MondayMotivation",
      timeout = 60 * 60 * 3,
      parse = FALSE
)

st <- data.frame(st)