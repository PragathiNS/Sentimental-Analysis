import re
import tweepy
from tweepy import OAuthHandler
from textblob import TextBlob

class TwitterClient(object):
    def __init__(self):
        consumer_key = "ckckckckckckckckckckck"
        consumer_secret = "ckscksckscksckscksckscks"
        access_token = "111111111-atatatatatatatatatatattatatatat"
        access_token_secret = "atsatsatsatsatsatsatsatsatsatsatsatsats"

        # Authentication
        try:
            self.auth = OAuthHandler(consumer_key, consumer_secret)
            self.auth.set_access_token(access_token, access_token_secret)
            self.api = tweepy.API(self.auth)
        except:
            print ("Error: Authentication failed")

    def process_tweet(self, tweet):
        return ' '.join(re.sub("(@[A-Za-z0-9]+)|([^0-9A-Za-z \t]) | (\w +:\ / \ / \S +)", " ", tweet).split())

    def tweet_sentiment(self, tweet):
        analysis = TextBlob(self.process_tweet(tweet))
        if analysis.sentiment.polarity > 0:
            return 'positive'
        elif analysis.sentiment.polarity == 0:
            return 'neutral'
        else:
            return 'negative'

    def get_tweets(self, query, count = 15):
        tweets = []

        try:
            fetched_tweets = self.api.search(q = query, count = count)

            for tweet in fetched_tweets:
                parsed_tweet= {}

                parsed_tweet['text'] = tweet.text
                parsed_tweet['sentiment'] = self.tweet_sentiment(tweet.text)

                if tweet.retweet_count > 0:
                    if parsed_tweet not in tweets:
                        tweets.append(parsed_tweet)
                else:
                    tweets.append(parsed_tweet)

            return tweets
        except tweepy.TweepError as e:
            print ("Error: " + str(e))

def main():
    api = TwitterClient()

    tweets = api.get_tweets(query = 'American Airlines', count = 200)
    pos_tweets = [tweet for tweet in tweets if tweet['sentiment'] == 'positive']
    print("Positive tweets percentage: {} %".format(100 * len(pos_tweets) / len(tweets)))
    neg_tweets = [tweet for tweet in tweets if tweet['sentiment'] == 'negative']
    print("Negative tweets percentage: {} %".format(100 * len(neg_tweets) / len(tweets)))

if __name__ == "__main__":
    main()
