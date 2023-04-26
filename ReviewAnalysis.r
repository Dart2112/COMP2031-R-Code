install.packages("syuzhet")
install.packages("reshape2")

library(mongolite)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(stringi)
#Setup the URL to be used for connecting to mongo DB
#This database is IP address locked and will not work everywhere even with the password
connection_string = 'mongodb+srv://benjamin:Cn5NOjrAtA5goreY@cluster1.ab84x48.mongodb.net/?retryWrites=true&w=majority'

#DB Password
#Cn5NOjrAtA5goreY

#Get the listingsAndReviews data set from Mongo
airbnbcollection = mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)
#airbnbcollection$count()

#Get an iterator for the collection of listings
iterator = airbnbcollection$iterate()

#Run this code ten times meaning we get 10 listings in total from the collection to analyse
for(y in 0:10){
  #Get the next listing from the iterator
  entry = iterator$one();
  #Get the reviews from that listing
  review_list = entry[["reviews"]]
  #Make sure the listing has more than 10 reviews so that we have enough data to analyse
  #This loop will continue to grab the next item from the iterator until we have a list of reviews with greater than 10 entries
  while(length(review_list) < 10){
    #If it has less then 10 reviews we grab another listing from the collection and try again
    entry = iterator$one();
    #Make sure we update the review list value so that the while loop can check it
    review_list = entry[["reviews"]]
  }
  #Fetch the total rating from the reviews
  total_rating = entry[["review_scores"]][["review_scores_rating"]]
  #Make a new vector to store the contents of the reviews from this listing
  reviews = vector()
  #Loop over the entire list of reviews and add them to the vector
  for (x in 1:length(review_list)){
    reviews[x] = review_list[[x]][["comments"]]
  }
  
  #Setup sentiment analysis to process the Vector
  s = get_nrc_sentiment(reviews)
  sentiment = vector()
  #Store the sentiment scores in the sentiment vector
  sentiment = c(sentiment, 0:length(review_list))
  
  #loop over the reviews
  for (x in 0:length(review_list)){
    #For each review we will grab the positive and negative sentiment
    positive = s$positive[x]
    negative = s$negative[x]
    string = review_list[x]
    #Apply a ratio to the sentiment based on the length of the review, essentially calculating the ratio of positive and negative sentiments per word
    positive_ratio = (positive / stri_count_words(string)) * 100
    negative_ratio = (negative / stri_count_words(string)) * 100
    #Store the overall score as positive subtract negative, meaning that a more positive review will create a more positive score and vise versa for negative reviews
    sentiment[x] = positive_ratio - negative_ratio
  }
  #Generate an overall score for each listing, again applying a ratio of sentiment per review. Meaning a listing with many reviews doesn't get a higher score than one with few reviews
  sentiment_total = sum(sentiment) / length(review_list) * 10
  
  #Print out the results
  print("ListingURL:")
  print(entry[["listing_url"]])
  print("Dataset Total = ")
  print(total_rating)
  print(" Sentiment Total = ")
  print(sentiment_total)
  print("==================")
}
