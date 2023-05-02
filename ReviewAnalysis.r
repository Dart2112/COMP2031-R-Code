install.packages("syuzhet")
install.packages("reshape2")

#Language detection
install.packages("cld2")

library(mongolite)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(stringi)
library(cld2)
#Setup the URL to be used for connecting to Mongo DB
#This database is IP address locked and will not work everywhere even with the password
connection_string = 'mongodb+srv://benjamin:Cn5NOjrAtA5goreY@cluster1.ab84x48.mongodb.net/?retryWrites=true&w=majority'

#DB Password
#Cn5NOjrAtA5goreY

#Get the listingsAndReviews data set from Mongo
airbnbcollection = mongo(collection = "listingsAndReviews",
                         db = "sample_airbnb",
                         url = connection_string)
#airbnbcollection$count()

#Get an iterator for the collection of listings
iterator = airbnbcollection$iterate()

givenScores = vector()
calculatedScores = vector()

retrivedReviews = vector()

enoughReviews = 0

#We get all listings from the collection to analyse
for (y in 1:airbnbcollection$count()) {
#Only grab 800 Listings because that's how many we can get before the cursor times out
#for (y in 1:1000) {
  #Get the next listing from the iterator
  entry = iterator$one()
  
  #Get the reviews from that listing
  review_list = entry[["reviews"]]
  #Make sure the listing has more than 10 reviews so that we have enough data to analyse
  #This loop will continue to grab the next item from the iterator until we have a list of reviews with greater than 10 entries
  if (length(review_list) < 25) {
    #If it has less then 10 reviews we grab another listing from the collection and try again
    print(paste(y, " Not enough reviews"))
    next
  }
  #Logging that we made it to this point in the code so that we can track this
  enoughReviews = enoughReviews + 1
  #Fetch the total rating from the reviews
  total_rating = entry[["review_scores"]][["review_scores_rating"]]
  #Make a new vector to store the contents of the reviews from this listing
  reviews = vector()
  #Initialize a value to store the current index to insert values at, this is used since we are not including all reviews
  j = 1
  #Loop over the entire list of reviews and add them to the vector if they are in English
  for (x in 1:length(review_list)) {
    #Get the contents of the review, this is the written component of the review
    review = review_list[[x]][["comments"]]
    #Check that the review isn't null before attempting to process it
    if (is.null(review)) {
      print(paste(y, x, " Review is null"))
      next
    }
    #Detect the language of the comment
    #Our sentiment analysis only works on English so we need to make sure the text is in English
    language = detect_language(review, plain_text = TRUE, lang_code = TRUE)
    #If the language is not detected, then we skip this review
    if (is.na(language)) {
      print(paste(y, x, " Cannot detect language"))
      next
    }
    #If the language is not English, then we skip this review
    if (language != "en") {
      print(paste(y, x, " Not English"))
      next
    }
    #Add it to the next slot in the reviews vector
    reviews[j] = review
    #Increment the j value since we have added a value
    j = j + 1
  }
  #Check that we are still left with enough reviews to process
  if (length(reviews) < 10) {
    print(paste(y, "Not enough reviews after filtering"))
    next
  }
  #Put the given and calculated scores into their vectors for the data frame later
  givenScores <- append(givenScores, total_rating)
  retrivedReviews[length(retrivedReviews) + 1] = reviews
}

for (y in 1:length(retrivedReviews)) {
  reviews = retrivedReviews[y]
  #Setup sentiment analysis to process the Vector
  s = get_nrc_sentiment(reviews)
  sentiment = vector()
  #Store the sentiment scores in the sentiment vector
  sentiment = c(sentiment, 0:length(reviews))
  
  #loop over the reviews
  for (x in 0:length(reviews)) {
    #For each review we will grab the positive and negative sentiment
    positive = s$positive[x]
    negative = s$negative[x]
    string = reviews[x]
    #Apply a ratio to the sentiment based on the length of the review, essentially calculating the ratio of positive and negative sentiments per word
    positive_ratio = (positive / stri_count_words(string)) * 100
    negative_ratio = (negative / stri_count_words(string)) * 100
    
    #Store the overall score as positive subtract negative, meaning that a more positive review will create a more positive score and vise versa for negative reviews
    sentiment[x] = (positive_ratio - negative_ratio) + 50
  }
  #Generate an overall score for each listing, again applying a ratio of sentiment per review. Meaning a listing with many reviews doesn't get a higher score than one with few reviews
  sentiment_total = sum(sentiment) / length(reviews)
  calculatedScores <- append(calculatedScores, sentiment_total)
  
  print(paste(y, "Given -" , givenScores[y], "Calculated -", sentiment_total))
}


df <- data.frame(givenScores, calculatedScores)
print(df)

plot(
  df$calculatedScores,
  df$givenScores,
  type = "p",
  col = "black",
  xlab = "Calculated Scores",
  ylab = "Gives Scores",
  main = "Scores Comparison"
)
