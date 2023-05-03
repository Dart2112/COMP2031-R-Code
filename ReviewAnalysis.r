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

#Vectors to store the results after they are calculated
givenScores = vector()
calculatedScores = vector()

#This vector will store the vectors of reviews for each property
#So each entry in this vector is an individual listing and contains all filtered reviews
retrivedReviews = vector()

#This is used to track how many listings pass initial filtering
enoughReviews = 0

#
#Data Collection, Wrangling and Tidying Loop
#

#We get all listings from the collection to analyse
for (y in 1:airbnbcollection$count()) {
  #Get the next listing from the iterator
  entry = iterator$one()
  
  #Get the reviews from that listing
  review_list = entry[["reviews"]]
  #Make sure the listing has more than 10 reviews so that we have enough data to analyse
  if (length(review_list) < 10) {
    #If it has less then 10 reviews we grab skip this listing
    print(paste(y, " Not enough reviews"))
    next
  }
  #Logging that we made it to this point in the code so that we can track this
  enoughReviews = enoughReviews + 1
  #Fetch the total rating from the reviews
  total_rating = entry[["review_scores"]][["review_scores_rating"]]
  #Make a new vector to store the contents of the reviews from this listing
  reviews = vector()
  #Loop over the entire list of reviews and add them to the vector if they are English and not null
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
    reviews[length(reviews) + 1] = review
  }
  #Check that we are still left with enough reviews to process
  if (length(reviews) < 10) {
    print(paste(y, "Not enough reviews after filtering"))
    next
  }
  #Put the given scores into the for the data frame later
  givenScores <- append(givenScores, total_rating)
  #Store the reviews into the vector for the sentiment analysis for later
  retrivedReviews[length(retrivedReviews) + 1] = reviews
}

#
#Data Analysis Loop
#

#Loop over each listing that we stored in the first loop
for (y in 1:length(retrivedReviews)) {
  #Load the reviews for this listing into a variable
  reviews = retrivedReviews[y]
  #Setup sentiment analysis to process the Vector
  s = get_nrc_sentiment(reviews)
  sentiment = vector()
  #Store the sentiment scores in the sentiment vector
  sentiment = c(sentiment, 1:length(reviews))
  
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
    #The adding of 50 is to normalize it compared to the given scores. e.g. 0-100
    sentiment[x] = (positive_ratio - negative_ratio) + 50
  }
  #Generate an overall score for each listing, again applying a ratio of sentiment per review. Meaning a listing with many reviews doesn't get a higher score than one with few reviews
  sentiment_total = sum(sentiment) / length(reviews)
  #Store the calculated score into the vector for visualizations
  calculatedScores <- append(calculatedScores, sentiment_total)
  
  #Print the two scores for debugging
  print(paste(y, "Given -" , givenScores[y], "Calculated -", sentiment_total))
}

#Insert data into a data frame
df <- data.frame(givenScores, calculatedScores)
#Print the data frame
print(df)

#Make a plot of all items in the dataset
plot(
  df$calculatedScores,
  df$givenScores,
  type = "p",
  col = "black",
  xlab = "Calculated Scores",
  ylab = "Gives Scores",
  main = "Scores Comparison"
)
