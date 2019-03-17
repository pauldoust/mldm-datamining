
# setwd("F:\\MLDM\\2nd Semester\\Data Mining and Knowledge Discovery\\Practical\\Project\\10k")
library(ggplot2)
library(recommenderlab)
library(data.table)
library(dplyr)
library(magrittr)
library(tidyr)
library(pander)
library(arules)


books <- fread('books.csv')
tags <- fread('tags.csv')
book_tags <- fread('book_tags.csv')
ratings <- fread('ratings.csv')
to_read <- fread('to_read.csv')

#Book
dim(books)
summary(books$id)
summary(books$original_publication_year)

#Ratings
dim(ratings)
head(ratings)
summary(ratings)

# Tags
dim(tags)
head(tags)
summary(tags)



# Book Tags
dim(book_tags)
head(book_tags)
summary(book_tags)



summary(ratings)

hist(ratings$rating, main="Histogram for Ratings",
     xlab="Ratings", border="black", col="blue", breaks=5)


# clean duplicates
ratings = unique(setDT(ratings), by = c("book_id", "user_id"))

# find count of ratings/ user
ratings = ratings %>% group_by(user_id) %>% mutate(RatingPerUser = n())

ratings <- ratings[order(ratings$RatingPerUser),]

hist(ratings$RatingPerUser,xlab="#Ratings per user", border="black", col="blue")


#display histogram Average rating per user
ratingsAvgPerUsers = ratings %>% group_by(user_id) %>% summarize(AvgRatingPerUser = mean(rating))
hist( ratingsAvgPerUsers$AvgRatingPerUser, main="Histogram of average rating per user",xlab="#Avg Ratings per user", border="black", col="blue")

# To Read
to_read = unique(setDT(to_read), by = c("book_id", "user_id"))

percentage_users_to_read = (dim(unique(setDT(to_read), by = 'user_id'))[1] )/ (dim(unique(setDT(ratings), by = 'user_id'))[1])
cat("Percentage of users with to_read to all users: ", percentage_users_to_read * 100)


percentage_users_to_read = (dim(unique(setDT(to_read), by = 'book_id'))[1] )/ (dim(unique(setDT(books), by = 'book_id'))[1])
cat("Percentage of books in to_read to books: ", percentage_users_to_read * 100)


# Rating Disribution per book
ratingsCountPerBook = ratings %>% group_by(book_id) %>% summarize(ratingsNoPerBook  = n())
ratingsCountPerBook[order(ratingsCountPerBook$ratingsNoPerBook),]
summary(ratingsCountPerBook)
hist( ratingsCountPerBook$ratingsNoPerBook, main="Histogram for # of ratings per book",xlab="# of ratings", border="black", col="blue", breaks = 50)
