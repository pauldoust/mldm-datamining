# setwd("F:\\MLDM\\2nd Semester\\Data Mining and Knowledge Discovery\\Practical\\Project")
library(recommenderlab)
library(data.table)
library(ggplot2)
library(dplyr)
library("PerformanceAnalytics")
library(magrittr)
library(tidyr)
library(pander)
library(arules)


ratings <- fread('ratings.csv')
books <- fread('books.csv')
ratings = unique(setDT(ratings), by = c("book_id", "user_id"))
booksToAnalyze = select(books, books_count, original_publication_year, average_rating, ratings_count)
chart.Correlation(booksToAnalyze, histogram=TRUE, pch=19)
dimension_names <- list(user_id = sort(unique(ratings$user_id)), book_id = sort(unique(ratings$book_id)))
RatingTable =  dcast(ratings, formula = user_id ~ book_id, value.var = "rating")
RatingTable <- as.matrix(RatingTable[,-1])
dimnames(RatingTable) <- dimension_names

# To save time :
# RatingTable = RatingTable[1:2500,]

ratingmat0 <- RatingTable
ratingmat0[is.na(ratingmat0)] <- 0
rm(RatingTable)
gc()

SparseTable <- as(ratingmat0, "sparseMatrix")

rm(ratingmat0)
gc()


RealRatingsTable <- new("realRatingMatrix", data = SparseTable)
RealRatingsTable



model <- Recommender(RealRatingsTable, method = "UBCF", param = list(method = "pearson", nn = 4))



scheme <- evaluationScheme(RealRatingsTable[1:10,], method = "cross-validation", k = 3, given = -1, goodRating = 5)



algorithms <- list("Random_Items" = list(name = "RANDOM", param = list(normalize = "Z-score")),
                   "Popular_Items" = list(name = "POPULAR", param = list(normalize = "Z-score")),
                   "User_based" = list(name = "UBCF", param = list(normalize = "Z-score", nn = 50))
)
resultsRating <- evaluate(scheme, algorithms, type = "ratings")
resultsMetric <- evaluate(scheme, algorithms)
results <- evaluate(scheme, algorithms, n = c(1, 3, 5, 10, 15, 20) )

plot(resultsMetric)
plot(resultsRating, annotate = 1:4, legend = "topleft")



modelcomp <- as.data.frame(sapply(avg(resultsRating), rbind))
modelcompnew <- as.data.frame(t(as.matrix(modelcomp)))
colnames(modelcompnew) <- c("RMSE", "MSE", "MAE")

pander(modelcompnew, caption = "Model Comparison Based On Varying Recommendation")

