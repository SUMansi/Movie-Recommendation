#' ---
#' title: "Movie Recommendation"
#' author: 'From: Mansi Agarwal, 2nd TeamMember, 3rd TeamMember'
#' date: '`r format(Sys.time(), "%B %d, %Y")`'
#' output:
#'   md_document:
#'     variant: markdown_github 
#'     toc: true
#'     toc_depth: '5'
#'  ---
#' ***
#' ## Objectives    
## ------------------------------------------------------------------------
# Course: 
# Purpose: 1. To predict user ratings for movies
#          2. Learn from data and recommend best movies to users, based on UBCF & others models

#'   
## ---- message=FALSE, warning=FALSE, echo=TRUE----------------------------
## Clean the current environment  
knitr::opts_chunk$set(echo = TRUE, tidy = TRUE)

# Clear environment
rm(list = ls(all = TRUE)) 
 
# Load packages
library(tidyverse)
library(sqldf)
library(readxl)
library(data.table)
library(scales)
library(gridExtra)
library(stringr)
library(recommenderlab)
library(Matrix)
library(arules)
library(knitr)
library(kableExtra)

#' 
#' ## Load Data to begin EDA       
## ------------------------------------------------------------------------
## Import Data
Movie_Title  <- read.csv("movies.csv",  header = T)
Movie_Rating <- read.csv("ratings.csv", header = T)

# To extract the year of release from the movie title
Movie_Title$YearOfRelease <- str_sub(Movie_Title$title,-5,-2)
# To remove the year of release from movie title
Movie_Title$title         <- gsub('.{6}$', '', Movie_Title$title)

# Remove those 10 movies for which year of release is not mentioned
Movie_Title <-
Movie_Title %>%
  filter(!(YearOfRelease %in% c("986)" , "011)" , "007-", "010)" ," Roa", "espo" ,"hing" ,"boxe")))

#' 
#' ### Transform the Data
## ------------------------------------------------------------------------
# Split Genre 
MV_total = data.frame()

for(i in 1: nrow(Movie_Title)){
a  <- (strsplit(as.vector(Movie_Title$genres[i]),"\\|"))

DB <- data.frame(a,Movie_Title$movieId[i], Movie_Title$title[i], Movie_Title$YearOfRelease[i])
colnames(DB) <- c("Genre", "Movie_ID", "Movie_Name", "Year_Of_Release")

MV_total <- rbind(MV_total,DB)
}

#Spread the data across multiple movie Genre
MV_total_spread_data <- MV_total %>% 
                         group_by(Movie_ID,Movie_Name, Year_Of_Release, Genre) %>%
                          summarise(count = n()) %>%
                              spread(Genre, count)

MV_total_spread_data[is.na(MV_total_spread_data)] <- 0

M_T_R_Join <-
sqldf("SELECT * FROM Movie_Rating MR INNER JOIN MV_total_spread_data MT ON MR.movieID=MT.Movie_ID")
  
M_T_R_Join2 <-
sqldf("SELECT * FROM Movie_Rating MR INNER JOIN MV_total MT ON MR.movieID=MT.Movie_ID")

#' 
#' ## Exploratory Data Analysis
#' 
#' ### Number of Movies across each Genre    
#' 
## ---- fig.width=9, fig.height=5------------------------------------------
#To display the number of Movies across each Genre 
MV_total %>%
  group_by(Genre) %>%
  summarise(Num_Of_Mov = n()) %>%
  arrange(desc(Num_Of_Mov)) %>%
   ggplot(aes(x=reorder(Genre,Num_Of_Mov), y=Num_Of_Mov, fill = "red")) +
    geom_bar(stat = "identity", position = "dodge") + 
    theme_classic() + 
    xlab("Movie Genre") +
    ylab("Number of Movies") +
    guides(fill=FALSE) +
    geom_text(aes(label = round(Num_Of_Mov,0)) , size = 4, vjust = -0.3,  color = "black") +
    theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1)) +
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 11)) +
    ggtitle("Number of Movies across each Genre") +
    theme(plot.title = element_text(hjust = 0.5))

#' 
#' ### Average Movie Rating across each Genre
#' 
## ---- fig.width=9, fig.height=4------------------------------------------
M_T_R_Join2 %>%
  filter(!(Genre %in% "(no genres listed)")) %>%
  group_by(Genre) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(aes(x=reorder(Genre,avg_rating), y=avg_rating, fill = "red")) +
    geom_bar(stat = "identity", position = "dodge") + 
    theme_classic() + 
    xlab("Movie Genre") +
    ylab("Movie Rating") +
    guides(fill=FALSE) +
    geom_text(aes(label = round(avg_rating,2)) , size = 4, vjust = 1.3,  color = "black") +
    theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1)) +
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 11)) +
    ggtitle("Average movie rating across each Genre") +
    theme(plot.title = element_text(hjust = 0.5))

#' 
#' ### Number of Movie Reviews across each rating
#' 
## ---- fig.width=9, fig.height=6------------------------------------------
M_T_R_Join %>%
  group_by(rating) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x=rating, y=Count, fill = "red")) +
  #ggplot(aes(x=reorder(rating,Count), y=Count, fill = "red")) +
    geom_bar(stat = "identity", position = "dodge") + 
    theme_classic() + 
    xlab("Movie Rating") +
    ylab("Number of Reviews") +
    guides(fill=FALSE) +
    geom_text(aes(label = round(Count,2)) , size = 4, vjust = -0.3,  color = "black") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1)) +
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 11)) +
    ggtitle("Number of Movie Reviews across each rating") +
    theme(plot.title = element_text(hjust = 0.5))

#' 
#' ### Number of Movie Reviews of the top Movies
#' 
## ---- fig.width=13, fig.height=6-----------------------------------------
Table1 <-
M_T_R_Join %>%
  #transform(Movie = paste(Movie_Name, "(", Year_Of_Release, ")", sep="")) %>%
  group_by(Movie_Name) %>%
  summarise(Count = n(), Rating = mean(rating)) %>%
  filter(Count >= 250)

G1 <-
Table1 %>%
  #ggplot(aes(x=Movie, y=Count, fill = "red")) +
  ggplot(aes(x=reorder(Movie_Name,Count), y=Count, fill = "red")) +
    geom_bar(stat = "identity", position = "dodge") + 
    theme_classic() + 
    xlab("Movie Title") +
    ylab("Number of Reviews") +
    guides(fill=FALSE) +
    geom_text(aes(label = round(Count,2)) , size = 4, vjust = -0.3,  color = "black") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 11)) +
    theme(axis.title.x = element_blank()) +
    ggtitle("Number of Movie Reviews of the top movies")+
    theme(plot.title = element_text(hjust = 0.5))

G2 <-
Table1 %>%
  #ggplot(aes(x=Movie, y=Count, fill = "red")) +
  ggplot(aes(x=reorder(Movie_Name,Count), y=Rating, fill = "red")) +
    geom_bar(stat = "identity", position = "dodge") + 
    theme_classic() + 
    xlab("Movie Title") +
    ylab("Movie Rating") +
    guides(fill=FALSE) +
    geom_text(aes(label = round(Rating,2)) , size = 4, vjust = -0.3,  color = "black") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 11)) +
    theme(axis.title.x = element_blank()) +
    ggtitle("Average rating for popular movies")+
    theme(plot.title = element_text(hjust = 0.5))

grid.arrange(G1, G2, nrow=1)

#' 
## ---- include=FALSE------------------------------------------------------
##convert rating matrix into a sparse matrix of type "realRatingMatrix"
RatingMatrix <-  M_T_R_Join %>%
                  select(userId,movieId,rating) %>%
                   spread(movieId, rating)

# Remove userIds
RatingMatrix <- as.matrix(RatingMatrix[,-1]) 

# Convert rating matrix into a recommenderlab sparse matrix
M_T_R_Join_rm <- as(RatingMatrix, "realRatingMatrix") #671 x 9057 rating matrix of class 'realRatingMatrix' with 99995 ratings.

#' 
#' ### Similarity between Users
#' 
## ------------------------------------------------------------------------
#Collaborative Filtering: Recommendations that use the preferences or characteristics of the user and the collective ratings of other users
set.seed(1113)

# Similarity function to compute similarity

##===Similarity between 6 random users
Similar_Users <- similarity(M_T_R_Join_rm[sample(1:671, 6), ], method = "cosine", which = "users") #pearson, jaccard

# The more red the cell is, the more similar two users are. Note that the diagonal is red, since it's comparing each user with itself:
image(as.matrix(Similar_Users), main = "Similarity between Users")

#' 
#' ### Similarity between Movies
#' 
## ------------------------------------------------------------------------
##===Similarity between 6 random movies
Similar_Movies <- similarity(M_T_R_Join_rm[, sample(1:671, 6)], method = "cosine", which = "items") #pearson , jaccard

# The more red the cell is, the more similar two users are. Note that the diagonal is red, since it's comparing each user with itself:
image(as.matrix(Similar_Movies), main = "Similarity between Movies")

#' 
#' ### Heatmap of the top Users and Movies
#' 
## ---- fig.width=10-------------------------------------------------------
# At least 20 movies are rated by every user
Min_User <- min(rowCounts(M_T_R_Join_rm))  # 20

# At Max 2389 movies are rated by a user
Max_User <- max(rowCounts(M_T_R_Join_rm))# 2389

# Users have rated at least 1 movie
Min_Movie <-  min(colCounts(M_T_R_Join_rm))  # 1

# At Max a movie is rated by 341 users
Max_Movie <- max(colCounts(M_T_R_Join_rm))  # 341

# Rows : USER ID, Column : Movie ID
Ratings_Movies <- M_T_R_Join_rm[rowCounts(M_T_R_Join_rm) > 900,
                             colCounts(M_T_R_Join_rm) > 225]

#Visualize the top users and movies, In the heatmap, some rows darker than others mean that some users give higher ratings to all the movies. 
image(Ratings_Movies, main = "Heatmap of the top Users and Movies")

#' 
#' ## Movie Rating Prediction
#' 
#' ### Splitting the data into training and test sets using a 70/30 proportion
#' 
## ------------------------------------------------------------------------
# Training set consist of 70% of the data.
set.seed(1234)
evlt <- evaluationScheme(M_T_R_Join_rm, method="split", train=0.7,
                         given=12)

tr <- getData(evlt, "train")
tst_known   <- getData(evlt, "known")
tst_unknown <- getData(evlt, "unknown")

#' 
#' ### UBCF Model by splitting the data into training and test sets
#' 
## ------------------------------------------------------------------------
##=============================== Cosine Distance

########User-based Collaborative Filtering Model - Using Cosine Distance, Center normalization

## create a user-based CF recommender using training data
UBCF_Model_C_C_R <- Recommender(data = tr, method = "UBCF",param=list(method="Cosine", normalize = "center"))
## create predictions for the test users using known ratings
UBCF_Pred_C_C_R <- predict(UBCF_Model_C_C_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
UBCF_Pred_Acc_C_C_R <- calcPredictionAccuracy(UBCF_Pred_C_C_R, tst_unknown)

########User-based Collaborative Filtering Model - Using Cosine Distance, Z-score normalization

## create a user-based CF recommender using training data
UBCF_Model_C_Z_R <- Recommender(data = tr, method = "UBCF",param=list(method="Cosine", normalize = "Z-score"))
## create predictions for the test users using known ratings
UBCF_Pred_C_Z_R <- predict(UBCF_Model_C_Z_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
UBCF_Pred_Acc_C_Z_R <- calcPredictionAccuracy(UBCF_Pred_C_Z_R, tst_unknown)

########User-based Collaborative Filtering Model - Using Cosine Distance, non-normalized

## create a user-based CF recommender using training data
UBCF_Model_C_N_R <- Recommender(data = tr, method = "UBCF",param=list(method="Cosine", normalize = NULL))
## create predictions for the test users using known ratings
UBCF_Pred_C_N_R <- predict(UBCF_Model_C_N_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
UBCF_Pred_Acc_C_N_R <- calcPredictionAccuracy(UBCF_Pred_C_N_R, tst_unknown)

##=============================== Pearson Distance

########User-based Collaborative Filtering Model - Using Pearson Distance, Center normalization

## create a user-based CF recommender using training data
UBCF_Model_P_C_R <- Recommender(data = tr, method = "UBCF", param=list(method="Pearson", normalize = "center"))
## create predictions for the test users using known ratings
UBCF_Pred_P_C_R <- predict(UBCF_Model_P_C_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
UBCF_Pred_Acc_P_C_R <- calcPredictionAccuracy(UBCF_Pred_P_C_R, tst_unknown)

########User-based Collaborative Filtering Model - Using Pearson Distance, Z-score normalization

## create a user-based CF recommender using training data
UBCF_Model_P_Z_R <- Recommender(data = tr, method = "UBCF", param=list(method="Pearson", normalize = "Z-score"))
## create predictions for the test users using known ratings
UBCF_Pred_P_Z_R <- predict(UBCF_Model_P_Z_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
UBCF_Pred_Acc_P_Z_R <- calcPredictionAccuracy(UBCF_Pred_P_Z_R, tst_unknown)

########User-based Collaborative Filtering Model - Using Pearson Distance, non-normalized

## create a user-based CF recommender using training data
UBCF_Model_P_N_R <- Recommender(data = tr, method = "UBCF", param=list(method="Pearson", normalize = NULL))
## create predictions for the test users using known ratings
UBCF_Pred_P_N_R <- predict(UBCF_Model_P_N_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
UBCF_Pred_Acc_P_N_R <- calcPredictionAccuracy(UBCF_Pred_P_N_R, tst_unknown)

##=============================== Jaccard Distance

########User-based Collaborative Filtering Model - Using Jaccard Distance, Center normalization
## create a user-based CF recommender using training data
UBCF_Model_J_C_R <- Recommender(data = tr, method = "UBCF", param=list(method="Jaccard", normalize = "center"))
## create predictions for the test users using known ratings
UBCF_Pred_J_C_R <- predict(UBCF_Model_J_C_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
UBCF_Pred_Acc_J_C_R <- calcPredictionAccuracy(UBCF_Pred_J_C_R, tst_unknown)

########User-based Collaborative Filtering Model - Using Jaccard Distance, Z-score normalization

## create a user-based CF recommender using training data
UBCF_Model_J_Z_R <- Recommender(data = tr, method = "UBCF", param=list(method="Jaccard", normalize = "Z-score"))
## create predictions for the test users using known ratings
UBCF_Pred_J_Z_R <- predict(UBCF_Model_J_Z_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
UBCF_Pred_Acc_J_Z_R <- calcPredictionAccuracy(UBCF_Pred_J_Z_R, tst_unknown)

########User-based Collaborative Filtering Model - Using Jaccard Distance, non-normalized

## create a user-based CF recommender using training data
UBCF_Model_J_N_R <- Recommender(data = tr, method = "UBCF", param=list(method="Jaccard", normalize = NULL))
## create predictions for the test users using known ratings
UBCF_Pred_J_N_R <- predict(UBCF_Model_J_N_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
UBCF_Pred_Acc_J_N_R <- calcPredictionAccuracy(UBCF_Pred_J_N_R, tst_unknown)


##=============================== Euclidean Distance

########User-based Collaborative Filtering Model - Using Euclidean Distance, Center normalization

## create a user-based CF recommender using training data
UBCF_Model_E_C_R <- Recommender(data = tr, method = "UBCF", param=list(method="Euclidean", normalize = "center"))
## create predictions for the test users using known ratings
UBCF_Pred_E_C_R <- predict(UBCF_Model_E_C_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
UBCF_Pred_Acc_E_C_R <- calcPredictionAccuracy(UBCF_Pred_E_C_R, tst_unknown)

########User-based Collaborative Filtering Model - Using Euclidean Distance, Z-score normalization

## create a user-based CF recommender using training data
UBCF_Model_E_Z_R <- Recommender(data = tr, method = "UBCF", param=list(method="Euclidean", normalize = "Z-score"))
## create predictions for the test users using known ratings
UBCF_Pred_E_Z_R <- predict(UBCF_Model_E_Z_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
UBCF_Pred_Acc_E_Z_R <- calcPredictionAccuracy(UBCF_Pred_E_Z_R, tst_unknown)

########User-based Collaborative Filtering Model - Using Euclidean Distance, non-normalized

## create a user-based CF recommender using training data
UBCF_Model_E_N_R <- Recommender(data = tr, method = "UBCF", param=list(method="Euclidean", normalize = NULL))
## create predictions for the test users using known ratings
UBCF_Pred_E_N_R <- predict(UBCF_Model_E_N_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
UBCF_Pred_Acc_E_N_R <- calcPredictionAccuracy(UBCF_Pred_E_N_R, tst_unknown)

###########=====================================================================########### Actual & Prediction rating Matrix
#Actual Movie rating provided by user
original  <- as(tst_unknown, "data.frame")
#Predicted movie rating : User ID in row, Movie ID in column
predicted_UBCF <- as(UBCF_Pred_C_C_R, "data.frame")

colnames(predicted_UBCF) <- c("User", "Movie", "Predicted Rating")
predicted_UBCF <- sqldf("select p.*, o.rating as Original_Rating from predicted_UBCF p left outer join original o on p.User = o.user and p.Movie = o.item"  )

P_A_UBCF <- sqldf("SELECT P.*, M.Movie_Name FROM predicted_UBCF P INNER JOIN MV_total_spread_data M ON P.Movie = M.Movie_ID")

Original_Rating_Matrix <-
P_A_UBCF %>%
  filter(User %in% c(402,367,487,316,623,5)) %>%
  filter(Movie %in% c(2571, 4963, 318, 59315,296)) %>%
  select(User, Movie_Name, Original_Rating) %>%
  spread (Movie_Name, Original_Rating) %>%
  select(User,`Ocean's Eleven `, `Iron Man `,`Matrix, The `,`Shawshank Redemption, The `, `Pulp Fiction ` ) %>%
  kable("html", row.names= TRUE, caption = "Actual Movie Ratings") %>%
  kable_styling(bootstrap_options = c("striped","bordered"), full_width = F, position = "center") 
  

Predicted_Rating_Matrix_UBCF <-
P_A_UBCF %>%
  filter(User %in% c(402,367,487,316,623,5)) %>%
  filter(Movie %in% c(2571, 4963, 318, 59315,296)) %>%
  select(User, Movie_Name, `Predicted Rating`) %>%
  spread (Movie_Name, `Predicted Rating`) %>%
  select(User,`Ocean's Eleven `, `Iron Man `,`Matrix, The `,`Shawshank Redemption, The `, `Pulp Fiction ` ) %>%
  kable("html", row.names= TRUE, caption = "Predicted Movie Ratings by UBCF Method") %>%
  kable_styling(bootstrap_options = c("striped","bordered"), full_width = F, position = "center") 
 
Original_Rating_Matrix
Predicted_Rating_Matrix_UBCF

#' 
#' ### ALS Model by splitting the data into training and test sets
#' 
## ------------------------------------------------------------------------
######## ALS Collaborative Filtering Model - Using Cosine Distance

## create a user-based CF recommender using training data
ALS_Model_R <- Recommender(data = tr, method = "ALS")
## create predictions for the test users using known ratings
ALS_Pred_R <- predict(ALS_Model_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
ALS_Pred_Acc_R <- calcPredictionAccuracy(ALS_Pred_R, tst_unknown)

###########===============================Actual & Prediction rating
#Actual Movie rating provided by user
original <- as(tst_unknown, "data.frame")
#Predicted movie rating : User ID in row, Movie ID in column
predicted_ALS <- as(ALS_Pred_R, "data.frame")

colnames(predicted_ALS) <- c("User", "Movie", "Predicted Rating")
predicted_ALS <- sqldf("select p.*, o.rating as Original_Rating from predicted_ALS p left outer join original o on p.User = o.user and p.Movie = o.item"  )

P_A_ALS <- sqldf("SELECT P.*, M.Movie_Name FROM predicted_ALS P INNER JOIN MV_total_spread_data M ON P.Movie = M.Movie_ID")

Predicted_Rating_Matrix_ALS <-
P_A_ALS %>%
  filter(User %in% c(402,367,487,316,623,5)) %>%
  filter(Movie %in% c(2571, 4963, 318, 59315,296)) %>%
  select(User, Movie_Name, `Predicted Rating`) %>%
  spread (Movie_Name, `Predicted Rating`) %>%
  select(User,`Ocean's Eleven `, `Iron Man `,`Matrix, The `,`Shawshank Redemption, The `, `Pulp Fiction ` ) %>%
  kable("html", row.names= TRUE, caption = "Predicted Movie Ratings by ALS Method") %>%
  kable_styling(bootstrap_options = c("striped","bordered"), full_width = F, position = "center") 
 
Predicted_Rating_Matrix_ALS

#' 
#' ### POPULAR Model by splitting the data into training and test sets
#' 
## ------------------------------------------------------------------------
######## POPULAR Collaborative Filtering Model - Using Cosine Distance

## create a user-based CF recommender using training data
POPULAR_Model_R <- Recommender(data = tr, method = "POPULAR")
## create predictions for the test users using known ratings
POPULAR_Pred_R <- predict(POPULAR_Model_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
POPULAR_Pred_Acc_R <- calcPredictionAccuracy(POPULAR_Pred_R, tst_unknown)

###########===============================Actual & Prediction rating
#Actual Movie rating provided by user
original  <- as(tst_unknown, "data.frame")
#Predicted movie rating : User ID in row, Movie ID in column
predicted_POPULAR <- as(POPULAR_Pred_R, "data.frame")

colnames(predicted_POPULAR) <- c("User", "Movie", "Predicted Rating")
predicted_POPULAR <- sqldf("select p.*, o.rating as Original_Rating from predicted_POPULAR p left outer join original o on p.User = o.user and p.Movie = o.item"  )

P_A_POPULAR <- sqldf("SELECT P.*, M.Movie_Name FROM predicted_POPULAR P INNER JOIN MV_total_spread_data M ON P.Movie = M.Movie_ID")

Predicted_Rating_Matrix_POPULAR <-
P_A_POPULAR %>%
  filter(User %in% c(402,367,487,316,623,5)) %>%
  filter(Movie %in% c(2571, 4963, 318, 59315,296)) %>%
  select(User, Movie_Name, `Predicted Rating`) %>%
  spread (Movie_Name, `Predicted Rating`) %>%
  select(User,`Ocean's Eleven `, `Iron Man `,`Matrix, The `,`Shawshank Redemption, The `, `Pulp Fiction ` ) %>%
  kable("html", row.names= TRUE, caption = "Predicted Movie Ratings by POPULAR Method") %>%
  kable_styling(bootstrap_options = c("striped","bordered"), full_width = F, position = "center") 
 
Predicted_Rating_Matrix_POPULAR

#' ### RANDOM Model by splitting the data into training and test sets
#' 
## ------------------------------------------------------------------------
######## RANDOM Collaborative Filtering Model - Using Cosine Distance

## create a user-based CF recommender using training data
RANDOM_Model_R <- Recommender(data = tr, method = "RANDOM")
## create predictions for the test users using known ratings
RANDOM_Pred_R <- predict(RANDOM_Model_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
RANDOM_Pred_Acc_R <- calcPredictionAccuracy(RANDOM_Pred_R, tst_unknown)

######## RANDOM Collaborative Filtering Model - Using Pearson Distance

## create a user-based CF recommender using training data
RANDOM_Model_P_R <- Recommender(data = tr, method = "RANDOM", param=list(method="Pearson"))
## create predictions for the test users using known ratings
RANDOM_Pred_P_R <- predict(RANDOM_Model_P_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
RANDOM_Pred_Acc_P_R <- calcPredictionAccuracy(RANDOM_Pred_P_R, tst_unknown)

######## RANDOM Collaborative Filtering Model - Using Jaccard Distance

## create a user-based CF recommender using training data
RANDOM_Model_J_R <- Recommender(data = tr, method = "RANDOM", param=list(method="Jaccard"))
## create predictions for the test users using known ratings
RANDOM_Pred_J_R <- predict(RANDOM_Model_J_R, tst_known, type="ratings")
## evaluate recommendations on "unknown" ratings
RANDOM_Pred_Acc_J_R <- calcPredictionAccuracy(RANDOM_Pred_J_R, tst_unknown)

###########===============================Actual & Prediction rating
#Actual Movie rating provided by user
Actual_Rating_RANDOM_R <- as(tst_unknown, "matrix")[1:8,1:4]
#Predicted movie rating : User ID in row, Movie ID in column
Prediction_Rating_RANDOM_R <- as(RANDOM_Pred_R, "matrix")[1:8,1:4]

A_MV_Name <- c()
P_MV_Name <- c()
for(i in 1:ncol(Prediction_Rating_RANDOM_R)) {
     P_MV_Name[i] = as.character(MV_total_spread_data$Movie_Name[which(colnames(Prediction_Rating_RANDOM_R)[i] == MV_total_spread_data$Movie_ID)])
     A_MV_Name[i] = as.character(MV_total_spread_data$Movie_Name[which(colnames(Actual_Rating_RANDOM_R)[i] == MV_total_spread_data$Movie_ID)])
}

colnames(Actual_Rating_RANDOM_R) <- A_MV_Name
colnames(Prediction_Rating_RANDOM_R) <- P_MV_Name

## Not displaying the original & predicted rating for the same movies as we displayed above, because  for this model we are not able to convert matrix into dataframe. Hence not feasible to find those users/movies dimensions.

#Original Rating for movies
# Actual_Rating_RANDOM_R %>%
#   kable("html", row.names= TRUE,  caption = "Actual Movie Ratings") %>%
#   kable_styling(bootstrap_options = c("striped","bordered"), full_width = F, position = "center") 
# 
# #Predicted rating for movies by RANDOM Model
# Prediction_Rating_RANDOM_R %>%
#   kable("html", row.names= F, caption = "Predicted Movie Ratings by RANDOM Method") %>%
#   kable_styling(bootstrap_options = c("striped","bordered"), full_width = F, position = "center")

#' 
#' ### Error Metrics
#' 
## ---- fig.width=10-------------------------------------------------------
##### Error Metrics
Model_Accuracy_R <-
rbind(UBCF_C_C = UBCF_Pred_Acc_C_C_R, UBCF_C_Z = UBCF_Pred_Acc_C_Z_R, UBCF_C_N = UBCF_Pred_Acc_C_N_R,
      UBCF_P_C = UBCF_Pred_Acc_P_C_R, UBCF_P_Z = UBCF_Pred_Acc_P_Z_R, UBCF_P_N = UBCF_Pred_Acc_P_N_R,
      UBCF_J_C = UBCF_Pred_Acc_J_C_R, UBCF_J_Z = UBCF_Pred_Acc_J_Z_R, UBCF_J_N = UBCF_Pred_Acc_J_N_R,
      UBCF_E_C = UBCF_Pred_Acc_E_C_R, UBCF_E_Z = UBCF_Pred_Acc_E_Z_R, UBCF_E_N = UBCF_Pred_Acc_E_N_R,
      ALS_C = ALS_Pred_Acc_R, POPULAR_C = POPULAR_Pred_Acc_R,
      RANDOM_C = RANDOM_Pred_Acc_R, RANDOM_P = RANDOM_Pred_Acc_P_R, RANDOM_J = RANDOM_Pred_Acc_J_R)

#========Table to display Error Metrics
Model_Accuracy_R %>%
  kable("html", row.names= TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = F, position = "center") %>%
  column_spec(1, bold = T)

Model_Accuracy_EM_Split <- as.data.frame(Model_Accuracy_R) 
Model_Name <-  as.data.frame(rownames(Model_Accuracy_R))
colnames(Model_Name) <- c("Model_Name")

#========Graph to display Error Metrics

cbind(Model_Accuracy_EM_Split,Model_Name ) %>%
  ggplot(aes(x=reorder(Model_Name,RMSE), y=RMSE)) +
    geom_bar(stat = "identity", position = "dodge", fill = "indianred") + 
    theme_classic() + 
    xlab("Model Name") +
    ylab("RMSE") +
    guides(fill=FALSE) +
    geom_text(aes(label = round(RMSE,3)) , size = 4, vjust = -0.3,  color = "black") +
    theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1)) +
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 11)) +
    theme(axis.title.x = element_blank()) +
    #scale_fill_manual(values = c("lightgoldenrod1")) +
    ggtitle("Model RMSE's") +
    geom_rect(aes(xmin=2.5,xmax=18,ymin=0,ymax=3.55),alpha=0.07,fill="white")  +
    theme(plot.title = element_text(hjust = 0.5))


#' 
#' ## Using k-fold cross-validation  to validate models
#' 
## ------------------------------------------------------------------------
set.seed(1234)

# Split the data into 12 chunks
# Cross Validation
eval_sets <- evaluationScheme(M_T_R_Join_rm, method = "cross-validation", k = 12, given = 12)

tr          <- getData(eval_sets, "train")
tst_known   <- getData(eval_sets, "known")
tst_unknown <- getData(eval_sets, "unknown")

#' 
#' ### Evavluating the ratings
#' 
## ---- fig.width=10-------------------------------------------------------
##=============================== UBCF Cosine Distance

########UBCF Cross Validation - Using Cosine Distance, Center normalization
UBCF_Model_CV_C_C_R       <- Recommender(data = tr, method = "UBCF", param=list(method="Cosine", normalize = "center"))
UBCF_Pred_CV_C_C_R        <- predict(UBCF_Model_CV_C_C_R, tst_known, type = "ratings")
UBCF_Pred_Acc_CV_C_C_R    <- calcPredictionAccuracy(UBCF_Pred_CV_C_C_R, tst_unknown)

########UBCF Cross Validation - Using Cosine Distance, Z-score normalization
UBCF_Model_CV_C_Z_R       <- Recommender(data = tr, method = "UBCF", param=list(method="Cosine", normalize = "Z-score"))
UBCF_Pred_CV_C_Z_R        <- predict(UBCF_Model_CV_C_Z_R, tst_known, type = "ratings")
UBCF_Pred_Acc_CV_C_Z_R    <- calcPredictionAccuracy(UBCF_Pred_CV_C_Z_R, tst_unknown)

########UBCF Cross Validation - Using Cosine Distance, non-normalized
UBCF_Model_CV_C_N_R       <- Recommender(data = tr, method = "UBCF", param=list(method="Cosine", normalize = NULL))
UBCF_Pred_CV_C_N_R        <- predict(UBCF_Model_CV_C_N_R, tst_known, type = "ratings")
UBCF_Pred_Acc_CV_C_N_R    <- calcPredictionAccuracy(UBCF_Pred_CV_C_N_R, tst_unknown)

##=============================== UBCF Pearson Distance

########UBCF Cross Validation - Using Pearson Distance, Center normalization
UBCF_Model_CV_P_C_R       <- Recommender(data = tr, method = "UBCF", param=list(method="Pearson", normalize = "center"))
UBCF_Pred_CV_P_C_R        <- predict(UBCF_Model_CV_P_C_R, tst_known, type = "ratings")
UBCF_Pred_Acc_CV_P_C_R    <- calcPredictionAccuracy(UBCF_Pred_CV_P_C_R, tst_unknown)

########UBCF Cross Validation - Using Pearson Distance, Z-score normalization
UBCF_Model_CV_P_Z_R       <- Recommender(data = tr, method = "UBCF", param=list(method="Pearson", normalize = "Z-score"))
UBCF_Pred_CV_P_Z_R        <- predict(UBCF_Model_CV_P_Z_R, tst_known, type = "ratings")
UBCF_Pred_Acc_CV_P_Z_R    <- calcPredictionAccuracy(UBCF_Pred_CV_P_Z_R, tst_unknown)

########UBCF Cross Validation - Using Pearson Distance, non-normalized
UBCF_Model_CV_P_N_R       <- Recommender(data = tr, method = "UBCF", param=list(method="Pearson", normalize = NULL))
UBCF_Pred_CV_P_N_R        <- predict(UBCF_Model_CV_P_N_R, tst_known, type = "ratings")
UBCF_Pred_Acc_CV_P_N_R    <- calcPredictionAccuracy(UBCF_Pred_CV_P_N_R, tst_unknown)

##=============================== UBCF Jaccard Distance

########UBCF Cross Validation - Using Jaccard Distance, Center normalization
UBCF_Model_CV_J_C_R       <- Recommender(data = tr, method = "UBCF", param=list(method="Jaccard", normalize = "center"))
UBCF_Pred_CV_J_C_R        <- predict(UBCF_Model_CV_J_C_R, tst_known, type = "ratings")
UBCF_Pred_Acc_CV_J_C_R    <- calcPredictionAccuracy(UBCF_Pred_CV_J_C_R, tst_unknown)

########UBCF Cross Validation - Using Jaccard Distance, Z-score normalization
UBCF_Model_CV_J_Z_R       <- Recommender(data = tr, method = "UBCF", param=list(method="Jaccard", normalize = "Z-score"))
UBCF_Pred_CV_J_Z_R        <- predict(UBCF_Model_CV_J_Z_R, tst_known, type = "ratings")
UBCF_Pred_Acc_CV_J_Z_R    <- calcPredictionAccuracy(UBCF_Pred_CV_J_Z_R, tst_unknown)

########UBCF Cross Validation - Using Jaccard Distance, non-normalized
UBCF_Model_CV_J_N_R       <- Recommender(data = tr, method = "UBCF", param=list(method="Jaccard", normalize = NULL))
UBCF_Pred_CV_J_N_R        <- predict(UBCF_Model_CV_J_N_R, tst_known, type = "ratings")
UBCF_Pred_Acc_CV_J_N_R    <- calcPredictionAccuracy(UBCF_Pred_CV_J_N_R, tst_unknown)

##=============================== UBCF Euclidean Distance

########UBCF Cross Validation - Using Euclidean Distance, Center normalization
UBCF_Model_CV_E_C_R       <- Recommender(data = tr, method = "UBCF", param=list(method="Euclidean", normalize = "center"))
UBCF_Pred_CV_E_C_R        <- predict(UBCF_Model_CV_E_C_R, tst_known, type = "ratings")
UBCF_Pred_Acc_CV_E_C_R    <- calcPredictionAccuracy(UBCF_Pred_CV_E_C_R, tst_unknown)

########UBCF Cross Validation - Using Euclidean Distance, Z-score normalization
UBCF_Model_CV_E_Z_R       <- Recommender(data = tr, method = "UBCF", param=list(method="Euclidean", normalize = "Z-score"))
UBCF_Pred_CV_E_Z_R        <- predict(UBCF_Model_CV_E_Z_R, tst_known, type = "ratings")
UBCF_Pred_Acc_CV_E_Z_R    <- calcPredictionAccuracy(UBCF_Pred_CV_E_Z_R, tst_unknown)

########UBCF Cross Validation - Using Euclidean Distance, non-normalized
UBCF_Model_CV_E_N_R       <- Recommender(data = tr, method = "UBCF", param=list(method="Euclidean", normalize = NULL))
UBCF_Pred_CV_E_N_R        <- predict(UBCF_Model_CV_E_N_R, tst_known, type = "ratings")
UBCF_Pred_Acc_CV_E_N_R    <- calcPredictionAccuracy(UBCF_Pred_CV_E_N_R, tst_unknown)

##=============================== ALS

ALS_Model_CV_R       <- Recommender(data = tr, method = "ALS")
ALS_Pred_CV_R        <- predict(ALS_Model_CV_R, tst_known, type = "ratings")
ALS_Pred_Acc_CV_R    <- calcPredictionAccuracy(ALS_Pred_CV_R, tst_unknown)

##=============================== POPULAR

POPULAR_Model_CV_R    <- Recommender(data = tr, method = "POPULAR")
POPULAR_Pred_CV_R     <- predict(POPULAR_Model_CV_R, tst_known, type = "ratings")
POPULAR_Pred_Acc_CV_R <- calcPredictionAccuracy(POPULAR_Pred_CV_R, tst_unknown)

##=============================== RANDOM , Parameter 'method' decides similarity measure: Cosine Distance

RANDOM_Model_CV_C_R     <- Recommender(data = tr, method = "RANDOM", param=list(method="Cosine"))
RANDOM_Pred_CV_C_R      <- predict(RANDOM_Model_CV_C_R, tst_known, type = "ratings")
RANDOM_Pred_Acc_CV_C_R  <- calcPredictionAccuracy(RANDOM_Pred_CV_C_R, tst_unknown)

##=============================== RANDOM Parameter 'method' decides similarity measure: Pearson Distance

RANDOM_Model_CV_P_R     <- Recommender(data = tr, method = "RANDOM", param=list(method="Pearson"))
RANDOM_Pred_CV_P_R      <- predict(RANDOM_Model_CV_P_R, tst_known, type = "ratings")
RANDOM_Pred_Acc_CV_P_R  <- calcPredictionAccuracy(RANDOM_Pred_CV_P_R, tst_unknown)

##=============================== RANDOM Parameter 'method' decides similarity measure: Jaccard Distance

RANDOM_Model_CV_J_R     <- Recommender(data = tr, method = "RANDOM", param=list(method="Jaccard"))
RANDOM_Pred_CV_J_R      <- predict(RANDOM_Model_CV_J_R, tst_known, type = "ratings")
RANDOM_Pred_Acc_CV_J_R  <- calcPredictionAccuracy(RANDOM_Pred_CV_J_R, tst_unknown)

#' 
#' ### Error-Metrics by Cross Validation    
#' 
## ----fig.width=10--------------------------------------------------------
##=============================== Error Metrics by cross validation

Model_Accuracy_CV_R <-
rbind(UBCF_CV_C_C = UBCF_Pred_Acc_CV_C_C_R, UBCF_CV_C_Z = UBCF_Pred_Acc_CV_C_Z_R, UBCF_CV_C_N = UBCF_Pred_Acc_CV_C_N_R, 
      UBCF_CV_P_C = UBCF_Pred_Acc_CV_P_C_R, UBCF_CV_P_Z = UBCF_Pred_Acc_CV_P_Z_R, UBCF_CV_P_N = UBCF_Pred_Acc_CV_P_N_R, 
      UBCF_CV_J_C = UBCF_Pred_Acc_CV_J_C_R, UBCF_CV_J_Z = UBCF_Pred_Acc_CV_J_Z_R, UBCF_CV_J_N = UBCF_Pred_Acc_CV_J_N_R, 
      UBCF_CV_E_C = UBCF_Pred_Acc_CV_E_C_R, UBCF_CV_E_Z = UBCF_Pred_Acc_CV_E_Z_R, UBCF_CV_E_N = UBCF_Pred_Acc_CV_E_N_R, 
      ALS_CV_C = ALS_Pred_Acc_CV_R, POPULAR_CV_C = POPULAR_Pred_Acc_CV_R,
      RANDOM_CV_C = RANDOM_Pred_Acc_CV_C_R, RANDOM_CV_P = RANDOM_Pred_Acc_CV_P_R, RANDOM_CV_J = RANDOM_Pred_Acc_CV_J_R) 

colnames(Model_Accuracy_CV_R) <- c("RMSE_CV", "MSE_CV","MAE_CV")

#========Table to display Error Metrics
# Model_Accuracy_CV_R %>%
#   kable("html", row.names= TRUE) %>%
#     kable_styling(bootstrap_options = "striped", full_width = F, position = "center") %>%
#     column_spec(1, bold = T) 

Model_Accuracy_CV_EM_Split <- as.data.frame(Model_Accuracy_CV_R) 
Model_Name_CV <-  as.data.frame(rownames(Model_Accuracy_CV_R))
colnames(Model_Name_CV) <- c("Model_Name")

#========Graph to display Error Metrics
cbind(Model_Accuracy_CV_EM_Split, Model_Name_CV ) %>%
  ggplot(aes(x=reorder(Model_Name,RMSE_CV), y=RMSE_CV)) +
    geom_bar(stat = "identity", position = "dodge", fill = "indianred") + 
    theme_classic() + 
    xlab("Model Name") +
    ylab("Cross Validation RMSE") +
    guides(fill=FALSE) +
    geom_text(aes(label = round(RMSE_CV,3)) , size = 4, vjust = -0.3,  color = "black") +
    theme(axis.text.x = element_text(angle = 51, hjust = 1, vjust = 1)) +
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 11)) +
    theme(axis.title.x = element_blank()) +
    ggtitle("Model RMSE's by Cross Validation") +
    geom_rect(aes(xmin=2.5,xmax=18,ymin=0,ymax=3.55),alpha=0.07,fill="white")  +
    theme(plot.title = element_text(hjust = 0.5))

#' 
#' 
#' ### RMSE Comparision between split & cross validation
## ------------------------------------------------------------------------
Data <- cbind(Model_Accuracy_CV_EM_Split, Model_Name_CV, Model_Accuracy_EM_Split, Model_Name )
rownames(Data) <- NULL

Data[,c(8,5,1)] %>%
  kable("html", row.names= TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = F, position = "center") 

#' 
#' ## Movie Recommendation based on various algorithms
#' 
#' ### USER-based Collaborative Filtering Model
#' 
## ------------------------------------------------------------------------
# Training set consist of 70% of the data.
set.seed(1234)
evlt <- evaluationScheme(M_T_R_Join_rm, method="split", train=0.7,
                         given=12)

tr <- getData(evlt, "train")
tst_known   <- getData(evlt, "known")
tst_unknown <- getData(evlt, "unknown")

# Build the model
UBCF_Model <- Recommender(data = tr, method = "UBCF")

# Number of movies to recommend for each use
num_movies_recommend <- 6

# Perform recommendations
UBCF_test_users_recommendations <- predict(object = UBCF_Model, newdata = tst_known, n = num_movies_recommend) 

UBCF_recommendations_matrix <- sapply(UBCF_test_users_recommendations@items, 
                                function(x){ as.integer(colnames(M_T_R_Join_rm)[x]) })

# Display recommendations: The below matrix contain movieId of each recommended movie (rows) for the first six users (columns) in our test dataset.
UBCF_Mov_Rec <- UBCF_recommendations_matrix[, 1:3]

UBCF_MATRIX <- matrix(nrow = nrow(UBCF_Mov_Rec), ncol = ncol(UBCF_Mov_Rec)) 

for(i in 1:ncol(UBCF_Mov_Rec)){
  for(j in 1:nrow(UBCF_Mov_Rec)){
    UBCF_MATRIX[j,i] =
   as.character(unname(unlist(M_T_R_Join %>%
                                filter(movieId == UBCF_Mov_Rec[j,i]) %>%
                                distinct(Movie_Name))))
  }
}

colnames(UBCF_MATRIX) <- c(colnames(UBCF_Mov_Rec))
  
as.data.frame(UBCF_MATRIX) %>%
  kable("html", row.names= TRUE, caption = "Top 6 Recommended Movies for various users by UBCF Model" ) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = F, position = "center") 

#'  
## ---- fig.width=7, fig.height=4------------------------------------------
##User-based Collaborative Filtering Model : UBCF
UBCF_Recommended_Movie_Count <- as.data.frame(table(UBCF_recommendations_matrix))

UBCF_Recommended_Movie_Count <-
sqldf("SELECT R.*, M.title FROM UBCF_Recommended_Movie_Count R INNER JOIN Movie_Title M WHERE R.UBCF_recommendations_matrix = M.movieId")

UBCF_Recommended_Movie_Count %>%
  arrange(desc(Freq)) %>%
  head(5) %>%
  ggplot(aes(x=(reorder(title,(Freq))), y=Freq, fill = "red")) +
    geom_bar(stat = "identity", position = "dodge") + 
    theme_classic() + 
    xlab("Movie Title") +
    ylab("Count") +
    guides(fill=FALSE) +
    geom_text(aes(label = round(Freq,2)) , size = 4.5, hjust = 1.3,  color = "black") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1)) +
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 11)) +
    theme(axis.title.y =  element_blank()) +
    ggtitle("UBCF: Top 5 Recommened Movies") + 
    #ggtitle("UBCF") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_flip()

#' 
#' ### ALS Model
#' 
## ------------------------------------------------------------------------
# Build the model
ALS_Model <- Recommender(data = tr, method = "ALS")

# Create Recommendations on Test data

# Number of movies to recommend for each use
num_movies_recommend <- 6

# Perform recommendations
ALS_test_users_recommendations <- predict(object = ALS_Model, newdata = tst_known, n = num_movies_recommend) 

ALS_recommendations_matrix <- sapply(ALS_test_users_recommendations@items, 
                                function(x){ as.integer(colnames(M_T_R_Join_rm)[x]) })

#d# Display recommendations: The below matrix contain movieId of each recommended movie (rows) for the first six users (columns) in our test dataset.
ALS_Mov_Rec <- ALS_recommendations_matrix[, c(1:3)]

ALS_MATRIX <- matrix(nrow = nrow(ALS_Mov_Rec), ncol = ncol(ALS_Mov_Rec)) 

for(i in 1:ncol(ALS_Mov_Rec)){
  for(j in 1:nrow(ALS_Mov_Rec)){
    ALS_MATRIX[j,i] =
   as.character(unname(unlist(M_T_R_Join %>%
                                filter(movieId == ALS_Mov_Rec[j,i]) %>%
                                distinct(Movie_Name))))
  }
}

colnames(ALS_MATRIX) <- c(colnames(ALS_Mov_Rec))
  
as.data.frame(ALS_MATRIX) %>%
  kable("html", row.names= TRUE, caption= "Top 6 Recommended Movies for various users by ALS Model") %>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = F, position = "center") 

#' 
## ---- fig.width=7, fig.height=4,  message=FALSE, warning=FALSE-----------
ALS_Recommended_Movie_Count <- as.data.frame(table(ALS_recommendations_matrix))

ALS_Recommended_Movie_Count <-
sqldf("SELECT R.*, M.title FROM ALS_Recommended_Movie_Count R INNER JOIN Movie_Title M WHERE R.ALS_recommendations_matrix = M.movieId")

ALS_Recommended_Movie_Count %>%
  arrange(desc(Freq)) %>%
  head(5) %>%
  separate(col = title, into = c('Movie_Name'), sep = "\\(") %>%
  ggplot(aes(x=(reorder(Movie_Name,(Freq))), y=Freq, fill = "red")) +
    geom_bar(stat = "identity", position = "dodge") + 
    theme_classic() + 
    xlab("Movie Title") +
    ylab("Count") +
    guides(fill=FALSE) +
    geom_text(aes(label = round(Freq,2)) , size = 4.5, hjust = 1.2,  color = "black") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1.2, vjust = 1)) +
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 11)) +
    theme(axis.title.y =  element_blank()) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    #ggtitle("ALS") +
    ggtitle("ALS: Top 5 Recommened Movies") + 
    coord_flip()

#' 
#' ### POPULAR Model 
#' 
## ------------------------------------------------------------------------
# Build the model
POPULAR_Model <- Recommender(data = tr, method = "POPULAR")

# Create Recommendations on Test data

# Number of movies to recommend for each use
num_movies_recommend <- 6

# Perform recommendations
POPULAR_test_users_recommendations <- predict(object = POPULAR_Model, newdata = tst_known, n = num_movies_recommend) 

POPULAR_recommendations_matrix <- sapply(POPULAR_test_users_recommendations@items, 
                                function(x){ as.integer(colnames(M_T_R_Join_rm)[x]) })

#d# Display recommendations: The below matrix contain movieId of each recommended movie (rows) for the first six users (columns) in our test dataset.
POPULAR_Mov_Rec <- POPULAR_recommendations_matrix[, 1:3]

POPULAR_MATRIX <- matrix(nrow = nrow(POPULAR_Mov_Rec), ncol = ncol(POPULAR_Mov_Rec)) 

for(i in 1:ncol(POPULAR_Mov_Rec)){
  for(j in 1:nrow(POPULAR_Mov_Rec)){
    POPULAR_MATRIX[j,i] =
   as.character(unname(unlist(M_T_R_Join %>%
                                filter(movieId == POPULAR_Mov_Rec[j,i]) %>%
                                distinct(Movie_Name))))
  }
}

colnames(POPULAR_MATRIX) <- c(colnames(POPULAR_Mov_Rec))
  
as.data.frame(POPULAR_MATRIX) %>%
  kable("html", row.names= TRUE, caption = "Top 6 Recommended Movies for various users by POPULAR Model") %>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = F, position = "center") 

#' 
#' 
## ---- fig.width=7, fig.height=4,  message=FALSE, warning=FALSE-----------
POPULAR_Recommended_Movie_Count <- as.data.frame(table(POPULAR_recommendations_matrix))

POPULAR_Recommended_Movie_Count <-
sqldf("SELECT R.*, M.title FROM POPULAR_Recommended_Movie_Count R INNER JOIN Movie_Title M WHERE R.POPULAR_recommendations_matrix = M.movieId")

POPULAR_Recommended_Movie_Count %>%
  arrange(desc(Freq)) %>%
  head(5) %>%
  separate(col = title, into = c('Movie_Name'), sep = "\\(") %>%
  ggplot(aes(x=(reorder(Movie_Name,(Freq))), y=Freq, fill = "red")) +
    geom_bar(stat = "identity", position = "dodge") + 
    theme_classic() + 
    xlab("Movie Title") +
    ylab("Count") +
    guides(fill=FALSE) +
    geom_text(aes(label = round(Freq,2)) , size = 4.5, hjust = 1.2,  color = "black") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1.2, vjust = 1)) +
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 11)) +
    theme(axis.title.y =  element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    #ggtitle("POPULAR") +
    ggtitle("POPULAR: Top 5 Recommened Movies") + 
    coord_flip()

#' 
#' ### Movies recommended for a single user  
#' 
## ------------------------------------------------------------------------
User_5_Movies_Rec <-
cbind(
as.data.frame(UBCF_MATRIX)[3],
as.data.frame(POPULAR_MATRIX)[3],
as.data.frame(ALS_MATRIX)[1]) 

colnames(User_5_Movies_Rec) <- c("UBCF Recommendation", "POPULAR Recommendation", "ALS Recommendation")

User_5_Movies_Rec %>%
  kable("html", row.names= TRUE, caption = "Recommended Movies for User 5") %>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = F, position = "center")


#' 
#' 
