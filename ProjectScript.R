#R Project

require(RCurl)

reviews <-read.csv(text=getURL("https://raw.githubusercontent.com/rohanbapat/STAT-6430-R-Project/master/reviews.txt"), header=F, sep ="\t")
reviews #reviews.txt file read in

colnames(reviews) <- c('ReviewerID', 'MovieID', 'RatingID', 'TimestampUTC')
reviews

sum(unique(reviews$MovieID))


#HOW MANY REVIEWS EACH REVIEWER LEFT
library(dplyr)
ReviewerID_count <- reviews %>%
                     group_by(ReviewerID) %>%
                     summarise(count = n_distinct(MovieID))
moviereviews_df <-read.csv(text=getURL("https://raw.githubusercontent.com/rohanbapat/STAT-6430-R-Project/master/reviews.txt"), header=F, sep ="\t")
colnames(moviereviews_df) <- c('ReviewerID','MovieID','Rating','TimeStampUTC')
moviereviews_df$TimeStampLT<-as.POSIXct(moviereviews_df$TimeStampUTC,origin = "1970-01-01")

#-------------------------------------------------------------------------------------------------------------------------
moviereviews_df <-read.csv(text=getURL("https://raw.githubusercontent.com/rohanbapat/STAT-6430-R-Project/master/reviews.txt"), header=F, sep ="\t")
colnames(moviereviews_df) <- c('ReviewerID','MovieID','Rating','TimeStampUTC')
moviereviews_df$TimeStampLT<-as.POSIXct(moviereviews_df$TimeStampUTC,origin = "1970-01-01")

# Problem 1

# Number of movies by movie rating
rating_summary_df <- moviereviews_df%>%group_by(Rating)%>%summarise(countRating = length(Rating))

# Percentage by movie rating
rating_summary_df$perRating <- rating_summary_df$countRating/sum(rating_summary_df$countRating)

#-------------------------------------------------------------------------------------------------------------------------

# Problem 2

# Number of reviews provided by each reviewer 
reviewercount_summary_df <- moviereviews_df%>%group_by(ReviewerID)%>%summarise(ratingProvided = length(Rating))

# Top 10 reviewers
top10_reviewers <- head(reviewercount_summary_df[order(-reviewercount_summary_df$ratingProvided),],10)

#-------------------------------------------------------------------------------------------------------------------------

# Problem 3

# Average ratings provided at user level
reviewermean_summary_df <- moviereviews_df%>%group_by(ReviewerID)%>%summarise(ratingProvided = mean(Rating))

# Metrics for calculating confidence intervals
mean_overall_reviewers <- mean(reviewermean_summary_df$ratingProvided)
sd_overall_reviewers <- sd(reviewermean_summary_df$ratingProvided)
n_overall_reviewers <- nrow(reviewermean_summary_df)
t_overall_reviewers <- qt(0.975,n_overall_reviewers-1)

# Upper and lower limits of 95% confidence interval for all reviewers
upper_lt_overall_reviewers <- mean_overall_reviewers + t_overall_reviewers*sd_overall_reviewers/sqrt(n_overall_reviewers)
lower_lt_overall_reviewers <- mean_overall_reviewers - t_overall_reviewers*sd_overall_reviewers/sqrt(n_overall_reviewers)

# Average ratings provided by top 10 reviewers
top10_reviewermean_summary_df <- reviewer_summary_df[reviewer_summary_df$ReviewerID %in% top_10_reviewers$ReviewerID,]

# Metrics for calculating confidence intervals
mean_top10_reviewers <- mean(top10_reviewermean_summary_df$ratingProvided)
sd_top10_reviewers <- sd(top10_reviewermean_summary_df$ratingProvided)
n_top10_reviewers <- nrow(top10_reviewermean_summary_df)
t_top10_reviewers <- qt(0.975,n_top10_reviewers-1)

# Upper and lower limits of 95% confidence interval for all reviewers
upper_lt_top10_reviewers <- mean_top10_reviewers + t_top10_reviewers*sd_top10_reviewers/sqrt(n_top10_reviewers)
lower_lt_top10_reviewers <- mean_top10_reviewers - t_top10_reviewers*sd_top10_reviewers/sqrt(n_top10_reviewers)

#-----------------------------------------------------------------------------------------------------
#Problem 4

#import genres.txt to get Movie Title from MovieID
genres_df <-read.csv(text=getURL("https://raw.githubusercontent.com/rohanbapat/STAT-6430-R-Project/master/genres.txt"), header=F, sep ="|")
colnames(genres_df) <- c('ReviewerID','MovieID','Rating','TimeStampUTC')

#NEED TO FIGURE OUT HOW TO GET RELEASEDATE RIGHT AFTER MOVIE TITLE WHICH IS NOT DELIMETED BY A |
colnames(genres_df) <- c('MovieID','MovieTitle','ReleaseDate','VideoReleaseDate','IMDbURL','unknown','Action','Adventure','Animation',
                         'Childrens','Comedy','Crime','Documentary','Drama','Fantasy','FilmNoir','Horror','Musical',
                         'Mystery','Romance','SciFi','Thriller','War','Western')

#library(stringr)

#separate1 <- for i in row
#              int(i) >= 1 then genres_df$VideoReleaseDate = i
              
#separate(genres_df$MovieTitle)
#str_split_fixed(genres_df$MovieTitle, " (", 2)

movie_genres_merged <- merge(moviereviews_df, genres_df,by="MovieID")



# Number of movies reviewed 
MovieTitle_summary_df <- movie_genres_merged%>%group_by(MovieTitle)%>%summarise(TimesReviewed = length(MovieID))

# Top 10 movies reviewed by MovieTitle
top10_movies_reviewed <- head(MovieTitle_summary_df[order(-MovieTitle_summary_df$TimesReviewed),],10)

#-----------------------------------------------------------------------------------------------------
#Problem 5

#specifying only genres - excluding unknown
genres_only_df <- movie_genres_merged[, 11:28]

genres_only_df%>%group_by(MovieTitle)%>%summarise(TimesReviewed = length(MovieID))

genres_only_df_adjusted <- genres_only_df(countAction = sum(movie_genres_merged$Action == 1))


#sum(test==1)
genres_only_df_adjusted <- data.frame(sum(movie_genres_merged$Action == 1),
                                      sum(movie_genres_merged$Adventure == 1),
                                      sum(movie_genres_merged$Animation == 1),
                                      sum(movie_genres_merged$Childrens == 1),
                                      sum(movie_genres_merged$Comedy == 1),
                                      sum(movie_genres_merged$Crime == 1),
                                      sum(movie_genres_merged$Documentary == 1),
                                      sum(movie_genres_merged$Drama == 1),
                                      sum(movie_genres_merged$Fantasy == 1),
                                      sum(movie_genres_merged$FilmNoir == 1),
                                      sum(movie_genres_merged$Horror == 1),
                                      sum(movie_genres_merged$Musical == 1),
                                      sum(movie_genres_merged$Mystery == 1),
                                      sum(movie_genres_merged$Romance == 1),
                                      sum(movie_genres_merged$SciFi == 1),
                                      sum(movie_genres_merged$Thriller == 1),
                                      sum(movie_genres_merged$War == 1),
                                      sum(movie_genres_merged$Western == 1))

colnames(genres_only_df_adjusted) <- c('Action','Adventure','Animation',
                         'Childrens','Comedy','Crime','Documentary','Drama','Fantasy','FilmNoir','Horror','Musical',
                         'Mystery','Romance','SciFi','Thriller','War','Western')

#Ordering genres by count
genres_only_df_adjusted <- sort(genres_only_df_adjusted)

#ANSWER
#Genre that occured MOST = Drama
#Genre that occured LEAST = Documentary
                        

#-----------------------------------------Problem 6--------------------------------------------------

#adding new column that identifies number of genres for each movie
movie_genres_merged$CountOfGenres <- rowSums(movie_genres_merged[10:28])

#number of movies with two or more genres
sum(movie_genres_merged$CountOfGenres >= 2)
nrow(movie_genres_merged[movie_genres_merged$CountOfGenres >= 2,])
#69938

#number of movies total
nrow(movie_genres_merged)
#100000

#percentage of reviews that invovled movies classified in at least two genres 
nrow(movie_genres_merged[movie_genres_merged$CountOfGenres >= 2,])/nrow(movie_genres_merged)
#0.69938
#ANSWER 69.9938% of all reviews involved at least two genres

#----------------------------------------Problem 7----------------------------------------------------

