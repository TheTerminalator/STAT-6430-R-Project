
#------------------------------------------------------------------------------------------

# Problem 7

# Read in reviewers.txt as a csv file, and name the columns
reviewers_df <- read.csv(text = getURL("https://github.com/rohanbapat/STAT-6430-R-Project/blob/master/reviewers.txt"), header = F, sep = "|")
colnames(reviewers_df) <- c('ReviewerID', 'Age', 'Gender', 'Occupation', 'Zipcode')

# Merge moviereviews and reviewers together
reviews_reviewers <- merge(moviereviews_df, reviewers_df, by = "ReviewerID")

# Split the dataframe by gender
gendermean_summary_df <- split(reviews_reviewers, reviews_reviewers$Gender)

# Metrics for calculating confidence interval (male)
mean_m_reviewers <- mean(gendermean_summary_df$M$Rating)
n_m_reviewers <- nrow(gendermean_summary_df$M)
t_m_reviewers <- qt(0.975, n_m_reviewers - 1)
sd_m_reviewers <- sd(gendermean_summary_df$M$Rating)
# Upper and lower limits of 95% confidence interval for male reviewers
upper_lt_m_reviewers <- mean_m_reviewers + t_m_reviewers * sd_m_reviewers / sqrt(n_m_reviewers)
lower_lt_m_reviewers <- mean_m_reviewers - t_m_reviewers * sd_m_reviewers / sqrt(n_m_reviewers)
c(lower_lt_m_reviewers, upper_lt_m_reviewers)
# Answer: 3.521309 3.537269

# Metrics for calculating confidence interval (female)
mean_f_reviewers <- mean(gendermean_summary_df$F$Rating)
n_f_reviewers <- nrow(gendermean_summary_df$F)
t_f_reviewers <- qt(0.975, n_f_reviewers - 1)
sd_f_reviewers <- sd(gendermean_summary_df$F$Rating)
# Upper and lower limits of 95% confidence interval for female reviewers
upper_lt_f_reviewers <- mean_f_reviewers + t_f_reviewers * sd_f_reviewers / sqrt(n_f_reviewers)
lower_lt_f_reviewers <- mean_f_reviewers - t_f_reviewers * sd_f_reviewers / sqrt(n_f_reviewers)
c(lower_lt_f_reviewers, upper_lt_f_reviewers)
# Answer: 3.517202 3.545813

#------------------------------------------------------------------------------------------

# Problem 8

zipcodes_df <- read.csv(text = getURL("https://github.com/rohanbapat/STAT-6430-R-Project/blob/master/zipcodes.csv"), header = T)

# Merge reviews_reviewers and zipcodes together
reviews_reviewers_zipcodes <- merge(reviews_reviewers, zipcodes_df, by = "Zipcode")

# Find the number of reviews for each state, and order them in descending orders
statecount_summary_df <- data.frame(table(reviews_reviewers_zipcodes$State))
statecount_summary_df <- statecount_summary_df[order(-statecount_summary_df$Freq),]

# The states with top 5 most reviews
statecount_summary_df$Var1[1:5]
# Answer: CA MN NY IL TX

#------------------------------------------------------------------------------------------

# Problem 9

# Find the number of reviews for each movie, and order them in ascending orders
mvidcount_summary_df <- data.frame(table(moviereviews_df$MovieID))
mvidcount_summary_df <- mvidcount_summary_df[order(mvidcount_summary_df$Freq),]

# Find the number of movies with 1-20 reviews
reviewscount_summary_df <- data.frame(table(mvidcount_summary_df$Freq))
reviewscount_summary_df <- reviewscount_summary_df$Freq[1:20]

# Create a list to store all percentage of movies that have 1-20 reviews
mvreviews_pctg <- list()
for (i in 1:20){
  mvreviews_pctg <- c(mvreviews_pctg, reviewscount_summary_df[i] / length(mvidcount_summary_df$Var1))
}
mvreviews_pctg
# Answer: 0.08382878 0.04042806  0.03567182 0.03804994  0.03032105 (1-5 reviews)
#         0.02318668 0.02615933  0.01783591 0.0196195   0.0196195 (6-10)
#         0.01189061 0.01664685  0.01486326 0.008323424 0.01307967 (11-15)
#         0.01129608 0.005945303 0.01426873 0.01070155  0.007134364 (16-20)

# In Percentage: 8.382878% 4.042806%  3.567182% 3.804994%  3.032105% (1-5 reviews)
#                2.318668% 2.615933%  1.783591% 1.96195%   1.96195% (6-10)
#                1.189061% 1.664685%  1.486326% 0.8323424% 1.307967% (11-15)
#                1.129608% 0.5945303% 1.426873% 1.070155%  0.7134364% (16-20)

#------------------------------------------------------------------------------------------

# Problem 10

# Create a dataframe to store all genres and their corresponding average ratings
genre_rating_df <- as_data_frame(matrix(NA, nrow = 19, ncol = 2))
for (i in 10:28){
  genre_rating <- split(movie_genres_merged, movie_genres_merged[[i]])
  genremean_rating <- mean(genre_rating$`1`$Rating)
  genre_rating_df[i-9, 1] <- colnames(movie_genres_merged)[i]
  genre_rating_df[i-9, 2] <- genremean_rating
}

# Sort the dataframe based on ratings, and find the genre with highest and lowest ratings
genre_rating_df <- genre_rating_df[order(genre_rating_df$V2),]
highest_rating_genre <- genre_rating_df$V1[19]
lowest_rating_genre <- genre_rating_df$V1[1]
highest_rating_genre
lowest_rating_genre
# Answer: FilmNoir
#         unknown

#-----------------------------------------------------------------------------------------

# Problem 11

# Merge moviereviews, genres and reviewers together
movie_genres_age <- merge(movie_genres_merged, reviewers_df, by = "ReviewerID")
movie_genres_age

# Divide the dataframe into two parts based on age
is_under_30 <- subset(movie_genres_age, movie_genres_age$Age <= 30)
above_30 <- subset(movie_genres_age, movie_genres_age$Age > 30)

# For reviewers age 30 and under
genre_rating_u30_df <- as_data_frame(matrix(NA, nrow = 19, ncol = 2))
for (i in 10:28){
  genre_rating_u30 <- split(is_under_30, is_under_30[[i]])
  genremean_rating_u30 <- mean(genre_rating_u30$`1`$Rating)
  genre_rating_u30_df[i-9, 1] <- colnames(is_under_30)[i]
  genre_rating_u30_df[i-9, 2] <- genremean_rating_u30
}
genre_rating_u30_df <- genre_rating_u30_df[order(genre_rating_u30_df$V2),]
highest_rating_genre_u30 <- genre_rating_u30_df$V1[19]
lowest_rating_genre_u30 <- genre_rating_u30_df$V1[1]
highest_rating_genre_u30
lowest_rating_genre_u30
# Answer: FilmNoir
#         Fantasy

# For reviewers over 30
genre_rating_a30_df <- as_data_frame(matrix(NA, nrow = 19, ncol = 2))
for (i in 10:28){
  genre_rating_a30 <- split(above_30, above_30[[i]])
  genremean_rating_a30 <- mean(genre_rating_a30$`1`$Rating)
  genre_rating_a30_df[i-9, 1] <- colnames(above_30)[i]
  genre_rating_a30_df[i-9, 2] <- genremean_rating_a30
}
genre_rating_a30_df <- genre_rating_a30_df[order(genre_rating_a30_df$V2),]
highest_rating_genre_a30 <- genre_rating_a30_df$V1[19]
lowest_rating_genre_a30 <- genre_rating_a30_df$V1[1]
highest_rating_genre_a30
lowest_rating_genre_a30
# Answer: FilmNoir
#         unknown


