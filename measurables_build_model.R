






#######################################################################################################################
#######################################################################################################################

# This program is for creating the POST COMBINE Measurables Draft Model.

# 1) Import the measurables table from SQL
# 2) Truncated Regression for Pct of Cap
# 3) Write pct of cap data back to SQL

# v4 is when you took out age
# v5 takes out grade projection
# v6 adds in the ranking and checking how well the ranking predicts pro salary (to compare model to model)

#######################################################################################################################
#######################################################################################################################

#######################################################################################################################
#  Library Loading
#######################################################################################################################

# Load the necessary libraries
library(VGAM)
library(RODBC)
library(caret)
library(dplyr)


#######################################################################################################################
# Import Data
#######################################################################################################################

# Establish the connection channel to the SQL Server
channel <- odbcDriverConnect('driver={SQL Server};server=RAVENS-DATA-AN;database=Analytics;uid=;pwd=')

# Create the main data table
stmt <- paste("select * from dbo.r_input_draft_model_post_combine_measurables",sep="") # SQL Statement to pull data
data_to_regress <- sqlQuery(channel,query=stmt,stringsAsFactors=F)

# close the connection channel to the SQL Server
odbcClose(channel)

# Create a copy of the data table you imported
data_to_regress_copy <- data_to_regress





data_to_regress <- data_to_regress_copy
#######################################################################################################################
#  Variable Creation
#######################################################################################################################

# Set the draft model year
draft_model_year <- max(data_to_regress$draft_year)

# Set the draft model created date
date_for_model <- max(data_to_regress$created_date)

# Set the minimum value for the Truncated Regression
model_minimum <- 0

position_for_model <- 'DS'

data_to_regress$age_at_draft_squared <- data_to_regress$age_at_draft^2
data_to_regress$height_squared <- data_to_regress$height^2
data_to_regress$weight_squared <- data_to_regress$weight^2
data_to_regress$hand_size_squared <- data_to_regress$hand_size^2
data_to_regress$arm_length_squared <- data_to_regress$arm_length^2
data_to_regress$wingspan_squared <- data_to_regress$wingspan^2
data_to_regress$broad_jump_squared <- data_to_regress$broad_jump^2
data_to_regress$vertical_jump_squared <- data_to_regress$vertical_jump^2
data_to_regress$forty_dash_squared <- data_to_regress$forty_dash^2
data_to_regress$twenty_split_squared <- data_to_regress$twenty_split^2
data_to_regress$ten_split_squared <- data_to_regress$ten_split^2
data_to_regress$flying_twenty_squared <- data_to_regress$flying_twenty^2
data_to_regress$flying_ten_squared <- data_to_regress$flying_ten^2
data_to_regress$short_shuttle_squared <- data_to_regress$short_shuttle^2
data_to_regress$long_shuttle_squared <- data_to_regress$long_shuttle^2
data_to_regress$three_cone_squared <- data_to_regress$three_cone^2
data_to_regress$bench_reps_squared <- data_to_regress$bench_reps^2
data_to_regress$bench_work_squared <- data_to_regress$bench_work^2
data_to_regress$wonderlic_squared <- data_to_regress$wonderlic^2

data_to_regress$height_pass <- ifelse(data_to_regress$height >=71,1,0)
data_to_regress$weight_pass <- ifelse(data_to_regress$weight >=205,1,0)
data_to_regress$hand_size_pass <- ifelse(data_to_regress$hand_size >=9.375,1,0)
data_to_regress$arm_length_pass <- ifelse(data_to_regress$arm_length >=31.5,1,0)
data_to_regress$forty_dash_pass <- ifelse(data_to_regress$forty_dash_vs_expected < 0,1,0)
data_to_regress$twenty_split_pass <- ifelse(data_to_regress$twenty_split_vs_expected < 0,1,0)
data_to_regress$ten_split_pass <- ifelse(data_to_regress$ten_split_vs_expected < 0,1,0)
data_to_regress$flying_twenty_pass <- ifelse(data_to_regress$flying_twenty_vs_expected < 0,1,0)
data_to_regress$flying_ten_pass <- ifelse(data_to_regress$flying_ten_vs_expected < 0,1,0)
data_to_regress$short_shuttle_pass <- ifelse(data_to_regress$short_shuttle_vs_expected < 0,1,0)
data_to_regress$three_cone_pass <- ifelse(data_to_regress$three_cone_vs_expected < 0,1,0)
data_to_regress$vertical_jump_pass <- ifelse(data_to_regress$vertical_jump_vs_expected > 0,1,0)
data_to_regress$broad_jump_pass <- ifelse(data_to_regress$broad_jump_vs_expected > 0,1,0)
data_to_regress$bench_reps_pass <- ifelse(data_to_regress$bench_reps_vs_expected > 0,1,0)
data_to_regress$bench_work_pass <- ifelse(data_to_regress$bench_work_vs_expected > 0,1,0)
data_to_regress$wonderlic_pass <- ifelse(data_to_regress$wonderlic > 24,1,0)


#######################################################################################################################
# Filter the data
#######################################################################################################################

# Draft model year filter
data_to_regress <- filter(data_to_regress, draft_model_year == max(data_to_regress$draft_year))

# Draft model date filter
data_to_regress <- filter(data_to_regress, created_date == date_for_model)

# Position Filter
data_to_regress <- filter(data_to_regress, position == position_for_model 
                          #| position == 'DC_SLOT'
                          )

# Age Filter
data_to_regress <- filter(data_to_regress, age_at_draft < 29)

# Season in League Filter
data_to_regress <- filter(data_to_regress, season_in_league == 1)

# Entry Year Filter
data_to_regress <- filter(data_to_regress, draft_year < draft_model_year - 1)

#Y Variable Filter
data_to_regress <- filter(data_to_regress,!is.na(pct_of_cap))

#Alternate data Filter
data_to_regress <- filter(data_to_regress,alternate_data == 0)

# Weight Filter
#data_to_regress <- filter(data_to_regress, weight <= 250)

# Basic X Variable filter
data_to_regress <- filter(data_to_regress,!is.na(height))
data_to_regress <- filter(data_to_regress,!is.na(weight))
data_to_regress <- filter(data_to_regress,!is.na(hand_size))
data_to_regress <- filter(data_to_regress,!is.na(arm_length))

data_to_regress <- filter(data_to_regress,!is.na(broad_jump_vs_expected))
data_to_regress <- filter(data_to_regress,!is.na(vertical_jump_vs_expected))
data_to_regress <- filter(data_to_regress,!is.na(bench_work_vs_expected))
data_to_regress <- filter(data_to_regress,!is.na(forty_dash_vs_expected))
data_to_regress <- filter(data_to_regress,!is.na(twenty_split_vs_expected))
data_to_regress <- filter(data_to_regress,!is.na(ten_split_vs_expected))
data_to_regress <- filter(data_to_regress,!is.na(flying_twenty_vs_expected))
data_to_regress <- filter(data_to_regress,!is.na(flying_ten_vs_expected))
data_to_regress <- filter(data_to_regress,!is.na(three_cone_vs_expected))
#data_to_regress <- filter(data_to_regress,!is.na(long_shuttle_vs_expected))
data_to_regress <- filter(data_to_regress,!is.na(short_shuttle_vs_expected))


#######################################################################################################################
# Basic Data Exploration - Pct of Cap
#######################################################################################################################

# Summary and Structure
summary(data_to_regress)
str(data_to_regress)

# Plot the data
for_plotting <- subset(data_to_regress, select = c("height","pct_of_cap"))
histogram(for_plotting$height, breaks=25, freq=TRUE)
plot(for_plotting)

modela<-lm(pct_of_cap~height,data=for_plotting)
abline(modela)
summary(modela)


#######################################################################################################################
# Partition Data
#######################################################################################################################

# Partition data: 80% of the data to train the model, 20% to test
set.seed(907)
train <- createDataPartition(y=data_to_regress$pct_of_cap, p=0.80, list=FALSE)

training <- data_to_regress[train,]
#training <- data_to_regress #For fitting final model once you use holdout set to evaluate
testing <- data_to_regress[-train,]


######################################################################################################################
# Truncated Regression Model - Pct of Cap
#######################################################################################################################

#Regression Variable List
regression_variable_list <- c('height'
                              ,'height_squared'
                              ,'weight'
                              #,'weight_squared'
                              ,'hand_size'
                              #,'hand_size_squared'
                              ,'arm_length'
                              #,'arm_length_squared'
                              ,'vertical_jump_pass'
                              ,'bench_work_pass'
                              ,'forty_dash_vs_expected'
                              #,'twenty_split_pass'
                              #,'ten_split_vs_expected'
                              #,'flying_twenty_vs_expected'
                              #,'flying_ten_vs_expected'
                              #,'long_shuttle_vs_expected'
                              #,'short_shuttle_vs_expected'
                              ,'three_cone_vs_expected'
)

#Create the y variable for the model
y <- training$pct_of_cap

#Create the (x) variables for the model and remove NA rows (they cause issues)
x <- as.matrix(training[,c(regression_variable_list)])

for (i in 1:length(regression_variable_list))
{
    eliminate <- !is.na(x[,regression_variable_list[i]])
    x <- x[eliminate,]
    y <- y[eliminate]
}


# Truncated Regression
model <- vglm(y ~ x
              ,cennormal(zero = 2)
              ,trace = FALSE
              ,maxit=500
              ,extra = list(leftcensored = (y <= model_minimum)
              )
)

summary(model)


#######################################################################################################################
# Save the Model to an external file - Pct of Cap
#######################################################################################################################

#save(file=paste("X:/R/college_scouting/draft_modeling/post_combine/measurables/2019/model/model_pct_of_cap_",position_for_model,sep=''),model)
#load(file=paste("X:/R/college_scouting/draft_modeling/post_combine/measurables/2019/model/model_pct_of_cap_",position_for_model,sep=''))


#######################################################################################################################
# Predict values - Pct of Cap
#######################################################################################################################

# The second coefficient is the standard deviation - need to skip it.
coefficients <- coef(model)
coefficients <- c(coefficients[1],coefficients[3:length(coefficients)])

# Select with dataset you want to predict for
predictions <- testing
#predictions <- data_to_regress

# Put identifying info into variables so you can add it to prediction results later
predictions_ids <- predictions$bane_player_id
predictions_names <- predictions$player
predictions_draft_year <- predictions$draft_year
predictions_season_in_league <- predictions$season_in_league
predictions_age <- predictions$age
predictions_height <- predictions$height
predictions_weight <- predictions$weight
predictions_forty_time <- predictions$forty_time
predictions_pro_grade <- predictions$pro_grade
predictions_pct_of_cap <- predictions$pct_of_cap
predictions_position <- predictions$position

# Add a column with the value of 1 to mulitply by the intercept
predictions <- as.matrix(cbind(1,predictions[,regression_variable_list]))

# Calculate predicted values
predictions_values <- (predictions%*%(coefficients))

# Convert data set back into a data frame so you can add a column
predictions <- as.data.frame(predictions)

# Add the predicted value column to the data table
predictions$model_score <- predictions_values

#Rank the model scores
predictions$model_score_rank <- dense_rank(desc(predictions$model_score))

# Add the identifying info back in
predictions$bane_player_id <- predictions_ids
predictions$player <- predictions_names
predictions$draft_year <- predictions_draft_year
predictions$season_in_league <- predictions_season_in_league
predictions$age <- predictions_age
predictions$height <- predictions_height
predictions$weight <- predictions_weight
predictions$forty_time <- predictions_forty_time
predictions$pro_grade <- predictions_pro_grade
predictions$pct_of_cap <- predictions_pct_of_cap
predictions$position <- predictions_position
predictions$draft_model_position <- position_for_model
predictions$draft_model_year <- draft_model_year
predictions$secondary_position <- ifelse(predictions$position==position_for_model,0,1)
predictions$created_date <- date_for_model


#######################################################################################################################
# Calculate the residuals - Pct of Cap
#######################################################################################################################

predictions$residuals <- predictions$pct_of_cap - predictions$model_score
predictions$residuals_sqd <- predictions$residuals^2
predictions$residuals_abs <- abs(predictions$residuals)

#Plot the predictions
plot_predictions <- subset(predictions, select = c("pct_of_cap","model_score"))
plot(plot_predictions)

#Plot the residuals
histogram(predictions$residuals, breaks=25, freq=TRUE)

plot_residuals <- subset(predictions, select = c("pct_of_cap","residuals"))
plot(plot_residuals)
abline(h=0)


#######################################################################################################################
# Validation Set Results
#######################################################################################################################

#Regression of score to pct of cap
results <-lm(pct_of_cap~model_score,data=predictions)
summary(results)
#Regression of rank to pct of cap
results <-lm(pct_of_cap~model_score_rank,data=predictions)
summary(results)

#RMSE
sqrt(mean(predictions$residuals_sqd, na.rm = TRUE))

#AMSE
mean(predictions$residuals_abs, na.rm = TRUE)


#######################################################################################################################
# Write the results to an external file - Pct of Cap
#######################################################################################################################

# Write out the predictions to a csv
write.table(predictions, "X:/R/college_scouting/draft_modeling/post_combine/measurables/2019/DS.csv", sep=",")


#######################################################################################################################
# Write the new data back into an SQL table - Pct of Cap
#######################################################################################################################

# Establish the connection channel to the SQL Server
channel <- odbcDriverConnect('driver={SQL Server};server=RAVENS-DATA-AN;database=Analytics;uid=;pwd=')

#Take the necessary columns for SQL import
for_sql <- predictions %>% select(bane_player_id
                                  ,player
                                  ,position
                                  ,draft_year
                                  ,season_in_league
                                  ,draft_model_year
                                  ,draft_model_position
                                  ,model_score_type
                                  ,model_score
                                  ,secondary_position
                                  ,created_date
)

sqlSave(channel
        ,for_sql
        ,rownames=FALSE
        ,tablename="dbo.r_output_draft_model_post_combine_measurables"
        ,colname=FALSE
        ,append=TRUE
)

# close the connection channel to the SQL Server
odbcClose(channel)

