



#######################################################################################################################
#######################################################################################################################

# This program is for building the expected measurables models for all positions. You use expected measurables to 
# adjust for the different sizes players run / jump / bench at.

# 1) Import the measurables table from SQL
# 2) 10 Fold Cross Validation for Model Selection
# 3) Models done by position (and maybe eventually 3 year draft year buckets?)

#######################################################################################################################
#######################################################################################################################

#######################################################################################################################
#  Library Loading
#######################################################################################################################

# Load the necessary libraries
library(dplyr)
library(RODBC)
library(caret)


#######################################################################################################################
# Import Data
#######################################################################################################################

# Establish the connection channel to the SQL Server
channel <- odbcDriverConnect('driver={SQL Server};server=RAVENS-DATA-AN;database=Analytics;uid=;pwd=')

# Create the main data table
stmt <- paste("select * from dbo.r_input_draft_model_expected_measurables",sep="") # SQL Statement to pull data
data_to_regress <- sqlQuery(channel,query=stmt,stringsAsFactors=F)

# close the connection channel to the SQL Server
odbcClose(channel)

# Create a copy of the data table you imported
data_to_regress_copy <- data_to_regress


#######################################################################################################################
#  Variable Creation
#######################################################################################################################

# Set the draft model year
draft_model_year <- max(data_to_regress$draft_model_year)

# Set the draft model created date
date_for_model <- max(data_to_regress$created_date)

# Create squared variables
data_to_regress$age_at_draft_squared <- data_to_regress$age_at_draft^2
data_to_regress$height_squared <- data_to_regress$height^2
data_to_regress$weight_squared <- data_to_regress$weight^2
data_to_regress$weight_cubed <- data_to_regress$weight^3
data_to_regress$arm_length_squared <- data_to_regress$arm_length^2
data_to_regress$forty_dash_squared <- data_to_regress$forty_dash^2
data_to_regress$twenty_split_squared <- data_to_regress$twenty_split^2
data_to_regress$ten_split_squared <- data_to_regress$ten_split^2


#######################################################################################################################
# Filter the data
#######################################################################################################################

# Created date filter
data_to_regress <- filter(data_to_regress, created_date == date_for_model) 

# X Variable Filter
#data_to_regress <- filter(data_to_regress,!is.na(age_at_draft))
data_to_regress <- filter(data_to_regress,!is.na(height))
data_to_regress <- filter(data_to_regress,!is.na(weight))
#data_to_regress <- filter(data_to_regress,!is.na(arm_length))
data_to_regress <- filter(data_to_regress,!is.na(forty_dash))
#data_to_regress <- filter(data_to_regress,!is.na(flying_ten))
#data_to_regress <- filter(data_to_regress,!is.na(twenty_split))
data_to_regress <- filter(data_to_regress,!is.na(short_shuttle))

# Age Filter
data_to_regress <- filter(data_to_regress, age_at_draft < 29)

# Take out 2019 for model building
data_to_regress <- filter(data_to_regress, draft_year < draft_model_year)

data_to_regress <- filter(data_to_regress, bane_player_id != 4106)
data_to_regress <- filter(data_to_regress, bane_player_id != 211715)
data_to_regress <- filter(data_to_regress, bane_player_id != 47125)
data_to_regress <- filter(data_to_regress, bane_player_id != 7271)
data_to_regress <- filter(data_to_regress, bane_player_id != 10895)


#######################################################################################################################
#  Model parameters
#######################################################################################################################

# Define training control (for repeated k-fold validation)
train_control <- trainControl(method="repeatedcv", number=10, repeats=5) #10 fold cross validation, 5 iterations


#######################################################################################################################
#  Model Specific Variable Creation 
#######################################################################################################################

# Create the position list looping variable
position_list <- unique(data_to_regress$position)

# Create the model type variable
measurable_type <- 'fill_in_short_shuttle'
measurable_code <- 'short_shuttle'


#######################################################################################################################
# Model Details
#######################################################################################################################


# Filter so it is only Combine runs
data_to_regress_model <- filter(data_to_regress, adjusted_priority == 2)
#data_to_regress_model <- filter(data_to_regress_model, attempt == 1)

# Filter by position
data_to_regress_model$position <- ifelse(data_to_regress_model$position == 'OC','OG',data_to_regress_model$position)
data_to_regress_model$position <- ifelse(data_to_regress_model$position == 'DC_SLOT','DC',data_to_regress_model$position)
data_to_regress_model$position <- ifelse(data_to_regress_model$position == 'WR_SLOT','WR',data_to_regress_model$position)

position_for_model <- 'DS'
data_to_regress_model <- filter(data_to_regress_model, position == position_for_model)

# Filter by year
#year_for_model <- 2017
#data_to_regress_model <- filter(data_to_regress_model, draft_year >= year_for_model - 2 & draft_year <= year_for_model)


#######################################################################################################################
# Basic Data Exploration
#######################################################################################################################

# Summary and Structure
summary(data_to_regress)
str(data_to_regress)

# Plot the data
for_plotting <- subset(data_to_regress_model, select = c("weight",measurable_code))
#histogram(for_plotting$weight, breaks=25, freq=TRUE)
#plot(for_plotting)

modela<-lm(short_shuttle~weight,data=for_plotting)
#abline(modela)
summary(modela)


#######################################################################################################################
# Regression Model - 10 Fold Cross Validation
#######################################################################################################################


model <- train(short_shuttle ~ 
                 weight
               #+ weight_squared
               + height
               #+ height_squared
               + forty_dash
               , data=data_to_regress_model
               , trControl=train_control
               , method="lm"
               #, preProcess=c('center','scale')
               #, subset=(adjusted_priority==1)
)

summary(model)


#######################################################################################################################
# Save the Model to an external file
#######################################################################################################################

save(file=paste('X:/R/college_scouting/draft_modeling/post_combine/expected_measurables/2019/model/model_',measurable_type,'_',position_for_model,sep=''),model)
#save(file=paste('X:/R/college_scouting/draft_modeling/post_combine/expected_measurables/2019/model/model_',measurable_type,'_',position_for_model,'_',year_for_model,sep=''),model)



