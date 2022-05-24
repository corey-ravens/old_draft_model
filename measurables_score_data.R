



#######################################################################################################################
#######################################################################################################################

# This program is for creating the POST COMBINE Measurables Draft Model. 

# 1) Import the measurables table from SQL
# 2) Truncated Regression for Pct of Cap
# 3) Write pct of cap data back to SQL

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
#  Variable Creation - For Both Models
#######################################################################################################################

# Set the draft model year
draft_model_year <- max(data_to_regress$draft_model_year)

# Set the draft model created date
date_for_model <- max(data_to_regress$created_date)

model_score_type <- 'pct_of_cap'

position_for_model <- 'QB'


# Use the fill in short shuttle and 3 cone values for when players don't have them for real
data_to_regress$short_shuttle_vs_expected <- ifelse(is.na(data_to_regress$short_shuttle_vs_expected),data_to_regress$fill_in_short_shuttle_vs_expected,data_to_regress$short_shuttle_vs_expected)
data_to_regress$three_cone_vs_expected <- ifelse(is.na(data_to_regress$three_cone_vs_expected),data_to_regress$fill_in_three_cone_vs_expected,data_to_regress$three_cone_vs_expected)
data_to_regress$short_shuttle <- ifelse(is.na(data_to_regress$short_shuttle),data_to_regress$fill_in_short_shuttle,data_to_regress$short_shuttle)
data_to_regress$three_cone <- ifelse(is.na(data_to_regress$three_cone),data_to_regress$fill_in_three_cone,data_to_regress$three_cone)

# Follow up for guys that have neither an actual NOR a fill in value
data_to_regress$short_shuttle_vs_expected <- ifelse(is.na(data_to_regress$short_shuttle_vs_expected),0,data_to_regress$short_shuttle_vs_expected)
data_to_regress$three_cone_vs_expected <- ifelse(is.na(data_to_regress$three_cone_vs_expected),0,data_to_regress$three_cone_vs_expected)
data_to_regress$forty_dash_vs_expected <- ifelse(is.na(data_to_regress$forty_dash_vs_expected),0,data_to_regress$forty_dash_vs_expected)
data_to_regress$twenty_split_vs_expected <- ifelse(is.na(data_to_regress$twenty_split_vs_expected),0,data_to_regress$twenty_split_vs_expected)
data_to_regress$ten_split_vs_expected <- ifelse(is.na(data_to_regress$ten_split_vs_expected),0,data_to_regress$ten_split_vs_expected)
data_to_regress$flying_twenty_vs_expected <- ifelse(is.na(data_to_regress$flying_twenty_vs_expected),0,data_to_regress$flying_twenty_vs_expected)
data_to_regress$flying_ten_vs_expected <- ifelse(is.na(data_to_regress$flying_ten_vs_expected),0,data_to_regress$flying_ten_vs_expected)

# use 0 to fill in missing benches and jumps
data_to_regress$bench_reps_vs_expected <- ifelse(is.na(data_to_regress$bench_reps_vs_expected),0,data_to_regress$bench_reps_vs_expected)
data_to_regress$bench_work_vs_expected <- ifelse(is.na(data_to_regress$bench_work_vs_expected),0,data_to_regress$bench_work_vs_expected)
data_to_regress$broad_jump_vs_expected <- ifelse(is.na(data_to_regress$broad_jump_vs_expected),0,data_to_regress$broad_jump_vs_expected)
data_to_regress$vertical_jump_vs_expected <- ifelse(is.na(data_to_regress$vertical_jump_vs_expected),0,data_to_regress$vertical_jump_vs_expected)

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

#data_to_regress$height_pass <- ifelse(data_to_regress$height >=74,1,0)
#data_to_regress$weight_pass <- ifelse(data_to_regress$weight >=255,1,0)
#data_to_regress$hand_size_pass <- ifelse(data_to_regress$hand_size >=9,1,0)
#data_to_regress$arm_length_pass <- ifelse(data_to_regress$arm_length >=32.75,1,0)
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

data_to_regress$forty_dash_pass <- ifelse(is.na(data_to_regress$forty_dash_pass) & data_to_regress$draft_year == draft_model_year,0,data_to_regress$forty_dash_pass)
data_to_regress$twenty_split_pass <- ifelse(is.na(data_to_regress$twenty_split_pass) & data_to_regress$draft_year == draft_model_year,0,data_to_regress$twenty_split_pass)
data_to_regress$ten_split_pass <- ifelse(is.na(data_to_regress$ten_split_pass) & data_to_regress$draft_year == draft_model_year,0,data_to_regress$ten_split_pass)
data_to_regress$flying_twenty_pass <- ifelse(is.na(data_to_regress$flying_twenty_pass) & data_to_regress$draft_year == draft_model_year,0,data_to_regress$flying_twenty_pass)
data_to_regress$flying_ten_pass <- ifelse(is.na(data_to_regress$flying_ten_pass) & data_to_regress$draft_year == draft_model_year,0,data_to_regress$flying_ten_pass)


#######################################################################################################################
# Filter the data
#######################################################################################################################

# Draft model year filter
data_to_regress <- filter(data_to_regress, draft_model_year == max(data_to_regress$draft_year))

# Draft model date filter
data_to_regress <- filter(data_to_regress, created_date == date_for_model)

# Position Filter
data_to_regress <- filter(data_to_regress, position == 'QB' 
                          #| position == 'TE'
                          #| position == 'WR_SLOT'
                          #| position == 'RB'
                          ) #For scoring

# Season in League Filter
data_to_regress <- filter(data_to_regress, season_in_league == 1)


#######################################################################################################################
# Load the Model - Pct of Cap
#######################################################################################################################


load(file=paste("X:/R/college_scouting/draft_modeling/post_combine/measurables/2019/model/model_pct_of_cap_",position_for_model,sep=''))
summary(model)



#Regression Variable List
regression_variable_list <- c(
    
########## QB    
#'height'             
#,'weight'             
#,'arm_length'         
#,'ten_split_vs_expected' 
#,'flying_twenty_vs_expected'
#,'three_cone_vs_expected'

########## RB
#'height'                  
#,'weight'                
#,'hand_size'               
#,'arm_length'              
#,'broad_jump_vs_expected' 
#,'forty_dash_pass' 
#,'twenty_split'     
#,'short_shuttle'    

########## FB    
#'height'                
#,'weight'                
#,'weight_squared'        
#,'ten_split_vs_expected' 
#,'three_cone_vs_expected' 

########## WR
#'height'                
#,'height_squared'        
#,'weight'                
#,'weight_squared'        
#,'hand_size'             
#,'arm_length'            
#,'vertical_jump_vs_expected'    
#,'forty_dash_vs_expected'       
#,'twenty_split_pass'
#,'ten_split_pass'
#,'short_shuttle_vs_expected'    

########## TE
#'height'             
#,'weight'             
#,'hand_size'      
#,'vertical_jump'          
#,'bench_work'
#,'forty_dash_pass'
#,'ten_split_vs_expected'
#,'long_shuttle_vs_expected'
#,'three_cone_vs_expected'

########## OT
#'height'
#,'height_squared'
#,'weight'
#,'weight_squared'
#,'hand_size'
#,'arm_length'
#,'ten_split_vs_expected'
#,'flying_twenty_vs_expected'
#,'flying_ten_vs_expected'
#,'short_shuttle_vs_expected'

########## OG    
#'height'
#,'height_squared'
#,'weight'
#,'weight_squared'
#,'hand_size'
#,'arm_length'
#,'arm_length_squared'
#,'bench_reps_vs_expected'
#,'ten_split_vs_expected'
#,'flying_ten_vs_expected'
#,'short_shuttle_vs_expected'

########## OC
#'height'
#,'weight'
#,'weight_squared'
#,'hand_size'
#,'arm_length'
#,'arm_length_squared'
#,'flying_ten_vs_expected'
#,'short_shuttle_vs_expected'

########## DT
#'height'
#,'height_squared'
#,'weight'
#,'weight_squared'
#,'arm_length'
#,'vertical_jump_vs_expected'
#,'bench_reps_pass'
#,'twenty_split_vs_expected'
#,'ten_split_pass'
#,'three_cone_vs_expected'

########## DE
#'height'
#,'weight'
#,'weight_squared'
#,'arm_length'
#,'forty_dash_pass'
#,'ten_split_vs_expected'
#,'three_cone_vs_expected'

########## OB
#'height'
#,'weight'
#,'weight_squared'
#,'hand_size'
#,'arm_length'
#,'vertical_jump_vs_expected'
#,'bench_reps_vs_expected'
#,'forty_dash_vs_expected'
#,'ten_split_pass'
#,'three_cone_vs_expected'

########## IB
#'height'
#,'weight'
#,'hand_size'
#,'arm_length'
#,'twenty_split'
#,'ten_split_pass'
#,'flying_twenty'
#,'short_shuttle'

########## DC
#'height'
#,'height_squared'
#,'weight'
#,'weight_squared'
#,'hand_size'
#,'arm_length'
#,'broad_jump_vs_expected'
#,'twenty_split_pass'
#,'flying_ten_vs_expected'
#,'three_cone_vs_expected'

########## DS
#'height'
#,'height_squared'
#,'weight'
#,'hand_size'
#,'arm_length'
#,'vertical_jump_pass'
#,'bench_work_pass'
#,'forty_dash_vs_expected'
#,'three_cone_vs_expected'

)


#######################################################################################################################
# Predict values - Pct of Cap
#######################################################################################################################

# The second coefficient is the standard deviation - need to skip it.
coefficients <- coef(model)
coefficients <- c(coefficients[1],coefficients[3:length(coefficients)])

# Select with dataset you want to predict for
predictions <- data_to_regress

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
predictions$model_score_type <- model_score_type
predictions$draft_model_year <- draft_model_year
predictions$secondary_position <- ifelse(predictions$position==position_for_model,0,1)
predictions$created_date <- date_for_model


#######################################################################################################################
# Write the results to an external file
#######################################################################################################################

# Write out the predictions to a csv
#write.table(predictions, paste("X:/R/college_scouting/draft_modeling/post_combine/measurables/",position_for_model,".csv",sep=""),sep=",")


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

