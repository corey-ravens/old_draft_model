



#######################################################################################################################
#######################################################################################################################

# This program is for creating the POST COMBINE Measurables Similarity Scores.

# 1) Import the measurables table from SQL
# 2) Generate the similarity scores for both models
# 3) Write the scores back to SQL

#V2 IS FOR 2018
#v3 adds the position filter on the target player

#######################################################################################################################
#######################################################################################################################

#######################################################################################################################
#  Library Loading
#######################################################################################################################

# Load the necessary libraries
library(dplyr)
library(RODBC)
library(VGAM)
library(data.table)


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


#######################################################################################################################
# Filter the data - For Both Models
#######################################################################################################################

# Position Filter
data_to_regress <- subset(data_to_regress, !(position %in% c('PK','PT','LS')))
data_to_regress <- filter(data_to_regress,!is.na(position))

# Season in league filter
data_to_regress <- filter(data_to_regress, season_in_league == 1)

# Draft model year filter
draft_model_year <- max(data_to_regress$draft_year)
data_to_regress <- filter(data_to_regress, draft_model_year == max(data_to_regress$draft_year))

# Date created filter
date_for_model <- max(data_to_regress$created_date)
data_to_regress <- filter(data_to_regress, created_date == date_for_model)


#######################################################################################################################
#  Variable Creation - For Both Models
#######################################################################################################################

# Create the position list looping variable
position_list <- unique(data_to_regress$position)
position_list <- c('QB','RB','FB','WR','TE','OT','OG','OC','DT','DE','OB','IB','DC','DS')

# Use the fill in short shuttle and 3 cone values for when players don't have them for real
data_to_regress$short_shuttle_vs_expected <- ifelse(is.na(data_to_regress$short_shuttle_vs_expected),data_to_regress$fill_in_short_shuttle_vs_expected,data_to_regress$short_shuttle_vs_expected)
data_to_regress$three_cone_vs_expected <- ifelse(is.na(data_to_regress$three_cone_vs_expected),data_to_regress$fill_in_three_cone_vs_expected,data_to_regress$three_cone_vs_expected)
data_to_regress$short_shuttle <- ifelse(is.na(data_to_regress$short_shuttle),data_to_regress$fill_in_short_shuttle,data_to_regress$short_shuttle)
data_to_regress$three_cone <- ifelse(is.na(data_to_regress$three_cone),data_to_regress$fill_in_three_cone,data_to_regress$three_cone)

# Follow up for guys that have neither an actual OR a fill in value
data_to_regress$short_shuttle_vs_expected <- ifelse(is.na(data_to_regress$short_shuttle_vs_expected),0,data_to_regress$short_shuttle_vs_expected)
data_to_regress$three_cone_vs_expected <- ifelse(is.na(data_to_regress$three_cone_vs_expected),0,data_to_regress$three_cone_vs_expected)

# use 0 to fill in missing benches and jumps
data_to_regress$bench_reps_vs_expected <- ifelse(is.na(data_to_regress$bench_reps_vs_expected),0,data_to_regress$bench_reps_vs_expected)
data_to_regress$bench_work_vs_expected <- ifelse(is.na(data_to_regress$bench_work_vs_expected),0,data_to_regress$bench_work_vs_expected)
data_to_regress$broad_jump_vs_expected <- ifelse(is.na(data_to_regress$broad_jump_vs_expected),0,data_to_regress$broad_jump_vs_expected)
data_to_regress$vertical_jump_vs_expected <- ifelse(is.na(data_to_regress$vertical_jump_vs_expected),0,data_to_regress$vertical_jump_vs_expected)

data_to_regress$height_squared <- data_to_regress$height^2
data_to_regress$weight_squared <- data_to_regress$weight^2
data_to_regress$hand_size_squared <- data_to_regress$hand_size^2
data_to_regress$arm_length_squared <- data_to_regress$arm_length^2
data_to_regress$wingspan_squared <- data_to_regress$wingspan^2

data_to_regress$hand_size_pass <- ifelse(data_to_regress$hand_size >=9,1,0)
data_to_regress$arm_length_pass <- ifelse(data_to_regress$arm_length >=32.75,1,0)
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
# Basic Data Exploration - For Both Models
#######################################################################################################################

# Summary and Structure
summary(data_to_regress)
str(data_to_regress)


#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

# Similarity Scores Section

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

#######################################################################################################################
# Assign Model Type 
#######################################################################################################################

#Set which model type you are running
similarity_score_type <- 'pct_of_cap'

# Set the number of end columns to exclude from coefficients (for pct of cap the 3 dummy season vars don't exist)
dummy_coefs <- ifelse(similarity_score_type=='pro_grade',3,0)


#######################################################################################################################
# Loop through the data to generate scores for each target player
#######################################################################################################################

# Loop through the positions
for(p in 1:length(position_list)){
    
    position_for_model <- position_list[p]

    # Load the model for the current position
    load(file=paste("X:/R/college_scouting/draft_modeling/post_combine/measurables/2019/model/model_",similarity_score_type,'_',position_for_model,sep=''))

    # Get the model coefficients
    coefficients <- coef(model)

    # The 1st coef is intercept, the 2nd is the standard deviation, the last 3 are dummy season variables (if pro grades model being run) - remove them.
    coefficients <- c(coefficients[3:(length(coefficients)-dummy_coefs)])
    
    # Get the names of the coefficients
    coefficients_names <- names(coefficients)

    # Filter the data by position
    if(position_for_model == 'QB'){
        data_by_position <- filter(data_to_regress, position == 'QB')
        data_by_position <- filter(data_by_position,!is.na(height))
        data_by_position <- filter(data_by_position,!is.na(weight))                     
        data_by_position <- filter(data_by_position,!is.na(arm_length))
        data_by_position <- filter(data_by_position,!is.na(ten_split_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(flying_twenty_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(three_cone_vs_expected))
    }
    else if(position_for_model == 'RB'){
        data_by_position <- filter(data_to_regress, position == 'WR' | position == 'RB' | position == 'FB' | position == 'WR_SLOT')
        data_by_position <- filter(data_by_position,!is.na(height))              
        data_by_position <- filter(data_by_position,!is.na(weight))                
        data_by_position <- filter(data_by_position,!is.na(hand_size))               
        data_by_position <- filter(data_by_position,!is.na(arm_length))              
        data_by_position <- filter(data_by_position,!is.na(broad_jump_vs_expected)) 
        data_by_position <- filter(data_by_position,!is.na(forty_dash_pass)) 
        data_by_position <- filter(data_by_position,!is.na(twenty_split))     
        data_by_position <- filter(data_by_position,!is.na(short_shuttle))
    }
    else if(position_for_model == 'FB'){
        data_by_position <- filter(data_to_regress, position == 'RB' | position == 'FB')
        data_by_position <- filter(data_by_position,!is.na(height))                
        data_by_position <- filter(data_by_position,!is.na(weight))                
        data_by_position <- filter(data_by_position,!is.na(weight_squared))        
        data_by_position <- filter(data_by_position,!is.na(ten_split_vs_expected)) 
        data_by_position <- filter(data_by_position,!is.na(three_cone_vs_expected)) 
    }
    else if(position_for_model == 'WR'){
        data_by_position <- filter(data_to_regress, position == 'WR' | position == 'RB' | position == 'TE' | position == 'WR_SLOT')
        data_by_position <- filter(data_by_position,!is.na(height))                
        data_by_position <- filter(data_by_position,!is.na(height_squared))        
        data_by_position <- filter(data_by_position,!is.na(weight))                
        data_by_position <- filter(data_by_position,!is.na(weight_squared))        
        data_by_position <- filter(data_by_position,!is.na(hand_size))             
        data_by_position <- filter(data_by_position,!is.na(arm_length))            
        data_by_position <- filter(data_by_position,!is.na(vertical_jump_vs_expected))    
        data_by_position <- filter(data_by_position,!is.na(forty_dash_vs_expected))       
        data_by_position <- filter(data_by_position,!is.na(twenty_split_pass))
        data_by_position <- filter(data_by_position,!is.na(ten_split_pass))
        data_by_position <- filter(data_by_position,!is.na(short_shuttle_vs_expected))    
    }
    else if(position_for_model == 'TE'){
        data_by_position <- filter(data_to_regress, position == 'TE' | position == 'WR' | position == 'WR_SLOT')
        data_by_position <- filter(data_by_position,!is.na(height))
        data_by_position <- filter(data_by_position,!is.na(weight))
        data_by_position <- filter(data_by_position,!is.na(hand_size))
        data_by_position <- filter(data_by_position,!is.na(vertical_jump))
        data_by_position <- filter(data_by_position,!is.na(bench_work))
        data_by_position <- filter(data_by_position,!is.na(forty_dash_pass))
        data_by_position <- filter(data_by_position,!is.na(ten_split_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(long_shuttle_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(three_cone_vs_expected))
    }
    else if(position_for_model == 'OT'){
        data_by_position <- filter(data_to_regress, position == 'OT' | position == 'OG' | position == 'OC')
        data_by_position <- filter(data_by_position,!is.na(height))
        data_by_position <- filter(data_by_position,!is.na(height_squared))
        data_by_position <- filter(data_by_position,!is.na(weight))
        data_by_position <- filter(data_by_position,!is.na(weight_squared))
        data_by_position <- filter(data_by_position,!is.na(hand_size))
        data_by_position <- filter(data_by_position,!is.na(arm_length))
        data_by_position <- filter(data_by_position,!is.na(ten_split_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(flying_twenty_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(flying_ten_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(short_shuttle_vs_expected))
    }
    else if(position_for_model == 'OG'){
        data_by_position <- filter(data_to_regress, position == 'OT' | position == 'OG' | position == 'OC')
        data_by_position <- filter(data_by_position,!is.na(height))
        data_by_position <- filter(data_by_position,!is.na(height_squared))
        data_by_position <- filter(data_by_position,!is.na(weight))
        data_by_position <- filter(data_by_position,!is.na(weight_squared))
        data_by_position <- filter(data_by_position,!is.na(hand_size))
        data_by_position <- filter(data_by_position,!is.na(arm_length))
        data_by_position <- filter(data_by_position,!is.na(arm_length_squared))
        data_by_position <- filter(data_by_position,!is.na(bench_reps_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(ten_split_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(flying_ten_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(short_shuttle_vs_expected))        
    }
    else if(position_for_model == 'OC'){
        data_by_position <- filter(data_to_regress, position == 'OT' | position == 'OG' | position == 'OC')
        data_by_position <- filter(data_by_position,!is.na(height))
        data_by_position <- filter(data_by_position,!is.na(weight))
        data_by_position <- filter(data_by_position,!is.na(weight_squared))
        data_by_position <- filter(data_by_position,!is.na(hand_size))
        data_by_position <- filter(data_by_position,!is.na(arm_length))
        data_by_position <- filter(data_by_position,!is.na(arm_length_squared))
        data_by_position <- filter(data_by_position,!is.na(flying_ten_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(short_shuttle_vs_expected))
    }
    else if(position_for_model == 'DT'){
        data_by_position <- filter(data_to_regress, position == 'DT' | position == 'DE')
        data_by_position <- filter(data_by_position,!is.na(height))
        data_by_position <- filter(data_by_position,!is.na(height_squared))
        data_by_position <- filter(data_by_position,!is.na(weight))
        data_by_position <- filter(data_by_position,!is.na(weight_squared))
        data_by_position <- filter(data_by_position,!is.na(arm_length))
        data_by_position <- filter(data_by_position,!is.na(vertical_jump_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(bench_reps_pass))
        data_by_position <- filter(data_by_position,!is.na(twenty_split_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(ten_split_pass))
        data_by_position <- filter(data_by_position,!is.na(three_cone_vs_expected))
    }
    else if(position_for_model == 'DE'){
        data_by_position <- filter(data_to_regress, position == 'DE' | position == 'DT' | position == 'OB')
        data_by_position <- filter(data_by_position,!is.na(height))
        data_by_position <- filter(data_by_position,!is.na(weight))
        data_by_position <- filter(data_by_position,!is.na(weight_squared))
        data_by_position <- filter(data_by_position,!is.na(arm_length))
        data_by_position <- filter(data_by_position,!is.na(forty_dash_pass))
        data_by_position <- filter(data_by_position,!is.na(ten_split_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(three_cone_vs_expected))
    }
    else if(position_for_model == 'OB'){
        data_by_position <- filter(data_to_regress, position == 'OB' | position == 'DE' | position == 'IB')
        data_by_position <- filter(data_by_position,!is.na(height))
        data_by_position <- filter(data_by_position,!is.na(weight))
        data_by_position <- filter(data_by_position,!is.na(weight_squared))
        data_by_position <- filter(data_by_position,!is.na(hand_size))
        data_by_position <- filter(data_by_position,!is.na(arm_length))
        data_by_position <- filter(data_by_position,!is.na(vertical_jump_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(bench_reps_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(forty_dash_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(ten_split_pass))
        data_by_position <- filter(data_by_position,!is.na(three_cone_vs_expected))
    }    
    else if(position_for_model == 'IB'){
        data_by_position <- filter(data_to_regress, position == 'IB' | position == 'OB')
        data_by_position <- filter(data_by_position,!is.na(height))
        data_by_position <- filter(data_by_position,!is.na(weight))
        data_by_position <- filter(data_by_position,!is.na(hand_size))
        data_by_position <- filter(data_by_position,!is.na(arm_length))
        data_by_position <- filter(data_by_position,!is.na(twenty_split))
        data_by_position <- filter(data_by_position,!is.na(ten_split_pass))
        data_by_position <- filter(data_by_position,!is.na(flying_twenty))
        data_by_position <- filter(data_by_position,!is.na(short_shuttle))
    }
    else if(position_for_model == 'DC'){
        data_by_position <- filter(data_to_regress, position == 'DC' | position == 'DS' | position == 'DC_SLOT')
        data_by_position <- filter(data_by_position,!is.na(height))
        data_by_position <- filter(data_by_position,!is.na(height_squared))
        data_by_position <- filter(data_by_position,!is.na(weight))
        data_by_position <- filter(data_by_position,!is.na(weight_squared))
        data_by_position <- filter(data_by_position,!is.na(hand_size))
        data_by_position <- filter(data_by_position,!is.na(arm_length))
        data_by_position <- filter(data_by_position,!is.na(broad_jump_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(twenty_split_pass))
        data_by_position <- filter(data_by_position,!is.na(flying_ten_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(three_cone_vs_expected))
    }
    else if(position_for_model == 'DS'){
        data_by_position <- filter(data_to_regress, position == 'DC' | position == 'DS' | position == 'DC_SLOT')
        data_by_position <- filter(data_by_position,!is.na(height))
        data_by_position <- filter(data_by_position,!is.na(height_squared))
        data_by_position <- filter(data_by_position,!is.na(weight))
        data_by_position <- filter(data_by_position,!is.na(hand_size))
        data_by_position <- filter(data_by_position,!is.na(arm_length))
        data_by_position <- filter(data_by_position,!is.na(vertical_jump_pass))
        data_by_position <- filter(data_by_position,!is.na(bench_work_pass))
        data_by_position <- filter(data_by_position,!is.na(forty_dash_vs_expected))
        data_by_position <- filter(data_by_position,!is.na(three_cone_vs_expected))
    }
    else {
        data_by_position <- filter(data_to_regress, position == position_for_model)
    }

    # Loop through the filtered data
    for(i in 1:nrow(data_by_position)){
      
      # Create an empty dataframe for the similarity scores
      similarity_scores <- data.frame(target_bane_player_id = integer()
                                      ,comp_bane_player_id = integer()
                                      ,position = character()
                                      ,similarity_score = numeric()
                                      ,stringsAsFactors=FALSE
      )
      
        if(data_by_position[i,'draft_year'] >= draft_model_year & data_by_position[i,'position'] == position_for_model){
        
        # Loop to find the difference between target and every other player
        for (j in 1:nrow(data_by_position)){
            
            # Set the total difference variable to 0
            total_difference <- 0
            
            # Loop through the coefficients
            for (k in 1:length(coefficients)){
                
                coefficient_name <- substr(coefficients_names[k],2,nchar(coefficients_names[k]))
                
                if(j != i & data_by_position[j,'draft_year'] < draft_model_year){
                    
                    
                    #If height and height squared are in the model, perform the adjustment into one combined variable
                    if('xheight' %in% coefficients_names & 'xheight_squared' %in% coefficients_names & coefficients_names[k] %in% c('xheight','xheight_squared')){
                        
                        if(coefficients_names[k] == 'xheight'){
                        
                            optimal_holder <- (-1 * coefficients['xheight']) / (2*coefficients['xheight_squared'])
                            optimal_height <- (coefficients['xheight'] * optimal_holder) + (coefficients['xheight_squared'] * optimal_holder^2)
                            
                            target_height <- (optimal_height - coefficients['xheight']*data_by_position[i,'height']) - (coefficients['xheight_squared'] * data_by_position[i,'height_squared'])
                            comp_height <- (optimal_height - coefficients['xheight']*data_by_position[j,'height']) - (coefficients['xheight_squared'] * data_by_position[j,'height_squared'])
                            
                            if((target_height < 0 & comp_height < 0) | (target_height > 0 & comp_height > 0)){
                                
                                difference <- abs(target_height - comp_height)
                            }               
                            else{
                                
                                difference <- abs(target_height + comp_height)
                            }
                            
                            # Add the current difference to the total player difference
                            total_difference <- total_difference + difference
                        }
                    }
                    
                    #If weight and weight squared are in the model, perform the adjustment into one combined variable
                    else if('xweight' %in% coefficients_names & 'xweight_squared' %in% coefficients_names & coefficients_names[k] %in% c('xweight','xweight_squared')){
                        
                        if(coefficients_names[k] == 'xweight'){
                            
                            optimal_holder <- (-1 * coefficients['xweight']) / (2*coefficients['xweight_squared'])
                            optimal_weight <- (coefficients['xweight'] * optimal_holder) + (coefficients['xweight_squared'] * optimal_holder^2)
                            
                            target_weight <- (optimal_weight - coefficients['xweight']*data_by_position[i,'weight']) - (coefficients['xweight_squared'] * data_by_position[i,'weight_squared'])
                            comp_weight <- (optimal_weight - coefficients['xweight']*data_by_position[j,'weight']) - (coefficients['xweight_squared'] * data_by_position[j,'weight_squared'])
                            
                            if((target_weight < 0 & comp_weight < 0) | (target_weight > 0 & comp_weight > 0)){
                                
                                difference <- abs(target_weight - comp_weight)
                            }               
                            else{
                                
                                difference <- abs(target_weight + comp_weight)
                            }
                            
                            # Add the current difference to the total player difference
                            total_difference <- total_difference + difference
                        }
                    }
                    
                    #If arm length and arm length squared are in the model, perform the adjustment into one combined variable
                    else if('xarm_length' %in% coefficients_names & 'xarm_length_squared' %in% coefficients_names & coefficients_names[k] %in% c('xarm_length','xarm_length_squared')){
                        
                        if(coefficients_names[k] == 'xarm_length'){
                            
                            optimal_holder <- (-1 * coefficients['xarm_length']) / (2*coefficients['xarm_length_squared'])
                            optimal_arm_length <- (coefficients['xarm_length'] * optimal_holder) + (coefficients['xarm_length_squared'] * optimal_holder^2)
                            
                            target_arm_length <- (optimal_arm_length - coefficients['xarm_length']*data_by_position[i,'arm_length']) - (coefficients['xarm_length_squared'] * data_by_position[i,'arm_length_squared'])
                            comp_arm_length <- (optimal_arm_length - coefficients['xarm_length']*data_by_position[j,'arm_length']) - (coefficients['xarm_length_squared'] * data_by_position[j,'arm_length_squared'])
                            
                            if((target_arm_length < 0 & comp_arm_length < 0) | (target_arm_length > 0 & comp_arm_length > 0)){
                                
                                difference <- abs(target_arm_length - comp_arm_length)
                            }               
                            else{
                                
                                difference <- abs(target_arm_length + comp_arm_length)
                            }
                            
                            # Add the current difference to the total player difference
                            total_difference <- total_difference + difference
                        }
                    }
                    
                    #If hand size and hand size squared are in the model, perform the adjustment into one combined variable
                    else if('xhand_size' %in% coefficients_names & 'xhand_size_squared' %in% coefficients_names & coefficients_names[k] %in% c('xhand_size','xhand_size_squared')){
                        
                        if(coefficients_names[k] == 'xhand_size'){
                            
                            optimal_holder <- (-1 * coefficients['xhand_size']) / (2*coefficients['xhand_size_squared'])
                            optimal_hand_size <- (coefficients['xhand_size'] * optimal_holder) + (coefficients['xhand_size_squared'] * optimal_holder^2)
                            
                            target_hand_size <- (optimal_hand_size - coefficients['xhand_size']*data_by_position[i,'hand_size']) - (coefficients['xhand_size_squared'] * data_by_position[i,'hand_size_squared'])
                            comp_hand_size <- (optimal_hand_size - coefficients['xhand_size']*data_by_position[j,'hand_size']) - (coefficients['xhand_size_squared'] * data_by_position[j,'hand_size_squared'])
                            
                            if((target_hand_size < 0 & comp_hand_size < 0) | (target_hand_size > 0 & comp_hand_size > 0)){
                                
                                difference <- abs(target_hand_size - comp_hand_size)
                            }               
                            else{
                                
                                difference <- abs(target_hand_size + comp_hand_size)
                            }
                            
                            # Add the current difference to the total player difference
                            total_difference <- total_difference + difference
                        }
                    }
                    else{
                        difference <- abs(data_by_position[i,coefficient_name] - data_by_position[j,coefficient_name]) * abs(coefficients[k])
                        
                        # Add the current difference to the total player difference
                        total_difference <- (total_difference + difference)
                        
                    }
                } else{
                    difference <- NA
                    total_difference <- NA
                }
                
            } #End coefficient loop
            
            print(paste('POSITION: '
                        ,position_for_model
                        ,':  '
                        ,p
                        ,' OF '
                        ,length(position_list)
                        ,'    PLAYER: '
                        ,i
                        ,' OF '
                        ,nrow(data_by_position)
                        ,'------------------------- '
                        ,'INNER PLAYER: '
                        ,j
                        ,' OF '
                        ,nrow(data_by_position)
                        ,sep=''))   
            
            # Write the data to a new table
            similarity_scores[j,'target_bane_player_id'] <- data_by_position[i,'bane_player_id']
            similarity_scores[j,'comp_bane_player_id'] <- data_by_position[j,'bane_player_id']
            similarity_scores[j,'position'] <- position_for_model
            similarity_scores[j,'similarity_score'] <- total_difference
            
        } #End secondary player loop

        # Add in the model information
        similarity_scores$draft_model_year <- draft_model_year
        similarity_scores$similarity_score_type <- similarity_score_type

        # Remove the NA rows (usually the same target and comp player)
        similarity_scores <- filter(similarity_scores, !is.na(similarity_score))
          
        #Change the data frame to a data table
        similarity_scores <- as.data.table(similarity_scores)
        
        #Rank the similarity scores by target player
        similarity_scores[,similarity_score_rank:=rank(similarity_score, na.last = "keep", ties.method="first"),by=c("target_bane_player_id")]
        
        #Change the data table back to a data frame
        similarity_scores <- as.data.frame(similarity_scores)
        
        #######################################################################################################################
        # Write the Similarity Score data back into an SQL table
        #######################################################################################################################
        
        # Establish the connection channel to the SQL Server
        channel <- odbcDriverConnect('driver={SQL Server};server=RAVENS-DATA-AN;database=Analytics;uid=;pwd=')
        
        # Clear out old data from the table
        #sqlClear(channel,"dbo.r_output_draft_model_post_combine_measurables")
        
        #Take the necessary columns for SQL import
        for_sql <- similarity_scores %>% select(target_bane_player_id
                                                ,comp_bane_player_id
                                                ,position
                                                ,draft_model_year
                                                ,similarity_score_type
                                                ,similarity_score
                                                ,similarity_score_rank
        )
        
        sqlSave(channel
                ,for_sql
                ,rownames=FALSE
                ,tablename="dbo.r_output_draft_model_post_combine_measurables_sim_scores"
                ,colname=FALSE
                ,append=TRUE
        )
        
        # close the connection channel to the SQL Server
        odbcClose(channel)
        
        # Delete the similarity scores data frame
        rm(similarity_scores)
        
        #######################################################################################################################
        # End Writing the Similarity Score data back into an SQL table
        #######################################################################################################################

        } #End if draft_model_year entry year

    }#End loop through filtered data   

} #End loop through the positions

# Write out the predictions to a csv
#write.table(similarity_scores, "X:/R/college_scouting/draft_modeling/post_combine/measurables/sim.csv", sep=",")





