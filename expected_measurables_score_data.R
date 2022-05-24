



#######################################################################################################################
#######################################################################################################################

# This program is for generating the expected measurables for players based on models created
# in the other program

# 1) Import the measurables table from SQL
# 2) Loop through all the measurables and positions to adjust the dashes
# 3) Loop through all the measurables and positions to generate the expectations
# 4) Write the data back into SQL (do this during the loop to speed up program - 'write as you go')

#v5 adds the "fill in" measurables

#######################################################################################################################
#######################################################################################################################

#######################################################################################################################
#  Library Loading
#######################################################################################################################

# Load the necessary libraries
library(dplyr)
library(RODBC)


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

#######################################################################################################################
# Filter the data
#######################################################################################################################

# Position Filter
data_to_regress <- subset(data_to_regress, !(position %in% c('PK','PT','LS','K','ST')))
data_to_regress <- filter(data_to_regress,!is.na(position))

# Set the draft model year
draft_model_year <- max(data_to_regress$draft_model_year)
data_to_regress <- filter(data_to_regress, draft_model_year == max(data_to_regress$draft_model_year))

# Set the maximum created date
date_for_model <- max(data_to_regress$created_date)
data_to_regress <- filter(data_to_regress, created_date == date_for_model) 


#######################################################################################################################
#  Variable Creation
#######################################################################################################################

data_to_regress$age_at_draft_squared <- data_to_regress$age_at_draft^2
data_to_regress$height_squared <- data_to_regress$height^2
data_to_regress$weight_squared <- data_to_regress$weight^2
data_to_regress$arm_length_squared <- data_to_regress$arm_length^2
data_to_regress$forty_dash_squared <- data_to_regress$forty_dash^2
data_to_regress$twenty_split_squared <- data_to_regress$twenty_split^2
data_to_regress$ten_split_squared <- data_to_regress$ten_split^2

# Change the centers to guards because they use the same model
data_to_regress$position <- ifelse(data_to_regress$position == 'OC','OG',data_to_regress$position)

# Change the priority 4 and 6 to 5 because they use the same model
#data_to_regress$adjusted_priority_model <- ifelse(data_to_regress$adjusted_priority == 4,5,data_to_regress$adjusted_priority)
data_to_regress$adjusted_priority_model <- ifelse(data_to_regress$adjusted_priority %in% c(4,7),5,data_to_regress$adjusted_priority)

# Create the position list
position_list <- unique(data_to_regress$position)

adjustment_list <- c('forty_dash'
                     ,'twenty_split'
                     ,'ten_split'
                     ,'flying_twenty'
                     ,'flying_ten'
)

priority_list <- c(2,5)

measurable_list <- c('expected_forty_dash'
                     ,'expected_twenty_split'
                     ,'expected_ten_split'
                     ,'expected_flying_twenty'
                     ,'expected_flying_ten'
                     ,'expected_long_shuttle'
                     ,'expected_short_shuttle'
                     ,'expected_three_cone'
                     ,'expected_broad_jump'
                     ,'expected_vertical_jump'
                     ,'expected_bench_reps'
                     ,'expected_bench_work'
                     ,'expected_swim_right'
                     ,'expected_rip_left'
                     ,'expected_left_turn'
                     ,'expected_right_turn'
                     ,'expected_throw_speed'
                     ,'fill_in_short_shuttle'
                     ,'fill_in_three_cone'
)


#######################################################################################################################
# Loop through models to get the adjusted dash times
#######################################################################################################################

# Loop through the positions
for(p in 1:length(position_list)){
    
    position_for_model <- position_list[p]
    
    for(a in 1:length(adjustment_list)){
        
        adjustment_for_model <- adjustment_list[a]
        
        for(i in 1:length(priority_list)){
            
            priority_for_model <- priority_list[i]
    
            model_to_load <- paste('X:/R/college_scouting/draft_modeling/post_combine/adjusted_measurables/model/model_',adjustment_for_model,'_',position_for_model,'_',priority_for_model,sep='')
            
            if(file.exists(model_to_load)){
                
                load(file=model_to_load)
                
                for(j in 1:nrow(data_to_regress)){
                    
                    if(data_to_regress[j,'position'] == position_for_model & data_to_regress[j,'adjusted_priority_model'] == priority_for_model){
                        
                        row_to_score <- data_to_regress[j,]

                        data_to_regress[j,paste(adjustment_for_model,'_adjusted',sep='')] <- predict(model,row_to_score,na.action = na.pass)
                    
                    } #End if this row should get predicted by this model

                } #End loop through the dataset
                
             } #End if model exists
            
        } #End loop through priority list
        
    } #End loop through adjustment list
    
    print(p)

} #End position list loop   


#######################################################################################################################
# Loop through models to get the expected measurables
#######################################################################################################################

# Loop through the positions
for(p in 1:length(position_list)){
    
    position_for_model <- position_list[p]
    
    data_by_position <- filter(data_to_regress, position == position_for_model)
    
    for(m in 1:length(measurable_list)){
        
        measurable_for_model <- measurable_list[m]
        
        model_to_load <- paste('X:/R/college_scouting/draft_modeling/post_combine/expected_measurables/2019/model/model_',measurable_for_model,'_',position_for_model,sep='')
    
        if(file.exists(model_to_load)){
    
            load(file=model_to_load)
            
            data_by_position[,measurable_for_model] <- predict(model,data_by_position,na.action = na.pass)
            
        } #End if model exists
        
        print(paste(p,'------',position_for_model,m,'------',measurable_for_model))
        
    } #End measurable list loop
    

    if(position_for_model %in% c('QB','OT','OG','OC','DT','DE')){
        data_by_position[,'expected_long_shuttle'] <- NA
    }
    
    if(position_for_model != 'QB'){
        data_by_position[,'expected_throw_speed'] <- NA
    }
    
    if(!(position_for_model %in% c('DC','DS'))){
        data_by_position[,'expected_left_turn'] <- NA
        data_by_position[,'expected_right_turn'] <- NA
    }
    
    if(!(position_for_model %in% c('DT','DE','OB','IB'))){
        data_by_position[,'expected_rip_left'] <- NA
        data_by_position[,'expected_swim_right'] <- NA
    }
  
    # Write out the predictions to a csv
    #write.table(data_to_regress, "X:/R/college_scouting/draft_modeling/post_combine/adjusted_measurables/test2.csv", sep=",") 
    
    data_by_position$draft_model_year <- draft_model_year
    data_by_position$created_date <- date_for_model
    
    #######################################################################################################################
    # Write the new data back into an SQL table - 'write as you go'
    #######################################################################################################################
    
    # Establish the connection channel to the SQL Server
    channel <- odbcDriverConnect('driver={SQL Server};server=RAVENS-DATA-AN;database=Analytics;uid=;pwd=')
    
    #Take the necessary columns for SQL import
    for_sql <- data_by_position %>% select(workout_id
                                      ,adjusted_priority
                                      ,attempt
                                      ,expected_forty_dash
                                      ,expected_twenty_split
                                      ,expected_ten_split
                                      ,expected_flying_twenty
                                      ,expected_flying_ten
                                      ,expected_long_shuttle
                                      ,expected_short_shuttle
                                      ,expected_three_cone
                                      ,expected_broad_jump
                                      ,expected_vertical_jump
                                      ,expected_bench_reps
                                      ,expected_bench_work
                                      ,expected_swim_right
                                      ,expected_rip_left
                                      ,expected_left_turn
                                      ,expected_right_turn
                                      ,expected_throw_speed
                                      ,forty_dash_adjusted
                                      ,twenty_split_adjusted
                                      ,ten_split_adjusted
                                      ,flying_twenty_adjusted
                                      ,flying_ten_adjusted
                                      ,fill_in_short_shuttle
                                      ,fill_in_three_cone
                                      ,draft_model_year
                                      ,created_date
    )
    
    sqlSave(channel
            ,for_sql
            ,rownames=FALSE
            ,tablename='dbo.r_output_draft_model_expected_measurables'
            ,colname=FALSE
            ,append=TRUE
    )
    
    # close the connection channel to the SQL Server
    odbcClose(channel)    
    
    
    
} #End position list loop


