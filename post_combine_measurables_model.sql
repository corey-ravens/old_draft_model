


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

This is the code for creating the post combine measurables data tables.


----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Find the salary value for injured players

OUTPUT TABLES:
#temp_injured_salaries_one_row

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

DECLARE @current_season INT
SELECT @current_season = (SELECT MAX(season) FROM Analytics.dbo.map_nfl_league_year_dates WHERE GETDATE() >= regular_season_start)

-- Check if #temp_injured_salaries exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_injured_salaries') IS NOT NULL
	DROP TABLE #temp_injured_salaries

	SELECT PlayerID AS nfl_player_id
		,MAX([SalaryFit]) AS salaries
	INTO #temp_injured_salaries
	FROM [AnalyticsWork].[dbo].[rd1_players_defense_3] r1
	WHERE report_week = 6
		AND report_season = @current_season
		AND Season = (SELECT MAX(season) FROM [AnalyticsWork].[dbo].[rd1_players_defense_3] r2 WHERE r2.PlayerID = r1.PlayerID AND r2.report_week = 6 AND r2.report_season = @current_season)
	GROUP BY PlayerID


	INSERT INTO #temp_injured_salaries
	SELECT PlayerID AS nfl_player_id
		,MAX([SalaryFit]) AS salaries
	FROM [AnalyticsWork].[dbo].[rd1_players_offense_3] r1
	WHERE report_week = 6
		AND report_season = @current_season
		AND Season = (SELECT MAX(season) FROM [AnalyticsWork].[dbo].[rd1_players_offense_3] r2 WHERE r2.PlayerID = r1.PlayerID AND r2.report_week = 6 AND r2.report_season = @current_season)
	GROUP BY PlayerID


-- Check if #temp_injured_salaries_one_row exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_injured_salaries_one_row') IS NOT NULL
	DROP TABLE #temp_injured_salaries_one_row

	SELECT nfl_player_id
		,MAX(salaries) AS salaries
	INTO #temp_injured_salaries_one_row
	FROM #temp_injured_salaries
	GROUP BY nfl_player_id


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Remove players who haven't yet been drafted from the pro grades table.

OUTPUT TABLES:
#temp_pro_grades

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_pro_grades exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_pro_grades') IS NOT NULL
	DROP TABLE #temp_pro_grades

	SELECT pg.*
	INTO #temp_pro_grades
	FROM Analytics.dbo.analysis_players_pro_grades_first_4_seasons pg
	INNER JOIN BaneProductionAnalytics.dbo.players pl
		ON pg.bane_player_id = pl.id
		AND pl.is_deleted = 0
	WHERE draft_year <= @current_Season


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Add bane_player_id to the season position tables

OUTPUT TABLES:
#temp_season_position_offense
#temp_season_position_defense

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_season_position_offense exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_season_position_offense') IS NOT NULL
	DROP TABLE #temp_season_position_offense

	SELECT nfl_player_id
		,pl.id AS bane_player_id
		,season
		,receiver_type
	INTO #temp_season_position_offense
	FROM Analytics.dbo.analysis_players_season_position_offense po
	INNER JOIN BaneProductionAnalytics.dbo.players pl
		ON po.nfl_player_id = pl.nfl_id
		AND pl.is_deleted = 0
	WHERE season_type_adjusted = 'REGPOST'

-- Check if #temp_season_position_defense exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_season_position_defense') IS NOT NULL
	DROP TABLE #temp_season_position_defense

	SELECT nfl_player_id
		,pl.id AS bane_player_id
		,season
		,position_blt AS position_defense
	INTO #temp_season_position_defense
	FROM Analytics.dbo.analysis_players_season_position_defense pd
	INNER JOIN BaneProductionAnalytics.dbo.players pl
		ON pd.nfl_player_id = pl.nfl_id
		AND pl.is_deleted = 0
	WHERE season_type_adjusted = 'REGPOST'
		AND defense_type = 'ALL'


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Get down to one row of draft board position. Take a players draft
board position from as late in the process as available. 

OUTPUT TABLES:
#temp_draft_board_positions_with_order

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_draft_board_positions exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_draft_board_positions') IS NOT NULL
	DROP TABLE #temp_draft_board_positions

	SELECT player_id
		,CASE WHEN position_translation IN ('RUSH','SAM') THEN 'OB'
			WHEN position_translation IN ('FS','SS') THEN 'DS'
			ELSE position_translation
		END AS position
		,CASE WHEN UPPER(LTRIM(RTRIM([type]))) = 'PRE-DRAFT' THEN 1
			WHEN UPPER(LTRIM(RTRIM([type]))) = 'POST-APR' THEN 2
			WHEN UPPER(LTRIM(RTRIM([type]))) = 'POST-FEB' THEN 3
			WHEN UPPER(LTRIM(RTRIM([type]))) = 'POST-DEC' THEN 4
			WHEN UPPER(LTRIM(RTRIM([type]))) = 'PRE-DEC' THEN 5
			ELSE NULL
	END AS order_by
	INTO #temp_draft_board_positions
	FROM [BaneProductionAnalytics].[dbo].[draft_board_grades]
	WHERE position != '' 
		AND position IS NOT NULL
		AND player_id <> 64668
		AND position_translation NOT IN ('RS','ST')


-- Check if #temp_draft_board_positions_with_order exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_draft_board_positions_with_order') IS NOT NULL
	DROP TABLE #temp_draft_board_positions_with_order

	SELECT player_id
		,position
		,RANK() OVER (PARTITION BY player_id ORDER BY order_by) AS draft_board_position_order
	INTO #temp_draft_board_positions_with_order
	FROM #temp_draft_board_positions

/*
SELECT *
FROM #temp_draft_board_positions_with_order
*/

-- Check if #temp_draft_board_positions_latest exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_draft_board_positions_latest') IS NOT NULL
	DROP TABLE #temp_draft_board_positions_latest

	SELECT *
	INTO #temp_draft_board_positions_latest
	FROM #temp_draft_board_positions_with_order
	WHERE draft_board_position_order = 1

/*
SELECT *
FROM #temp_draft_board_positions
WHERE player_id = 232245
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Create the master DASH table for finding outlier dashes.

**Keep an eye on workouts where some times are 4 priority, some are 5. Example workout ids: 638693,638694,638696,638697,638698,638699,638700,638806,638808,638809,638810,638811

OUTPUT TABLES:
#temp_outlier_dash_master

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_outlier_dash_master exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_outlier_dash_master') IS NOT NULL
	DROP TABLE #temp_outlier_dash_master

	SELECT wo.id AS workout_id
		,wo.[description] AS workout_description
		,wo.player_id AS bane_player_id
		,pl.draft_year
		,re.code AS workout_code
		,wt.[type]
		,wo.[date] AS workout_date
		,me.measurable_type_id
		,mt.code
		,me.order_by AS attempt
		,me.[value] AS value
	INTO #temp_outlier_dash_master
	FROM [BaneProductionAnalytics].[dbo].[workouts] wo
	LEFT JOIN [BaneProductionAnalytics].[dbo].[measurables] me
		ON wo.id = me.workout_id
	LEFT JOIN [BaneProductionAnalytics].[dbo].[measurable_types] mt
		ON me.measurable_type_id = mt.id
	LEFT JOIN [BaneProductionAnalytics].[dbo].[player_workout_types] wt
		ON wo.player_workout_type_id = wt.id
	LEFT JOIN [BaneProductionAnalytics].[dbo].[reliabilities] re
		ON me.reliability_id = re.id
	LEFT JOIN [BaneProductionAnalytics].[dbo].[players] pl
		ON wo.player_id = pl.id
	WHERE re.translation IN ('I') --Only Indy, Pro Day, Combine, or Workout numbers
		AND NOT (wt.[type] = 'RPT' AND re.translation = 'I')  --Not from a scout entering the Combine numbers
		AND (wo.is_deleted = 0 OR (wo.is_deleted = 1 AND wo.[description] LIKE 'NCAA Scouting%')) --Not a deleted record
		AND NOT YEAR(wo.[date]) > pl.draft_year --Not from a workout that occured after his draft
		AND pl.draft_year >= 2005
		AND pl.draft_year <=  @current_season + 2
		AND mt.code = '40YTime'
		AND value BETWEEN 4.00 AND 7.00


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Self join the master table so you can put the electronic time alongside the two hand times.

OUTPUT TABLES:
#temp_timing_differences

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_timing_differences exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_timing_differences') IS NOT NULL
	DROP TABLE #temp_timing_differences

	SELECT m1.workout_id
		,m1.workout_description
		,m1.bane_player_id
		,m1.draft_year
		,m1.workout_code
		,m1.[type]
		,m1.workout_date
		,m1.measurable_type_id
		,m1.code
		,m1.attempt
		,m1.value AS hand_time
		,m2.value AS electronic_time
		,m1.value - m2.value AS time_difference
	INTO #temp_timing_differences
	FROM #temp_outlier_dash_master m1
	INNER JOIN #temp_outlier_dash_master m2
		ON m1.workout_id = m2.workout_id
		AND m2.attempt = 3
	WHERE m1.attempt IN (1,2)

	INSERT INTO #temp_timing_differences
	SELECT m1.workout_id
		,m1.workout_description
		,m1.bane_player_id
		,m1.draft_year
		,m1.workout_code
		,m1.[type]
		,m1.workout_date
		,m1.measurable_type_id
		,m1.code
		,m1.attempt
		,m1.value AS hand_time
		,m2.value AS electronic_time
		,m1.value - m2.value AS time_difference
	FROM #temp_outlier_dash_master m1
	INNER JOIN #temp_outlier_dash_master m2
		ON m1.workout_id = m2.workout_id
		AND m2.attempt = 6
	WHERE m1.attempt IN (4,5)


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Find the average differences

OUTPUT TABLES:
#temp_difference_avg

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_difference_avg exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_difference_avg') IS NOT NULL
	DROP TABLE #temp_difference_avg

	SELECT workout_description 
		,AVG(time_difference) AS time_difference_avg
		,STDEV(time_difference) AS time_difference_std
	INTO #temp_difference_avg
	FROM #temp_timing_differences td
	GROUP BY workout_description 


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Join the avg and stdev to the difference table so you can find the z scores.

OUTPUT TABLES:
#temp_difference_z_scores

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_difference_z_scores exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_difference_z_scores') IS NOT NULL
	DROP TABLE #temp_difference_z_scores

	SELECT td.*
		,time_difference_avg
		,time_difference_std
		,(time_difference - time_difference_avg) / time_difference_std AS time_difference_z
		,CASE WHEN (time_difference - time_difference_avg) / time_difference_std >= 2 OR (time_difference - time_difference_avg) / time_difference_std  <= -2 THEN 1 ELSE 0 END AS outlier_time
		,CONCAT(last_name,', ',goes_by) AS player
		,combine_jersey
		,sc.code AS school
	INTO #temp_difference_z_scores
	FROM #temp_timing_differences td
	INNER JOIN #temp_difference_avg av
		ON td.workout_description = av.workout_description
	INNER JOIN BaneProductionAnalytics.dbo.players pl
		ON td.bane_player_id = pl.id
		AND pl.is_deleted = 0
	INNER JOIN BaneProductionAnalytics.dbo.schools sc
		ON pl.ncaa_club_id = sc.id


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Note which players have a double outlier (both hand times in the same run off by more than 2 std devs).

OUTPUT TABLES:
#temp_outlier_list

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_outlier_list exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_outlier_list') IS NOT NULL
	DROP TABLE #temp_outlier_list

	SELECT workout_id
		,workout_description
		,bane_player_id
		,player
		,combine_jersey
		,school
		,SUM(CASE WHEN attempt IN (1,2) THEN outlier_time ELSE 0 END) AS outlier_first_run
		,SUM(CASE WHEN attempt IN (4,5) THEN outlier_time ELSE 0 END) AS outlier_second_run
	INTO #temp_outlier_list
	FROM #temp_difference_z_scores
	GROUP BY workout_id
		,workout_description
		,bane_player_id
		,player
		,combine_jersey
		,school


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Remove the electronic dash outliers from the measurables table.

OUTPUT TABLES:
#temp_measurables

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_measurables exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_measurables') IS NOT NULL
	DROP TABLE #temp_measurables

	SELECT me.id
		,me.measurable_type_id
		,CASE WHEN me.order_by = 3 AND ol.outlier_first_run = 2 AND mt.code IN ('40YTime','20YTime','10YTime') THEN NULL
			WHEN me.order_by = 6 AND ol.outlier_second_run = 2 AND mt.code IN ('40YTime','20YTime','10YTime') THEN NULL
			ELSE me.[value]
		END AS [value]
		,me.workout_id
		,me.created_at
		,me.updated_at
		,me.imported_with_errors
		,me.import_key
		,me.order_by
		,me.reliability_id
	INTO #temp_measurables
	FROM [BaneProductionAnalytics].[dbo].[measurables] me
	INNER JOIN [BaneProductionAnalytics].[dbo].[measurable_types] mt
		ON me.measurable_type_id = mt.id
	LEFT JOIN #temp_outlier_list ol
		ON me.workout_id = ol.workout_id
	WHERE imported_with_errors = 0


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Create the master measurables table.

**Keep an eye on workouts where some times are 4 priority, some are 5. Example workout ids: 638693,638694,638696,638697,638698,638699,638700,638806,638808,638809,638810,638811

OUTPUT TABLES:
#temp_measurables_master

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

DECLARE @current_season INT
SELECT @current_season = (SELECT MAX(season) FROM Analytics.dbo.map_nfl_league_year_dates WHERE GETDATE() >= regular_season_start)

-- Check if #temp_measurables_master exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_measurables_master') IS NOT NULL
	DROP TABLE #temp_measurables_master

	SELECT wo.id AS workout_id
		,wo.[description] AS workout_description
		,wo.player_id AS bane_player_id
		,pl.draft_year
		,CASE WHEN pl.draft_year <= @current_season THEN DATEDIFF(DD,(birth_date),CONCAT('04/01/',pl.draft_year)) / 365.2425 
			WHEN pl.draft_year IN (@current_season + 1) THEN DATEDIFF(DD,(birth_date),CONCAT('04/01/',@current_season + 1)) / 365.2425 
			ELSE NULL
		END AS age_at_draft
		,CASE WHEN dbp.position IN ('RUSH','SAM','DP') THEN 'OB'
			WHEN dbp.position IN ('FS','SS') THEN 'DS'
			WHEN dbp.position IN ('OH') THEN 'RB'
			WHEN dbp.position IN ('P') THEN 'PT'
			WHEN dbp.position IN ('WO') THEN 'WR'
			WHEN dbp.position IN ('WO') THEN 'WR'
			WHEN COALESCE(dbp.position,po.abbreviation) = 'DE43' THEN 'OB'
			ELSE REPLACE(COALESCE(dbp.position,po.translation),' ','')
		END AS position
		,eligibility
		,re.code AS workout_code
		,CASE WHEN re.[priority] != 1 AND UPPER(re.[description]) LIKE '%ELECTRONIC%' THEN 1.5
			ELSE re.[priority]
		END AS adjusted_priority
		,wt.[type]
		,wo.[date] AS workout_date
		,me.measurable_type_id
		,mt.code
		,me.order_by AS attempt
		,me.[value]
	INTO #temp_measurables_master
	FROM [BaneProductionAnalytics].[dbo].[workouts] wo
	LEFT JOIN #temp_measurables me
		ON wo.id = me.workout_id
	LEFT JOIN [BaneProductionAnalytics].[dbo].[measurable_types] mt
		ON me.measurable_type_id = mt.id
	LEFT JOIN [BaneProductionAnalytics].[dbo].[player_workout_types] wt
		ON wo.player_workout_type_id = wt.id
	LEFT JOIN [BaneProductionAnalytics].[dbo].[reliabilities] re
		ON me.reliability_id = re.id
	LEFT JOIN [BaneProductionAnalytics].[dbo].[players] pl
		ON wo.player_id = pl.id
	LEFT JOIN [BaneProductionAnalytics].[dbo].[positions] po
		ON pl.position_id = po.id
	LEFT JOIN #temp_draft_board_positions_latest dbp
		ON wo.player_id = dbp.player_id
	WHERE re.translation IN ('I','V','W','R') --Only Indy, Pro Day, Combine, or Workout numbers
		AND wo.[description] NOT LIKE '%(VET)%' --Not from the Veteran Combine
		AND NOT (wt.[type] = 'RPT' AND re.translation = 'I')  --Not from a scout entering the Combine numbers
		AND (wo.is_deleted = 0 OR (wo.is_deleted = 1 AND wo.[description] LIKE 'NCAA Scouting%')) --Not a deleted record
		AND NOT YEAR(wo.[date]) > pl.draft_year --Not from a workout that occured after his draft
		AND (pl.nfl_id IS NOT NULL OR pl.draft_year = @current_season + 1) --Only players who at least had one NFL transaction
		AND pl.draft_year >= 2005
		AND pl.draft_year <=  @current_season + 1
		AND value IS NOT NULL
		AND ((mt.code = 'HgtCnv' AND value BETWEEN 40 AND 90)
			OR (mt.code = 'Wgt' AND value BETWEEN 100 AND 450)
			OR (mt.code = '40YTime' AND value BETWEEN 4.00 AND 7.00)
			OR (mt.code = 'WSpanCnv' AND value BETWEEN 40 AND 120)
			OR (mt.code = 'ArmLngCnv' AND value BETWEEN 20 AND 48)
			OR (mt.code = 'HSpanCnv' AND value BETWEEN 6 AND 15)
			OR (mt.code = '20YShuttle' AND value BETWEEN 3 AND 30)
			OR (mt.code = 'VertJumpCnv' AND value BETWEEN 10 AND 60)
			OR (mt.code IN ('BroadJumpCnv','StrengthReps225','TestScore','20YTime','10YTime','3Cone','60YShuttle','LTurn','RTurn','RipRushL','SwimRushR','OutRouteL','OutRouteR'
			,'FPCT','FMass','FFMass','RMR','FlexFrontShoulderCnv','FlexBackShoulderCnv','FlexHamstringCnv','FlexGroinCnv'))
			)

/*
SELECT *
FROM #temp_measurables_master
WHERE bane_player_id = 90041
ORDER BY workout_code desc, attempt, code
*/

/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Find the average age by eligibility to fill in missing ages for the current draft year.

OUTPUT TABLES:
#temp_average_ages

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

DECLARE @current_season INT
SELECT @current_season = (SELECT MAX(season) FROM Analytics.dbo.map_nfl_league_year_dates WHERE GETDATE() >= regular_season_start)

-- Check if #temp_average_ages exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_average_ages') IS NOT NULL
	DROP TABLE #temp_average_ages

	SELECT draft_year
		,eligibility
		,AVG(age_at_draft) AS average_age
	INTO #temp_average_ages
	FROM #temp_measurables_master
	WHERE draft_year = @current_season + 1
	GROUP BY draft_year
		,eligibility

/*
SELECT *
FROM #temp_average_ages
ORDER BY draft_year, eligibility
*/

/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Pivot the non-run measures (height, weight, jump, bench, etc.)

OUTPUT TABLES:
#temp_measures_pivot

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_measures_pivot exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_measures_pivot') IS NOT NULL
	DROP TABLE #temp_measures_pivot

	SELECT workout_id
		,workout_description
		,bane_player_id
		,draft_year
		,age_at_draft
		,eligibility
		,position
		,[HgtCnv] AS height
		,[Wgt] AS [weight]
		,[HSpanCnv] AS hand_size
		,[ArmLngCnv] AS arm_length
		,[WSpanCnv] AS wingspan
		,[BroadJumpCnv] AS broad_jump
		,[VertJumpCnv] AS vertical_jump
		,[StrengthReps225] AS bench_reps
		,[TestScore] AS wonderlic
		,[OutRouteL] AS throw_speed_left
		,[OutRouteR] AS throw_speed_right
	INTO #temp_measures_pivot
	FROM (
	SELECT workout_id
		,workout_description
		,bane_player_id
		,draft_year
		,age_at_draft
		,eligibility
		,position
		,code
		,value
	FROM #temp_measurables_master WHERE code IN ('HgtCnv','Wgt','HSpanCnv','ArmLngCnv','WSpanCnv','BroadJumpCnv','VertJumpCnv','StrengthReps225','TestScore','OutRouteL','OutRouteR')) up
	PIVOT (MAX(value) FOR code IN ([HgtCnv],[Wgt],[HSpanCnv],[ArmLngCnv],[WSpanCnv],[BroadJumpCnv],[VertJumpCnv],[StrengthReps225],[TestScore],[OutRouteL],[OutRouteR])) AS pvt
	WHERE 1 = 1
	ORDER BY bane_player_id
		,workout_id

/*
SELECT *
FROM #temp_measures_pivot
WHERE bane_player_id = 89618
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Pivot the 40, 20, and 10 Times so you can get Flying 10 and 20 from the same run.
You want to avoid taking his best 40, and his best 20 from different runs to calculate
his best flying 20.  Also pivot his other runs (Shuttles, etc.) so you can calculate
the advanced measurables.

OUTPUT TABLES:
#temp_runs_pivot

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_runs_pivot exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_runs_pivot') IS NOT NULL
	DROP TABLE #temp_runs_pivot

	SELECT workout_id
		,workout_description
		,adjusted_priority
		,bane_player_id
		,attempt
		,[40YTime] AS forty_dash
		,[20YTime] AS twenty_split
		,[10YTime] AS ten_split
		,[20YShuttle] AS short_shuttle
		,[60YShuttle] AS long_shuttle
		,[3Cone] AS three_cone
		,[LTurn] AS left_turn
		,[RTurn] AS right_turn
		,[RipRushL] AS rip_left
		,[SwimRushR] AS swim_right
	INTO #temp_runs_pivot
	FROM (
	SELECT workout_id
		,workout_description
		,adjusted_priority
		,bane_player_id
		,attempt
		,code
		,value
	FROM #temp_measurables_master WHERE code IN ('40YTime', '20YTime', '10YTime', '20YShuttle', '3Cone', '60YShuttle','LTurn','RTurn','RipRushL','SwimRushR')) up
	PIVOT (MIN(value) FOR code IN ([40YTime], [20YTime], [10YTime], [20YShuttle], [3Cone], [60YShuttle],[LTurn],[RTurn],[RipRushL],[SwimRushR])) AS pvt
	WHERE 1 = 1
	ORDER BY bane_player_id
		,workout_id

/*
SELECT *
FROM #temp_runs_pivot
WHERE bane_player_id = 34960
ORDER BY adjusted_priority, attempt
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Create a table with adjusted workout priority for workouts where a player
doesn't run.

OUTPUT TABLES:
#temp_adjusted_workout_priority

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_adjusted_workout_priority exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_adjusted_workout_priority') IS NOT NULL
	DROP TABLE #temp_adjusted_workout_priority

	SELECT workout_id
		,MAX(adjusted_priority) AS adjusted_priority
		,MIN(attempt) AS attempt
	INTO #temp_adjusted_workout_priority
	FROM #temp_measurables_master
	GROUP BY workout_id

/*
SELECT *
FROM #temp_adjusted_workout_priority
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Join the runs to the measures. Also add in the "Flying" times.

OUTPUT TABLES:
#temp_measurables

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_measurables exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_measurables') IS NOT NULL
	DROP TABLE #temp_measurables

	SELECT me.workout_id
		,me.workout_description
		,COALESCE(ru.adjusted_priority,aw.adjusted_priority) AS adjusted_priority
		,me.bane_player_id
		,me.draft_year
		,COALESCE(age_at_draft,average_age) AS age_at_draft
		,position
		,CASE WHEN position IN ('OT','OG','OC') THEN 'OL'
			WHEN position IN ('WR','TE','RB','FB') THEN 'OSKILL'
			WHEN position IN ('DT','DE') THEN 'DL'
			WHEN position IN ('OB','IB') THEN 'LB'
			WHEN position IN ('DC','DS') THEN 'DB'
			ELSE position
		END AS position_group
		,COALESCE(ru.attempt,aw.attempt) AS attempt
		,height
		,[weight]
		,hand_size
		,arm_length
		,wingspan
		,broad_jump
		,vertical_jump
		,bench_reps
		,bench_reps * arm_length AS bench_work
		,wonderlic
		,ten_split
		,twenty_split
		,forty_dash
		,twenty_split - ten_split AS flying_ten
		,forty_dash - twenty_split AS flying_twenty
		,short_shuttle
		,long_shuttle
		,three_cone
		,throw_speed_left
		,throw_speed_right
		,CASE WHEN throw_speed_left > throw_speed_right THEN throw_speed_left
			WHEN throw_speed_left <= throw_speed_right THEN throw_speed_right
		END AS throw_speed
		,left_turn
		,right_turn
		,rip_left
		,swim_right
	INTO #temp_measurables
	FROM #temp_measures_pivot me
	LEFT JOIN #temp_runs_pivot ru
		ON me.workout_id = ru.workout_id
	LEFT JOIN #temp_adjusted_workout_priority aw--Join this in so you can get priority when a player didn't run at that workout
		ON me.workout_id = aw.workout_id
	LEFT JOIN #temp_average_ages age
		ON me.draft_year = age.draft_year
		AND me.eligibility = age.eligibility

/*
SELECT *
FROM #temp_measurables
WHERE bane_player_id = 87073
ORDER BY adjusted_priority, attempt
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Create a table with a player's best priority height to add where height is missing.


----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_height_fills exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_height_fills') IS NOT NULL
	DROP TABLE #temp_height_fills

	SELECT bane_player_id
		,MAX(height) AS height_fill_in
	INTO #temp_height_fills
	FROM #temp_measurables m1
	WHERE adjusted_priority = (SELECT MIN(adjusted_priority) FROM #temp_measurables m2 WHERE m1.bane_player_id = m2.bane_player_id)
	GROUP BY bane_player_id


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Create an r input table that you can feed in to generate expected measurables.
Join in the fill in heights for when a guy didn't have a height measured at a workout.

LONG SHUTTLE: NO QB, OL, DL

OUTPUT TABLES:
Analytics.dbo.r_input_draft_model_expected_measurables

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

--DELETE FROM Analytics.dbo.r_input_draft_model_expected_measurables
--WHERE created_date = (SELECT MAX(created_date) FROM Analytics.dbo.r_input_draft_model_expected_measurables WHERE draft_model_year = @current_season)

	INSERT INTO Analytics.dbo.r_input_draft_model_expected_measurables
	SELECT workout_id
		,workout_description
		,adjusted_priority
		,me.bane_player_id
		,draft_year
		,age_at_draft
		,position
		,position_group
		,attempt
		,CASE WHEN height IS NULL THEN height_fill_in ELSE height END AS height
		,[weight]
		,hand_size
		,arm_length
		,wingspan
		,broad_jump
		,vertical_jump
		,bench_reps
		,bench_work
		,wonderlic
		,ten_split
		,twenty_split
		,forty_dash
		,flying_ten
		,flying_twenty
		,short_shuttle
		,long_shuttle
		,three_cone
		,throw_speed_left
		,throw_speed_right
		,throw_speed
		,left_turn
		,right_turn
		,rip_left
		,swim_right
		,2019 AS draft_model_year
		,GETDATE() AS created_date
	FROM #temp_measurables me
	LEFT JOIN #temp_height_fills hf
		ON me.bane_player_id = hf.bane_player_id
	ORDER BY position
		,adjusted_priority
		,bane_player_id

/*
SELECT *
FROM Analytics.dbo.r_input_draft_model_expected_measurables
WHERE draft_model_year = 2019
	AND created_date = (SELECT MAX(created_date) FROM Analytics.dbo.r_input_draft_model_expected_measurables WHERE draft_model_year = 2019)
ORDER BY adjusted_priority, attempt
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

RUN R PROGRAM
X:\R\college_scouting\draft_modeling\post_combine\expected_measurables\expected_measurables_score_data_v5

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Join the r input and output tables so you can compare actual to expected values.

*****Watch for the join on created_date. R cuts the last three numbers off of the sql date time so they may not match exactly.
*****You usually have to hard code the MAX created date from the output table to the inner join betwen the input and the output table.

(Inner join to r_output removes LS, PT, PK, and NULL positions)

OUTPUT TABLES:
#temp_advanced_measurables

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_advanced_measurables exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_advanced_measurables') IS NOT NULL
	DROP TABLE #temp_advanced_measurables

	SELECT inp.workout_id
		,inp.workout_description
		,inp.adjusted_priority
		,inp.bane_player_id
		,inp.draft_year
		,inp.age_at_draft
		,inp.position
		,inp.position_group
		,inp.attempt
		,inp.height
		,inp.[weight]
		,inp.hand_size
		,inp.arm_length
		,inp.wingspan
		,inp.broad_jump
		,inp.vertical_jump
		,inp.bench_reps
		,inp.bench_work
		,inp.wonderlic
		,CONVERT(DECIMAL(20,6),(CASE WHEN inp.adjusted_priority = 1 THEN forty_dash ELSE forty_dash_adjusted END)) AS forty_dash
		,CONVERT(DECIMAL(20,6),(CASE WHEN inp.adjusted_priority = 1 THEN twenty_split ELSE twenty_split_adjusted END)) AS twenty_split
		,CONVERT(DECIMAL(20,6),(CASE WHEN inp.adjusted_priority = 1 THEN ten_split ELSE ten_split_adjusted END)) AS ten_split
		,CONVERT(DECIMAL(20,6),(CASE WHEN inp.adjusted_priority = 1 THEN flying_twenty ELSE flying_twenty_adjusted END)) AS flying_twenty
		,CONVERT(DECIMAL(20,6),(CASE WHEN inp.adjusted_priority = 1 THEN flying_ten ELSE flying_ten_adjusted END)) AS flying_ten
		,inp.short_shuttle
		,inp.long_shuttle
		,inp.three_cone
		,inp.throw_speed_left
		,inp.throw_speed_right
		,inp.throw_speed
		,inp.left_turn
		,inp.right_turn
		,inp.rip_left
		,inp.swim_right
		,CONVERT(DECIMAL(20,6),(CASE WHEN inp.adjusted_priority = 1 THEN forty_dash ELSE forty_dash_adjusted END) - expected_forty_dash) AS forty_dash_vs_expected
		,CONVERT(DECIMAL(20,6),(CASE WHEN inp.adjusted_priority = 1 THEN twenty_split ELSE twenty_split_adjusted END) - expected_twenty_split) AS twenty_split_vs_expected
		,CONVERT(DECIMAL(20,6),(CASE WHEN inp.adjusted_priority = 1 THEN ten_split ELSE ten_split_adjusted END) - expected_ten_split) AS ten_split_vs_expected
		,CONVERT(DECIMAL(20,6),(CASE WHEN inp.adjusted_priority = 1 THEN flying_twenty ELSE flying_twenty_adjusted END) - expected_flying_twenty) AS flying_twenty_vs_expected
		,CONVERT(DECIMAL(20,6),(CASE WHEN inp.adjusted_priority = 1 THEN flying_ten ELSE flying_ten_adjusted END) - expected_flying_ten) AS flying_ten_vs_expected
		,CONVERT(DECIMAL(20,6),long_shuttle - expected_long_shuttle) AS long_shuttle_vs_expected
		,CONVERT(DECIMAL(20,6),short_shuttle - expected_short_shuttle) AS short_shuttle_vs_expected
		,CONVERT(DECIMAL(20,6),fill_in_short_shuttle - expected_short_shuttle) AS fill_in_short_shuttle_vs_expected
		,CONVERT(DECIMAL(20,6),fill_in_short_shuttle) AS fill_in_short_shuttle
		,CONVERT(DECIMAL(20,6),three_cone - expected_three_cone) AS three_cone_vs_expected
		,CONVERT(DECIMAL(20,6),fill_in_three_cone - expected_three_cone) AS fill_in_three_cone_vs_expected
		,CONVERT(DECIMAL(20,6),fill_in_three_cone) AS fill_in_three_cone
		,CONVERT(DECIMAL(20,6),broad_jump - expected_broad_jump) AS broad_jump_vs_expected
		,CONVERT(DECIMAL(20,6),vertical_jump - expected_vertical_jump) AS vertical_jump_vs_expected
		,CONVERT(DECIMAL(20,6),bench_reps - expected_bench_reps) AS bench_reps_vs_expected
		,CONVERT(DECIMAL(20,6),bench_work - expected_bench_work) AS bench_work_vs_expected
		,CONVERT(DECIMAL(20,6),swim_right - expected_swim_right) AS swim_right_vs_expected
		,CONVERT(DECIMAL(20,6),rip_left - expected_rip_left) AS rip_left_vs_expected
		,CONVERT(DECIMAL(20,6),left_turn - expected_left_turn) AS left_turn_vs_expected
		,CONVERT(DECIMAL(20,6),right_turn - expected_right_turn) AS right_turn_vs_expected
		,CONVERT(DECIMAL(20,6),throw_speed - expected_throw_speed) AS throw_speed_vs_expected
	INTO #temp_advanced_measurables
	FROM Analytics.dbo.r_input_draft_model_expected_measurables inp
	INNER JOIN Analytics.dbo.r_output_draft_model_expected_measurables outp
		ON inp.workout_id = outp.workout_id
		AND inp.adjusted_priority = outp.adjusted_priority
		AND inp.attempt = outp.attempt
		AND inp.draft_model_year = outp.draft_model_year
		AND outp.created_date = (SELECT MAX(created_date) FROM Analytics.dbo.r_output_draft_model_expected_measurables)
	WHERE inp.draft_model_year = 2019
		AND inp.created_date = (SELECT MAX(created_date) FROM Analytics.dbo.r_input_draft_model_expected_measurables)

/*
SELECT *
FROM #temp_advanced_measurables
WHERE bane_player_id = 34960
ORDER BY adjusted_priority, attempt
*/

/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Unpivot the best runs and measures.

OUTPUT TABLES:
#temp_advanced_runs
#temp_advanced_measures

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_advanced_runs exists, if it does drop it
	IF OBJECT_ID('tempdb..#temp_advanced_runs') IS NOT NULL
		DROP TABLE #temp_advanced_runs

	SELECT bane_player_id
		,draft_year
		,age_at_draft
		,position
		,adjusted_priority
		,attempt
		,measurable_type
		,value
	INTO #temp_advanced_runs
	FROM #temp_advanced_measurables
	UNPIVOT (value FOR measurable_type IN (forty_dash
							,twenty_split
							,ten_split
							,flying_twenty
							,flying_ten
							,short_shuttle
							,long_shuttle
							,three_cone
							,swim_right
							,rip_left
							,left_turn
							,right_turn
							,forty_dash_vs_expected
							,twenty_split_vs_expected
							,ten_split_vs_expected
							,flying_twenty_vs_expected
							,flying_ten_vs_expected
							,short_shuttle_vs_expected
							,fill_in_short_shuttle_vs_expected
							,fill_in_short_shuttle
							,long_shuttle_vs_expected
							,three_cone_vs_expected
							,fill_in_three_cone_vs_expected
							,fill_in_three_cone
							,swim_right_vs_expected
							,rip_left_vs_expected
							,left_turn_vs_expected
							,right_turn_vs_expected
							)) AS me

/*
SELECT *
FROM #temp_advanced_runs
*/

-- Check if #temp_advanced_measures exists, if it does drop it
	IF OBJECT_ID('tempdb..#temp_advanced_measures') IS NOT NULL
		DROP TABLE #temp_advanced_measures

	SELECT bane_player_id
		,draft_year
		,age_at_draft
		,position
		,adjusted_priority
		,attempt
		,measurable_type
		,value
	INTO #temp_advanced_measures
	FROM #temp_advanced_measurables
	UNPIVOT (value FOR measurable_type IN (height
								,[weight]
								,hand_size
								,arm_length
								,wingspan
								,broad_jump
								,vertical_jump
								,bench_reps
								,bench_work
								,wonderlic
								,throw_speed
								,broad_jump_vs_expected
								,vertical_jump_vs_expected
								,bench_reps_vs_expected
								,bench_work_vs_expected
								,throw_speed_vs_expected
								)) AS me

/*
SELECT *
FROM #temp_advanced_measures
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Order the player's measurables...if a player has combine measurables, use those.  If he 
does not have combine measurables, use his adjusted pro day / workout / regional combine
measurables (but include a dummy to represent that it isn't electronic).

OUTPUT TABLES:
#temp_advanced_measures_with_order
#temp_advanced_runs_with_order

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_advanced_runs_with_order exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_advanced_runs_with_order') IS NOT NULL
	DROP TABLE #temp_advanced_runs_with_order

	SELECT *
		,RANK() OVER (PARTITION BY bane_player_id, measurable_type ORDER BY adjusted_priority, value, attempt) AS order_for_models
		--,CASE WHEN adjusted_priority < 2 THEN 1 ELSE 0 END AS electronic
	INTO #temp_advanced_runs_with_order
	FROM #temp_advanced_runs

/*
SELECT *
FROM #temp_advanced_runs_with_order
*/

-- Check if #temp_advanced_measures_with_order exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_advanced_measures_with_order') IS NOT NULL
	DROP TABLE #temp_advanced_measures_with_order

	SELECT *
		,RANK() OVER (PARTITION BY bane_player_id, measurable_type ORDER BY adjusted_priority, value DESC, attempt) AS order_for_models
		--,CASE WHEN adjusted_priority < 2 THEN 1 ELSE 0 END AS electronic
	INTO #temp_advanced_measures_with_order
	FROM #temp_advanced_measures

/*
SELECT *
FROM #temp_advanced_measures_with_order
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Append the ordered measures to the ordered runs table so that you can pivot the best ones.

OUTPUT TABLES:
#temp_advanced_measurables_all

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_advanced_measurables_all exists, if it does drop it
	IF OBJECT_ID('tempdb..#temp_advanced_measurables_all') IS NOT NULL
		DROP TABLE #temp_advanced_measurables_all

	SELECT *
	INTO #temp_advanced_measurables_all
	FROM #temp_advanced_runs_with_order

	INSERT INTO #temp_advanced_measurables_all
	SELECT *
	FROM #temp_advanced_measures_with_order

/*
SELECT * 
FROM #temp_advanced_measurables_all
WHERE bane_player_id = 34960
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Pivot the previous table so you can have one row for every player with his best value
in every measurable. You appended before pivoting because you can just take the MAX
in the pivot, even for when you want a MIN (like with runs) since if you only take
where the order_for_models = 1, you already have the one you want.

OUTPUT TABLES:
#temp_advanced_measurables_pivot

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_advanced_measurables_pivot exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_advanced_measurables_pivot') IS NOT NULL
	DROP TABLE #temp_advanced_measurables_pivot

	SELECT bane_player_id
		,draft_year
		,age_at_draft
		,position
		,height
		,[weight]
		,hand_size
		,arm_length
		,wingspan
		,broad_jump
		,broad_jump_vs_expected
		,vertical_jump
		,vertical_jump_vs_expected
		,bench_reps
		,bench_reps_vs_expected
		,bench_work
		,bench_work_vs_expected
		,wonderlic
		,throw_speed
		,throw_speed_vs_expected
		,forty_dash
		,forty_dash_vs_expected
		,twenty_split
		,twenty_split_vs_expected
		,ten_split
		,ten_split_vs_expected
		,flying_twenty
		,flying_twenty_vs_expected
		,flying_ten
		,flying_ten_vs_expected
		,short_shuttle
		,short_shuttle_vs_expected
		,fill_in_short_shuttle_vs_expected
		,fill_in_short_shuttle
		,long_shuttle
		,long_shuttle_vs_expected
		,three_cone
		,three_cone_vs_expected
		,fill_in_three_cone_vs_expected
		,fill_in_three_cone
		,swim_right
		,swim_right_vs_expected
		,rip_left
		,rip_left_vs_expected
		,left_turn
		,left_turn_vs_expected
		,right_turn
		,right_turn_vs_expected
	INTO #temp_advanced_measurables_pivot
	FROM (
	SELECT bane_player_id
		,draft_year
		,age_at_draft
		,position
		,order_for_models
		,measurable_type
		,value
	FROM #temp_advanced_measurables_all WHERE measurable_type IN ('height', 'weight', 'arm_length', 'wingspan', 'hand_size', 'broad_jump', 'broad_jump_vs_expected', 'vertical_jump', 'vertical_jump_vs_expected', 'bench_reps', 'bench_reps_vs_expected', 'bench_work', 'bench_work_vs_expected', 'wonderlic', 'throw_speed', 'throw_speed_vs_expected', 'forty_dash', 'forty_dash_vs_expected', 'twenty_split', 'twenty_split_vs_expected', 'ten_split', 'ten_split_vs_expected', 'flying_twenty', 'flying_twenty_vs_expected', 'flying_ten', 'flying_ten_vs_expected', 'short_shuttle', 'short_shuttle_vs_expected', 'fill_in_short_shuttle_vs_expected', 'fill_in_short_shuttle', 'long_shuttle', 'long_shuttle_vs_expected', 'three_cone', 'three_cone_vs_expected', 'fill_in_three_cone_vs_expected', 'fill_in_three_cone', 'swim_right', 'swim_right_vs_expected', 'rip_left', 'rip_left_vs_expected', 'right_turn', 'right_turn_vs_expected', 'left_turn', 'left_turn_vs_expected')) up
	PIVOT (MAX(value) FOR measurable_type IN ([height], [weight], [arm_length], [wingspan], [hand_size], [broad_jump], [broad_jump_vs_expected], [vertical_jump], [vertical_jump_vs_expected], [bench_reps], [bench_reps_vs_expected], [bench_work], [bench_work_vs_expected], [wonderlic], [throw_speed], [throw_speed_vs_expected], [forty_dash], [forty_dash_vs_expected], [twenty_split], [twenty_split_vs_expected], [ten_split], [ten_split_vs_expected], [flying_twenty], [flying_twenty_vs_expected], [flying_ten], [flying_ten_vs_expected], [short_shuttle], [short_shuttle_vs_expected], [fill_in_short_shuttle_vs_expected], [fill_in_short_shuttle], [long_shuttle], [long_shuttle_vs_expected], [three_cone], [three_cone_vs_expected], [fill_in_three_cone_vs_expected], [fill_in_three_cone], [swim_right], [swim_right_vs_expected], [rip_left], [rip_left_vs_expected], [right_turn], [right_turn_vs_expected], [left_turn], [left_turn_vs_expected])) AS pvt
	WHERE order_for_models = 1
	ORDER BY bane_player_id
  
/*
SELECT * 
FROM #temp_advanced_measurables_pivot
WHERE bane_player_id = 7011
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Join his best measurables to his pro grade and salary data.

OUTPUT TABLES:
Analytics.dbo.r_input_post_combine_measurables

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

DECLARE @current_season INT
SELECT @current_season = (SELECT MAX(season) FROM Analytics.dbo.map_nfl_league_year_dates WHERE GETDATE() >= regular_season_start)

--DELETE FROM Analytics.dbo.r_input_draft_model_post_combine_measurables
--WHERE draft_model_year = @current_season + 1

INSERT INTO Analytics.dbo.r_input_draft_model_post_combine_measurables
    SELECT mp.bane_player_id
		,mp.draft_year
		,mp.age_at_draft
		,CASE WHEN COALESCE(mp.position,pn.translation) = 'DC' AND position_defense = 'NB' THEN 'DC_SLOT'
			WHEN COALESCE(mp.position,pn.translation) = 'WR' AND receiver_type = 'SLOT' THEN 'WR_SLOT'
			ELSE COALESCE(mp.position,pn.translation)
		END AS position
		,mp.height
		,mp.[weight]
		,mp.hand_size
		,mp.arm_length
		,mp.wingspan
		,mp.broad_jump
		,mp.broad_jump_vs_expected
		,mp.vertical_jump
		,mp.vertical_jump_vs_expected
		,mp.bench_reps
		,mp.bench_reps_vs_expected
		,mp.bench_work
		,mp.bench_work_vs_expected
		,mp.wonderlic
		,mp.throw_speed
		,mp.throw_speed_vs_expected
		,mp.forty_dash
		,mp.forty_dash_vs_expected
		,mp.twenty_split
		,mp.twenty_split_vs_expected
		,mp.ten_split
		,mp.ten_split_vs_expected
		,mp.flying_twenty
		,mp.flying_twenty_vs_expected
		,mp.flying_ten
		,mp.flying_ten_vs_expected
		,mp.short_shuttle
		,mp.short_shuttle_vs_expected
		,mp.fill_in_short_shuttle_vs_expected
		,mp.fill_in_short_shuttle
		,mp.long_shuttle
		,mp.long_shuttle_vs_expected
		,mp.three_cone
		,mp.three_cone_vs_expected
		,mp.fill_in_three_cone_vs_expected
		,mp.fill_in_three_cone
		,mp.swim_right
		,mp.swim_right_vs_expected
		,mp.rip_left
		,mp.rip_left_vs_expected
		,mp.left_turn
		,mp.left_turn_vs_expected
		,mp.right_turn
		,mp.right_turn_vs_expected
		,CONCAT(pl.last_name,', ',pl.goes_by) AS player
		,CASE WHEN pl.draft_year > @current_season OR sal.salaries IN ('Pre-2011') OR (pl.draft_year <= 2007 AND sal.salaries IS NULL) THEN NULL 
			ELSE CASE WHEN sal.salaries IN ('Injured') THEN ins.salaries
					ELSE COALESCE(CAST(sal.salaries AS FLOAT),0) 
				END 
		END AS pct_of_cap
		,pg.season
		,CASE WHEN pl.draft_year > @current_season THEN 1 ELSE season_in_league END AS season_in_league
		,CASE WHEN pl.draft_year > @current_season THEN NULL ELSE COALESCE(grade_value,50) END AS pro_grade
		,@current_season + 1 AS draft_model_year
		,GETDATE() AS created_date
		,0 AS alternate_data
	FROM #temp_advanced_measurables_pivot mp
	INNER JOIN BaneProductionAnalytics.dbo.players pl
		ON mp.bane_player_id = pl.id
		AND pl.is_deleted = 0
	LEFT JOIN BaneProductionAnalytics.dbo.positions pn
		ON pl.position_id = pn.id  
	LEFT JOIN AnalyticsWork.dbo.salaries_for_modeling sal
		ON mp.bane_player_id = sal.id
		--AND sal.salaries NOT IN ('Injured','Pre-2011')
	LEFT JOIN #temp_pro_grades pg
		ON mp.bane_player_id = pg.bane_player_id
		AND pg.created_date = (SELECT MAX(created_date) FROM #temp_pro_grades)
	LEFT JOIN #temp_injured_salaries_one_row ins
		ON pl.nfl_id = ins.nfl_player_id
	LEFT JOIN #temp_season_position_offense poff
		ON pg.bane_player_id = poff.bane_player_id
		AND pg.season = poff.season
	LEFT JOIN #temp_season_position_defense pdef
		ON pg.bane_player_id = pdef.bane_player_id
		AND pg.season = pdef.season
	WHERE pl.draft_year <= @current_season
		OR (pl.draft_year IN (@current_season + 1,@current_season + 2) AND pg.season IS NULL)

	DELETE FROM Analytics.dbo.r_input_draft_model_post_combine_measurables
	WHERE bane_player_id = 64668
		AND position = 'WR'

	--Add Lamar Jackson measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET three_cone_vs_expected = -0.0594643125000092
		,flying_twenty_vs_expected = -0.141826559999999
	WHERE bane_player_id = 88748

	--Add Mike McGlinchey measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET ten_split_vs_expected = 0
		,flying_twenty_vs_expected = 0
	WHERE bane_player_id = 63864

	--Add Rashaan Evans measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET ten_split_vs_expected = 0
		,flying_ten_vs_expected = 0
	WHERE bane_player_id = 72961

	--Add Quenton Nelson measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET ten_split_vs_expected = 0
		,twenty_split = 2.94
	WHERE bane_player_id = 84627

	--Add Arden Key measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET ten_split_vs_expected = 0
		,flying_twenty_vs_expected = 0
	WHERE bane_player_id = 	85615

	--Add Chris Herndon measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET flying_twenty_vs_expected = 0
	WHERE bane_player_id = 73531

	--Add Vita Vea measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET vertical_jump = (SELECT AVG(vertical_jump) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'DT' AND draft_year >= 2016 AND weight BETWEEN 345 AND 350) 
	WHERE bane_player_id = 87301

	--Add Dallas Goedert measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET flying_twenty_vs_expected = 0
	WHERE bane_player_id = 76704

	--Add James Daniels measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET flying_ten_vs_expected = 0
	WHERE bane_player_id = 87480

	--Add Dante Pettis measurables
	--UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	--SET flying_ten_vs_expected = 0
		--,forty_dash_pass = 0
	--WHERE bane_player_id = 80868

	--Add Jordan Akins measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET flying_twenty_vs_expected = 1.922666 - 1.98710525
	WHERE bane_player_id = 74096

	--Add Geron Christian measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET broad_jump = (SELECT AVG(broad_jump) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'OT' AND draft_year >= 2016 AND weight BETWEEN 295 AND 300) 
	WHERE bane_player_id = 	88463

	--Add Martinas Rankin measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET ten_split_vs_expected = 0
		,flying_twenty_vs_expected = 0
	WHERE bane_player_id = 233243
	
	--Add Isaiah Wynn measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET twenty_split = (SELECT AVG(twenty_split) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'OG' AND draft_year >= 2016 AND weight BETWEEN 310 AND 315)
		,three_cone = (SELECT AVG(three_cone) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'OG' AND draft_year >= 2016 AND weight BETWEEN 310 AND 315)  
	WHERE bane_player_id = 72871

	--Add Billy Price measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET flying_ten_vs_expected = 0
	WHERE bane_player_id = 78530

	--Add Tim Settle measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET bench_reps = (SELECT AVG(bench_reps) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'DT' AND draft_year >= 2016 AND weight BETWEEN 325 AND 330) 
	WHERE bane_player_id = 232674

	--Add Duke Ejiofor measurables
	--UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	--SET broad_jump = (SELECT AVG(broad_jump) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'OB' AND draft_year >= 2016 AND weight BETWEEN 260 AND 265) 
		--,three_cone = (SELECT AVG(three_cone) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'OB' AND draft_year >= 2016 AND weight BETWEEN 260 AND 265) 
		--,flying_twenty_vs_expected = 0
	--WHERE bane_player_id = 61358

	--Add Troy Fumagalli measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET flying_twenty_vs_expected = 0
	WHERE bane_player_id = 82282

	--Add Davin Bellamy measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET flying_twenty_vs_expected = 0
	WHERE bane_player_id = 74106

	--Add Jack Cichy measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET flying_ten_vs_expected = 0
		,ten_split_vs_expected = 0
	WHERE bane_player_id = 71450

	--Add Bryce Love measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET twenty_split = (SELECT AVG(twenty_split) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'RB' AND draft_year >= 2016 AND weight BETWEEN 198 AND 203) 
		,short_shuttle = (SELECT AVG(short_shuttle) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'RB' AND draft_year >= 2016 AND weight BETWEEN 198 AND 203) 
	WHERE bane_player_id = 88699

	--Add Jalin Moore measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET twenty_split = (SELECT AVG(twenty_split) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'RB' AND draft_year >= 2016 AND weight BETWEEN 210 AND 215) 
		,short_shuttle = (SELECT AVG(short_shuttle) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'RB' AND draft_year >= 2016 AND weight BETWEEN 210 AND 215) 
	WHERE bane_player_id = 73239

	--Add Rodney Anderson measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET twenty_split = (SELECT AVG(twenty_split) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'RB' AND draft_year >= 2016 AND weight BETWEEN 222 AND 227) 
		,short_shuttle = (SELECT AVG(short_shuttle) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'RB' AND draft_year >= 2016 AND weight BETWEEN 222 AND 227) 
	WHERE bane_player_id = 87228

	--Add Chase Hansen measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET twenty_split = (SELECT AVG(twenty_split) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'IB' AND draft_year >= 2016 AND weight BETWEEN 220 AND 225) 
		,flying_twenty = (SELECT AVG(flying_twenty) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'IB' AND draft_year >= 2016 AND weight BETWEEN 220 AND 225) 
		,short_shuttle = (SELECT AVG(short_shuttle) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'IB' AND draft_year >= 2016 AND weight BETWEEN 220 AND 225) 
	WHERE bane_player_id = 89604

	--Add Vosean Joseph measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET twenty_split = (SELECT AVG(twenty_split) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'IB' AND draft_year >= 2016 AND weight BETWEEN 228 AND 233) 
		,flying_twenty = (SELECT AVG(flying_twenty) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'IB' AND draft_year >= 2016 AND weight BETWEEN 228 AND 233) 
		,short_shuttle = (SELECT AVG(short_shuttle) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'IB' AND draft_year >= 2016 AND weight BETWEEN 228 AND 233) 
	WHERE bane_player_id = 232137

	--Add David Long measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET twenty_split = (SELECT AVG(twenty_split) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'IB' AND draft_year >= 2016 AND weight BETWEEN 225 AND 230) 
		,flying_twenty = (SELECT AVG(flying_twenty) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'IB' AND draft_year >= 2016 AND weight BETWEEN 225 AND 230) 
		,short_shuttle = (SELECT AVG(short_shuttle) FROM Analytics.dbo.r_input_draft_model_post_combine_measurables WHERE position = 'IB' AND draft_year >= 2016 AND weight BETWEEN 225 AND 230) 
	WHERE bane_player_id = 231572

/*
	--Add Kyler Murray measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET ten_split_vs_expected = 0
		,flying_twenty_vs_expected = 0
		,three_cone_vs_expected = 0
	WHERE bane_player_id = 87653

	--Add Marquise Brown measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET vertical_jump_vs_expected = 0
		,short_shuttle_vs_expected = 0
		,forty_dash_vs_expected = 0
	WHERE bane_player_id = 	257484

	--Add Jawaan Taylor measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET vertical_jump_vs_expected = 0
		,ten_split_vs_expected = 0
		,flying_twenty_vs_expected = 0
		,flying_ten_vs_expected = 0
		,short_shuttle_vs_expected = 0
	WHERE bane_player_id = 232135

	--Add Max Scharping measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET ten_split_vs_expected = 0
		,flying_twenty_vs_expected = 0
		,flying_ten_vs_expected = 0
	WHERE bane_player_id = 82212

	--Add Connor McGovern measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET ten_split_vs_expected = 0
		,flying_twenty_vs_expected = 0
		,flying_ten_vs_expected = 0
		,vertical_jump_vs_expected = 0
	WHERE bane_player_id = 231485

	--Add Yodny Cajuste measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET vertical_jump_vs_expected = 0
		,ten_split_vs_expected = 0
		,flying_twenty_vs_expected = 0
		,flying_ten_vs_expected = 0
		,short_shuttle_vs_expected = 0
	WHERE bane_player_id = 87551

	--Add Ben Powers measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET ten_split_vs_expected = 0
		,flying_twenty_vs_expected = 0
		,flying_ten_vs_expected = 0
	WHERE bane_player_id = 234540

	--Add Dawson Knox measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET long_shuttle_vs_expected = 0
	WHERE bane_player_id = 243695

	--Add Kaden Smith measurables
	UPDATE Analytics.dbo.r_input_draft_model_post_combine_measurables
	SET long_shuttle_vs_expected = 0
	WHERE bane_player_id = 260623
*/



