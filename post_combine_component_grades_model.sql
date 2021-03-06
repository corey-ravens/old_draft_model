



/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

This is the code for creating the post combine grades data tables. You also normalize the grades by scout to 
account for the differences in how they give them out.

Should it be done by grade level as well as scout and position and trait?

Inherently, players the the top of the board will have higher avg position specific grades

So this could potentially lead to some scouts (Eric, Chad, Vince) who tend to view the higher players to have higher average grades

And other scouts (Andrew) who tend to view the lower players to have lower average grades

v2 adds in the grade flags
v3 is for 2018 draft, it adds injured players salaries and updates the component skills, ids, etc.
v4 changes min to the "blended" max
v5 has some slight updates for 2019 rather than 2018 draft

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
SELECT DISTINCT position
FROM #temp_draft_board_positions_latest
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Adjust the skill ids on the evaluations table for the changes that have been made the last few years.

OUTPUT TABLES:
#temp_evaluations

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_evaluations exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_evaluations') IS NOT NULL
	DROP TABLE #temp_evaluations

	SELECT ev.id
		,CASE WHEN skill_id IN (1436,1524,1391) THEN 1436
			WHEN skill_id IN (1455,1427) THEN 1455
			WHEN skill_id IN (1525,1390) THEN 1525
			WHEN skill_id IN (1370,1439,1389) THEN 1370
			WHEN skill_id IN (1397,1401) THEN 1397
			WHEN skill_id IN (1402,1526) THEN 1402
			WHEN skill_id IN (1523,1396,1398) THEN 1523
			WHEN skill_id IN (1530,1422) THEN 1530
			WHEN skill_id IN (1361,1359) THEN 1361
			WHEN skill_id IN (1412,1408) THEN 1412 --,1429 ST tackling
			WHEN skill_id IN (1426,1441,1446) THEN 1426
			WHEN skill_id IN (1421,1363) THEN 1421
			WHEN skill_id IN (1420,1400,1448) THEN 1420
			WHEN skill_id IN (1532,1405) THEN 1532
			WHEN skill_id IN (1414,1418) THEN 1414
			WHEN skill_id IN (1368,1394) THEN 1368
			WHEN skill_id IN (1369,1430,1395) THEN 1369
			WHEN skill_id IN (1393,1451,1445) THEN 1393
			WHEN skill_id IN (1450,1528) THEN 1450
			WHEN skill_id IN (1539,1459,1388) THEN 1539
			WHEN skill_id IN (1366,1392) THEN 1366
			WHEN skill_id IN (1536,1417) THEN 1536
			WHEN skill_id IN (1533,1409) THEN 1533
			WHEN skill_id IN (1534,1410) THEN 1534
			WHEN skill_id IN (1535,1416) THEN 1535
			WHEN skill_id IN (1415,1537,1411) AND po.translation IN ('OB','IB') THEN 1535
			WHEN skill_id IN (1415) AND po.translation IS NULL THEN NULL
			WHEN skill_id IN (1415,1538) THEN 1538
			WHEN skill_id IN (1537,1411) THEN 1537
			WHEN skill_id IN (1364) AND po.translation IN ('DC','DS','IB') THEN 1534
			WHEN skill_id IN (1364) AND po.translation IS NULL THEN NULL
			WHEN skill_id IN (1364,1449,1529,1527,1496,1404) THEN 1449
			ELSE skill_id
		END AS skill_id
		,ev.grade_id
		,ev.report_id
		,ev.explanation
		,ev.created_at
		,ev.updated_at
		,ev.interview_id
	INTO #temp_evaluations
	FROM [BaneProductionAnalytics].[dbo].[evaluations] ev
	LEFT JOIN [BaneProductionAnalytics].[dbo].[reports] re
		ON ev.report_id = re.id
		AND re.is_deleted = 0
	LEFT JOIN [BaneProductionAnalytics].[dbo].[positions] po
		ON re.position_id = po.id
	WHERE ev.is_deleted = 0


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Create the master evaluations table.

OUTPUT TABLES:
#temp_evaluations_3_to_7
#temp_evaluations_overall

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

DECLARE @current_season INT
SELECT @current_season = (SELECT MAX(season) FROM Analytics.dbo.map_nfl_league_year_dates WHERE GETDATE() >= regular_season_start)

-- Check if #temp_evaluations_3_to_7 exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_evaluations_3_to_7') IS NOT NULL
	DROP TABLE #temp_evaluations_3_to_7

	SELECT re.id AS report_id
		,author_id AS scout_id
		,CASE WHEN re.created_at >= '05/01/2017' THEN 'NEW' ELSE 'OLD' END AS grade_distribution
		,player_id AS bane_player_id
		,pl.draft_year
		,CASE WHEN re.position_id IS NULL THEN 'NONE' ELSE CASE WHEN po.abbreviation = 'DE43' THEN 'OB' ELSE translation END END AS position		
		,ev.[id] AS evaluation_id
		,sk.id AS skill_id
		,sk.[code] AS skill_code
		,sk.[type] AS skill_type
		,sk.[name] AS skill_name
		,CAST(gr.value AS FLOAT) AS value
	INTO #temp_evaluations_3_to_7
	FROM [BaneProductionAnalytics].[dbo].[reports] re
	INNER JOIN [BaneProductionAnalytics].[dbo].[taggings] t 
		ON t.taggable_id = re.id 
		AND UPPER(t.taggable_type) = 'REPORT'
		AND t.tag_id = 11 --(it is a college report)
	INNER JOIN [BaneProductionAnalytics].[dbo].[players] pl 
		ON re.player_id = pl.id
	LEFT JOIN #temp_evaluations ev
		ON re.id=ev.report_id
	LEFT JOIN [BaneProductionAnalytics].[dbo].[skills] sk
		ON ev.skill_id=sk.id
	LEFT JOIN [BaneProductionAnalytics].[dbo].[grades] gr
		ON ev.grade_id=gr.id
	LEFT JOIN [BaneProductionAnalytics].[dbo].[users] us
		ON re.author_id=us.id    
	LEFT JOIN [BaneProductionAnalytics].[dbo].[positions] po
		ON re.position_id = po.id
     WHERE 1=1
       AND re.is_deleted = 0
	   AND UPPER(us.[type]) NOT LIKE '%COACH%'  
	   AND UPPER(re.[type]) <> 'SPECIAL TEAMS' 
	   AND translation NOT IN ('ST','PK','PT','LS')
       AND gr.scale_id = 5 --(it is a 3-7 component grade)
	   AND re.[type] IN ('fall','all star game','cross-check','post cc')
	   --AND MONTH(re.created_at) IN (6,7,8,9,10,11,12,1,2,3,4)
	   AND draft_year <= @current_season + 2

/*
SELECT *
FROM #temp_evaluations_3_to_7
WHERE skill_id = 1438
*/

-- Check if #temp_evaluations_overall exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_evaluations_overall') IS NOT NULL
	DROP TABLE #temp_evaluations_overall

	SELECT re.id AS report_id
		,author_id AS scout_id
		,'OLD' AS grade_distribution
		,player_id AS bane_player_id
		,pl.draft_year
		,CASE WHEN re.position_id IS NULL THEN 'NONE' ELSE CASE WHEN po.abbreviation = 'DE43' THEN 'OB' ELSE translation END END AS position
		,NULL AS evaluation_id
		,-999 AS skill_id
		,'GRADE' AS skill_code
		,'overall grade' AS skill_type
		,'overall grade' AS skill_name
		,CASE WHEN CAST(gr.value AS NUMERIC (3,1)) IN (8.0) THEN 100
			WHEN CAST(gr.value AS NUMERIC (3,1)) IN (7.0) THEN 92
			WHEN CAST(gr.value AS NUMERIC (3,1)) IN (6.9) THEN 82
			WHEN CAST(gr.value AS NUMERIC (3,1)) IN (6.7,6.5) THEN 71
			WHEN CAST(gr.value AS NUMERIC (3,1)) IN (6.4,6.3,6.2) THEN 64
			WHEN CAST(gr.value AS NUMERIC (3,1)) IN (6.1,6.0) THEN 58
			WHEN CAST(gr.value AS NUMERIC (3,1)) IN (5.9) THEN 53.5
			WHEN CAST(gr.value AS NUMERIC (3,1)) IN (5.8) THEN 50
			WHEN CAST(gr.value AS NUMERIC (3,1)) IN (5.7) THEN 47
			WHEN CAST(gr.value AS NUMERIC (3,1)) IN (5.6) THEN 45.5
			WHEN CAST(gr.value AS NUMERIC (3,1)) IN (5.4) THEN 43
			WHEN CAST(gr.value AS NUMERIC (3,1)) IN (5.2) THEN 41
			WHEN CAST(gr.value AS NUMERIC (3,1)) IN (5.0) THEN 40
			ELSE NULL
		END AS value      
	INTO #temp_evaluations_overall
	FROM [BaneProductionAnalytics].[dbo].[reports] re
	INNER JOIN [BaneProductionAnalytics].[dbo].[taggings] t 
		ON t.taggable_id = re.id 
		AND UPPER(t.taggable_type) = 'REPORT'
		AND t.tag_id = 11 --(it is a college report)
	INNER JOIN [BaneProductionAnalytics].[dbo].[players] pl 
		ON re.player_id = pl.id
	LEFT JOIN [BaneProductionAnalytics].[dbo].[grades] gr
		ON re.grade_id=gr.id
	LEFT JOIN [BaneProductionAnalytics].[dbo].[users] us
		ON re.author_id=us.id    
	LEFT JOIN [BaneProductionAnalytics].[dbo].[positions] po
		ON re.position_id = po.id           
	WHERE 1=1
		AND re.is_deleted = 0
		AND UPPER(us.[type]) NOT LIKE '%COACH%'  
		AND UPPER(re.[type]) <> 'SPECIAL TEAMS' 
		AND translation NOT IN ('ST','PK','PT','LS')
		AND gr.scale_id IN (4,6) --(it is an overall grade)
		AND re.[type] IN ('fall','all star game','cross-check','post cc')
		--AND MONTH(re.created_at) IN (6,7,8,9,10,11,12,1,2,3,4)
		AND draft_year >= 2005
		AND draft_year <= @current_season + 2


/*
SELECT *
FROM #temp_evaluations_overall
WHERE bane_player_id = 48730
*/

/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Append the overall grades to the 3 to 7 scale grades.

OUTPUT TABLES:
#temp_evaluations_append

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_evaluations_append exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_evaluations_append') IS NOT NULL
	DROP TABLE #temp_evaluations_append

	SELECT *
	INTO #temp_evaluations_append
	FROM #temp_evaluations_3_to_7

	INSERT INTO #temp_evaluations_append
	SELECT *
	FROM #temp_evaluations_overall

--Add cover grades for Rashan Gary
INSERT INTO #temp_evaluations_append VALUES (51972475,122,'NEW',230831,2019,'OB',NULL,1536,'PS-COVABIL/RANGE','position specifics','Cover Ability / Range',5.5)
INSERT INTO #temp_evaluations_append VALUES (51974977,95,'NEW',230831,2019,'OB',NULL,1536,'PS-COVABIL/RANGE','position specifics','Cover Ability / Range',6)
INSERT INTO #temp_evaluations_append VALUES (51975703,4,'NEW',230831,2019,'OB',NULL,1536,'PS-COVABIL/RANGE','position specifics','Cover Ability / Range',5)

--Add pursuit grades for Rashan Gary
INSERT INTO #temp_evaluations_append VALUES (51972475,122,'NEW',230831,2019,'OB',NULL,1419,'PS-PUR','position specifics','Pursuit',5)
INSERT INTO #temp_evaluations_append VALUES (51974977,95,'NEW',230831,2019,'OB',NULL,1419,'PS-PUR','position specifics','Pursuit',5)
INSERT INTO #temp_evaluations_append VALUES (51975703,4,'NEW',230831,2019,'OB',NULL,1419,'PS-PUR','position specifics','Pursuit',6)

--Add run defense/shed grades for Rashan Gary
INSERT INTO #temp_evaluations_append VALUES (51972475,122,'NEW',230831,2019,'OB',NULL,1535,'PS-POAR/SHEDB','position specifics','POA Run / Shed Blockers',6)
INSERT INTO #temp_evaluations_append VALUES (51974977,95,'NEW',230831,2019,'OB',NULL,1535,'PS-POAR/SHEDB','position specifics','POA Run / Shed Blockers',5.5)
INSERT INTO #temp_evaluations_append VALUES (51975703,4,'NEW',230831,2019,'OB',NULL,1535,'PS-POAR/SHEDB','position specifics','POA Run / Shed Blockers',6)

/*
SELECT u.last_name, o.*
FROM #temp_evaluations_append o
inner join BaneProductionAnalytics.dbo.users u
on o.scout_id = u.id
WHERE bane_player_id IN (48730,67847,73542,87917,58615)
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Take the skill id average in each report. This takes care of the fields that have been merged into one (like
hands and ball awareness merging together into ball awareness for DBs).

OUTPUT TABLES:
#temp_evaluations_master

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_evaluations_master exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_evaluations_master') IS NOT NULL
	DROP TABLE #temp_evaluations_master

	SELECT report_id
		,scout_id
		,grade_distribution
		,bane_player_id
		,draft_year
		,position		
		--,evaluation_id
		,skill_id
		,skill_code
		,skill_type
		,skill_name
		,AVG([value]) AS [value]
	INTO #temp_evaluations_master
	FROM #temp_evaluations_append
	GROUP BY report_id
		,scout_id
		,grade_distribution
		,bane_player_id
		,draft_year
		,position		
		--,evaluation_id
		,skill_id
		,skill_code
		,skill_type
		,skill_name

/*
SELECT distinct skill_name
FROM #temp_evaluations_master
WHERE bane_player_id IN (48730,67847,73542,87917,58615)
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Count the number of skill grades per scout per position. So that you can exclude any where a scout
hasn't graded at least 5 players.

OUTPUT TABLES:
#temp_evaluations_scout_counts

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_evaluations_scout_counts exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_evaluations_scout_counts') IS NOT NULL
	DROP TABLE #temp_evaluations_scout_counts

	SELECT scout_id
		,grade_distribution
		,skill_type
		,position
		,COUNT(*) AS evaluation_count
		,AVG(value) AS evaluation_mean
		,STDEVP(value) AS evaluation_stdev
	INTO #temp_evaluations_scout_counts
	FROM #temp_evaluations_master
	WHERE skill_name NOT IN ('Durability','Size','Speed','Playing Speed')
	GROUP BY scout_id
		,grade_distribution
		,skill_type
		,position


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Join in the evaluation count so you can exclude the evaluations where the scout hasn't done enough evaluations.

OUTPUT TABLES:
#temp_evaluations_updated

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_evaluations_updated exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_evaluations_updated') IS NOT NULL
	DROP TABLE #temp_evaluations_updated

	SELECT ev.*
	INTO #temp_evaluations_updated
	FROM #temp_evaluations_master ev
	INNER JOIN #temp_evaluations_scout_counts co
		ON ev.scout_id = co.scout_id
		AND ev.skill_type = co.skill_type
		AND ev.grade_distribution = co.grade_distribution
		AND ev.position = co.position
		AND co.evaluation_count >= 5
	WHERE skill_name NOT IN ('Durability','Size','Speed','Playing Speed')

/*
SELECT *
FROM #temp_evaluations_updated
WHERE bane_player_id = 87917
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Create the table for generating the grade adjustments in R.

OUTPUT TABLES:
Analytics.dbo.r_input_post_combine_grades_to_adjust

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

/*
DECLARE @current_season INT
SELECT @current_season = (SELECT MAX(season) FROM Analytics.dbo.map_nfl_league_year_dates WHERE GETDATE() >= regular_season_start)

--DELETE FROM Analytics.dbo.r_input_draft_model_post_combine_grades_to_adjust
--WHERE draft_model_year = @current_season + 1
	--AND created_date = (SELECT MAX(created_date) FROM Analytics.dbo.r_input_draft_model_post_combine_grades_to_adjust WHERE draft_model_year = @current_season)

INSERT INTO Analytics.dbo.r_input_draft_model_post_combine_grades_to_adjust
	SELECT report_id
		,scout_id
		,grade_distribution
		,bane_player_id
		,draft_year
		,position		
		,NULL AS evaluation_id
		,skill_id
		,skill_code
		,skill_type
		,skill_name
		,[value]
		,@current_season + 1 AS draft_model_year
		,GETDATE() AS created_date
	FROM #temp_evaluations_updated
	WHERE skill_name NOT IN ('Durability','Size','Speed','Playing Speed')
*/

/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Run the below R Program to find the grade adjustment factors.

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

--X:\R\college_scouting\draft_modeling\post_combine\adjusted_grades\post_combine_grade_adjustments


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Join in the scout grade adjustment factors to create the adjusted grades

OUTPUT TABLES:
#temp_evaluations_adjusted

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_evaluations_adjusted exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_evaluations_adjusted') IS NOT NULL
	DROP TABLE #temp_evaluations_adjusted

	SELECT ev.*
		,(value - evaluation_mean) / NULLIF(evaluation_stdev,0) AS value_z_score
		,CASE WHEN ev.scout_id = 2 THEN value ELSE value - COALESCE(adjustment,0) END AS value_adjusted --Scout ID 2 does not get a grade adjustment, it is the zero in the regression
	INTO #temp_evaluations_adjusted
	FROM #temp_evaluations_master ev
	LEFT JOIN Analytics.dbo.r_output_draft_model_post_combine_grade_adjustments ga
		ON ev.scout_id = ga.scout_id
		AND ev.grade_distribution = ga.grade_distribution
		AND ev.skill_type = ga.skill_type
		AND ev.position = ga.position
		AND ga.created_date = (SELECT MAX(created_date) FROM Analytics.dbo.r_output_draft_model_post_combine_grade_adjustments)
	LEFT JOIN #temp_evaluations_scout_counts sc
		ON ev.scout_id = sc.scout_id
		AND ev.grade_distribution = sc.grade_distribution
		AND ev.skill_type = sc.skill_type
		AND ev.position = sc.position

/*
SELECT *
FROM #temp_evaluations_adjusted
WHERE bane_player_id IN (48730,67847,73542,87917,58615,257484)
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Find the mean adjusted grade for every player / skill.

OUTPUT TABLES:
#temp_evaluations_player_means

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_evaluations_player_means exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_evaluations_player_means') IS NOT NULL
	DROP TABLE #temp_evaluations_player_means

	SELECT bane_player_id
		,skill_id
		,skill_code
		,MAX(value) AS value_max
		,AVG(value) AS value_mean
		,MIN(value) AS value_min
		,MAX(value_adjusted) AS value_adjusted_max
		,AVG(value_adjusted) AS value_adjusted_mean
		,MIN(value_adjusted) AS value_adjusted_min
		,MAX(value_z_score) AS value_z_score_max
		,AVG(value_z_score) AS value_z_score_mean
		,MIN(value_z_score) AS value_z_score_min
		,STDEVP(value_z_score) AS value_z_score_stdev
		,VAR(value_z_score) AS value_z_score_variance
	INTO #temp_evaluations_player_means
	FROM #temp_evaluations_adjusted
	GROUP BY bane_player_id
		,skill_id
		,skill_code

/*
SELECT *
FROM #temp_evaluations_player_means
WHERE bane_player_id IN (48730,67847,73542,87917,58615)
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Pivot the player grades so you have one row per player with all of his mean grades.
Then you can join this table to his eventual pro grades and model based off of that.

OUTPUT TABLES:
#temp_evaluations_pivot_mean

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_evaluations_pivot_mean exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_evaluations_pivot_mean') IS NOT NULL
	DROP TABLE #temp_evaluations_pivot_mean

    SELECT bane_player_id

		,[CF-AA] AS cf_athletic_ability_mean
		,[CF-CMP] AS cf_competitiveness_mean
		,[CF-PLYSPD] AS cf_playing_speed_mean
		,[CF-PRO] AS cf_production_mean
		,[CF-SIZ] AS cf_size_mean
		,[CF-ST] AS cf_special_teams_mean
		,[CF-STR/EXPL] AS cf_strength_explosion_mean

		,[PS-ACC] AS ps_accuracy_mean
		,[PS-ARMSTR] AS ps_arm_strength_mean
		,[PS-BALLAWAR/PLAYTHEBALL] AS ps_ball_awareness_playing_the_ball_mean
		,[PS-BALLPROT] AS ps_ball_protection_mean
		,[PS-BLTZ/PICK] AS ps_blitz_pickup_fbi_mean
		,[PS-BLOCK] AS ps_blocking_mean
		,[PS-CLSONBALL/RNG] AS ps_close_on_the_ball_range_mean
		,[PS-COVABIL/RANGE] AS ps_cover_ability_range_mean
		,[PS-DEC/M] AS ps_decision_making_mean
		,[PS-DIA/INST] AS ps_diagnose_instincts_mean
		,[PS-DRP/SET/PKTA] AS ps_drops_setup_pocket_awareness_mean
		,[PS-HNDUSE] AS ps_hand_use_mean
		,[PS-HNDS/BDYA] AS ps_hands_body_adjustment_mean
		,[PS-INSRUN] AS ps_inside_run_mean
		,[PS-LAT/M] AS ps_lateral_movement_mean
		,[PS-MENTAL/AWARE] AS ps_mental_awareness_mean
		,[PS-MM/C] AS ps_mm_cover_mean
		,[PS-OUTRUN] AS ps_outside_run_mean
		,[PS-PASS/BLK] AS ps_pass_blocking_mean
		,[PS-PASS/R] AS ps_pass_rush_ability_mean
		,[PS-PLY/M] AS ps_play_making_mean
		,[PS-POAR/SHEDB] AS ps_poa_run_shed_blockers_mean
		,[PS-PULL] AS ps_pulling_mean
		,[PS-PUR] AS ps_pursuit_mean
		,[PS-RLS] AS ps_release_mean
		,[PS-ROU] AS ps_routes_mean
		,[PS-RAC] AS ps_run_after_catch_mean
		,[PS-RUNDEF/2GAP] AS ps_run_defense_2gap_mean
		,[PS-SHED/HNDUSE] AS ps_shed_hand_use_mean
		,[PS-SUS/RUNB] AS ps_sustain_run_blocking_mean
		,[PS-TKL] AS ps_tackling_mean
		,[PS-VERSA] AS ps_versatility_mean
		,[PS-VIS] AS ps_vision_instincts_mean
		,[PS-ZNE/C] AS ps_zone_cover_mean

		,[S] AS sticd_speed_mean
		,[T] AS sticd_toughness_mean
		,[I] AS sticd_instincts_mean
		,[C] AS sticd_character_mean
		,[D] AS sticd_durability_mean

		,[I-OFFCHAR] AS intangibles_off_field_character_mean
		,[I-ONCHAR] AS intangibles_on_field_character_mean

		,[GRADE] as grade_overall_mean
	INTO #temp_evaluations_pivot_mean
	FROM (
		SELECT bane_player_id
			,skill_code
			,value_z_score_mean
	FROM #temp_evaluations_player_means WHERE skill_code IN ('CF-AA','CF-CMP','CF-PLYSPD','CF-PRO','CF-SIZ','CF-ST','CF-STR/EXPL','I-OFFCHAR','I-ONCHAR','PS-ACC','PS-ARMSTR','PS-BALLAWAR/PLAYTHEBALL','PS-BALLPROT','PS-BLTZ/PICK','PS-BLOCK','PS-CLSONBALL/RNG','PS-COVABIL/RANGE','PS-DEC/M','PS-DIA/INST','PS-DRP/SET/PKTA','PS-HNDUSE','PS-HNDS/BDYA','PS-INSRUN','PS-LAT/M','PS-MENTAL/AWARE','PS-MM/C','PS-OUTRUN','PS-PASS/BLK','PS-PASS/R','PS-PLY/M','PS-POAR/SHEDB','PS-PULL','PS-PUR','PS-RLS','PS-ROU','PS-RAC','PS-RUNDEF/2GAP','PS-SHED/HNDUSE','PS-SUS/RUNB','PS-TKL','PS-VERSA','PS-VIS','PS-ZNE/C','C','D','I','S','T','GRADE')) up
    PIVOT (MAX(value_z_score_mean) FOR skill_code IN ([CF-AA],[CF-CMP],[CF-PLYSPD],[CF-PRO],[CF-SIZ],[CF-ST],[CF-STR/EXPL],[I-OFFCHAR],[I-ONCHAR],[PS-ACC],[PS-ARMSTR],[PS-BALLAWAR/PLAYTHEBALL],[PS-BALLPROT],[PS-BLTZ/PICK],[PS-BLOCK],[PS-CLSONBALL/RNG],[PS-COVABIL/RANGE],[PS-DEC/M],[PS-DIA/INST],[PS-DRP/SET/PKTA],[PS-HNDUSE],[PS-HNDS/BDYA],[PS-INSRUN],[PS-LAT/M],[PS-MENTAL/AWARE],[PS-MM/C],[PS-OUTRUN],[PS-PASS/BLK],[PS-PASS/R],[PS-PLY/M],[PS-POAR/SHEDB],[PS-PULL],[PS-PUR],[PS-RLS],[PS-ROU],[PS-RAC],[PS-RUNDEF/2GAP],[PS-SHED/HNDUSE],[PS-SUS/RUNB],[PS-TKL],[PS-VERSA],[PS-VIS],[PS-ZNE/C],[C],[D],[I],[S],[T],[GRADE])) AS pvt
	ORDER BY bane_player_id

/*
SELECT *
FROM #temp_evaluations_pivot_mean 
*/

/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Pivot the player grades so you have one row per player with all of his max grades.
Then you can join this table to his eventual pro grades and model based off of that.

OUTPUT TABLES:
#temp_evaluations_pivot_max

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_evaluations_pivot_max exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_evaluations_pivot_max') IS NOT NULL
	DROP TABLE #temp_evaluations_pivot_max

    SELECT bane_player_id

		,[CF-AA] AS cf_athletic_ability_max
		,[CF-CMP] AS cf_competitiveness_max
		,[CF-PLYSPD] AS cf_playing_speed_max
		,[CF-PRO] AS cf_production_max
		,[CF-SIZ] AS cf_size_max
		,[CF-ST] AS cf_special_teams_max
		,[CF-STR/EXPL] AS cf_strength_explosion_max

		,[PS-ACC] AS ps_accuracy_max
		,[PS-ARMSTR] AS ps_arm_strength_max
		,[PS-BALLAWAR/PLAYTHEBALL] AS ps_ball_awareness_playing_the_ball_max
		,[PS-BALLPROT] AS ps_ball_protection_max
		,[PS-BLTZ/PICK] AS ps_blitz_pickup_fbi_max
		,[PS-BLOCK] AS ps_blocking_max
		,[PS-CLSONBALL/RNG] AS ps_close_on_the_ball_range_max
		,[PS-COVABIL/RANGE] AS ps_cover_ability_range_max
		,[PS-DEC/M] AS ps_decision_making_max
		,[PS-DIA/INST] AS ps_diagnose_instincts_max
		,[PS-DRP/SET/PKTA] AS ps_drops_setup_pocket_awareness_max
		,[PS-HNDUSE] AS ps_hand_use_max
		,[PS-HNDS/BDYA] AS ps_hands_body_adjustment_max
		,[PS-INSRUN] AS ps_inside_run_max
		,[PS-LAT/M] AS ps_lateral_movement_max
		,[PS-MENTAL/AWARE] AS ps_mental_awareness_max
		,[PS-MM/C] AS ps_mm_cover_max
		,[PS-OUTRUN] AS ps_outside_run_max
		,[PS-PASS/BLK] AS ps_pass_blocking_max
		,[PS-PASS/R] AS ps_pass_rush_ability_max
		,[PS-PLY/M] AS ps_play_making_max
		,[PS-POAR/SHEDB] AS ps_poa_run_shed_blockers_max
		,[PS-PULL] AS ps_pulling_max
		,[PS-PUR] AS ps_pursuit_max
		,[PS-RLS] AS ps_release_max
		,[PS-ROU] AS ps_routes_max
		,[PS-RAC] AS ps_run_after_catch_max
		,[PS-RUNDEF/2GAP] AS ps_run_defense_2gap_max
		,[PS-SHED/HNDUSE] AS ps_shed_hand_use_max
		,[PS-SUS/RUNB] AS ps_sustain_run_blocking_max
		,[PS-TKL] AS ps_tackling_max
		,[PS-VERSA] AS ps_versatility_max
		,[PS-VIS] AS ps_vision_instincts_max
		,[PS-ZNE/C] AS ps_zone_cover_max

		,[S] AS sticd_speed_max
		,[T] AS sticd_toughness_max
		,[I] AS sticd_instincts_max
		,[C] AS sticd_character_max
		,[D] AS sticd_durability_max

		,[I-OFFCHAR] AS intangibles_off_field_character_max
		,[I-ONCHAR] AS intangibles_on_field_character_max

		,[GRADE] as grade_overall_max
	INTO #temp_evaluations_pivot_max
	FROM (
		SELECT bane_player_id
			,skill_code
			,value_z_score_max
	FROM #temp_evaluations_player_means WHERE skill_code IN ('CF-AA','CF-CMP','CF-PLYSPD','CF-PRO','CF-SIZ','CF-ST','CF-STR/EXPL','I-OFFCHAR','I-ONCHAR','PS-ACC','PS-ARMSTR','PS-BALLAWAR/PLAYTHEBALL','PS-BALLPROT','PS-BLTZ/PICK','PS-BLOCK','PS-CLSONBALL/RNG','PS-COVABIL/RANGE','PS-DEC/M','PS-DIA/INST','PS-DRP/SET/PKTA','PS-HNDUSE','PS-HNDS/BDYA','PS-INSRUN','PS-LAT/M','PS-MENTAL/AWARE','PS-MM/C','PS-OUTRUN','PS-PASS/BLK','PS-PASS/R','PS-PLY/M','PS-POAR/SHEDB','PS-PULL','PS-PUR','PS-RLS','PS-ROU','PS-RAC','PS-RUNDEF/2GAP','PS-SHED/HNDUSE','PS-SUS/RUNB','PS-TKL','PS-VERSA','PS-VIS','PS-ZNE/C','C','D','I','S','T','GRADE')) up
    PIVOT (MAX(value_z_score_max) FOR skill_code IN ([CF-AA],[CF-CMP],[CF-PLYSPD],[CF-PRO],[CF-SIZ],[CF-ST],[CF-STR/EXPL],[I-OFFCHAR],[I-ONCHAR],[PS-ACC],[PS-ARMSTR],[PS-BALLAWAR/PLAYTHEBALL],[PS-BALLPROT],[PS-BLTZ/PICK],[PS-BLOCK],[PS-CLSONBALL/RNG],[PS-COVABIL/RANGE],[PS-DEC/M],[PS-DIA/INST],[PS-DRP/SET/PKTA],[PS-HNDUSE],[PS-HNDS/BDYA],[PS-INSRUN],[PS-LAT/M],[PS-MENTAL/AWARE],[PS-MM/C],[PS-OUTRUN],[PS-PASS/BLK],[PS-PASS/R],[PS-PLY/M],[PS-POAR/SHEDB],[PS-PULL],[PS-PUR],[PS-RLS],[PS-ROU],[PS-RAC],[PS-RUNDEF/2GAP],[PS-SHED/HNDUSE],[PS-SUS/RUNB],[PS-TKL],[PS-VERSA],[PS-VIS],[PS-ZNE/C],[C],[D],[I],[S],[T],[GRADE])) AS pvt
	ORDER BY bane_player_id

/*
SELECT *
FROM #temp_evaluations_pivot_mean 
*/

/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Pivot the player grades so you have one row per player with all of his min grades.
Then you can join this table to his eventual pro grades and model based off of that.

OUTPUT TABLES:
#temp_evaluations_pivot_min

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

-- Check if #temp_evaluations_pivot_min exists, if it does drop it
IF OBJECT_ID('tempdb..#temp_evaluations_pivot_min') IS NOT NULL
	DROP TABLE #temp_evaluations_pivot_min

   SELECT bane_player_id

		,[CF-AA] AS cf_athletic_ability_min
		,[CF-CMP] AS cf_competitiveness_min
		,[CF-PLYSPD] AS cf_playing_speed_min
		,[CF-PRO] AS cf_production_min
		,[CF-SIZ] AS cf_size_min
		,[CF-ST] AS cf_special_teams_min
		,[CF-STR/EXPL] AS cf_strength_explosion_min

		,[PS-ACC] AS ps_accuracy_min
		,[PS-ARMSTR] AS ps_arm_strength_min
		,[PS-BALLAWAR/PLAYTHEBALL] AS ps_ball_awareness_playing_the_ball_min
		,[PS-BALLPROT] AS ps_ball_protection_min
		,[PS-BLTZ/PICK] AS ps_blitz_pickup_fbi_min
		,[PS-BLOCK] AS ps_blocking_min
		,[PS-CLSONBALL/RNG] AS ps_close_on_the_ball_range_min
		,[PS-COVABIL/RANGE] AS ps_cover_ability_range_min
		,[PS-DEC/M] AS ps_decision_making_min
		,[PS-DIA/INST] AS ps_diagnose_instincts_min
		,[PS-DRP/SET/PKTA] AS ps_drops_setup_pocket_awareness_min
		,[PS-HNDUSE] AS ps_hand_use_min
		,[PS-HNDS/BDYA] AS ps_hands_body_adjustment_min
		,[PS-INSRUN] AS ps_inside_run_min
		,[PS-LAT/M] AS ps_lateral_movement_min
		,[PS-MENTAL/AWARE] AS ps_mental_awareness_min
		,[PS-MM/C] AS ps_mm_cover_min
		,[PS-OUTRUN] AS ps_outside_run_min
		,[PS-PASS/BLK] AS ps_pass_blocking_min
		,[PS-PASS/R] AS ps_pass_rush_ability_min
		,[PS-PLY/M] AS ps_play_making_min
		,[PS-POAR/SHEDB] AS ps_poa_run_shed_blockers_min
		,[PS-PULL] AS ps_pulling_min
		,[PS-PUR] AS ps_pursuit_min
		,[PS-RLS] AS ps_release_min
		,[PS-ROU] AS ps_routes_min
		,[PS-RAC] AS ps_run_after_catch_min
		,[PS-RUNDEF/2GAP] AS ps_run_defense_2gap_min
		,[PS-SHED/HNDUSE] AS ps_shed_hand_use_min
		,[PS-SUS/RUNB] AS ps_sustain_run_blocking_min
		,[PS-TKL] AS ps_tackling_min
		,[PS-VERSA] AS ps_versatility_min
		,[PS-VIS] AS ps_vision_instincts_min
		,[PS-ZNE/C] AS ps_zone_cover_min

		,[S] AS sticd_speed_min
		,[T] AS sticd_toughness_min
		,[I] AS sticd_instincts_min
		,[C] AS sticd_character_min
		,[D] AS sticd_durability_min

		,[I-OFFCHAR] AS intangibles_off_field_character_min
		,[I-ONCHAR] AS intangibles_on_field_character_min

		,[GRADE] as grade_overall_min
	INTO #temp_evaluations_pivot_min
	FROM (
		SELECT bane_player_id
			,skill_code
			,value_z_score_min
	FROM #temp_evaluations_player_means WHERE skill_code IN ('CF-AA','CF-CMP','CF-PLYSPD','CF-PRO','CF-SIZ','CF-ST','CF-STR/EXPL','I-OFFCHAR','I-ONCHAR','PS-ACC','PS-ARMSTR','PS-BALLAWAR/PLAYTHEBALL','PS-BALLPROT','PS-BLTZ/PICK','PS-BLOCK','PS-CLSONBALL/RNG','PS-COVABIL/RANGE','PS-DEC/M','PS-DIA/INST','PS-DRP/SET/PKTA','PS-HNDUSE','PS-HNDS/BDYA','PS-INSRUN','PS-LAT/M','PS-MENTAL/AWARE','PS-MM/C','PS-OUTRUN','PS-PASS/BLK','PS-PASS/R','PS-PLY/M','PS-POAR/SHEDB','PS-PULL','PS-PUR','PS-RLS','PS-ROU','PS-RAC','PS-RUNDEF/2GAP','PS-SHED/HNDUSE','PS-SUS/RUNB','PS-TKL','PS-VERSA','PS-VIS','PS-ZNE/C','C','D','I','S','T','GRADE')) up
    PIVOT (MAX(value_z_score_min) FOR skill_code IN ([CF-AA],[CF-CMP],[CF-PLYSPD],[CF-PRO],[CF-SIZ],[CF-ST],[CF-STR/EXPL],[I-OFFCHAR],[I-ONCHAR],[PS-ACC],[PS-ARMSTR],[PS-BALLAWAR/PLAYTHEBALL],[PS-BALLPROT],[PS-BLTZ/PICK],[PS-BLOCK],[PS-CLSONBALL/RNG],[PS-COVABIL/RANGE],[PS-DEC/M],[PS-DIA/INST],[PS-DRP/SET/PKTA],[PS-HNDUSE],[PS-HNDS/BDYA],[PS-INSRUN],[PS-LAT/M],[PS-MENTAL/AWARE],[PS-MM/C],[PS-OUTRUN],[PS-PASS/BLK],[PS-PASS/R],[PS-PLY/M],[PS-POAR/SHEDB],[PS-PULL],[PS-PUR],[PS-RLS],[PS-ROU],[PS-RAC],[PS-RUNDEF/2GAP],[PS-SHED/HNDUSE],[PS-SUS/RUNB],[PS-TKL],[PS-VERSA],[PS-VIS],[PS-ZNE/C],[C],[D],[I],[S],[T],[GRADE])) AS pvt
	ORDER BY bane_player_id

/*
SELECT *
FROM #temp_evaluations_pivot_mean 
*/


/*---------------------------------------------------------------------------------------------------------------------------------------------------------------------

Join his pivoted college grades to the pivoted pro grades table.

OUTPUT TABLES:
Analytics.dbo.r_input_draft_model_post_combine_grades

----------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

DECLARE @current_season INT
SELECT @current_season = (SELECT MAX(season) FROM Analytics.dbo.map_nfl_league_year_dates WHERE GETDATE() >= regular_season_start)

--DELETE FROM Analytics.dbo.r_input_draft_model_post_combine_grades
--WHERE created_date = (SELECT MAX(created_date) FROM Analytics.dbo.r_input_draft_model_post_combine_grades WHERE draft_model_year = 2018)

INSERT INTO Analytics.dbo.r_input_draft_model_post_combine_grades
	SELECT ep_mean.bane_player_id
		,CONCAT(pl.last_name,', ',pl.goes_by) AS player
		,CASE WHEN pl.draft_year <= @current_season THEN pl.draft_year
			WHEN pl.draft_year IN (@current_season + 1,@current_season + 2) THEN @current_season + 1
			ELSE NULL
		END AS draft_year
		,NULL AS age_at_draft
		,CASE WHEN COALESCE(pg.position,pn.translation) = 'OH' THEN 8
			WHEN pg.position = 'DE' AND (pn.abbreviation = 'DE43' OR position_defense = 'DE43') THEN 45
			WHEN COALESCE(pg.position,pn.translation) = 'DC' AND position_defense = 'NB' THEN 20
			WHEN COALESCE(pg.position,pn.translation) = 'WR' AND receiver_type = 'SLOT' THEN 999
			ELSE COALESCE(pg.position_id,pl.position_id)
		END AS position_id 
		,CASE WHEN COALESCE(pg.position,dbp.position,pn.translation) = 'OH' THEN 'RB'
			WHEN pg.position = 'DE' AND (pn.abbreviation = 'DE43' OR position_defense = 'DE43') THEN 'OB'
			WHEN COALESCE(pg.position,dbp.position,pn.translation) = 'DC' AND position_defense = 'NB' THEN 'DC_SLOT'
			WHEN COALESCE(pg.position,dbp.position,pn.translation) = 'WR' AND receiver_type = 'SLOT' THEN 'WR_SLOT'
			ELSE COALESCE(pg.position,dbp.position,pn.translation)
		END AS position
		,grade_overall_max
		,grade_overall_mean
		,grade_overall_min
		,cf_athletic_ability_max
		,cf_athletic_ability_mean
		,cf_athletic_ability_max * 0.75 + cf_athletic_ability_mean * 0.25 AS cf_athletic_ability_min
		,cf_competitiveness_max
		,cf_competitiveness_mean
		,cf_competitiveness_max * 0.75 + cf_competitiveness_mean * 0.25 AS cf_competitiveness_min
		,cf_playing_speed_max
		,cf_playing_speed_mean
		,cf_playing_speed_max * 0.75 + cf_playing_speed_mean * 0.25 AS cf_playing_speed_min
		,cf_production_max
		,cf_production_mean
		,cf_production_max * 0.75 + cf_production_mean * 0.25 AS cf_production_min
		,cf_size_max
		,cf_size_mean
		,cf_size_max * 0.75 + cf_size_mean * 0.25 AS cf_size_min
		,cf_special_teams_max
		,cf_special_teams_mean
		,cf_special_teams_max * 0.75 + cf_special_teams_mean * 0.25 AS cf_special_teams_min
		,cf_strength_explosion_max
		,cf_strength_explosion_mean
		,cf_strength_explosion_max * 0.75 + cf_strength_explosion_mean * 0.25 AS cf_strength_explosion_min
		,ps_accuracy_max
		,ps_accuracy_mean
		,ps_accuracy_max * 0.75 + ps_accuracy_mean * 0.25 AS ps_accuracy_min
		,ps_arm_strength_max
		,ps_arm_strength_mean
		,ps_arm_strength_max * 0.75 + ps_arm_strength_mean * 0.25 AS ps_arm_strength_min
		,ps_ball_awareness_playing_the_ball_max
		,ps_ball_awareness_playing_the_ball_mean
		,ps_ball_awareness_playing_the_ball_max * 0.75 + ps_ball_awareness_playing_the_ball_mean * 0.25 AS ps_ball_awareness_playing_the_ball_min
		,ps_ball_protection_max
		,ps_ball_protection_mean
		,ps_ball_protection_max * 0.75 + ps_ball_protection_mean * 0.25 AS ps_ball_protection_min
		,ps_blitz_pickup_fbi_max
		,ps_blitz_pickup_fbi_mean
		,ps_blitz_pickup_fbi_max * 0.75 + ps_blitz_pickup_fbi_mean * 0.25 AS ps_blitz_pickup_fbi_min
		,ps_blocking_max
		,ps_blocking_mean
		,ps_blocking_max * 0.75 + ps_blocking_mean * 0.25 AS ps_blocking_min
		,ps_close_on_the_ball_range_max
		,ps_close_on_the_ball_range_mean
		,ps_close_on_the_ball_range_max * 0.75 + ps_close_on_the_ball_range_mean * 0.25 AS ps_close_on_the_ball_range_min
		,ps_cover_ability_range_max
		,ps_cover_ability_range_mean
		,ps_cover_ability_range_max * 0.75 + ps_cover_ability_range_mean * 0.25 AS ps_cover_ability_range_min
		,ps_decision_making_max
		,ps_decision_making_mean
		,ps_decision_making_max * 0.75 + ps_decision_making_mean * 0.25 AS ps_decision_making_min
		,ps_diagnose_instincts_max
		,ps_diagnose_instincts_mean
		,ps_diagnose_instincts_max * 0.75 + ps_diagnose_instincts_mean * 0.25 AS ps_diagnose_instincts_min
		,ps_drops_setup_pocket_awareness_max
		,ps_drops_setup_pocket_awareness_mean
		,ps_drops_setup_pocket_awareness_max * 0.75 + ps_drops_setup_pocket_awareness_mean * 0.25 AS ps_drops_setup_pocket_awareness_min
		,ps_hand_use_max
		,ps_hand_use_mean
		,ps_hand_use_max * 0.75 + ps_hand_use_mean * 0.25 AS ps_hand_use_min
		,ps_hands_body_adjustment_max
		,ps_hands_body_adjustment_mean
		,ps_hands_body_adjustment_max * 0.75 + ps_hands_body_adjustment_mean * 0.25 AS ps_hands_body_adjustment_min
		,ps_inside_run_max
		,ps_inside_run_mean
		,ps_inside_run_max * 0.75 + ps_inside_run_mean * 0.25 AS ps_inside_run_min
		,ps_lateral_movement_max
		,ps_lateral_movement_mean
		,ps_lateral_movement_max * 0.75 + ps_lateral_movement_mean * 0.25 AS ps_lateral_movement_min
		,ps_mental_awareness_max
		,ps_mental_awareness_mean
		,ps_mental_awareness_max * 0.75 + ps_mental_awareness_mean * 0.25 AS ps_mental_awareness_min
		,ps_mm_cover_max
		,ps_mm_cover_mean
		,ps_mm_cover_max * 0.75 + ps_mm_cover_mean * 0.25 AS ps_mm_cover_min
		,ps_outside_run_max
		,ps_outside_run_mean
		,ps_outside_run_max * 0.75 + ps_outside_run_mean * 0.25 AS ps_outside_run_min
		,ps_pass_blocking_max
		,ps_pass_blocking_mean
		,ps_pass_blocking_max * 0.75 + ps_pass_blocking_mean * 0.25 AS ps_pass_blocking_min
		,ps_pass_rush_ability_max
		,ps_pass_rush_ability_mean
		,ps_pass_rush_ability_max * 0.75 + ps_pass_rush_ability_mean * 0.25 AS ps_pass_rush_ability_min
		,ps_play_making_max
		,ps_play_making_mean
		,ps_play_making_max * 0.75 + ps_play_making_mean * 0.25 AS ps_play_making_min
		,ps_poa_run_shed_blockers_max
		,ps_poa_run_shed_blockers_mean
		,ps_poa_run_shed_blockers_max * 0.75 + ps_poa_run_shed_blockers_mean * 0.25 AS ps_poa_run_shed_blockers_min
		,ps_pulling_max
		,ps_pulling_mean
		,ps_pulling_max * 0.75 + ps_pulling_mean * 0.25 AS ps_pulling_min
		,ps_pursuit_max
		,ps_pursuit_mean
		,ps_pursuit_max * 0.75 + ps_pursuit_mean * 0.25 AS ps_pursuit_min
		,ps_release_max
		,ps_release_mean
		,ps_release_max * 0.75 + ps_release_mean * 0.25 AS ps_release_min
		,ps_routes_max
		,ps_routes_mean
		,ps_routes_max * 0.75 + ps_routes_mean * 0.25 AS ps_routes_min
		,ps_run_after_catch_max
		,ps_run_after_catch_mean
		,ps_run_after_catch_max * 0.75 + ps_run_after_catch_mean * 0.25 AS ps_run_after_catch_min
		,ps_run_defense_2gap_max
		,ps_run_defense_2gap_mean
		,ps_run_defense_2gap_max * 0.75 + ps_run_defense_2gap_mean * 0.25 AS ps_run_defense_2gap_min
		,ps_shed_hand_use_max
		,ps_shed_hand_use_mean
		,ps_shed_hand_use_max * 0.75 + ps_shed_hand_use_mean * 0.25 AS ps_shed_hand_use_min
		,ps_sustain_run_blocking_max
		,ps_sustain_run_blocking_mean
		,ps_sustain_run_blocking_max * 0.75 + ps_sustain_run_blocking_mean * 0.25 AS ps_sustain_run_blocking_min
		,ps_tackling_max
		,ps_tackling_mean
		,ps_tackling_max * 0.75 + ps_tackling_mean * 0.25 AS ps_tackling_min
		,ps_versatility_max
		,ps_versatility_mean
		,ps_versatility_max * 0.75 + ps_versatility_mean * 0.25 AS ps_versatility_min
		,ps_vision_instincts_max
		,ps_vision_instincts_mean
		,ps_vision_instincts_max * 0.75 + ps_vision_instincts_mean * 0.25 AS ps_vision_instincts_min
		,ps_zone_cover_max
		,ps_zone_cover_mean
		,ps_zone_cover_max * 0.75 + ps_zone_cover_mean * 0.25 AS ps_zone_cover_min
		,sticd_character_max
		,sticd_character_mean
		,sticd_character_max * 0.75 + sticd_character_mean * 0.25 AS sticd_character_min
		,sticd_durability_max
		,sticd_durability_mean
		,sticd_durability_max * 0.75 + sticd_durability_mean * 0.25 AS sticd_durability_min
		,sticd_instincts_max
		,sticd_instincts_mean
		,sticd_instincts_max * 0.75 + sticd_instincts_mean * 0.25 AS sticd_instincts_min
		,sticd_speed_max
		,sticd_speed_mean
		,sticd_speed_max * 0.75 + sticd_speed_mean * 0.25 AS sticd_speed_min
		,sticd_toughness_max
		,sticd_toughness_mean
		,sticd_toughness_max * 0.75 + sticd_toughness_mean * 0.25 AS sticd_toughness_min
		,intangibles_off_field_character_max
		,intangibles_off_field_character_mean
		,intangibles_off_field_character_max * 0.75 + intangibles_off_field_character_mean * 0.25 AS intangibles_off_field_character_min
		,intangibles_on_field_character_max
		,intangibles_on_field_character_mean
		,intangibles_on_field_character_max * 0.75 + intangibles_on_field_character_mean * 0.25 AS intangibles_on_field_character_min
		,CASE WHEN [A] > 0 THEN 1 ELSE 0 END AS flag_a
		,CASE WHEN [B] > 0 THEN 1 ELSE 0 END AS flag_b
		,CASE WHEN [C] > 0 THEN 1 ELSE 0 END AS flag_c
		,CASE WHEN [D] > 0 THEN 1 ELSE 0 END AS flag_d
		,CASE WHEN [E] > 0 THEN 1 ELSE 0 END AS flag_e
		,CASE WHEN [F] > 0 THEN 1 ELSE 0 END AS flag_f
		,CASE WHEN [M] > 0 THEN 1 ELSE 0 END AS flag_m
		,CASE WHEN [O] > 0 THEN 1 ELSE 0 END AS flag_o
		,CASE WHEN [P] > 0 THEN 1 ELSE 0 END AS flag_p
		,CASE WHEN [S] > 0 THEN 1 ELSE 0 END AS flag_s
		,CASE WHEN [T] > 0 THEN 1 ELSE 0 END AS flag_t
		,CASE WHEN [U] > 0 THEN 1 ELSE 0 END AS flag_u
		,CASE WHEN [X] > 0 THEN 1 ELSE 0 END AS flag_x
		,CASE WHEN [Z] > 0 THEN 1 ELSE 0 END AS flag_y
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
		,0 AS with_coach_grades
	FROM #temp_evaluations_pivot_mean ep_mean
	INNER JOIN #temp_evaluations_pivot_min ep_min
		ON ep_mean.bane_player_id = ep_min.bane_player_id
	INNER JOIN #temp_evaluations_pivot_max ep_max
		ON ep_mean.bane_player_id = ep_max.bane_player_id
	INNER JOIN BaneProductionAnalytics.dbo.players pl
		ON ep_mean.bane_player_id = pl.id
		AND pl.is_deleted = 0
	LEFT JOIN BaneProductionAnalytics.dbo.positions pn
		ON pl.position_id = pn.id  
	LEFT JOIN AnalyticsWork.dbo.salaries_for_modeling sal
		ON ep_mean.bane_player_id = sal.id
		--AND sal.salaries NOT IN ('Injured','Pre-2011')
	LEFT JOIN #temp_injured_salaries_one_row ins
		ON pl.nfl_id = ins.nfl_player_id
	LEFT JOIN #temp_pro_grades pg
		ON ep_mean.bane_player_id = pg.bane_player_id
		AND pg.created_date = (SELECT MAX(created_date) FROM #temp_pro_grades)
	LEFT JOIN AnalyticsWork.dbo.scout_grade_flags sf
		ON ep_mean.bane_player_id = sf.bane_player_id
	LEFT JOIN #temp_season_position_offense poff
		ON pg.bane_player_id = poff.bane_player_id
		AND pg.season = poff.season
	LEFT JOIN #temp_season_position_defense pdef
		ON pg.bane_player_id = pdef.bane_player_id
		AND pg.season = pdef.season
	LEFT JOIN #temp_draft_board_positions_latest dbp
		ON ep_mean.bane_player_id = dbp.player_id
	WHERE pl.draft_year <= @current_Season
		OR (pl.draft_year IN (@current_Season + 1,@current_Season + 2) AND pg.season IS NULL)

/*
SELECT *
FROM Analytics.dbo.r_input_draft_model_post_combine_grades
WHERE bane_player_id IN (48730,67847,73542,87917,58615,257484)
	AND draft_model_year = 2019
	AND season_in_league = 1
*/


