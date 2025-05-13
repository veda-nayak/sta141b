library(RSQLite)
library(DBI)

# https://github.com/duncantl/STA141B_S25/blob/main/Day11/lahman-model.png

# Setup ------------------------------------------------------------------------
dir = "C:/cygwin64/home/vedan/Code/sta141b"
setwd(dir)

# DO SOURCE STUFF LATER

file = "lahman_1871-2022.sqlite"

db = dbConnect(SQLite(), file) # I am connecting to a SQL light database, here is the name of the file

dbListTables(db)


# Question 1 -------------------------------------------------------------------


 # Question: How many teams are there in the database? how many teams are there in 
 # 2022? List the names of the teams for 2022, including the league and division 
 # they are in and how many games they won and lost in that year. Group the results 
 # by league, division and then by games won.


# Teams seemed like it would contain their thing
dbListFields(db, "Teams")

head( query("SELECT * FROM Teams"), 3)

num_teams = query("SELECT COUNT(DISTINCT name) 
                  FROM Teams")[1,1] # > 140 teams

class(num_teams) # > integer, slay

team_info_2022 = query("SELECT name, divID, lgID, W, L
                        FROM Teams
                        WHERE yearID == 2022
                        ORDER BY divID, lgID, W
                       ")

# VALIDATION TIME

# Question 2 -------------------------------------------------------------------

# What are the 10 teams that had the highest average attendance since 2015? 
# Give the name and the average and total attendance for this period for each 
# time.

average_attendance_NA = query("SELECT name, AVG(attendance)
                               FROM Teams
                              GROUP BY name") 


# gave me NAs --> removing NAs, the NAs might just be where data was not collected
average_attendance_no_NA = query("SELECT name, AVG(attendance)
                                 FROM Teams
                                 WHERE attendance != 'NA'
                                 GROUP BY name") 

top_10_avg_attendance_after_2015 = query("SELECT name as Team_Name, AVG(attendance) as 'Average_Attendance'
                                           FROM Teams
                                           WHERE attendance != 'NA' 
                                                AND 
                                                 yearID > 2015
                                           GROUP BY name
                                           ORDER BY Average_Attendance
                                          LIMIT 10") 

# VALIDATION TIME

# Question 3 -------------------------------------------------------------------

# Who are the 10 people that had the “longest” career, i.e., spans the largest 
# number of years? Who are the 10 people that played the most number of games?

dbListFields(db, "People")

head(query("SELECT * FROM PEOPLE"), 3)

# I wanted to see how to find the year from the finalGame and debut columns so I tried to use strftime()
query("SELECT (strftime('%Y', finalGame)
      FROM People
      LIMIT 5")

top_10_career_len = query("SELECT nameFirst, 
                                  nameLast, 
                                  nameGiven, 
                                  (strftime('%Y', finalGame) - strftime('%Y', debut)) AS careerLen
                           FROM People
                           ORDER BY careerLen DESC
                           LIMIT 10
                           ")

peopleYearInfo = query("SELECT nameFirst, 
                               nameLast, 
                               nameGiven, 
                               finalGame, 
                               debut
                        FROM People
                        LIMIT 10
                       ") # use later for validation

# Question 4 -------------------------------------------------------------------

# Compute which team won the world series in 2022? Include the team name, 
# league, division, number of games won and lost during the season, and the 
# total number of games in the world series.

dbListFields(db, "SeriesPost")

head(query("SELECT * FROM SeriesPost"), 3)

world_series_winner_2022 = query("SELECT teamIDwinner, wins, losses, (wins + losses + ties) as total
                                  FROM SeriesPost
                                  WHERE yearID == 2022  
                                      AND
                                        round = 'WS'
                                  
                                 ")
world_series_winner_info_with_NAs = query("SELECT teamIDwinner, wins, losses, (wins + losses + ties) as total, name, lgID, divID, lgID
                                           FROM SeriesPost
                                           INNER JOIN Teams
                                           ON SeriesPost.teamIDWinner = Teams.teamID
                                           WHERE SeriesPost.yearID == 2022
                                              AND
                                                 SeriesPost.round = 'WS'
                                           GROUP BY SeriesPost.teamIDWinner
                                                 ")

query("SELECT teamIDwinner, wins, losses, (wins + losses + ties) as total, lgID, divID, lgID
       FROM SeriesPost
       INNER JOIN Teams
       ON SeriesPost.teamIDWinner = Teams.teamID
       WHERE SeriesPost.yearID == 2022 
     AND 
       Teams.teamID == 'HOU'
      ")

# I realised that this was giving me NAs at times, so it might have to do with the year not being factored into the teams table

world_series_winner_info = query("SELECT teamIDwinner, 
                                         (W + L + ties) as totalGames, 
                                         lgID, 
                                         divID, 
                                         lgID
                                  FROM SeriesPost
                                  INNER JOIN Teams
                                  ON SeriesPost.teamIDWinner = Teams.teamID
                                  WHERE SeriesPost.yearID == 2022 
                                      AND 
                                        Teams.yearID == 2022
                                      AND
                                        SeriesPost.round = 'WS'
                                        ")

# The winner is team HOU

# Question 5 -------------------------------------------------------------------

# Compute the winner and the loser of the world series for all years. Include 
# the team name, league, division, number of games won and lost during the 
# season for both the winning and the losing team.

# BREAK THIS DOWN FOR THE REPORT

query("SELECT SeriesPost.yearID,
              teamID,
              lgID, 
              W, 
              L,
              CASE WHEN teamID = teamIDwinner
                THEN 'winner'
                ELSE 'loser'
                END AS outcome
       FROM SeriesPost
       INNER JOIN Teams
       ON (SeriesPost.round = 'WS' 
            AND 
              SeriesPost.teamIDwinner = Teams.teamID
            AND 
              SeriesPost.yearID = Teams.yearID)
          OR 
            (SeriesPost.round = 'WS' 
           AND 
             SeriesPost.teamIDloser = Teams.teamID
          AND 
              SeriesPost.yearID = Teams.yearID)
      LIMIT 20
      ")

# THIS ONE WORKS BUT IT IS UGLY
query("SELECT SeriesPost.yearID,
              teamIDwinner,
              teamIDloser,
              teamID as teamIfo,
              lgID, 
              W, 
              L
       FROM SeriesPost
       INNER JOIN Teams
       ON (SeriesPost.round = 'WS' 
            AND 
              SeriesPost.teamIDwinner = Teams.teamID
            AND 
              SeriesPost.yearID = Teams.yearID)
          OR 
            (SeriesPost.round = 'WS' 
           AND 
             SeriesPost.teamIDloser = Teams.teamID
          AND 
              SeriesPost.yearID = Teams.yearID)
      LIMIT 20
      ")

# In an ideal world, we would have teamID, a column for winner or loser, 
# followed by the team information

# Like "if (teamIDwinner = teamInfo){status = 'winner'}"

world_series_info = query("SELECT SeriesPost.yearID,
                                  teamID,
                                  CASE WHEN teamID = teamIDwinner
                                    THEN 'winner'
                                    ELSE 'loser'
                                    END AS outcome,
                                  lgID, 
                                  divID,
                                  W, 
                                  L,
                                  (W + L + ties) AS totalGames
                           FROM SeriesPost
                           INNER JOIN Teams
                           ON (SeriesPost.round = 'WS' 
                                AND 
                                  SeriesPost.teamIDwinner = Teams.teamID
                                AND 
                                  SeriesPost.yearID = Teams.yearID)
                              OR 
                                (SeriesPost.round = 'WS' 
                               AND 
                                 SeriesPost.teamIDloser = Teams.teamID
                              AND 
                                  SeriesPost.yearID = Teams.yearID)
                          ")

# JUST NEEDS VALIDATION

# Question 6 -------------------------------------------------------------------

# Compute the total payroll for each team for each year. Plot these.
# Bonus: In the SQL query, also compute whether the team won or lost the world 
# series for that year and
# add that to the plot.

dbListFields(db, "Salaries")

query("SELECT yearID, teamID, SUM(salary) AS totalSalary
       FROM Salaries
       GROUP BY teamID
       LIMIT 5")





# Question 7 -------------------------------------------------------------------

# What are the 10 colleges/universities that have produced the most players in
# MLB since 2000? Give the college name, city and state and the number of 
# players they have produced in this time period.


# Question 8 -------------------------------------------------------------------

# How many all-star games are there each year? How many players are there in 
# each all-star game(s) each year?


# Question 9 -------------------------------------------------------------------


# Consider players who have appeared in at least 15 all-star games. How many 
# all-star games have they played in? Give the players’ names, teams and years 
# for each year, in which they were in the all-star games, and the first and 
# last year they were in an all-star game.



# Question 10 ------------------------------------------------------------------

#  Find the 10 players who have appeared in the most number of years in the 
# playoffs. This is the number of years, not the total number of playoff games 
# they were in. Return the table with the person’s name, number of years they 
# have been in the playoffs, the first and last year they appeared in the 
# playoffs, and the number of different teams they appeared for in the playoffs.

