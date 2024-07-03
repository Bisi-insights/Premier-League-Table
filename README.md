**Premier-League-Table**
This repository contains an R function to update and display the English Premier League (EPL) standings as matches are played. The function processes match data for the last three seasons, starting from the 2021-2022 season up to the most recent season.

**Overview**
The EPL_Standings function reads match data from publicly available CSV files, calculates various statistics for each team, and outputs a table with updated standings. The statistics include total wins, losses, ties, points, points per match, and other relevant metrics.

**Features**
**Seasonal Data:** The function supports data from the 2021/22, 2022/23, and 2023/24 EPL seasons.
**Comprehensive Team Statistics:** Calculates total wins, losses, ties, matches played, points, points per match, goals scored, and goals allowed.
**Detailed Records:** Provides home and away records for each team.

**Sorting and Tiebreakers:** Sorts teams by points per match, with tiebreakers for total wins, goals scored per match, and goals allowed per match.

**Usage**
To use the EPL_Standings function, specify the date up to which you want to include match data and the season you are interested in.

**Example**
EPL_Standings("05/21/2022", "2021/22")
