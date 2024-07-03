# Olabisi Odusanya

library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)

# Function
EPL_Standings <- function(Date, Season) {
  date_limit <- mdy(Date)

# reading the file using read.csv  
  epl_2324 <-
    read.csv("https://www.football-data.co.uk/mmz4281/2324/E0.csv")
  epl_2223 <-
    read.csv("https://www.football-data.co.uk/mmz4281/2223/E0.csv")
  epl_2122 <-
    read.csv("https://www.football-data.co.uk/mmz4281/2122/E0.csv")

# ifelse statement to differentiate and walk through the datasets
  if (Season == "2021/22") {
    epl_data <- epl_2122
  }
  else if (Season == "2022/23") {
    epl_data <- epl_2223
  }
  else if (Season == "2023/24") {
    epl_data <- epl_2324
  }
  else
    return ("No Info") #when date doesn't fall within the bounds set, it returns 'No Info"

# Mutate and filter and select feature for the date function, date scope limit as well as columns needed for the updated table
  epl_data <- epl_data %>%
    mutate(Date = dmy(Date)) %>%
    filter(Date <= date_limit) %>%
    select ("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")
  
# home and away table separation 
  home_df <- epl_data %>%
    select(
      "Team" = "HomeTeam",
      "Team Goals" = "FTHG",
      "Away Goals" = "FTAG",
      "Match Type" = "FTR",
      "Date"
    )
  away_df <- epl_data %>%
    select(
      "Team" = "AwayTeam",
      "Team Goals" = "FTHG",
      "Away Goals" = "FTAG",
      "Match Type" = "FTR",
      "Date"
    )
# combinantion of both tables into one using the rbind function
  epl_data <- rbind(home_df, away_df)
  
  
# Defined function to calculate Record wins, losses, and ties, also incorporating the paste function
  calculate_record <- function(data) {
    wins <- sum(data$`Match Type` == "H")
    losses <- sum(data$`Match Type` == "A")
    ties <- sum(data$`Match Type` == "D")
    paste(wins, "-", losses, "-", ties, sep = "")
  }
  
  
# Calculate total HomeRec home wins, loses, and ties for each team by grouping and summarising
  home_totals <- home_df %>%
    group_by(Team) %>%
    summarise(
      totalHomeWins = sum(`Match Type` == "H"),
      totalHomeLoses = sum(`Match Type` == "A"),
      totalHomeTies = sum(`Match Type` == "D"),
      homeGoalsScored = sum(`Team Goals`),
      homeGoalsConceeded = sum(`Away Goals`)
    )
  
# Calculate total AwayRec away wins, loses, and ties for each team
  away_totals <- away_df %>%
    group_by(Team) %>%
    summarise(
      totalAwayWins = sum(`Match Type` == "A"),
      totalAwayLoses = sum(`Match Type` == "H"),
      totalAwayTies = sum(`Match Type` == "D"),
      awayGoalsScored = sum(`Away Goals`),
      awayGoalsConceeded = sum(`Team Goals`)
    )
  
# Merge home_totals and away_totals
  team_totals <-
    merge(home_totals, away_totals, by = "Team", all = TRUE)
  
# Fill NA values with 0
  team_totals[is.na(team_totals)] <- 0
  
# calculate total wins, loses, ties, matches played, points, PPM and every other team stats
  team_totals <- team_totals %>%
    group_by(Team) %>%
    mutate(
      totalWins = totalHomeWins + totalAwayWins,
      totalLoses = totalHomeLoses + totalAwayLoses,
      totalTies = totalHomeTies + totalAwayTies
    ) %>%
    mutate(
      MatchesPlayed = totalWins + totalLoses + totalTies,
      Points = sum(totalWins * 3, totalTies),
      PPM = round(Points / MatchesPlayed, 2),
      PtPct = round(Points / (3 * MatchesPlayed), 2),
      Record = paste0(totalWins, "-", totalLoses, "-", totalTies),
      HomeRec = paste0(totalHomeWins, "-", totalHomeLoses, "-", totalHomeTies),
      AwayRec = paste0(totalAwayWins, "-", totalAwayLoses, "-", totalAwayTies),
      GS = homeGoalsScored + awayGoalsScored,
      GSM = round(GS / MatchesPlayed, 2),
      GA = homeGoalsConceeded + awayGoalsConceeded,
      GAM = round(GA / MatchesPlayed, 2)
    )
# sort according to the PPM column in desc  
  team_totals_sorted <- team_totals%>%
    arrange(desc(PPM))
  
# Calculate total wins again if there is a tiebreaker by PPM  
  if (any(duplicated(team_totals_sorted$PPM) | duplicated(team_totals_sorted$PPM, fromLast = TRUE))) {

# Then by Total Wins (descending)   
    team_totals_sorted <- team_totals_sorted %>%
      arrange(
        desc(totalWins)  
      )
    
# Check if there is a tiebreaker by total wins
    if (any(duplicated(team_totals_sorted$totalWins) | duplicated(team_totals_sorted$totalWins, fromLast = TRUE))) {
      
# Sort by goals scored per match only if there is a tiebreaker by total wins
      team_totals_sorted <- team_totals_sorted %>%
        arrange(
          desc(GSM)  # Then by Goals Scored per Match (descending)
        )
      
# Check if there is a tiebreaker by goals scored per match
      if (any(duplicated(team_totals_sorted$GSM) | duplicated(team_totals_sorted$GSM, fromLast = TRUE))) {
        
# Sort by goals allowed per match only if there is a tiebreaker by goals scored per match
        team_totals_sorted <- team_totals_sorted %>%
          arrange(GAM)  # Finally by Goals Allowed per Match (ascending)
      }
    }
  }
#final output.   
  
finalOutput <- team_totals_sorted %>% 
    select(Teamname = Team,Record,HomeRec,AwayRec,MatchesPlayed,Points,PPM,PtPct,GS,GSM,GA,GAM)
  
# Print the resulting table
  return(finalOutput)
}

EPL_Standings("05/19/2024", "2023/24")

#My process: I followed the helpful guide you provided last week and broke the solution into bits:
# - Function creation and data formatting, then I read the file as variables epl_2122, epl_2223, and epl_2324.
# After which I did home and away df to have a better overview of the data
# epl_data: This is a combination of home and away df using rbind
# Other variables such as team_totals, away_totals, home_totals, and calculate_record, all were created to allow me be able to calculate the data points needed.

# Reflection: I'm a football fan so the excitement was me being able to formulate this data point by myself. While it was exciting
#it's quite stressful to deal with especially the part where I had to compute the columns. 
# I also noticed I'm unable to get an updated table for the latest season, the game on Sunday for example I was able to view Monday evening, 
#Same as the match Arsenal played yesterday against Chelsea, that is still not coming up when I run the date till now.


