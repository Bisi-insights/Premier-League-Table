## Premier-League-Table
This repository contains an R function to update and display the English Premier League (EPL) standings as matches are played. The function processes match data for the last three seasons, starting from the 2021-2022 season up to the most recent season.

## Overview
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

# Code Summary 

Here is a summary of the code used in executing the EPL Standing Function
EPL_Standings <- function(Date, Season) {
    date_limit <- mdy(Date)
 '''   
    
    
    epl_2324 <-
        read.csv("https://www.football-data.co.uk/mmz4281/2324/E0.csv")
    epl_2223 <-
        read.csv("https://www.football-data.co.uk/mmz4281/2223/E0.csv")
    epl_2122 <-
        read.csv("https://www.football-data.co.uk/mmz4281/2122/E0.csv")
    
    
    # Define a function to calculate wins, losses, and ties
    calculate_record <- function(data) {
        wins <- sum(data$`Match Type` == "H")
        losses <- sum(data$`Match Type` == "A")
        ties <- sum(data$`Match Type` == "D")
        paste(wins, "-", losses, "-", ties, sep = "")
    }
    
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
        return ("No Info")
    
    epl_data <- epl_data %>%
        mutate(Date = dmy(Date)) %>%
        filter(Date <= date_limit) %>%
        select ("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")
    
    
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
    epl_data <- rbind(home_df, away_df)
    
    
    # Calculate total home wins, loses, and ties for each team
    home_totals <- home_df %>%
        group_by(Team) %>%
        summarise(
            totalHomeWins = sum(`Match Type` == "H"),
            totalHomeLoses = sum(`Match Type` == "A"),
            totalHomeTies = sum(`Match Type` == "D"),
            homeGoalsScored = sum(`Team Goals`),
            homeGoalsConceeded = sum(`Away Goals`)
        )
    
    # Calculate total away wins, loses, and ties for each team
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
    
    team_totals_sorted <- team_totals%>%
        arrange(desc(PPM)) # sort according to the PPM column in desc
    
    if (any(duplicated(team_totals_sorted$PPM) | duplicated(team_totals_sorted$PPM, fromLast = TRUE))) {
        # Calculate total wins again if there is a tiebreaker by PPM
        team_totals_sorted <- team_totals_sorted %>%
            arrange(
                desc(totalWins)  # Then by Total Wins (descending)
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
    
    finalOutput <- team_totals_sorted %>% 
        select(Team,Record,HomeRec,AwayRec,MatchesPlayed,Points,PPM,PtPct,GS,GSM,GA,GAM)
    
    # Print the resulting table
    print(finalOutput)
}




