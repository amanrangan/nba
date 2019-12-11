nba <- read.csv("team_12042019.csv")

nba$pct <- (nba$W / (nba$W + nba$L))
str(nba)

## Rank OffRtg, DefRtg, Pace, SRS
nba$OffRtgRank <- NA
order.offrtg <- order(nba$OffRtg, nba$Team, decreasing = TRUE)
nba$OffRtgRank[order.offrtg] <- 1:nrow(nba)

nba$DefRtgRank <- NA
order.defrtg <- order(nba$DefRtg, nba$Team)
nba$DefRtgRank[order.defrtg] <- 1:nrow(nba)

nba$SRSRank <- NA
order.srs <- order(nba$SRS, nba$Team, decreasing = TRUE)
nba$SRSRank[order.srs] <- 1:nrow(nba)

nba$PaceRank <- NA
order.pace <- order(nba$Pace, nba$Team, decreasing = TRUE)
nba$PaceRank[order.pace] <- 1:nrow(nba)

homeScore <- function(hometeam, awayteam)
  return((nba$PPG[nba$Team == hometeam] + nba$OPPG[nba$Team == awayteam]) / 2) + 1

awayScore <- function(hometeam, awayteam)
  return((nba$PPG[nba$Team == awayteam] + nba$OPPG[nba$Team == hometeam]) / 2) - 1

game <- function(hometeam, awayteam) {
  homeScore <- ((nba$PPG[nba$Team == hometeam] + nba$OPPG[nba$Team == awayteam]) / 2) + 1
  awayScore <- ((nba$PPG[nba$Team == awayteam] + nba$OPPG[nba$Team == hometeam]) / 2) - 1
  if (homeScore > awayScore) {
    return(hometeam)
  } else {
    return(awayteam)
  }
}

score <- function(hometeam, awayteam) {
  homeScore <- ((nba$PPG[nba$Team == hometeam] + nba$OPPG[nba$Team == awayteam]) / 2) + 1
  awayScore <- ((nba$PPG[nba$Team == awayteam] + nba$OPPG[nba$Team == hometeam]) / 2) - 1
  return(cat(homeScore, "-", awayScore))
  
}

gameSummary <- function(hometeam, awayteam) {
  cat("Hometeam: ", hometeam, "; SRS Rank: ", nba$SRSRank[nba$Team == hometeam], "\n")
  cat("Awayteam: ", awayteam, "; SRS Rank: ", nba$SRSRank[nba$Team == awayteam], "\n")
  cat("Home ORTG Rank: ", nba$OffRtgRank[nba$Team == hometeam],
      "; DRTG Rank: ", nba$DefRtgRank[nba$Team == hometeam],
      "\n")
  cat("Away ORTG Rank: ", nba$OffRtgRank[nba$Team == awayteam], 
      "; DRTG Rank: ", nba$DefRtgRank[nba$Team == awayteam],
      "\n")
  cat("Home Avg Score: ", nba$PPG[nba$Team == hometeam], "-", 
      nba$OPPG[nba$Team == hometeam],
      "(Total: ", nba$PPG[nba$Team == hometeam] + nba$OPPG[nba$Team == hometeam], ")",
      "\n")
  cat("Away Avg Score: ", nba$PPG[nba$Team == awayteam], "-", 
      nba$OPPG[nba$Team == awayteam],
      "(Total: ", nba$PPG[nba$Team == awayteam] + nba$OPPG[nba$Team == awayteam], ")",
      "\n")
  cat("Predicted Winner: ", game(hometeam, awayteam), "\n")
  cat("Predicted Score: ", "\t") 
  cat(score(hometeam, awayteam), "\n")
  cat("Next Game", "\n")
  }

gameSummary("IND", "BOS")
gameSummary("ORL", "LAL")
gameSummary("CLE", "HOU")
gameSummary("BRK", "CHA")
gameSummary("MIN", "UTA")
gameSummary("CHI", "ATL")
gameSummary("PHO", "MEM")
gameSummary("MIL", "NOP")
gameSummary("GSW", "NYK")

## Write Results to CSV

  # Right now, manually creating csv - figure out how to scrape and auto create csv based on daily NBA schedule

    # filename <- function(date) {
    #   file <- cat("\'", "./games/", date, "/", date, ".csv", "\'", sep="")
    #   file <- as.character(file)
    #   return(file)
    # }

daily <- read.csv("./games/12112019/12112019.csv")


for (i in 1:nrow(daily)) {
  home <- as.character(daily[i, "Home"])
  away <- as.character(daily[i, "Away"])
  daily[i, "winner"] <- game(home,away)
  daily[i, "spread"] <-
    if (homeScore(home,away) > awayScore(home,away)) {
      -(homeScore(home,away) - awayScore(home,away))
    } else {
      -(awayScore(home,away) - homeScore(home,away))
    }
    
  daily[i, "homeScore"] <- homeScore(home,away)
  daily[i, "awayScore"] <- awayScore(home,away)
  daily[i, "totalScore"] <- daily[i, "homeScore"] + daily[i, "awayScore"]

}    
write.csv(daily, "./games/12112019/12112019.csv")  

    



## Task - Betting Data
  ## Predict score and compare to actual lines from nbaBettingData.xlsx
  ## Spit out delta between predicted and actual spread, TT, ML

## ELO
# install.packages("elo")
# library(elo)
# data(tournament)
# str(tournament)

## Idea - team needs an intrinsic score that factors in offrtg, defrtg, SOS (similar to ELO)
##  How to factor in last season's results given that team composition has changed significantly
## Resulting Model - should be able to plug in hometeam, awayteam and have model that predicts score
## Year Dummy variable or time from now, seasons from now