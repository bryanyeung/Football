require (data.table)

m6 <- function() {sort(sample(c(1:49),6))}
url<- "https://projects.fivethirtyeight.com/soccer-api/club/spi_matches_latest.csv"
destfile = "tmp.csv"
x <- download.file(url, destfile = destfile)
raw = read.csv  (destfile)


####


today = Sys.Date()

targetDays = c(today-1,
               today,
               today +1,
               today +2)

targetDays = today + c(-1:2)

#target_league = 2411



leagues = c("UEFA Europa League", "UEFA Champions League",
            "Barclays Premier League",
            "French Ligue 1", "French Ligue 2", "Italy Serie A",
            "English League Championship",   "English League One",
            "Dutch Eredivisie","German Bundesliga", "German 2. Bundesliga",
            "Spanish Primera Division", "Portuguese Liga","Major League Soccer")


x<- subset (raw, as.Date(raw$date) %in% targetDays &  raw$league %in% leagues & is.na(raw$xg1) )


#y <- subset (raw, as.Date(raw$date) %in% targetDays &  raw$league %in% leagues & !is.na(raw$xg1) )


#x<- subset (raw, as.Date(raw$date) %in% targetDays &  raw$league_id == target_league & is.na(raw$xg1) )

result = data.table ( Date= x$date,
                      League = x$league,
                      Team1 = x$team1,
                      Team2 = x$team2,
                      Win =  round( 1/x$prob1,digits =2), 
                      Draw = round(1 / x$probtie,digits =2),
                      Lose = round(1/x$prob2,digits =2)
                      #ProjScore1 = x$proj_score1,
                      #ProjScore2 = x$proj_score2, 
                      #ProjTotalScore = x$proj_score1 + x$proj_score2
                    )

print ( result [order(Date, League )])




print ( subset(result, League =="Barclays Premier League" ) )

for ( league in leagues){
  tmp =  subset(result, League == league)
  if (nrow(tmp))
    print ( subset(result, League == league  ) )
  
}
  


print ( subset(result, League =="Barclays Premier League" ) )




