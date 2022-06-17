getPastModelResult <- function(raw, nday)
{
  
  today = Sys.Date()
  
  leagues = getLeagues()
  
  targetDays = today+ c((-nday):1)
  
  x = subset(raw, as.Date(raw$date) %in% targetDays  &  raw$league %in% leagues & !is.na(score1)  )
  
  result = data.table ( #Date= x$date,
                        League = as.factor(x$league),
                        #Team1 = unlist(lapply( x$team1, sanitizeName)),
                        #Team2 = unlist(lapply( x$team2, sanitizeName)),
                        HomeProb = x$prob1, 
                        AwayProb = x$prob2, 
                        DrawProb = 1-x$prob1-x$prob2,
                        I_Home   = as.numeric(x$score1 > x$score2),
                        I_Away   = as.numeric(x$score1 < x$score2),
                        I_Draw   = as.numeric(x$score1 == x$score2)
                        )
  
  result$R_Home = result$HomeProb-result$I_Home
  result$R_Away = result$AwayProb-result$I_Away
  result$R_Draw = result$DrawProb-result$I_Draw
  
  
  summary(result)
  
  return (result)
}