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


tmpanalysis <- function()
{
  matchData = getBetData()
  matchData$Result = sign(matchData$Score1 - matchData$Score2)
  matchData$HomeSignal = matchData$HomeJ > matchData$HomeM 
  matchData$AwaySignal = matchData$AwayJ > matchData$AwayM 
  matchData$DrawSignal = matchData$DrawJ > matchData$DrawM 
  matchData$hasSignal = matchData$HomeSignal | matchData$AwaySignal | matchData$DrawSignal 
  matchData$betAction = matchData$hasSignal & is.na(matchData$PrevRow)
  matchData$alreadyBet = matchData$betAction
  
  
  for( i in 1:nrow(matchData))
  {
    if (!is.na(matchData$PrevRow[i]))
    {
      # has prev 
      if (matchData$alreadyBet[matchData$PrevRow[i]])
      {
        matchData$alreadyBet[i] = TRUE
      } 
      else
      {
        if (matchData$hasSignal[i])
        {
          matchData$betAction[i] = TRUE
          matchData$alreadyBet[i] = TRUE
        }
      }
    }
  }
  
  x = ifelse( matchData$betAction,
              matchData$HomeSignal * (ifelse(matchData$Result == 1 , matchData$HomeJ,0)-1) +
                matchData$AwaySignal * (ifelse(matchData$Result == -1 ,matchData$AwayJ,0)-1) +
                matchData$DrawSignal * (ifelse(matchData$Result == 0 ,matchData$DrawJ,0)-1)
              , 0)
  
  x[which(is.na(x))]=0
  
  matchData$PnL = x 
  y = subset(matchData, betAction)$PnL
}



