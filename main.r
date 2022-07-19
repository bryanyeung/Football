require (data.table)
require(curl)

library(jsonlite)

kellyBet <- function(odd_model, odd_hkjc)
{
  p = 1/ odd_model
  b = odd_hkjc - 1 
  q = 1 - p 
  f = p - q /b 
  return(f)
}

kellyBet_p <- function(odd_model_p, odd_hkjc_p)
{
  return(kellyBet(odd_model_p/100,odd_hkjc_p/100) *100)
}

m6 <- function() {sort(sample(1:49,6))}

getModelFile <- function( )
{
  url<- "https://projects.fivethirtyeight.com/soccer-api/club/spi_matches_latest.csv"
  destfile = "data/tmp.csv"
  x <- download.file(url, destfile = destfile)
  raw = read.csv  (destfile)
  return (raw)
}

# Wanted Leagues in Model file
getLeagues <- function()
{
  return (  leagues = c("UEFA Europa League", "UEFA Champions League",
                        "UEFA Europa Conference League",
                        "Barclays Premier League","English League Championship","English League One",
                        "English League Two" ,
                        "French Ligue 1", "French Ligue 2", "Italy Serie A",
                        "Dutch Eredivisie", "Belgian Jupiler League",
                        "German Bundesliga", "German 2. Bundesliga",
                        "Spanish Primera Division", "Spanish Segunda Division","Portuguese Liga",
                        "Major League Soccer",
                        "Swedish Allsvenskan", "Norwegian Tippeligaen",
                        "Scottish Premiership",
                        "Japanese J League","Russian Premier Liga"
                        #,"Brasileiro SÃ©rie A", "Argentina Primera Division"
                        ))
}

# Leagues Ignored in HKJC 
getIgnoredLeagues <- function()
{
  return ( c("Dutch Division 2","Chilean Division 1", "South American Cup", "Japanese Division 2", "Mexican Premier",
             "Korean Division 1","Eng League Trophy","Japanese League Cup","Euro Nations League"
             ,"WC Qualifiers","Argentine Cup", "International Matches", "U21 Euro Qualifiers"
             ,"Brazilian Division 1", "Argentine Division 1", "U23 Asian Cup"
             ))
}

sanitizeName <- function (name )
{
  if (substr( name, 0, 12) == "Sporting Gij") return ("Sporting Gijon")
  if (substr( name, 0, 7)  == "1. FC N" & substr(name, 10,15)== "rnberg" ) return ("1. FC Nurnberg")
  if (substr( name, 0, 4)  == "Alav" & substr(name, 7,8)== "s" ) return ("Alaves")
  if (substr(name, 3, 14)  ==  "stersunds FK")  return ("Ostersunds FK")
  if (substr( name, 0, 14) == "SpVgg Greuther" ) return("SpVgg Greuther Furth")
  if (substr( name, 0, 9)  == "Fortuna D" &&  substr(name,12,19) == "sseldorf") return ("Fortuna Dusseldorf")
  if (substr( name, 0, 1)  == "M" && substr(name,4,10) == "laga") return ("Malaga")
  return (name)
}

getModelOdds <- function(raw, nday)
{
  
  today = Sys.Date()
  
  leagues = getLeagues()
  
  targetDays = today+ c((-1):nday)
  
  x<- subset (raw, as.Date(raw$date) %in% targetDays &  raw$league %in% leagues & is.na(raw$xg1) )
  
  result = data.table ( Date= x$date,
                        League = x$league,
                        Team1 = unlist(lapply( x$team1, sanitizeName)),
                        Team2 = unlist(lapply( x$team2, sanitizeName)),
                        Win =  round( 1/x$prob1,digits =2), 
                        Draw = round(1 / x$probtie,digits =2),
                        Lose = round(1/x$prob2,digits =2),
                        spi1 = x$spi1,
                        spi2 = x$spi2
                        #ProjScore1 = x$proj_score1,
                        #ProjScore2 = x$proj_score2, 
                        #ProjTotalScore = x$proj_score1 + x$proj_score2
  )
  return (result)
}


printModel <- function (model_odds)
{
  leagues = getLeagues()
  for ( league in leagues){
    tmp =  subset(model_odds, League == league)
    if (nrow(tmp))
      print ( subset(model_odds, League == league  ) )
    
  }
  #write.table ( result [model_odds(League, Date )], file= 'output.csv')
}


GetOddFromString <-function ( oddstr )
{
  return (as.numeric(strsplit(oddstr, '@')[[1]][2]))
}


ReadOddsFromPath <- function(json_path)
{
  
  oddsdata =  fromJSON(json_path, simplifyVector = TRUE, flatten= TRUE)$matches
  
  #if (!is.null(oddsdata$livescore.home))
  #{
  #  oddsdata = oddsdata[which(is.na(oddsdata$livescore.home)),] #Filter started game
  #}
 
  oddsdata = subset(oddsdata, matchState == "PreEvent")
  
  res = data.table ( 
    MatchNum =  oddsdata$frontEndId,
    MatchDate = as.Date(oddsdata$matchDate ),
    MatchDay  = oddsdata$matchDay,
    League   =  oddsdata$tournament.tournamentNameEN,
    HomeTeam = oddsdata$homeTeam.teamNameEN, 
    AwayTeam = oddsdata$awayTeam.teamNameEN, 
    HomeOdd = unlist(lapply(oddsdata$hadodds.H, GetOddFromString )), 
    DrawOdd = unlist(lapply(oddsdata$hadodds.D, GetOddFromString )), 
    AwayOdd = unlist(lapply(oddsdata$hadodds.A, GetOddFromString )))
  
  res = res[which( ! res$League %in% getIgnoredLeagues() ),]
  
  return (res)  
}

SaveMatchTeams <- function( mapping)
{
  write.csv(file ='Matching.csv', mapping, row.names =FALSE)
}

LoadMatchTeams <- function(){
  res = read.csv(file = 'data/Matching.csv',header = TRUE)
  additionals = loadAdditionalNames()
  res = rbind(res, additionals)
  
  if ( nrow( res[ which (duplicated(res$Model)),])!= 0 |
       nrow( res[ which (duplicated(res$JC)),])!= 0 )
  {
    print ("ERROR: DUPLICATE in matching.csv")
    print( res[ which (duplicated(res$Model)),])
    print( res[ which (duplicated(res$JC)),])
  }
  
  # Doesnt work for some reason, maybe the encoding during source('main/r')
  #res = data.frame()
  #res = rbind ( res , c("IFK VÃ¤rnamo","Varnamo"))
  #res = rbind ( res , c("1. FC NÃ¼rnberg","Nurnberg"))
  #res = rbind ( res , c("Fortuna DÃ¼sseldorf","Dusseldorf"))
  #res = rbind ( res , c("AlavÃ©s","Alaves"))
  #res = rbind ( res , c("SpVgg Greuther FÃ¼rth","Greuther Furth"))
  #res = rbind ( res , c("Ã–stersunds FK" ,"Ostersunds FK"))
  #res = rbind ( res , c("Sporting GijÃ³n" ,"Sporting Gijon"))
  #newMappings = data.table(Model = res[,1], JK = res[,2] )
  
  return ( res)
}

loadAdditionalNames <- function()
{
  
  file_path = 'data/additionalMappings.rds'
  if (!file.exists(file_path))
  {
    return (data.table())
  }
  return(readRDS(file_path))
}

addAdditionalNames<- function(newMappings)
{
  file_path = 'data/additionalMappings.rds'
  if (!file.exists(file_path))
  {
    saveRDS(newMappings,file = file_path)
    return(0)
  }
  
  oldData  = loadAdditionalNames()
  saveRDS(rbind(oldData,newMapping), file = file_path)
}

addToMappingFile <- function(newMapping)
{
  existing = LoadMatchTeams()
  toAdd = data.table()
  
  for(i in 1:nrow(newMapping))
  {
    if (newMapping$Model[i] %in% existing$Model)
      next
    toAdd = rbind(toAdd, newMapping[i,])
  }
  SaveMatchTeams(rbind(existing,toAdd))
}


### Find the row id in model_teams 
resolveInd <- function( team, modelTeams,storedMatch)
{
  if( team %in% modelTeams )
    return (which(team == modelTeams))
  
  #Look up mapping
  findFromMapping = which( storedMatch$JC == team)
  if (length(findFromMapping)>0 )
  {
    teamModelName = storedMatch$Model[findFromMapping[1]]
    findmatch =  which(modelTeams == teamModelName)
    if (length(findmatch)>0)
      return (findmatch)
  }
  return (c())  
}

MatchRowInds <- function(hkjc_odds, model_odds)
{
  storedMatch = LoadMatchTeams()
  ignoreLeagues =getIgnoredLeagues()
  
  findind = c()
  for (i in 1:nrow(hkjc_odds)) 
  {
  
    x = resolveInd( hkjc_odds$HomeTeam[i],model_odds$Team1,storedMatch)
    y = resolveInd( hkjc_odds$AwayTeam[i],model_odds$Team2,storedMatch)
    
    commonRow = intersect(x,y)
    if (length(commonRow) == 1)
    {
      findind[i] = commonRow[1]
      next
    }
  }
  return(findind)
}

kellyToBet <- function(x)
{
  return ( 10*floor(round( ifelse( x > 0, x*100 , 0 ),1)))
}
kellyToBetAggressive <- function(x)
{
  return ( 10*round(round( ifelse( x > 0, x*100 , 0 ),1)))
}

getBetData <- function()
{
  filepath = "data/betdata.rds"
  return(readRDS(file = filepath))
}

saveBetaData <- function(x)
{
  filepath = "data/betdata.rds"
  saveRDS(x, file = filepath)
}


archiveBetData <- function (betData)
{
  if(nrow(betData)==0)
  {
    return(0)
  }
    
  filepath = "data/betdata.rds"
  if (!file.exists(filepath))
  {
    saveRDS(betData,file = filepath)
    return(0)
  }
  
  existingData = readRDS(file =filepath)
  hasOverlap =  existingData[nrow(existingData)]$MatchDate >= betData[1]$MatchDate
  
  if (!hasOverlap)
  {
    saveRDS(rbind(existingData, betData, fill = TRUE), file= filepath)
    return(0)
  }

  #Looking for repeated rows
  startInd = min(which(existingData$MatchDate == betData$MatchDate[1]))
  endInd = nrow(existingData)
  
  compareTarget = existingData[startInd:endInd]
  
  newMatchInds = c()
  changedOddInds = c()
  prevOddInds = c()
  
  for(i in 1:nrow(betData))
  {
    entry = betData[i]

    sameMatch = which(compareTarget$MatchDate == entry$MatchDate & 
                      compareTarget$Home == entry$Home &
                      compareTarget$Away == entry$Away )
    
    if (length(sameMatch)==0) {
      newMatchInds =c(newMatchInds,i)
      next
    }

    #if exactly the same
    prevEntry =  compareTarget[sameMatch[length(sameMatch)]]
    isIdentical = prevEntry$HomeM == entry$HomeM &
      prevEntry$AwayM == entry$AwayM &
      prevEntry$HomeJ == entry$HomeJ &
      prevEntry$AwayJ == entry$AwayJ
    if(isIdentical)
      next
    
    changedOddInds = c(changedOddInds, i)
    prevOddInds = c(prevOddInds,sameMatch[length(sameMatch)])
  }    
  
  # Save new maches
  if(length(newMatchInds)>0)
  {
    saveRDS(rbind(existingData, betData[newMatchInds],fill = TRUE), file= filepath)
  }
  
  # There is no way to tell whether the odd change is due to mid-game. need manual work
  if(length(changedOddInds)>0)
  {
    changedData = betData[changedOddInds]
    changedFilename = paste( "data/", gsub(" ","_", gsub("[:-]","_",  changedData$DataTime[1])) ,"_change.rds",sep = "")
    changedData$PrevRow = prevOddInds + startInd - 1
     
    print ("From: ")
    
    displayColInds <- c(2,3,5,6,10,11,12)
    print(compareTarget[prevOddInds,..displayColInds]) 
    
    print("To: ")
    print(changedData[,..displayColInds])
    warning("ALERT!!!!!!!!!!!!! Odds have changed")
    saveRDS(rbind(readRDS(file =filepath), changedData, fill = TRUE), file= filepath)
    
    #saveRDS(changedData, file = changedFilename )
    #warning("ALERT!!!!!!!!!!!!! Odds have changed. File generated: ",changedFilename)
  }
}



BetSignal <- function( hkjc_odds, findind, model_odds)
{
  x = which(!is.na(findind))
  if ( length(x)==0)
  {
    warning("No matches")
    return(NULL)
  }
  
  
  hkjc_odds = hkjc_odds[x,]
  findind = findind[!is.na(findind)]
  ModelImpHomeOdd = model_odds$Win[findind]
  ModelImpDrawOdd = model_odds$Draw[findind]
  ModelImpAwayOdd = model_odds$Lose[findind]
  HomeBet <-kellyToBetAggressive(kellyBet(ModelImpHomeOdd , hkjc_odds$HomeOdd))
  AwayBet <-kellyToBetAggressive(kellyBet(ModelImpAwayOdd , hkjc_odds$AwayOdd))
  DrawBet <-kellyToBetAggressive(kellyBet(ModelImpDrawOdd , hkjc_odds$DrawOdd))
  
  

  if (length(findind)>0)
  {
    betData = data.table( DataTime= Sys.time(),
                          MatchDate = model_odds$Date[findind],
                          #MatchDateJ = hkjc_odds$MatchDate,
                          MatchNum = hkjc_odds$MatchNum,
                          League = model_odds$League[findind],
                          Home = model_odds$Team1[findind],
                          Away =  model_odds$Team2[findind],
                          HomeM = ModelImpHomeOdd,
                          DrawM = ModelImpDrawOdd,
                          AwayM = ModelImpAwayOdd,
                          HomeJ = hkjc_odds$HomeOdd,
                          DrawJ = hkjc_odds$DrawOdd,
                          AwayJ = hkjc_odds$AwayOdd,
                          HomeSpi = model_odds$spi1[findind],
                          AwaySpi = model_odds$spi2[findind],
                          Score1 = NA,
                          Score2 = NA
    )
    
    archiveBetData(betData)
  }
  haveSignal = which( HomeBet>0 | AwayBet >0 | DrawBet >0 )
  if ( length(haveSignal)==0) 
  {
    print ("No signal")
    return (NULL)
  }
  Result = data.table ( Day= hkjc_odds$MatchDay,
                        Num = hkjc_odds$MatchNum,
                        HomeBet = HomeBet, 
                        DrawBet = DrawBet, 
                        AwayBet = AwayBet, 
                        Home = substr(hkjc_odds$HomeTeam,0,15),
                        Away = substr(hkjc_odds$AwayTeam,0,15),
                        HomeM = ModelImpHomeOdd,
                        DrawM = ModelImpDrawOdd,
                        AwayM = ModelImpAwayOdd)
  
  return(Result[haveSignal,])
}


#### Best effort to look for unresolved JC team names
newMappingSuggestion <-function(model_odds, unresolvedJCNames)
{
  modelTeamNames <- unique(c( model_odds$Team1,model_odds$Team2))
  hkjcTeamNames  <- unique (unresolvedJCNames )
  storedMatch = LoadMatchTeams()
  
  mapping = data.table()
  
  for ( team in modelTeamNames )
  {
    if (team %in% storedMatch$Model )
      next
    ## Same name 
    if (team %in% hkjcTeamNames) 
    {
      mapping = rbind( mapping, data.table( Model = team, JC = team ))  
      next
    }
    
    grep_result =  grep( team, hkjcTeamNames, ignore.case = TRUE)
    nResult = length(grep_result)
    
    if (nResult > 0)
    {
      mapping = rbind( mapping, data.table( Model = team, JC = hkjcTeamNames[grep_result] ))
      if ( nResult > 1 )
        print(paste(team,"needs resolve"))
      
      next 
    } else
    {
      #print(paste(team,"not found"))
    }
    
  }
  
  for ( team in hkjcTeamNames)
  {
    if (team %in% storedMatch$JC )
      next
    
    if ( team %in% mapping$JC)
      next
    
    grep_result =  grep( team, modelTeamNames,ignore.case = TRUE)
    nResult = length(grep_result)
    if (nResult > 0)
    {
      mapping = rbind( mapping, data.table( Model = modelTeamNames[grep_result], JC = team  ))  
      if ( nResult > 1 )
        print(paste(team,"needs resolve"))
      next 
    }else   {
      print(paste(team,"not found"))
    }
  }
  
  if (nrow(mapping)== 0) return (mapping)
  return (mapping[which(JC!=Model)])
}

markResult <- function(raw)
{
  archived = getBetData()
  
  noResultInds = which(is.na(archived$Score1))
  
  if (length(noResultInds)==0)
    return (0)
  
  minDate = min(archived$MatchDate[noResultInds])
  maxDate = max(archived$MatchDate[noResultInds])
  
  results = subset(raw, date >=minDate &
                        date <= maxDate &
                        !is.na(score1) &
                        !is.na(score2))
  
  #todo lapply?
  for ( i in 1:length(noResultInds))
  {
    entry  = archived[noResultInds[i]]
    findInd = which( results$date == entry$MatchDate &
                     results$team1 == entry$Home & 
                     results$team2 == entry$Away )
    
    if(length(findInd)==0)
    {
      next
    }
    archived$Score1[noResultInds[i]] = results$score1[findInd[1]] 
    archived$Score2[noResultInds[i]] = results$score2[findInd[1]] 
  }
  
  saveBetaData(archived)
}

sanitizeBetData<- function()
{
  tmp = getBetData()
  
  i = 2 
  while(TRUE) # no for loop becoz we want to delete rows
  {
    if (i > nrow(tmp)) break;
    
    entry = tmp[i]
    prevInds = which(tmp$MatchDate[1:(i-1)] == entry$MatchDate & 
                     tmp$MatchNum[1:(i-1)] == entry$MatchNum & 
                     tmp$Home[1:(i-1)] == entry$Home &
                       tmp$Away[1:(i-1)] == entry$Away
                       )
    if(length(prevInds)==0)
    {
      if (!is.na(entry$PrevRow))
      {
        warning(paste("wrong prevRow,should be NA at ", i))
        tmp$PrevRow[i] = NA
      }
      
      i <- i+1
      next
    }
    
    #verify prevRow
    prevRow = prevInds[length(prevInds)]
    if ( is.na( entry$PrevRow) | entry$PrevRow != prevRow )
    {
      #warning(paste("Incorrect prevRow value found at row", i )) 
      tmp$PrevRow[i] = prevRow
    }
    
    if ( ( !is.na(entry$Score1) &&
           !is.na(entry$Score2)) && (
        entry$Score1 !=  tmp$Score1[prevInds[length(prevInds)]]  |
        entry$Score2 !=  tmp$Score2[prevInds[length(prevInds)]] ) )
    {
      warning(paste("Inconsistent Score found at row ",i))
      break; ## need attention
    }
    
    next_ind = i+1
    #verify if identical odd
    if( entry$HomeM == tmp$HomeM[prevInds[length(prevInds)]] &
        entry$AwayM == tmp$AwayM[prevInds[length(prevInds)]] & 
        entry$HomeJ == tmp$HomeJ[prevInds[length(prevInds)]] & 
        entry$AwayJ == tmp$AwayJ[prevInds[length(prevInds)]] 
        )
    {
      #remove?
      #warning(paste("identical odds found at row", i ))
      if(i == nrow(tmp))
      {
        tmp = tmp[1:(i-1)]
      } else
      {
        len= nrow(tmp)
        tmp$PrevRow[(i+1):len] =  tmp$PrevRow[(i+1):len] -1 
        tmp = tmp[c(1:(i-1),(i+1):len )]
        next_ind = i 
      }
    }
    i = next_ind
  }
  
  saveBetaData(tmp)
  
}



