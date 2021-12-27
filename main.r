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

m6 <- function() {sort(sample(c(1:49),6))}

getModelFile <- function( )
{
  url<- "https://projects.fivethirtyeight.com/soccer-api/club/spi_matches_latest.csv"
  destfile = "tmp.csv"
  x <- download.file(url, destfile = destfile)
  raw = read.csv  (destfile)
  return (raw)
}

# Wanted Leagues in Model
getLeagues <- function()
{
  return (  leagues = c("UEFA Europa League", "UEFA Champions League",
                        "Barclays Premier League","English League Championship","English League One",
                        "English League Two" ,
                        "French Ligue 1", "French Ligue 2", "Italy Serie A",
                        "Dutch Eredivisie", "Belgian Jupiler League",
                        "German Bundesliga", "German 2. Bundesliga",
                        "Spanish Primera Division", "Spanish Segunda Division","Portuguese Liga",
                        "Major League Soccer",
                        "Swedish Allsvenskan", "Norwegian Tippeligaen",
                        "Scottish Premiership",
                        "Japanese J League","Russian Premier Liga"))
}

# Leagues Ignored in HKJC 
getIgnoredLeagues <- function()
{
  return ( c("UE Conference","Dutch Division 2","Chilean Division 1", "South American Cup", "Japanese Division 2","Argentine Division 1", "Mexican Premier",
             "Korean Division 1","Brazilian Division 1","Eng League Trophy","Japanese League Cup","Euro Nations League"
             ,"WC Qualifiers","Argentine Cup", "International Matches", "U21 Euro Qualifiers"))
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

getModelOdds <- function(nday)
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
                        Lose = round(1/x$prob2,digits =2)
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
  #oddsdata  = read_json(json_file, simplifyVector = TRUE, flatten= TRUE)[[2]][[2]]
  oddsdata =  fromJSON(json_path,simplifyVector = TRUE, flatten= TRUE)[[2]][[2]]
  res = data.table ( 
    MatchNum = oddsdata$matchNum,
    MatchDate = as.Date(oddsdata$matchDate ),
    MatchDay  = oddsdata$matchDay,
    League   =  oddsdata$league.leagueNameEN,
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
  res = read.csv(file = 'Matching.csv',header = TRUE)
  
  if ( nrow( res[ which (duplicated(res$Model)),])!= 0 |
       nrow( res[ which (duplicated(res$JC)),])!= 0 )
  {
    print ("ERROR: DUPLICATE")
    print( res[ which (duplicated(res$Model)),])
    
  }
  
  # Doesnt work for some reason, maybe the encoding during source('main/r')
  #res = rbind ( res , c("1. FC NÃ¼rnberg","Nurnberg"))
  #res = rbind ( res , c("Fortuna DÃ¼sseldorf","Dusseldorf"))
  #res = rbind ( res , c("AlavÃ©s","Alaves"))
  #res = rbind ( res , c("SpVgg Greuther FÃ¼rth","Greuther Furth"))
  #res = rbind ( res , c("Ã–stersunds FK" ,"Ostersunds FK"))
  #res = rbind ( res , c("Sporting GijÃ³n" ,"Sporting Gijon"))
  
  return ( res)
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
    return (which(team == modelTeams)[1])
  
  #Look up mapping
  findFromMapping = which( storedMatch$JC == team)
  if (length(findFromMapping)>0 )
  {
    teamModelName = storedMatch$Model[findFromMapping[1]]
    findmatch =  which(modelTeams == teamModelName)
    if (length(findmatch)>0)
      return (findmatch[1])
  }
  return (NA)  
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
    
  
    if ( is.na(x)  | is.na(y)  | x!=y )
    {
      #### not found 
      findind[i]=NA
      next
    } else
    {
      findind[i] = x 
    }
  }
  return(findind)
}

kellyToBet <- function(x)
{
  return ( 10*floor(round( ifelse( x > 0, x*100 , 0 ),1)))
}

BetSignal <- function( hkjc_odds, findind, model_odds)
{
  hkjc_odds = hkjc_odds[!is.na(findind),]
  findind = findind[!is.na(findind)]
  ModelImpHomeOdd = model_odds$Win[findind]
  ModelImpDrawOdd = model_odds$Draw[findind]
  ModelImpAwayOdd = model_odds$Lose[findind]
  
  HomeBet <-kellyToBet(kellyBet(ModelImpHomeOdd , hkjc_odds$HomeOdd))
  AwayBet <-kellyToBet(kellyBet(ModelImpAwayOdd , hkjc_odds$AwayOdd))
  DrawBet <-kellyToBet(kellyBet(ModelImpDrawOdd , hkjc_odds$DrawOdd))
  
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