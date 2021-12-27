source('main.r' )


############# Dummy Feature B


############# Dummy Feature B2


##### 1.  Prepare Model
raw = getModelFile()
model_odds = getModelOdds(4) 

#### 2. Manually download json  TODO automate this 
json_path <- "https://bet.hkjc.com/football/getJSON.aspx?jsontype=odds_had.aspx"
hkjc_odds <- ReadOddsFromPath(json_path )

if(nrow(hkjc_odds) == 0)
{
  print ("No relevant matches")
} else {
  
  #### 3. Process and match entries 
  
  findind = MatchRowInds(hkjc_odds,model_odds )
  
  hkjc_odds_missing = data.table()
  if (length(which(is.na(findind)))>0) {
    print ("Missing")
    hkjc_odds_missing = hkjc_odds[which(is.na(findind)),]
    print(hkjc_odds_missing)
    newMapping  <- newMappingSuggestion(model_odds,unique(c( hkjc_odds_missing$HomeTeam,hkjc_odds_missing$AwayTeam)))
    print("Add to mapping?")
    newMapping
    if(nrow(newMapping))
    {
      addToMappingFile(newMapping)
      
    }
  }
  BettingSuggestion = BetSignal(hkjc_odds,findind,model_odds)
  cleanBettingSuggestion = subset(BettingSuggestion, HomeM + DrawM + AwayM < Inf)  
  
  
  #cleanBettingSuggestion
  
subset(cleanBettingSuggestion, HomeBet  > 0 ) 

  
    
  ### Continue with betting action 
  

}

#View(BettingSuggestion)
