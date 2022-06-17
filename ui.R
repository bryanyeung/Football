library(shiny)
fluidPage(
  
  # Application title
  titlePanel("Get rich fast with football betting"),
  hr(),
  mainPanel(
    h3("Suggested Bets"),
    tableOutput("BettingSuggestion"),
    hr(),
    h3("Possible New Mappings"),
    tableOutput("newMapping"),
    textOutput("message"),
  )
)