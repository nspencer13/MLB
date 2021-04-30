
library(shiny)
library(tidyverse)
library(Lahman)
library(lubridate)
library(mdsr)
library(plotly)
library(reshape2)
library(tidyr)
library(dplyr)
library(bslib)
#install.packages("thematic")
#library(thematic)

## Making Hitter Stats
batting2 <- Batting %>% 
  select(playerID, yearID,teamID, H, X2B, X3B, HR) %>%
  filter(yearID %in% c("2015","2016")) %>% 
  group_by(playerID, teamID, yearID) %>%
  summarise_all(funs(sum(., na.rm=TRUE))) %>% 
  mutate(X1 = H-(X2B+X3B+HR)) %>% 
  mutate(Single = X1) %>% 
  mutate(Double = X2B) %>% 
  mutate(Triple = X3B) %>% 
  mutate(HomeRun = HR)


## Slimming down Hitters Stats
bat2 <- batting2 %>% 
  select(teamID, yearID, Single, Double, Triple, HomeRun)

## Making it easier for ploting
bat_long2 <- melt(bat2, id.vars = c("playerID","teamID","yearID"))

newteam <- Teams %>% 
  group_by(teamID, name) %>% 
  summarise(1) %>% 
  select(teamID, name)
bat_long3 <- inner_join(bat_long2, newteam, by = "teamID")

batmaster <- inner_join(bat_long2, Master, by = "playerID") %>% 
  select("teamID", "yearID", "nameGiven", "variable", "value")
Batmaster <- inner_join(batmaster, newteam, by = "teamID")

## Getting Rid of retired team names
Teams1 <- bat_long3 %>% 
  group_by(name) %>% 
  summarise() %>% 
  filter(!name %in% c("Boston Americans","Chicago Orphans", "Chicago Colts", "Chicago White Stockings",
                    "Cincinnati Redlegs","Cleveland Blues", "Cleveland Bronchos",
                    "Cleveland Naps","Houston Colt .45's","Los Angeles Angels of Anaheim",
                    "New York Highlanders","Philadelphia Quakers","Philadelphia Blue Jays"
                    ,"Pittsburg Alleghenys","St. Louis Browns","St. Louis Perfectos",
                    "Washington Senators","Tampa Bay Devil Rays"))

## Making Year selector Option 
Years <- seq(2015,2016,1)

## Making Salaries List
money <- Salaries %>% 
  filter(yearID %in% c(2015,2016)) %>% 
  group_by(teamID, yearID, playerID) %>% 
  summarise(salary = sum(salary))

salaries1 <- inner_join(money, newteam, by = "teamID")
mastersalaries <- inner_join(salaries1,Master, by = "playerID") %>% 
  select("yearID", "nameGiven", "salary", "name")%>% 
  filter(!name %in% c("Boston Americans","Chicago Orphans", "Chicago Colts", "Chicago White Stockings",
                      "Cincinnati Redlegs","Cleveland Blues", "Cleveland Bronchos",
                      "Cleveland Naps","Houston Colt .45's","Los Angeles Angels of Anaheim",
                      "New York Highlanders","Philadelphia Quakers","Philadelphia Blue Jays"
                      ,"Pittsburg Alleghenys","St. Louis Browns","St. Louis Perfectos",
                      "Washington Senators","Tampa Bay Devil Rays"))

## Making Players Names for options
nameplayers<- mastersalaries %>% 
  group_by(nameGiven) %>% 
  summarise() 

## Updated Batmaster for interactive graph 1 and 2
TeamHits <- Batmaster %>% 
  group_by(yearID, name, variable) %>% 
  summarise(value = sum(value))


ui <-fluidPage(
  headerPanel("MLB Analysis: Salary"),
  helpText("Select the Options below to Compare Salaries with Batting Stats"),
  theme = shinythemes::shinytheme("yeti"),
  
 

  
  sidebarLayout(
    sidebarPanel(
      helpText("Three options below are for selection 1:"),
  selectInput("team","Teams:",Teams1),
  selectInput("year", "Years:",Years),
  uiOutput("player1"),
  helpText("Three options below are for selection 2:"),
    selectInput("team2","Teams:",Teams1),
  selectInput("year2", "Years:",Years),
  uiOutput("player2"),
  
  actionButton("goButton", "Compare MLB Stats!"),
  width = 3),
  
  
  
  
 fluidRow(
   mainPanel(
    column(6,plotlyOutput("plot1")),
    column(6,plotlyOutput("plot2")),
    column(6,plotlyOutput("plot3")),
    column(6,plotlyOutput("plot4")),
    column(6,plotlyOutput("plot5")),
    column(6,plotlyOutput("plot6"))
  
  ))))

server <- function(input, output, session) {
  
  
  values <- reactiveValues(
    a = NULL,
    b = NULL,
    c = NULL,
    d = NULL,
    e = NULL
    
  )
  observeEvent(input$goButton, {
    values$a<- input$team
    values$b<- input$year
    values$c<- input$player
    values$d<- input$team2
    values$e<- input$year2
    values$f<- input$player2
  })
  
  output$player1 <- renderUI({
    req(input$team, input$year)
    player_choices <- mastersalaries %>% 
      filter(name == input$team & yearID == input$year) %>% 
      pull(nameGiven)
    selectInput("player", "Player:",
                choices = player_choices)})
  
  output$player2 <- renderUI({
    req(input$team2, input$year2)
    player_choices <- mastersalaries %>% 
      filter(name == input$team2 & yearID == input$year2) %>% 
      pull(nameGiven)
    selectInput("player2", "Player:",
                choices = player_choices)})
    
   output$plot1 <- renderPlotly({
     req(values$a, values$b, values$c, values$d, values$e,values$f )
    TeamHits %>% 
    filter(name == values$a) %>% 
    filter(yearID == values$b) %>% 
      ggplot(aes(x = variable, y = value,text = paste("Type of Hit =", variable, "\nAmount of Hits =", value)))+
      geom_bar(stat = "identity", aes(fill = variable))+
      theme(legend.position = "none")+
      labs(x = "", y = "", title = paste(values$a,",",values$b))})
  
  output$plot2 <- renderPlotly({
    req(values$a, values$b, values$c, values$d, values$e,values$f)
    TeamHits %>% 
      filter(name == values$d) %>% 
      filter(yearID == values$e) %>% 
      ggplot(aes(x = variable, y = value,text = paste("Type of Hit =", variable, "\nAmount of Hits =", value)))+
      geom_bar(stat = "identity", aes(fill = variable))+
      theme(legend.position = "none")+
      labs(x = "", y = "", title = paste(values$d,",",values$e))})
  
  output$plot3 <- renderPlotly({
    req(values$a, values$b, values$c, values$d, values$e,values$f)
    mastersalaries %>% 
      filter(name == values$a & yearID == values$b) %>% 
      ggplot(aes(x = nameGiven, y = salary))+
      geom_bar(stat = "identity", fill = "orange")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
      labs(x = "Players", y ="Salary" , title = paste(values$a,",",values$b))
   }) 
  
  output$plot4 <- renderPlotly({
    req(values$a, values$b, values$c, values$d, values$e,values$f)
    mastersalaries %>% 
      filter(name == values$d & yearID == values$e) %>% 
      ggplot(aes(x = nameGiven, y = salary))+
      geom_bar(stat = "identity", fill = "orange")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
      labs(x = "Players", y ="Salary" , title = paste(values$d,",",values$e))
  }) 
  
  output$plot5 <- renderPlotly({
    req(values$a, values$b, values$c, values$d, values$e,values$f)
    Batmaster %>% 
      filter(name == values$a  & nameGiven == values$c & yearID == values$b) %>% 
      ggplot(aes(x = variable, y = value,text = paste("Type of Hit =", variable, "\nAmount of Hits =", value)))+
      geom_bar(stat = "identity", aes(fill = variable))+
      theme(legend.position = "none")+
      labs(x = "", y ="" , title = paste("Batting Stats for", values$c,"for",values$a,"in", values$b))
    })
  output$plot6 <- renderPlotly({
    req(values$a, values$b, values$c, values$d, values$e,values$f)
    Batmaster %>% 
      filter(name == values$d  & nameGiven == values$f & yearID == values$e) %>% 
      ggplot(aes(x = variable, y = value,text = paste("Type of Hit =", variable, "\nAmount of Hits =", value)))+
      geom_bar(stat = "identity", aes(fill = variable))+
      theme(legend.position = "none")+
      labs(x = "", y ="" , title = paste("Batting Stats for", values$f,"for",values$d, "in", values$e))
  })
}


shinyApp(ui, server)

