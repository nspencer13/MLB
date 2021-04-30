
library(shiny)
library(tidyverse)
library(Lahman)
library(lubridate)
library(mdsr)
library(plotly)
library(reshape2)
library(tidyr)
library(dplyr)

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

bat2 <- batting2 %>% 
  select(teamID, yearID, Single, Double, Triple, HomeRun)

bat_long2 <- melt(bat2, id.vars = c("playerID","teamID","yearID"))

newteam <- Teams %>% 
  group_by(teamID, name) %>% 
  summarise(1) %>% 
  select(teamID, name)
bat_long3 <- inner_join(bat_long2, newteam, by = "teamID")

batmaster <- inner_join(bat_long2, Master, by = "playerID") %>% 
  select("teamID", "yearID", "nameGiven", "variable", "value")
Batmaster <- inner_join(batmaster, newteam, by = "teamID")

Teams1 <- bat_long3 %>% 
  group_by(name) %>% 
  summarise() %>% 
  filter(!name %in% c("Boston Americans","Chicago Orphans", "Chicago Colts", "Chicago White Stockings",
                    "Cincinnati Redlegs","Cleveland Blues", "Cleveland Bronchos",
                    "Cleveland Naps","Houston Colt .45's","Los Angeles Angels of Anaheim",
                    "New York Highlanders","Philadelphia Quakers","Philadelphia Blue Jays"
                    ,"Pittsburg Alleghenys","St. Louis Browns","St. Louis Perfectos",
                    "Washington Senators","Tampa Bay Devil Rays"))
Years <- seq(2015,2016,1)

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

nameteam <- mastersalaries %>% 
  group_by(nameGiven) %>% 
  summarise() 



ui <-fluidPage(
  headerPanel("Hit's Counter : MLB Teams"),
  helpText("This Shiny App will let you look at whhat kind of hits a MLB teams had in a single year!"),
  
  fluidRow( column(
    width = 6,
    selectInput("team","Teams:",Teams1),
  selectInput("year", "Years:",Years),
  #selectInput("player", "Player:", nameteam)),
  uiOutput("player1")),
  
  column(
    width =6,
    selectInput("team2","Teams:",Teams1),
  selectInput("year2", "Years:",Years)),
  
  
  actionButton("goButton", "Go!"),
  
  
  
  
 fluidRow(
   mainPanel(
    column(6,plotOutput("plot1")),
    column(6,plotOutput("plot2")),
    column(6,plotOutput("plot3")),
    column(6,plotOutput("plot4"))
  
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
  })
  
  output$player1 <- renderUI({
    req(input$team, input$year)
    player_choices <- mastersalaries %>% 
      filter(name == input$team & yearID == input$year) %>% 
      pull(nameGiven)
    selectInput("player", "Player1",
                choices = player_choices)
  })
  
   output$plot1 <- renderPlot({
    req(values$a, values$b, values$c, values$d, values$e)
    Batmaster %>% 
    filter(name == values$a) %>% 
    filter(yearID == values$b) %>% 
      ggplot(aes(x = variable, y = value,text = paste("Type of Hit =", variable, "\nAmount of Hits =", value)))+
      geom_bar(stat = "identity", aes(fill = variable))+
      labs(x = "", y = "", title = paste(values$a,",",values$b))})
  
  output$plot2 <- renderPlot({
    req(values$a, values$b, values$c, values$d, values$e)
    bat_long3 %>% 
      filter(name == input$team2) %>% 
      filter(yearID == input$year2) %>% 
      ggplot(aes(x = variable, y = value,text = paste("Type of Hit =", variable, "\nAmount of Hits =", value)))+
      geom_bar(stat = "identity", aes(fill = variable))+
      labs(x = "", y = "", title = paste(input$team2,",",input$year2))})
  
  output$plot3 <- renderPlot({
    req(values$a, values$b, values$c, values$d, values$e)
    mastersalaries %>% 
      filter(name == input$team & yearID == input$year) %>% 
      ggplot(aes(x = nameGiven, y = salary))+
      geom_bar(stat = "identity", fill = "orange")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
      labs(x = "Players", y ="Salary" , title = paste(input$team,",",input$year))
   }) 
  
  output$plot4 <- renderPlot({
    req(values$a, values$b, values$c, values$d, values$e)
    Batmaster %>% 
      filter(name == values$a  & nameGiven == values$c & yearID == values$b) %>% 
      ggplot(aes(x = variable, y = value,text = paste("Type of Hit =", variable, "\nAmount of Hits =", value)))+
      geom_bar(stat = "identity", aes(fill = variable))
    
  
  })
}


shinyApp(ui, server)

