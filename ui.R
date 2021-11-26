library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(title="IPL ANALYSIS"),
    dashboardSidebar(width = 360,
      sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("fas fa-database")),
      menuItem("Average runs per ball by over each season and innings",tabName = "avg",icon = icon("fas fa-chart-line")),
      menuItem("Toss Advantage",tabName = "toss",icon = icon("fas fa-chart-bar")),
      menuItem("Breakdown of dismissal type",tabName = "runs",icon = icon("fas fa-chart-bar")),
      menuItem("Total Runs of Tournament winners season wise",tabName = "totalruns",icon = icon("fas fa-chart-bar")),
      menuItem("Total wickets of Tournament winners  season wise",tabName = "totalwks",icon = icon("fas fa-chart-bar")),
      menuItem("Highest run scorers ",tabName = "highest_runs_scorer",icon = icon("fas fa-chart-bar")),
      menuItem("Highest wickter takers ",tabName = "highest_wkts_taker",icon = icon("fas fa-chart-bar")),
      menuItem("Total runs of teams by season wise",tabName = "team_runs",icon = icon("fas fa-chart-line")),
      menuItem("Total wickets of teams by season wise",tabName = "team_wkts",icon = icon("fas fa-chart-line")),
      menuItem("Total runs scored in 1s to 6s",tabName = "runs_scored",icon = icon("fas fa-chart-bar")),
      menuItem("Toss and Match wins by each Team",tabName = "toss_and_match",icon = icon("fas fa-chart-bar")),
      menuItem("Toss decisions by each Team",tabName = "toss_decision",icon = icon("fas fa-chart-bar")),
      menuItem("Decision Tree ",tabName = "Decision_Tree"),
      menuItem("Decision Tree 2",tabName = "D_Tree")
    )),
                    
    dashboardBody(
      
      tabItems(
        tabItem(tabName = "avg",
                fluidRow(
          box(plotOutput("geom",height = 600),width = "40%"))
        ),
        tabItem(tabName = "toss",
                fluidRow(
                  box(plotOutput("win",height = 600),width = "40%")
                )
        ),
        tabItem(tabName = "runs",
                fluidRow(box(plotOutput("run",height = 500),width = "40%"))
                ),
        tabItem(tabName = "totalruns",
                fluidRow(box(plotOutput("tr",height = 500),width = "40%"))
              
          ),
        
        tabItem(tabName = "totalwks",
                fluidRow(box(plotOutput("twks",height = 500),width = "40%"))
        ),
        tabItem(tabName = "highest_runs_scorer",
                fluidRow(box(plotOutput("hrs",height = 500),width = "40%"))
        ),
        tabItem(tabName = "highest_wkts_taker",
                fluidRow(box(plotOutput("hwt",height = 500),width = "40%")
        )),
        tabItem(tabName = "team_runs",
                fluidRow(box(plotOutput("trs",height = 500),width = "40%")
        )),
        tabItem(tabName = "team_wkts",
                fluidRow(box(plotOutput("twk",height = 500),width = "40%"))
        ),
        tabItem(tabName = "runs_scored",
                fluidRow(box(plotOutput("rs",height = 500),width = "40%"))
        ),
        tabItem(tabName = "toss_and_match",
                fluidRow(box(plotOutput("tam",height = 500),width = "40%"))
        ),
        tabItem(tabName = "toss_decision",
                fluidRow(box(plotOutput("td",height = 500),width = "40%"))
        ),
        tabItem(tabName = "data",
                fluidRow(box(title = "Choose Csv File",status = "primary",background = "black",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             fileInput('file1', 'CSV File',
                                       accept=c('text/csv', 
                                                'text/comma-separated-values,text/plain', 
                                                '.csv')))),
                fluidRow(box(title="Wordcloud"
                             ,background = "blue",width=12,plotOutput("wordcloud"))),
                
                tableOutput("content")
                ),
        tabItem(tabName = "Decision_Tree",
                fluidRow(box(plotOutput("dt",height = 600),width = "40%"),
                         fluidRow(box(title="Accuray",
                                      textOutput("acc")))     
                         )
        ),
        tabItem(tabName = "D_Tree",
                fluidRow(box(plotOutput("dtt",height = 600),width = "40%"))
        )
        
               
                             
        
      
    
    
    
)
)
)
)

