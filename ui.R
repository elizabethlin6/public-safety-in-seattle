library("shiny")
library("rsconnect")
library("dplyr")
library("ggplot2")
library("lubridate")
library("mapproj")
library("maps")

df_crime <- read.csv(file = "data/crime.csv", sep = ",", stringsAsFactors = FALSE)
df_911 <- read.csv(file = "data/SEAfire911.csv", sep = ",", stringsAsFactors = FALSE)

ui <- fluidPage(
  fluidRow(
    
    column(
      10,
      offset = 1,
      tags$h2(tags$b("INFO 201 BE6: Public Safety in Seattle"))
    ),
    
    tags$br(),
    # summary of the project
    column(
      7,
      offset = 1,
      tags$br(),
      tags$br(),
      tags$div(textOutput("summary"))
    ),
    
    # basic info of team
    column(3,
           offset = 1,
           tags$blockquote(
           tags$b("Group BE6"),
           tags$br(),
           tags$div("This is the distribution of our work:"),
           tags$br(),
           tags$li(tags$b("Elizabeth:"), "interactive pie chart and description"),
           tags$li(tags$b("Yingrui:"), "interactive bar plot and description"),
           tags$li(tags$b("Colton:"), "interactive map and description"),
           tags$li(tags$b("Cheng:"), "Overview plots and summarise codes into a shiny app")
           )
    )
  ),
  
  tags$br(),
  tags$br(),
  
  # 1
  fluidRow(
    
    # title
    column(
      11,
      offset = 1,
      tags$h2(tags$b("1 Overview"))
    ),
    
    # Overview text & plots
    column(
      8,
      offset = 1,
      fluidRow(
        splitLayout(
          cellWidths = c("50%", "50%"), 
          plotOutput("firecall"), 
          plotOutput("crime")
          
        ),
      tags$br(),
      tags$div(textOutput("descrip_overview"))
      )
    ),
    
    # widgets of Overview
    column(
      3,
      tags$br(),
      tags$br(),
      tags$blockquote(textOutput("wid_overview")),
      radioButtons(
        "firecall",
        label = "Distribution of Firecall: Select a year",
        choices = list("Summary" = 1, "2017" = "2017", "2018" = "2018"),
        selected = 1
      ),
      radioButtons(
        "crime",
        label = "Distribution of Crime: Select a year",
        choices = list("Summary" = 1, "2017" = "2017", "2018" = "2018"),
        selected = 1
      )
      
    )
    
    
  ),
  
    tags$br(),
    tags$br(),
    
   # 2
    fluidRow( 
      
      # title
      column(
        11,
        offset = 1,
        tags$h2(tags$b("2 Crime Dataset Exploration"))
        
      ),
      
      # text & plots
    column(
      8,
      offset = 1,
      splitLayout(
        cellWidths = c("55%", "45%"),
        plotOutput('pie'),
        plotOutput("barPlot_state")
      ),
      tags$br(),
      tags$h4(tags$b("2.1 Pie Chart")),
      tags$div(textOutput("descrip_pie")),
      tags$br(),
      tags$h4(tags$b("2.2 Bar Chart")),
      tags$div(textOutput("descrip_bar"))
    
    ), 
    
    # widgets of Crime Dataset Exploration
    column(
      3,
      tags$br(),
      
      # type of crimes
      tags$blockquote(textOutput("wid_bar")),
      selectInput("type", 
                  label = "Select a Type of Crime", 
                  choices = unique(df_crime$Crime.Subcategory)
      ),
      tags$br(),
      # crime neiborhoods
      tags$blockquote(textOutput("wid_pie")),
      radioButtons("rank", 
                   label = "Pick Which Neighborhoods",
                   choices = list("Top 10 Crime Neighborhoods" = "top10", 
                                  "Last 10 Crime Neighborhoods" = "last10")
      )
      
    )
    
    
  ),
  
    tags$br(),
    tags$br(),

  # 3  
  fluidRow(  
    
    # title
    column(
      11,
      offset = 1,
      tags$h2(tags$b("3 Firecalls Dataset Exploration"))
    ),
    
    # text & plot
    column(
      8,
      offset = 1,
      splitLayout(
        cellWidths = c("50%", "50%"),
        tags$div(textOutput("descrip_map")),
        plotOutput("map"),
        cellArgs = list(style='white-space: normal;')
      )
    ),
    
    # widgets of 911 Dataset Exploration
    column(
      3,
      tags$br(),
      tags$br(),
      tags$blockquote(textOutput("wid_map")),
      
      selectInput("color", 
                  label = "Select a color:", 
                  c(Green = "green", 
                    Red = "red", 
                    Blue = "blue", 
                    Grey = "grey", 
                    Fire = "fire")
                  ),
      radioButtons(
        "year",
        "Select a year",
        c("2017", "2018")
      )
      
    
    )
    
  )  
      
      
    
)
  
