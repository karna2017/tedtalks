#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(googleVis)
library(corrplot)
library(ggplot2)
library(anytime)
library(dplyr)
#library(plyr)
library(ggplot2)
library(stringr)
library(qdapRegex)
library(rjson)
library(rsconnect)
library(wordcloud)
#library(tm)
library(DT)
library(jpeg)
library(tidyr)
library(forcats)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Observations from TED Talks"),
  
  
  sidebarLayout(fluid = TRUE,
    
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      
      conditionalPanel(
        'input.dataset === "Home"',
        strong("Background:"),
      fluidRow("TED is a not-for-profit organization devoted to spreading ideas via short talks. Please see ted.com for more details. The dataset used in this shiny-app was obtained from"), 
    a("Kaggle.",href="https://www.kaggle.com/rounakbanik/ted-talks",target="_blank"),
    fluidRow("The app analyzes all the talks published from 2006 to 2017."),
      br(),
      br(),
    fluidRow("Some preliminary insights:"),
    fluidRow("(1) The publication of talks increased steadily from 2006 to 2012, and there has been a decline afterwards."),
    fluidRow("(2) Importantly, people's level of engagement increased sharply from the beginning of the publications of talks to the year 2010, and then, there has been a shart decline in people's engagement.
             Level of engagement is measured by dividing the sum of all comments for all talks by the sum of all views for all talks."),
    fluidRow("(3) The median number of views for a talk in a given year has not drastically changed over the years."),
    br()
      ),
      
#      conditionalPanel(
#        'input.dataset === "diamonds"',
#        checkboxGroupInput("show_vars", "Columns in diamonds to show:",
#                           names(diamonds), selected = names(diamonds))
#      )
      
      fluidRow(
        conditionalPanel(
          'input.dataset === "Wordcloud of Ratings"',
        selectInput("selection", "Choose a Talk:",
                  choices = ted$name),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency of Comments:",
                  min = 1,  max = 25000, value = 1)
#      sliderInput("max",
#                 "Maximum Types of Ratings (please use the maximum for completeness):",
#                  min = 1,  max = 14,  value = 14)
        ),
conditionalPanel(
  'input.dataset === "Most Viewed"',
  selectInput("selection_MW", "Choose from the top 1% most-watched Talks:",
              choices =ted$name[ted$Views_Percentile>99]),
  actionButton("update", "Change"),
  hr(),
  sliderInput("freq",
              "Minimum Frequency of Comments:",
              min = 1,  max = 25000, value = 1)
 ),

conditionalPanel(
  'input.dataset === "Most Viewed, Most Commented"',
  selectInput("selection_MW_MC", "Choose from the top 85% of the most-viewed AND the top 85% of the most commented Talks:",
              choices = ted$name[ted$Views_Percentile>85 & ted$C_1K_V_Percentile>85]),
  actionButton("update", "Change"),
  hr(),
  sliderInput("freq",
              "Minimum Frequency of Comments:",
              min = 1,  max = 25000, value = 1)
),

conditionalPanel(
  'input.dataset === "Most Viewed, Least Commented"',
  selectInput("selection_MW_LC", "Choose from the top 95% of the most-viewed AND the bottom 10% of the most commented Talks:",
              choices = ted$name[ted$Views_Percentile>95 & ted$C_1K_V_Percentile<10]),
  actionButton("update", "Change"),
  hr(),
  sliderInput("freq",
              "Minimum Frequency of Comments:",
              min = 1,  max = 25000, value = 1)
),

conditionalPanel(
  'input.dataset === "Most Commented, Least Viewed"',
  selectInput("selection_MC_LW", "Choose from the top 95% of the most-commented AND the bottom 2% of the least viewed Talks:",
              choices = ted$name[ted$Views_Percentile<2 & ted$C_1K_V_Percentile>95]),
  actionButton("update", "Change"),
  hr(),
  sliderInput("freq",
              "Minimum Frequency of Comments:",
              min = 1,  max = 25000, value = 1)
),

      conditionalPanel(
        'input.dataset === "Speaker Occupation"',
      sliderInput("min_f_Sp",
                  "Minimum Frequency of Occupation of Speakers:",
                  min = 1,  max = 40, value = 5)
      )
    )),
    
    # Show Word Cloud
    mainPanel(
        tabsetPanel(
          id = 'dataset',
          tabPanel("Home", 
                   fluidRow(
                         ),
                   plotOutput("figure1"),plotOutput("figure5")
                   ),
          tabPanel("Views and Comments", 
                   fluidRow(
                   ),
                   plotOutput("figure3"),
                   br(),
                   plotOutput("figure4"),plotOutput("figure2"),plotOutput("figure6")
                  ),
          tabPanel("Speaker Occupation", 
                   p("Writers lead this cohort followed by artists, designers, journalists, and entrepreneurs."),
                   plotOutput("speaker_plot")),
          
          tabPanel("Correlation",
                   br(),
                   br(),
                   plotOutput("corr_plot")),
          
          tabPanel("Wordcloud of Ratings",
                   plotOutput("plot")),
#          tabPanel("Top Category Ratings",
#                   plotOutput(ratings_plot)),
#                print(img(src='ted_talks.pdf'))),
#          print(tags$iframe(src="./ted_talks.pdf"))),
          tabPanel("Most Viewed", 
                   plotOutput("plot_MW"),
                  DT::dataTableOutput("mytable1")),
          tabPanel("Most Viewed, Most Commented", 
                   plotOutput("plot_MW_MC"),
                   DT::dataTableOutput("mytable2")),
          tabPanel("Most Viewed, Least Commented", 
                   plotOutput("plot_MW_LC"),
                   DT::dataTableOutput("mytable3")),
          tabPanel("Most Commented, Least Viewed", 
                   plotOutput("plot_MC_LW"),
                   DT::dataTableOutput("mytable4")),
          tabPanel("Most Rated", 
                   DT::dataTableOutput("mytable5")),
          tabPanel("Most Rated, Most Commented ", 
                  DT::dataTableOutput("mytable6"))

        )
    )
    
    
  )
))
