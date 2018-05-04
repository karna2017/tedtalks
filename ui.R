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
    
    fluidRow("The aim of this work is to,"),
    fluidRow("(1) understand people's overall engagement with talks, and"),
    fluidRow("(2) provide recommendations to potential viewers by providing popular talks and relevant ratings.
                  A person can select any of the 2550 talks and view its ratings."),
    br(),
    strong("Summary:"),
    fluidRow("TED talks continue to be well received but the level of viewers' engagement 
             as demonstrated by their comments has steeply gone down in recent years."),
    br(),
    
    strong("Observations from figures on the right:"),
    fluidRow("(1) The number of talks peaked in 2012 and has decreased slightly since then."),
    fluidRow("(2) Talks are described by viewers as inspiring, informative, fascinating, persuasive, 
             and beautiful. In some cases, talks are found to be just 'OK'."),
    fluidRow("Please see the figure on the right for a comprehensive list of rating-descriptions given
              by viewers. For each talk, every one of the 14 descriptions was selected by 
              0 or more viewers.
              The box plot shows the agragate number of rating-votes of viewers for all descriptions for all talks. 
             For example: the median for the rating 'inspiring' was 220, meaning, on average 220 people rated a talk as
             'inspiring', and half of the talks were rated 'inspiring' by more than 220 people.")
        ),
    
    conditionalPanel(
      'input.dataset === "Views and Comments"',
      
      strong("People's level of engagement, as demonstrated by the number of comments and views,
            increased sharply from the beginning of the publications 
          of talks to the year 2010, but there has been a sharp decline in people's engagement
            since 2010."),
      br(),
      br(),
      
      fluidRow("Some preliminary insights:"),
      fluidRow("(1) The grand total sum of views for all talks has remain more or less 
               the same from 2006 to 2017. But the same is not true for comments as number of comments
               have gone down significantly."),
      fluidRow("(2) The top two figures indicate that the ratio of 'comments per views' would 
              have gone down as well, and we do see the trend. The quantity plotted in the third 
              figure on the right is the 'number of comments per 1000 views'."),
      fluidRow("(3) The fourth figure shows the median of 'number of comments per 1000 views'
               for all talks.")
    ),      
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
      ),

conditionalPanel(
  'input.dataset === "Most Rated"',
  fluidRow("A set of 13 talks with 'rating count %' greater than 0.25%.  A 'rating count %' is defined 
           as the number of votes received for a particular rating-description divided by the total
           number of viewers."),
  fluidRow("An observation: 'Building a park in the sky' by Robert Hammond was rated beautiful
            by the highest % of viewers.")
),
conditionalPanel(
  'input.dataset === "Most Rated, Most Commented"',
  fluidRow("A set of 17 talks with 'rating count %' greater than 0.05% and the 'comment per 1000 views'
          greater than 1.")
  ),
conditionalPanel(
  'input.dataset === "Correlation"',
  fluidRow("The variable 'number of languages in which a talk is translated' has a positive 
           correlation with views and comments, a negative correlation with 'comments per views'."),
  br(),
  fluidRow("Comments, comments per views, and languages of translation have a negative correlation
           with the publication year.")
  )


    ),
    
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
          tabPanel("Most Rated, Most Commented", 
                  DT::dataTableOutput("mytable6"))

        )
    )
    
    
  )
))
