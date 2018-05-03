#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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
library(tm)
library(DT)
library(jpeg)
library(tidyr)
library(forcats)



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
    # Define a reactive expression for the document term matrix
    terms <- reactive({
      # Change when the "update" button is pressed...
      input$update
      # ...but not for anything else
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          getTermMatrix(input$selection)
        })
      })
    })
    
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot <- renderPlot({
      df_wordcloud = df_final[(df_final$talk_name==input$selection),]
      set.seed(1234)
      wordcloud(words = df_wordcloud$name, freq = df_wordcloud$count, min.freq = input$freq,
                random.order=FALSE, rot.per=0, 
                colors=brewer.pal(8, "Dark2"))
    })

    output$plot_MW <- renderPlot({
      df_wordcloud = df_final[(df_final$talk_name==input$selection_MW),]
      set.seed(1234)
      wordcloud(words = df_wordcloud$name, freq = df_wordcloud$count, min.freq = input$freq,
                random.order=FALSE, rot.per=0, 
                colors=brewer.pal(8, "Dark2"))
    })
    
    output$plot_MC_LW <- renderPlot({
      df_wordcloud = df_final[(df_final$talk_name==input$selection_MC_LW),]
      set.seed(1234)
      wordcloud(words = df_wordcloud$name, freq = df_wordcloud$count, min.freq = input$freq,
                random.order=FALSE, rot.per=0, 
                colors=brewer.pal(8, "Dark2"))
    })
    
    output$plot_MW_LC <- renderPlot({
      df_wordcloud = df_final[(df_final$talk_name==input$selection_MW_LC),]
      set.seed(1234)
      wordcloud(words = df_wordcloud$name, freq = df_wordcloud$count, min.freq = input$freq,
                random.order=FALSE, rot.per=0, 
                colors=brewer.pal(8, "Dark2"))
    })
    
    output$plot_MW_MC <- renderPlot({
      df_wordcloud = df_final[(df_final$talk_name==input$selection_MW_MC),]
      set.seed(1234)
      wordcloud(words = df_wordcloud$name, freq = df_wordcloud$count, min.freq = input$freq,
                random.order=FALSE, rot.per=0, 
                colors=brewer.pal(8, "Dark2"))
    })
    
    
        output$speaker_plot <- renderPlot({
      set.seed(1234)
      wordcloud(words = ted_speaker_occupation$speaker_occupation, freq = ted_speaker_occupation$n, 
                min.freq = input$min_f_Sp,scale=c(3,0.008),
                random.order=TRUE, rot.per=0, 
                colors=brewer.pal(8, "Dark2"))
    })
    
    
    # customize the length drop-down menu; display 5 rows per page by default
    output$mytable1 <- DT::renderDataTable({
      DT::datatable(MW, options = list(lengthMenu = c(5, 10, 15), 
                                       pageLength = 5,class = 'white-space: nowrap'))
    })
    
    output$mytable2 <- DT::renderDataTable({
      DT::datatable(MW_MC, options = list(lengthMenu = c(5, 10, 15), 
                                       pageLength = 5,class = 'white-space: nowrap'))
    })
    
    output$mytable3 <- DT::renderDataTable({
      DT::datatable(MW_LC, options = list(lengthMenu = c(5, 10, 15), 
                                       pageLength = 5,class = 'white-space: nowrap'))
    })
    
    
    output$mytable4 <- DT::renderDataTable({
      DT::datatable(LW_MC, options = list(lengthMenu = c(5, 10, 15), 
                                       pageLength = 5,class = 'white-space: nowrap'))
    })

    output$mytable5 <- DT::renderDataTable({
      DT::datatable(MR, options = list(lengthMenu = c(5, 10, 15), 
                                          pageLength = 5,class = 'white-space: nowrap'))
    })
    
    output$mytable6 <- DT::renderDataTable({
      DT::datatable(MR_MC, options = list(lengthMenu = c(5, 10, 15), 
                                          pageLength = 5,class = 'white-space: nowrap'))
    })
    
    output$figure1 <- renderPlot({
      g=ggplot(data=talks_year,aes(x=year,y=yearly_count))+labs(x = "year", y="number of talks")
      g + geom_bar(stat="identity",width=0.4,color="blue", fill="azure1")+ theme_minimal()
    })
    
    output$figure2 <- renderPlot({
      g1=ggplot(data=talks_year,aes(x=year,y=C_1K_V))+labs(x = "year", y = "cumulative comments per 1000-views for all talks")
      g1 + geom_bar(stat="identity",width=0.4,color="red4", fill="mistyrose")+ theme_minimal()
    })

    output$figure3 <- renderPlot({
      g3 =  ggplot(ted, aes(x=year, y=views)) + geom_boxplot(color='blue4')+labs(x = "year", y="cumulative views")
      g3+scale_y_log10()+ theme_minimal()
    })
    
    output$figure4 <- renderPlot({
      g4 =  ggplot(ted, aes(x=year, y=comments)) + geom_boxplot(color='blue4')+labs(x = "year", y="cumulative comments")
      g4+scale_y_log10()+ theme_minimal()
    })
 
    output$figure5 <- renderPlot({
      g_talks = ggplot(df_final, aes(x= fct_reorder(name,count, fun = median), y=count)) + labs(x = "Rating Category",y="Count")+geom_boxplot(color='blue4')+theme_minimal(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=45))
      g_talks+scale_y_log10()+ ggtitle("Vote distribution for rating categories")
    })
 
    output$figure6 <- renderPlot({
      g_c_1000v=ggplot(mean_median_CperV, aes(year_num))+geom_line(aes(y = mean_CperV, colour = "mean"))+geom_line(aes(y = median_CperV, colour = "median"))+ labs(x = "Year",y="Comment per 1000 Views")+ theme_minimal()
    })
    
    
    output$corr_plot <- renderPlot({
      corrplot(cor(ted_corr),method="circle",type = "upper",
               bg="white",title="Correlation between variables")
    })
    
    output$ratings_plot <- renderPlot({
     readJPEG("ted_talks_ratings_top.jpeg", native = FALSE)
    })
    
    })
