library(shiny)
library(tidyverse)
library(janitor)
library(ggpmisc) 
library(lubridate)
library(plotly)
library(DT)


tesla <- read_csv("Tesla.csv - Tesla.csv.csv") %>%
  clean_names() %>%
  mutate(date = parse_date(date, format = '%m/ %d/ %Y')) %>%
  filter(date > "2016-02-08") %>%
  filter(date < "2017-09-29")

tweets <- read_csv("data_elonmusk.csv") %>%
  clean_names() %>%
  mutate(time = as.Date(strptime(time, '%Y-%m-%d %H:%M'),format = '%Y-%m-%d')) %>%
  filter(time > "2015-02-08") %>%
  filter(time < "2017-09-29") %>%
  rename(date = time) %>%
  select(date, tweet)

join <- left_join(tesla, tweets, by = "date") %>%
  select(date, open, close, volume, tweet) %>%
  mutate(tweetDisplay = paste("tweet:", tweet))


ui <- fluidPage(
  titlePanel("Elon Musk and Tesla Stock"),
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Plot", plotlyOutput("plot1")),
                tabPanel("Tweets", dataTableOutput("table")),
                tabPanel("About", htmlOutput("summary"))))
)

server <- function(input, output) {
  output$plot1 <- renderPlotly({
    
    p <- join %>%
      select(date, volume, tweet, tweetDisplay) %>%
      filter(!is.na(tweet)) %>%
      ggplot(aes(x = date, y = volume, text = tweetDisplay)) +
      geom_point() +
      geom_line() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date",
           y = "Volume") 
    
    p %>%
      ggplotly()
    
# This has the shiny app create my ggplot, which shows peaks in tesla stock
# volume.
    

  })
  output$summary <- renderUI({
    
    str1 <- paste("About this App")
    str2 <- paste("This app explores how Elon Musk's tweets affect the volume and share price of Tesla stock.")
    str3 <- paste("About the Creator")
    str4 <- paste("This app was created by Celia Concannon as a final project for GOV1005: Data at Harvard College in Spring 2019.")
    str5 <- paste("Contact Information")
    str6 <- paste("Email: celiaconcannon@college.harvard.edu")
    str7 <- paste("Github Link")
    str8 <- paste("https://github.com/celiaconcan/Tesla_Stock_Analysis")
    str9 <- paste("Data Source")
    str10 <- paste("Kaggle.com")
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h5(str5), p(str6), h5(str7), p(str8), h5(str9), p(str10)))}) 
  
  
  output$table <- DT::renderDataTable({
    DT::datatable(tweets)
  })  

}



shinyApp(ui = ui, server = server)


