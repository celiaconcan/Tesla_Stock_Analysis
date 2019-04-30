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

# I picked this date range as a reasonale amount of time to display on the
# x-axis of the graph without it being cluttered with two many points.

tweets <- read_csv("data_elonmusk.csv") %>%
  clean_names() %>%
  mutate(time = as.Date(strptime(time, '%Y-%m-%d %H:%M'),format = '%Y-%m-%d')) %>%
  filter(time > "2015-02-08") %>%
  filter(time < "2017-09-29") %>%
  rename(date = time) %>%
  select(date, tweet)

# Had to clean, names, get the date columns to match and to be in the same date
# range so I could join the two.

join <- left_join(tesla, tweets, by = "date") %>%
  select(date, open, close, volume, tweet) %>%
  mutate(tweetDisplay = paste("tweet:", tweet))

# Joins the two data sets by date and creates a new variable called tweetDisplay
# which I will use in my plotly hover feature.

ui <- fluidPage(
  titlePanel("Elon Musk and Tesla Stock"),
  
# Gives a title to the app.  
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Plot", plotlyOutput("plot1")),
                tabPanel("Tweets", dataTableOutput("table")),
                tabPanel("About", htmlOutput("summary"))))
)

# Creates my tabs, I plan on creating additional tabs by the due date.  I also
# want to show how tweets affect stock closing prices.

server <- function(input, output) {
  output$plot1 <- renderPlotly({
    
    p <- join %>%
      select(date, volume, tweet, tweetDisplay) %>%
      filter(!is.na(tweet)) %>%
      ggplot(aes(x = date, y = volume, text = tweetDisplay)) +
      geom_point(size = 0.7, color = 'red') +
      geom_line(group = 1) +
      
# Without the group aesthetic the line does not show up.
  
      scale_y_continuous(labels = scales::comma) +
      
# Takes the y-axis labels out of scientific notation.  
  
      labs(x = "Date",
           y = "Volume") 
    
    p %>%
      ggplotly()
    
# This has the shiny app create my ggplot, which shows peaks in tesla stock
# volume. ggplotly() wraps the ggplot in a plotly and allows me to use the hover
# function to see text for each point.  I would like to use similar code going
# forward to create a similar graph but using closing prices instead of volume.

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
  
# This creates each line of text for my about page.  The last line formats the
# size of the text with headers being larger.
  
  output$table <- DT::renderDataTable({
    DT::datatable(tweets)
  })  

}



shinyApp(ui = ui, server = server)


