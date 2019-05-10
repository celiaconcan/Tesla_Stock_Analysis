library(shiny)
library(tidyverse)
library(janitor)
library(ggpmisc) 
library(lubridate)
library(plotly)
library(shinythemes)
library(DT)


tesla <- read_csv("Tesla.csv - Tesla.csv.csv") %>%
  clean_names() %>%
  mutate(date = parse_date(date, format = '%m/ %d/ %Y')) %>%
  filter(date > "2016-02-08") %>%
  filter(date < "2017-09-29")

# I picked this date range as a reasonale amount of time to display on the
# x-axis of the graph without it being cluttered with too many points.

tweets <- read_csv("data_elonmusk.csv") %>%
  clean_names() %>%
  mutate(time = as.Date(strptime(time, '%Y-%m-%d %H:%M'),format = '%Y-%m-%d')) %>%
  filter(time > "2015-02-08") %>%
  filter(time < "2017-09-29") %>%
  rename(date = time) %>%
  select(date, tweet)

# Had to clean, names, get the date columns to match and to be in the same date
# range so I could join the two by date.

join <- left_join(tesla, tweets, by = "date") %>%
  select(date, open, close, volume, tweet) %>%
  mutate(tweetDisplay = paste("tweet:", tweet))

# Joins the two data sets by date and creates a new variable called tweetDisplay
# which I will use in my plotly hover feature.

ui <- navbarPage("Elon Musk and Tesla Stock", 
                 theme = shinytheme("yeti"),
  
# Gives a title to the app and adds the yeti theme from the shiny themes
# package.
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Stock Volume",
                         plotlyOutput("plot1"),
                         br(),
                         h6("Volume is the number of shares or contracts traded in a security during a given period of time.  Higher volume indicates higher interest in a stock on a certain day.  This graph shows spikes in volume of Tesla stock traded over a period of two and a half years and the corresponding Elon Musk tweets on those days.")),
                tabPanel("Stock Price",
                                                       
                                                       br(),
                                                       
                                                       sidebarPanel(
                                                         selectInput("var",
                                                                     label = "Choose a variable to display",
                                                                     choices = c("Closing Price", "Opening Price"),
                                                                     selected = "Closing Price")
                                                       ),
                                                       mainPanel(
                                                         plotlyOutput("plot2"),
                                                         br()
                                                       ),
                                                       br(),
                                                       h6("The price movement of a stock is an indication of how much investors value a company at a certain time.")
                ),

# Creates a user select feature in the stock price tab which allows the user to
# select between closing prices and opening prices to be displayed.
                
                tabPanel("Search Tweets", dataTableOutput("table")),
                tabPanel("About", htmlOutput("summary")))
))

# tabPanel creates all my tabs, which the user can click between to view
# different features of the app.

server <- function(input, output) {
  output$plot1 <- renderPlotly({
    
    p <- join %>%
      select(date, volume, tweet, tweetDisplay) %>%
      filter(!is.na(tweet)) %>%
      
# Removes all tweets which are NA.
      
      ggplot(aes(x = date, y = volume, text = tweetDisplay)) +
      
# Creates a graph with date on the x-axis and volume on the y-axis, with the
# text for the plotly hover feature to be the tweetDisplay variable I created
# above.
      
      geom_point(size = 0.7, color = 'blue') +
      
# Alters the size and color of the points on the graph.
      
      geom_line(group = 1) +
      
# Without the group aesthetic the line does not show up.
  
      scale_y_continuous(labels = scales::comma) +
      
# Takes the y-axis labels out of scientific notation.  
  
      labs(x = "Date",
           y = "Volume") 

# Names the axis of the graph.    
    
    p %>%
      ggplotly()
    
  })
  
# Wraps the ggplot with plotly to make it interactive.
  
output$plot2 <- renderPlotly({  
  if(input$var == "Opening Price") {
    c <- join %>%
      select(date, open, close, tweet, tweetDisplay) %>%
      filter(!is.na(tweet)) %>%
      ggplot(aes(x = date, y = open, text = tweetDisplay)) +
      geom_point(size = 0.7, color = 'green') +
      geom_line(group = 1) +
      scale_y_continuous() +
      labs(x = "Date",
           y = "Price") 
    
    c %>%
      ggplotly()
    
  } else {
    c <- join %>%
      select(date, open, close, tweet, tweetDisplay) %>%
      filter(!is.na(tweet)) %>%
      ggplot(aes(x = date, y = close, text = tweetDisplay)) +
      geom_point(size = 0.7, color = 'red') +
      geom_line(group = 1) +
      scale_y_continuous() +
      labs(x = "Date",
           y = "Price") 
    
    c %>%
      ggplotly()
  }

# This is the same code I used in my graph which looks at stock volume, but
# looking at the user selected variable, either opening price or closing price.
  
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


