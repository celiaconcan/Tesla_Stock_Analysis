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
  rename(date = time)

join <- left_join(tesla, tweets, by = "date") %>%
  select(date, open, close, volume, tweet) %>%
  mutate(tweetDisplay = paste("tweet:", tweet))


ui <- fluidPage(
  titlePanel("Elon Musk and Tesla Stock"),
  verbatimTextOutput("info"),
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Plot", plotlyOutput("plot1")),
                tabPanel("Summary", verbatimTextOutput("summary"))))
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
}



shinyApp(ui, server)


