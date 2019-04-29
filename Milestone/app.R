library(shiny)
library(tidyverse)
library(janitor)
library(ggpmisc) 
library(lubridate)
library(plotly)

Sys.setlocale("LC_ALL", "en_US.UTF-8")

tesla <- read_csv("Tesla.csv - Tesla.csv.csv") %>%
  clean_names() %>%
  mutate(date = parse_date(date, format = '%m/ %d/ %Y')) %>%
  filter(date > "2016-02-08") %>%
  filter(date < "2017-09-29")

tweets <- read_csv("data_elonmusk.csv", locale = locale(encoding = "UTF-8")) %>%
  clean_names() %>%
  mutate(time = as.Date(strptime(time, '%Y-%m-%d %H:%M:%S'),format = '%Y-%m-%d')) %>%
  filter(time > "2015-02-08") %>%
  filter(time < "2017-09-29") %>%
  rename(date = time)

join <- left_join(tesla, tweets, by = "date") %>%
  select(date, open, close, volume, tweet)


ui <- basicPage(
  titlePanel("Spikes in Tesla Stock Volume"),
  plotlyOutput("plot1"),
  verbatimTextOutput("info")
  
  #mainPanel(
    
    # Output: Tabset w/ plot, summary, and table ----
    #tabsetPanel(type = "tabs",
                #tabPanel("Stock Volume", plotOutput("volume")),
                #tabPanel("Tweets", verbatimTextOutput("tweets"))
# I plan to create tabs next, still have to figure out how to make the plot that will return which value you click within a tab.
)

server <- function(input, output) {
  output$plot1 <- renderPlotly({
    
    p <- join %>%
      select(date, volume, tweet) %>%
      filter(!is.na(tweet)) %>%
      ggplot(as.numeric = FALSE, aes(x = date, y = volume, text = paste("tweet:", tweet))) +
      geom_line() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date",
           y = "Volume") +
      stat_peaks(colour = "red") +
      stat_peaks(geom = "rug", colour = "red", sides = "b") +
      expand_limits(y = 80) 
    
    p %>%
      ggplotly()
    
# This has the shiny app create my ggplot, which shows peaks in tesla stock
# volume.
    

  })
}



shinyApp(ui, server)


