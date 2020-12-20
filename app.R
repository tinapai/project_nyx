library(shiny)
library(lubridate)
library(plotly)
library(tidyverse)
movies <- readRDS("movie_final.rds")
movie_genres <- unique(movies$Genre)
# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Movielens Demographic Dive"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "genre",
                  label = "Select Movie Genre",
                  choices = movie_genres[-1]) # exclude unknown genre
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("genderPlot")
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  movies_filtered <- reactive({
    movies %>%
      filter(movie_id != 267) %>%
      filter(Genre == input$genre, `Tagged?` == 1) %>% # no need to use !duplicated since we specify the specific genre
      group_by(gender, Year_Rev = year(timestamp), Mon_Rev = month(timestamp)) %>%
      summarise(NumReviews = n()) %>% 
      arrange(Year_Rev, Mon_Rev) %>%
      mutate(Day_Rev = 1) %>%
      unite("ReviewDateChr", Mon_Rev, Day_Rev, Year_Rev, sep = "/") %>%
      mutate(ReviewDate = mdy(ReviewDateChr)) %>%
      select(-ReviewDateChr)
  })
  output$genderPlot <- renderPlot({
    ggplot(movies_filtered(), aes(ReviewDate, NumReviews)) + 
      geom_line(aes(color = gender)) + 
      scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month") +
      theme_minimal() +
      xlab("Review Date")+
      ylab("Number of Reviews")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)