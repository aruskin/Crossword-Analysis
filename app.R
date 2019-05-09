library(shiny)

source("get_latimes_crossword.R")

ui <- fluidPage(
  dateRangeInput("dateRange", "Select date range:"),
  actionButton("getData", "Get Crossword Data!"),
  verbatimTextOutput("messageCenter"),
  tableOutput('top10words')
)

server <- function(input, output, session){
  data_reactive <- eventReactive(input$getData, {
    min_date <- input$dateRange[1]
    max_date <- input$dateRange[2]
    validate(
      need(!(is.na(min_date) | is.na(max_date)), 'Must enter beginning and end dates'),
      need(min_date <= max_date, 'Beginning date must be earlier than end date'),
      need(max_date <= Sys.Date(), 'End date cannot be in future'),
      need(min_date >= MIN_AVAILABLE_DATE, 
           paste('Earliest available crossword is from',
                 as.character(MIN_AVAILABLE_DATE)))
    )
    my_data <- get_crosswords_range(min_date, max_date)
    x <- clean_crossword_dataset(my_data)
    x
  })
  
  output$messageCenter<- renderText({
    x <- data_reactive()
    min_date <- min(x$clean_data$date)
    max_date <- max(x$clean_data$date)
    n_cwds <- x$clean_data$date %>% unique %>% length

   paste(data_reactive()$message,
        '\n',
        paste0("Pulled ", n_cwds, " puzzles from ", min_date, " to ", max_date))
  })
  
  output$top10words <- renderTable({
    x <- data_reactive()
    n_most_frequent_answers(x$clean_data, 10)
  })
}

shinyApp(ui, server)