library(shiny)

source("get_latimes_crossword.R")

ui <- fluidPage(
  dateInput("minDate", "Beginning Date:"),
  dateInput("maxDate", "End Date"),
  actionButton("getData", "Get Crossword Data!"),
  verbatimTextOutput("messageCenter"),
  tableOutput('top10words')
)

server <- function(input, output, session){
  data_reactive <- eventReactive(input$getData, {
    validate(
      need(!(is.na(input$minDate) | is.na(input$maxDate)), 'Must enter beginning and end dates'),
      need(input$minDate <= input$maxDate, 'Beginning date must be earlier than end date'),
      need(input$maxDate <= Sys.Date(), 'End date cannot be in future')
      # should also check when earliest available crossword is and make sure min date 
      # isn't before that
    )
    my_data <- get_crosswords_range(input$minDate, input$maxDate)
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