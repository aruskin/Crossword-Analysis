library(shiny)
library(shinyjs)
library(stringr)

source('get_latimes_crossword.R')

ui <- fluidPage(
  shinyjs::useShinyjs(),
  sidebarPanel(
    dateRangeInput('dateRange', 'Select date range:'),
    actionButton('getData', 'Get Crossword Data!'),
    br(),
    verbatimTextOutput('messageCenter'),
    downloadButton('downloadData', 'Download CSV')
  ),
  mainPanel(
    fluidRow(
      column(3, 
             h4('Top n most frequent answers:'),
             numericInput('topN', 'Choose n:', 10, min=1, max=25),
             tableOutput('topNTable')),
      column(4,
             h4('Search answers or clues:'),
             selectInput('searchType', label=NULL, 
                         choices=c('Answers', 'Clues'),
                         selected='Answers'),
             textInput('searchQuery', label=NULL),
             actionButton('searchButton', label='Search'),
             tableOutput('searchResultsTable'))
    )
  )
)

server <- function(input, output, session){
  shinyjs::disable('downloadData')
  shinyjs::disable('searchButton')
  
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
    shinyjs::enable('downloadData')
    shinyjs::enable('searchButton')
    x
  })
  
  search_query <- eventReactive(input$searchButton, {
    validate(
      need(input$searchQuery != "", 'Please enter a search term')
    )
    x <- data_reactive()
    if(input$searchType == 'Answers')
      search_answers(x$clean_data, input$searchQuery)
    else
      search_clues(x$clean_data, input$searchQuery)
  })
  
  output$messageCenter<- renderText({
    x <- data_reactive()
    min_date <- min(x$clean_data$date)
    max_date <- max(x$clean_data$date)
    n_cwds <- x$clean_data$date %>% unique %>% length
    
    message <- paste(data_reactive()$message,
                     '\n',
                     paste0('Pulled ', n_cwds, ' puzzles from ', min_date, ' to ', max_date))
    str_wrap(message, width = 25)
    
  })
  
  output$topNTable <- renderTable({
    validate(
      need(!is.na(input$topN), 'Please select n'),
      need(input$topN >= 1, 'Please select positive nonzero n'),
      need(input$topN <= 25, 'Will only display up to top 25')
    )
    x <- data_reactive()
    n_most_frequent_answers(x$clean_data, input$topN)
  })
  
  output$searchResultsTable <- renderTable({
    table_out <- search_query()
    table_out <- mutate(table_out, date = as.character(date))
    table_out
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste0('lax_cwd_', input$dateRange[1], '_', input$dateRange[2], '.csv')
    },
    content = function(file){
      x <- data_reactive()
      write.csv(x$clean_data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)