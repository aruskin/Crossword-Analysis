Code to facilitate analysis of trends in LA Times crossword puzzle

Using LAXCrossword.com blog as source for puzzle clues and answers, since the entries there tend to be fairly standardized. However, there are some exceptions that the code isn't currently accounting for. Not sure if there's a better source for this data out there...

- get_latimes_crossword.R: set of helper functions for scraping and analyzing crossword data
- user_script.R: basic example for using functions in interactive R session to create dataset
- app.R: so far super basic Shiny app--user selects date range and sees most used answers in that period. Functionality to add:
  - Searching over clues and answers
  - Visualization of word usage over time
  - Get crossword data from two different date ranges and compare usage of certain answers
  - Prettier UI
