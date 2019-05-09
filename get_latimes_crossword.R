library(httr)
library(dplyr)
library(lubridate)
library(ggplot2)

##### some functions for scraping & using LA Times crossword data

# earliest crossword on laxcrossword.com is from September 18, 2012
MIN_AVAILABLE_DATE <- as.Date('2012-09-18')

get_crossword <- function(yyyy, mm, dd){
  month_num <- as.numeric(mm)
  day_num <- as.numeric(dd)
  month_abb <- month.abb[month_num]
  yy <- substr(yyyy, 3, 4)
  day_of_week <- weekdays.Date(as.Date(paste0(yyyy, '-', mm, '-', dd)))
  
  if(yyyy < 2017){
    site <- GET(paste0('https://laxcrossword.com/', yyyy, '/', mm, '/',
                       'la-times-crossword-answers-', day_num, 
                       '-', month_abb, '-', yy, '.html'))
  }else{
    site <- GET(paste0('https://laxcrossword.com/', yyyy, '/', mm, '/',
                       'la-times-crossword-', day_num, 
                       '-', month_abb, '-', yy, '-', day_of_week, '.html'))
  }
  if(site$status == 404)
    site <- GET(paste0('https://laxcrossword.com/', yyyy, '/', mm, '/', dd))
  
  headers <- c('<h2 id="all-clues">Complete List of Clues/Answers</h2>', 
               '<i><b>Across</b><br />',
               '<h2 id="all-clues">Complete List of Clues and Answers</h2>') %>%
    paste0('(', ., ')') %>%
    paste0(., collapse='|')
  
  footers <- c('</p></div>', '</i></p>', '</div>',
               '<p id=\"post_bottom\" class=\"return_to_top\"><a href=\"#top\">Return to top of page</a></p>') %>%
    paste0('(', ., ')') %>%
    paste0(., collapse='|')
  
  gross_html <- c('<p>', '<p class=\"no_bottom_margin\"><em>', '	</em>',
                  '</p>', '<br />') %>%
    paste0('(', ., ')') %>%
    paste0(., collapse='|')
    
  txt <- content(site, "text") %>%
    strsplit(headers) %>% unlist %>% .[2] %>%
    strsplit(footers)  %>% unlist %>% .[1] %>%
    strsplit("\n") %>% unlist %>%
    gsub(pattern = gross_html, replacement = "", .) %>% 
    .[!(. %in% c("", "<h3>Down</h3>", "<h3>Across</h3>", "<b>Down</b>", "Down</b>"))] %>%
    gsub(pattern = "[0-9]+[.][ ]*", replacement = "", .)
  
  tryCatch({
    # analogy clues are going to fuck things up for us
    analogies <- grep('.+ : .+ :: .+ :', txt)
    analogy_handler <- function(analogy){
      analogy <- analogy %>% strsplit(':') %>% unlist
      paste0(analogy[1], 'is to', analogy[2], 'as',
             analogy[4], 'is to', analogy[5], ' : ', 
             paste0(analogy[6:length(analogy)], collapse = " "))
    }
    
    txt[analogies] <- analogy_handler(txt[analogies])
    
    txt <- gsub(pattern = "  (&#8220;|&#8221;|&#8217;)", replacement = " ",
                x = txt)
    z <- lapply(txt, function(x) strsplit(x, "( +: +)|(  )|( &nbsp;)") %>% unlist) %>%
      do.call("rbind", .) %>%
      as.data.frame(stringsAsFactors=FALSE) %>%
      .[,1:2]
    
    colnames(z) <- c("clue", "answer")
    z <- mutate(z, date=as.Date(paste0(yyyy, '-', mm, '-', dd)))
    z
  }, error = function(e) {
    data.frame(clue=NA, answer=NA, date=as.Date(paste0(yyyy, '-', mm, '-', dd)))
  })
  
}

get_crosswords_range <- function(start_date, end_date){
  all_dates <- seq(ymd(start_date), ymd(end_date), by='1 day')
  z <- lapply(all_dates,
              function(x) {
                get_crossword(yyyy=substr(x, 1, 4),
                              mm=substr(x, 6, 7),
                              dd=substr(x, 9, 10))
              })
  bind_rows(z)
}

strip_answers <- function(answers){
  # unfortunately not all answers have right parenthesis, but hopefully none
  # of them have parenthetical statement before answer?
  gsub(pattern = '[(].*', '', answers) %>% 
    gsub(pattern = '(<b>)|(</b>)', replacement = '', x = .) %>% 
    gsub(pattern = '(&#8212;) [a-z]+.*', replacement = '', x = .) %>% 
    gsub(pattern = '[[:punct:]]+', replacement = '', x = .) %>%
    gsub(pattern = '[[:digit:]]+', replacement = '', x = .) %>%
    gsub(pattern = ' ', replacement = '', x = .)
}

clean_crossword_dataset <- function(data){
  temp_data <- mutate(data, clean_answers = strip_answers(answer))
  malformed_rows <- grep('[a-z]', temp_data$clean_answers)
  bad_dates <- temp_data[malformed_rows,]$date %>% unique
  if(length(malformed_rows) > 0){
    message <- paste0("Had issues processing crosswords from ", 
                      paste0(bad_dates, collapse = ", "),
                      ". Removing from dataset.")
  }else{
    message <- "No processing errors detected"
  }
  
  clean_data <- filter(temp_data, !(date %in% bad_dates))
  list(clean_data=clean_data, message=message, bad_data=temp_data[malformed_rows,])
}

plot_word_usage <- function(data, word){
  # require(zoo)
  usage <- mutate(data, word_used = clean_answers == word) %>%
    group_by(date) %>%
    summarise(word_used=sum(word_used))
  # not super useful unless looking at years worth of data probably
  # usage <- mutate(usage, 
  #                 word_used_ma7 = rollmean(word_used, k=7, fill=NA),
  #                 word_used_ma14 = rollmean(word_used, k=14, fill=NA),
  #                 word_used_ma28 = rollmean(word_used, k=28, fill=NA))
  # usage %>%
  #   gather(metric, value, word_used_ma7:word_used_ma28) %>%
  #   ggplot(aes(date, value, color = metric))
  ggplot(usage, aes(x=date, y=word_used)) + geom_smooth() + geom_point()
}

n_most_frequent_answers <- function(data, n){
  most_freq_answers <- data %>% 
    group_by(clean_answers) %>% 
    summarise(count=n()) %>%
    arrange(-count)
  colnames(most_freq_answers) <- c('Answer', 'Occurrences')
  most_freq_answers[1:n,]
}

search_clues <- function(data, word){
  matches <- grep(word, data$clue, ignore.case = TRUE)
  select(data[matches,], one_of(c('date', 'clue', 'answer')))
}

search_answers <- function(data, word){
  matches <- grep(word, data$clean_answers, ignore.case = TRUE)
  select(data[matches,], one_of(c('date', 'clue', 'answer')))
}