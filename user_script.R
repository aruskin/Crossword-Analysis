source("get_latimes_crossword.R")
##################################################
# may want to eventually add error handling here (i.e. make sure user
# actually entered date, make sure that max_date later than min_date)
min_date <- readline(prompt="Enter beginning date (yyyy-mm-dd):")
max_date <- readline(prompt="Enter end date (yyyy-mm-dd):")

# this will probably take a while
system.time(my_data <- get_crosswords_range(min_date, max_date))

# should expect these to be in the 70s Monday through Saturday (although
# Saturday might be a bit less since they tend to have more long words)
# and in the 140s on Sundays
check_counts <- my_data %>% group_by(date) %>% summarise(count=n())

x <- clean_crossword_dataset(my_data)

# What went wrong in processing?
print(x$message)
x$bad_data

n_cwds <- x$clean_data$date %>% unique %>% length
print(paste0("Pulled ", n_cwds, " puzzles from ", min_date, " to ", max_date))

write.csv(x$clean_data, paste0('lax_cwd_', min_date, '_', max_date, '.csv'))
