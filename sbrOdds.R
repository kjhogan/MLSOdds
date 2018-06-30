
get_DateOdds <- function(date) {
  
  library(reshape2)
  library(rvest)
  library(tidyverse)
  library(stringr)
  date <- as.Date(date, "%m/%d/%Y")
  totals_url <- paste0("https://www.sportsbookreview.com/betting-odds/soccer/totals/?leagueId=mls&date=" ,format(date, "%Y%m%d"))
  message(totals_url)

  dfspreads <- data.frame()
  dftotals <- data.frame()
  Sys.sleep(2)
  totals_page <- read_html(totals_url) %>% html_nodes("#oddsGridContainer")

  
  if (length(totals_page) > 0){
    
    teams <- totals_page %>% html_nodes('.team-name') %>% html_text()
    totals <- totals_page %>% html_nodes('.eventLine-book-value') %>% html_text()
    
    #booksformatting
    books <- read_html(totals_url) %>% html_nodes("#bookName") %>% html_text()
    books_col <- rep(books, each = 2)
    books_col[c(TRUE, FALSE)] <- paste0(books_col[c(TRUE, FALSE)], '_over')
    books_col[c(FALSE, TRUE)] <- paste0(books_col[c(FALSE, TRUE)], '_under')
    
    #extract game times
    game_times_index <- seq(0, length(totals), by = 24)
    game_times_index[1] <- 1
    game_times <- totals[game_times_index]
    totals <- totals[-game_times_index]
    
    #instantiate blank df
    totals_df <- data.frame()
    df_col_names <- c('game_date','team', 'opponent', 'site', 'game_time', 'opener_over','opener_under', books_col[1:20])

    games_no <- length(teams)/2
    site_subset <- c('home', 'away')
    
    for(i in 1:2) {
      totals_split <- split(totals, ceiling(seq_along(totals)/2))
      date_subset <- c(date, date)
      if(i == 1){
      
        team_subset <- teams[1:2]
        game_time_subset <- c(game_times[1], game_times[1])
        
        totals_subset <- totals_split[1:(length(totals_split)/2)]
        totals_subset_df <- data.frame(matrix(NA, nrow = 2, ncol =22))
        totals_subset_df[1,] <- totals_subset %>% unlist()
        totals_subset_df[2,] <- totals_subset %>% unlist()
        
        totals_df <- data.frame(game_date = date_subset, team = team_subset, opponent = rev(team_subset), site = site_subset, game_time = game_time_subset)
        totals_df <- cbind(totals_df, totals_subset_df)
        names(totals_df) <- df_col_names
      }
      
      else {
        
        team_subset <- teams[(2*i-1):(2*i)]
        game_time_subset <- c(game_times[i], game_times[i])

        totals_subset <- totals_split[(i*11-10):(i*11)]

        totals_subset_df <- data.frame(matrix(NA, nrow = 2, ncol =22))
        totals_subset_df[1,] <- totals_subset %>% unlist()
        totals_subset_df[2,] <- totals_subset %>% unlist()
       
        totals_df_temp <- data.frame(game_date = date_subset, team = team_subset, opponent = rev(team_subset), site = site_subset, game_time = game_time_subset)
        
        totals_df_temp <- cbind(totals_df_temp, totals_subset_df)
        names(totals_df_temp) <- df_col_names
        
        totals_df <- rbind(totals_df, totals_df_temp)
      }
      
    }
    return(totals_df)
  }
  
  return(-1)
}

get_Odds_Between <- function(startdate, enddate) {
  gamedates <- seq.Date(as.Date(startdate, "%m/%d/%Y"), as.Date(enddate, "%m/%d/%Y"), "day")
  all_data <- lapply(gamedates, get_DateOdds)
  non_empty_day <- all_data %>% map_lgl(is.data.frame)
  all_data <- all_data[non_empty_day]
  all_data <- bind_rows(all_data)
  return(all_data)
}


update_Odds_DF <- function(odds_df) {
  today <- "06/05/2018"
  most_recent<- "06/04/2018"
  update_df <- get_Odds_Between(most_recent,today)
  update_df$season <- "2018"
  update_df$type <- "first_half"
  odds_df <- rbind(odds_df, update_df)
  
  return(odds_df)
}