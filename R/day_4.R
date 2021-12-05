library(tidyverse)
library(rlang)

# This one got a little out of hand....

# Board class ----
board <- function(rows) {
  board <- tibble(test = rows) %>%
    mutate(row_num = 1:n(),
           test = map(test, prep_board)) %>% 
    unnest(test) %>%
    mutate(called = FALSE)
  
  class(board) <- c("board", class(board))
  board
}

print.board <- function(b) {
  b %>%
    mutate(value = if_else(called, paste0("<", value, ">"), as.character(value))) %>%
    select(-called) %>%
    pivot_wider(names_from = col_num) %>%
    print()
}

call_num <- function(b, n) {
  UseMethod("call_num")
}

call_num.board <- function(b, n) {
  b %>%
    mutate(called = if_else(value == n, TRUE, called))
}

prep_board <- function(x) {
  values = str_split(x, " +")[[1]]
  tibble(value = values, col_num = 1:n())
}

# Check functions look at whether there are any rows or columns that are all called
check_board <- function(b) {
  check_dim(b, col_num) || check_dim(b, row_num)
}

check_dim <- function(b, dim) {
  b %>% group_by({{dim}}) %>%
    summarise(check = and_vec(called)) %>%
    pull(check) %>%
    any()
}

and_vec <- function(v) {
  reduce(v, `&`)
}

# Reading in functions ----

read_day4 <- function(path) {
  text <- read_lines(path)

  list(draws = parse_draws(text),
       boards = parse_boards(text))
}

parse_draws <- function(text) {
  text[[1]] %>%
    str_split(",") %>%
    pluck(1)
}

parse_boards <- function(text) {
  text[3:length(text)] %>%
    as.list() %>%
    split_boards(list())      
}

split_boards <- function(rest, boards) {
  if (length(rest) > 2) {
    boards <- append(boards, list(board(take(rest, 5))))
    split_boards(drop(rest, 6),
                 boards)
  } else {
    boards
  }
}

# --- Playing Bingo
play <- function(bs, ns) {
  
  if (length(ns) == 0) {
    return("no solutions")
  }
  
  n <- take(ns, 1)
  
  bs <- map(bs, call_num, n)
  
  checked_boards <- keep(bs, check_board)
  
  if (length(checked_boards) > 0) {
    return(list(b = checked_boards[[1]],
                n = n))
  } else {
    play(bs, drop(ns, 1))
  }
}


# --- Utils ----
take <- function(l, n) {
  l[1:n]
}

drop <- function(l, n) {
  if (length(l) > n) {
    l[n+1:(length(l)-n)]
  } else {
    list()
  }
}

# --- Calculating Score

calc_score <- function(winner) {
  board_score <- winner$b %>%
    filter(called == FALSE) %>%
    pull(value) %>%
    as.numeric() %>%
    sum()
  
  board_score * as.numeric(winner$n)
}

part1 <- function(path) {
  inputs <- read_day4(path)
  
  winner <- play(inputs$boards, inputs$draws)
  
  calc_score(winner)
}

part1("data/4_1.txt") #%>% calc_score() # 27027

# ---- part 2 ----

play_all <- function(bs, ns, step = 0) {
  
  if (length(ns) == 0) {
    return()
  }
  
  n <- take(ns, 1)
  bs <- map(bs, call_num, n)
  
  checked_boards <- keep(bs, check_board)
  
  if (length(checked_boards) > 0) {
    return(list(b = checked_boards[[1]],
                n = n))
  } else {
    play(bs, drop(ns, 1))
  }
}

play2 <- function(bs, ns, most_recent_winner) {
  print(paste0("n left: ", length(ns), "| boards remaining: ", length(bs), sep = ""))
  n <- take(ns, 1)
  bs <- map(bs, call_num, n)
  
  new_winner <- keep(bs, check_board)
  if (length(new_winner) > 0) {
    most_recent_winner <- new_winner
  }
  
  still_playing <- discard(bs, check_board)
  
  if (length(still_playing) == 0) {
    return(list(b = most_recent_winner[[1]],
                n = take(ns, 1)))
  }
  
  play2(still_playing, drop(ns, 1), most_recent_winner)
}



part2 <- function(path) {
  inputs <- read_day4(path)
  
  last_winner <- play2(inputs$boards, inputs$draws, list())
  last_winner %>% calc_score()
}

part2("data/4_1.txt") # 36975

