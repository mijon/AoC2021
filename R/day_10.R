library(tidyverse)

read_day10 <- function(path) {
  read_lines(path) %>%
    str_split("")
}

sample_data <- read_day10("data/10_sample.txt")
openers <- c("(", "[", "{", "<")
closers <- c("(" = ")",
             "[" = "]",
             "{" = "}",
             "<" = ">")
close_value <- c(")" = 3,
                 "]" = 57,
                 "}" = 1197,
                 ">" = 25137)
autocomplete_value <- c(")" = 1,
                        "]" = 2,
                        "}" = 3,
                        ">" = 4)


rest <- function(stack) {
  if (length(stack) <= 1) {
    c()
  } else {
    stack[2:length(stack)]
  }
}

first <- function(stack) {
  if (length(stack) >0) {
    stack[[1]]
  } else {
    NULL
  }
}

push <- function(item, stack) {
  c(item, stack)
}

pop <- function(stack) {
  rest(stack)
}

check_line <- function(stack, string) {
  if (length(string) == 0) {
    return(0)
  } 
  
  candidate <- string[[1]]
  if (candidate %in% openers) {
    check_line(push(candidate, stack), rest(string))
  } else {
    if (candidate == closers[[first(stack)]]) {
      check_line(pop(stack), rest(string))
    } else {
        return(close_value[[candidate]])
    }
  }
}

part1 <- function(path) {
  read_day10(path) %>%
    map(~check_line(c(), .x)) %>%
    reduce(`+`)
}

part1("data/10_1.txt") # 399153


# ---- part 2 ----

discard_corrupt <- function(lines) {
  corrupt_scores <- lines %>%
    map(~check_line(c(), .x))
  
  lines[corrupt_scores == 0]
}

return_stack <- function(stack, string){
  if (length(string) == 0) {
    return(stack)
  } 
  
  candidate <- string[[1]]
  if (candidate %in% openers) {
    return_stack(push(candidate, stack), rest(string))
  } else {
    return_stack(pop(stack), rest(string))
  }
}

flip_stack <- function(openers) {
  map_chr(openers, ~closers[[.x]])
}

step_autocomplete_score <- function(curr_score, next_char) {
  curr_score * 5 + autocomplete_value[[next_char]]
}

calc_autocomplete_score <- function(stack) {
  reduce(stack, step_autocomplete_score, .init = 0)
}


part2 <- function(path) {
  read_day10(path) %>%
    discard_corrupt() %>%
    map(~return_stack(c(), .x)) %>%
    map(flip_stack) %>%
    map(calc_autocomplete_score) %>%
    as.numeric() %>%
    median()
}

part2("data/10_1.txt") # 2995077699
