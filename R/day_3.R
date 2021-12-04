library(tidyverse)
library(rlang)

sample_data <- read_day3("data/3_sample.txt")

# Part 1 ----

read_day3 <- function(path) {
  tmp <- read_table(path,
             col_names = FALSE) 
  tmp_len <- str_length(tmp$X1[[1]])
  
  tmp %>%
    separate(col = X1, sep = "",
             into = paste("col",
                          seq(tmp_len, 0),
                          sep = "_")) %>%
    select(-!!paste0("col_", tmp_len)) %>%
    mutate(across(everything(), as.numeric))
}

count_occurences <- function(vec, value) {
  length(keep(vec, ~.x == value))
}

most_common <- function(bits) {
  ones  <- count_occurences(bits, 1)
  zeros <- count_occurences(bits, 0)
  
  if (zeros > ones) 0 else 1
}

least_common <- function(bits) {
  1 - most_common(bits)
}

to_decimal <- function(df) {
  df %>%
    pivot_longer(cols = everything(),
                 names_to = c("blank", "exp"),
                 values_to = "value",
                 names_sep = "_") %>%
    mutate(exp = as.numeric(exp)) %>%
    mutate(value = value * 2^exp) %>%
    pull(value) %>%
    sum()
}

get_rate <- function(df, agg_fn = most_common) {
  df %>%
    summarise(across(everything(),
                     agg_fn)) %>%
    to_decimal()
}

part1 <- function(df) {
  gamma <- get_rate(df, most_common)
  epsilon <- get_rate(df, least_common)
  gamma * epsilon
}

part1(read_day3("data/3_1.txt")) # 3242606

# Part 2 ----

bit_criteria <- function(df, col, fn) {
  colname <- sym(names(df)[col])
  
  output <- filter(df,
                   !!colname == fn(df[[col]]))
  if (nrow(output) == 1) {
    output
  } else {
    bit_criteria(output, col + 1, fn)
  }
}

oxygen_bit_criteria <- function(df) {
  bit_criteria(df, 1, most_common)
}

co2_bit_criteria <- function(df) {
  bit_criteria(df, 1, least_common) 
}

get_oxygen_rating <- function(df) {
  df %>%
    oxygen_bit_criteria() %>%
    to_decimal()
}

get_co2_rating <- function(df) {
  df %>%
    co2_bit_criteria() %>%
    to_decimal()
}

part2 <- function(df) {
  get_oxygen_rating(df) * get_co2_rating(df)
}

part2(read_day3("data/3_1.txt")) # 4856080
