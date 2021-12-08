library(tidyverse)

read_day8 <- function(path) {
  path %>%
    read_delim(delim = " | ",
               col_names = c("examples", "output"),
               show_col_types = FALSE) %>%
    mutate(across(everything(), str_split, " "))
}

# ---- part 1 ----

part1 <- function(path) {
  read_day8(path) %>%
    mutate(output_lengths = map(output, str_length)) %>%
    unnest_longer(output_lengths) %>%
    filter(output_lengths %in% c(2,3,4,7)) %>%
    nrow()
}

part1("data/8_1.txt") # 440

# ---- part 2 ----

# given a list of characters, split each element into a vector of length one
# character vectors
make_vec <- function(l) {
  l %>%
    map(~str_split(.x, "")[[1]])
}

# decodes the value for the 'a' segment
determine_a <- function(examples) {
  tmp <- keep(examples, ~str_length(.x) <=3) %>%
    make_vec()
  
  output <- c("a")
  names(output) <- setdiff(reduce(tmp, union), reduce(tmp, intersect))
  output
}

# find what could be in the 'c' or 'f' segment
determine_cf_potentials <- function(examples) {
  tmp <- keep(examples, ~str_length(.x) <=3) %>%
    make_vec()
  
  reduce(tmp, intersect)
}

determine_bd_potentials <- function(examples) {
  tmp <- keep(examples, ~str_length(.x) == 4 || str_length(.x) == 2) %>%
    make_vec()
  
  setdiff(reduce(tmp, union), reduce(tmp, intersect))
}

determine_eg <- function(examples) {
  digits_5 <- keep(examples, ~str_length(.x) == 5) %>% make_vec()
  digits_234 <- keep(examples, ~str_length(.x) %in% c(2,3,4)) %>% make_vec() %>% reduce(union)
  
  # This determines the candidates for positions 'g' and 'e'. All elements of
  # this 3-list will have a common value (the 'g') while one element will have 2
  # values, the other being 'e'
  eg_candidates <- digits_5 %>% map(~setdiff(.x, digits_234))
  
  g <- eg_candidates %>% keep(~length(.x) == 1) %>% unique() %>% as.character()
  e <- eg_candidates %>% keep(~length(.x) == 2) %>% pluck(1) %>% setdiff(g)
  # browser()
  
  output <- c("e", "g")
  names(output) <- c(e, g)
  output
  
}


determine_cf <- function(examples) {
  cf_potentials <- determine_cf_potentials(examples)
  digits_6 <- keep(examples, ~str_length(.x) == 6) %>% make_vec() %>%
    map(intersect, cf_potentials)
  
  f <- digits_6 %>% keep(~length(.x) == 1) %>% unique() %>% pluck(1) %>% as.character()
  c <- digits_6 %>% keep(~length(.x) == 2) %>% pluck(1) %>% setdiff(f)
  
  # browser()
  output <- c("f", "c")
  names(output) <- c(f, c)
  output
}

determine_bd <- function(examples) {
  bd_potentials <- determine_bd_potentials(examples)
  digits_6 <- keep(examples, ~str_length(.x) == 6) %>% make_vec() %>%
    map(intersect, bd_potentials)
  
  b <- digits_6 %>% keep(~length(.x) == 1) %>% unique() %>% pluck(1) %>% as.character()
  d <- digits_6 %>% keep(~length(.x) == 2) %>% pluck(1) %>% setdiff(b)
  
  output <- c("b", "d")
  names(output) <- c(b, d)
  output
}


# deduce the wiring on the display
find_wiring <- function(examples) {
  c(determine_a(examples),
    determine_bd(examples),
    determine_cf(examples),
    determine_eg(examples))
}

# take a string, sort the letters and return a string
sort_in_string <- function(s){
  s %>%
    str_split("", simplify = TRUE) %>%
    str_sort() %>%
    str_c(collapse = "")
}

# translate a segment activation code into a number (based on correct wiring)
segment_to_number <- function(segment) {
    case_when(
    segment == "abcefg" ~ 0,
    segment == "cf" ~ 1,
    segment == "acdeg" ~ 2,
    segment == "acdfg" ~ 3,
    segment == "bcdf" ~ 4,
    segment == "abdfg" ~ 5,
    segment == "abdefg" ~ 6,
    segment == "acf" ~ 7,
    segment == "abcdefg" ~ 8,
    segment == "abcdfg" ~ 9
  )
}

# use a wiring table, translate an input into what it should be
# Wiring tables are named lists 
replace_using_table <- function(s, table) {
  table[[s]]
}

# Takes a single string showing what wires are hot and a wiring table
# and returns the number that represents
decode_output <- function(output, wiring_table) {
  str_split(output, "") %>%
    pluck(1) %>%
    map(replace_using_table, wiring_table) %>%
    str_sort() %>%
    str_c(collapse = "") %>%
    segment_to_number()
}


# Go from the 4-list of scrambled hot wires to the 4 digit numeric value
decode_outputs <- function(examples, outputs) {
  wiring_table <- find_wiring(examples)
  
  outputs %>%
    map(decode_output, wiring_table) %>%
    reduce(paste0, collapse = "") %>%
    as.numeric()
}


part2 <- function(path) {
  path %>%
    read_day8() %>%
    mutate(result = map2_dbl(examples, output, decode_outputs)) %>%
    pull(result) %>%
    sum()
}

part2("data/8_1.txt") # 1046281
