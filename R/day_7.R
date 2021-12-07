library(tidyverse)

read_day7 <- function(path) {
  scan(path,
       sep = ",")
}

# ---- Part 1 ---- The is exactly the same problem setting as trying to decide
# what city to meet in when you have friends in many cities (all in a line) and
# you want to minimise *total* travel time. It's the median position!

part1 <- function(path) {
  crabs <- read_day7(path)
  m <- median(crabs)
  
  map(crabs, \(x) {abs(x - m)}) %>% # I'm not sold on the new lambdas, but let's have a go
    reduce(`+`)
}

part1("data/7_1.txt") # 355764

# ---- part 2 ----

part2 <- function(path) {
  crabs <- read_day7(path)
  
  # because we're dealing with the mean, it's likely that we'll be getting a
  # non-integer. Since the crabs have to go in integer steps, we need to try
  # both and take the minimum distance. Unlike the previous result, I didn't
  # know this one already, (one probably could have figured it out by noticing
  # that the fuel consumption is quadratic in the distance and noticed that
  # something that minimises squared distance is the mean), but I figured it out
  # using differention and some paper (not included).
  ms <- c(floor(mean(crabs)), ceiling(mean(crabs)))
  
  check_fuel <- function(m) {
    map(crabs, \(x) {abs(x - m)}) %>%
      map(\(n) {n*(n+1)/2}) %>% # triangle numbers
      reduce(`+`)
  }
  
  min(map_dbl(ms, check_fuel))
}

part2("data/7_1.txt") # 99634572
