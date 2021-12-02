library(tidyverse)

sample_data <- c("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")


# --- part 1 ----

parse_forwards <- function(fwd) {
  parse_number(fwd)
}

parse_depths <- function(depths) {
  depths %>%
    str_replace("down ", "+") %>% 
    str_replace("up ", "-") %>% 
    parse_number()
}

extract_forwards <- function(instructions) {
  keep(instructions, ~str_detect(.x, "forward")) %>% 
    map(parse_forwards)
}

extract_depths <- function(instructions) {
  keep(instructions, ~str_detect(.x, "up|down")) %>% 
    map(parse_depths)
}

part1 <- function(instructions) {
  forwards <- instructions %>% 
    extract_forwards() %>% 
    reduce(sum)
  
  depths <- instructions %>% 
    extract_depths() %>% 
    reduce(sum)
  
  forwards * depths
}


answer1 <- readLines("data/2_1.txt") %>% 
  part1() # 1698735

# --- part 2 ----

sub <- function(horizontal = 0, depth = 0, aim = 0) {
  list(
    horizontal = horizontal,
    depth = depth,
    aim = aim
  )
}

move_sub <- function(sub, instruction) {
  if (str_detect(instruction, "forward")) {
    move_forwards(sub, instruction)
  } else {
    move_depths(sub, instruction)
  }
}

move_depths <- function(sub, instruction) {
  sub(
    horizontal = sub$horizontal,
    depth = sub$depth,
    aim = sub$aim + parse_depths(instruction)
  )
}

move_forwards <- function(sub, instruction) {
  value <- parse_forwards(instruction)
  sub(
    horizontal = sub$horizontal + value,
    depth = sub$depth + sub$aim * value,
    aim = sub$aim
  )
}

part2 <- function(instructions) {
  output <- reduce(instructions, .f = move_sub, .init = sub(0,0,0))
  output$horizontal * output$depth
}

answer2 <- part2(readLines("data/2_1.txt")) # 1594785890
