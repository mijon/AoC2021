library(tidyverse)


# ---- part 1 ----

# Given a fish of a certain age, and a period over which to watch it.
# How many fish will there be at the end including this one
count_fish <- function(days_left, age, first_cycle = 8, regular_cycle = 6) {
  if (days_left < age) {
    1
  } else {
    1 + count_decendents(days_left, age, first_cycle, regular_cycle)
  }
}


count_decendents <- function(days_left, age, first_cycle, regular_cycle) {
  # Betty is a fish.
  # The number of Jim's decedents is going to be the number of her children
  # plus the number of their children.
  # This will be equal to calling count_fish on each direct child
  days_left_at_next_cycle <- days_left - age
  n_direct_children <- ceiling(days_left_at_next_cycle / regular_cycle)
  childrens_birthdays <- days_left_at_next_cycle - (((seq_len(n_direct_children)-1) * (regular_cycle+1)) + 1)
  childrens_birthdays <- keep(childrens_birthdays, \(x) x>=0)
  map_dbl(childrens_birthdays, count_fish,
          first_cycle, first_cycle, regular_cycle) %>%
    reduce(`+`, .init = 0)
  
}

read_day6 <- function(path) {
  path %>%
    read_file() %>%
    str_split(",", simplify = TRUE) %>%
    as.numeric() %>%
    tibble(ages = .) %>%
    group_by(ages) %>%
    count()
}

part1 <- function(path, days_left = 80, first_cycle = 8, regular_cycle = 6) {
  read_day6(path) %>%
    mutate(fish_for_one = pmap_dbl(list(age = ages),
                                   count_fish, 
                                   days_left, first_cycle, regular_cycle)) %>%
    mutate(total_fish = n * fish_for_one) %>%
    pull(total_fish) %>%
    sum()
}

part1("data/6_1.txt", 80)




