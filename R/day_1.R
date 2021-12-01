library(tidyverse)
library(slider)

sample_data <- c(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

mark_movements <- function(depths) {
  sign(depths - lag(depths))
}

is_one <- function(x) {
  x == 1
}

count_ups <- function(depths) {
  mark_movements(depths) %>%
    discard(is.na) %>% 
    keep(is_one) %>% 
    sum()
}

# part 1
part_1 <- read_csv("data/day_1_part_1.csv")
count_ups(part_1$depth) # 1342

# part 2
slide_sum_n <- function(depths, n) {
  slide_dbl(.x = depths,
            .f = sum,
            .after = n - 1,
            .complete = TRUE) %>% 
    discard(is.null) 
}

count_ups_windowed <- function(depths) {
  slide_sum_n(depths, 3) %>% 
    count_ups
}

count_ups_windowed(part_1$depth)   # 1378
