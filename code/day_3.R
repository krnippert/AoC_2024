rm(list = ls())
library(tidyverse)

## Part 1

input <- readLines("data/input_day3") |>
  paste(collapse = "")|>
  unlist()|>
  as.character()

nchar <- nchar(input)

substrings <- str_extract_all(input, regex("([m][u][l][(]\\d{1,3},\\d{1,3}[)])"))|>
  unlist()

sapply(substrings, \(x)
       str_extract_all(x, regex("\\d{1,3}")) |>
         unlist() |>
         as.numeric() |>
         prod()) |>
  unlist() |>
  sum()

## oh my gosh finally got it to work, have to unlist so many times
## thank you bmgaldo for actually having code that worked - I think it was a mix
## of the pattern input and unlisting so many freaking times
## I just a baby

## Part 2

muls <- gregexpr(pattern = "([m][u][l][(]\\d{1,3},\\d{1,3}[)])", text = input)|>
  unlist()

dos <- gregexpr(pattern = "([d][o][(][)])", text = input) |>
  unlist()

donts <- gregexpr(pattern = "([d][o][n]['][t])", text = input)|>
  unlist()

n_muls <- length(muls)

add_muls <- numeric(n_muls)

# ordering do and don't
do_df = rbind(data.frame(idx=dos,do=1),data.frame(idx=donts,do=0))
do_df = do_df[order(do_df$idx),]
do_df = left_join(data.frame(idx = 1:max(muls)), do_df)

do_df$do[1] <- 1
do_df = do_df |>
  fill(do, .direction = "down")

mul_df <- data.frame(idx = muls,
                     which_mul = 1:length(muls))|>
  left_join(do_df) |>
  filter(do==TRUE)

substrings <- str_extract_all(input, 
                              regex("([m][u][l][(]\\d{1,3},\\d{1,3}[)])"))|>
  unlist()

sapply(substrings[mul_df$which_mul], \(x)
       str_extract_all(x, regex("\\d{1,3}")) %>% 
         unlist() %>% 
         as.numeric() %>% 
         prod()) %>% 
  unlist() %>% 
  sum()

## Praise bmgaldo you're my hero <3

