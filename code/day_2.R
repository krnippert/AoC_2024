rm(list = ls())
library(tidyverse)

## part 1
reports <- readLines("data/day2_input")
levels <- lapply(str_split(string = reports, pattern = " "), as.numeric)

isSafe <- function(lvls) {
  diffs <- diff(lvls)
  return(
    length(unique(sign(diffs))) == 1 &
      all(abs(diffs) >= 1 & abs(diffs) <= 3)
  )
}

sum(sapply(levels, isSafe))
 

## part 2


sum(sapply(levels, function(lvl){
  if(isSafe(lvl)) return(TRUE)
  for (i in seq(length(lvl))) {
    if (isSafe(lvl[-i])) return (TRUE)
  }
  return(FALSE)
}))
