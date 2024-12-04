library(tidyverse)
library(stringr)
### Part 1 

# load in data
list <- read_csv("data/list_day1.csv")

# order smallest to largest
list_order <- list %>% 
  mutate(across(everything(), sort))

# pull into two separate lists
list_1 <- sort(list_order$list_1)
list_2 <- sort(list_order$list_2)

# find and sum differences
diff <- abs(list_1 - list_2)

sum(diff)

### Part 2

sim_score <- 0

for(i in 1:length(list_1)){
  count<-sum(list_2==list_1[i])
  sim_score<-sim_score+count*list_1[i]
}

sim_score
