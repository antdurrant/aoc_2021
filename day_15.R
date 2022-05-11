test_input <-
"1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"
test <- na.omit(as.numeric(unlist(strsplit(test_input, ""))))

mat <- matrix(test, ncol = 10)

library(tidyverse)
library(tidygraph)

graph <-
  reshape2::melt(mat) %>%
  rename(x = Var1,
         y = Var2) %>%
  rowwise() %>%
  mutate( from = list(c(x, y)),
          to = list(list(c(x-1, y), c(x+1, y), c(x, y-1), c(x, y+1)))) %>%
  unnest(to) %>%
  select(node = value, from, to) %>%
  as_tbl_graph()


strtoi(c("D2FE28"), base = 2)
?strtoi
