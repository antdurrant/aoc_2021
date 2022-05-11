library(tidyverse)
test_ <-
"00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"
test_data <- read_tsv(test_, col_names = "data")


convert_data <- function(path){
  data <-
  read_tsv(path, col_names = "data")

  data %>%
    separate(col = data,
             into = c(paste0("col_", 0:max(nchar(data$data)))),
             sep = "",
             convert = TRUE)  %>%
    select(-col_0)
}
convert_data(test_)


filter_by <- function(x, keep =c(0,1)){
  other <- as.numeric(!as.logical(keep))
  round_me <- mean(x)
  if(length(x) == 1){
    keep = c(0,1)
  } else if(length(x) == 2) {
    keep
  } else{

  if(round_me > .5) {
     keep
  } else if(round_me < .5 ){
     other
  } else if(round_me == .5){
    keep
  }
  }
}


filter_func <- function(data, col, keep){
  data %>%
    filter({{col}} %in% filter_by({{col}}, keep = keep))
}


testing <- convert_data(test_)

filter_by(x = testing$col_1, keep = 0)

testing %>%
  filter(col_1 == filter_by(col_1, keep = 1)) %>%
  filter(col_2 == filter_by(col_2, keep = 1)) %>%
  filter(col_3 == filter_by(col_3, keep = 1)) %>%
  filter(col_4 == filter_by(col_4, keep = 1)) %>%
  filter(col_5 == filter_by(col_5, keep = 1)) %>%
  paste0(collapse = "") %>% strtoi(base = 2)

testing %>%
  filter(col_1 == filter_by(col_1, keep = 0)) %>%
  filter(col_2 == filter_by(col_2, keep = 0)) %>%
  filter(col_3 == filter_by(col_3, keep = 0)) %>%
  filter(col_4 == filter_by(col_4, keep = 0)) %>%
  filter(col_5 == filter_by(col_5, keep = 0)) %>%
  paste0(collapse = "") %>% strtoi(base = 2)


reduce(c(paste0("col_", 1:5)),
      .x %>%
        ~filter_func(col = .y, keep = 1),
        .init = testing)


testing %>%
  filter_func(col = sym("col_1"), keep = 1)




(testing %>%
  accumulate(
    colnames(testing),
    ~.x %>%
      filter_func(col = enquo(.y), keep = 1),
    .init = .
  )
)


df <- convert_data("./data/day3.txt")

a <- function(data, kp){
data %>%
  filter_func(col = col_1, keep = kp) %>%
  filter_func(col = col_2, keep = kp) %>%
  filter_func(col = col_3, keep = kp) %>%
  filter_func(col = col_4, keep = kp) %>%
  filter_func(col = col_5, keep = kp) %>%
  filter_func(col = col_6, keep = kp) %>%
  filter_func(col = col_7, keep = kp) %>%
  filter_func(col = col_8, keep = kp) %>%
  filter_func(col = col_9, keep = kp) %>%
  filter_func(col = col_10, keep = kp) %>%
  filter_func(col = col_11, keep = kp) %>%
  filter_func(col = col_12, keep = kp)
}

gen <- a(data = df, kp = 1) %>%
  paste0(collapse = "") %>%
  strtoi(base  = 2)

o2 <- a(data = df, kp = 0) %>%
  paste0(collapse = "") %>%
  strtoi(base  = 2)


oc <- "0010110001" %>% strtoi(base = 2)

gen *oc
