
library(tidyverse)
day_14

NNCB

lookup <- tribble(
    ~"value", ~"insert",
    "CH", "B",
    "HH", "N",
    "CB" , "H",
    "NH" , "C",
    "HB" , "C",
    "HC" , "B",
    "HN" , "C",
    "NN" , "C",
    "BH" , "H",
    "NC" , "B",
    "NB" , "B",
    "BN" , "B",
    "BB" , "N",
    "BC" , "B",
    "CC" , "N",
    "CN" , "C"
)


lookup_vec <- lookup$insert
names(lookup_vec) <- lookup$value
lookup_vec


vec <- c("N", "N", "C", "B")
res <- character()

for(i in 1:4){

    for(i in seq_along(vec)){
        lookup_value <- na.omit(lookup_vec[paste0(vec[i], vec[i+1])])

        interim_res <- paste0(vec[i], lookup_value)

        res <- paste0(res, interim_res)
        res
    }
    vec <- unlist(strsplit(res, ""))
    res <- character()
}

identical("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB", out)
vec


polymer_1 <- function(start_char, start_res = character()){
    vec <- unlist(strsplit(start_char, ""))
    res <- start_res

    for(i in seq_along(vec)){
        lookup_value <- na.omit(lookup_vec[paste0(vec[i], vec[i+1])])

        interim_res <- paste0(vec[i], lookup_value)

        res <- paste0(res, interim_res)
        res
    }
 res
}

out <- reduce(1:10, ~polymer_1(.x), .init = "NNCB")
nchar(out)
post_process <- function(input){
tibble(character = unlist(strsplit(input, ""))) %>% count(character) %>% summarise(answer = max(n) - min(n))
}


lookup <- read_delim(file = "~/Documents/personal_github_projects/aoc_2021/data/day_14.txt", skip = 2, col_names = c("value", "insert"), delim = " -> ", trim_ws = TRUE)


lookup_vec <- df$insert
names(lookup_vec) <- df$value

vec <- readLines("~/Documents/personal_github_projects/aoc_2021/data/day_14.txt", 1)

tictoc::tic()
reduce(1:10, ~polymer_1(.x), .init = vec) %>% post_process()
tictoc::toc()

tictoc::tic()
reduce(1:20, ~polymer_1(.x), .init = vec) %>% post_process()
tictoc::toc()

nrow(lookup)


reduce(1:40, ~(.x + .x -1), .init = nchar(vec))
# 21 trillion characters
# hmm


lookup

unlist(strsplit(vec, split = ""))

"NNCB" %>%
    strsplit(split = NULL) %>%
    unlist() %>%
    slider::slide_chr(paste0, collapse = "", .before = 0, .after = 1, .step = 1, .complete = FALSE) %>%
    table() %>%
    as_tibble() %>%
    rename(x = ".") %>%
    filter(str_count(x) == 2) %>%
    separate(x, into = c("x1", "x2"), sep = 1)

# vec <- "NNCB"
start <- vec
res <- character()
y <- unlist(strsplit(start, ""))
for(x in 1:(nchar(start)-1)){

    result <- paste0(y[x], y[x+1])
    res <- c(res, result)
    res
}
res


# keep track of counts


next_step <-
    tibble(x = res) %>%
    separate(x, into = c("x1", "x2"), sep = 1) %>%
    count(x1, x2)



lk <- lookup %>%
    separate(value, into = c("x1", "x2"), sep = 1)
for(i in 1:40){

temp <-
    next_step %>%
    left_join(lk)

left <- temp %>%
    select(x1, x2 = insert, n)

right <- temp %>%
    select(x1 = insert, x2, n)

next_step <- bind_rows(left, right) %>%
    count(x1, x2, wt = n) %>%
    ungroup()
}

l <- unlist(str_split(vec, ""))
l <- l[length(l)]

answer <-
next_step %>%
    count(x1, wt = n) %>%
    mutate(final_n = if_else(x1 == l, n + 1, n)) %>%
    summarise(answer = max(final_n) - min(final_n)) %>%
    pull(answer) %>%
    as.character()
answer



# actually nice ------

lookup_table <- read_delim(file = "~/Documents/personal_github_projects/aoc_2021/data/day_14.txt",
                           skip = 2,
                           col_names = c("value", "insert"),
                           delim = " -> ",
                           trim_ws = TRUE) %>%
    separate(value,
            into = c("x1", "x2"),
            sep = 1)

start <- readLines("~/Documents/personal_github_projects/aoc_2021/data/day_14.txt", 1)


make_pairs <- function(input){
res <- character()
y <- unlist(strsplit(input, ""))
for(x in 1:(nchar(input)-1)){

    result <- paste0(y[x], y[x+1])
    res <- c(res, result)
    res
}
res
}

start_table <- tibble(pairs = make_pairs(start)) %>%
    separate(pairs, into = c("x1", "x2"), 1) %>%
    mutate(n = 1)


polymer_growth <- function(input, lookup){

    temp <- left_join(input, lookup)

    keep_1 <-
        temp %>%
        select(x1 = insert, x2, n)

    keep_2 <-
        temp %>%
        select(x1, x2 = insert, n)

    output <-
        bind_rows(keep_1, keep_2) %>%
        count(x1, x2, wt = n) %>%
        ungroup()

    output
}

last_char <- unlist(strsplit(start, ""))
last_char <- last_char[length(last_char)]

counts <- reduce(1:40, ~.x %>% polymer_growth(lookup = lk), .init = start_table) %>%
    count(x1, wt = n) %>%
    mutate(n = if_else(x1 == last_char, n+1, n))

counts %>% summarise(answer = max(n) - min(n))

