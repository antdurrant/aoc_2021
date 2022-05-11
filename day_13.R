test_data <-
c("6,10",
  "0,14",
  "9,10",
  "0,3",
  "10,4",
  "4,11",
  "6,0",
  "6,12",
  "4,1",
  "0,13",
  "10,12",
  "3,4",
  "3,0",
  "8,4",
  "1,10",
  "2,14",
  "8,10",
  "9,0")

test_x <- as.numeric(stringi::stri_extract_first_regex(test_data, pattern = "\\d+")) +1
test_y <- as.numeric(stringi::stri_extract_last_regex(test_data, pattern = "\\d+")) +1

test_x
test_y


paper <- matrix(data = NA, nrow = max(test_y), ncol = max(test_x))
paper
populate <- function(x,y, mat){
  mat[y,x] <- TRUE
  mat[is.na(mat)] <- FALSE
  mat
}

paper_filled <- purrr::reduce2(test_x, test_y, ~populate(..2, ..3, ..1), .init = paper)


fold_y <- function(mat, pos){

  mat_a <- mat[1:(pos),]
  mat_b <- mat[(pos+2):nrow(mat), ]
  mat_a + mat_b[nrow(mat_b):1,]
}




fold_x <- function(mat, pos){
  mat_a <- mat[,1:(pos)]
  mat_b <- mat[,(pos+2):ncol(mat) ]
  mat_a + mat_b[,ncol(mat_b):1]

}

new <- fold_y(mat = paper_filled, pos = 7)

newer <- fold_x(mat = new, pos = 5 )

sum(newer != 0)
fold_paper <- function(mat, dir = c("x", "y")){
  if(dir == "x") fold_x(mat = mat) else if(dir == "y") fold_y(mat = mat)
}




input <- readLines("./data/day_13.txt")

folds <- input[stringr::str_detect(input, "fold")]

xy <- stringr::str_extract(folds, "x|y")
folds <- readr::parse_number(folds)
names(folds)<- xy

folds

coords <- input[stringr::str_detect(input, "^\\d.+\\d$")]
real_x <-as.numeric(stringi::stri_extract_first_regex(coords, pattern = "\\d+")) +1
real_y <- as.numeric(stringi::stri_extract_last_regex(coords, pattern = "\\d+")) +1




paper <- matrix(data = NA, nrow = max(real_y), ncol = max(real_x))
paper_filled <- purrr::reduce2(real_x, real_y, ~populate(..2, ..3, ..1), .init = paper)
paper_filled


res <- fold_paper(mat = paper_filled, dir = names(folds)[1], pos = folds[1])
sum(res != 0)


res <- purrr::accumulate(1:4, ~fold_paper(.x), .init = paper_filled )


fold_y <- function(mat){

  mat_a <- mat[1:floor(nrow(mat)/2),]
  mat_b <- mat[(ceiling(nrow(mat)/2)+1):nrow(mat), ]
  mat_a + mat_b[nrow(mat_b):1,]
}


fold_x <- function(mat){
  mat_a <- mat[,1:floor(ncol(mat)/2)]
  mat_b <- mat[,(ceiling(ncol(mat)/2)+1):ncol(mat)]
  mat_a + mat_b[,ncol(mat_b):1]

}

fold_paper <- function(mat, dir = c("x", "y")){
  if(dir == "x") fold_x(mat = mat) else if(dir == "y") fold_y(mat = mat)
}

res <- fold_paper(mat = paper_filled, dir = "x")
sum(res != 0)
res <- purrr::reduce(xy, ~ fold_paper(.x, dir = .y), .init = paper_filled)


check <- paper_filled %>%
  fold_paper(xy[1]) %>%
  fold_paper(xy[2]) %>%
  fold_paper(xy[3]) %>%
  fold_paper(xy[4]) %>%
  fold_paper(xy[5]) %>%
  fold_paper(xy[6]) %>%
  fold_paper(xy[7]) %>%
  fold_paper(xy[8]) %>%
  fold_paper(xy[9]) %>%
  fold_paper(xy[10]) %>%
  fold_paper(xy[11]) %>%
  fold_paper(xy[12])


letter_1 <- check[,6:10]
letter_1[letter_1 == 0] <- "#"
letter_1[str_detect(letter_1, "\\d")] <- "|"
letter_1
view(res)
tibble(res) %>%
  mutate(line = row_number()) %>%
  group_by(line) %>%
  summarise(paste0(collapse = ""))




reshape2::melt(mat) %>%
  ggplot(aes(Var2, -Var1, fill = value)) +
  geom_raster()

