#aoc1
require(dplyr)
require(stringr)
input <- read.csv("input1.txt",header=FALSE)

map <- data.frame(
  c("1","2","3","4","5","6","7","8","9","one","two","three","four","five","six","seven","eight","nine"),
  c("1","2","3","4","5","6","7","8","9","1","2","3","4","5","6","7","8","9")
)

names(map) <- c("char","value")

s <- 0
keys <- data.frame(key=numeric(),input=character(),sum=numeric())
for (row_num in seq(1,nrow(input))) {
  row <- input[row_num,]
  key <- ""
  found <- FALSE
  for (c in seq(1,nchar(row))) {
    if (!found) {
      hay <- substr(row,1,c)
      for (needle in map$char) {
        if (!is.na(str_locate(hay,needle)[1])) {
          foundkey <- map[map$char==needle,]$value
          key <- paste(key,foundkey,sep="")
          found <- TRUE
          break
        }
      }
    }
  }
  #reverse
  found <- FALSE
  for (c in seq(nchar(row),1)) {
    if (!found) {
      hay <- substr(row,c,nchar(row))
      for (needle in map$char) {
        if (!is.na(str_locate(hay,needle)[1])) {
          foundkey <- map[map$char==needle,]$value
          key <- paste(key,foundkey,sep="")
          print(paste(row,key,sep=":"))
          #keys <- append(keys, as.numeric(key))
          s <- s + as.numeric(key)
          keys[nrow(keys)+1,] <- c(
            key=as.numeric(key),
            input=as.character(row),
            sum=s
          )
          found <- TRUE
          break
        }
      }
    }
  }
  
}
 
input %>% print()
 

numbers <- c(
  "one" = 1, "two" = 2, "three" = 3, "four" = 4, "five" = 5,
  "six" = 6, "seven" = 7, "eight" = 8, "nine" = 9,
  setNames(nm = 1:9)
)
srebmun <- numbers
names(srebmun) <- stringi::stri_reverse(names(srebmun))

get_digit <- function(x, ref) {
  res <- x %>%
    stringr::str_extract(paste0("(", paste0(names(ref), collapse = "|"), ")"))
  
  ref[res]
}

sum(
  get_digit(input, numbers) * 10 +
    get_digit(stringi::stri_reverse(input), srebmun)
)


shopping_list <- c("apples x4", "bag of flour", "bag of sugar", "milk x2")
str_extract(shopping_list, "\\d")
