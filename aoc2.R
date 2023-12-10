#aoc 2

input <- readLines("input2.txt")

blue <- function(line) {
  res <- line %>% str_extract_all("\\d+ blue") %>% str_extract_all("\\d+") %>% unlist() %>% as.integer()
  return(all(res <=14))
}

green <- function(line) {
  res <- line %>% str_extract_all("\\d+ green") %>% str_extract_all("\\d+") %>% unlist() %>% as.integer()
  return(all(res <=13))
}

red <- function(line) {
  res <- line %>% str_extract_all("\\d+ red") %>% str_extract_all("\\d+") %>% unlist() %>% as.integer()
  return(all(res <=12))
}

game <- function(line,possible_games) {
  res <- line %>% str_extract("Game \\d+")
  res <- strsplit(res, " ")[[1]][2] %>% as.integer()
  print(paste0(line,possible_games[res]))
  if (possible_games[res]) return(res)
  else return(0)
}

games <- input %>% lapply(FUN=function(x) blue(x) && green(x) && red(x)) %>% unlist()
sum <- input %>% lapply(function(x) sum(game(x,games))) %>% unlist() %>% sum()

# part 2
blue <- function(line) {
  res <- line %>% str_extract_all("\\d+ blue") %>% str_extract_all("\\d+") %>% unlist() %>% as.integer()
  max(res)
}

green <- function(line) {
  res <- line %>% str_extract_all("\\d+ green") %>% str_extract_all("\\d+") %>% unlist() %>% as.integer()
  max(res)
}

red <- function(line) {
  res <- line %>% str_extract_all("\\d+ red") %>% str_extract_all("\\d+") %>% unlist() %>% as.integer()
  max(res)
}

sum <- input %>% lapply(FUN=function(x) blue(x)*red(x)*green(x)) %>% unlist() %>% sum()


