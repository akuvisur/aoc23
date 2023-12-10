#aoc6
require(purrr)

input <- readLines("input6.txt")
times <- input[1] %>% str_extract_all("\\d+") %>% unlist() %>% as.integer()
distances <- input[2] %>% str_extract_all("\\d+") %>% unlist() %>% as.integer()

map <- data.frame(t=times,d=distances)

wins <- function(t,d) {
  wins <- 0
  for (time in seq(0,t)) {
    dist <- (t-time) * time
    #print(paste(time,dist))
    if (dist > d) wins <- wins + 1
  }
  wins
}

s <- purrr::map2_dbl(map[,1],map[,2], ~ wins(.x,.y)) %>% prod()

#-- part 2
s2 <- wins(as.numeric(paste0(map[,1],collapse="")),as.numeric(paste0(map[,2],collapse="")))
