library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)


get_rawg_page <- function(page) {
  resp <- request("https://api.rawg.io/api/games") |>
    req_url_query(
      key = Sys.getenv("RAWG_KEY"), #en .R_environ pues no quiero dar mi API xd
      page = page,
      page_size = 40
    ) |>
    req_perform()

  fromJSON(resp_body_string(resp), flatten = TRUE)
}

pages <- map(1:100, get_rawg_page)
rawg <- bind_rows(map(pages, "results"))

glimpse(rawg)
head(rawg)
games<-write_csv(rawg, "rawg_games.csv")

games <- read.csv("rawg_games.csv")
games <- games %>% select(c(id,name,released,rating, ratings_count, metacritic, playtime, updated, added_by_status.owned, added_by_status.beaten,  added_by_status.toplay, added_by_status.dropped, added_by_status.playing, esrb_rating.name))


plot(games$rating, games$playtime, main = "Rating vs Playtime", xlab = "Rating", ylab = "Playtime")
head(games)
glimpse(games)
write_csv(games, "rawg_games.csv")



