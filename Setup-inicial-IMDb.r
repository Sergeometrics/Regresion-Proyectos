#pkgs <- c("tidyverse", "httr2", "tibble", "purrr")
#new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
#if (length(new_pkgs) > 0) install.packages(new_pkgs)
#invisible(lapply(pkgs, library, character.only = TRUE))


#  download.file("https://datasets.imdbws.com/title.basics.tsv.gz", "title.basics.tsv.gz")
#  download.file("https://datasets.imdbws.com/title.ratings.tsv.gz", "title.ratings.tsv.gz")



basics  <- read_tsv("title.basics.tsv.gz",  na = "\\N", show_col_types = FALSE)
ratings <- read_tsv("title.ratings.tsv.gz", na = "\\N", show_col_types = FALSE)

imdb <- basics %>%
  filter(titleType == "movie") %>%
  inner_join(ratings, by = "tconst") %>%
  select(tconst, primaryTitle, startYear, runtimeMinutes,
         genres, averageRating, numVotes)




set.seed(123)   # reproducible sample
imdb_muestra <- imdb %>%
  filter(numVotes > 500, !is.na(runtimeMinutes)) %>%
  slice_sample(n = 5000)



# api_key <- "ACA QUI VA LA API KEY DE TMDB" (correr en consola y no publicar) 

get_tmdb <- function(tconst, api_key) {
  out <- tibble(tconst = tconst,
                presupuesto = NA_real_,
                taquilla    = NA_real_,
                popularity  = NA_real_)
  tryCatch({
    # call 1: find the TMDb internal id
    resp <- request("https://api.themoviedb.org/3/find/") |>
      req_url_path_append(tconst) |>
      req_url_query(api_key = api_key, external_source = "imdb_id") |>
      req_retry(max_tries = 3) |>
      req_perform()

    res <- resp_body_json(resp)$movie_results
    if (length(res) > 0) {
      out$popularity <- res[[1]]$popularity
      id <- res[[1]]$id

      det <- request("https://api.themoviedb.org/3/movie/") |>
        req_url_path_append(as.character(id)) |>
        req_url_query(api_key = api_key) |>
        req_retry(max_tries = 3) |>
        req_perform()
      d <- resp_body_json(det)

      out$presupuesto <- ifelse(is.null(d$budget)  || d$budget  == 0, NA_real_, d$budget)
      out$taquilla    <- ifelse(is.null(d$revenue) || d$revenue == 0, NA_real_, d$revenue)
    }
  }, error = function(e) NULL)
  out
}




resultados <- map_dfr(imdb_muestra$tconst,
                      get_tmdb,
                      api_key = api_key,
                      .progress = TRUE)

# 7. Join, filter, export --------------------------------------
imdb_final <- imdb_muestra %>%
  left_join(resultados, by = "tconst") %>%
  filter(!is.na(presupuesto)) %>%
  mutate(log_presupuesto = log(presupuesto),
         log_taquilla    = log(taquilla))

nrow(imdb_final)   # check: should be a few thousand, not 0

write_csv(resultados, "imdb_taquilla_presupuesto.csv")
write_csv(imdb_final, "imdb_datos_completos.csv")
