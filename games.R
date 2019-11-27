library(httr)
library(jsonlite)
library(dplyr)

if (Sys.info()["user"] == "Denouement") {
  source("~/Documents/info201_R/igdb_sample/igdb_key.R")
} else {
  source("keys.R")
}

# order by total ratings count in descending order
game_url <- paste0("https://api-endpoint.igdb.com",
                   "/games/?order=total_rating_count:desc&fields=*&limit=50&scroll=1")
game_res <- GET(game_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
game_datas_all <- flatten(fromJSON(rawToChar(content(game_res, "raw"))))

# continuation of the last query
for(i in 1:99) {
  game_nextpage_url <- paste0("https://api-endpoint.igdb.com",
                              game_res[["headers"]][["x-next-page"]])
  game_nextpage_content <- GET(game_nextpage_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
  game_nextpage <- flatten(fromJSON(rawToChar(content(game_nextpage_content, "raw"))))
  
  # Join the datas
  
  game_datas_all <- bind_rows(game_datas_all, game_nextpage)  
}

# You can repeatedly call game_nextpage and join datas for a larger dataframe
# You can also order by popularity, rating, etc.
# game_res_all[["headers"]][["x-count"]] gives you total number of games that imdb has, so we can call a for loop depends on it
# But ideally we should call query less, since I think that there is a monthly limit of 3000 queries.

load("data/1000games.Rda")
load("data/5000games.Rda")

# This step converts the first release date of games from UNIX epoch to human readable form
library(anytime)
# Note: I should rewrite this as a function

# game_datas_all <- mutate(game_datas_all, first_release_date = anydate(first_release_date / 1000))
# save(game_datas_all, file = "data/5000games.Rda")

# This code chunk tests the endpoint of game companies
company_url <- paste0("https://api-endpoint.igdb.com",
                      "/companies/?fields=*&limit=50&scroll=1")
company_res <- GET(company_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
company_datas <- flatten(fromJSON(rawToChar(content(company_res, "raw"))))

# This code chunk tests the endpoint of game genres
genre_url <- paste0("https://api-endpoint.igdb.com",
                    "/genres/?fields=*&limit=50&scroll=1")
genre_res <- GET(genre_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
genre_datas <- flatten(fromJSON(rawToChar(content(genre_res, "raw"))))

# This code chunk tests the endpoint of game franchises
franchises_url <- paste0("https://api-endpoint.igdb.com",
                         "/franchises/?fields=*&limit=50&scroll=1")
franchises_res <- GET(franchises_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
franchises_datas <- flatten(fromJSON(rawToChar(content(franchises_res, "raw"))))

# This code chunk extracts all developers from the list of 5000 games
unique_dev <- unique(game_datas_all$developers)
unique_dev <- unlist(unique_dev)
unique_dev <- unique(unique_dev)

# This code chunk utilizes the list of developers and extract all their information from the api
unique_dev_string <- paste0(as.character(unique_dev[1:50]), collapse=",")
unique_dev <- unique_dev[-(1:50)]

company_url <- paste0("https://api-endpoint.igdb.com",
                      "/companies/", unique_dev_string,
                      "?fields=*")
company_res <- GET(company_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
company_datas <- flatten(fromJSON(rawToChar(content(company_res, "raw"))))

unique_dev_string <- paste0(as.character(unique_dev[1:1978]), collapse=",")
# unique_dev <- unique_dev[-(1:200)]

company_url <- paste0("https://api-endpoint.igdb.com",
                      "/companies/", unique_dev_string,
                      "?fields=*")
company_res <- GET(company_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
company_datas2 <- flatten(fromJSON(rawToChar(content(company_res, "raw"))))
company_datas <- bind_rows(company_datas, company_datas2)

# Change the companies' founded date to human readable form
company_datas <- mutate(company_datas, start_date = anydate(start_date / 1000))

save(company_datas, file = "data/companylist.Rdat")
save(genre_datas, file = "data/genrelist.Rdat")

load("data/companylist.Rdat")
load("data/genrelist.Rdat")

# This code chunk starts to extract information of franchises from the list of 5000 games.
unique_fran <- unique(game_datas_all$franchise)
unique_fran <- na.omit(unique_fran)
unique_fran_string <- paste0(as.character(unique_fran[1:333]), collapse=",")

franchises_url <- paste0("https://api-endpoint.igdb.com",
                         "/franchises/", unique_fran_string,
                         "?fields=*&limit=50&scroll=1")
franchises_res <- GET(franchises_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
franchises_datas <- flatten(fromJSON(rawToChar(content(franchises_res, "raw"))))

save(franchises_datas, file = "data/franchisedata.Rdat")
load("data/franchisedata.Rdat")

# This code chunk experiments with collection field
unique_col <- unique(game_datas_all$collection)
unique_col <- na.omit(unique_col)

col_url <- paste0("https://api-endpoint.igdb.com",
                  "/collections/", unique_col[1],
                  "?fields=*")
col_res <- GET(col_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
collection_datas <- flatten(fromJSON(rawToChar(content(col_res, "raw"))))

for (i in 2:300) {
  col_url <- paste0("https://api-endpoint.igdb.com",
                    "/collections/", unique_col[i],
                    "?fields=*")
  col_res <- GET(col_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
  collection_datas <- bind_rows(collection_datas, flatten(fromJSON(rawToChar(content(col_res, "raw")))))
}

save(collection_datas, file = "data/collectiondata.Rdat")

# This code chunk extracts theme data from IGDB
unique_theme <- unlist(game_datas_all$themes)
unique_theme <- unique(unique_theme)
unique_theme_string <- paste0(as.character(unique_theme[1:21]), collapse=",")
theme_url <- paste0("https://api-endpoint.igdb.com",
                    "/themes/", unique_theme_string,
                    "?fields=*")
theme_res <- GET(theme_url, add_headers("user-key" = game_key, "Accept" = "application/json"))
theme_datas <- flatten(fromJSON(rawToChar(content(theme_res, "raw"))))

save(theme_datas, file = "data/themedata.Rdat")