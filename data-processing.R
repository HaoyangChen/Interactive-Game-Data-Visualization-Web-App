library(dplyr)
library(plotly)
#ufo_test_data <- read.csv("UFOCoords.csv")
#p <- plot_ly(ufo_test_data, x = ~lat, y = ~lng, type = "scatter")
#print(p)
load(file = "data/5000games.Rda")

id <- 12
count <- length(which(grepl(12, game_datas_all$genres)))
genre <- filter(game_datas_all, grepl(12, game_datas_all$genres))
load(file = "5000games.Rda")
load(file = "genrelist.Rdat")
