library(shiny)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(textclean)

load("data/5000games.Rda")
load("data/companylist.Rdat")
load("data/genrelist.Rdat")
load("data/themedata.Rdat")
load("data/collectiondata.Rdat")

game_datas_all <- game_datas_all %>%
  mutate(first_release_date = as.Date(game_datas_all$first_release_date)) %>%
  # Fixes some API encoding issue.
  mutate(summary = mgsub(game_datas_all$summary,
                         c("â€™", "Â®", "Î»", "Â²", "â€“", "â€¦", "â„¢", "â€”", "Ã©"),
                         c("'", "®", "λ", "²", "", "", "", " ", "é"))) %>%
  mutate(name = gsub("Ã©", "é", game_datas_all$name))
game_datas <- game_datas_all %>% select(
  id, name, collection,
  total_rating, game,
  developers, themes,
  genres, first_release_date)
collection_datas <- collection_datas %>% arrange(name)

# Generate a vector of unique release year 
generate_unique_release_year <- function() {
  filtered_data <- game_datas_all %>%
    select(id, name, first_release_date, genres)
  unique_year <- substr(filtered_data$first_release_date, 1, 4)
  unique_year <- unique(unique_year)
  unique_year <- unique_year[!is.na(unique_year)]
  unique_year <- sort(unique_year, decreasing = TRUE)
}

# Given a genre's id as paramter, generates its total occurrences. 
generate_genre_occurrence <- function(genre_id) {
  length(which(grepl(genre_id, game_datas_all$genres)))
}

# Given a genre name, a genre id, and a selected year as parameters, generates 
# a pie chart of genre distribution in the given year
generate_pie_chart <- function(genre_name, genre_id, year) {
  result <- plot_ly(
    labels = genre_name, values = genre_id,
    # textposition = 'inside',
    textinfo = "label+percent",
    width = 950,
    height = 600
    ) %>%
    add_pie(hole = 0.6) %>%
    layout(
      title = paste(
        "Genre distribution of video games in the year of",
        year
      ),
      font = list(
        color = "#fff"
      ),
      showlegend = F,
      paper_bgcolor = "transparent",
      xaxis = list(
        showgrid = FALSE, zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE, zeroline = FALSE,
        showticklabels = FALSE
      )
    )
}

#Plots out the gauge plot
generate_gauge_chart <- function(value, max, name, measure, tabs) {
  rad <- (1 - (value / max)) * pi
  if (is.na(value)) {
    value <- "Non exisitent"
  }
  
  # plots out the base donut chart in transparent color,
  # serves as the base for the gauge.
  base_plot <- plot_ly(
    type = "pie",
    values = c(40, 10, 10, 10, 10, 10, 10),
    labels = c(
      " ", " ", " ", " ", " ", " ",
      round(max)
    ),
    rotation = 108,
    direction = "clockwise",
    hole = 0.6,
    textinfo = "label",
    textposition = "outside",
    hoverinfo = "none",
    domain = list(x = c(0, 1), y = c(0, 1)),
    marker = list(colors = c(
      "transparent", "transparent", "transparent",
      "transparent", "transparent", "transparent",
      "transparent"
    )),
    showlegend = FALSE,
    width = 950,
    height = 475
  )
  #plot out the colored donut chart to serve as the dashboard.
  base_plot <- add_trace(
    base_plot,
    type = "pie",
    values = c(50, 10, 15, 12.5, 7.5, 5),
    labels = tabs,
    rotation = 90,
    direction = "clockwise",
    hole = 0.5,
    textinfo = "label",
    textposition = "inside",
    hoverinfo = "none",
    domain = list(x = c(0, 1), y = c(0, 1)),
    marker = list(colors = c(
      "transparent", "rgb(232,226,202)",
      "rgb(244,220,66)", "rgb(244,166,66)",
      "rgb(244,100,66)", "rgb(244,66,66)"
    )),
    showlegend = FALSE
  )
  
  a <- list(
    showticklabels = FALSE,
    autotick = FALSE,
    showgrid = FALSE,
    zeroline = FALSE
  )
  
  #defines font color and draws the hand of the gauge
  base_chart <- layout(
    base_plot,
    font = list(
      color = "#fff"
    ),
    #draws the hand of the gauge as a vector shape
    shapes = list(
      list(
        type = "path",
        path = paste(
          "M 0.475 0.5 L", as.character(0.15 * cos(rad) + 0.5),
          as.character(0.3 * sin(rad) + 0.5), "L 0.525 0.5 Z"
        ),
        xref = "paper",
        yref = "paper",
        fillcolor = "yellow"
      )
    ),
    xaxis = a,
    yaxis = a,
    paper_bgcolor = "transparent",
    annotations = list(
      xref = "paper",
      yref = "paper",
      x = 0.5,
      y = 0.4,
      showarrow = F,
      text = paste("The", measure, "for", name, "is", round(value))
    )
  )
}

# Define server logic required to produce home page, and 3 plot tabs
shinyServer(function(input, output, session) {
  # This reactive function filters out a list of games needed for plotting.
  reactive_data <- reactive({
    game_data <- game_datas_all
    # Filters out the desired genre, theme, and franchise
    if (!is.null(input$franchise) & !is.null(input$genre) & !is.null(input$theme)) {
      if (input$theme[1] != "all") {
        selected_theme <- theme_datas %>% filter(name == input$theme[1])
        gamelist_theme <- unlist(selected_theme$games, use.names = FALSE)
        game_data <- game_data %>% filter(id %in% gamelist_theme)
      }
      if (input$franchise[1] != "all") {
        selected_franchise <- collection_datas %>% filter(name == input$franchise[1])
        gamelist_franchise <- unlist(selected_franchise$games, use.names = FALSE)
        game_data <- game_data %>% filter(id %in% gamelist_franchise)
      }
      if (input$genre[1] != "all") {
        selected_genre <- genre_datas %>% filter(name == input$genre[1])
        gamelist_genre <- unlist(selected_genre$games, use.names = FALSE)
        game_data <- game_data %>% filter(id %in% gamelist_genre)
      }
    }
    start_year <- as.Date(paste0(input$year[1], "-01-01"))
    end_year <- as.Date(paste0(input$year[2], "-12-31"))
    # Filters out the desired games in a year range.
    game_data <- game_data %>%
      filter(start_year <= first_release_date & end_year >= first_release_date)
    # Filters out base games.
    if (input$base[1]) {
      game_data <- game_data %>% filter(is.na(game))
    }
    game_data <- game_data %>%
      select(name, first_release_date, total_rating) %>%
      arrange(first_release_date)
  })
  # This function renders the lists of dropdown for users to select.
  output$select_element <- renderUI({
    output <- tagList()
    output[[1]] <- selectizeInput("genre",
      label = h4("Genre"),
      choices = c("Select all" = "all", genre_datas$name),
      selected = "all"
    )
    output[[2]] <- selectizeInput("theme",
      label = h4("Theme"),
      choices = c("Select all" = "all", theme_datas$name),
      selected = "all"
    )
    output[[3]] <- selectizeInput("franchise",
      label = h4("Franchise"),
      choices = c("Select all" = "all", collection_datas$name),
      size = 20,
      selected = "all"
    )
    output
  })
  # This funtion renders a slider for user to select a range of year.
  output$select_year_wayne <- renderUI({
    sliderInput("year",
      label = "Year",
      min = 1971,
      max = 2018,
      value = c(1971, 2018)
    )
  })
  # This function renders a checkbox for user to check
  # if they want dlcs (expansion) to be included along with base game.
  output$base_game <- renderUI({
    checkboxInput("base", label = "Base games only", value = FALSE)
  })
  # This function renders a plotly plot for the "game rating summary" page.
  output$lineplot <- renderPlotly({
    game_data <- reactive_data()
    if (nrow(game_data) >= 0) {
      x_style <- list(title = "Launch Date")
      y_style <- list(title = "Rating", range = c(10, 100))
      plot <- plot_ly(game_data,
        x = ~first_release_date, y = ~total_rating,
        type = "scatter", mode = "lines+markers",
        text = ~name
      ) %>%
        layout(xaxis = x_style, yaxis = y_style)
    }
  })
  # This function renders a paragraph of summary
  # for the "game rating summary" page.
  output$lineplot_text <- renderText({
    game_data <- reactive_data()
    if (nrow(game_data) > 0) {
      sum_text <- paste0(
        "<p>According to our database, from ",
        input$year[1], "-01 to ",
        input$year[2], "-12, there are <strong>",
        nrow(game_data), "</strong> games in ",
        input$genre[1], " genre, ",
        input$theme, " theme, ", " and ",
        input$franchise, " franchise. "
      )
      if (mean(game_data$total_rating) >= 90) {
        score_text <- "\"<strong>Universal Acclaim</strong>\""
      } else if (mean(game_data$total_rating) >= 75) {
        score_text <- "\"<strong>Generally Favorable</strong>\""
      } else if (mean(game_data$total_rating) >= 50) {
        score_text <- "\"<strong>Mixed or Average</strong>\""
      } else if (mean(game_data$total_rating) >= 20) {
        score_text <- "\"<strong>Generally Unfavorablev\""
      } else {
        score_text <- "\"<strong>Universal Dislike\""
      }
      if (nrow(game_data) <= 1) {
        sum_text <- paste0(
          sum_text, "</p><p>",
          "According to the criteria of Metacritic,",
          " the score of this game would qualify as ",
          score_text, ". </p>"
        )
      } else if (nrow(game_data) <= 2) {
        sum_text <- paste0(
          sum_text, "</p><p>",
          " The average score of these games are ",
          round(mean(game_data$total_rating), 2), ". </p><p>",
          "According to the criteria of Metacritic, ",
          "the average score of this group would qualify as ",
          score_text, ". </p>"
        )
      } else {
        sum_text <- paste0(
          sum_text,
          "</p><p>The highest rated game here is <strong>",
          game_data[which.max(game_data$total_rating), ]$name,
          "</strong> with a rating of <strong>",
          round(max(game_data$total_rating), 2),
          "</strong>, and the lowest rated game is <strong>",
          game_data[which.min(game_data$total_rating), ]$name,
          "</strong> with a rating of <strong>",
          round(min(game_data$total_rating), 2),
          "</strong>. </p><p>", " The <strong>average rating",
          "</strong> of these games are <strong>",
          round(mean(game_data$total_rating), 2),
          "</strong> with a standard deviation of <strong>",
          round(sd(game_data$total_rating), 2),
          "</strong>. </p><p>", "According to the",
          "criteria of <a href=\"https://www.metacritic.com",
          "/about-metascores\">Metacritic</a>,",
          "the average score of this group would qualify as ",
          score_text, ". </p>"
        )
      }
    } else {
      sum_text <- paste0(
        "There is 0 game in your selected genre, ",
        "theme, and franchise."
      )
    }
    sum_text
  })
  # displays the information based on the selected game and measurement. 
  output$recommandation <- renderText({
    game <- arrange(filter(game_datas_all, name == input$search[1]), -popularity)[1, ]
    game_html <- paste0(
      "<h4 align = center>", game$name,
      "</h4><p align = center><img src=",
      game$cover.url,
      "></p><p align = center>Rating: ",
      round(game$total_rating, 2),
      "</p><p>Summary: ",
      game$summary, "</p>"
    )
  })

  #select or search a particular game to be used in the guage chart.
  output$search_game <- renderUI({
    selectizeInput(
      "search", "Select Game", choices = game_datas_all$name
    )
  })
  
  #select or search a particular variable to measure in the guage chart.
  output$select_measure <- renderUI({
    game <- game_datas_all
    game <- arrange(game, -popularity)
    selectizeInput("Measurement",
      label = "Measurement",
      size = 1,
      choices = c("Overall Rating", "User Rating", "Critic Rating", "Media Attention")
    )
  })
  
  #provides dropdown selection menu with type-in search function and initiates gauge chart plotting.
  output$gauge_plot <- renderPlotly({
    game <- arrange(filter(game_datas_all, name == input$search[1]), -popularity)[1, ]
    name <- game$name
    measure <- input$Measurement[1]
    tabs <- c(" ", "Mixed", "Negagtive", "Mostly Negative", "Positive", "Best Game Ever!") 
    if (measure == "Media Attention") {
      value <- game$popularity
      max <- max(game_datas_all$popularity)
      tabs <- c(" ", "Talk-about", "Quiet", "Fading away", "Under a spotlight", "Under a microscope")
    } else if (measure == "Overall Rating") {
      value <- game$total_rating
      max <- max(game_datas_all$total_rating)
    } else if (measure == "User Rating") {
      value <- game$rating
      max <- max(filter(game_datas_all, !is.na(rating))$rating)
    } else {
      value <- game$aggregated_rating
      max <- max(filter(game_datas_all, !is.na(aggregated_rating))$aggregated_rating)
    }
    generate_gauge_chart(value, max, name, measure, tabs)
  })
  
  # Displays a dropdown selection menu with different release year for users to select from. 
  output$select_year <- renderUI({
    return(selectInput("year_selection", "Select Release Year", choices = generate_unique_release_year(), selected = 2018))
  })
  
  # Displays multiple checkboxes of different game types for users to choose. The values of the 
  # checkboxes are based on the selected year. 
  output$filter_genre <- renderUI({
    selected_year_data <- game_datas_all %>%
      select(id, name, first_release_date, genres) %>%
      filter(substr(first_release_date, 1, 4) == input$year_selection)
    unique_genre_number <- unique(unlist(selected_year_data$genres))
    id <- unique_genre_number
    unique_genre_dataframe <- data.frame(id, stringsAsFactors = FALSE)
    small_genre_data <- select(genre_datas, id, name)
    joined_dataframe <- inner_join(small_genre_data,
      unique_genre_dataframe,
      by = "id"
    )
    return(checkboxGroupInput("genre_types", "Game Type(s)",
      choices = joined_dataframe$name,
      selected = joined_dataframe$name
    ))
  })

  # Observes whether the button of reset is clicked. Once the button is clicked, 
  # all the checkboxes will become unchecked.
  observe({
    x <- input$reset
    if (is.null(x)) {
      x <- character(0)
    }
    updateCheckboxGroupInput(session, "genre_types", selected = x)
  })

  # Displays the pie chart in the main panel based on the input of year and chosen game types.
  # Information about the total number of different game types in the selected year, 
  # the least developed game type and the most developed game type will also be displayed. 
  output$genre_pie_chart <- renderPlotly({
    if (!is.null(input$genre_types)) {
      year_data <- game_datas_all %>%
        select(id, name, first_release_date, genres) %>%
        filter(substr(first_release_date, 1, 4) == input$year_selection)
      genres_vector <- unlist(year_data$genres)
      filtered_genres <- genre_datas %>% filter(name %in% input$genre_types)
      genre_vector <- filtered_genres$id
      genre_occurrences <- sapply(genre_vector, generate_genre_occurrence)
      total_count <- sum(genre_occurrences)
      output$total_genres <- renderText({
        different_genres_total <- length(input$genre_types)
        paste("There are", different_genres_total, "different game types in", input$year_selection)
      })
      output$most_genre <- renderText({
        most_popular_number <- max(genre_occurrences)
        most_popular_index <- which(genre_occurrences == most_popular_number)
        most_popular_name <- input$genre_types
        most_popular_name <- most_popular_name[most_popular_index]
        most_popular_percent <- round(most_popular_number / total_count * 100,
          digits = 3
        )
        paste(
          "Most developed:", most_popular_name,
          ", with a proportion of", most_popular_percent,
          "% in the selected types."
        )
      })
      output$least_genre <- renderText({
        least_popular_number <- min(genre_occurrences)
        least_popular_index <- which(genre_occurrences == least_popular_number)
        least_popular_name <- input$genre_types
        least_popular_name <- least_popular_name[least_popular_index]
        least_popular_percent <- round(least_popular_number / total_count * 100,
          digits = 3
        )
        paste(
          "Least developed:", least_popular_name,
          ", with a proportion of", least_popular_percent,
          "% in the selected types."
        )
      })
      generate_pie_chart(input$genre_types, genre_occurrences, input$year_selection)
    }
  })
})