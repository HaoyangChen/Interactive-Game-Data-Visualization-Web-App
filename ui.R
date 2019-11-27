library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
# Defines the UI that creates an about page, and 3 different plots in each tab for Game-Viz app.
shinyUI(tagList(
  # shinythemes::themeSelector(),
  navbarPage(
    theme = shinytheme("superhero"),
    "Game-Viz",
    tabPanel(
      "About",
      mainPanel(
        h3("Group Name: Game-Viz-Group"),
        HTML({
          paste(
            "<ul><strong>Group Members</strong>",
            "<li>Haoyang Chen</li>",
            "<li>Andi Ren</li>",
            "<li>Wayne Li</li>",
            "</ul>"
          )
        }),
        h3("What is this app and what is it for?"),
        p("This Shiny web app helps the users to visualize video game related information."),
        h3("What is the dataset we are working with?"),
        p("We'll be working with the Internet Games Database(IGDB), a community-driven site 
        that gathers and shares game-related information. 
        The data is collected by IGDB.com, and we gain access through its API."),
        h3("Who is the target audience?"),
        p("Our target audience is video game players who likes to analyze and critique"),
        h3("How do users interact with the app and what visualizations will be shown?"),
        p("The user can visualize the information about video game by interacting 
         with the widgets in the side panel."),
        p("In the tab of 'Genre Distribution', the user can enter a particular release year 
        and check different genre types to compare the proportions shown in the pie graph. 
        Information about the least popular genre and the most popular genre in the selected year 
        is also shown above the pie graph. "),
        p("In the tab of 'Gaming Rating Summary', the users can type and select a particular genre, 
         theme, and franchise. They also can adjust the range of the year and check the 
         option of showing base games only. The corresponding line plot will be shown in the 
         main panel based on the selected values. A summary, which includes the total number 
        of games in the selected categories, the highest rated game, the lowest rated game, 
        average rating, the average rating of the games, the standard deviation of the games 
        and the average score of the group, is shown below the line plot."),
        p("In the tab of 'Game Hotness Gauge', the users can select one measurement of a particular 
        game that they are interested in, and the gauge chart will show the value of the selected 
        game in the selected measurement. A picture and an overview of the selected game are shown 
        below the gauge chart.")
      )
    ),
    tabPanel(
      "Genre Distribution",
      titlePanel("Genre distribution of video games over the years"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("select_year"),
          actionButton("reset", "Clear All Game Types",
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
          ),
          uiOutput("filter_genre")
        ),
        mainPanel(
          h3("Information Obtained from the Pie Chart"),
          br(),
          textOutput("total_genres"),
          br(),
          textOutput("least_genre"),
          br(),
          textOutput("most_genre"),
          br(),
          hr(),
          plotlyOutput("genre_pie_chart")
        )
      )
    ),
    tabPanel(
      "Game Rating Summary",
      titlePanel("Rating Summary of Games Across Time, Genre, Theme, and Franchise"),
      sidebarPanel(
        uiOutput("select_element"),
        uiOutput("select_year_wayne"),
        uiOutput("base_game")
      ),
      mainPanel(
        plotlyOutput("lineplot"),
        br(),
        htmlOutput("lineplot_text")
      )
    ),
    tabPanel(
      "Game Hotness Gauge",
      titlePanel("Game Status Gauge and Summary"),
      sidebarPanel(
        uiOutput("search_game"),
        uiOutput("select_measure")
      ),
      mainPanel(
        plotlyOutput("gauge_plot"),
        br(),
        fluidPage(
          htmlOutput("recommandation")
        )
      )
    )
  ),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  )
))