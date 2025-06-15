#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(bslib)
library(ggplot2)
library(ggthemes)
library(googlesheets4)
library(htmltools)
library(shiny)
library(thematic)
library(tidyverse)


function(request){
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
}

# Define UI for application that draws a histogram
ui <- page_navbar(
  nav_menu(
    title = "About",
    markdown("
      ## About
      We fielded a short poll to determine the areas of greatest interest to the [LinkedIn People of Interior group](https://www.linkedin.com/groups/13267119/). Here are the results.

      **About**
      [Next Interior](https://www.nextinterior.org)

      **Questions?**
      [Get in touch](mailto:info@nextinterior.org)

      **Code**
      [GitHub](https://github.com/Next-Interior/POI-focus-areas-dash)
    ")
  ),

  layout_columns(
    card(card_header("Jobs, jobs, jobs"),
         plotOutput(outputId = "jobs")),
    card(card_header("Social engagement"),
         plotOutput(outputId = "social")),
    card(card_header("Who is still around?"),
         plotOutput(outputId = "around"))
  ),

  layout_columns(
    card(card_header("Retirement"),
         plotOutput(outputId = "retire")),
    card(card_header("Network-of-networks"),
         plotOutput(outputId = "network")),
    card(card_header("Ideas"),
         markdown(htmlOutput(outputId = "ideas")))
  ),

  title = "POI Focal Areas Poll",
  fillable = TRUE,
  theme = bs_theme(
    version = "5",
    bootswatch = "sandstone",
    "navbar-bg" = "#805640",
    "nav-text-color" = "#ffffff !important",
  ) |>
    bs_add_rules(
      list(
        ".card { margin: 10px 0 }",
        ".bslib-page-fill { background-color: #f8f5f0 }",
        "a { color: #649DD7; }"
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  gs4_deauth()
  thematic_rmd()

  dat <- read_sheet("https://docs.google.com/spreadsheets/d/1jWmFviisMfDAYlNbXgpNAR12zkll-tqnYhcG6SuLJF8/edit?usp=sharing")
  names(dat) <- c("time", "jobs", "social", "still_around",
                  "comms", "retire", "network", "other")

  output$jobs <- renderPlot({
    j <- table(factor(dat$jobs, levels = 1:5)) |> data.frame()
    names(j) <- c("rating", "votes")

    ggplot(j, aes(x=rating, y=votes)) +
      geom_bar(stat = "identity", fill="#805640cc") +
      labs(x = "Rating", y = "# votes") +
      theme_tufte(base_size=22, base_family = "sans")
  })

  output$social <- renderPlot({
    s <- table(factor(dat$social, levels = 1:5)) |> data.frame()
    names(s) <- c("rating", "votes")

    ggplot(s, aes(x=rating, y=votes)) +
      geom_bar(stat = "identity", fill="#805640cc") +
      labs(x = "Rating", y = "# votes") +
      theme_tufte(base_size=22, base_family = "sans")
  })

  output$around <- renderPlot({
    w <- table(factor(dat$still_around, levels = 1:5)) |> data.frame()
    names(w) <- c("rating", "votes")

    ggplot(w, aes(x=rating, y=votes)) +
      geom_bar(stat = "identity", fill="#805640cc") +
      labs(x = "Rating", y = "# votes") +
      theme_tufte(base_size=22, base_family = "sans")
  })

  output$retire <- renderPlot({
    r <- table(factor(dat$retire, levels = 1:5)) |> data.frame()
    names(r) <- c("rating", "votes")

    ggplot(r, aes(x=rating, y=votes)) +
      geom_bar(stat = "identity", fill="#805640cc") +
      labs(x = "Rating", y = "# votes") +
      theme_tufte(base_size=22, base_family = "sans")
  })

  output$retire <- renderPlot({
    r <- table(factor(dat$retire, levels = 1:5)) |> data.frame()
    names(r) <- c("rating", "votes")

    ggplot(r, aes(x=rating, y=votes)) +
      geom_bar(stat = "identity", fill="#805640cc") +
      labs(x = "Rating", y = "# votes") +
      theme_tufte(base_size=22, base_family = "sans")
  })

  output$network <- renderPlot({
    n <- table(factor(dat$network, levels = 1:5)) |> data.frame()
    names(n) <- c("rating", "votes")

    ggplot(n, aes(x=rating, y=votes)) +
      geom_bar(stat = "identity", fill="#805640cc") +
      labs(x = "Rating", y = "# votes") +
      theme_tufte(base_size=22, base_family = "sans")
  })

  output$ideas <- renderText({
    string.list <- na.omit(dat$other)
    paste(paste('-', string.list), sep="\n")
  })

}

# Run the application
shinyApp(ui = ui, server = server)
