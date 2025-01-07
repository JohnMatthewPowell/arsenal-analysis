# Install and load necessary packages
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("worldfootballR", quietly = TRUE)) devtools::install_github("JaseZiv/worldfootballR")

library(shiny)
library(ggplot2)
library(dplyr)
library(worldfootballR)

# Fetch Premier League match data for the 2024 season
premier_stats <- fb_match_results(
  "ENG",
  "M",
  "2024",
  tier = "1st",
  non_dom_league_url = NA
)

# Filter Arsenal matches and add derived variables
arsenal_matches <- premier_stats %>%
  filter(Home == "Arsenal" | Away == "Arsenal") %>%
  mutate(
    Result = case_when(
      Home == "Arsenal" & HomeGoals > AwayGoals ~ "Win",
      Away == "Arsenal" & AwayGoals > HomeGoals ~ "Win",
      HomeGoals == AwayGoals ~ "Draw",
      TRUE ~ "Loss"
    ),
    Goals_Scored = ifelse(Home == "Arsenal", HomeGoals, AwayGoals),
    Goals_Conceded = ifelse(Home == "Arsenal", AwayGoals, HomeGoals),
    xG_Scored = ifelse(Home == "Arsenal", Home_xG, Away_xG),
    Match_Date = as.Date(Date),
    Total_Goals = HomeGoals + AwayGoals
  )

# Shiny UI
# Add a new tab in the UI to display the about content
ui <- fluidPage(
  titlePanel("Arsenal Performance Analysis - 2024 Season"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("result_type", "Select Result Type:",
                  choices = c("All", "Win", "Draw", "Loss"), selected = "All"),
      selectInput("venue_type", "Select Venue:",
                  choices = c("All", "Home", "Away"), selected = "All"),
      dateRangeInput("date_range", "Select Date Range:",
                     start = min(arsenal_matches$Match_Date),
                     end = max(arsenal_matches$Match_Date)),
      textInput("opponent", "Enter Opponent Name (Optional):", value = "")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("About", 
                 includeMarkdown("about.Rmd")),  # This includes the contents of the "about.Rmd"
        tabPanel("Analysis", 
                 textOutput("summary"),
                 plotOutput("goalsPlot"),
                 tableOutput("matchTable"))
      )
    )
  )
)

# Shiny Server
server <- function(input, output) {
  # Filter matches based on user input
  filtered_matches <- reactive({
    matches <- arsenal_matches
    if (input$venue_type == "Home") {
      matches <- matches[matches$Home == "Arsenal", ]
    } else if (input$venue_type == "Away") {
      matches <- matches[matches$Away == "Arsenal", ]
    }
    if (input$opponent != "") {
      matches <- matches[matches$Home == input$opponent | matches$Away == input$opponent, ]
    }
    matches <- matches[matches$Match_Date >= input$date_range[1] & matches$Match_Date <= input$date_range[2], ]
    if (input$result_type != "All") {
      matches <- matches[matches$Result == input$result_type, ]
    }
    matches
  })
  
  # Display summary
  output$summary <- renderText({
    matches <- filtered_matches()
    if (nrow(matches) == 0) return("No matches found for the selected filters.")
    total_matches <- nrow(matches)
    wins <- sum(matches$Result == "Win")
    draws <- sum(matches$Result == "Draw")
    losses <- sum(matches$Result == "Loss")
    goals_scored <- sum(matches$Goals_Scored, na.rm = TRUE)
    goals_conceded <- sum(matches$Goals_Conceded, na.rm = TRUE)
    xg_scored <- sum(matches$xG_Scored, na.rm = TRUE)
    
    paste(
      "Total Matches:", total_matches,
      "\nWins:", wins,
      "\nDraws:", draws,
      "\nLosses:", losses,
      "\nGoals Scored:", goals_scored,
      "\nGoals Conceded:", goals_conceded,
      "\nAverage xG:", round(xg_scored / max(1, total_matches), 2)
    )
  })
  
  # Plot goals scored vs. conceded
  output$goalsPlot <- renderPlot({
    matches <- filtered_matches()
    if (nrow(matches) == 0) return(NULL)
    ggplot(matches, aes(x = Match_Date)) +
      geom_line(aes(y = Goals_Scored, color = "Goals Scored")) +
      geom_line(aes(y = Goals_Conceded, color = "Goals Conceded")) +
      labs(title = "Goals Scored vs. Goals Conceded",
           x = "Match Date", y = "Goals") +
      scale_color_manual(values = c("Goals Scored" = "red", "Goals Conceded" = "blue"))
  })
  
  # Display match details
  output$matchTable <- renderTable({
    filtered_matches()[, c("Date", "Home", "Away", "HomeGoals", "AwayGoals", "Result")]
  })
}

# Run the app
shinyApp(ui, server)