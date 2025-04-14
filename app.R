library(pacman)
p_load('shiny','ggplot2','dplyr','tidyr','readxl','shinydashboard','DT','reshape2','rworldxtra','rworldmap','readr')

# Load the data
scores <- read_csv("data/full_womentoring_scoring_matrix.csv")

# Extract country info from the pairing sheet
mentor_raw <- read_excel("data/womentoring_pairing_sheet.xlsx", sheet = "womentors_anon")
mentee_raw <- read_excel("data/womentoring_pairing_sheet.xlsx", sheet = "womentees_anon")

# Extract and clean relevant columns
womentors <- mentor_raw %>%
  select(ID, Country = `Country of residence`) %>%
  mutate(Role = "Womentor")

womentees <- mentee_raw %>%
  select(ID, Country = `Country of residence`) %>%
  mutate(Role = "Womentee")

# Combine both for mapping
womentoring_locations <- bind_rows(womentors, womentees) %>%
  mutate(Country = trimws(Country))

# UI
ui <- fluidPage(
  titlePanel("WiMS Womentoring Match Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Detailed Table'",
        selectInput("mentee", "Select Mentee:", choices = c("All", sort(unique(scores$mentee))))
      ),
      conditionalPanel(
        condition = "input.tabs == 'Metric Distribution'",
        selectInput("metric", "Select Score Metric:", choices = c("score"))
      ),
      conditionalPanel(
        condition = "input.tabs == 'Map of Participants'",
        selectInput("map_view", "Select Group to Display:", choices = c("Womentors", "Womentees", "Both"))
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs",
                  # tabPanel("Top Womentors", value = "Top Womentors", plotOutput("topWomentorsPlot")),
                  tabPanel("Match Heatmap", value = "Match Heatmap", plotOutput("heatmapPlot")),
                  tabPanel("Metric Distribution", value = "Metric Distribution", plotOutput("distributionPlot")),
                  tabPanel("Detailed Table", value = "Detailed Table", DTOutput("scoreTable")),
                  tabPanel("Map of Participants", value = "Map of Participants", plotOutput("mapPlot"))
      )
    )
  )
)

server <- function(input, output) {
  
  # output$topWomentorsPlot <- renderPlot({
  #   scores %>%
  #     group_by(mentor) %>%
  #     summarise(top_scores = sum(score >= 0.8)) %>%
  #     arrange(desc(top_scores)) %>%
  #     ggplot(aes(x = reorder(mentor, top_scores), y = top_scores)) +
  #     geom_col(fill = "darkorchid") +
  #     labs(x = "Womentor", y = "# of High Matches (Score >= 0.8)", title = "Top Womentors by High-Scoring Matches") +
  #     coord_flip() +
  #     theme_minimal()
  # })
  
  output$heatmapPlot <- renderPlot({
    heat_data <- scores %>%
      select(mentor, mentee, score) %>%
      pivot_wider(names_from = mentee, values_from = score, values_fill = 0)
    
    heat_matrix <- as.matrix(heat_data[,-1])
    rownames(heat_matrix) <- heat_data$mentor
    
    melted <- melt(heat_matrix)
    colnames(melted) <- c("Mentor", "Mentee", "Score")
    
    ggplot(melted, aes(x = Mentee, y = as.factor(Mentor), fill = Score)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "darkblue") +
      labs(title = "Heatmap of Match Scores", x = "Womentee", y = "Womentor") +
      theme_minimal()
  })
  
  output$distributionPlot <- renderPlot({
    ggplot(scores, aes_string(x = input$metric)) +
      geom_histogram(binwidth = 0.01, fill = "steelblue", color = "white") +
      labs(title = paste("Distribution of", input$metric, "Scores"), x = input$metric, y = "Count") +
      theme_minimal()
  })
  
  output$scoreTable <- renderDT({
    data <- if (input$mentee == "All") scores else filter(scores, mentee == input$mentee)
    datatable(data, options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$mapPlot <- renderPlot({
    fix_country <- function(name) {
      name <- trimws(name)
      name <- gsub("Réunion|La Réunion|Mayotte|France", "Reunion", name)
      name <- gsub("United States", "USA", name)
      name <- gsub("United Kingdom", "UK", name)
      name
    }
    
    country_data <- womentoring_locations %>% mutate(Country = fix_country(Country))
    
    if (input$map_view == "Womentors") {
      filtered <- filter(country_data, Role == "Womentor")
    } else if (input$map_view == "Womentees") {
      filtered <- filter(country_data, Role == "Womentee")
    } else {
      filtered <- country_data
    }
    
    country_counts <- filtered %>% count(Country, name = "count") %>% rename(country = Country)
    map_data <- joinCountryData2Map(country_counts, joinCode = "NAME", nameJoinColumn = "country", verbose = FALSE)
    selected_countries <- map_data[!is.na(map_data@data$count), ]
    bounds <- bbox(selected_countries)
    
    mapCountryData(map_data,
                   nameColumnToPlot = "count",
                   catMethod = "fixedWidth", numCats = 5,
                   colourPalette = "heat",
                   mapTitle = paste("Distribution of", input$map_view),
                   addLegend = TRUE,
                   xlim = bounds[1, ], ylim = bounds[2, ])
  })
}

shinyApp(ui, server)
