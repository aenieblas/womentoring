library(pacman)
p_load('shiny','ggplot2','dplyr','tidyr','readxl','shinydashboard','DT','reshape2','rworldxtra','rworldmap','readr')

# Load the data
scores <- read_csv("data/full_womentoring_scoring_matrix.csv")

# Extract country info from the pairing sheet
mentor_raw <- read_excel("data/womentoring_pairing_sheet.xlsx", sheet = "womentors")
mentee_raw <- read_excel("data/womentoring_pairing_sheet.xlsx", sheet = "womentees")
summary_data <- read.csv("data/womentoring_top3_summary_table.csv")


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
  
  tags$head(tags$style(HTML("
  table.dataTable thead th.wrap-header {
    white-space: normal !important;
    word-wrap: break-word;
    overflow-wrap: break-word;
    max-width: 11summary(jaccard_matrix)
hist(jaccard_matrix)
0px;
    text-align: center;
    vertical-align: middle;
  }
"))),
  
  titlePanel("WiMS Womentoring Match Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
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
                  tabPanel("Top 3 Summary Table", value = "Top 3 Summary Table", DTOutput("summaryTable")),
                  tabPanel("Detailed Table", value = "Detailed Table", DTOutput("scoreTable")),
                  tabPanel("Map of Participants", value = "Map of Participants", plotOutput("mapPlot")),
                  tabPanel("Common Challenges", plotOutput("challengeFreqPlot")),
                  tabPanel("Desired Skills", plotOutput("skillsComparePlot")),
                  tabPanel("Meeting Format", plotOutput("formatComparePlot")),
                  tabPanel("Meeting Frequency", plotOutput("frequencyComparePlot")),
                  tabPanel("Sector Preference", plotOutput("sectorComparePlot"))
                  
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
  
  # output$heatmapPlot <- renderPlot({
  #   heat_data <- scores %>%
  #     select(mentor, mentee, score) %>%
  #     pivot_wider(names_from = mentee, values_from = score, values_fill = 0)
  #   
  #   heat_matrix <- as.matrix(heat_data[,-1])
  #   rownames(heat_matrix) <- heat_data$mentor
  #   
  #   melted <- melt(heat_matrix)
  #   colnames(melted) <- c("Mentor", "Mentee", "Score")
  #   
  #   ggplot(melted, aes(x = Mentee, y = as.factor(Mentor), fill = Score)) +
  #     geom_tile(color = "white") +
  #     # scale_fill_gradient(low = "white", high = "darkblue") +
  #     scale_fill_gradientn(colors= c("#FFE625", "#2A9D8F", "#264653")) +
  #     labs(title = "Heatmap of Match Scores", x = "Womentee", y = "Womentor") +
  #         theme_minimal()+
  #     
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # })
  output$frequencyComparePlot <- renderPlot({
    mentor_freq <- mentors_clean %>%
      select(frequency) %>%
      separate_rows(frequency, sep = ",\\s*") %>%
      count(frequency, name = "count") %>%
      mutate(role = "Womentor")
    
    mentee_freq <- mentees_clean %>%
      select(frequency) %>%
      separate_rows(frequency, sep = ",\\s*") %>%
      count(frequency, name = "count") %>%
      mutate(role = "Womentee")
    
    combined_freq <- bind_rows(mentor_freq, mentee_freq) %>%
      group_by(role) %>%
      mutate(prop = count / sum(count)) %>%
      ungroup()
    
    ggplot(combined_freq, aes(x = "", y = prop, fill = frequency)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      facet_wrap(~ role, ncol = 2) +
      scale_fill_manual(values = setNames(
        colorRampPalette(c("#FFE625", "#2A9D8F", "#264653"))(length(unique(combined_freq$frequency))),
        unique(combined_freq$frequency)
      )) +
      theme_void(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 18, face = "bold"),
        strip.text = element_text(size = 16)
      ) +
      labs(title = "Meeting Frequency", fill = "Frequency")
  })
  
  output$challengeFreqPlot <- renderPlot({
    mentees_clean %>%
      select(challenges) %>%
      filter(!is.na(challenges)) %>%
      separate_rows(challenges, sep = ",\\s*") %>%
      count(challenges, sort = TRUE) %>%
      ggplot(aes(x = reorder(challenges, n), y = n)) +
      geom_col(fill = "#2A9D8F") +
      coord_flip() +
      labs(title = "Frequency of Challenges faced by Womentees", x = "Challenge", y = "Count") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)
      )
    
  })
  
  output$skillsComparePlot <- renderPlot({
    mentor_skills <- mentors_clean %>%
      select(skills) %>%
      separate_rows(skills, sep = ",\\s*") %>%
      mutate(skills = str_trim(tolower(skills))) %>%
      count(skills, name = "count") %>%
      mutate(role = "Womentor")
    
    mentee_skills <- mentees_clean %>%
      select(skills) %>%
      separate_rows(skills, sep = ",\\s*") %>%
      mutate(skills = str_trim(tolower(skills))) %>%
      count(skills, name = "count") %>%
      mutate(role = "Womentee")
    
    
    all_skills <- union(mentor_skills$skills, mentee_skills$skills)
    combined <- expand.grid(skills = all_skills, role = c("Womentor", "Womentee")) %>%
      left_join(bind_rows(mentor_skills, mentee_skills), by = c("skills", "role")) %>%
      mutate(count = ifelse(is.na(count), 0, count)) %>%
      group_by(role) %>%
      mutate(prop = count / sum(count)) %>%
      ungroup()
    
    ggplot(combined, aes(x = "", y = prop, fill = skills)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      facet_wrap(~ role, ncol = 2) +
      scale_fill_manual(values = setNames(
        colorRampPalette(c("#FFE625", "#2A9D8F", "#264653"))(length(unique(combined$skills))),
        unique(combined$skills)
      )) +
    theme_void(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 18, face = "bold"),
        strip.text = element_text(size = 16)
      ) +
      labs(title = "Womentees desired skills / Skills that interest womentors", fill = "Skill")
  })
  
  
  
  
  # output$formatFreqComparePlot <- renderPlot({
  #   mentor_format <- mentors_clean %>%
  #     select(format) %>%
  #     separate_rows(format, sep = ",\\s*") %>%
  #     count(format, name = "mentor_count")
  #   
  #   mentee_format <- mentees_clean %>%
  #     select(format) %>%
  #     separate_rows(format, sep = ",\\s*") %>%
  #     count(format, name = "mentee_count")
  #   
  #   combined_format <- full_join(mentor_format, mentee_format, by = "format") %>%
  #     pivot_longer(cols = c(mentor_count, mentee_count), names_to = "role", values_to = "count") %>%
  #     mutate(type = "Format")
  #   
  #   mentor_freq <- mentors_clean %>%
  #     select(frequency) %>%
  #     separate_rows(frequency, sep = ",\\s*") %>%
  #     count(frequency, name = "mentor_count")
  #   
  #   mentee_freq <- mentees_clean %>%
  #     select(frequency) %>%
  #     separate_rows(frequency, sep = ",\\s*") %>%
  #     count(frequency, name = "mentee_count")
  #   
  #   combined_freq <- full_join(mentor_freq, mentee_freq, by = "frequency") %>%
  #     pivot_longer(cols = c(mentor_count, mentee_count), names_to = "role", values_to = "count") %>%
  #     rename(format = frequency) %>%
  #     mutate(type = "Frequency")
  #   
  #   bind_rows(combined_format, combined_freq) %>%
  #     ggplot(aes(x = reorder(format, count), y = count, fill = role)) +
  #     geom_col(position = "dodge") +
  #     facet_wrap(~ type, scales = "free_y") +
  #     scale_fill_manual(values = c("mentor_count" = "#264653", "mentee_count" = "#FFE625")) +
  #     coord_flip() +
  #     labs(title = "Womentor vs Womentee: Format & Frequency Preferences", x = "", y = "Count", fill = "Role") +
  #     theme_minimal(base_size = 14) +
  #     theme(
  #       axis.text = element_text(size = 14),
  #       axis.title = element_text(size = 16),
  #       plot.title = element_text(size = 18, face = "bold"),
  #       legend.text = element_text(size = 12),
  #       legend.title = element_text(size = 14)
  #     )
  #   
  # })
  # 
  # output$sectorComparePlot <- renderPlot({
  #   mentor_sector <- mentors_clean %>%
  #     select(sector) %>%
  #     separate_rows(sector, sep = ",\\s*") %>%
  #     count(sector, name = "mentor_count")
  #   
  #   mentee_sector <- mentees_clean %>%
  #     select(sector) %>%
  #     separate_rows(sector, sep = ",\\s*") %>%
  #     count(sector, name = "mentee_count")
  #   
  #   full_join(mentor_sector, mentee_sector, by = "sector") %>%
  #     pivot_longer(cols = c(mentor_count, mentee_count), names_to = "role", values_to = "count") %>%
  #     ggplot(aes(x = reorder(sector, count), y = count, fill = role)) +
  #     geom_col(position = "dodge") +
  #     scale_fill_manual(values = c("mentor_count" = "#264653", "mentee_count" = "#FFE625")) +
  #         coord_flip() +
  #     labs(title = "Womentor Sectors vs Womentee Sector Preferences", x = "Sector", y = "Count", fill = "Role") +
  #     theme_minimal(base_size = 14) +
  #     theme(
  #       axis.text = element_text(size = 14),
  #       axis.title = element_text(size = 16),
  #       plot.title = element_text(size = 18, face = "bold"),
  #       legend.text = element_text(size = 12),
  #       legend.title = element_text(size = 14)
  #     )
  #   
  # })
  
  output$formatComparePlot <- renderPlot({
    # Format data
    mentor_format <- mentors_clean %>%
      select(format) %>%
      separate_rows(format, sep = ",\\s*") %>%
      count(format, name = "count") %>%
      mutate(role = "Womentor")
    
    mentee_format <- mentees_clean %>%
      select(format) %>%
      separate_rows(format, sep = ",\\s*") %>%
      count(format, name = "count") %>%
      mutate(role = "Womentee")
    
    combined_format <- bind_rows(mentor_format, mentee_format) %>%
      group_by(role) %>%
      mutate(prop = count / sum(count)) %>%
      ungroup()
    
    ggplot(combined_format, aes(x = "", y = prop, fill = format)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      facet_wrap(~ role, ncol = 2) +
      scale_fill_manual(values = setNames(
        colorRampPalette(c("#FFE625", "#2A9D8F", "#264653"))(length(unique(combined_format$format))),
        unique(combined_format$format)
      )) +
      theme_void(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 18, face = "bold"),
        strip.text = element_text(size = 16)
      ) +
      labs(title = "Preferred Meeting Format ", fill = "Format")
  })
  
  output$sectorComparePlot <- renderPlot({
    mentor_sector <- mentors_clean %>%
      select(sector) %>%
      separate_rows(sector, sep = ",\\s*") %>%
      count(sector, name = "count") %>%
      mutate(role = "Womentor")
    
    mentee_sector <- mentees_clean %>%
      select(sector) %>%
      separate_rows(sector, sep = ",\\s*") %>%
      count(sector, name = "count") %>%
      mutate(role = "Womentee")
    
    combined_sector <- bind_rows(mentor_sector, mentee_sector) %>%
      group_by(role) %>%
      mutate(prop = count / sum(count)) %>%
      ungroup()
    
    ggplot(combined_sector, aes(x = "", y = prop, fill = sector)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      facet_wrap(~ role, ncol = 2) +
      scale_fill_manual(values = setNames(
        colorRampPalette(c("#FFE625", "#2A9D8F", "#264653"))(length(unique(combined_sector$sector))),
        unique(combined_sector$sector)
      )) +
      theme_void(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 18, face = "bold"),
        strip.text = element_text(size = 16)
      ) +
      labs(title = "Womentee Preferred, Womentor Actual Sectors", fill = "Sector")
  })
  
  output$summaryTable <- renderDT({
    datatable(
      summary_data,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        columnDefs = list(list(
          targets = "_all",
          className = "wrap-header"
        ))
      ),
      rownames = FALSE
    )
  })
  
  
  
  
  output$heatmapPlot <- renderPlot({
    # Create wide → matrix → melted data
    heat_data <- scores %>%
      select(mentor, mentee, score) %>%
      pivot_wider(names_from = mentee, values_from = score, values_fill = 0)
    
    heat_matrix <- as.matrix(heat_data[,-1])
    rownames(heat_matrix) <- heat_data$mentor
    
    melted <- melt(heat_matrix)
    colnames(melted) <- c("Womentor", "Womentee", "Score")
    
    # Flag top 3 scores per Womentor
    top3 <- melted %>%
      group_by(Womentor) %>%
      slice_max(order_by = Score, n = 3, with_ties = FALSE) %>%
      mutate(Top = TRUE)
    
    melted <- left_join(melted, top3 %>% select(Womentor, Womentee, Top), by = c("Womentor", "Womentee"))
    melted$Top[is.na(melted$Top)] <- FALSE
    
    # Plot with overlay
    ggplot(melted, aes(x = Womentee, y = as.factor(Womentor), fill = Score)) +
      geom_tile(color = "white") +
      geom_tile(data = filter(melted, Top), color = "red", size = 0.8, fill = NA) +
      scale_fill_gradientn(colors= c("#FFE625", "#2A9D8F", "#264653")) +
      labs(title = "Heatmap of Match Scores", x = "Womentee", y = "Womentor", fill = "Score") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)
      )
  })
  
  output$distributionPlot <- renderPlot({
    ggplot(scores, aes_string(x = input$metric)) +
      geom_histogram(binwidth = 0.01, fill = "steelblue", color = "white") +
      labs(title = paste("Distribution of", input$metric, "Scores"), x = input$metric, y = "Count") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)
      )
    
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
                   colourPalette = c("#FFE625", "#2A9D8F", "#264653"),
                   mapTitle = paste("Distribution of", input$map_view),
                   addLegend = TRUE,
                   xlim = bounds[1, ], ylim = bounds[2, ])
  })
}

shinyApp(ui, server)
