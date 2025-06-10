library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)  # For interactive plots
library(tidyr)   # For the replace_na function
library(DT)      # For interactive tables


reviews <- read.csv("honeycomb_test_data.csv", stringsAsFactors = FALSE)


reviews$self_review <- as.logical(reviews$self_review)
reviews$completed_at <- as.Date(reviews$completed_at)


reviews$relationship <- ifelse(is.na(reviews$relationship), "Self", reviews$relationship)


ui <- fluidPage(
  titlePanel("Employee Review Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("population", "Filter by Population (reviewee):",
                         choices = unique(reviews$u.population_id)),
      uiOutput("user_selector"),
      uiOutput("relationship_selector")
    ),
    
    mainPanel(
      textOutput("hover_message"),  
      
      tabsetPanel(
        tabPanel("Plot", plotlyOutput("scorePlot")),  
        tabPanel("Data Table", DTOutput("scoreTable"))
      ),
      
      hr(),  # horizontal line separator
      
      h3("Summary of Findings"),
      
      h4("1. Self vs. Others Ratings"),
      p("Users consistently rate themselves higher than they are rated by their peers and managers, indicating a possible perception gap."),
      
      h4("2. Feedback-related Behaviour"),
      p("The behaviour ", em("\"Listens to feedback about their impact on others\""), 
        " received low ratings across many users, suggesting this is a common area for improvement."),
      
      h4("3. Duplicate Reviews and Score Aggregation"),
      p("Some users have participated in multiple review cycles, resulting in duplicate responses. In these cases, scores have been aggregated to avoid duplication and present a unified view of each individual's feedback.")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive expression to filter data based on population using tidyverse
  filtered_data <- reactive({
    req(input$population)  # Ensure population filter is selected
    reviews %>%
      filter(u.population_id %in% input$population)
  })
  
  # Dynamically generate user selector based on selected population using tidyverse
  output$user_selector <- renderUI({
    req(filtered_data())  # Ensure filtered data is available
    selectInput("user", "Select Employee (reviewee):",
                choices = filtered_data() %>%
                  pull(user_id) %>%
                  unique(),
                selected = filtered_data() %>%
                  pull(user_id) %>%
                  unique() %>%
                  .[1])  # Select the first user by default
  })
  
  # Dynamically generate review sources based on selected user using tidyverse
  output$relationship_selector <- renderUI({
    req(input$user)  # Ensure user selection is available
    
    # Filter the data to get unique review sources for the selected user
    user_reviews <- filtered_data() %>%
      filter(user_id == input$user)
    
    # Get the unique review sources for the selected user
    review_sources <- user_reviews %>%
      pull(relationship) %>%
      unique()
    
    checkboxGroupInput("relationship", "Select Review Sources:",
                       choices = review_sources, selected = review_sources)
  })
  
  # Display hover suggestion message
  output$hover_message <- renderText({
    "💡 Hover over the bars in the graph to see the behaviours."
  })
  
  # Reactive function to aggregate the data for plotting and table display
  aggregated_data <- reactive({
    req(input$user, input$relationship)  # Ensure user and relationship are selected
    
    # Filter data further by selected user and review source
    df <- filtered_data() %>%
      filter(user_id == input$user,
             relationship %in% input$relationship)
    
    # Replace NAs in 'value' with 0 before aggregation and ensure 'value' is numeric
    df_aggregated <- df %>%
      mutate(value = as.numeric(replace_na(value, 0))) %>%
      group_by(behaviour_id, relationship) %>%
      summarise(aggregated_value = mean(value, na.rm = TRUE), .groups = 'drop')
    
    # Now, add the behaviour_title by joining the original dataset with the aggregated data
    df_aggregated <- df_aggregated %>%
      left_join(
        df %>% select(behaviour_id, behaviour_title) %>% distinct(),
        by = "behaviour_id"
      )
    
    df_aggregated
  })
  
  # Plot of scores by behaviour and review source (relationship)
  output$scorePlot <- renderPlotly({
    df_aggregated <- aggregated_data()  # Get the aggregated data
    
    if (nrow(df_aggregated) == 0) {
      return(NULL)  # No data to plot
    }
    
    # Create a ggplot with behaviour_id on the x-axis
    p <- ggplot(df_aggregated, aes(x = factor(behaviour_id), y = aggregated_value, fill = relationship, text = behaviour_title)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +  # Dodge bars by review source
      labs(title = paste("Average Behavioural Scores for User:", input$user),
           x = "Behaviour ID", y = "Average Score") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
    
    # Convert ggplot to plotly for interactive tooltip display
    ggplotly(p, tooltip = "text")  # `text` will show behaviour_title on hover
  })
  
  # Display aggregated data table with behaviour_title and behaviour_id
  output$scoreTable <- renderDT({
    df_aggregated <- aggregated_data()  # Get the aggregated data
    
    if (nrow(df_aggregated) == 0) {
      return(NULL)  # No data to display
    }
    
    # Display the aggregated data as an interactive table, now including behaviour_title and behaviour_id
    datatable(df_aggregated, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Show the initial population selection dialog when the app first loads
  observe({
    if (length(input$population) == 0) {
      showModal(modalDialog(
        title = "🔔 Select the Population ID to Continue",
        "Please select at least one population group from the checkbox above to continue.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Close the modal when population is selected
  observeEvent(input$population, {
    if (length(input$population) > 0) {
      removeModal()  # Hide the modal once a population is selected
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)

