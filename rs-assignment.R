getwd()

rsconnect::setAccountInfo(name='brandonlee07',
                          token='7D29AC5850A1ABEB8AB425E00C3CA973',
                          secret='BHevcEMEzBanNK4xxJ27nQN4bOE7KnS4TgPEY10E')
deployApp()

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(rsconnect)

ui <- fluidPage(
  titlePanel("Claims Data and Input Parameters"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("upload", label = "Upload your File", accept = c(".csv", ".xlsx")),
      numericInput("n", "Rows", value = 7, min = 1, step = 1),
      sliderInput("tail", label = "Tail Factor", min = 1, max = 5, value = 1.1, step = 0.05),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Claims Data", tableOutput("head")),
        tabPanel("Cumulative Paid Claims", tableOutput("cumulative_table")),
        tabPanel("Cumulative Paid Claims Graph", plotOutput("cumulative_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  params_data <- reactiveVal(data.frame(TailFactor = numeric(0)))
  data <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    validate(
      need(ext %in% c("csv", "tsv"), "Invalid file; Please upload a .csv or .tsv file")
    )
    
    switch(
      ext,
      csv = vroom::vroom(input$upload$datapath, delim = ","),
      tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
    ) 
  })
  
  cumulative_data <- reactive({
    
    claims <- data()
    tail_factor <- input$tail
    
    if (!is.null(claims) && !is.null(tail_factor)) {
      unique_years <- unique(claims$Year)
      unique_development_years <- unique(claims$Development_Year)
      
      largest_development_year <- max(unique_development_years)
      
      cumulative_matrix <- matrix(0, nrow = length(unique_years), ncol = length(unique_development_years) + 2)  # +2 for the additional column
      colnames(cumulative_matrix) <- c("Loss Year", paste("Development Year", unique_development_years), paste("Development Year", largest_development_year+1))
      
      for (i in 1:length(unique_years)) {
        loss_year <- unique_years[i]
        claims_subset <- claims[claims$Year == loss_year, ]
        deve_years <- unique(claims_subset$Development_Year)
        
        for (j in 1:length(deve_years)) {
          deve_year <- deve_years[j]
          subset <- claims_subset[claims_subset$Development_Year <= deve_year, ]
          cumulative_matrix[i, j + 1] <- sum(subset$Paid)
        }
        
        cumulative_matrix[i, 1] <- round(loss_year)
        
        
        ## For Development year 2 and 3
        cumulative_matrix[3, 3] <- round(((cumulative_matrix[1, 3] + cumulative_matrix[2, 3]) / (cumulative_matrix[1, 2] + cumulative_matrix[2, 2])) * cumulative_matrix[3, 2])
        cumulative_matrix[2, 4] <- round((cumulative_matrix[1, 4] / cumulative_matrix[1, 3]) * cumulative_matrix[2, 3])
        cumulative_matrix[3, 4] <- round((cumulative_matrix[1, 4] / cumulative_matrix[1, 3]) * cumulative_matrix[3, 3])
        
        
        ## For Development year 4
        cumulative_matrix[1, 5] <- round(cumulative_matrix[1, 4] * tail_factor)
        cumulative_matrix[2, 5] <- round(cumulative_matrix[2, 4] * tail_factor)
        cumulative_matrix[3, 5] <- round(cumulative_matrix[3, 4] * tail_factor)
      }
      
      data.frame(cumulative_matrix)
    } else {
      NULL
    }
  })
  
  output$head <- renderTable({
    head(data())
  })
  
  output$cumulative_table <- renderTable({
    cumulative_data()
  })
  
  output$cumulative_plot <- renderPlot({
    
    cumulative_matrix_data <- cumulative_data()  # Get the cumulative matrix data
    
    df <- as.data.frame(cumulative_matrix_data)
    
    data_long <- df %>%
      pivot_longer(cols = starts_with("Development.Year"),
                   names_to = "Development_Year",
                   values_to = "Cumulative_Paid_Claims")
    
    data_long$Development_Year <- as.numeric(gsub("Development.Year.", "", data_long$Development_Year))
    
    # Plot the data using ggplot
    ggplot(data = data_long, aes(x = Development_Year, y = Cumulative_Paid_Claims, color = factor(Loss.Year))) +
      geom_line() +
      geom_point() +
      labs(title = "Cumulative Paid Claims" , x = "Development Year" , y = "Cumulative Amount of Claims Paid" , color = "Loss Year") +
      scale_color_discrete(name = "Loss Year") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
}

shinyApp(ui, server)
