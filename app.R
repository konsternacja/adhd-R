# title: ADHD and MSI Dashboard with R shiny
# author: konsternacja
# output: html_document

# LIBRARIES -->
require(shiny)
require(readxl)
require(ggplot2)
require(GGally)
require(cowplot)
require(dplyr)
require(corrplot)

# data -->
data <- read_excel("data/DWTdata_analysis.xlsx")
data <- data %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))
res <- cor(data[,-1])
group_labels <- c("ASD", "Control", "ADHD")

# UI -->
ui <- fluidPage(
  titlePanel("Data analysis"),
  
  navbarPage("Tabs",
             
             tabPanel("Raw data",
                      dataTableOutput("raw_data_table")
             ),
             
             tabPanel("Correlation",
                      plotOutput("correlation_plot")
             ),
             
             tabPanel("Gender statistics",
                      radioButtons("gender", "Choose gender:", choices = c("0", "1")),
                      verbatimTextOutput("gender_stats")
             ),
             
             tabPanel("Gender",
                      plotOutput("gender")
             ),
             
             tabPanel("Read Me",
                      includeMarkdown("data/readme.md")
             )
  )
)

# Server -->
server <- function(input, output) {
  
  # Raw data
  output$raw_data_table <- renderDataTable({
    data
  })
  
  # Correlation
  output$correlation_plot <- renderPlot({
    corrplot(res, type = "upper", order = "hclust", 
             tl.col = "black", tl.srt = 45)
  })
  
  # Gender statistics
  output$gender_stats <- renderPrint({
    filtered_data <- subset(data, gender == input$gender)
    summary(filtered_data)
  })
  
  #clinical groups vs gender
  output$gender <- renderPlot({
    ggplot(data, aes(x = factor(group), fill = factor(gender))) +
      geom_bar(position = "dodge") +
      labs(x = "Group", y = "People", fill = "Gender") +
      scale_fill_manual(values = c("blue", "pink"), labels = c("Male", "Female")) +
      scale_x_discrete(labels = group_labels) +
      theme_minimal()
})
}

# run the app -->
shinyApp(ui, server)