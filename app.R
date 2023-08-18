# LIBRARIES -->
require(shiny)
require(readxl)
require(ggplot2)
require(GGally)
require(cowplot)
require(dplyr)

# DEFINING UI -->
ui <- fluidPage(
  titlePanel("ADHD and MSI data visualisation"),
  theme = bslib::bs_theme(version = 4, bootswatch = 'minty'),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h1('Explore the dataset')
    ),
    mainPanel(
      plotOutput("plot")  # Change "plot" instead of "wykres"
    )
  )
)

# SERVER -->
server <- function(input, output) {
  
  # Wczytanie danych z pliku Excel
  clean_df <- read_excel(path = "data/ADHD_MC.xlsx")
  clean_df[clean_df == 999] <- NA
  clean_df <- clean_df %>%
    mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))
  
  # Tworzenie funkcji do generowania wykresu
  output$plot <- renderPlot({  # Change "plot" instead of "wykres"
    pastel_colors <- c("#A6CEE3", "#B2DF8A")
    labels <- c("Women", "Men")
    ggplot(clean_df, aes(x = factor(SEXO), fill = factor(SEXO))) +
      geom_bar() +
      labs(title = "Ratio between men and women", x = "Sex", y = "Number") +
      scale_x_discrete(labels = c("Women", "Men")) +
      scale_fill_manual(values = pastel_colors, labels = labels) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
}

# Running the app
# Uruchamianie aplikacji Shiny
shinyApp(ui, server)