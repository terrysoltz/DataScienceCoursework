setwd("C:/Users/soltz/OneDrive/Documents/TechAcademy2020/Shiny/Iris_Dash")

# Load packages ----
library(shiny)
library(lattice)
library(dplyr)
library(hexbin)


# Load Iris Data ----
data(iris)
head(iris)

# User interface ----
ui <- fluidPage(
  titlePanel("Iris Statistics"),

  sidebarLayout(
    sidebarPanel(
      helpText("Select Which Species you wish to Compare"),
      checkboxGroupInput("speciesGroup", 
                         "",
                         choices = list("setosa", 
                                        "versicolor", 
                                        "virginica"),
                         selected = c("setosa", "versicolor", "virginica")),
      br(),
      textOutput("warning")
    ),

    mainPanel(
      plotOutput("bar"),
      plotOutput("box"),
      plotOutput("hexbin")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Check selected species and give warning
  output$warning <- reactive(
    validate(
      need(input$speciesGroup != "", "You must select at least one species!")
    )
  )
  
  # Filter by selected species
  selected <- reactive({
    iris %>%
      select(Petal.Length, Petal.Width, Species) %>%
      filter(Species %in% input$speciesGroup)
  })
  
  # Get mean values for bar chart
  barData <- reactive({
    selected() %>%
      group_by(Species) %>%
      summarize(Avg.PetLen = mean(Petal.Length), Avg.PetWid = mean(Petal.Width)) %>%
      as.data.frame()
  })
  
  # Bar Chart
  output$bar <- renderPlot({
    
    validate(
      need(input$speciesGroup != "", "")
    )
    
    barchart(
      x = Avg.PetLen + Avg.PetWid ~ Species,
      data = barData(),
      stack = F,
      horizontal = F,
      main = "Petal Length and Width by Species",
      xlab = "Species",
      ylab = "Length/Width (cm)",
      auto.key = list(
        x = 0.05,
        y = 0.95,
        title = "Petals",
        text = c("Length", "Width")
      )
    )
  })
  
  # Box Plot
  output$box <- renderPlot({
    
    validate(
      need(input$speciesGroup != "", "")
    )
    
    bwplot(
      x = Petal.Length ~ Species,
      data = selected(),
      main = "Distribution of Petal Length by Species",
      xlab = "Species",
      ylab = "Length (cm)"
    )
  })
  
  # Hexbin Plot
  output$hexbin <- renderPlot({
    
    validate(
      need(input$speciesGroup != "", "")
    )
    
    hexbinplot(
      x = Petal.Length ~ Petal.Width,
      data = selected(),
      xbins = 25,
      main = "Relationship of Petal Length to Petal Width",
      xlab = "Petal Width (cm)",
      ylab = "Petal Length (cm)"
    )
  })
  
}

# Run the app
shinyApp(ui, server)


