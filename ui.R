Data <- read.csv("C:/Users/Ashu/Documents/Newdata.csv")
Waste <- read.csv("C:/Users/Ashu/Documents/Waste_Management.csv")
library(shiny)
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Garbage Bin Data Analysis"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput('dataset', 'Choose the dataset:',
                  choices = c('Bin_Data','Waste_Management')),
      numericInput("obs", "Number of observations to view:", 10),
      checkboxInput('summary','View summary of the dataset'),
      uiOutput('response'),
      checkboxInput("pred_type", 'Wishes to pick the predictors manually?'),
      conditionalPanel(
        condition = "input.pred_type == true",
        uiOutput('predictors')
      ),
      conditionalPanel(
        condition = "input.pred_type == false"
      ),
      HTML('<hr style="color: purple;" size="20" >')
      , hr()
      
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      h4("Observations"),
      tableOutput("view"),        
      conditionalPanel(
        condition = 'input.summary==true',
        h4("Summary"),
        verbatimTextOutput("summary")
      ),
      h4("Linear regression model fit"),
      verbatimTextOutput('model'),
      plotOutput('plt')
    )
  )
))