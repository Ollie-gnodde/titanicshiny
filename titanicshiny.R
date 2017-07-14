library(shiny)
library(rpart)
library(vcdExtra)

ui <- fluidPage(
  titlePanel("Survival Prediction"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("age",
                  "Age: ",
                  min = 0.0,
                  max = 80,
                  step = 5,
                  value = 3.0),
      
      sliderInput("proportion",
                  "Percentage of data used: ",
                  min = 0.2,
                  max = 1,
                  step = 0.05,
                  value = 3.0),
      
      selectInput("sex", label = h2("Sex is"), 
                  choices = list("Male" = "male", "Female" = "female"), 
                  selected = "male"),
      
      selectInput("pclass", label = h3("Class is"), 
                  choices = list("1st Class" = "1st", "2nd Class" = "2nd", "3rd Class" = "3rd"), 
                  selected = "3rd"),
      
      sliderInput("sibsp",
                  "Siblings or spouses: ",
                  min = 0,
                  max = 7.0,
                  step = 1,
                  value = 3.75),
      
      sliderInput("parch",
                  "Parents or Children: ",
                  min = 0,
                  max = 7.0,
                  step = 1,
                  value = 3.75)
      
    ),
    mainPanel(
      textOutput("prediction"),
      plotOutput("tree")
    )
  )
)

server <- function(input, output) {
  
  generate_fit <- reactive({
    
      sample.data <- dplyr::sample_frac(Titanicp, input$proportion, replace = TRUE)
      
      rpart(survived ~ ., data = sample.data)  #what we return 

  })
  
  output$prediction <- renderText({
    new.data = data.frame(
      age = input$age,
      sibsp = input$sibsp,
      sex = input$sex,
      pclass = input$pclass,
      parch = input$parch
    )
    
    titanic.predict <- predict(generate_fit(), new.data, type = "class")
    paste("In all likelihood, you ", titanic.predict, ".", sep = "")
    
  })
  output$tree <- renderPlot({
    model <- generate_fit()
    plot(model)   
    text(model) 
  })
}

shinyApp(ui = ui, server = server)