library(shiny)
library(rpart)
library(vcdExtra)

fit.titanic <- rpart(survived ~ ., data = Titanicp)

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
      
      selectInput("sex", label = h2("Sex is"), 
                  choices = list("Male" = "male", "Female" = "female"), 
                  selected = "male"),
      
      selectInput("pclass", label = h4("Class is"), 
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
  output$prediction <- renderText({
    new_data = data.frame(
      age = input$age,
      sibsp = input$sibsp,
      sex = input$sex,
      pclass = input$pclass,
      parch = input$parch
    )
    
    titanic.predict <- predict(fit.titanic, new_data, type = "class")
    paste("In all likelihood, you ", titanic.predict, ".", sep = "")
    
  })
  output$tree <- renderPlot({
    plot(fit.titanic)   
    text(fit.titanic) 
  })
}

shinyApp(ui = ui, server = server)