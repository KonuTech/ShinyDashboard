#install.packages('rsconnect')
library(rsconnect)
#install.packages("shiny")
library(shiny)
#install.packages("shinyjs")
library(shinyjs)
#install.packages("V8")
library(V8)

shinyApp(
#runApp(shinyApp(
  
ui = fluidPage(
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
      textInput("text", "Text", ""),
      actionButton("refresh","Odswiez"),

      fileInput(
        inputId = "fileInput",
        label = "Znajdz lokalizacje pliku CSV:",
        multiple = FALSE, 
        accept = NULL,
        width = NULL,
        buttonLabel = "Browse File" 
      ),
      
      textInput(
        inputId = "textInput1",
        label = "Podaj lokalizacje pliku CSV:",
        value = "Podaj lokalizacje pliku CSV:"
      ),
      
      verbatimTextOutput(
        "textInput1"
      ),
      
      actionButton(
        inputId = "actionButton1",
        label = "Skopiuj plik CSV",
        icon = NULL,
        width = NULL
      ),
      
      textInput(
        inputId = "textInput2",
        label = "Podaj lokalizacje do zapisu pliku CSV:",
        value = "Podaj lokalizacje do zapisu pliku CSV:"
      ),
      
      verbatimTextOutput(
        "textInput2"
      ),
      
      actionButton(
        inputId = "actionButton2",
        label = "Zapisz plik CSV",
        icon = NULL,
        width = NULL
      ),
      
      sliderInput(
        inputId = "num",
        label = "Liczba losowan",
        value = 25, min = 1, max = 100
      ),
      
      plotOutput(
        "plot1",
        click = "plot_click"
      )

),
      
server = function(input, output, session) {
  observeEvent(input$refresh, {
    shinyjs::js$refresh()  
  })
  
  output$plot1 = renderPlot({
    hist(rnorm(input$num))
  }) 
  
  output$value = renderText({
    input$textInput1
  })
  
  output$value = renderText({
    input$textInput2
  })
}
)

#shinyApp(ui = ui, server = server)