#install.packages('rsconnect')
library(rsconnect)
#install.packages("shiny")
library(shiny)
#install.packages("shinyjs")
library(shinyjs)
#install.packages("devtools")
library(devtools)
#install.packages("V8")
library(V8)
#install.packages("curl")
#library(curl)
#install.packages("shinydashboard")
library(shinydashboard)
#install.packages("stringi")
library(stringi)
#install.packages("DT")
library(DT)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
#install.packages("gapminder")
library(gapminder)
#install.packages("plotly")
library(plotly)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("wordcloud2")
library(wordcloud2)

gapminder = gapminder::gapminder

findemails <- function (x) {
  
  stopifnot (is.character(x))
  
  y <- unlist(stri_extract_all_regex(
    x,
    "([A-Za-z0-9_.!#$%&'*+-/=?^`{|}~]+)@([\\da-z.-]+)\\.([a-z.]{2,6})", simplify =FALSE
  ))
  
  
  #. not the first or last, nor appears consecutively
  
  y <- stri_replace_all_regex(y,"^([.])+", "")
  y <- stri_replace_all_regex(y,"([.])+$", "")
  y <- y[!stri_detect_regex(y,"[.]{2,}")]
  y <- y[!stri_detect_regex(y,"@[.]")]
  y <- y[!stri_detect_regex(y,"[.]@")]
  
  
  #lengths 
  
  y <- unlist(stri_extract_all_regex(y,"(^.{1,64})@(.{1,253})", simplify =FALSE))
  y <- y[!is.na(y)]
  y <- y[stri_length(y)<=254]
  y
  
}



jscode = "shinyjs.refresh = function() { history.go(0); }"

shinyApp(
  
  ui = dashboardPage(
    
    dashboardHeader(
      title = "Dashboard Demo"
    ),
    
    dashboardSidebar(
      
      sidebarMenu(
        menuItem(
          text = "Czysciciel Adresow E-mail",
          tabName = "czyscicielAdresow 3000",
          icon = icon("envelope"),
          menuSubItem(
            text = "Input",
            tabName = "input1"
          ),
          menuSubItem(
            text = "Output",
            tabName = "output1"
            )
        ),
        tags$head(tags$script(HTML('$(document).ready(function() {$(".treeview-menu").css("display", "block");})'))),
        menuItem(
          text = "Exploratory Data Analysis",
          icon = icon("th"),
          tabName = "eda",
          menuSubItem(
            text = "Input",
            tabName = "input2"
          ),
          menuSubItem(
            text = "Output",
            tabName = "output2"
          )
        ),
        menuItem(
          text = "Machine Learning",
          icon = icon("brain"),
          tabName = "machineLearning",
          menuSubItem(
            text = "Input",
            tabName = "input3"
          ),
          menuSubItem(
            text = "Output",
            tabName = "output3"
          )
        )
      )
    ),
    
    dashboardBody(
      
      useShinyjs(),
      extendShinyjs(text = jscode),
      
#      shinyjs::useShinyjs(),
#      shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
      
      tabItems(
        tabItem(
          tabName = "input1",
          h1(
            "Zawartosc zakladki Input"
          ),
          actionButton(
            "refresh1","Odswiez"
          ),
          fileInput(
            inputId = "fileInput1",
            label = "Wybierz plik CSV:",
            multiple = FALSE,
            width = NULL,
            buttonLabel = "Przegladaj",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          )
        ),
        tabItem(
          tabName = "output1",
          h1(
            "Zawartosc zakladki Output"
          ),
          DT::dataTableOutput(
            "table1"
          ),
          DT::dataTableOutput(
            "table11"
          ),
          wordcloud2Output(
            outputId = "cloud21"
          ),
          downloadButton(
            "downloadData1",
            "Download1"
          )
        ),
        tabItem(
          tabName = "input2",
          h1("Zawartosc zakladaki Exploratory Data Analysis Input"),
          actionButton(
            "refresh2","Odswiez"
          ),
          fileInput(
            inputId = "fileInput2",
            label = "Wybierz plik CSV:",
            multiple = FALSE,
            width = NULL,
            buttonLabel = "Przegladaj",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          )
        ),
        tabItem(
          tabName = "output2",
          h1(
            "Zawartosc zakladaki Exploratory Data Analysis Output"
          ),
          sidebarLayout(
            sidebarPanel(
              textInput("title", "Title", "GDP vs life exp"),
              numericInput("size", "Point size", 1, 1),
              checkboxInput("fit", "Add line of best fit", FALSE),
              radioButtons("color", "Point color", choices = c("blue", "red", "green", "black")),
              selectInput("continents", "Continents",
#                          choices = c("All", levels(gapminder$continent)),
                          choices = levels(gapminder$continent),
                          multiple = TRUE,
                          selected = c("Europe")),
              sliderInput("years", "Years",
                          min(gapminder$year), max(gapminder$year),
                          value = c(1977, 2002))
            ),
            mainPanel(
              plotlyOutput("plot21", 600, 600)
            )
          ),
          downloadButton(
            "downloadData2",
            "Download2"
          )
        ),
        tabItem(
          tabName = "input3",
          h1("Zawartosc zakladaki Machine Learning Input"),
          actionButton(
            "refresh3","Odswiez"
          ),
          fileInput(
            inputId = "fileInput3",
            label = "Wybierz plik CSV:",
            multiple = FALSE,
            width = NULL,
            buttonLabel = "Przegladaj",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          )
        ),
        tabItem(
          tabName = "output3",
          h1(
            "Zawartosc zakladaki Machine Learning Input"
          ),
          DT::dataTableOutput(
            "table3"
          ),
          downloadButton(
            "downloadData3",
            "Download3"
          )
        )
      )
      
      
      
    )
    
  ),
  

#SERVER
#SERVER
#SERVER

  server = function(input, output, session) {
    
    
    
    datasetInput1 = reactive({
      inFile1 = input$fileInput1
      
      if (is.null(inFile1))
        return(NULL)
      
      emails = read.csv(file= inFile1$datapath[1], sep=",", header = FALSE, quote = "\"", fill=TRUE, comment.char = "", stringsAsFactors = FALSE)
      
      przeszukiwaneMaile = sapply(paste(emails), findemails)
      
      
      for (i in 1:length(przeszukiwaneMaile)) {
        if (przeszukiwaneMaile[i] != "character(0)") {
          przeszukiwaneMaile[i] = paste0(przeszukiwaneMaile[i], ",OK")
        }
        else przeszukiwaneMaile[i]  = paste0(names(przeszukiwaneMaile[i]), ",not OK")
      }
      
      maileMatrix = do.call(rbind,strsplit(as.character(przeszukiwaneMaile),","))
      
      mailedf0 = as.data.frame(maileMatrix, fix.empty.names = FALSE, )
      columns = c("V2", "V1")
      mailedf1 = mailedf0[columns]
      mailedf2 =mailedf0[columns]
      names(mailedf2)[names(mailedf2) == "V3"] <- "V1"
      names(mailedf2)[names(mailedf2) == "V4"] <- "V2"
      
      mailedf = rbind(mailedf1,mailedf2)
      emailsCleaned = mailedf[!duplicated(mailedf),]
    })
    
    output$table1 = DT::renderDataTable({
      datasetInput1()
    })
    
    

    datasetInput2 = reactive({
      emailsCleaned = datasetInput1()
      emailsCleaned %>% group_by(emailsCleaned$V2) %>% summarise(Count = n())
    })
    
    output$table11 = DT::renderDataTable({
      datasetInput2()
    })
    
    
    
    
    output$cloud21 = renderWordcloud2({
      # Create a word cloud object
       wordcloud2(datasetInput2())
    })
    
    
    
    
    output$plot21 <- renderPlotly({
      # Convert the existing ggplot2 to a plotly plot
      ggplotly({
        data = subset(
          gapminder,
          continent %in% input$continents & year >= input$years[1] & year <= input$years[2]
        )
        
#        if (input$continents != "All") {
          data = subset(data, continent == input$continents)
#        }
        
        data
        
        p <- ggplot(data, aes(gdpPercap, lifeExp)) +
          geom_point(size = input$size, col = input$color) +
          scale_x_log10() +
          ggtitle(input$title)
        
#        if (input$fit) {
#          p <- p + geom_smooth(method = "lm")
#        }
#        
#        p
      
      })
    })
    
    datasetInput3 = reactive({
      inFile3 = input$fileInput3
      
      if (is.null(inFile3))
        return(NULL)
      
      counterpartyRisk = read.csv(file= inFile3$datapath[1], sep=";", header = TRUE, quote = "\"", fill=TRUE, comment.char = "", stringsAsFactors = FALSE)
    })

    output$table3 = DT::renderDataTable({
      datasetInput3()
    })    
    
    
    
    output$downloadData1 = downloadHandler(
      filename = function() {
        paste("dataCleaned", ".csv", sep = "")
      },
      
      content = function(file) {
        write.csv(datasetInput1(), file, row.names = FALSE)
      }
    )
    
    
    
    output$downloadData3 = downloadHandler(
      filename = function() {
        paste("counterpartyRisk", ".csv", sep = "")
      },
      
      content = function(file) {
        write.csv(datasetInput3(), file, row.names = FALSE)
      }
    )
    
    
    
    observeEvent(c(input$refresh1, input$refresh2, input$refresh3), {
      shinyjs::js$refresh()  
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    
  }
)

#shinyApp(ui = ui, server = server)

#NAIVE BAYES
#LOGISTIC REGRESSION
#DECISION TREE