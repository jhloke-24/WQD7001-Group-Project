library(shiny)
library(tidyverse)
library(DT)
library(data.table)
library(shinyalert)
library(shinythemes)
library(readr)

testdata <- read_csv("C:/Users/User/Documents/part1.csv")
testdata$config_split <- strsplit(testdata$ingredients,split = ",")
config <- unique(unlist(strsplit(as.character(testdata$ingredients), ",")))

side_width = 4
ui <- navbarPage(
  "Clear your Fridge",
  tabPanel("Find your Recipe",
          sidebarLayout(
          position = "left",
          sidebarPanel(width = side_width,
          h3("Filter Recipes"),
          selectizeInput('e1', '1.Select Ingredients', choices = config, multiple = TRUE),
          hr(),       
          br(), 
          selectizeInput('e2', '2. Select time range', choices = var, multiple = FALSE),
          sliderInput('e3','3.Select Calories Level',min=0,max=450000,value=0,step=NULL),
          sliderInput('e4','4.Select Carbohydrates Level',min=0,max=37000,value=0,step=NULL)),
          mainPanel(
          width = 12 - side_width,
          DT::dataTableOutput("mytable")
                         )
           )
)
  
  
  # tags$style(type = "text/css", "body {padding-top: 175px;}"),
  # theme = shinytheme("cosmo"),
  # position = "fixed-top"
  
)

server<-function(input,output){
  output$mytable <- renderDataTable({
    
    if(isTruthy(input$e1))
    {result <- filter(testdata,all(input$e1 %in% ingredients))
    r2<- result %>% ungroup %>% select(Recipe = name,
                                       Ingredient = ingredients,
                                       `Preparation Time` = hours)
    }else {
      r2<- testdata %>% ungroup %>% select(Recipe = name,
                                           Ingredient = ingredients,
                                           `Preparation Time` = hours)
    }
    print(r2)
    
  }
  ,escape = FALSE,extensions = 'Responsive',options = list(pageLength = 10, autoWidth = TRUE,
                                                           dom  = 'tip',columnDefs = list(list(
                                                             targets = 1,
                                                             render = JS(
                                                               "function(data, type, row, meta) {",
                                                               "return type === 'display' && data.length > 125 ?",
                                                               "'<span title=\"' + data + '\">' + data.substr(0, 125) + '...</span>' : data;",
                                                               "}")
                                                           ))), callback = JS('table.page(3).draw(false);'),
  rownames= FALSE)
  
}
shinyApp(ui=ui,server=server)
