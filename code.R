library(shiny)
library(tidyverse)
library(DT)
library(data.table)
library(shinyalert)
library(shinythemes)
library(readr)

testdata <- read_csv("part1.csv")
testdata <- select(testdata, -X1)
testdata$config_split <- strsplit(as.character(testdata$ingredients),split = ",")
testdata <- rowwise(testdata)
config <- unique(unlist(strsplit(as.character(testdata$ingredients), ",")))
duration <- sort(unique(unlist(strsplit(as.character(testdata$g_minutes), ","))))
calorie <- sort(unique(unlist(strsplit(as.character(testdata$g_calorie), ","))))
carbohydrate <- sort(unique(unlist(strsplit(as.character(testdata$g_carbohydrate), ","))))


side_width = 4
ui <- fluidPage(
  theme= shinytheme("united"),
  navbarPage(
    "Clear your Fridge",
    tabPanel("Find your Recipe",
             sidebarLayout(
               position = "left",
               sidebarPanel(width = side_width,
                            h3("Filter Recipes"),
                            selectizeInput('e1', '1.Type/Select Ingredients', choices = config, multiple = TRUE),
                            hr(),       
                            br(), 
                            selectizeInput('e2', '2. Select Time Range', choices = duration, multiple = TRUE),
                            selectizeInput('e3', '3. Select Calories Level', choices = calorie, multiple = TRUE),
                            selectizeInput('e4', '4. Select Carbohydrates Level', choices = carbohydrate, multiple = TRUE),   
                            #                           selectizeInput('e2', '2. Select Time Range', choices = duration, multiple = FALSE),
                            #                           sliderInput('e3','3.Select Calories Level',min=0,max=450000,value=0,step=NULL),
                            #                           sliderInput('e4','4.Select Carbohydrates Level',min=0,max=37000,value=0,step=NULL),
                            strong("5. Cooking Timer"),
                            br(),br(),
                            actionButton('start','Start'),
                            actionButton('stop','Stop'),
                            actionButton('reset','Reset'),
                            br(),
                            numericInput('seconds','Seconds:',value=10,min=0,max=99999,step=1),
                            textOutput('timeleft')
               ),
               mainPanel(
                 width = 12 - side_width,
                 DT::dataTableOutput("mytable")
               )
             )
    )
  )
  
)

server<-function(input,output,session){
  output$mytable <- renderDataTable({
    if(isTruthy(input$e1))
    {result <- testdata %>%
               filter(all(input$e1 %in% config_split)) %>%
               filter(all(input$e2 %in% g_minutes)) %>%
               filter(all(input$e3 %in% g_calorie)) %>%
               filter(all(input$e4 %in% g_carbohydrate))
    r2<- result %>% ungroup %>% select(Recipe = name,
                                       Ingredient = ingredients,
                                       `Preparation Time` = hours,
                                       Calorie = nutrition_calorie,
                                       Carbohydrate = nutrition_carbohydrate)
    }else {
      r2<- testdata %>% ungroup %>% select(Recipe = name,
                                           Ingredient = ingredients,
                                           `Preparation Time` = hours,
                                           Calorie = nutrition_calorie,
                                           Carbohydrate = nutrition_carbohydrate)
    }
    print(r2)
  }
  ,escape = FALSE,extensions = 'Responsive', selection=list(mode="single", target="row"),
  options = list(pageLength = 10, autoWidth = TRUE,
                 dom  = 'tip',columnDefs = list(list(
                   targets = 1,
                   render = JS(
                     "function(data, type, row, meta) {",
                     "return type === 'display' && data.length > 125 ?",
                     "'<span title=\"' + data + '\">' + data.substr(0, 125) + '...</span>' : data;",
                     "}")
                 ))), callback = JS('table.page(3).draw(false);'), 
  rownames= FALSE)
  
  #extract values from clicked rows 
  detail1 <-  renderText({
    filtered <- testdata %>%
                filter(all(input$e1 %in% config_split)) %>%
                filter(all(input$e2 %in% g_minutes)) %>%
                filter(all(input$e3 %in% g_calorie)) %>%
                filter(all(input$e4 %in% g_carbohydrate))
    s <- input$mytable_rows_selected
    paste(filtered[s,1])
  })
  
  detail2 <- renderText({
    filtered <- testdata %>%
                filter(all(input$e1 %in% config_split)) %>%
                filter(all(input$e2 %in% g_minutes)) %>%
                filter(all(input$e3 %in% g_calorie)) %>%
                filter(all(input$e4 %in% g_carbohydrate))
    s <- input$mytable_rows_selected
    paste(filtered[s,4])
  })
  
  detail3 <- renderText({
    filtered <- testdata %>%
                filter(all(input$e1 %in% config_split)) %>%
                filter(all(input$e2 %in% g_minutes)) %>%
                filter(all(input$e3 %in% g_calorie)) %>%
                filter(all(input$e4 %in% g_carbohydrate))
    s <- input$mytable_rows_selected
    paste(filtered[s,3])
  })
  
  detail4 <- renderText({
    filtered <- testdata %>%
                filter(all(input$e1 %in% config_split)) %>%
                filter(all(input$e2 %in% g_minutes)) %>%
                filter(all(input$e3 %in% g_calorie)) %>%
                filter(all(input$e4 %in% g_carbohydrate))
    s <- input$mytable_rows_selected
    paste(filtered[s,17]," , ",filtered[s,18]," , ",filtered[s,19])
  })
  
  output$modal_text <- renderPrint({
    HTML("<h2><b>Recipe</b></h2>", "<p></p>")})
  
  output$modal_text1 <- detail1
  
  output$modal_text2 <- renderPrint({
    HTML("<hr>", "<h4><b>Ingredients</b></h4>", "<p></p>")})
  
  output$modal_text3 <- detail2
  
  output$modal_text4 <- renderPrint({
    HTML("<p></p>", "<h4><b>Steps</b></h4>", "<p></p>")})  
  
  output$modal_text5 <- detail3
  
  output$modal_text6 <- renderPrint({
    HTML("<hr>", "<h4><b>Tags</b></h4>", "<p></p>")}) 
  
  output$modal_text7 <- detail4
  
  
  myModal <- function(failed=FALSE){
    modalDialog(
      htmlOutput('modal_text'),
      htmlOutput('modal_text1'),
      htmlOutput('modal_text2'),
      htmlOutput('modal_text3'),
      htmlOutput('modal_text4'),
      htmlOutput('modal_text5'),
      htmlOutput('modal_text6'),
      htmlOutput('modal_text7'),
      easyClose = TRUE
    )
  }
  
  observeEvent(input$mytable_rows_selected,{
    showModal(myModal())
  })
  
  # Initialize the timer, 10 seconds, not active.
  timer <- reactiveVal(10)
  active <- reactiveVal(FALSE)
  
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        timer(timer()-1)
        if(timer()<1)
        {
          active(FALSE)
          showModal(modalDialog(
            title = "Important message",
            "Countdown completed!"
          ))
        }
      }
    })
  })
  
  # observers for actionbuttons
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$stop, {active(FALSE)})
  observeEvent(input$reset, {timer(input$seconds)})
}

shinyApp(ui=ui,server=server)
