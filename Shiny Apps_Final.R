library(shiny)
library(dplyr)
library(DT)
library(data.table)
library(shinyalert)
library(shinythemes)
library(readr)
library(lubridate)

#load data
testdata <- read_csv("Recipe.csv")
#splitting variables for selection
testdata$config_split <- strsplit(as.character(testdata$ingredients),split = ",")
testdata <- rowwise(testdata)
config <- unique(unlist(strsplit(as.character(testdata$ingredients), ",")))
duration <- sort(unique(unlist(strsplit(as.character(testdata$g_minutes), ","))))
calorie <- sort(unique(unlist(strsplit(as.character(testdata$g_calorie), ","))))
carbohydrate <- sort(unique(unlist(strsplit(as.character(testdata$g_carbohydrate), ","))))
#create empty matrix to store filtered values
mat <- matrix(1:9, ncol = 9)


side_width = 4
ui <- fluidPage(
    theme= shinytheme("united"),
    navbarPage(
        "Clear your Fridge",
        #Tab Page 1
        tabPanel("About",HTML(paste0("This app helps to recommend recipes based on ingredients, preperation time and nutrition values.",
                                     br(),br(),'Instruction:',br(),
                                     "i) Select your ingredients, expected preperation time and desired nutrition level at the drop box on the left.",
                                     br(),"ii) Customize your cooking timer.",br(),
                                     "iii) Click on the recipe to prompt for detail steps and informations.",br(),
                                     br(),"Happy cooking!",br(),br())),
                 fluidPage(img(src = 'sushi.jpg', height = '100px', width = '100px'),br(),br(),
                           "Slide url: ",a("Rpubs", href="https://rpubs.com/qianhuit/recipefinder"),br(),
                           "Source code: ",a("Github", href="https://github.com/jhloke-24/WQD7001-Group-Project")
                 )),
        #Tab Page 2
        tabPanel("Find your Recipe",
                 sidebarLayout(
                     position = "left",
                     sidebarPanel(width = side_width,
                                  h3("Filter Recipes"),
                                  selectizeInput('e1', '1.Type/Select Ingredients', choices = config, multiple = TRUE),
                                  br(), 
                                  selectizeInput('e2', '2. Select Time Range', choices = duration, multiple = TRUE),
                                  selectizeInput('e3', '3. Select Calories Level', choices = calorie, multiple = TRUE),
                                  selectizeInput('e4', '4. Select Carbohydrates Level', choices = carbohydrate, multiple = TRUE),
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
    
    
    global <- reactiveValues(mat = NULL)
    
    #filter selected preferences
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
                                           Carbohydrate = nutrition_carbohydrate,
                                           Steps = steps,
                                           Tag1 = tags_01,
                                           Tag2 = tags_02,
                                           Tag3 = tags_03)
        }else {
            r2<- testdata %>% ungroup %>% select(Recipe = name,
                                                 Ingredient = ingredients,
                                                 `Preparation Time` = hours,
                                                 Calorie = nutrition_calorie,
                                                 Carbohydrate = nutrition_carbohydrate,
                                                 Steps = steps,
                                                 Tag1 = tags_01,
                                                 Tag2 = tags_02,
                                                 Tag3 = tags_03)
        }
        print(r2)
        global$mat <- r2
    }
    ,escape = FALSE,extensions = 'Responsive', selection=list(mode="single", target="row"),
    options = list(pageLength = 10, autoWidth = TRUE,
                   dom  = 'tip',columnDefs = list(list(visible=FALSE, targets=6:9)),
    rownames= FALSE))
    
    
    #extract values from clicked rows for popup
    detail1 <-  renderText({
        s <- input$mytable_rows_selected
        paste(global$mat[s,1])
    })
    
    detail2 <- renderText({
        s <- input$mytable_rows_selected
        paste(global$mat[s,2])
    })
    
    detail3 <- renderText({
        s <- input$mytable_rows_selected
        paste(global$mat[s,6])
    })
    
    detail4 <- renderText({
        s <- input$mytable_rows_selected
        paste(global$mat[s,7]," , ",global$mat[s,8]," , ",global$mat[s,9])
    })
    
    
    #print values for popup
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
    
    #display popup values in sequence
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
    
    #Timer
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
