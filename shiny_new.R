



```{r}
library(shiny)
library(shinyvalidate)
library(shinyjs)
library(tidyverse)
library(WVPlots)
bodyfat <- read.csv('BodyFat_Clean.csv')


ui <- fluidPage(
  #set title
  titlePanel("Adult male body fat prediction"),
  
  tags$head(
    tags$style(HTML(" .shiny-output-error-validation {color: #ff0000;font-weight: bold;}"))),
  
  
  fluidRow(
    column(6, 
                 
      selectInput('dropdown','How do you want your model', choices = c('Logical','Accurate')),
      textOutput('dropdown_explain'),
      tags$head(tags$style("#dropdown_explain{color: red;font-size: 10px; font-style: italic;}")),
      
      
      numericInput("age", "age:", NULL, min = 18, max = 90),
      textOutput('age_error_message',),
      tags$head(tags$style("#age_error_message{color: red; font-size: 12px;font-style: italic;}")),
       
      numericInput("weight", "weight", NULL, min = min(bodyfat$WEIGHT), max = max(bodyfat$WEIGHT)),
      textOutput('weight_error_message'),
      tags$head(tags$style("#weight_error_message{color: red;font-size: 12px;font-style: italic;}")),


      numericInput("height", "height", NULL, min = min(bodyfat$HEIGHT), max = max(bodyfat$HEIGHT)),
      textOutput('height_error_message'),
      tags$head(tags$style("#height_error_message{color: red;font-size: 12px; font-style: italic;}")),
      
      
      numericInput('abdomen','abdomen', NULL, min = 70, max = 110),
      textOutput('abdomen_error_message'),
      tags$head(tags$style("#abdomen_error_message{color: red;font-size: 12px; font-style: italic;}")),

      
      actionButton("button", "View your result"),
      textOutput('final_prediction'),
      tags$head(tags$style("#final_prediction{color: green;font-size: 20px;font-style: bold;}"))
    ),
    
    column(6, plotOutput('result_bar'))
  ),
  textOutput('check_drop')

)





server <- function(input, output) {
  
  #import df
  bodyfat_data <- reactive({
    bodyfat
  })
  
  
  #set required field
  output$dropdown_val <- reactive({
    return (input$dropdown)
  })
  
  iv_logi <- InputValidator$new()
  iv_logi$condition(~isTRUE(input$dropdown == 'Logical'))
  iv_logi$add_rule("age", sv_required())
  iv_logi$add_rule("height", sv_required())
  iv_logi$add_rule('weight', sv_required())
  iv_logi$enable()
  
  iv_acc <- InputValidator$new()
  iv_acc$condition(~isTRUE(input$dropdown != 'Logical'))
  iv_acc$add_rule('abdomen', sv_required())
  iv_acc$enable()
  
  
  
  #Below are the range of each input and error message
  output$age_error_message <- reactive({
    validate(need((input$age >= 18 && input$age <= 90),
                  "age should be between 18 and 90"))
    
  })
  output$weight_error_message <- reactive({
    validate(need((input$weight >= 40 && input$weight <= 200), 
                  "weight should be between 40kg to 200kg"))
  })
  
  output$height_error_message <- reactive({
    validate(need((input$height > 140 && input$height < 220), 
                  "height should be between 140cm and 220cm"))
  })
  
  output$abdomen_error_message <- reactive({
    validate(need((input$abdomen > 80 && input$abdomen < 110),
                  'abdomen circumference should be between 80cm to 110cm'))
  })
  
  
  output$dropdown_explain <- reactive({
    print('This selection box will perform your prediction on different models. Logical model uses most accessable features such as age, height and weight. 
                 For accuract model, you will be asking to input age and abdomen circumference.
          Default will be showing you the most accurate prediction')
  })
  
  
  
  #Button setup
  observeEvent(input$button,{
    if ( ( (input$dropdown == 'Logical') & ((input_provided(input$age) & input_provided(input$weight) & input_provided(input$height)) == TRUE)) == TRUE |
         ((input$dropdown == 'Accurate') & (input_provided(input$abdomen) == TRUE)) == TRUE)
      {
      if ( ((input$age <= 90 & input$age >= 18) & 
           (input$weight <= 200 & input$weight >= 40) &
           (input$height <= 220 & input$height >= 140)) |
           (input$abdomen <= 110 & input$abdomen >= 80)
          )
        {
              output$final_prediction <-renderText({
                input$button
                req(input$button)
                
                #User's information will be put into this data frame
                test_df = data.frame()
                test_df[1,] = c(1)
                
                if(input$dropdown == 'Logical'){
                  model_formula = paste('BODYFAT','HEIGHT', sep = '~')
                  if (isTruthy(input$age)){
                    model_formula = paste(model_formula,'AGE',sep = '+')
                    test_df$AGE = c(input$age)
                  }
          
                  if (isTruthy(input$weight)){
                    model_formula = paste(model_formula,'WEIGHT',sep = '+')
                    test_df$WEIGHT = c(input$weight)
                  }
                  
                  if (isTruthy(input$height)){
                    test_df$HEIGHT = c(input$height)
                  }
                }
                else{
                  model_formula = paste('BODYFAT','ABDOMEN', sep = '~')
                                    
                  if (isTruthy(input$abdomen)){
                    test_df$ABDOMEN = c(input$abdomen)
                  }
                  
                  if (isTruthy(input$age)){
                    model_formula = paste(model_formula,'AGE',sep = '+')
                    test_df$AGE = c(input$age)
                  }
                }
        
                model <- lm(model_formula, data = bodyfat_data())
                model_prediction <- round(predict(model, newdata = test_df),3)
                isolate(paste0('\n','You body fat is ', round(model_prediction,3) ,'%'))
                
                
                test_df$body_fat = c(model_prediction)
                output$result_bar <- renderPlot({
                  if(model_prediction <= 18){
                    barplot(test_df$body_fat,ylim=c(0,60),ylab = 'Body fat level', col = 'green')
                  }else if (between(model_prediction,18,24)){
                    barplot(test_df$body_fat,ylim=c(0,60),ylab = 'Body fat level', col = 'blue')
                  }else{
                    barplot(test_df$body_fat,ylim=c(0,60),ylab = 'Body fat level', col = 'red')
                  }
                })
                
                
                if( model_prediction < 13 & model_prediction >6){
                  print(paste0('\n','You body fat is ', model_prediction ,'%', ' and ','You have an athlete bodyfat'))
                }
                else if (model_prediction < 17 & model_prediction >14){
                  print(paste0('\n','You body fat is ', model_prediction ,'%', ' and ','You look fit'))
                }
                else if (model_prediction < 24 & model_prediction >18){
                  print(paste0('\n','You body fat is ', model_prediction ,'%', ' and ','You bodyfat level is normal'))
                }
                else if (model_prediction>25){
                  print(paste0('\n','You body fat is ', model_prediction ,'%',' and ','You might need to hit the gym'))
                }
              })
        }
    }
    else if( ((input$dropdown == 'Logical') & ((input_provided(input$age) & input_provided(input$weight) & input_provided(input$height)) == FALSE)) == FALSE |
             ((input$dropdown == 'Accurate') & (input_provided(input$abdomen) == FALSE)) == FALSE
             ){
      showModal(modalDialog(title = 'Warning!!!','Please fiil out all the required fields!!!'))
      hide('final_prediction')
    }
  })
  
}
shinyApp(ui, server)
```
