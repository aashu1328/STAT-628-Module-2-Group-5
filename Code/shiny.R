library(shiny)
library(shinyvalidate)
library(shinyjs)
library(tidyverse)
library(WVPlots)
library(rsconnect)
bodyfat <- read.csv('BodyFat_Clean.csv')


ui <- fluidPage(
  #set title
  titlePanel("Adult male body fat prediction"),
  
  tags$head(
    tags$style(HTML(" .shiny-output-error-validation {color: #ff0000;font-weight: bold;}"))),
  
  fluidRow(
    column(6,
           wellPanel(
             numericInput("height", "height", NULL, min = min(bodyfat$HEIGHT), max = max(bodyfat$HEIGHT)),
             textOutput('height_error_message'),
             tags$head(tags$style("#height_error_message{color: green;font-size: 12px; font-style: italic;}")),
             
             
             numericInput('abdomen','abdomen', NULL, min = 70, max = 110),
             textOutput('abdomen_error_message'),
             tags$head(tags$style("#abdomen_error_message{color: green;font-size: 12px; font-style: italic;}")),
             
             
             actionButton("button", "View your result")
           )
    ),
    
    column(6,  
           wellPanel(
             titlePanel(h4("Convert feet/inch to cm")),
             numericInput('convert_feet', 'convert feet to cm', NULL),
            numericInput('convert_inch', 'convert inch to cm', NULL),
           textOutput('height_in_cm'))
    )
  ),
  
  
  fluidRow(
    column(6,
           wellPanel(
             titlePanel(h4('Your body fat prediction result')),
             textOutput('final_prediction'),
             tags$head(tags$style("#final_prediction{color: green;font-size: 20px;font-style: bold;}")),
             plotOutput('result_bar')
           )
  ),
  column(6,img(src = 'images.png', align = "left",height='250px',width='500px'))

  )
)  





server <- function(input, output) {
  output$height_in_cm <- reactive({
    feet_cm <- input$convert_feet * 30.48
    inch_cm <- input$convert_inch * 2.54
    if (input_provided(input$convert_feet) == FALSE & input_provided(input$convert_inch) == FALSE){
      print (paste0('Your result is '))
    }
    else if(input_provided(input$convert_feet) == TRUE & input_provided(input$convert_inch) == TRUE){
      final_height <- feet_cm + inch_cm
      print (paste0('Your result is ', final_height, 'cm'))
    }
    else if ((input_provided(input$convert_feet) | input_provided(input$convert_inch)) == TRUE){
      if(input_provided(input$convert_feet) == TRUE){
        print(paste0('Your result is ', feet_cm, 'cm'))
      }
      else{
        print(paste0('Your result is ', input$convert_inch * 2.54, 'cm'))
      }
    }

  })
  
  
  
  #import df
  bodyfat_data <- reactive({
    bodyfat
  })
  
  
  #set required field
  output$dropdown_val <- reactive({
    return (input$dropdown)
  })
  
  
  iv_acc <- InputValidator$new()
  #iv_acc$condition(~isTRUE(input$dropdown != 'Logical'))
  iv_acc$add_rule('height', sv_required())
  iv_acc$add_rule('abdomen', sv_required())
  iv_acc$enable()
  
  
  
  #Below are the range of each input and error message
  output$height_error_message <- reactive({
    validate(need((input$height >= 140 && input$height <= 220), 
                  "height should be between 140cm and 220cm"))
  })
  
  output$abdomen_error_message <- reactive({
    validate(need((input$abdomen >= 70 && input$abdomen <= 120),
                  'abdomen circumference should be between 80cm to 110cm'))
  })
  
  
  output$dropdown_explain <- reactive({
    print('This selection box will perform your prediction on different models. Logical model uses most accessable features such as age, height and weight. 
                 For accuract model, you will be asking to input age and abdomen circumference.
          Default will be showing you the most accurate prediction')
  })
  
  
  
  #Button setup
  observeEvent(input$button,{
    if ((input_provided(input$height) == TRUE) & (input_provided(input$abdomen) == TRUE))
    {
      if ( 
            (input$height <= 220 & input$height >= 140)&
           (input$abdomen <= 110 & input$abdomen >= 80))
      {
        output$final_prediction <-renderText({
          input$button
          req(input$button)
          
          #User's information will be put into this data frame
          test_df = data.frame()
          test_df[1,] = c(1)
          

          model_formula = paste('BODYFAT','ABDOMEN', sep = '~')
          
          if (isTruthy(input$abdomen)){
            test_df$ABDOMEN = c(input$abdomen)
          }
          
          if (isTruthy(input$height)){
            model_formula = paste(model_formula,'HEIGHT',sep = '+')
            test_df$HEIGHT = c(input$height)
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
          }, height = 350, width = 350)
          
          
          if( model_prediction < 14 & model_prediction >=6){
            print(paste0('\n','You body fat is ', model_prediction ,'%', ' and ','You have an athlete bodyfat'))
          }
          else if (model_prediction < 18 & model_prediction >=14){
            print(paste0('\n','You body fat is ', model_prediction ,'%', ' and ','You look fit'))
          }
          else if (model_prediction < 24 & model_prediction >=18){
            print(paste0('\n','You body fat is ', model_prediction ,'%', ' and ','You bodyfat level is normal'))
          }
          else if (model_prediction >= 25){
            print(paste0('\n','You body fat is ', model_prediction ,'%',' and ','You might need to hit the gym'))
          }
        })
      }else{
        if((input$height > 220 | input$height < 140) & (input$abdomen > 110 | input$abdomen < 80) == TRUE){
          showModal(modalDialog(title = 'Warning!!!','Both inputs are wrong, please see the hint!!!'))
        }
        else if ((input$height > 220 | input$height < 140)){
          showModal(modalDialog(title = 'Warning!!!','Wrong height, please see the hint!!!'))
        } else if (input$abdomen > 110 | input$abdomen < 80){
          showModal(modalDialog(title = 'Warning!!!','Wrong abdomen circumference, please see the hint!!!'))
        }
      }
    }
    else if ((input_provided(input$abdomen) & input_provided(input$height)) == FALSE)
      {
      showModal(modalDialog(title = 'Warning!!!','Please fiil out all the required fields!!!'))
      hide('final_prediction')
    }
  })
  
}
shinyApp(ui, server)
