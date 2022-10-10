
#input box setting
# numericInput("neck", "neck", 0, min = min(bodyfat$NECK), max = max(bodyfat$NECK)),
# textOutput('neck_error_message'),
# numericInput("chest", "chest", 0, min = min(bodyfat$CHEST), max = max(bodyfat$CHEST)),
# textOutput('chest_error_message'),
# numericInput("abdomen", "abdomen", 0, min = min(bodyfat$ABDOMEN), max = max(bodyfat$ABDOMEN)),
# textOutput('abdomen_error_message'),
# numericInput("hip", "hip", 0, min = min(bodyfat$HIP), max = max(bodyfat$HIP)),
# textOutput('hip_error_message'),
# numericInput("thigh", "thigh", 0, min = min(bodyfat$THIGH), max = max(bodyfat$THIGH)),
# textOutput('thigh_error_message'),
# numericInput("knee", "knee", 0, min = min(bodyfat$KNEE), max = max(bodyfat$KNEE)),
# textOutput('knee_error_message'),
# numericInput("ankle", "ankle", 0, min = min(bodyfat$ANKLE), max = max(bodyfat$ANKLE)),
# textOutput('ankle_error_message'),
# numericInput("biceps", "biceps", 0, min = min(bodyfat$BICEPS), max = max(bodyfat$BICEPS)),
# textOutput('biceps_error_message'),
# 






# output$neck_error_message <- reactive({
#   validate(need((input$neck >= 30 && input$neck <= 60),"wrong input: neck circumfences should be between 30 and 60"))
# })
# 
# 
# output$chest_error_message <- reactive({
#   validate(need((input$chest > 96 && input$chest < 107), "wrong input: chest circumfences should be between 96cm and 107cm"))
# })
# 
# 
# output$abdomen_error_message <- reactive({
#   validate(need((input$abdomen > 60 && input$abdomen < 160), "wrong input: abdomen circumfences should be between 60cm and 160cm"))
# })
# 
# output$hip_error_message <- reactive({
#   validate(need((input$hip >= 85 && input$hip < 147.7), "wrong input: hip circumfences should be between 96cm and 107cm"))
# })
# 
# output$thigh_error_message <- reactive({
#   validate(need((input$thigh >= 40 && input$thigh < 95), "wrong input: thigh circumfences should be between 40cm and 95cm"))
# })
# 
# 
# output$ankle_error_message <- reactive({
#   validate(need((input$ankle >= 10 && input$ankle < 45), "wrong input: ankle circumfences should be between 10cm and 45cm"))
# })
#   
#   
# output$knee_error_message <- reactive({
#   validate(need((input$knee >= 25 && input$knee < 60), "wrong input: knee circumfences should be between 25cm and 60cm"))
# })
# 
# 
# 
# output$biceps_error_message <- reactive({
#   validate(need((input$biceps >= 15 && input$biceps < 60), "wrong input: biceps circumfences should be between 15cm and 60cm"))
# })
# 



###############################################################################################

# Will be tested later
# output$table <- renderDataTable({
#   test_df = data.frame()
#   test_df[1,] = c(1)
#   test_df$HEIGHT = c(180)
#   
# 
#     #If there is input, then add this input to the model formula
# feature_list = c("age", "weight","height", "adiposity" ,"neck", "chest","abdomen", "hip","thigh","knee","ankle","biceps" ,"forearm", "wrist")
#   
#   #model_formula = paste('BODYFAT','HEIGHT', sep = '~')
#   
#   # for (name in feature_list){
#   #   if (isTruthy(input$name)){
#   #     #没问题
#   #     #model_formula = paste(model_formula,toupper(input$namename),sep = '+')
#   #     #没问题
#   #     # feature_name = toupper(name)
#   #     # test_df[1,feature_name] = c(input$name)
#   #     
#   #     print(input$name)
#   #   }
#   # }
#   #print(test_df)
# })














#Botton setting
# observeEvent(input$button, {
#     output$show_prediction <- renderText({
#   
#       test_df = data.frame()
#       test_df[1,] = c(1)
#       
#       model_formula = paste('BODYFAT','HEIGHT', sep = '~')
#       
#       if (isTruthy(input$age)){
#         model_formula = paste(model_formula,'AGE',sep = '+')
#         test_df$AGE = c(input$age)
#       }
#       
#       if (isTruthy(input$weight)){
#         model_formula = paste(model_formula,'WEIGHT',sep = '+')
#         test_df$WEIGHT = c(input$weight)
#       }
#       
#       if (isTruthy(input$height)){
#         model_formula = paste(model_formula,'HEIGHT',sep = '+')
#         test_df$HEIGHT = c(input$height)
#       }
#       
#       
#       model <- lm(model_formula, data = bodyfat_data())
#       paste0('\n','You body fat is ', predict(model, newdata = test_df),'%')
#     })
# })

