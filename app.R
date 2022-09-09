packages = c('shiny','shinydashboard','tidyverse','dplyr', 'magrittr', 'plotly', 'ggplot2', 'scales', 'DT')

for (p in packages){
  if (!require(p,character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}
library(shiny)
library(datasets)
library(caret)
library(tidyverse)
library(ggthemes)
library(corrplot)
library(GGally)
library(DT)
library(ggplot2)
library(pROC)
library(readr)
library(shinyWidgets)
# Data pre-processing ----
# 



#clean_loan = read_csv("~/Desktop/R_Shiny_Project/clean_loan.csv")
loan =read_csv("~/Desktop/R_Shiny_Project/loan.csv" )
loan2 = loan  

                                                                                

# Clean the dataset and remove missing data
mean = mean(loan2$`Annual Income`[!is.na(loan2$`Annual Income`)])
loan2$`Annual Income` = ifelse((is.na(loan2$`Annual Income`)) | loan2$`Annual Income` >1000000, mean, as.numeric(loan2$`Annual Income`))
loan2$`Bankruptcies` =ifelse(is.na(loan2$`Bankruptcies`),0, ifelse(loan2$`Bankruptcies`>=1, 1, 0))
loan2$`Maximum Open Credit` = ifelse(is.na(loan2$`Maximum Open Credit`),median(loan2$`Maximum Open Credit`[!is.na(loan2$`Maximum Open Credit`)]), loan2$`Maximum Open Credit`)


status = substr(loan2$`Years in current job`, 0,2)
loan2$`Years in current job`  = NULL #ifelse(as.numeric(status) %in% seq(0:60), as.numeric(status), 0)
loan2$Purpose = ifelse(loan2$Purpose == "other", "Other", ifelse(loan2$Purpose == "small_business", "Business Loan", loan2$Purpose))
loan2$`Credit Score` = ifelse(loan2$`Credit Score`>800, as.numeric(substr(loan2$`Credit Score`, 0,3)), as.numeric(loan2$`Credit Score`))


loan2$`Monthly Debt` = ifelse((loan2$`Monthly Debt`)>=999999, 12*median(loan2$`Monthly Debt`[loan2$`Monthly Debt`< 999999]), 12*loan2$`Monthly Debt`)
loan2$`Tax Liens` = ifelse(is.na(loan2$`Tax Liens`), median(loan2$`Tax Liens`[!is.na(loan2$`Tax Liens`)]), loan2$`Tax Liens`)

loan2$`Current Loan Amount` = ifelse((loan2$`Current Loan Amount`)>=9999999, median(loan2$`Current Loan Amount`[loan2$`Current Loan Amount`< 999999]), loan2$`Current Loan Amount`)




loan2$`Monthly Debt` = ifelse(is.na(loan2$`Monthly Debt`), median(!is.na(loan2$`Monthly Debt`)), loan2$`Monthly Debt`)

loan2$`Months since last delinquent` = ifelse(is.na(loan2$`Months since last delinquent`), 0, loan2$`Months since last delinquent`)

loan2 = loan2%>% filter(!is.na(loan2$`Credit Score`))

#mean(loan2$`Annual Income`)
#mean(loan2$`Current Loan Amount`)
loan2 = loan2%>% mutate(`Debt Ratio`=(`Current Loan Amount`+`Monthly Debt`)/`Annual Income`)

loan2$loan_outcome = ifelse(loan2$`Loan Status` == "Charged Off", 1, 0)
clean_loan =loan2

clean_loan1= clean_loan[c(1:21)]

clean_loan1$loan_outcome = as.numeric(clean_loan1$loan_outcome)
idx = sample(nrow(clean_loan1) , 0.25*nrow(clean_loan1) , replace = F)
trainset = clean_loan1[idx , ]
testset = clean_loan1[-idx , ]

# We used metrics as the Akaike Information Criterion, or AIC, to explore which features
#to include in the model to balance accuracy and complexity.
glm.model = glm(loan_outcome ~`Credit Score`+`Annual Income`+`Debt Ratio`+`Monthly Debt`+`Current Loan Amount`+Term, trainset, family = "binomial")
#summary(glm.model)

preds = predict(glm.model , testset , type = 'response')

k = 0
accuracy = c()
sensitivity = c()
specificity = c()
precision=c()
f1score =c()
balanced_mean =c()
stepsize = 0.01
confusionMatrix =c()
thresholds  = seq(from = 0.01 , to = 0.5 , by = stepsize)
for(i in thresholds){
  k = k + 1
  preds_binomial = ifelse(preds > i , 1 , 0)
  #print(preds_binomial)
  confmat = table(testset$loan_outcome , preds_binomial)
  #print(confmat)
  #confusionMatrix[k] = list(confmat[1,1], confmat[1,2], confmat[2,1], confmat[2,2])
  #print(confmat)
  #print(" ")
  
  confmat = table(testset$loan_outcome , preds_binomial)
  accuracy[k] = sum(diag(confmat)) / sum(confmat)
  sensitivity[k] = confmat[1 , 1] / sum(confmat[ , 1])
  specificity[k] = confmat[2 , 2] / sum(confmat[ , 2])
  precision[k] = confmat[1 , 1] / sum(confmat[ , 1])
  f1score[k] = 2 * sensitivity[k]*precision[k]/(sensitivity[k] + precision[k])
  balanced_mean[k] = 0.5 * (sensitivity[k]+specificity[k])
  confusionMatrix[k] = array(list(confmat))
} 
#print(confusionMatrix)
data <-data.frame(thresholds ,  accuracy ,sensitivity , specificity, precision, f1score, balanced_mean)

newdata = gather(data, key = 'Metric' , value = 'Value' , 2:7)

cutoff =0.215
ggplot(newdata, aes(x = thresholds , y = Value , color = Metric)) + geom_line(size = 1)+geom_vline(xintercept =  cutoff,color="red" , linetype = "dashed")


preds_binomial = ifelse(preds > cutoff, 1 , 0)
table(testset$loan_outcome , preds_binomial)

preds_binomial = ifelse(preds > cutoff, 1 , 0)
potential_defaulters =testset%>% filter(preds_binomial==1 & testset$loan_outcome==1)
potential_nondefaulters = testset%>% filter(preds_binomial==0 & testset$loan_outcome==0)
uncertain_clents = testset%>% filter((preds_binomial==1 & testset$loan_outcome==0)|(preds_binomial==0 & testset$loan_outcome==1))

testset$prediction =ifelse((preds_binomial==1 & testset$loan_outcome==1),"Potential Defaulter", ifelse((preds_binomial==0 & testset$loan_outcome==0), "Potential Nondefaulter", "Uncertain Client"))
testset2 = testset%>% filter(testset$prediction=="Uncertain Client")


testset2

idx2 = sample(nrow(testset2), 0.35*nrow(testset2) , replace = F)
trainset3 = testset2[idx2 , ]
testset3 = testset2

# Fit logistic regression
glm.model2 = glm(loan_outcome ~ ., trainset3[c(21,12,9,7,6)] , family = "binomial")
summary(glm.model2)

preds2 = predict(glm.model2 , testset2 , type = 'response')
modelroc2 = roc(testset2$loan_outcome , preds2) 
plot(modelroc2, print.auc=TRUE, auc.polygon=TRUE,
     grid=c(0.1, 0.2), grid.col=c("green", "red"),
     max.auc.polygon=TRUE, auc.polygon.col="skyblue", print.thres=TRUE)
new_prediction =ifelse(preds2>cutoff, 1,0)




testset$prediction = ifelse(testset$prediction=="Potential Nondefaulter", "Potential Nondefaulter", ifelse(testset$prediction=="Potential Defaulter", "Potential Defaulter",ifelse((new_prediction==1 & testset2$loan_outcome==1),"Potential Defaulter", ifelse((new_prediction==0 & testset2$loan_outcome==0), "Potential Nondefaulter", "Status Unkwown"))))
#testset$prediction

















# Define UI for Credit Classification Model ----
ui <- fluidPage(
  
  
  setBackgroundColor(
      color = c("lightyellow", "lightblue"),
        gradient = "radial",
        direction = c("top", "left")
       ),
  
    
# App title ----
titlePanel(title="Credit Risk Classification Model for Banks and Digital Marketing"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          #sliderInput("cutoff", "Cutoff",
                      #min = 0.1, max = 0.5,
                      #value = 0.21),
          sliderInput(inputId = "bins",
                      label = "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 22),
          
          # Input: Select a dataset ----
          selectInput("dataset", "Choose a dataset:",
                      choices = c("clean_loan", "loan", "loan2", "testset")),
          #Select Customer ID
          selectInput("dataset1", "Choose a dataset:",
                     choices = head(testset$`Customer ID`),100),
                       
          #selectInput("dataset1", "Choose a dataset:",
                      #choices = list(c("clean_loan", "loan", "loan2", "clean_loan1"))),
          # Input: Specify the number of observations to view ----
          numericInput("obs", "Number of observations to view:", 10),
          
          # Include clarifying text ----
          helpText("Note: while the data view will show only the specified",
                   "number of observations, the summary will still be based",
                   "on the full dataset."),
          
          # Input: actionButton() to defer the rendering of output ----
          # until the user explicitly clicks the button (rather than
          # doing it immediately when inputs change). This is useful if
          # the computations required to render output are inordinately
          # time-consuming.
          actionButton("update", "Update View"),
          
          # Input: Select a dataset ----
          
        
          
          # Input: Select a file ----
          #fileInput("file1", "Choose CSV File",
                    #multiple = TRUE,
                    #accept = c("text/csv",
                     #          "text/comma-separated-values,text/plain",
                      #         ".csv")),
          
         
          
         
          
          
          # Input: Custom currency format for with basic animation ----
          #sliderInput("format", "Custom Format:",
                     # min = 0, max = 10000,
                      #value = 0, step = 2500,
                      #pre = "$", sep = ",",
                      #animate = TRUE),
          
          # Input: Animation with custom interval (in ms) ----
          # to control speed, plus looping
          #sliderInput("animation", "Looping Animation:",
                     # min = 1, max = 2000,
                      #value = 1, step = 10,
                      #animate =
                       # animationOptions(interval = 300, loop = TRUE)),
            
           
            
            selectInput("variable1", "X-Variable:",
                        c(colnames(clean_loan)[c(13,21,19,7,14,15,16,17)])),
            
            # Input: Checkbox for whether outliers should be included ----
            checkboxInput("outliers", "Show outliers", TRUE),
            # Input: Selector for variable of loan ----
            selectInput("variable", "Y-Variable:",
                        c(colnames(clean_loan)[c(7,9,11,12,13,14,15,16,17,19,21)])),
         
          plotOutput("mpgPlot1"),
          
            
            # Input: Checkbox for whether outliers should be included ----
            checkboxInput("outliers", "Show outliers", TRUE),
            # Input: Selector for variable of loan ----
          radioButtons("chart", "Type of Chart:",
                       c("histogram",
                         "scatterplot",
                         "boxplot")),
          
          # br() element to introduce extra vertical spacing ----
          br(),
          
          # Input: Slider for the number of observations to generate ----
          #sliderInput("n",
                     # "Number of observations:",
                      #value = 500,
                     # min = 1,
                      #max = 1000),
          # Input: Select a file ----
          
            
        ),
        
        # Main panel for displaying outputs ----
  mainPanel(
    
          h4("Summary"),
          verbatimTextOutput("summary"),
          # Output: Formatted text for caption ----
          #h3(textOutput("caption")),
            
            
            # Output: Plot of the requested variable against mpg ----
          plotOutput("mpgPlot"),
          
          
          #h4("Customer Data"),
      
          # Output: Header + table of distribution ----
          
          tableOutput("view"),
          plotOutput(outputId = "distPlot"),
          tableOutput("values"),
          
          plotOutput(outputId = "distPlot1"),
         # h4("Observations"),  
         # 
         h4("Dataset"),
         tableOutput("viewobs")
        )
    )
)












# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "clean_loan" = clean_loan,
           "loan" = loan,
           "loan2" = loan2, 
           "testset" =testset)
  }, ignoreNULL = FALSE)
  
  # Generate a summary of the dataset ----
  output$summary <- renderText({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  
  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  output$view <- renderTable({
   
    testset%>% filter(`Customer ID`==input$dataset1)
    #head(datasetInput(), n = isolate(input$obs))
  })
  
  
  
  #output$confmat1 <- renderTable({confusionMatrix[input$bins]})
  
  
  output$distPlot <- renderPlot({
    ggplot(newdata, aes(x = thresholds , y = Value , color = Metric)) + geom_line(size = 1)+geom_vline(xintercept =  0.01*input$bins, color="red" , linetype = "dashed")
  
  })
  
  output$distPlot2 <- renderTable({
   data.frame(confusionMatrix[input$bins])
    
  })
  
  
  output$distPlot1 <- renderPlot({
    ggplot(newdata, aes(x = thresholds , y = Value , color = Metric)) + geom_line(size = 1)+geom_vline(xintercept =  cutoff,color="red" , linetype = "dashed")
    
    plot(modelroc2, print.auc=TRUE, auc.polygon=TRUE,
         grid=c(0.1, 0.2), grid.col=c("green", "red"),
         max.auc.polygon=TRUE, auc.polygon.col="skyblue", print.thres=TRUE)
    #new_prediction =ifelse(preds2>0.01, 1,0)
    
  })
  
  
  
  
  
  
  
  
  
  
    # Compute the formula text ----
    # This is in a reactive expression since it is shared by the
    # output$caption and output$mpgPlot functions
    formulaText <- reactive({
        paste( "`",input$variable, "`~`", input$variable1, "`", sep ="")
    })
    
  
    # Return the formula text for printing as a caption ----
    output$caption <- renderText({
        formulaText()
    })
    
    # Generate a plot of the requested variable against mpg ----
    # and only exclude outliers if requested
    output$mpgPlot   <- renderPlot({
       boxplot(as.formula(formulaText()) , col = "lightblue", pch = 19, data = clean_loan)
        
           })
    
    
    
    output$mpgPlot1 <- renderPlot({
        
        ggplot(clean_loan , aes(x = `Loan Status` , y = ..count.., fill = factor(loan_outcome , c(1 , 0) , c('Default' , 'No Default')) )) + 
            geom_bar() + theme(legend.title = element_blank())
    })
    
    
    sliderValues2 <- reactive({
      
      data.frame( confusionMatrix=
       confusionMatrix[c(input$bins)]
        )
      
    })
    
    # Show the values in an HTML table ----
    output$values <- renderTable({
      sliderValues2()
    })
    output$viewobs <- renderTable({
      head(datasetInput(), n = isolate(input$obs))
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)