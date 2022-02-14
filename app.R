library(shiny)
library(ggplot2)
library(here)
library(DT)
library(rpart)
library(rpart.plot)
library(dplyr)

# funkcje ----
char2num<-function(x){ 
  groups = unique(x) 
  as.numeric(factor(x, levels=groups)) 
}






analysis <- read.table(file = here("data", "TitanicCleaned.tsv"), sep = '\t', header = TRUE)
analysis$Age <- ntile(analysis$Age, 4)
analysis$Fare <- ntile(analysis$Fare, 4)




ui <- pageWithSidebar(
  headerPanel('Examples of DataTables'),
  sidebarPanel(
      checkboxGroupInput('show_vars', 'Columns to show:', 
                         names(analysis),
                         selected = names(analysis),
                         ),
  numericInput(inputId = "kolumna", label = "Histogram to show", value = 1, min = 1, max = 1),
  selectInput("var", 
              label = "Choose a variable to display",
              choices = c("Survived", 
                          "Pclass",
                          "Sex",
                          "Age",
                          "SibSp",
                          "Parch",
                          "Fare",
                          "Embarked"),
              selected = "Survived"),
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Tabela", DT::dataTableOutput("mytable")),
      tabPanel("Wykres", plotOutput(outputId = "wykres")),
      tabPanel("Graf", plotOutput(outputId = "graf")),
      )
    )
  )
  
  


server <- function(input, output, session) {
  
  shinyInput <- function(FUN,id,num,...) {
    inputs <- character(num)
    for (i in seq_len(num)) {
      inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
    }
    inputs
  }

  
  rowSelect <- reactive({
    rows=names(input)[grepl(pattern = "srows_",names(input))]
    paste(unlist(lapply(rows,function(i){
      if(input[[i]]==T){
        return(substr(i,gregexpr(pattern = "_",i)[[1]]+1,nchar(i)))
        }
      })))
  })
  

  output$mytable = DT::renderDataTable({
    DT::datatable(cbind(
      analysis[, input$show_vars, drop=FALSE]),
                  options = list(orderClasses = TRUE,
                  lengthMenu = c(5, 25, 50),
                  pageLength = 5 ,
       drawCallback= JS(
       'function(settings) {
        Shiny.bindAll(this.api().table().node());}')
       ),selection='none',escape=F)
    })
  
  output$wykres <- renderPlot({
    analysis$Sex <- char2num(analysis$Sex)
    analysis$Embarked <- char2num(analysis$Embarked)
    
    updateNumericInput(session, "kolumna", max = length(analysis))
    hist(analysis[[input$kolumna]], main = paste("Histogram of" , colnames(analysis[input$kolumna])), xlab="")
  })
  


  output$graf <- renderPlot({
    Survived = rpart(Survived ~., data=analysis, method="class", control=rpart.control(maxdepth=4,minsplit=10, cp=0.008))
    Pclass = rpart(Pclass ~., data=analysis, method="class", control=rpart.control(maxdepth=4,minsplit=10, cp=0.008))
    Sex = rpart(Sex ~., data=analysis, method="class", control=rpart.control(maxdepth=4,minsplit=10, cp=0.008))
    Age = rpart(Age ~., data=analysis, method="class", control=rpart.control(maxdepth=4,minsplit=10, cp=0.008))
    SibSp = rpart(SibSp ~., data=analysis, method="class", control=rpart.control(maxdepth=4, minsplit=10, cp=0.008))
    Parch = rpart(Parch ~., data=analysis, method="class", control=rpart.control(maxdepth=4,minsplit=10, cp=0.008))
    Fare = rpart(Fare ~., data=analysis, method="class", control=rpart.control(maxdepth=4,minsplit=10, cp=0.008))
    Embarked = rpart(Embarked ~., data=analysis, method="class", control=rpart.control(maxdepth=4,minsplit=10, cp=0.008))
    
    rpart.plot(eval(parse(text = input$var)), roundint=FALSE)
  })

}

shinyApp(ui, server)









