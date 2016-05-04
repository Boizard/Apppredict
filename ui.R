library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("APP"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        fileInput("modelfile",label=h4("previous analysis"),accept=".RData"),      
        checkboxInput("help","show help",FALSE)
 
      )),
      mainPanel(
        conditionalPanel(condition =" !output.modelUploaded",
                         imageOutput("image1", height = 300)),           
        
        conditionalPanel(condition ="output.modelUploaded",
                         
                                        wellPanel(
                                           fluidRow(
                                             column(4,h3("Download new data"),
                                                 fileInput("predictionfile", label = h4("prediction File "),accept =  c("text/csv","application/vnd.ms-excel",
                                                                        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xls",".xlsx")) ),
                                             column(4,br(),br(),br(),radioButtons("filetypepred", "Extention of the file",c("csv" = "csv", "xlsx ou xls" = "xlsx"))),
                                             column(4,textInput('decpred', 'character for decimal point',value = "." ),
                                                    textInput("NAstringpred", label = "characters for missing values",value = "NA"))),
                                           hr(),
                                        fluidRow( column(4,
                                                 conditionalPanel(condition ="input.filetypepred=='csv' ",
                                                                  
                                                 helpText("For csv extension"),
                                                 radioButtons('seppred', 'Separator',c(Comma=',',Semicolon=';',Tab='\t') )),
                                        
                                                 conditionalPanel(condition ="input.filetypepred=='xlsx' ",
                                                 helpText("For excel extension"),
                                                 numericInput("skipnpred",label = "number of lines to skip",value = 0),
                                                 numericInput("sheetnpred",label = "sheet",value = 1))),
                                                 column(4,checkboxInput("changedata",h4("Transformation data"),FALSE),
                                                        checkboxInput("transposepred","Transpose the table",FALSE),
                                                        checkboxInput("zeroegalNApred","consider 0 as NA",FALSE)
                                                        ),
                                                 column(4,numericInput("ncolnamespred",label = "colnames",value = 1),
                                                        numericInput("nrownamespred",label = "rownames",value = 1))),
                                        hr(),
                                        actionButton("confirmdatabuttonpred","Confirm data")),
                         conditionalPanel(condition="output.filepredUploaded & input.confirmdatabuttonpred==0",      
                                          
                                       hr(),
                                       h3("predict Data"),
                                       dataTableOutput("JDDpredict")),
                         conditionalPanel(condition="input.confirmdatabuttonpred",
                                       hr(),
                                       h3("Model parameters"),
                                       tableOutput("parameters"),
                                       hr(),
                                       fluidRow(
                                       column(5,
                                       h3("Prediction, score"),
                                       tableOutput("resprediction")
                                       ),
                                       column(7,br(), plotOutput("plotscorepred",width = "100%",height = 500))
                                        ))
                                        ))
    )
  )
  
)