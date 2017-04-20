library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("La Boize 2"),
  hr(),
    sidebarLayout(
      conditionalPanel(condition="input.confirmdatabuttonpred==0",
        sidebarPanel(
          h4("Download previous analysis"),
          fileInput("modelfile",label=h6("Select a file from the Modelisation Application (RData extension) "),accept=".RData"),
          conditionalPanel(condition ="output.modelUploaded",
            p( h4("Model parameters"),tableOutput("modelparameters2"),align="center")
          ),
          checkboxInput("help","show help",FALSE),
          conditionalPanel(condition="output.filepredUploaded ",
            hr(),br(),br(),
            "Table have to appear :",
            HTML("<UL><LI> Missing values as empty<LI>Individuals in lines <LI> feature in columns </UL>"),
            imageOutput("structuredata")
          )
        )
      ),
      mainPanel(
        conditionalPanel(condition =" !output.modelUploaded",
                         imageOutput("image1", height = 300)
        ),
        conditionalPanel(condition="input.confirmdatabuttonpred==0",
          conditionalPanel(condition ="output.modelUploaded",
            wellPanel(            
              fluidRow(
                column(3,h4("Download new data"),br(),
                  radioButtons("filetypepred", "Extention of the file",c("csv" = "csv", "xlsx" = "xlsx"))
                ),
                column(4,br(),br(),br(),
                  fileInput("predictionfile", label = "Select a Prediction File",accept =  c("text/csv","application/vnd.ms-excel","application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xls",".xlsx"))),
                column(4,br(),
                  conditionalPanel(condition ="input.filetypepred=='csv' ", 
                    helpText("For csv extension"),
                    radioButtons('seppred', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'))
                  ),
                  conditionalPanel(condition ="input.filetypepred=='xlsx' ",
                                 helpText("For excel extension"),
                                 numericInput("skipnpred",label = "number of lines to skip",value = 0),
                                 numericInput("sheetnpred",label = "sheet",value = 1)
                  )
                ,offset = 1)
              ),
              hr(),
              fluidRow( 
                column(4,textInput('decpred', 'Character for decimal point',value = "." ),
                  textInput("NAstringpred", label = "Characters for missing values",value = "NA")
                  
                ),
                column(4,checkboxInput("transposepred","Transpose the table",FALSE),
                  checkboxInput("zeroegalNApred","Consider 0 as NA",FALSE)
                ,offset = 1),
                column(3,actionButton("confirmdatabuttonpred",h4("Confirm data"),width = 200),
                       h6("Confirm data after checking the prediction table below"))
              )
            ),
            conditionalPanel(condition="output.filepredUploaded ",
              p(h3("Prediction Table :"),textOutput("predictionfile3",inline=T), tags$head(tags$style("#predictionfile3{color: black;font-size: 20px;font-style: bold;}"))),
              dataTableOutput("JDDprediction"),
              p(downloadButton("downloaddataJDDprediction","Download dataset"),align="center")
            )
          )
        ),
        conditionalPanel(condition="input.confirmdatabuttonpred!=0",
          p(fluidRow(
            column(width=8,h2('Table for prediction : ')),
            column(width=2,h4("File name : ",inline=T)),
            column(width=4,textOutput("predictionfile2",inline=T), tags$head(tags$style("#predictionfile2{color: grey;font-size: 20px;font-style: bold;}")))
            )
          ),
          dataTableOutput("JDDpredictiondiff"), 
          p(downloadButton("downloaddataJDDpredictiondiff","Download dataset"),align="center"),
          hr(),
          wellPanel(            
            fluidRow(
              column(width=5,p( h3("Model parameters"),tableOutput("modelparameters"),align="center"),offset = 1),
              column(width=5,p(h3("Model results"),tableOutput("modelmainresults"),align="center"))
            )
          ),
          hr(),
          fluidRow(
            column(4,p(h3("Prediction, score"),
              tableOutput("resprediction"),
              downloadButton("downloaddataresprediction","Download dataset"),align="center"),offset=1),
            column(6,br(),br(),br(),
              plotOutput("plotscorepred",width = "100%",height = 500),
              p(downloadButton("downloadplotscorepred","Download plot"),downloadButton('downloaddatascorepred', 'Download raw data')),align="center"

            ,offset=1)
          )
        )
      )
    )
  )
)
  
