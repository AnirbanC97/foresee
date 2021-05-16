#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("libraries_sources.R")


# Define UI for application that draws a histogram
ui <- navbarPage("Foresee",
                 
                 tabPanel("App",
                 fluidRow(
                 
                 column(3,style='height:500px;',
                     wellPanel(
                          fileInput('file1', 'Choose file:xlsx,csv',accept = c(".xlsx",".csv")),
                          actionButton('Proceedupload','Proceed')
                          ),
                     
                     wellPanel(
                          selectInput("example_datasets", "Select from sample Datasets :",example_data()),                    
                          actionButton('Proceed','Proceed'),                                        
                          ),
                 ),
                              
                 column(9,tabsetPanel(id = "tabpanel",
                                      
                 tabPanel("Dataset",
                          h2("Data"),
                          DT::dataTableOutput("displaydata"),
                          h2("Data Summary"),
                          verbatimTextOutput("data_desc")
                          ),
                 
                 #Example Report Tab UI
                 tabPanel("Example-Report",
                 htmlOutput("example_head"),          
                 htmlOutput("example_doc"),
                 DT::dataTableOutput("example_table"),
                 h2("Data Summary"),
                 verbatimTextOutput("example_desc"),
                 h2("Time-Series Plot"),
                 plotOutput("example_plot"),
                 h2("Time-Series Seasonal Plot"),
                 plotOutput("example_seasonal_plot"),
                 h2("Time-Series Decomposition"),
                 plotOutput("example_decompose_plot"),
                 h2("Forecasts"),
                 htmlOutput("forecast_null"),
                 wellPanel(fluidRow(
                     column(3,selectInput("example_algo_select", "Select Algorithm :",algo_list(),multiple = TRUE)),                    
                     column(3,selectInput("example_test_split", " Test Split(0.X):",test_split_list())),
                     column(6,htmlOutput("ensemble_box")),
                     fluidRow(htmlOutput("FFNN_options")),
                     actionButton("example_forecast_button","Forecast")
                 )),
                 plotOutput("example_forecast_plot"),
                 textOutput("Metrics")
                 )
                 
                 
                 ),
                 )
    
                 )
                 ),
                 
                 tabPanel("Github",),
                 tabPanel("References")
                 

)


# Define server logic required to draw a histogram
server <- function(input, output) {
    

    
    observe({
        if(is.null(input$file1)){
            
            hideTab(inputId = "tabpanel", target = "Dataset")
            hideTab(inputId = "tabpanel", target = "Summary")
            hideTab(inputId = "tabpanel", target = "Processing")
            hideTab(inputId = "tabpanel", target = "Forecast")
            hideTab(inputId = "tabpanel", target = "Report")
            hideTab(inputId = "tabpanel", target = "Example-Report")
            }
    })

    #Example Report Outputs.........................
    observeEvent(input$Proceed, {

            showTab(inputId = "tabpanel", target = "Example-Report")
            hideTab(inputId = "tabpanel", target = "Dataset")
            req(input$example_datasets)
            name<-input$example_datasets
            ts <- example()
            data <- data.frame(as.matrix(ts), date=as.yearmon(time(ts)))
            output$example_head <- renderUI({
                if (is.null(data)){return()}
                else{
                    return(h1(input$example_datasets))
                    }
            })
            
            output$example_doc <- renderUI({
                if (is.null(data)){return()}
                else{
                    return(tags$p("For more info. on dataset click ",
                           tags$a(href="https://www.rdocumentation.org/packages/fpp2/versions/2.4", "here",target="_blank")
                           ))
                }
            })
            
            output$example_table <- DT::renderDataTable({
                if (is.null(data)){return()}
                else{return(data)}
            })
            
            output$example_desc <- renderPrint({
                if (is.null(data)){return()}
                else{return(summary(data))}
        
                })
            output$example_plot <- renderPlot({
                if (is.null(data)){return()}
                else{return(autoplot(ts))}
                
            })
            output$example_seasonal_plot <- renderPlot({
                if (is.null(data)){return()}
                else{return(ggseasonplot(ts, col=rainbow(12), year.labels=TRUE))}
            })
            
            output$example_decompose_plot <- renderPlot({
                if (is.null(data)){return()}
                else{return(autoplot(decompose(ts)))}
            })
            
            

            
    })
    
    observeEvent(input$Proceedupload,{
        showTab(inputId = "tabpanel", target = "Dataset")
        hideTab(inputId = "tabpanel", target = "Example-Report")
        data <- data()
        output$displaydata <- DT::renderDataTable({
            if (is.null(data)){
                return()}
            else{
                datatable(data)}
        })
        
        output$data_desc <- renderPrint({
            if (is.null(data)){return()}
            else{return(summary(data))}
        })
        })
    
    observeEvent(input$example_forecast_button,{
        algos = input$example_algo_select
        ens_method = input$example_ens_method
        ens_model = input$example_ens_model
        splt = input$example_test_split
        output$example_forecast_plot <- renderPlot({
        make_forecast(example(),as.numeric(splt),algos,ens_method,ens_model)})
        
    })

    observeEvent(input$example_algo_select,{
        
        if("Ensemble" %in% input$example_algo_select){
        output$ensemble_box<-renderUI({
        wellPanel(fluidRow(
        column(6,selectInput("example_ens_method", "Ensemble Method :",ensemble_algo_list())),
        column(6,selectInput("example_ens_model", "Ensemble Model :",algo_list2(),multiple = TRUE)),
        ))
        })}
        else{
            output$ensemble_box<-renderUI("")
        }
        
        
    
    })

    
    data<-reactive({
        req(input$file1)
        x<-input$file1
        make_data(x)
    })
    
    example <- reactive({
        req(input$example_datasets)
        x<-input$example_datasets
        make_example_data(x)
    })
    
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
