library(shiny);library(ggplot2);library(readxl);
library(dplyr);library(labeling)
library(DT);library(rpivotTable);library(readr)
library(stringr);library(tools)



ui <- fluidPage(
  #theme=shinytheme("slate"), ##
  # themeSelector(),
  
  titlePanel(" Benvenuto",windowTitle = "unicredit.app" ),
  tags$h3("Welcome to the Interactive User Platform"),
  tags$br(),
  tags$a("Copy Rights belong to Unicredit.it", href="https://www.unicredit.it/it/privati.html"),
  #tags$a("Contact the App Builder", href="http://www00.unibg.it/struttura/strutturasmst.asp?id_notizia=81930"),
  #tags$a(" Retrive Your Customize DataTable", href="https://seymakalay87.shinyapps.io/app5/"),
  
  # navbarPage( title = "",
  #            id="nav",
  
  sidebarLayout(
    
    # Inputs
    sidebarPanel( width = 3,
                  
                  # Select variable for y-axis
                  selectInput(inputId = "y", 
                              label = "Y-axis:",
                              choices = c("index.oninvestment", "index.on.prodection", "INDICE"), 
                              selected = "INDICE"),
                  
                  # Select variable for x-axis
                  selectInput(inputId = "x", 
                              label = "X-axis:",
                              choices = names(ClustereData[,-c(1:6,18:21,44:46)]),
                              
                              
                              selected = "RISERVA_MATEMATICA_201711"),
                  
                  
                  # Select variable for color
                  selectInput(inputId = "z", 
                              label = "Color by:",
                              choices = c("DES_REGIONE_ABI_GEST" = "DES_REGIONE_ABI_GEST", 
                                          "DES_PROVINCIA_ABI_GEST" = "DES_PROVINCIA_ABI_GEST", 
                                          #"FLAG_RECESSO_2017" = "FLAG_RECESSO_2017", 
                                          "c_I1I2Indice" = "c_I1I2Indice"),
                              selected = "c_I1I2Indice"),
                  
                  # Set alpha level
                  sliderInput(inputId = "alpha", 
                              label = "Alpha:", 
                              min = 0, max = 1, 
                              value = 0.3),
                  
      
                  
                  
    ),
    
    
    
    # Outputs
    
    mainPanel( width = 9,
               
               tabsetPanel(type = "tabs",
                           # Tab 1: Plot
                           tabPanel(title = "Plot", 
                                    
                                    br(),h4("Visualize Custom Plot"),br(), 
                                   
                                
                                    br(),
                                    h4("Visualize the Selected Points "), br(),
                                    
                                    
                                    DT::dataTableOutput(outputId = "moviestable"),
                                    
                                    # HTML("Select Points on the Graph, then hit 'Download data'."),
                                    # br(), br(), # line break and some visual separation
                                    # downloadButton("download_data1 ", "Download data"),
                                    
                                    
                                    
                                    #h2("Densityplot Based on Custom Plot Above"), 
                                    #plotOutput(outputId = "densityplot", height = 200),
                                    h4("Histogram of Xs"), 
                                    plotOutput(outputId = "histplot.x", height = 150),
                                    h4("Histogram of Ys"), 
                                    plotOutput(outputId = "histplot.y", height = 150)
                                    
                           ),
                           
                           
                           
                           # Tab 2: Data
                           tabPanel(title = "Summary", 
                                    br(),
                                    h4("Mean Values Based on color_by option"),br(), 
                                    h5("Higher the Deviation From the Mean of the Values Dangerous the Cluster is"),
                                    h5("In This Case Cluster 1 and 4 Seems the Most Dangerous Clusters -> INSPECT"),
                                    
                                    #DT::dataTableOutput("mytable"),
                                    
                                    tableOutput(outputId="mytable1" ),
                                    #bunu ekledim
                                    
                                    
                                    
                                    tableOutput(outputId="df.print")
                           ),
                           
                           
                           
                           
                           # Tab 2: Data
                           tabPanel(title = "Table", 
                                    br(),
                                    
                                    h4("Interactive Table"), br(),
                                    h5("Recomendend: Table with Sub Total Col Heat map (or Bar Chart) and Count"),
                                    h5("Recomendend: Changing the Place of DES_Regione, DES_Proviancia, Regione and c_I1I2Indice"),
                                   
                           
                           # Tab 3: Data
                           tabPanel(title = "Data", 
                                    br(),br(),
                                    #DT::dataTableOutput( "alldataset"),
                                    DT::dataTableOutput( "alldataset"),
                                    
                                    HTML("Select filetype and variables, then hit 'Download data'."),
                                    br(), br(), # line break and some visual separation
                                    #remove Download button  # downloadButton("download_data", "Download data"),
                                    br(),
                                    tags$a(" Visualize Your Data Table", href="https://seymakalay87.shinyapps.io/app5/")
                                    
                                    
                                    # DT::dataTableOutput(data= ClustereData %>% select(c(2:6),19:21,43:46),
                                    #              options = list(pageLength = 50)
                                    #DT::dataTableOutput(outputId = "moviestable")
                           )
                           
                           
                           
                           
                           
               )
    )
  )
)


#) this is for shinytheme
server <- 
  function(input, output, session) {

    
    output$histplot.x <- renderPlot({
      ggplot(data = ClustereData, aes_string(x = input$x)) +
        geom_histogram(aes(y = ..density..),bins = 100,col="darkgreen",fill="darkgreen")+
        geom_density(alpha=.2, fill="#FF6666",col = "red")
      # stat_function(fun = dnorm ,args = fun_args.x,  col = "blue") 
      
      
      
    })
    

    
    output$histplot.y <- renderPlot({
      ggplot(data = ClustereData, aes_string(x= input$y)) +
        geom_histogram(aes(y = ..density..),bins = 100,col="darkgreen",fill="darkgreen")+
        geom_density(col = "red",alpha=.2, fill="#FF6666")
      # stat_function(fun = dnorm,args = fun_args.y, col = "blue") 
      
    })
    
    
    output$df.print <- renderTable ({
      ClustereData %>%
        #group_by("c_I1I2Indice")%>%
        summarise(mean.indice =round( mean(INDICE), digits = 5),
                  mean.investment =round(  mean(index.oninvestment),digits = 5),
                  mean.protection = round( mean(index.on.prodection),digits = 5))
    })
    
    
    
    
    output$mytable1 <- renderTable ({
      ClustereData %>%
        group_by_(input$z) %>%
        summarize(mean.Indice = round( mean(INDICE), digits = 5), 
                  mean.Index.on.Investment=round(  mean(index.oninvestment), digits = 5), 
                  mean.Index.on.Protection = round( mean(index.on.prodection), digits = 5)) %>%
        arrange(desc(mean.Indice))    
    })
    
    
    
    
    
    
    output$mytable = DT::renderDataTable({
      DT::datatable(data= ClustedMean, options = list(pageLength = 11))
      
    }) 
    
    
    # Create data table
    output$alldataset <- DT::renderDataTable({
      DT::datatable(data = ClustereData %>% select(1:46),
                    options = list(pageLength = 20), 
                    rownames = FALSE)
    })
    

    
   
  
    
    
    
    
    }

shinyApp(ui, server)


