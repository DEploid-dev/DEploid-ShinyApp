
library(shinythemes)

navbarPage("DEploid",
           theme = shinytheme("flatly"),
           ########## tabPanel 1.
           tabPanel("Uploading Files",          
             fluidPage(
               sidebarPanel(
                 fileInput("vcfFile", "Choose VCF file",
                           accept=c('text', 'ref', 'txt'))
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Original Data", tableOutput("orig_data"))
                 )
               )
             )),
           ########## tabPanel 2.           
           tabPanel("ALT vs REF",
             fluidPage(
               sidebarLayout(
                  sidebarPanel(
                   actionButton("newplot", "Scatter Plot")
                  ),
                  mainPanel(
                    #      tableOutput(head('contents'))
                    #      plot(
                    #      print(head(coverage))
                    plotOutput("plot")

                 )
               )
             ))
           
                    
             
)

