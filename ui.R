
library(shinythemes)

navbarPage("DEploid-R",
           theme = shinytheme("flatly"),
           ########## tabPanel 1.
           tabPanel("Raw Data",          
             fluidPage(
               sidebarPanel(
                 fileInput("vcfFile", "Choose VCF file",
                           accept=c('text', 'ref', 'txt')),
                 fileInput("plafFile", "Choose PLAF file",
                           accept=c('text', 'ref', 'txt'))
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("First 20 Rows of Original Data", tableOutput("orig_data")),
                   tabPanel("First 20 Rows of PLAF Data", tableOutput("plaf_data"))
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
             )),
           ########## tabPanel 3.
           tabPanel("Allele Frequency",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   actionButton("af1", "WSAF"),
                   textOutput("text1"),
                   hr(),
                   actionButton("af2", "WSAF v.s.PLAF")
                 ),
                 mainPanel(
                   tabsetPanel(type = "tabs", 
                               tabPanel("WSAF", plotOutput("wsaf")),
                               tabPanel("WSAF v.s.PLAF", plotOutput("wsvspl")),
                               tabPanel("MCMC proportion", plotOutput("prop")),
                               tabPanel("WSAF (observed vs expected", plotOutput("obex"))
                   
                 ))
               )
             ))
           
                    
             
)

