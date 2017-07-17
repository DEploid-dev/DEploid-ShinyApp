
library(shinythemes)

navbarPage("DEploid-R",
           theme = shinytheme("flatly"),
           ########## tabPanel 1.
           tabPanel("Raw Data",          
             fluidPage(
               sidebarPanel(
                 fileInput("File1", "Choose VCF file",
                           accept=c('text', 'ref', 'txt')),
                 fileInput("File2", "Choose PLAF file",
                           accept=c('text', 'ref', 'txt'))
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("First 20 Rows of Original Data", tableOutput("orig_data")),
                   tabPanel("First 20 Rows of PLAF Data", tableOutput("plaf_data"))
                 ))


             )),
           
           ########## tabPanel 2.           
           tabPanel("Total Coverage",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          htmlOutput("text")
                        ),
                        fluidRow(
                          column(7,
                                 mainPanel(
                                   plotOutput("total")
                                 ))
                          
                        )
                      )
                   )),


           
           ########## tabPanel 3.           
           tabPanel("ALT vs REF",
             fluidPage(
               sidebarLayout(
                  sidebarPanel(
                    htmlOutput("text1")
                  ),
                  column(6,
                         mainPanel(
                           plotOutput("plot")
                           ), column(4))

                 )
               )
             ),
           ########## tabPanel 4.
           tabPanel("Allele Frequency",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                    htmlOutput("text2"),
                    hr(),
                    htmlOutput("text3")
                 ),
                 mainPanel(
                   tabsetPanel(type = "tabs", 
                               tabPanel("WSAF", 
                                        fluidRow(column(6, plotOutput("wsaf")))),
                               tabPanel("WSAF v.s.PLAF", 
                                        fluidRow(column(6, plotOutput("wsvspl"))))
                   
                         ))
                        )
                   ))
           
                    
             
)

