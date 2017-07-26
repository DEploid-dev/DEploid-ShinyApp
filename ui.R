library(shinythemes)

fluidPage(
  ######################## TITLE IMAGE
  titlePanel( title = div( img( src = "deploid.png",
                                height = 93,
                                width = 209,
                                style = "margin:1px 1px"
  )
  )
  ),
  
  ####################### NAV BAR
  navbarPage(title = "", theme = shinytheme("cerulean"),
             
             ########## tabPanel 1. would be great to have a map here
             tabPanel("Sample infos",
                      fluidPage(
                        fluidRow(
                          column(3, 
                                 wellPanel(
                                   radioButtons("sample", "What is this sample?",
                                                c("Plasmodium Falciparum",
                                                  "Plasmodium Vivax")),
                                   hr(),
                                   uiOutput("ui"),
                                   hr(),
                                   htmlOutput("note1"))
                                 ),
                          column(9,
                                 leafletOutput("mymap", height = "530px")
                                 )
                          
                        ))
             ),
             
             
             ############ tabPanel 2
             tabPanel("Sample sequence exploration",
                      fileInput("File1", "Choose VCF file",
                                accept=c('text', 'ref', 'txt')),
                      ### check if data is ready
                      tableOutput("coverage"),
                      tableOutput("plaf"),
                      ### 1. total coverage
                      fluidRow(column(1), column(10, plotOutput("total"))),
                      htmlOutput("note2"),
                      ### 2. Alt VS Ref
                      fluidRow(column(4), column(5, align = "center", plotlyOutput("altvsref"))),
                      htmlOutput("note3"),
                      ### 3. WSAF Histogram
                      fluidRow(column(4), column(4, align = "center", plotlyOutput("wsafhist"))),
                      htmlOutput("note4"),
                      ### 4. WSAF VS PLAF
                      fluidRow(column(4), column(4, align = "center", plotlyOutput("wsvspl"))),
                      htmlOutput("note5")
             ),
             
             ############ tabPanel 3
             tabPanel("Sequence deconvolution"),
             
             ############ tabPanel 4
             tabPanel("MCMC diagnostic")
             
             
  )
  
)

