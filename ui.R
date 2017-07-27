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
                                   radioButtons("inputSample", "What is this sample?",
                                                c("Plasmodium Falciparum",
                                                  "Plasmodium Vivax")),
                                   hr(),
                                   uiOutput("inputOriginUI"),
                                   hr(),
                                   htmlOutput("panelSampleInfoExplainSample"))
                                 ),
                          column(9,
                                 leafletOutput("panelSampleInfoMap", height = "530px")
                                 )
                          
                        ))
             ),
             
             
             ############ tabPanel 2
             tabPanel("Sample sequence exploration",
                      fluidRow(
                        column(4, fileInput("inputVCFfile", "Choose VCF file",
                                           accept=c('text', 'ref', 'txt'))),
                        ### check if data is ready
                        column(4, tableOutput("panelDataCoverageTable")),
                        column(4, tableOutput("panelDataPlafTable"))),
                      
                      
                      ### 1. total coverage
                      fluidRow(column(1), column(10, plotOutput("panelDataTotalCoverage"))),
                      htmlOutput("panelDataExplainTotalCoverage"),
                      
                      # ### 2. Alt VS Ref
                      fluidRow(column(4), column(5, align = "center", plotlyOutput("panelDataAltVsRef"))),
                      htmlOutput("panelDataExplainAltVsRef"),
                      # ### 3. WSAF Histogram
                      fluidRow(column(4), column(4, align = "center", plotlyOutput("panelDataHistWSAF"))),
                      htmlOutput("panelDataExplainHistWSAF"),
                      # ### 4. WSAF VS PLAF
                      fluidRow(column(4), column(4, align = "center", plotlyOutput("panelDataWSAFVsPLAF"))),
                      htmlOutput("panelDataExplainWSAFVsPLAF")
             ),
             
             ############ tabPanel 3
             tabPanel("Sequence deconvolution"),
             
             ############ tabPanel 4
             tabPanel("MCMC diagnostic")
             
             
  )
  
)

