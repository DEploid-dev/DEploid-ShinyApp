library(shinythemes)

fluidPage(
  ####################### Browser icon
  headerPanel(tags$head(tags$link(rel = "icon", type = "image/png", href = "deploidTransLowSmall.ico")), windowTitle = "DEploid-ShinyApp" ),

  ####################### Head image
  titlePanel( title = div( img( src = "deploidTrans.png",
                                height = 93,
                                width = 209,
                                style = "margin:1px 1px" ))),

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
                                   fileInput("inputVCFfile", "Choose VCF file",
                                            accept=c('text', 'ref', 'txt')),
                                   actionButton("do", "DEploid"),
                                   hr(),
                                   htmlOutput("panelSampleInfoExplainSample"))
                          ),
                          column(9,
                                 leafletOutput("panelSampleInfoMap", height = "530px")
                          )
                        )
#                      fluidRow(
#                        column(4, tableOutput("panelDataCoverageTable")),
#                        column(4, tableOutput("panelDataPlafTable"))
#                        )
                    )
             ),


             ############ tabPanel 2
             tabPanel("Sequence exploration",
                      htmlOutput("serverDataState"),

                      ### 1. total coverage
                      fluidRow(column(1), column(10, dygraphOutput("panelDataTotalCoverage"))),
                      htmlOutput("panelDataExplainTotalCoverage"),

#                      # ### 2. Alt VS Ref
                      fluidRow(column(4), column(5, align = "center", plotlyOutput("panelDataAltVsRef"))), #, height="550px", width="550px"
                      htmlOutput("panelDataExplainAltVsRef"),
#                      # ### 3. WSAF Histogram
                      fluidRow(column(4), column(4, align = "center", plotlyOutput("panelDataHistWSAF"))),
                      htmlOutput("panelDataExplainHistWSAF"),
#                      # ### 4. WSAF VS PLAF
                      fluidRow(column(4), column(4, align = "center", plotlyOutput("panelDataWSAFVsPLAF"))),
                      htmlOutput("panelDataExplainWSAFVsPLAF")
             ),

             ############ tabPanel 3
             tabPanel("Sequence deconvolution",
                      htmlOutput("severDeconvolutionState"),

                      selectInput("panelSequenceDeconSelectCHROM", label = h4("Choose a CHROMOSOME"),
                                  choices = list("Pf3D7_01_v3" = 1, "Pf3D7_02_v3" = 2, "Pf3D7_03_v3" = 3,
                                                 "Pf3D7_04_v3" = 4, "Pf3D7_05_v3" = 5, "Pf3D7_06_v3" = 6,
                                                 "Pf3D7_07_v3" = 7, "Pf3D7_08_v3" = 8, "Pf3D7_09_v3" = 9,
                                                 "Pf3D7_10_v3" = 10, "Pf3D7_11_v3" = 11, "Pf3D7_12_v3" = 12,
                                                 "Pf3D7_13_v3" = 13, "Pf3D7_14_v3" = 14),
                                  selected = c(1)),
                      dygraphOutput("panelSequenceDeconWSAFVsPOS"),
                      htmlOutput("panelSequenceDeconExplainWSAFVsPOS"),
                      hr(),
                      fluidRow(column(4), column(4, align = "center", plotlyOutput("panelSequenceDeconObsVsExpWSAF"))),
                      hr(),
                      hr()
             ),


             ############ tabPanel 4
             tabPanel("MCMC diagnostic",
                      htmlOutput("severMcMcState"),
                      fluidRow(column(3), column(5, align = "center", plotlyOutput("panelMCMCProportions")))
             ),

             ############ tabPanel 5
             navbarMenu("Documentation",
               tabPanel("Sample Infos"),
               tabPanel("Sequence Exploration"),
               tabPanel("Sequence Deconvolution"),
               tabPanel("Cite me", verbatimTextOutput("citeMe"))
             )
  )
)
