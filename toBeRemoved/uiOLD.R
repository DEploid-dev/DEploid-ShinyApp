library(shinythemes)
library(DEploid)

navbarPage("DEploid-R",
           theme = shinytheme("cerulean"),
           ########## tabPanel 1. Data Display
           tabPanel("Raw Data",
             fluidPage(
               sidebarPanel(
                 fileInput("File1", "Choose VCF file",
                           accept=c('text', 'ref', 'txt')),
                 fileInput("File2", "Choose PLAF file",
                           accept=c('text', 'ref', 'txt'))


#                  data.cache(loadWeatherData, cache.name='NRT', station_id='NRT')
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("First 20 Rows of Original Data", tableOutput("orig_data")),
                   tabPanel("First 20 Rows of PLAF Data", tableOutput("plaf_data"))
                 ))
             )),

           ########## tabPanel 2. Total Coverage
           tabPanel("Total Coverage",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          htmlOutput("text")
                        ),
                        mainPanel(
                          plotOutput("total")
                        )
                        )
                   )),

           ########## tabPanel 3. WSAF across Chromosome
           tabPanel("WSAF vs index",
                    fluidPage(
                      fluidRow(
                      # sidebarLayout(
                      #   sidebarPanel(
                           column(4,
                      #            htmlOutput("text4"),
                      selectInput("select", label = h4("Choose a CHROM"),
                                  choices = list("Pf3D7_01_v3" = 1, "Pf3D7_02_v3" = 2, "Pf3D7_03_v3" = 3,
                                                 "Pf3D7_04_v3" = 4, "Pf3D7_05_v3" = 5, "Pf3D7_06_v3" = 6,
                                                 "Pf3D7_07_v3" = 7, "Pf3D7_08_v3" = 8, "Pf3D7_09_v3" = 9,
                                                 "Pf3D7_10_v3" = 10, "Pf3D7_11_v3" = 11, "Pf3D7_12_v3" = 12,
                                                 "Pf3D7_13_v3" = 13, "Pf3D7_14_v3" = 14),
                                        selected = c(1))
                                  ),

                                 #)),



                         column(12,
                               mainPanel(
                                 dygraphOutput("dygraph"))))
                              # ))
                     # )
                    #)
           )),


           ########## tabPanel 4. ALT vs REF
           tabPanel("ALT vs REF",
             fluidPage(
               fixedRow(
                 column(width = 7,
                   htmlOutput("text1")
                 ),
                 column(width = 8,
                        mainPanel(width = 8, height = 12,
                          plotlyOutput("plot")
                        ))

                 )
               )
             ),
           ########## tabPanel 5. WSAF
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
                                        fluidRow(column(6, plotlyOutput("wsaf")))),
                               tabPanel("WSAF v.s.PLAF",
                                        fluidRow(column(6, plotlyOutput("wsvspl"))))

                         ))
                        )
                   ))



)

