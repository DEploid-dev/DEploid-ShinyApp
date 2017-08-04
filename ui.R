library(shinythemes)

fluidPage(
  ####################### Browser icon
  headerPanel(
    tags$head(tags$link(rel = "icon", type = "image/png",
      href = "deploidTransLowSmall.ico")), windowTitle = "DEploid-ShinyApp"),

  ####################### Head image
  titlePanel(
    title = div(img(src = "deploidTrans.png", height = 93, width = 209,
      style = "margin:1px 1px"))),

  ####################### NAV BAR
  navbarPage(title = "", theme = shinytheme("cerulean"),

    ########## tabPanel 1. would be great to have a map here
    tabPanel("Sample infos",
      sidebarLayout(
        sidebarPanel(width = 3,
          shinyjs::useShinyjs(),
          radioButtons("inputSample", "What is this sample?",
                             c("Plasmodium Falciparum",
                               "Plasmodium Vivax")),
          hr(),
          uiOutput("inputOriginUI"),
          hr(),
          fileInput("inputVCFfile", "Choose VCF file",
                   accept = c("text", "ref", "txt")),
          actionButton("do", "DEploid"),
          hr(),
          HTML("<font color=\"blue\">TODO: advanced parameter turning.</font>"),
          hr(),
          htmlOutput("panelSampleInfoExplainSample")
        ),
        mainPanel(width = 9,
          shinyjs::useShinyjs(),
          leafletOutput("panelSampleInfoMap", height = "530px")
        )
      )
     ),

    ############ tabPanel 2
    tabPanel("Sequence exploration",
      sidebarLayout(
        sidebarPanel(width = 2,
          HTML("<font color=\"blue\">TODO: interactive buttons.</font>")
        ),
        mainPanel(width = 10,
          shinyjs::useShinyjs(),
          tabsetPanel(id = "data",
            tabPanel("Coverage distribution across the genome", value = "p1",
              fluidRow(column(12, dygraphOutput("panelDataTotalCoverage")))
            ),
            tabPanel("Allele read counts", value = "p3",
              fluidRow(column(2),
                column(6, align = "center", plotlyOutput("panelDataAltVsRef")))
#              htmlOutput("panelDataExplainTotalCoverage"),
            ),
            tabPanel("WSAF Histogram", value = "p3",
              fluidRow(column(3),
                column(5, align = "center", plotlyOutput("panelDataHistWSAF")))
#              htmlOutput("panelDataExplainHistWSAF"),
            ),
            tabPanel("WSAF VS PLAF", value = "p4",
              fluidRow(column(3),
                column(5, align = "center",
                  plotlyOutput("panelDataWSAFVsPLAF")))
#              htmlOutput("panelDataExplainWSAFVsPLAF")
            )
          )
        )
      )
    ),

    ############ tabPanel 3
    tabPanel("Sequence deconvolution",
      sidebarLayout(
        sidebarPanel(width = 2,
          downloadButton("downloadHaplotypes", "Download haplotypes"),
          HTML("<font color=\"blue\">TODO: interactive buttons.</font>"),
          uiOutput("inputCHROMUI")
        ),
        mainPanel(width = 10,
          shinyjs::useShinyjs(),
          tabsetPanel(id = "deconvolution",
            tabPanel("Allele frequency across the genome", value = "p1",
              fluidRow(column(12, dygraphOutput("panelSequenceDeconWSAFVsPOS")))
            ),
            tabPanel("Expected vs observed allele frequency", value = "p1",
              fluidRow(column(4), column(4, align = "center",
                plotlyOutput("panelSequenceDeconObsVsExpWSAF")))),
            tabPanel("MCMC diagnostic",
              htmlOutput("severMcMcState"),
              tabsetPanel(id = "mcmc",
                tabPanel("Proportions",
                  fluidRow(
                    column(2)#,
#                   column(5, align = "center",
#                     plotlyOutput("panelMCMCProportions")
                  )
                ),
                tabPanel("LLKs")
              )
            )
          )
        )
      )
    ),

    ############ tabPanel 4
    ############ tabPanel 5
    navbarMenu("Documentation",
      tabPanel("Sample Infos"),
      tabPanel("Sequence Exploration"),
      tabPanel("Sequence Deconvolution"),
      tabPanel("Cite me", verbatimTextOutput("citeMe"))
    )

  )
)
