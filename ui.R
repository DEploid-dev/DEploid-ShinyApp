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
                                   textOutput("data"))
                                 ),
                          column(9,
                                 # textOutput("infosource"),
                                 leafletOutput("mymap", height = "500px")
                                 # tableOutput("data")
                                 )
                          
                        ))
             ),
             
             
             #******** tabPanel 1. would be great to have a map here
             tabPanel("Sample infos",
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
             #******** Backup End   
             
             
             ############ tabPanel 2
             tabPanel("Sample sequence exploration",
                      mainPanel(
                        plotOutput("total"),
                        
                        ##### This display item does not work yet...
                        plotOutput("plot")
                      )
                      
             ),
             
             ############ tabPanel 3
             tabPanel("Sequence deconvolution"),
             
             ############ tabPanel 4
             tabPanel("MCMC diagnostic")
             
             
  )
  
)

