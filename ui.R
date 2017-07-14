
fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
#      fileInput('refFile', 'Choose tab-delimited Reference allele count',
#                accept=c('text', 'ref', 'txt')),
      #fileInput('altFile', 'Choose tab-delimited Alternative allele count',
      #          accept=c('text', 'alt', 'txt')),
      actionButton("newplot", "Plot alt vs ref")
    ),
    mainPanel(
#      tableOutput(head('contents'))
#      plot(
#      print(head(coverage))
      plotOutput("plot")
    )
  )
)

