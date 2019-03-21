#install.packages("shiny")

library("shiny")
library(Rstox)

# Get taxa list
taxa0 <- getNMDinfo("taxa")
taxa <- taxa0[1:10,]

# download

ui <- fluidPage(
  titlePanel("Get biotic file"),
  
# Sidebar layout with input and output definitions ----
sidebarLayout(
  # Sidebar panel for inputs ----
  sidebarPanel(
    # Input: Select a dataset ----

    selectInput("taxa_nor", "Choose a species:",
                choices = taxa$Norwegian),
 
    # Input: actionButton() to defer the rendering of output ----
    # until the user explicitly clicks the button (rather than
    # doing it immediately when inputs change). This is useful if
    # the computations required to render output are inordinately
    # time-consuming.
    actionButton("update_nor", "Update View")
  ),

  # Main panel for displaying outputs ----
  mainPanel(
    
    # Output: Header + summary of distribution ----
    h4("Summary"),
    verbatimTextOutput("summary"),
    downloadButton("downloadData", "Download")
  )
  
)
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  datasetInput <- eventReactive(input$update_nor, {
    out<-taxa[which(taxa$Norwegian==input$taxa_nor),]
  }, ignoreNULL = FALSE) 

  output$summary <- renderPrint({
    dataset <- datasetInput()
  print(dataset)
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('http://tomcat7-test.imr.no:8080/apis/nmdapi/biotic/v3/2019/999080/search?version=3.0')
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}
# Run shiny
shinyApp(ui = ui, server = server)


