#install.packages("shiny")
#devtools::install_github("Sea2Data/Rstox", ref="develop")

library("shiny")
library(Rstox)

# Get taxa list
taxa0 <- getNMDinfo("taxa")
taxa <- taxa0[1:30,]

# Get year list
yr <- 2000:2019

# Oppdragstype
op <- getNMDinfo("missiontype")

#Platformtype
#pl <- getNMDinfo("platform")

# download

ui <- fluidPage(
  titlePanel("Filter biotic file"),
  
# Sidebar layout with input and output definitions ----
sidebarLayout(
  # Sidebar panel for inputs ----
  sidebarPanel(
    # Input: Select a dataset ----

    selectInput("taxa_nor", "Choose a species:",
                choices = taxa$Norwegian),
    selectInput("mission", "Choose a missiontype:",
                choices = op$name),
    selectInput("year", "Choose a year:",
                choices = yr),
    
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
    verbatimTextOutput("summary2")
    #downloadButton("downloadData", "Download")
  )
)
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  datasetInput <- eventReactive(input$update_nor, {
    out <-c(taxa[which(taxa$Norwegian==input$taxa_nor),],
    op[which(op$name==input$mission),])
  }) 

  output$summary <- renderPrint({
    dataset <- datasetInput()
  print(dataset)
  })
}
# Run shiny
shinyApp(ui = ui, server = server)


