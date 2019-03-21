#install.packages("shiny")
#devtools::install_github("Sea2Data/Rstox", ref="develop")

library("shiny")
library(Rstox)

# Get list of survey series
sts<-getNMDinfo("sts", recursive=FALSE)

ui <- fluidPage(
  titlePanel("View survey time series"),
  
# Sidebar layout with input and output definitions ----
sidebarLayout(
  # Sidebar panel for inputs ----
  sidebarPanel(
    # Input: Select a dataset ----
    selectInput("dataset", "Choose a dataset:",
                choices = sts),
    # Input: actionButton() to defer the rendering of output ----
    # until the user explicitly clicks the button (rather than
    # doing it immediately when inputs change). This is useful if
    # the computations required to render output are inordinately
    # time-consuming.
    actionButton("update", "Update View")
  ),

  # Main panel for displaying outputs ----
  mainPanel(
    
    # Output: Header + summary of distribution ----
    h4("Summary"),
    verbatimTextOutput("summary")
    )
)
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  #STStab=getNMDinfo(c("sts", input$variable))
  datasetInput <- eventReactive(input$update, {
    STStab<-input$dataset#getNMDinfo(c("sts", input$dataset))
    }, ignoreNULL = FALSE) 

  output$summary <- renderPrint({
    dataset <- datasetInput()
    #projects <- getNMDdata(dataset)
  print(dataset)
  #summary(projects)
    })
}


# Download all years of the survey time series:
#      system.time(projects <- getNMDdata(mySTS, abbrev=TRUE, subdir=TRUE, ow=TRUE))
    
# Link the data to the projects due to a bug in the zipped files:
#     updateProject(projects[2])
      
#    g <- getBaseline(projects[2])
#   head(g$outputData$ReadBioticXML[["ReadBioticXML_BioticData_Individual.txt"]])

shinyApp(ui = ui, server = server)


