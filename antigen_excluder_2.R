library(shiny)
library(dplyr)

# Read data (You would replace this with reading from 'data_excluder.csv')
data <- read.csv("data_excluder.csv")
antigen_info <- read.csv("antigen_info.csv")


# Define a function to determine the highest ranking value
calculateResult <- function(row) {
  values <- as.character(unlist(row[2:length(row)]))
  rank <- c("+", "+/0", "w", "+/w", "0")
  # Find the highest ranking value
  highestRank <- rank[min(match(values, rank, nomatch = length(rank)))]
  
  # Find the lowest ranking value
  lowestRank <- rank[max(match(values, rank, nomatch = length(rank)))]
  
  # Return NA if the highest rank is 0
  if (lowestRank == "0") {
    return(NA)
  } else {
    return(lowestRank)
  }
  
  return(lowestRank)
}

# Define UI
ui <- fluidPage(
  titlePanel("Antibody Test Selector with Results"),
  HTML("<p>Instructions and Key:</p>
       
       <li>1. Select at least one antigen and one antibody</li>
       <li>2. As you select multiple tests, the algorithm chooses the result with least residual antibody positivity</li>
       <li>3. Reach out to s.raza@mail.utoronto.ca to suggest more tests!</li>
       </br>
       <li>0 = antibody nonreactive</li>
       <li>+/0 = some examples reactive, others nonreactive</li>
       <li>w = reactions weakened (weak antibodies may be nonreactive)</li>
       <li>+/w = some examples reactive, others show weakened reactions</li>
       <li>+ = antibody reactive</li>
       </br>"),  # Instructions text
  actionButton("reset", "Reset"),
  actionButton("checkAll", "Check All Antibodies"),  # Add a button to check all antibodies
  actionButton("highFreq", "High Frequency"),  # Add a button for high frequency antibodies
  actionButton("lowFreq", "Low Frequency"),  # Add a button for high frequency antibodies
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(4,
               checkboxGroupInput("antibodyInput", "Select Antibodies:",
                                  choices = data$Antibody)
        ),
        column(4,
               checkboxGroupInput("testInput", "Select Tests:",
                                  choices = colnames(data)[-1])
        )
      )
    ),
    mainPanel(
      fluidRow(
        column(6,
               tableOutput("selectedData")
        ),
        column(6,
               
               HTML("<b>Non-Excluded Antibodies:</b>"),
               
               uiOutput("resultDisplay")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$highFreq, {
    # Get the list of high frequency antibodies
    highFreqAntibodies <- antigen_info$Antibody[antigen_info$High_Freq == 1]
    # Update the selected antibodies
    updateCheckboxGroupInput(session, "antibodyInput", selected = highFreqAntibodies)
  })
  
  
  observeEvent(input$lowFreq, {
    # Get the list of high frequency antibodies
    lowFreqAntibodies <- antigen_info$Antibody[antigen_info$Low_Freq == 1]
    # Update the selected antibodies
    updateCheckboxGroupInput(session, "antibodyInput", selected = lowFreqAntibodies)
  })
  
  
  observeEvent(input$reset, {
    # Reset checkboxes
    updateCheckboxGroupInput(session, "antibodyInput", selected = character(0))
    updateCheckboxGroupInput(session, "testInput", selected = character(0))
  })
  
  observeEvent(input$checkAll, {
    # Select all antibodies
    updateCheckboxGroupInput(session, "antibodyInput", selected = data$Antibody)
  })
  
  
  observeEvent(input$reset, {
    # Reset checkboxes
    updateCheckboxGroupInput(session, "antibodyInput", selected = character(0))
    updateCheckboxGroupInput(session, "testInput", selected = character(0))
  })
  
  output$selectedData <- renderTable({
    selectedData <- data[data$Antibody %in% input$antibodyInput, c("Antibody", input$testInput)]
    selectedData
  })
  
  output$resultDisplay <- renderUI({
    # Check if any antibodies and tests are selected
    if (length(input$antibodyInput) > 0 && length(input$testInput) > 0) {
      selectedData <- data[data$Antibody %in% input$antibodyInput, c("Antibody", input$testInput)]
      # Add result column
      selectedData$result <- apply(selectedData, 1, calculateResult)
      # Display only Antibody and result columns
      resultTable <- selectedData %>% select(Antibody, result)
      return(tableOutput("resultTable"))
    } else {
      return(HTML("<p>Please select at least one enzyme and antibody to display results</p>"))
    }
  })
  
  output$resultTable <- renderTable({
    selectedData <- data[data$Antibody %in% input$antibodyInput, c("Antibody", input$testInput)]
    # Add result column
    selectedData$result <- apply(selectedData, 1, calculateResult)
    # Display only Antibody and result columns
    
    ### remove null antibodies
    selectedData <- selectedData %>% filter(!is.na(result))
    
    ### display table
    resultTable <- selectedData %>% select(Antibody, result)
    resultTable
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

