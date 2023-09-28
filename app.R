### Alvaro Benitez Mateo, 2023 ###
# Right click -> "Select all" -> Control + Enter => Run the APP
library(shiny)
library(tidyverse)
library(furrr)
library(shinythemes)
library(shinycssloaders)

# UI
ui <- fluidPage(theme = shinytheme("sandstone"),
                navbarPage("VCF Viewer:",
                           tabPanel("Search Genes", 
                                    sidebarPanel(
                                      textInput("directory", "Directory:", value = ""),
                                      textInput("search_string", "Search term:", value = ""),
                                      actionButton("submit", "Search"),
                                      br(),
                                      br(),
                                      downloadButton("download_results", "Download Results")
                                    ),
                                    mainPanel(
                                      tableOutput("results") %>% withSpinner(color="#3E3F3A")

                                    )
                           ),
                           
                           tabPanel("About",
                                    titlePanel("About"), 
                                    div(includeMarkdown("D:/Datos/Escritorio/Alvaro/Scripts/GitHub/Scripts-UAL/R/RShiny/VCF_viewer/about.md"), 
                                        align="justify")
                           )
                           
                )
                
)

# Server
server <- function(input, output) {
  search_results <- eventReactive(input$submit, {
    vcf_files <- list.files(input$directory, pattern = "\\.vcf$", full.names = TRUE)
    
    results <- lapply(vcf_files, function(file) {
      vcf_data <- readLines(file)
      matching_rows <- grep(input$search_string, vcf_data, value = TRUE)
      tibble(File = basename(file), Rows = matching_rows)
    })
    
    results <- do.call(rbind, results)
  })
  
  output$results <- renderTable({
    search_results()
  })
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste("results", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      results <- search_results()
      write.table(results, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )
}

# Run APP
shinyApp(ui, server)
