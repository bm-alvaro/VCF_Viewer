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
                           
                           tabPanel("Search in Dat", 
                                    sidebarPanel(
                                      textInput("directory_dat", "Directory:", value = ""),
                                      textInput("search_string_dat", "Search term:", value = ""),
                                      actionButton("submit_dat", "Search"),
                                      br(),
                                      br(),
                                      downloadButton("download_results_dat", "Download Results")
                                    ),
                                    mainPanel(
                                      tableOutput("results_dat") %>% withSpinner(color="#3E3F3A")
                                    )
                           ),
                           
                           tabPanel("About",
                                    titlePanel("About"), 
                                    div(includeMarkdown("./about.md"), 
                                        align="justify")
                           )
                           
                )
)

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
  
  # New code for the "Search in Dat" tab
  
  search_results_dat <- eventReactive(input$submit_dat, {
    dat_files <- list.files(input$directory_dat, pattern = "\\.txt$", full.names = TRUE)
    
    results_dat <- lapply(dat_files, function(file) {
      dat_data <- readLines(file)
      matching_rows <- grep(input$search_string_dat, dat_data, value = TRUE)
      tibble(File = basename(file), Rows = matching_rows)
    })
    
    results_dat <- do.call(rbind, results_dat)
  })
  
  output$results_dat <- renderTable({
    search_results_dat()
  })
  
  output$download_results_dat <- downloadHandler(
    filename = function() {
      paste("results_dat", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      results_dat <- search_results_dat()
      
      # Column names to be added as a row
      col_names <- c("File", "X.GeneName", "GeneId", "TranscriptId", "BioType", 
                     "variants_impact_HIGH", "variants_impact_LOW", 
                     "variants_impact_MODERATE", "variants_impact_MODIFIER", 
                     "variants_effect_3_prime_UTR_variant", 
                     "variants_effect_5_prime_UTR_premature_start_codon_gain_variant", 
                     "variants_effect_5_prime_UTR_variant", 
                     "variants_effect_downstream_gene_variant", 
                     "variants_effect_initiator_codon_variant", 
                     "variants_effect_intron_variant", 
                     "variants_effect_missense_variant", 
                     "variants_effect_non_coding_transcript_exon_variant", 
                     "variants_effect_non_coding_transcript_variant", 
                     "variants_effect_splice_acceptor_variant", 
                     "variants_effect_splice_donor_variant", 
                     "variants_effect_splice_region_variant", 
                     "variants_effect_start_lost", "variants_effect_stop_gained", 
                     "variants_effect_stop_lost", 
                     "variants_effect_stop_retained_variant", 
                     "variants_effect_synonymous_variant", 
                     "variants_effect_upstream_gene_variant")
      
      # Writing column names as the first row
      write.table(t(col_names), file, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
      
      # Appending the actual data below the column names
      write.table(results_dat, file, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
    }
  )
}

# Run APP
shinyApp(ui, server)