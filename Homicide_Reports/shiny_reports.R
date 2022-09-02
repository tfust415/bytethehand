#Shiny app to generate reports

#State Selection & Year Range
#download PDF
#View PDF

library(shiny)
library(readr)
library(shinyWidgets)
library(dplyr)
library(rmarkdown)
library(tinytex)
library(readxl)
library(foreign)
library(tidyverse)
library(tigris)

supp_homi_fips <- readr::read_rds('supp_homi_fips_20.rds')
spss_supp_homi <- readr::read_rds('spss_supp_homi_20.rds')
supp_homi <- readr::read_rds('supp_homi_20.rds')
spss_supp_homi$CNTYFIPS <- gsub(" ", "", spss_supp_homi$CNTYFIPS, fixed = TRUE)   #subbing out space for fips codes for mapping
all_counties <- readr::read_rds('all_counties_22.rds')
state_fips <- read_tsv('cen_fips.txt')


state_list <- select(supp_homi_fips, State)%>% #removing Dist of Columbia
  filter(State!='District of Columbia')%>%
  filter(State!='PAPSP8')%>%
  unique()

states <- as.list(state_list$State)


# Define UI ----
ui <- fluidPage(
  
  br(),
              fluidRow(column(3, wellPanel(h4(strong('Run and Preview')),
                                           selectInput('state_sel', 'Select a State', choices = states),
                                           numericRangeInput('years', "Select Year Range", value = c(1,2), min=1976, max=2020),
                                           actionButton('run', 'Run Report'), actionButton("note", 'Download Note'))), 
                              
                              mainPanel(uiOutput("pdfview")))
  
  
  
)

# Define server logic ----
server <- function(input, output,session) {
  
  observeEvent(input$note, {
    
    showModal(modalDialog("You can download or print the reports in top right of the pdf preview"))
    
    
  })
  
  observeEvent(input$state_sel,{
  
    
    dates <- select(supp_homi_fips, State, Year)%>%
      filter(State==input$state_sel)
    
    min_year <-min(dates$Year)
    max_year <- max(dates$Year)
    
   updateNumericRangeInput(session,'years', value=c(min_year, max_year))
    
  })
  
  #view button
  observeEvent(input$run,{
    
    showModal(modalDialog("Running Report...", footer=NULL))
    
    state_selection <- input$state_sel
    
    for(i in state_selection){
      
      render(input = "./unsolved_report_shiny.rmd",
             output_format='pdf_document',
             output_file=paste(i),
             output_dir = "./www",
             params=i)
      
      gc(TRUE)
    }
              
               output$pdfview <- renderUI({
                 tags$iframe(style="height:800px; width:100%", src=paste(state_selection,".pdf", sep=''))
               })
               
               
               removeModal()
               
               })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)







#NOTES
#Filters work - but need to check each block for year filters