#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(europepmc)
library(tidytext)
library(xml2)
library(dplyr)
library(zip)
library(shinythemes)
library(qdapTools)
source("helper.R")


# tweaks, a list object to set up multicols for checkboxGroupInput
tweaks <- 
    list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 3;    /* Firefox */ 
                                   column-count: 3; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
    ))

# Define UI for application that draws a histogram
ui <- function(request){
    fluidPage(tweaks,
              
              theme = shinytheme("journal"),
              
              #Need to enable 'disabled' function
              useShinyjs(),
              # Application title
              titlePanel(title=div(img(src="logo.svg", width = "70px"), "Abbervimate")),
              #titlePanel("Abbervimate"),
              
              # Setup of the webpage layout
              fluidRow(
                  column(width = 3,
                         wellPanel( 
                             #Term for which abbreviations are needed
                             textInput("srch_term",
                                       "Search term",
                                       placeholder = "Enter you term here"),
                             
                             #Or CSV file with multiple terms
                             
                             fileInput("terms_list", "Or choose CSV File",
                                       multiple = FALSE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             checkboxInput("header", "CSV file with header", TRUE),
                             
                             #Range of date to search
                             dateRangeInput("date_range",
                                            "Published date",
                                            format = "yyyy",
                                            start = "1800-01-01",
                                            end = sprintf("%s-12-31", as.numeric(format(Sys.Date(), "%Y")) + 1),
                                            max = sprintf("%s-12-31", as.numeric(format(Sys.Date(), "%Y")) + 1)),
                             
                             #Include derivatives or not
                             checkboxInput("derivatives", "Include derivatives", TRUE),
                             
                             #Number of records in EuropePMC for search term 
                             textOutput("nhits"),
                             
                             #Number of papers to analyze
                             numericInput("que_limit",
                                          "Maximum number of records",
                                          value = 2,
                                          min = 1,
                                          max = 3000,
                                          step = 1),
                             
                             
                             #Button to start search
                             actionButton("search", "Search", class = "btn-primary"),
                             
                             #Bookmark bottom
                             bookmarkButton(class="btn btn-secondary")
                             
                         )
                         
                  ),
                  
                  #main panel with the search results
                  
                  column(width = 7,
                         tabsetPanel(id = "inTabset",
                                     tabPanel("Read me", 
                                              p("This application is designed to be your
                                                mate in the tedious task - creation of abbreviations dictionary."),
                                              p("The logic behind this application is rather simple:
                                                  in academic papers abbreviations should be indicated
                                                  in parenthesis following the full term. Knowning that,
                                                  we can search all occasions that looks like 'text (txt)'"),
                                              p("This application helps you in this task. Enter the term of interest or
                                                  upload a .csv file with multiple terms (note that search term field has priority over
                                                  uploaded file, so make sure to leave the search term field empty). Then define number of papers to be analyzed,
                                                  and select time range for publication date. Then application will generate requiest
                                                  to EuropePMC database (only open access papers will be searched) and analyze all 
                                                  the papers that match your request (and within the selected limit)."),
                                              p("The output from this analysis is a list of unique character strings with the structure
                                                  'Text (text)' among which you need to identify actual abbreviations."),
                                              p("This application allows you to download the search result as .csv file and use it later, or 
                                                  you can screen through the list in this application and copy-paste the abbreviations to the 
                                                  text box at the right side. By clicking Download dictionaty button, you will 
                                                  download your dictionary as .csv file. The original search term will be appended to your dictionary
                                                  (works only for single term search, in case of multiple terms search add original term manually)")
                                     ),
                                     
                                     
                                     tabPanel("Search results",
                                              h2("Results"),
                                              fluidRow(
                                                  column(width = 6,
                                                         disabled(
                                                             selectInput("show_term",
                                                                         "Which term to show:",
                                                                         choices = list())))),
                                              tabsetPanel(id = "inResults",
                                                          tabPanel("Abbreviation suspects",
                                                                   h4("Potential abbreviations"),
                                                                   tags$div(align = 'left', 
                                                                            class = 'multicol', 
                                                                            checkboxGroupInput(inputId  = 'abbr', 
                                                                                               label = NULL,
                                                                                               inline   = FALSE)),
                                                                   h4("Probably not abbreviations"),
                                                                   tags$div(align = 'left', 
                                                                            class = 'multicol', 
                                                                            checkboxGroupInput(inputId  = 'non_abbr',
                                                                                               label = NULL,
                                                                                               inline   = FALSE))),
                                                          tabPanel("Raw results",
                                                                   br(),
                                                                   fluidRow(
                                                                       column(width = 6,
                                                                              sliderInput("n_of_col",
                                                                                          "Number of columns:",
                                                                                          min = 1,
                                                                                          max = 3,
                                                                                          value = 2)),
                                                                       column(width = 6,
                                                                              #Button to download results
                                                                              disabled(downloadButton("download", "Download raw results")))
                                                                   ),
                                                                   #Output message
                                                                   p(textOutput("N_of_unique")),
                                                                   h5(textOutput("Term_to_show")),
                                                                   p(textOutput("N_of_matches")),
                                                                   #Output table
                                                                   tableOutput("raw_dictionary"))
                                              )
                                     )
                         )
                  ),
                  column(width = 2,
                         textAreaInput("dic", "Your dictionary",
                                       placeholder = "Enter you terms here",
                                       resize = "vertical",
                                       rows = 15),
                         disabled(actionButton("add_dic", "Add selected to dictionary", width = '100%', class = "btn-primary btn-sm")),
                         br(),
                         br(),
                         disabled(downloadButton("download_dic", "Download dictionary", style = "width:100%;", class = "btn-primary btn-sm")),
                         br(),
                         br(),
                         disabled(downloadButton("download_all", "Download all dictionaries", style = "width:100%;", class = "btn-primary btn-sm")))
                  
              )
    )
    
    
}
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$nhits = renderText({
        if (input$srch_term != ""){
            nhits = n_of_hits(srch_term = input$srch_term,
                              dateRange = input$date_range)
            paste0(nhits, " records found")
        } else {" "}
    })
    
    observeEvent(input$search, {
        if (input$srch_term != "") {
            
            collected.data = downSingle(srch_term = input$srch_term,
                                        que_limit = input$que_limit,
                                        dateRange = input$date_range,
                                        derivatives = input$derivatives)
            
            enable("download")
            enable("add_dic")
            
            updateTabsetPanel(session, "inTabset",
                              selected = "Search results")
            
            n_of_data = length(collected.data)
            
            output$Term_to_show = renderText({paste0("Results for ", input$srch_term)})
            
            output$N_of_matches = renderText({if (!(collected.data[1] %in% c("No papers", "No matches"))){
                paste0("The search resulted in ", n_of_data, " matches")
            } else {
                "The search resulted in 0 matches"
            }
                
            })
            
            output$raw_dictionary = renderTable(colnames = FALSE,
                                                striped = TRUE,
                                                bordered = TRUE,
                                                width = '100%',
                                                {if (!(collected.data[1] %in% c("No papers", "No matches"))){
                                                    if (n_of_data %% input$n_of_col == 0) {reps = 0}
                                                    else {reps = input$n_of_col - (n_of_data %% input$n_of_col)}
                                                    #https://www.tutorialspoint.com/how-to-split-a-vector-into-chunks-in-r
                                                    collected.chuncks = split(c(collected.data, rep("", times = reps)),
                                                                              ceiling(seq_along(c(collected.data, rep("", times = reps)))/ceiling(n_of_data/input$n_of_col)))
                                                    collected.chuncks = append(list(as.character(seq(from = 1, to = ceiling(n_of_data/input$n_of_col), by = 1))), collected.chuncks)
                                                } else if (collected.data == "No papers") {
                                                    
                                                    "No articles matching the request"
                                                    
                                                } else if (collected.data == "No matches") {
                                                    
                                                    "Nothing resambles abbreviation"
                                                    
                                                }
                                                })
            
            extended.dictionary = data.mining(collected.data)
            if (countSpaces(input$srch_term) > 0) {
                extended.dictionary = rbind(extended.dictionary,
                                            data.mining.with_spaces(srch_term = input$srch_term,
                                                                    collected.data = collected.data))
            }
            
            
            unique_words = unique(extended.dictionary[,c("word", "abbr")])
            
            unique_abbr = unique_words[which(unique_words$abbr == TRUE), "word"]
            
            unique_non_abbr = unique_words[which(unique_words$abbr == FALSE), "word"]
            
            unique_non_abbr = unique_non_abbr[which(mapply(countSpaces, unique_non_abbr) == 0),]
            
            updateCheckboxGroupInput(session, "abbr",
                                     choices = unique_abbr$word,
                                     selected = unique_abbr$word)
            
            updateCheckboxGroupInput(session, "non_abbr",
                                     choices = unique_non_abbr$word)
            
            enable("download_dic")
            enable("add_dic")
            
            output$download <- downloadHandler(
                filename = function() {
                    paste0(tolower(input$srch_term), ".csv")
                },
                content = function(file) {
                    write.csv(collected.data, file, row.names = FALSE)
                })
            
        } else {
            
            file <- input$terms_list
            ext <- tools::file_ext(file$datapath)
            
            req(file)
            validate(need(ext == "csv", "Please upload a csv file or type the search term"))
            
            list_of_terms = read.csv(file$datapath, header = input$header)
            
            list_of_terms = unlist(list_of_terms)
            
            list_of_terms = unique(list_of_terms)
            
            n_of_terms = length(list_of_terms)
            
            output$N_of_unique = renderText({paste0("File with ", n_of_terms, " unique terms was uploaded.")})
            
            updateTabsetPanel(session, "inTabset",
                              selected = "Search results")
            
            withProgress(message = 'Total progress', value = 0, {
                collected.data = list()
                extended.dictionary = list()
                for (i in 1:n_of_terms){
                    collected.data[[i]] = downSingle(srch_term = list_of_terms[i],
                                                     que_limit = input$que_limit,
                                                     dateRange = input$date_range,
                                                     derivatives = input$derivatives)
                    
                    extended.dictionary[[i]] = data.mining(collected.data[[i]])
                    
                    if (countSpaces(list_of_terms[i]) > 0) {
                        extended.dictionary[[i]] = rbind(extended.dictionary[[i]],
                                                         data.mining.with_spaces(srch_term = list_of_terms[i],
                                                                                 collected.data = collected.data[[i]]))
                    }
                    
                    
                    incProgress(1/n_of_terms)   
                } 
                
            })
            
            enable("download")
            enable("show_term")
            updateSelectInput(session, "show_term",
                              choices = list_of_terms, selected = list_of_terms[1])
            
            
            output$raw_dictionary = renderTable(colnames = FALSE,
                                                striped = TRUE,
                                                bordered = TRUE,
                                                width = '100%',
                                                {if (!(collected.data[[which(list_of_terms == input$show_term)]] %in% c("No papers", "No matches"))){
                                                    unique_words = unique(extended.dictionary[[which(list_of_terms == input$show_term)]][,c("word", "abbr")])
                                                    
                                                    unique_abbr = unique_words[which(unique_words$abbr == TRUE), "word"]
                                                    
                                                    unique_non_abbr = unique_words[which(unique_words$abbr == FALSE), "word"]
                                                    
                                                    unique_non_abbr = unique_non_abbr[which(mapply(countSpaces, unique_non_abbr) == 0),]
                                                    
                                                    updateCheckboxGroupInput(session, "abbr",
                                                                             choices = unique_abbr$word,
                                                                             selected = unique_abbr$word)
                                                    
                                                    updateCheckboxGroupInput(session, "non_abbr",
                                                                             choices = unique_non_abbr$word)
                                                    
                                                    n_of_data = length(collected.data[[which(list_of_terms == input$show_term)]])
                                                    if (n_of_data %% input$n_of_col == 0) {reps = 0}
                                                    else {reps = input$n_of_col - (n_of_data %% input$n_of_col)}
                                                    #https://www.tutorialspoint.com/how-to-split-a-vector-into-chunks-in-r
                                                    collected.chuncks = split(c(collected.data[[which(list_of_terms == input$show_term)]], rep("", times = reps)),
                                                                              ceiling(seq_along(c(collected.data[[which(list_of_terms == input$show_term)]], rep("", times = reps)))/ceiling(n_of_data/input$n_of_col)))
                                                    collected.chuncks = append(list(as.character(seq(from = 1, to = ceiling(n_of_data/input$n_of_col), by = 1))), collected.chuncks)
                                                    
                                                    
                                                    
                                                    
                                                } else if (collected.data[[which(list_of_terms == input$show_term)]] == "No papers") {
                                                    
                                                    "No articles matching the request"
                                                    
                                                } else if (collected.data[[which(list_of_terms == input$show_term)]] == "No matches") {
                                                    
                                                    "Nothing resambles abbreviation"
                                                    
                                                }
                                                })
            
            output$N_of_matches = renderText({n_of_data = length(collected.data[[which(list_of_terms == input$show_term)]])
            if (!(collected.data[[which(list_of_terms == input$show_term)]] %in% c("No papers", "No matches"))){
                paste0("The search resulted in ", n_of_data, " matches")
            } else {
                "The search resulted in 0 matches"
            }
            
            })
            
            enable("add_dic")
            enable("download_dic")
            enable("download_all")
            
            output$Term_to_show = renderText({paste0("Results for ", input$show_term)})
            
            output$download <- downloadHandler(
                filename = function(){paste0("All_raw_tables.zip")},
                content = function(file){
                    files <- NULL;
                    
                    #loop through the sheets
                    for (i in 1:n_of_terms){
                        #write csv file for each search term, save the name
                        fileName <- paste0(gsub(" ", "", tolower(list_of_terms[i])), ".csv")
                        write.csv(collected.data[i], fileName, row.names = FALSE)
                        files <- c(fileName,files)
                    }
                    
                    #create the zip file
                    zip::zip(file,files)
                }
            )
            
            output$download_all <- downloadHandler(
                filename = function(){paste0("All_dictionaries.zip")},
                content = function(file){
                    files <- NULL;
                    
                    #loop through the sheets
                    for (i in 1:n_of_terms){
                        #write csv file for each search term, save the name
                        fileName <- paste0(gsub(" ", "", tolower(list_of_terms[i])), ".csv")
                        unique_words = unique(extended.dictionary[[which(list_of_terms == list_of_terms[i])]][,c("word", "abbr")])
                        unique_abbr = unique_words[which(unique_words$abbr == TRUE), "word"]
                        write.csv(unique_abbr, fileName, row.names = FALSE)
                        files <- c(fileName,files)
                    }
                    
                    #create the zip file
                    zip::zip(file,files)
                }
            )
        }
    })
    
    observeEvent(input$add_dic, {
        updateTextAreaInput(session, "dic",
                            value = {paste(paste(input$abbr, collapse = "\n"),
                                           paste(input$non_abbr, collapse = "\n"),
                                           sep = "\n")})
    })
    
    output$download_dic <- downloadHandler(
        filename = function() {
            paste0("dictionary.csv")
        },
        content = function(file) {
            write.csv(unlist(strsplit(input$dic, "\n")), file, row.names = FALSE)   
        })
    
    
} 

shinyApp(ui = ui, server = server, enableBookmarking = "url")
