#Function to remove html tags from the text of paper
cleanFun = function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

#Build search pattern
srch_pattern = function(srch_term = input$srch_term,
                        derivatives = input$derivatives){
  if (derivatives == TRUE) {
    search_pattern = paste0("(\\S*",
                            gsub(" ", ".?", tolower(srch_term)),
                            "\\w*[ ][(].*?[)])")
  } else {
    search_pattern = paste0("(\\b",
                            gsub(" ", ".?", tolower(srch_term)),
                            "\\b[ ][(].*?[)])")
  }
  
  return(search_pattern)
  
}

#Number of records
n_of_hits = function(srch_term = input$srch_term,
                     dateRange = input$date_range){
  
  start = format(dateRange[1], "%Y")
  end = format(dateRange[2], "%Y")
  query = paste0("\"", tolower(srch_term), "\"",
                 " AND (FIRST_PDATE:[", start, " TO ", end,
                 "]) AND (OPEN_ACCESS:y)")
  n_of_hits = europepmc::epmc_hits(query = query)
  
}

#Download records list function
downSingle = function (srch_term = input$srch_term,
                       que_limit = input$que_limit,
                       dateRange = input$date_range,
                       derivatives = input$derivatives) {
  notif = showNotification("Loading records from EuropePMC")
  
  start = format(dateRange[1], "%Y")
  end = format(dateRange[2], "%Y")
  
  query = paste0("\"", tolower(srch_term), "\"",
                 " AND (FIRST_PDATE:[", start, " TO ", end,
                 "]) AND (OPEN_ACCESS:y)")
  
  search_results = europepmc::epmc_search(query = query,
                                          limit = que_limit,
                                          verbose = FALSE)
  
  #remove cases when pmcid is NA
  search_results = search_results[complete.cases(search_results[,"pmcid"]),]
  
  removeNotification(notif)
  
  if (!is.null(search_results)) {
    search_pattern = srch_pattern(srch_term = srch_term,
                                  derivatives = derivatives)
    
    collected.data = c()
    #https://shiny.rstudio.com/articles/progress.html
    withProgress(message = 'Analyzing data', value = 0, {
      for (i in 1:nrow(search_results)){
        full_paper = europepmc::epmc_ftxt(search_results$pmcid[i])
        splitted_paper = trimws(xml_find_all(xml_children(xml_children(full_paper)), "//p"))
        splitted_paper = cleanFun(splitted_paper)
        n_of_parag = length(splitted_paper)
        if(n_of_parag > 0){
          splitted_paper.df = data.frame(parag = 1:n_of_parag,
                                         text = splitted_paper)
          token.df = (splitted_paper.df %>%
                        unnest_tokens(sentence, text, token = "sentences"))
          collected.data = c(collected.data,
                             as.vector(stringr::str_extract_all(string = token.df$sentence,
                                                  pattern = search_pattern,
                                                  simplify = TRUE)))
          collected.data = collected.data[!is.na(collected.data)]
          
        }
        #https://shiny.rstudio.com/articles/progress.html
        incProgress(1/nrow(search_results))
      }
    })
    collected.data = unique(collected.data)
    if (length(collected.data) == 0) {
      collected.data = "No matches"
    }
  } else {
    
    collected.data = "No papers"
    
  }
  
  return(collected.data)
}

#Identification of abbreviations
is.abbr = function(term, abbr){
  pattern = sapply(strsplit(abbr, ""),
                   FUN = function(x){paste("\\b",
                                           paste(unlist(strsplit(x, "")), collapse = ".*"),
                                           sep = "")})
  return(mapply(grepl, pattern = pattern, x = term, USE.NAMES = FALSE))
}

#Extract term from the string
extract.term = function(string){
  require(stringr)
  return(gsub(" \\(", "", str_extract(string, ".*\\(")))
}

#Deeper analyze collected data to find actual abbreviations
data.mining = function(collected.data = collected.data){
  extended_dictionary = tibble(raw = collected.data,
                               term = extract.term(collected.data),
                               raw2 = collected.data)
  
  extended_dictionary = extended_dictionary %>%
    unnest_tokens(word, raw2)
  
  extended_dictionary$abbr = is.abbr(term = extended_dictionary$term,
                                     abbr = extended_dictionary$word)
  
  extended_dictionary$word.length = nchar(extended_dictionary$word)
  
  extended_dictionary$number = !is.na(as.numeric(extended_dictionary$word))
  
  extended_dictionary = extended_dictionary[which(extended_dictionary$word.length > 1 & 
                                                    extended_dictionary$number == FALSE),
                                            c("raw", "term", "word", "abbr")]
    return(extended_dictionary)
} 

data.mining.with_spaces = function(srch_term = input$srch_term,
                                   collected.data = collected.data){
  require(qdapTools)
  n_of_spaces = countSpaces(srch_term)
  
  extended_dictionary = tibble(raw = collected.data,
                               term = extract.term(collected.data))
  
  word_sets = mapply(set_extractor, extended_dictionary$raw,
                     MoreArgs = list(n_of_spaces = n_of_spaces))
  word_sets = list2df(word_sets, col1 = "word", col2 = "raw")
  extended_dictionary = merge(extended_dictionary, word_sets)
  
  extended_dictionary$abbr = is.abbr(term = extended_dictionary$term,
                                     abbr = extended_dictionary$word)
  
  extended_dictionary$word.length = nchar(extended_dictionary$word)
  
  extended_dictionary$number = !is.na(as.numeric(extended_dictionary$word))
  
  extended_dictionary = extended_dictionary[which(extended_dictionary$word.length > 1 & 
                                                    extended_dictionary$number == FALSE),
                                            c("raw", "term", "word", "abbr")]
  
  
  
  return(extended_dictionary)
}

#https://stackoverflow.com/questions/12403312/find-the-number-of-spaces-in-a-string
countSpaces <- function(s) { sapply(gregexpr(" ", s), function(p) { sum(p>=0) } ) }

set_extractor = function(x, n_of_spaces = 1){
  pattern = paste0("(?=(\\b\\S+", paste(rep("\\s\\b\\S+", times = n_of_spaces), collapse = ""), "\\b))")
  found_matches = str_match_all(x, '(?=(\\b\\S+\\s\\b\\S+\\b))')[[1]][, 2]
  return(found_matches)
}
