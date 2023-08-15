#Reading the required files
dataSources <- data.table::fread("dataSource.csv")

#Define a function for adding mapping cardinality
add_mapping_cardinality <- function(dataFile) {
  
  # Calculate counts
  dataFile <- dataFile %>%
    group_by(primaryID) %>% 
    mutate(count_primaryID = n(),
           count_primaryID = ifelse(primaryID == "Entry Withdrawn", 0, count_primaryID)) %>%
    group_by(secondaryID) %>% 
    mutate(count_secondaryID = n()) %>%
    ungroup()
  
  # Add mapping_cardinality column
  dataFile <- dataFile %>%
    mutate(mapping_cardinality = ifelse(count_secondaryID == 1 & count_primaryID == 1,
                                        "1:1", ifelse(
                                          count_secondaryID > 1 & count_primaryID == 1,
                                          "1:n", ifelse(
                                            count_secondaryID == 1 & count_primaryID > 1,
                                            "n:1", ifelse(
                                              count_secondaryID == 1 & count_primaryID == 0,
                                              "1:0", ifelse(
                                                count_secondaryID > 1 & count_primaryID > 1,
                                                "n:n", NA
                                              ))))))
  # Return the updated data
  return(select(dataFile, -c(count_primaryID, count_secondaryID)))
}


# define a function for BridgeDb mapping
Xref_function <- function(identifiers, inputSpecies = "Human",
                          inputSystemCode = "HGNC symbol2alias", outputSystemCode = "All") {
  
  # Preparing the query
  input_source <- dataSources$systemCode[dataSources$source == inputSystemCode]
  if(length(identifiers) != 0) {
    if(length(identifiers) == 1) {
      post_con <- paste0(identifiers, "\t", input_source, "\n")
    } else {
      post_con <- paste0(identifiers, collapse = paste0("\t", input_source, "\n"))
      post_con <- paste0(post_con, "\t", input_source, "\n")
    }
    # Setting up the query url
    url <- "https://webservice.bridgedb.org"
    query_link  <- paste(url, inputSpecies, "xrefsBatch", sep = "/")
    # Getting the response to the query
    res <- tryCatch({
      POST(url = query_link, body = post_con)
    }, error = function(e) {
      message("Error: ", e$message)
      return(NULL)
    })
    # Extracting the content in the raw text format
    out <- content(res, as="text")
    
    if (jsonlite::validate(out)) { # check if JSON string is valid
      res <- fromJSON(json_str = out)
      # Convert to data frame
      df <- do.call(rbind, lapply(names(res), function(name) {
        data.frame(
          identifier = rep(name, length(res[[name]]$`result set`)),
          identifier.source = rep(res[[name]]$datasource, length(res[[name]]$`result set`)),
          target = gsub("^[^:]*:", "", res[[name]]$`result set`),
          target.source = sapply(strsplit(res[[name]]$`result set`, ":"), `[`, 1)
        )
      })) %>% 
        mutate(target.source = dataSources$source[match(target.source, dataSources$to_map)])
      if(outputSystemCode == "All") {
        return(df)
      } else {
        return(df %>% filter(target.source == outputSystemCode))
      }
    } else {
      return(paste0("The response is not a valid JSON string."))
    }
  }
  
}

# Plot theme
plot_theme <- theme_minimal() +
  theme(
    axis.title = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 16, face = "bold")
  )
