# Read the required files
dataSources <- data.table::fread("dataSource.csv")

# Load tooltip data from file
tooltips <- read.csv("tooltips.txt", sep = "\t")

# Define a function for id type
id_type <- function(type) {
  if(type == "identifierType") { 
    output_dataSource <- 
      selectInput(
        inputId = "sec2priDataSource", 
        label = HTML(
          paste("Choose the datasource&nbsp;<i class='fas fa-info-circle info-icon' data-toggle='tooltip' title='", 
                tooltips$description[tooltips$tooltip == "Data source"], "'></i> :")),
        choices = c("ChEBI", "HMDB", "Wikidata metabolites", "Wikidata genes/proteins", "HGNC Accession number", "NCBI", "UniProt"),
        selected = "ChEBI" 
      )
  } else if(type == "symbolType") {
    output_dataSource <- 
      selectInput(
        inputId = "sec2priDataSource", 
        label = HTML(
          paste("Choose the datasource&nbsp;<i class='fas fa-info-circle info-icon' data-toggle='tooltip' title='",
                tooltips$description[tooltips$tooltip == "Data source"], "'></i> :")),
        choices = c("Metabolite synonym2name", "ChEBI synonym2name", "HMDB synonym2name", "Wikidata synonym2name",
                    "Gene alias2symbol", "HGNC alias2symbol", "NCBI alias2symbol"),
        selected = "ChEBI synonym2name" 
      )
  }
  return(output_dataSource)
}

# Define a function to define the area text value
text_value <- function(sec2priDataSource) {
  text = 
    ifelse(
      sec2priDataSource == "HGNC alias2symbol", "HOXA11\nHOX12\nCD31", 
      ifelse(sec2priDataSource == "HGNC Accession number","HGNC:24\nHGNC:32\nHGNC:13349\nHGNC:7287\n",
             ifelse(sec2priDataSource == "HMDB","HMDB0000005\nHMDB0004990\nHMDB60172\nHMDB00016",
                    ifelse(sec2priDataSource == "HMDB synonym2name","(+)-2-fenchanone\n2-Acyl-1-alkyl-sn-glycerol\nPi-methylhistidine\nCS2\nrenzapride",
                          ifelse(sec2priDataSource == "ChEBI", "CHEBI:20245\nCHEBI:136845\nCHEBI:656608\nCHEBI:4932",
                                 ifelse(sec2priDataSource == "Wikidata metabolites","Q422964\nQ25500867\nQ16634192",
                                        ifelse(sec2priDataSource == "Wikidata genes/proteins","Q21118320\nQ21119955\nQ21122914",
                                               ifelse(sec2priDataSource == "Wikidata synonym2name","antifade\nCS2\n",
                                                      ifelse(sec2priDataSource == "ChEBI synonym2name","(+)-2-fenchanone\n2-Acyl-1-alkyl-sn-glycerol\n2-Hydroxybutanoic acid",
                                                             ifelse(sec2priDataSource == "UniProt","A0A011PKA5\nA0A016SR66\nP9WES5",
                                                                    ifelse(sec2priDataSource == "Metabolite synonym2name","(+)-2-fenchanone\n2-Acyl-1-alkyl-sn-glycerol\nPi-methylhistidine\nCS2\nrenzapride",
                                                                           ifelse(sec2priDataSource == "NCBI","61\n76\n79839\n99",
                                                                                  ifelse(sec2priDataSource == "NCBI alias2symbol","S863-7\nNAT1\nEIEE29",
                                                                                         ifelse(sec2priDataSource == "Gene alias2symbol","S863-7\nNAT1\nEIEE29\nHOXA11\nHOX12\nCD31",
                                                                                                ""))))))))))))))
  return(text)
}

# Define a function to read input dataset
read_input <- function(sec2priDataSource) {
  if(sec2priDataSource == "HGNC Accession number"){
    mapping_table <- data.table::fread("processed_mapping_files/HGNC_secID2priID.tsv")
  } else if(sec2priDataSource == "HGNC alias2symbol"){
    mapping_table <- data.table::fread("processed_mapping_files/HGNC_alias&prev2symbol.tsv")
  } else if(sec2priDataSource == "ChEBI synonym2name"){
    mapping_table <- data.table::fread("processed_mapping_files/ChEBI_synonym2name.tsv")
  } else if(sec2priDataSource == "HMDB synonym2name"){
    mapping_table <- data.table::fread("processed_mapping_files/HMDB_synonym2name.tsv")
  } else if(sec2priDataSource == "Metabolite synonym2name"){
    mapping_table <- dplyr::bind_rows(HMDB = data.table::fread("processed_mapping_files/HMDB_synonym2name.tsv"),
                                ChEBI = data.table::fread("processed_mapping_files/ChEBI_synonym2name.tsv"), 
                                Wikidata = data.table::fread("processed_mapping_files/Wikidata_metabolites_synonym2name.tsv"), .id = "sourceFile")
  } else if(sec2priDataSource == "Wikidata metabolites"){
    mapping_table <- data.table::fread("processed_mapping_files/Wikidata_metabolites_secID2priID.tsv")
  } else if(sec2priDataSource == "Wikidata genes/proteins"){
    mapping_table <- data.table::fread("processed_mapping_files/Wikidata_geneProtein_secID2priID.tsv")
  } else if(sec2priDataSource == "Wikidata synonym2name"){
    mapping_table <- data.table::fread("processed_mapping_files/Wikidata_metabolites_synonym2name.tsv")
  } else if(sec2priDataSource == "Gene alias2symbol"){
    mapping_table <- dplyr::bind_rows(HGNC = data.table::fread("processed_mapping_files/HGNC_alias&prev2symbol.tsv"),
                                NCBI = data.table::fread("processed_mapping_files/NCBI_alias&prev2symbol.tsv") %>%
                                  dplyr::mutate(primaryID = as.character(primaryID)), .id = "sourceFile")
  } else if(sec2priDataSource == "NCBI alias2symbol"){
    mapping_table <- data.table::fread("processed_mapping_files/NCBI_alias&prev2symbol.tsv") %>%
      dplyr::mutate(primaryID = as.character(primaryID))
  } else if(sec2priDataSource == "NCBI"){
    mapping_table <- data.table::fread("processed_mapping_files/NCBI_secID2priID.tsv") %>%
      dplyr::mutate(primaryID = as.character(primaryID), secondaryID = as.character(secondaryID))
  } else {
    mapping_table = data.table::fread(paste0("processed_mapping_files/", sec2priDataSource, "_secID2priID.tsv"))
  }
  return(mapping_table)
}

# Define a function to create identifier list
create_id_list <- function(inputFile, inputText, type) {
  if(!is.null(inputFile)){
    print("Reading identifiers from file...")
    input_ids <- readLines(inputFile)
    # Remove empty or white space-only last line
    last_line <- input_ids[length(input_ids)]
    if(nchar(trimws(last_line)) == 0){
      input_ids <- input_ids[-length(input_ids)]
    }
    # Split identifiers on newline, comma, or space
    input_ids <- unlist(strsplit(input_ids, 
                                 ifelse(
                                   type == "identifierType", '\\"|\n|\t|,|\\s+', '\n|\t'), 
                                 perl = TRUE))
    # Remove empty strings and return the list of identifiers
    input_ids <- input_ids[input_ids != ""]
  } else if(!is.null(inputText)){
    # Split identifiers entered in text area by newline, comma, or space
    input_ids <- as.character(inputText)
    input_ids <- unlist(strsplit(input_ids, 
                                 ifelse(
                                   type == "identifierType", '\n|, |\\s+','\n'),
                                 perl = TRUE))
    # Remove empty strings and return the list of identifiers
    input_ids <- input_ids[input_ids != ""]
  }
  return(as.character(input_ids))
}

# Define a function to read primary IDs
read_primary_input <- function(sec2priDataSource) {
  if(sec2priDataSource == "HGNC Accession number"){
    priID_list <- unique(data.table::fread("processed_mapping_files/HGNC_priIDs.tsv")[["primaryID"]])
  } else if(sec2priDataSource == "HGNC alias2symbol"){
    priID_list <- unique(data.table::fread("processed_mapping_files/HGNC_priIDs.tsv")[["primarySymbol"]])
  } else if(sec2priDataSource == "NCBI"){
    priID_list <- unique(data.table::fread("processed_mapping_files/NCBI_priIDs.tsv")[["primaryID"]])
  } else if(sec2priDataSource == "NCBI alias2symbol"){
    priID_list <- unique(data.table::fread("processed_mapping_files/NCBI_priIDs.tsv")[["primarySymbol"]])
  } else if(sec2priDataSource == "ChEBI synonym2name"){
    priID_list <- unique(data.table::fread("processed_mapping_files/ChEBI_synonym2name.tsv")[["name"]]) ##TODO: check the other file priIDs
  } else if(sec2priDataSource == "HMDB synonym2name"){
    priID_list <- unique(data.table::fread("processed_mapping_files/HMDB_synonym2name.tsv", fill = TRUE)[["name"]]) ##TODO: check the other file priIDs
  } else if(sec2priDataSource == "Wikidata metabolites"){
    priID_list <- unique(data.table::fread("processed_mapping_files/Wikidata_metabolites_priIDs.tsv")[["primaryID"]])
  } else if(sec2priDataSource == "Wikidata genes/proteins"){
    priID_list <- unique(data.table::fread("processed_mapping_files/Wikidata_geneProtein_priIDs.tsv")[["primaryID"]])
  } else if(sec2priDataSource == "Metabolite synonym2name"){
    priID_list <- unique(c(data.table::fread("processed_mapping_files/HMDB_synonym2name.tsv", fill = TRUE)[["name"]], ##TODO: check the other file priIDs
                           data.table::fread("processed_mapping_files/ChEBI_synonym2name.tsv")[["name"]], ##TODO: check the other file priIDs
                           data.table::fread("processed_mapping_files/Wikidata_metabolites_synonym2name.tsv")[["name"]]))
  } else if(sec2priDataSource == "Gene alias2symbol"){
    priID_list <- unique(c(data.table::fread("processed_mapping_files/NCBI_priIDs.tsv", fill = TRUE)[["primarySymbol"]],
                           data.table::fread("processed_mapping_files/HGNC_priIDs.tsv")[["primarySymbol"]]))
  } else if(sec2priDataSource == "Wikidata synonym2name"){
    priID_list <- unique(data.table::fread("processed_mapping_files/Wikidata_metabolites_synonym2name.tsv")[["name"]])
    
  } else {
    priID_list <- unique(unlist(data.table::fread(paste0("processed_mapping_files/", sec2priDataSource, "_priIDs.tsv"))))
  }
  return(priID_list)
}

# Define a list of primary identifiers
get_primary_ids <- function(type, inputIdentifierList, priID_list) {
  primaryIDs <- as.character(
    intersect(
      unique(inputIdentifierList),
      priID_list)
    )
  return(primaryIDs)
}

# Define a list of secondary identifiers
get_secondary_ids <- function(type, inputIdentifierList, mapping_table) {
  secondaryIDs <- as.character(
    intersect(
      unique(inputIdentifierList),
      unique(mapping_table[[grep(
        ifelse(type == "identifierType", 
               "secondaryID", "secondarySymbol|synonym"),
        colnames(mapping_table), value = TRUE)]]))
    )
  return(secondaryIDs)
}

# Define a list of unknown identifiers
get_unknown_ids <- function(type, inputIdentifierList, priID_list, mapping_table) {
  primaryIDs <- get_primary_ids(type, inputIdentifierList, priID_list)
  secondaryIDs <- get_secondary_ids(type, inputIdentifierList, mapping_table)
  unknownIDs <- setdiff(setdiff(
    unique(inputIdentifierList), 
    primaryIDs),
    secondaryIDs)
  return(unknownIDs)
}

# Define a list of both secondary and primary identifiers
get_sec_pri_ids <- function(type, inputIdentifierList, priID_list, mapping_table) {
  primaryIDs <- get_primary_ids(type, inputIdentifierList, priID_list)
  secondaryIDs <- get_secondary_ids(type, inputIdentifierList, mapping_table)
  secPriIDs <- intersect(primaryIDs, secondaryIDs)
  return(secPriIDs)
}

# Define a function to count the input primary and secondary ids
count_id_group <- function(type, inputIdentifierList, priID_list, mapping_table) {
  primaryIDs <- get_primary_ids(type, inputIdentifierList, priID_list)
  secondaryIDs <- get_secondary_ids(type, inputIdentifierList, mapping_table)
  secPriIDs <- get_sec_pri_ids(type, inputIdentifierList, priID_list, mapping_table)
  unknownIDs <- get_unknown_ids(type, inputIdentifierList, priID_list, mapping_table)
  
  freq_table = data.frame(
    type = c(
      "#input IDs",
      "#pri_sec",
      "#primary",
      "#secondary",
      "#unknown"),
    no = c(
      length(unique(inputIdentifierList)),
      length(secPriIDs),
      length(primaryIDs),
      length(secondaryIDs),
      length(unknownIDs))
  )
  return(freq_table)
}

# Define a function to create the output table
create_output_table <- function(sec2priDataSource, inputIdentifierList, mapping_table) {
  if(grepl("alias2symbol", sec2priDataSource)){
    sec2pri_table_output <- mapping_table %>% 
      dplyr::filter(
        secondarySymbol %in% c(inputIdentifierList)) %>%
      dplyr::select(
        secondarySymbol, primarySymbol, primaryID, comment) %>%
      dplyr::rename(
        `input (secondary)` = secondarySymbol, 
        `primary symbol` = primarySymbol, 
        `primary ID` = primaryID) %>%
      dplyr::arrange(`input (secondary)`, `primary ID`)
  } else if(grepl("synonym2name", sec2priDataSource)){
    sec2pri_table_output <- mapping_table %>% 
      dplyr::filter(
        synonym %in% c(inputIdentifierList)) %>%
      dplyr::select(
        synonym, name, primaryID) %>%
      dplyr::rename(
        `input (secondary)` = synonym,
        `primary ID` = primaryID) %>%
      dplyr::arrange(
        `input (secondary)`, `primary ID`)
  } else {
    sec2pri_table_output <- mapping_table %>% 
      dplyr::filter(secondaryID %in% c(inputIdentifierList)) %>%
      dplyr::select(secondaryID, primaryID) %>%
      dplyr::rename(
        `input (secondary)` = secondaryID,
        `primary ID` = primaryID) %>%
      dplyr::mutate(
        `input (secondary)` = as.character(`input (secondary)`)) %>%
      dplyr::arrange(
        `input (secondary)`, `primary ID`)
  }
  return(sec2pri_table_output)
}

# Define a function for the metadata
create_metadata <- function(sec2priDataSource) {
  sourceVersion <- read.table("processed_mapping_files/dataSourceVersion.tsv", 
                              sep = "\t", header = TRUE, as.is = TRUE)
  sourceVersion <- sourceVersion[match(gsub(" .*", "", sec2priDataSource), sourceVersion$datasource),]
  sourceVersion <- paste0("The data was ", sourceVersion$type, " on ", "<b>", sourceVersion$date, "</b>", ifelse(sourceVersion$type == "queried", " from ", " in "), 
                          paste0("<a href='", sourceVersion$website, "' target='_blank'>", gsub(" .*", "", sec2priDataSource) ," database</a>"), 
                          ifelse(is.na(sourceVersion$version), ".", paste0(" (version: ", sourceVersion$version, ").")))
  
  if(sec2priDataSource == "Metabolite synonym2name"){
    sec2pri_metadata <- renderText(HTML("Data is available from <b>HMDB</b>, <b>ChEBI</b>, and <b>Wikidata</b>."))
  } else if(sec2priDataSource == "Gene alias2symbol"){
    sec2pri_metadata <- renderText(HTML("Data is available from <b>HGNC</b> and <b>NCBI</b>."))
  } else {
    sec2pri_metadata <- renderText(HTML(sourceVersion))
  }
  return(sec2pri_metadata)
}

# Define a function for the bar chart
create_plot <- function(freq_table, IDtype){
  color_palette <- c(`#unknown` = "lightgray",
                     `#primary` = "#96b77d",
                     `#secondary` = "#7da190")
  color_palette <- color_palette[names(color_palette) %in% freq_table$type]
  row_order <- c(`#unknown` = 3,  `#primary` = 2, `#secondary` = 1)

  ggplot2::ggplot(
    freq_table %>% 
      dplyr::filter(!type %in% c("#input IDs", "#pri_sec"), no != 0) %>%
      dplyr::mutate(row_order = row_order[match(type, names(row_order))]),
    ggplot2::aes(x = reorder(type, row_order), y = no, fill = type)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.9), width = 0.9) +
    ggplot2::scale_fill_manual(values = color_palette) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "none",
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 16),
      plot.title = ggplot2::element_text(size = 16, face = "bold")
    ) + 
    ggplot2::ggtitle(ifelse(is.na(freq_table$no[freq_table$type == "#input IDs"]), "",
                            paste0(freq_table$no[freq_table$type == "#input IDs"], 
                                   ifelse(IDtype == "identifierType",
                                          " (unique) input identifiers", 
                                          " (unique) input symbols/names"), ":"))) +
    ggplot2::labs(subtitle = ifelse(freq_table$no[freq_table$type == "#pri_sec"] == 0, "",
                                    paste0(freq_table$no[freq_table$type == "#pri_sec"], " (unique) input", 
                                           ifelse(freq_table$no[freq_table$type == "#pri_sec"] == 1, " is ", "s are "), 
                                           "both primary and secondary."))) +
    ggplot2::geom_text(ggplot2::aes(y = no, label = no), size = 6,
                       position = ggplot2::position_stack(vjust = .5)) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(1, 1, -0.5, 1), "cm"))
}

# Define a list of primary identifiers
primary_id_list <- function(type, inputIdentifierList, priID_list, mapping_table, sec2pri_table) {
  primaryIDs <- get_primary_ids(type, inputIdentifierList, priID_list)
  if("name" %in% colnames(mapping_table)){
    primaryIDs <- mapping_table$primaryID[mapping_table$name %in% primaryIDs]
  } else {
    primaryIDs <- mapping_table$primaryID[mapping_table$primarySymbol %in% primaryIDs]

  }
  primary_id <- unique(c(sec2pri_table$`primary ID`, primaryIDs))
  return(primary_id)
}

# Define a function to define output file name
idrefiner_filename = function(sec2priDataSource, download_format) {
  paste0("IDRefiner_mapping_", 
         gsub(" ", "_", sec2priDataSource), ".", download_format)
}

# Define a function to create download table in tsv or csv format
create_tsvORcsv_output <- function(type, inputIdentifierList, sec2priDataSource, priID_list, mapping_table, sec2pri_table) {
  primaryIDs <- get_primary_ids(type, inputIdentifierList, priID_list)
  unknownIDs <- get_unknown_ids(type, inputIdentifierList, priID_list, mapping_table)
  secPriIDs <- get_sec_pri_ids(type, inputIdentifierList, priID_list, mapping_table)
  
  if(length(primaryIDs) == 0){
    output <- sec2pri_table
  } else if(length(primaryIDs) != 0){
    if(type == "identifierType"){
      output <- dplyr::bind_rows(
        sec2pri_table,
        data.frame(
          `input (secondary)` = primaryIDs,
          `primary ID` = primaryIDs,
          comment = "The input is a primary ID.", 
          check.names = FALSE))

    } else if(grepl("alias2symbol", sec2priDataSource)){
      output <- dplyr::bind_rows(
        sec2pri_table,
        data.frame(`input (secondary)` = primaryIDs, 
                   `primary symbol` = primaryIDs, 
                   comment = "The input is a primary gene symbol.", check.names = FALSE) %>%
          mutate(`primary ID` = mapping_table$primaryID[match(`primary symbol`, mapping_table$primarySymbol)])
      )
      
      
    } else if(sec2priDataSource %in% c("Metabolite synonym2name", "ChEBI synonym2name", "HMDB synonym2name", "Wikidata synonym2name")){
      output <- dplyr::bind_rows(
        sec2pri_table,
        data.frame(`input (secondary)` = primaryIDs, 
                   name = primaryIDs, 
                   comment = "The input is a primary metabolite name.", check.names = FALSE) %>%
          mutate(`primary ID` = mapping_table$primaryID[match(name, mapping_table$name)])
      )
    }
  }
  
  # Add unknown IDs
  if(length(unknownIDs) != 0)
    output <- dplyr::bind_rows(
      output, data.frame(
        `input (secondary)` = unknownIDs, 
        comment = "The input is unknown.", check.names = FALSE))
  
  # Add a comment for ambiguous IDs (secPriIDs)
  if(length(secPriIDs) != 0){
    output <- output %>%
      mutate(
        comment = ifelse(
          `input (secondary)` %in% secPriIDs, 
          paste0(
            "NOTE: The input is ambiguous, meaning that based on the database it has been used as both a secondary and a primary name or symbol. ",
            comment),
          comment
          )
      )
  }
  return(output)
}

# Define a function to get datasource version
get_source_version <- function(sec2priDataSource, mapping_table){
  sourceVersion <- read.table("processed_mapping_files/dataSourceVersion.tsv", sep = "\t", header = TRUE, as.is = TRUE)
  sourceVersion$metadata <- paste0(
    "The data was ", 
    sourceVersion$type,
    ifelse(sourceVersion$type == "queried", " from ", " on "),
    sourceVersion$website,
    ifelse(sourceVersion$type == "queried", " on ", " and downloaded on "), 
    sourceVersion$date, 
    ifelse(sourceVersion$type == "queried", ".", paste0(" (version: ", sourceVersion$version, ")."))
  )
  return(sourceVersion)
}

# Define a function for adding mapping cardinality
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
    mutate(mapping_cardinality = ifelse(
      count_secondaryID == 1 & count_primaryID == 1,
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

# Define a function for adding mapping predicate
add_predicate <- function(dataFile) {
  # Add mapping_cardinality column
  dataFile <- dataFile %>%
    mutate(
      predicate_id = ifelse(
        mapping_cardinality %in% c("1:n", "n:n"), #the secondary ID that Split into multiple OR multiple secondary IDs merged/splited into multiple primary IDs
        "oboInOwl:consider", ifelse(
          mapping_cardinality %in% c("1:1", "n:1", "1:0"), #the secondary ID replace by new ID OR multiple secondary IDs merged into one primary ID
          "IAO:0100001", NA
        )))
  
  # Return the updated data
  return(dataFile)
}

# Define a function to create download table in SSSOM format
create_sssom_output <- function(type, inputIdentifierList, sec2priDataSource, sourceVersion, priID_list, mapping_table, sec2pri_table) {
  primaryIDs <- get_primary_ids(type, inputIdentifierList, priID_list)
  unknownIDs <- get_unknown_ids(type, inputIdentifierList, priID_list, mapping_table)
  secPriIDs <- get_sec_pri_ids(type, inputIdentifierList, priID_list, mapping_table)
  
  if(sec2priDataSource %in% c("HGNC Accession number", "NCBI")){
    if(length(primaryIDs) == 0){
      output <- mapping_table %>% 
        dplyr::filter(secondaryID %in% c(inputIdentifierList)) %>% # input is secondary ID 
        dplyr::select(
          secondaryID,
          secondarySymbol,
          predicateID,
          primaryID,
          primarySymbol,
          mapping_cardinality_sec2pri,
          comment,
          source)
    } else {
      output <- dplyr::bind_rows(
        mapping_table %>% 
          dplyr::filter(secondaryID %in% c(inputIdentifierList)) %>% # input is secondary ID 
          dplyr::select(
            secondaryID, 
            secondarySymbol, 
            predicateID, 
            primaryID, 
            primarySymbol, 
            mapping_cardinality_sec2pri, 
            comment, 
            source) %>%
          dplyr::mutate(secondaryID = as.character(secondaryID)),
        mapping_table %>% 
          dplyr::filter(primaryID %in% primaryIDs) %>% # input is primary ID 
          dplyr::select(primaryID, primarySymbol, source) %>%
          dplyr::mutate(
            secondaryID = as.character(primaryID),
            secondarySymbol = primarySymbol,
            predicateID = "skos:exactMatch",
            mapping_cardinality_sec2pri = NA,
            comment = "The input is a primary ID.")
      )
    }
    output <- output %>% 
      dplyr::rename(
        subject_id = secondaryID,
        subject_label = secondarySymbol,
        predicate_id = predicateID,
        object_id = primaryID, 
        object_label = primarySymbol,
        mapping_cardinality = mapping_cardinality_sec2pri)
  } else if(grepl("alias2symbol", sec2priDataSource)){
    if(length(primaryIDs) == 0){
      output <- mapping_table %>% 
        dplyr::filter(secondarySymbol %in% c(inputIdentifierList)) %>% # input is secondary ID 
        dplyr::select(
          secondarySymbol,
          predicateID,
          primaryID, 
          primarySymbol,
          mapping_cardinality_sec2pri,
          comment,
          source) %>%
        dplyr::mutate(
          secondaryID = NA,
          primaryID = as.character(primaryID))
    } else {
      output <- dplyr::bind_rows(
        mapping_table %>% 
          dplyr::filter(secondarySymbol %in% c(inputIdentifierList)) %>% # input is secondary ID 
          dplyr::select(
            secondarySymbol, 
            predicateID,
            primaryID,
            primarySymbol, 
            mapping_cardinality_sec2pri, 
            comment, 
            source) %>%
          dplyr::mutate(primaryID = as.character(primaryID)),
        mapping_table %>% 
          dplyr::filter(primarySymbol %in% primaryIDs) %>% # input is primary ID 
          dplyr::select(
            primaryID,
            primarySymbol,
            source) %>%
          dplyr::mutate(
            primaryID = as.character(primaryID),
            secondaryID = primaryID,
            secondarySymbol = primarySymbol,
            predicateID = "skos:exactMatch",
            mapping_cardinality_sec2pri = NA,
            comment = "The input is a primary ID.")
      ) 
    }
    output <- output %>%
      dplyr::rename(
        subject_id = secondaryID,
        subject_label = secondarySymbol, 
        predicate_id = predicateID,
        object_id = primaryID, 
        object_label = primarySymbol, 
        mapping_cardinality = mapping_cardinality_sec2pri) 
  } else if(sec2priDataSource == "UniProt"){
    if(length(primaryIDs) == 0){
      output <- mapping_table %>% 
        dplyr::filter(secondaryID %in% c(inputIdentifierList)) %>% # input is secondary ID 
        dplyr::select(
          secondaryID, 
          predicateID, 
          primaryID, 
          mapping_cardinality_sec2pri, 
          comment, 
          source)
    } else {
      output <- dplyr::bind_rows(
        mapping_table %>% 
          dplyr::filter(secondaryID %in% c(inputIdentifierList)) %>% # input is secondary ID 
          dplyr::select(
            secondaryID,
            predicateID, 
            primaryID, 
            mapping_cardinality_sec2pri, 
            comment,
            source),
        mapping_table %>% 
          dplyr::filter(primaryID %in% primaryIDs) %>% # input is primary ID 
          dplyr::select(primaryID, source) %>%
          dplyr::mutate(
            secondaryID = primaryID,
            predicateID = "skos:exactMatch",
            mapping_cardinality_sec2pri = NA,
            comment = "The input is a primary ID.")
      )}
    output <- output %>%
      dplyr::rename(
        subject_id = secondaryID, 
        predicate_id = predicateID, 
        object_id = primaryID, 
        mapping_cardinality = mapping_cardinality_sec2pri) 
  } else if(sec2priDataSource %in% c("ChEBI", "HMDB", "Wikidata genes/proteins", "Wikidata metabolites")){
    mapping_table <- mapping_table %>% add_mapping_cardinality() %>% add_predicate()
    if(length(primaryIDs) == 0){
      output <-  mapping_table %>% 
        dplyr::filter(secondaryID %in% c(inputIdentifierList)) # input is secondary ID 
    } else {
      output <- dplyr::bind_rows(
        mapping_table %>% 
          dplyr::filter(secondaryID %in% c(inputIdentifierList)), # input is secondary ID
        mapping_table %>% 
          dplyr::filter(primaryID %in% primaryIDs) %>% # input is primary ID 
          dplyr::select(primaryID) %>%
          dplyr::mutate(
            secondaryID = primaryID,
            predicate_id = "skos:exactMatch",
            mapping_cardinality = NA,
            comment = "The input is a primary ID.")
      )}
    output <- output %>% dplyr::rename(subject_id = secondaryID, object_id = primaryID)
  } else if(grepl("synonym2name", sec2priDataSource)){
    if(length(primaryIDs) == 0){
      output <-  mapping_table %>% 
        dplyr::filter(synonym %in% c(inputIdentifierList)) %>% # input is secondary ID 
        dplyr::mutate(
          secondaryID = NA,
          predicate_id = NA,
          mapping_cardinality = NA)
    } else {
      output <- dplyr::bind_rows(
        mapping_table %>% 
          dplyr::filter(synonym %in% c(inputIdentifierList)), # input is secondary ID
        mapping_table %>% 
          dplyr::filter(name %in% primaryIDs) %>% # input is primary ID 
          dplyr::select(primaryID, name, sourceFile) %>%
          dplyr::mutate(
            secondaryID = primaryID,
            synonym = name,
            predicate_id = NA,
            mapping_cardinality = NA,
            comment = "The input is a metabolite name.")
      )}
    output <- output %>% dplyr::rename(
      subject_id = secondaryID, 
      subject_label = synonym, 
      object_id = primaryID, 
      object_label = name)
    if(sec2priDataSource == "Metabolite synonym2name"){
      output <- output %>%
        dplyr::mutate(source = sourceVersion$metadata[match(sourceFile, sourceVersion$datasource)]) 
    }
  }
  
  # Add unknown IDs
  if(length(unknownIDs) != 0){
    unknownData <- data.frame(
      subject = unknownIDs,
      comment = "The input is unknown.", check.names = FALSE)
    colnames(unknownData)[1] = ifelse(grepl("synonym2name|alias2symbol", sec2priDataSource), "subject_label", "subject_id")
    output <- dplyr::bind_rows(output, unknownData) 
  }
  
  # Add a comment for ambiguous IDs (secPriIDs)
  if(length(secPriIDs) != 0){
    output$comment <- ifelse(
      output$subject_label %in% secPriIDs | output$subject_id %in% secPriIDs, 
      paste0(
        "NOTE: The input is ambiguous, meaning that based on the database it has been used as both a secondary and a primary name or symbol. ",
        output$comment),
      output$comment)
                             
  }
  
  output <- output %>%
    dplyr::select(dplyr::one_of(
      c("subject_id",
        "subject_label",
        "predicate_id",
        "object_id",
        "object_label",
        "mapping_cardinality",
        "comment",
        "source"))) %>%
    unique()
  
  return(output)
}

# Define a function to write the sssom file
write_sssom_tsv <- function(input_data, output_file, source) {
  # Define the CURIE map
  curie_map <- list(
    "CHEBI:" = "http://purl.obolibrary.org/obo/CHEBI_",
    "HMDB:" = "http://www.hmdb.ca/metabolites/",
    "Wikidata:" = "http://www.wikidata.org/entity/",
    "UniProt:" = "http://identifiers.org/uniprot/",
    "HGNC:" = "http://identifiers.org/hgnc/",
    "NCBI:" = "http://www.ncbi.nlm.nih.gov/gene/",
    "skos:" = "http://www.w3.org/2004/02/skos/core#",
    "owl:" = "http://www.w3.org/2002/07/owl#"
  )
  
  
  # Write the CURIE map as comments in the output file
  curie_comments <- paste0("# curie_map:\n")
  for(curie in names(curie_map)) {
    curie_comments <- paste0(curie_comments, "#   ", curie, " ", curie_map[[curie]], "\n")
  }

  if(source != "") curie_comments <- paste0(curie_comments, "# source: ", source, "\n")
  if(any(colnames(input_data) == "source") && length(unique(input_data$source[!is.na(input_data$source)])) == 1){
    source = unique(input_data$source[input_data$source != "NA"][!is.na(input_data$source)])
    curie_comments <- paste0(curie_comments, "# source: ", source, "\n")
    input_data$source = NULL
  }
  
  # Write the SSSOM data frame and CURIE map comments to the output file
  writeLines(curie_comments, con = output_file, sep = "")
  write.table(input_data, file = output_file, sep = "\t", quote = FALSE, row.names = FALSE, append = TRUE)
}

# Define a function for BridgeDb mapping
Xref_function <- function(identifiers, inputSpecies = "Human",
                          inputSystemCode = "HGNC", outputSystemCode = "All") {
  
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
    
    if(jsonlite::validate(out)) { # check if JSON string is valid
      res <- rjson::fromJSON(json_str = out)
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



