#Empty the R environment
rm(list = ls())
options(shiny.appmode = "shiny")

# Load required packages
library(dplyr)
library(httr)
library(ggplot2)
library(shiny)
library(DT)
library(data.table)
library(rjson)
library(shinyBS)

# Load required functions and data
source("functions.R")

options(rsconnect.max.bundle.files = 3145728000)

#### Shinny App
ui <- fluidPage(
  shinyjs::useShinyjs(), # needed for download button to work
  tags$head(
    tags$style(
      HTML('<hr style = "border-color: #0088cc;">
           <style>
             .main-panel {
               padding-top: 60px; /* adjust this value as needed */
             }
             .my-plot {
               height: 300px; /* set the height of the plot */
               margin-bottom: 10px; /* add a small margin at the bottom */
             }
             .my-table {
               height: 500px; /* set the height of the table */
               overflow-y: auto; /* add a vertical scrollbar if necessary */
             }
             .navbar {
               margin-bottom: 0px !important;
               margin-top: 0px!important!
             }
             .tab-content {
               padding-top: 0px !important;
               min-height: 570px;
             }
             .input-group {
               margin-bottom: 0px;
             }
         </style>
      ')
    )
  ),# Set the page title and add a horizontal line separator
  # Add a title panel
  titlePanel(
    div(
      div(
        div(
          h5(""),
          strong("Omics IDRefiner"),
          style = "float:left;justify-content: flex-end;"
        ),
        div(
          imageOutput("Maastricht_logo"),
          style = "display: flex; align-items: right; justify-content: flex-end;"
        ),
        style = "display:flex; justify-content: space-between;margin: 0px; height: 70px;"
      ),
      div(style = "margin-top: -15px"),
      div(
        h4("A user friendly dashboard for refining OMICS identifiers and cross-referencing mappings across different biological datasources."),
        style = "text-align: left;"
      )
    )
  ),
  # Add a tabset panel with three tabs
  navbarPage(
    title = NULL,
    # Tab 1: About
    tabPanel(
      "About", 
      icon = icon("book"),
      align = "justify",
      # Add a summary section
      h3(strong("Summary")),
      # onclick = "ga('send', 'event', 'click', 'link')",
      p("Biological entities such as genes, proteins, complexes, and metabolites often have diverse identifiers across various databases, which pose a challenge for data integration. To solve this problem, identifier mapping is required to convert identifiers from one database to corresponding entities from other databases."),
      # imageOutput("bridgeDb_logo", height = "70px"),
      # p("BridgeDb (bridgedb.org) is an open source tool introduced in 2010 that connects identifiers from various biological databases and related resources, facilitating data harmonization and multi-omics analysis. BridgeDb is an ELIXIR Recommended Interoperability Resource (RIR) and provides mappings for genes and proteins for 35 species, metabolites, metabolic reactions, diseases, complexes, human coronaviruses, and publications. It includes:",
      #   tags$ul(
      #     tags$li("a Java library for software integration,"),
      #     tags$li("a REST-API for programmatic access from any programming language,"),
      #     tags$li("a dedicated R package,"),
      #     tags$li("a Python package,"),
      #     tags$li("and example code for Matlab integration through the webservice."),
      #   )
      # ),
      br(),
      h4(strong("The secondary identifier challenge")),
      p("After mapping identifiers from one database to another, integrating data can remain a challenge due to the presence of", 
        HTML("<b>retired, deleted, split, and/or merged identifiers</b>"), 
        " currently present in databases and datasets alike. These outdated identifiers are called",
        HTML("<b><span style='font-size:15px;'>“secondary”</span></b>"),
        " while the identifiers currently supported by databases are referred to as",
        HTML("<b><span style='font-size:15px;'>“primary”</span></b>"),
        ". The presence of secondary identifiers in a used dataset or database can lead to information loss and hinders effective data integration. While some tools exist to convert secondary identifiers to current ones, these tools only support one type of data (either genes/proteins or metabolites): ",
        tags$ul(
          tags$li("https://www.genenames.org/tools/multi-symbol-checker/"),
          tags$li("https://www.metaboanalyst.ca/MetaboAnalyst/upload/ConvertView.xhtml")
        ),
        "These tools currently do not have an API or other form of programmatic access, leading to issues in big OMICS data analysis."),
      br(),
      h4(strong("OMICS IDRefiner")),
      p("To address the challenges of integrating data from different biological sources that contain secondary identifiers, we developed a user-friendly Shiny app called OMICS IDRefiner, which provides two key functions:",
        tags$ul(
          tags$li(style = "list-style-type: decimal;",
                  HTML("<b><span style='font-size:15px;'>IDRefiner:</span></b>"),
                  br(),
                  "provides statistics on the percentage of secondary identifiers in the dataset and converts outdated secondary identifiers to current primary identifiers, if available.",
                  "The IDRefiner functionality currently covers secondary identifiers from",
                  HTML("<b>HGNC</b>"), ", ",
                  HTML("<b>HMDB</b>"), ", ",
                  HTML("<b>ChEBI</b>"), "and ",
                  HTML("<b>Wikidata</b>"),
                  "which can be converted to the corresponding primary identifier from the initial database.",
                  "After this step, the IDMapper can be used to convert the primary-ID-enhanced dataset to any other database currently supported by BridgeDb:",
                  tags$ul(
                    tags$li(
                      "The full overview of supported databases is available on the BridgeDb website (bridgedb.org/pages/system-codes)."
                    )
                  )
          ),
          tags$li(style = "list-style-type: decimal;",
                  HTML("<b><span style='font-size:15px;'>IDMapper:</span></b>"),
                  br(),
                  "uses BridgeDb's REST-API to convert identifiers.")
          
        )
      ),
      p("The metadata for the latest update of the mapping files is also available to users for queries within the app."),
      br(),
      p(HTML("<b>Future development </b>"),
        "entails updating the Secondary-to-Primary identifier mapping files regularly via GitHub Actions to ensure accuracy."),
      br(),
      hr(),
      # Add a citation section
      # h4("How to Cite BridgeDb"),
      # p("van Iersel, Martijn P et al. “The BridgeDb framework: standardized access to gene, protein and metabolite identifier mapping services.” BMC bioinformatics vol. 11 5. 4 Jan. 2010."),
      # p("doi:10.1186/1471-2105-11-5")
    ),
    # Tab 2: Sec2pri
    tabPanel(
      "IDRefiner", 
      icon = icon("table"),
      div(
        style = "margin-top: 15px;",
        # Add a sidebar layout
        sidebarLayout(
          # Add a sidebar panel with input controls
          sidebarPanel(
            div(style = "margin-top: -10px"),
            # Render the input options for selecting a identifier type
            radioButtons ("type", "Mapping type:", inline = TRUE,
                          c ("Identifier" = "identifierType", "Symbol/Name" = "symbolType"),
                          selected = "Identifier" 
            ),
            div(style = "margin-top: -10px"),
            # Render the input options for selecting a data source
            uiOutput('dataSource'),
            # Add a file input for uploading a text file containing identifiers
            fileInput(
              "sec2pri_identifiers_file",
              "Upload identifiers File",
              accept = c(".csv", ".xlsx", ".xls", ".tsv", ".txt"),
              placeholder = "Please upload file.."
            ),
            div(style = "margin-top: -30px"),
            # Add a text area input for entering identifiers
            textAreaInput(
              'sec2pri_identifiers', 
              'or insert identifier(s) here', 
              value = "", 
              width = NULL, 
              placeholder = 'one identifier per row'
            ),
            # Add buttons for performing the identifier mapping and clearing the list
            div(style = "margin-top: -10px"),
            div(
              actionButton(
                "sec2pri_get", "mapping",
                style = "color: white; background-color: gray; border-color: black"),
              actionButton(
                "sec2pri_clear_list", "Clear",
                style = "color: white; background-color: gray; border-color: black"),
              br(),
              br(),
              div(style = "margin-top: -10px"),
              selectInput(
                inputId = "sec2pri_download_format",
                label = "Choose a download format:",
                choices = c("csv", "tsv", "sssom.tsv"),
                selected = "tsv"
              ),
              div(style = "margin-top: -10px"),
              downloadButton(
                outputId = "sec2pri_download",
                label = "Download results", 
                style = "color: white; background-color: gray; border-color: black"
              ),
              div(style = "margin-top: -10px")
            ),
            width = 3
          ),
          # Add a main panel for displaying the bridge list
          mainPanel(
            div(htmlOutput("sec2pri_metadata")),
            div(plotOutput("sec2pri_piechart_results", height = "300px"), class = "my-plot"),
            div(DTOutput("sec2pri_mapping_results"), style = "margin-top: -100px;"),
            width = 9
          )
        )
      )
    ),
    # Tab 3: BridgeDb
    tabPanel(
      "IDMapper", 
      icon = icon("table"),
      div(
        style = "margin-top: 15px;",
        # Add a sidebar layout
        sidebarLayout(
          # Add a sidebar panel with input controls
          sidebarPanel(
            div(style = "margin-top: -10px"),
            
            # Render the input options for selecting a identifier type
            radioButtons ("type", "Choose identifier type:", inline = TRUE,
                          c ("Gene/Protein" = "gene", "Metabolites" = "metabolite"),
                          selected = "metabolite" 
            ),
            # Render the input options for selecting a species
            conditionalPanel(
              condition = "input.type == 'gene'",
              uiOutput('inputSpecies')
            ),
            div(style = "margin-top: -5px"),
            # Add a file input for uploading a text file containing identifiers
            fileInput(
              "BridgeDb_identifiers_file",
              "Upload identifiers File",
              accept = c(".csv", ".xlsx", ".xls", ".tsv", ".txt"),
              placeholder = "Please upload file.."
            ),
            div(style = "margin-top: -30px"),
            # Add a text area input for entering identifiers
            textAreaInput(
              'BridgeDb_identifiers',
              'or insert identifier(s) here',
              value = NULL,
              width = NULL,
              placeholder = 'one identifier per row'
            ),
            div(style = "margin-top: -10px"),
            # Render the input options for selecting a data source
            uiOutput('inputDataSource'),
            div(style = "margin-top: -10px"),
            # Render the input options for selecting an output data source
            uiOutput('outputDataSource'),
            # Add buttons for performing the identifier mapping and clearing the list
            div(style = "margin-top: -10px"),
            div(
              actionButton(
                "BridgeDb_get", "mapping",
                style = "color: white; background-color: gray; border-color: black"),
              actionButton(
                "BridgeDb_clear_list", "Clear",
                style = "color: white; background-color: gray; border-color: black"),
              br(),
              br(),
              div(style = "margin-top: -10px"),
              selectInput(
                inputId = "BridgeDb_download_format",
                label = "Choose a download format:",
                choices = c("csv", "tsv"),
                selected = "tsv"
              ),
              div(style = "margin-top: -10px"),
              downloadButton(
                outputId = "BridgeDb_download", 
                label = "Download results", 
                style = "color: white; background-color: gray; border-color: black"
              ),
              div(style = "margin-top: -10px")
            ),
            width = 3
          ),
          # Add a main panel for displaying the bridge list
          mainPanel(
            div(DTOutput("BridgeDb_mapping_results", height = "500px")),
            width = 9
          )
        )
      ),
      style = "height: 300px;"
    ),
    # Tab 4: Contact us
    tabPanel(
      "Contact us",
      icon = icon("envelope"),
      div(
        style = "margin-top: 15px;",
        # Add a contact us section
        br(),
        p(HTML("<b><span style='font-size:16px;'>For questions and comments:</span></b>")),
        p(HTML("<b>Tooba Abbassi-Daloii</b>"), ": t.abbassidaloii@maastrichtuniversity.nl"),
        br(),
        p("Department Bioinformatics - BiGCaT"),
        p("NUTRIM, Maastricht University, Maastricht, The Netherlands")
      )
    ),
    inverse = T
  ),
  div(
    style = "display: flex; flex-direction: column;",
    div(style = "flex: 0;",
    ),
    div(
      # imageOutput("bridgeDb_logo_wide", height = "70px"),
      style = "background-color: white; text-align: center; padding: 10px;"
    ),
    div(
      p("licensed under the ", a("Apache License, version 2.0", href = "https://www.apache.org/licenses/LICENSE-2.0")),
      style = "text-align: center; padding: 50px;"
    )
  )
)

server <- function(input, output, session) {
  #Maastricht university logo
  output$Maastricht_logo <- renderImage({
    list(src = "www/Maastricht.png",
         width = "280px",
         height = "70px", 
         contentType = "image/png")
  }, deleteFile = FALSE)
  
  #sec2pri tab
  #Define the input options based on identifier or symbol/name
  observe({
    if(input$type == "identifierType") { 
      output$dataSource <- renderUI({
        selectInput(
          inputId = 'sec2priDataSource', 
          label = 'Choose the data source:',
          choices = c("ChEBI","HMDB", "Wikidata", "HGNC Accession number", "UniProt"),
          selected = "ChEBI" 
          )
        })
    } else if(input$type == "symbolType") {
      output$dataSource <- renderUI({
        selectInput(
          inputId = 'sec2priDataSource', 
          label = 'Choose the data source:',
          choices = c("Metabolite name2synonym", "Gene symbol2alias", "ChEBI name2synonym", "HMDB name2synonym", "Wikidata name2synonym", "HGNC symbol2alias"),
          selected = "ChEBI name2synonym" 
        )
      })
    }
  })


  
  # Update the TextArea based on the selected database
  observeEvent(input$sec2priDataSource, {
    updateTextAreaInput(session, "sec2pri_identifiers",
                        value = ifelse(input$sec2priDataSource == "HGNC symbol2alias", "HOXA11\nHOX12\nCD31", 
                                       ifelse(input$sec2priDataSource == "HGNC Accession number","HGNC:24\nHGNC:32\nHGNC:13349\nHGNC:7287\n",
                                              ifelse(input$sec2priDataSource == "HMDB","HMDB0000005\nHMDB0004990\nHMDB60172\nHMDB00016",
                                                     ifelse(input$sec2priDataSource == "ChEBI", "CHEBI:20245\nCHEBI:136845\nCHEBI:656608\nCHEBI:4932",
                                                            ifelse(input$sec2priDataSource == "Wikidata","Q422964\nQ65174948\nQ25436441",
                                                                   ifelse(input$sec2priDataSource == "HMDB name2synonym","Pi-methylhistidine\nTrimethylenediamine\nalpha-oxo-N-Butyrate",
                                                                          ifelse(input$sec2priDataSource == "ChEBI name2synonym","(+)-2-fenchanone\n2-Acyl-1-alkyl-sn-glycerol\n2-Hydroxybutanoic acid",
                                                                                 ifelse(input$sec2priDataSource == "UniProt","A0A011PKA5\nA0A016SR66\nP9WES5",
                                                                                        ifelse(input$sec2priDataSource == "Metabolite name2synonym","(+)-2-fenchanone\n2-Acyl-1-alkyl-sn-glycerol\nPi-methylhistidine",
                                                                                               "")))))))))
                        )
    })
  
  #Check the input
  seq2pri_input_file <- reactiveVal(NULL)
  observeEvent(input$sec2pri_identifiers_file, {
    if(!is.null(input$sec2pri_identifiers_file)){
      seq2pri_input_file(input$sec2pri_identifiers_file)
    }
  })
  
  #Function to make a vector for input identifiers
  secIdentifiersList <- reactive({
    if(!is.null(seq2pri_input_file())){
      print("Reading identifiers from file...")
      input_ids <- readLines(seq2pri_input_file()$datapath)
      # Remove empty or whitespace-only last line
      last_line <- input_ids[length(input_ids)]
      if(nchar(trimws(last_line)) == 0){
        input_ids <- input_ids[-length(input_ids)]
      }
      # Split identifiers on newline, comma, or space
      input_ids <- unlist(strsplit(input_ids, ifelse(input$type == "identifierType", '\\"|\n|\t|,|\\s+', '\n|\t'), perl = TRUE))
      # Remove empty strings and return the list of identifiers
      input_ids[input_ids != ""]
      input_ids
    } else if(!is.null(input$sec2pri_identifiers)){
      # Split identifiers entered in text area by newline, comma, or space
      input_ids <- as.character(input$sec2pri_identifiers)
      input_ids <-  unlist(strsplit(input_ids, ifelse(input$type == "identifierType",'\n|,|\\s+','\n'), perl = TRUE))
      # Remove empty strings and return the list of identifiers
      input_ids <- input_ids[input_ids != ""]
      input_ids
    }
  })
  
  # Function to read data related to selected datasource  
  read_data_all <- reactive({
    req(input$sec2priDataSource) 
    if(input$sec2priDataSource == "HGNC Accession number"){
      dataset <- data.table::fread("processed_mapping_files/HGNC_secID2priID.tsv")
    } else if(input$sec2priDataSource == "HGNC symbol2alias"){
      dataset <- data.table::fread("processed_mapping_files/HGNC_symbol2alia&prev.tsv")
    } else if(input$sec2priDataSource == "ChEBI name2synonym"){
      dataset <- data.table::fread("processed_mapping_files/ChEBI_name2synonym.tsv")
    } else if(input$sec2priDataSource == "HMDB name2synonym"){
      dataset <- data.table::fread("processed_mapping_files/HMDB_name2synonym.tsv")
    } else if(input$sec2priDataSource == "Metabolite name2synonym"){
      dataset <- dplyr::bind_rows(HMDB = data.table::fread("processed_mapping_files/HMDB_name2synonym.tsv"),
                                  ChEBI = data.table::fread("processed_mapping_files/ChEBI_name2synonym.tsv"), .id = "sourceFile")
    } else if(input$sec2priDataSource == "Gene name2synonym"){
      ##:TODO:
    } else {
      dataset = data.table::fread(paste0("processed_mapping_files/", input$sec2priDataSource, "_secID2priID.tsv"))
    }
  })
  
  # Function to read primary ids/symbols/names related to selected datasource 
  read_data_primary <- reactive({
    req(input$sec2priDataSource) 
    if(input$sec2priDataSource == "HGNC Accession number"){
      priID_list <- unique(data.table::fread("processed_mapping_files/HGNC_priIDs.tsv")[["primaryID"]])
    } else if(input$sec2priDataSource == "HGNC symbol2alias"){
      priID_list <- unique(data.table::fread("processed_mapping_files/HGNC_priIDs.tsv")[["primarySymbol"]])
    } else if(input$sec2priDataSource == "ChEBI name2synonym"){
      priID_list <- unique(data.table::fread("processed_mapping_files/ChEBI_name2synonym.tsv")[["name"]])
    } else if(input$sec2priDataSource == "HMDB name2synonym"){
      priID_list <- unique(data.table::fread("processed_mapping_files/HMDB_name2synonym.tsv", fill = TRUE)[["name"]])
    } else if(input$sec2priDataSource == "Metabolite name2synonym"){
      priID_list <- unique(c(data.table::fread("processed_mapping_files/HMDB_name2synonym.tsv", fill = TRUE)[["name"]],
                             data.table::fread("processed_mapping_files/ChEBI_name2synonym.tsv")[["name"]]))
    } else if(input$sec2priDataSource == "Gene name2synonym"){
      ##:TODO:
    } else {
      priID_list <- unlist(data.table::fread(paste0("processed_mapping_files/", input$sec2priDataSource, "_priIDs.tsv"))) 
    }
  })
  
  # Function to calculate the number of primary and secondary identifiers in the input table
  sec2pri_proportion <- reactive({
    req(input$sec2priDataSource) 
    # (1) calculate the number of primary and secondary identifiers in the input table
    if(!is.null(input$sec2pri_identifiers_file) | !is.null(input$sec2pri_identifiers)) {
      dataset <- read_data_all()
      priID_list <- read_data_primary()
      proportion_table = data.frame(
        type = c("#input IDs", 
                 "#primary IDs",
                 "#secondary IDs", 
                 "#unknown"),
        no = c(length(unique(secIdentifiersList())),
               length(intersect(unique(secIdentifiersList()), priID_list)),
               length(intersect(unique(secIdentifiersList()), dataset[[grep(ifelse(input$type == "identifierType", "secondaryID", "secondarySymbol|synonym"), colnames(dataset), value = TRUE)]])),
               length(unique(secIdentifiersList())) - 
                 (length(intersect(unique(secIdentifiersList()), priID_list)) +
                    length(intersect(unique(secIdentifiersList()), dataset[[grep(ifelse(input$type == "identifierType", "secondaryID", "secondarySymbol|synonym"), colnames(dataset), value = TRUE)]])))
        )
      )# %>% mutate(prop = no/length(unique(secIdentifiersList())))
      return(proportion_table[proportion_table$no != 0, ])
    }
  })
  
  # Function to make the output table
  sec2pri_output <- reactive({
    req(input$sec2priDataSource)
    if(!is.null(input$sec2pri_identifiers_file) | !is.null(input$sec2pri_identifiers)) {
      dataset <- read_data_all()
      if(grepl("HGNC symbol", input$sec2priDataSource)){
        seq2pri_table_output <- dataset %>% 
          filter(secondarySymbol %in% c(secIdentifiersList())) %>%
          select(secondarySymbol, primarySymbol, primaryID, comment) %>%
          dplyr::rename(input = secondarySymbol, `primary symbol` = primarySymbol, `primary ID` = primaryID) %>%
          dplyr::arrange(input, `primary ID`)
      } else if (grepl("name2synonym", input$sec2priDataSource)){
        seq2pri_table_output <- dataset %>% 
          filter(synonym %in% c(secIdentifiersList())) %>%
          select(synonym, name, primaryID) %>%
          dplyr::rename(input = synonym, `primary ID` = primaryID) %>%
          dplyr::arrange(input, `primary ID`)
      } else {
        seq2pri_table_output <- dataset %>% 
          filter(secondaryID %in% c(secIdentifiersList())) %>%
          select(secondaryID, primaryID) %>%
          dplyr::rename(input = secondaryID, `primary ID` = primaryID) %>%
          dplyr::arrange(input, `primary ID`)
      }
      return(seq2pri_table_output)
    }
  })
  
  # Function to clear previous outputs
  # clearPreviousSec2priOutputs <- function() {
  #   updateTextAreaInput(session, "sec2pri_identifiers", value = "")
  #   # Reset file input appearance
  #   js_reset_file_input <- "$('#sec2pri_identifiers_file').val(null); $('.custom-file-label').html('Please upload file..');"
  #   session$sendCustomMessage(type = 'jsCode', message = js_reset_file_input)
  #   seq2pri_mapping$seq2pri_pieChart <- NULL
  #   seq2pri_mapping$metadata <- NULL
  # }
  
  seq2pri_mapping <- reactiveValues(seq2pri_pieChart = NULL, metadata = NULL, seq2pri_table = NULL)
  
  observeEvent(input$sec2pri_get, {
    seq2pri_mapping$seq2pri_pieChart <- 
      # Function to draw the piechart
      ggplot(sec2pri_proportion() [c(-1), ],
             aes(x = type, y = no, fill = type)) +
        geom_col(position = position_dodge(0.9), width = 0.9) +
        scale_fill_brewer(palette = "Blues") +
        coord_flip() +
        plot_theme + 
        ggtitle(ifelse(is.na(sec2pri_proportion()$no[1]), "No input provided",
                             paste0(sec2pri_proportion()$no[1], ifelse(input$type == "identifierType", " (unique) input identifiers", " (unique) input symbols/names")))) +
        geom_text(aes(y = no, label = no), size = 6,
                  position = position_stack(vjust = .5)) +
        theme(plot.margin = unit(c(1, 1, -0.5, 1), "cm"))

    if(nrow(sec2pri_output()) != 0) {
      seq2pri_mapping$seq2pri_table <- req(
        DT::datatable(sec2pri_output(),
                      options = list(orderClasses = TRUE,
                                     lengthMenu = c(10, 25, 50, 100),
                                     pageLength = 10)
        )
      )
    } 
  }, ignoreInit = TRUE)
  
  # Update output display
  output$sec2pri_mapping_results <- 
    renderDT({
      if (length(secIdentifiersList()) != 0) {
        seq2pri_mapping$seq2pri_table
      } else {
        NULL
      }
    })


  ## Download results
  output$sec2pri_download <- downloadHandler(
    filename = function() {
      paste0("IDRefiner_mapping_", gsub(" ", "_", input$sec2priDataSource), ".", input$sec2pri_download_format)
    },
    content = function(file) {
      if(!is.null(sec2pri_output())) {
        dataset <- read_data_all()
        priID_list <- read_data_primary()
        
        primaryIDs <- intersect(unique(secIdentifiersList()), priID_list)
        unknownIDs <- unique(secIdentifiersList())[!unique(secIdentifiersList()) %in%
                                                     c(priID_list, dataset[[grep(ifelse(input$type == "identifierType", "secondaryID", "secondarySymbol|synonym"), colnames(dataset), value = TRUE)]])]
        
        if(input$sec2pri_download_format %in% c("tsv", "csv")){
          if(length(primaryIDs) == 0){
            output <- sec2pri_output()
          } else if(length(primaryIDs) != 0){
            if(input$type == "identifierType"){
              output <- dplyr::bind_rows(
                sec2pri_output(),
                data.frame(input = primaryIDs,
                           `primary ID` = primaryIDs,
                           comment = "The input is a primary ID.", check.names = FALSE))
          } else if(input$sec2priDataSource == "HGNC symbol2alias"){
            output <- dplyr::bind_rows(
              sec2pri_output(),
              data.frame(input = primaryIDs, `primary symbol` = primaryIDs, comment = "The input is a primary symbol.", check.names = FALSE) %>%
                mutate(`primary ID` = dataset$primaryID[match(`primary symbol`, dataset$primarySymbol)])
            )
          } else if(grep("name2synonym", input$sec2priDataSource)){
            output <- dplyr::bind_rows(
              sec2pri_output(),
              data.frame(input = primaryIDs, name = primaryIDs, comment = "The input is a primary symbol.", check.names = FALSE) %>%
                mutate(`primary ID` = dataset$primaryID[match(name, dataset$name)])
            )
          }
          }
          
          # Add unknown IDs
          if(length(unknownIDs) != 0)
            output <- dplyr::bind_rows(output, data.frame(input  = unknownIDs, comment = "The input is unknown.", check.names = FALSE))
              
          write.table(
            output, file, row.names = FALSE, 
            sep = ifelse (input$sec2pri_download_format == "csv", ",", "\t"),
            quote = FALSE
          )
        } else if(input$sec2pri_download_format == "sssom.tsv"){
          if(input$sec2priDataSource == "HGNC Accession number"){
            if(length(primaryIDs) == 0){
              output <- dataset %>% 
                dplyr::filter(secondaryID %in% c(secIdentifiersList())) %>% # input is secondary ID 
                dplyr::select(secondaryID, secondarySymbol, predicateID, primaryID, primarySymbol, mapping_cardinality_sec2pri, comment, source)
            } else {
              output <- dplyr::bind_rows(
                dataset %>% 
                  dplyr::filter(secondaryID %in% c(secIdentifiersList())) %>% # input is secondary ID 
                  dplyr::select(secondaryID, secondarySymbol, predicateID, primaryID, primarySymbol, mapping_cardinality_sec2pri, comment, source),
                dataset %>% 
                  dplyr::filter(primaryID %in% primaryIDs) %>% # input is primary ID 
                  dplyr::select(primaryID, primarySymbol, source) %>%
                  dplyr::mutate(secondaryID = primaryID,
                                secondarySymbol = primarySymbol,
                                predicateID = "skos:exactMatch",
                                mapping_cardinality_sec2pri = NA,
                                comment = "The input is a primary ID.")
              )}
            output <- output %>% dplyr::rename(subject_id = secondaryID , subject_label = secondarySymbol, predicate_id = predicateID,
                                               object_id = primaryID, object_label = primarySymbol, mapping_cardinality = mapping_cardinality_sec2pri) 
            
          } else if(input$sec2priDataSource == "HGNC symbol2alias"){
            if(length(primaryIDs) == 0){
              output <- dataset %>% 
                dplyr::filter(secondarySymbol %in% c(secIdentifiersList())) %>% # input is secondary ID 
                dplyr::select(secondarySymbol, predicateID, primaryID, primarySymbol, mapping_cardinality_sec2pri, comment, source) %>%
                dplyr::mutate(secondaryID = NA)
            } else {
              output <- dplyr::bind_rows(
                dataset %>% 
                  dplyr::filter(secondarySymbol %in% c(secIdentifiersList())) %>% # input is secondary ID 
                  dplyr::select(secondarySymbol, predicateID, primaryID, primarySymbol, mapping_cardinality_sec2pri, comment, source),
                dataset %>% 
                  dplyr::filter(primarySymbol %in% primaryIDs) %>% # input is primary ID 
                  dplyr::select(primaryID, primarySymbol, source) %>%
                  dplyr::mutate(secondaryID = primaryID,
                                secondarySymbol = primarySymbol,
                                predicateID = "skos:exactMatch",
                                mapping_cardinality_sec2pri = NA,
                                comment = "The input is a primary ID.")
              ) 
            }
            output <- output %>% dplyr::rename(subject_id = secondaryID , subject_label = secondarySymbol, predicate_id = predicateID,
                                               object_id = primaryID, object_label = primarySymbol, mapping_cardinality = mapping_cardinality_sec2pri) 
          } else if(input$sec2priDataSource == "UniProt"){
            if(length(primaryIDs) == 0){
              output <- dataset %>% 
                dplyr::filter(secondaryID %in% c(secIdentifiersList())) %>% # input is secondary ID 
                dplyr::select(secondaryID, predicateID, primaryID, mapping_cardinality_sec2pri, comment, source)
            } else {
              output <- dplyr::bind_rows(
                dataset %>% 
                  dplyr::filter(secondaryID %in% c(secIdentifiersList())) %>% # input is secondary ID 
                  dplyr::select(secondaryID, predicateID, primaryID, mapping_cardinality_sec2pri, comment, source),
                dataset %>% 
                  dplyr::filter(primaryID %in% primaryIDs) %>% # input is primary ID 
                  dplyr::select(primaryID, source) %>%
                  dplyr::mutate(secondaryID = primaryID,
                                predicateID = "skos:exactMatch",
                                mapping_cardinality_sec2pri = NA,
                                comment = "The input is a primary ID.")
              )}
            output <- output %>% dplyr::rename(subject_id = secondaryID, predicate_id = predicateID, object_id = primaryID, mapping_cardinality = mapping_cardinality_sec2pri) 
          } else if(input$sec2priDataSource %in% c("ChEBI", "HMDB")){
            sourceVersion <- read.table("processed_mapping_files/dataSourceVersion.tsv", sep = "\t", header = TRUE)
            sourceVersion <- sourceVersion$versionInfo[match(input$sec2priDataSource, sourceVersion$datasource)]
            dataset <- dataset %>% add_mapping_cardinality() %>% add_predicate()
            if(length(primaryIDs) == 0){
              output <-  dataset %>% 
                dplyr::filter(secondaryID %in% c(secIdentifiersList())) # input is secondary ID 
            } else {
              output <- dplyr::bind_rows(
                dataset %>% 
                  dplyr::filter(secondaryID %in% c(secIdentifiersList())), # input is secondary ID
                dataset %>% 
                  dplyr::filter(primaryID %in% primaryIDs) %>% # input is primary ID 
                  dplyr::select(primaryID) %>%
                  dplyr::mutate(secondaryID = primaryID,
                                predicate_id = "skos:exactMatch",
                                mapping_cardinality = NA,
                                comment = "The input is a primary ID.")
              )}
            output <- output %>% dplyr::rename(subject_id = secondaryID, object_id = primaryID)
            
          } else if(grep("name2synonym", input$sec2priDataSource)){
            sourceVersion <- read.table("processed_mapping_files/dataSourceVersion.tsv", sep = "\t", header = TRUE)
            if(length(primaryIDs) == 0){
              output <-  dataset %>% 
                dplyr::filter(synonym %in% c(secIdentifiersList())) %>% # input is secondary ID 
                dplyr::mutate(secondaryID = NA,
                              predicate_id = NA,
                              mapping_cardinality = NA)
            } else {
              output <- dplyr::bind_rows(
                dataset %>% 
                  dplyr::filter(synonym %in% c(secIdentifiersList())), # input is secondary ID
                dataset %>% 
                  dplyr::filter(name %in% primaryIDs) %>% # input is primary ID 
                  dplyr::select(primaryID, name, sourceFile) %>%
                  dplyr::mutate(secondaryID = primaryID,
                                synonym = name,
                                predicate_id = NA,
                                mapping_cardinality = NA,
                                comment = "The input is a metabolite name.")
              )}
            output <- output %>% dplyr::rename(subject_id = secondaryID, subject_label = synonym, object_id = primaryID, object_label = name) %>%
              dplyr::mutate(source = sourceVersion$versionInfo[match(sourceFile, sourceVersion$datasource)])
          }
          
          # Add unknown IDs
          if(length(unknownIDs) != 0){
            unknownData <- data.frame(subject = unknownIDs, comment = "The input is unknown.", check.names = FALSE)
            colnames(unknownData)[1] = ifelse(grepl("name2synonym|symbol2alias", input$sec2priDataSource), "subject_label", "subject_id")
            output <- dplyr::bind_rows(output, unknownData) %>%
              dplyr::select(dplyr::one_of(c("subject_id", "subject_label", "predicate_id", "object_id", "object_label", "mapping_cardinality", "comment", "source"))) %>%
              unique()
          }
          
          write_sssom_tsv(output, file, source = ifelse(input$sec2priDataSource %in% c("ChEBI", "HMDB"), sourceVersion, ""))

      }
      }
    })
  
  
  observe({
    if(nrow(sec2pri_output()) == 0) {
      shinyjs::disable("sec2pri_download")
    } else {
      shinyjs::enable("sec2pri_download")
    }
  })
  

  output$sec2pri_piechart_results <- renderPlot({
    seq2pri_mapping$seq2pri_pieChart
  },  height = 200, width = 400)
  
  observeEvent(input$sec2pri_get, {
    output$sec2pri_metadata <-  
      if(grepl("HGNC", input$sec2priDataSource) & nrow(sec2pri_output()) != 0){
        renderText(HTML("The data was obtained from the <a href='https://ftp.ebi.ac.uk/pub/databases/genenames/hgnc/archive/monthly/tsv/' target='_blank'>HGNC database</a> in its monthly release of <b>June 2023</b>."))
      } else if (grepl("HMDB", input$sec2priDataSource) & nrow(sec2pri_output()) != 0){
        renderText(HTML("The data was obtained from the <a href='https://hmdb.ca/downloads' target='_blank'>HMDB database</a> (version 5.0) released on <b>November 2021</b>."))
      } else if (grepl("ChEBI", input$sec2priDataSource) & nrow(sec2pri_output()) != 0){
        renderText(HTML("The data was obtained from the <a href='https://ftp.ebi.ac.uk/pub/databases/chebi/archive/rel211/SDF/' target='_blank'>ChEBI database</a> (rel211) released on <b>June 2022</b>."))
      } else if (grepl("Wikidata", input$sec2priDataSource) & nrow(sec2pri_output()) != 0){
        renderText(HTML("The data was obtained from the <a href='https://query.wikidata.org/' target='_blank'>Wikidata database</a> on <b>July 30, 2022</b>."))
      } 
  })
  
  # Handle clearing of input and output
  observeEvent(input$sec2pri_clear_list, {
    updateTextAreaInput(session, "sec2pri_identifiers", value = "")
    # Reset file input appearance
    seq2pri_input_file(NULL) # Reset the file input
    js_reset_file_input <- "$('#sec2pri_identifiers_file').val(null); $('.custom-file-label').html('Please upload file..');"
    session$sendCustomMessage(type = 'jsCode', message = js_reset_file_input)
    seq2pri_mapping$seq2pri_pieChart <- NULL
    seq2pri_mapping$metadata <- NULL
    seq2pri_mapping$seq2pri_table <- NULL
  })
  
  # add BridgeDb logo (in the text)
  # output$bridgeDb_logo <- renderImage({
  #   list(src = "www/logo_BridgeDb.png",
  #        width = "120px",
  #        height = "70px")
  # }, deleteFile = F)
  
  # add BridgeDb logo (page footer)
  # output$bridgeDb_logo_wide <- renderImage({
  #   list(src = "www/logo_BridgeDb_footer.png",
  #        width = "100%",
  #        height = "auto")
  # }, deleteFile = F)
  
  #BridgeDb tab
  ##Define the input options based
  ### Species
  observe({
    if(input$type == "gene") {
      output$inputSpecies <- renderUI({
        # Render the input options for selecting a species
        selectInput(
          inputId = 'inputSpecies',
          label = 'Choose species:',
          choices = c("Human"),
          selected = "Human"
        )
      })
    } else if(input$type == "metabolite") {
      output$inputSpecies <- renderUI({
        # Render an empty UI if identifier type is not gene
        NULL
      })
    }
  })
  ### Input data source
  observe({
    if(input$type == "gene") {
      output$inputDataSource <- renderUI({
        # Render the input options for selecting a species
        selectInput(
          inputId = 'inputDataSource', 
          label = 'Choose the input data source:',
          choices = sort(dataSources$source[dataSources$type == "gene"]),
          selected = "HGNC symbol2alias"
        )
      })
    } else if(input$type == "metabolite") {
      output$inputDataSource <- renderUI({
        # Render the input options for selecting a species
        selectInput(
          inputId = 'inputDataSource', 
          label = 'Choose the input data source:',
          choices = sort(dataSources$source[dataSources$type == "metabolite"]),
          selected = "ChEBI"
        )
      })
    }
  })
  ### Output data source
  observe({
    if(input$type == "gene") {
      output$outputDataSource <- renderUI({
        # Render the input options for selecting a species
        selectInput(
          inputId = 'outputDataSource', 
          label = 'Choose one or more output data source:', 
          choices = c("All", sort(dataSources$source[dataSources$type == "gene"])),
          selected = "Ensembl"
        )
      })
    } else if(input$type == "metabolite") {
      output$outputDataSource <- renderUI({
        # Render the input options for selecting a species
        selectInput(
          inputId = 'outputDataSource', 
          label = 'Choose one or more output data source:', 
          choices = c("All", sort(dataSources$source[dataSources$type == "metabolite"])),
          selected = "HMDB"
        )
      })
    }
  })
  
  BridgeDb_input_file <- reactiveVal(NULL)
  observeEvent(input$BridgeDb_identifiers_file, {
    if(!is.null(input$BridgeDb_identifiers_file)){
      BridgeDb_input_file(input$BridgeDb_identifiers_file)
    }
  })
  
  # Function to make a vector for input identifiers
  identifiersList <- reactive({
    if(!is.null(BridgeDb_input_file())){
      print("Reading identifiers from file...")
      input_ids <- readLines(BridgeDb_input_file()$datapath)
      # Remove empty or whitespace-only last line
      last_line <- input_ids[length(input_ids)]
      if(nchar(trimws(last_line)) == 0){
        input_ids <- input_ids[-length(input_ids)]
      }
      # Split identifiers on newline, comma, or space
      input_ids <- unlist(strsplit(input_ids, '\\"|\n|\t|,|\\s+', perl = TRUE))
      # Remove empty strings and return the list of identifiers
      input_ids[input_ids != ""]
      input_ids
    } else if(!is.null(input$BridgeDb_identifiers)){
      # Split identifiers entered in text area by newline, comma, or space
      input_ids <- as.character(input$BridgeDb_identifiers)
      input_ids <- unlist(strsplit(input_ids, '\n|,|\\s+', perl = TRUE))
      # Remove empty strings and return the list of identifiers
      input_ids <- input_ids[input_ids != ""]
      input_ids
    } 
  })
  
  # Function to make the output table
  BridgeDb_output <- reactive({
    req(!is.null(identifiersList())) 
    if(input$type == "gene") {
      input_species <- input$inputSpecies
      input_data_source <- input$inputDataSource
      output_data_source <- input$outputDataSource
      BridgeDb_results <- Xref_function(
        identifiersList(), 
        inputSpecies = input_species, 
        inputSystemCode = input_data_source, 
        outputSystemCode = output_data_source)
      return(BridgeDb_results)
      # return(NULL)
    } else if(input$type == "metabolite") {
      input_data_source <- input$inputDataSource
      output_data_source <- input$outputDataSource
      BridgeDb_results <- Xref_function(
        identifiersList(), 
        inputSystemCode = input_data_source, 
        outputSystemCode = output_data_source)
      return(BridgeDb_results)
    }
  })
  
  # Function to clear previous outputs
  # clearPreviousOutputs <- function() {
  #   updateTextAreaInput(session, "BridgeDb_identifiers", value = "")
  #   BridgeDb_input_file(NULL) # Reset the file input
  #   # Reset file input appearance
  #   js_reset_file_input <- "$('#BridgeDb_input_file').val(null); $('.custom-file-label').html('Please upload file..');"
  #   session$sendCustomMessage(type = 'jsCode', message = js_reset_file_input)
  #   BridgeDb_mapping$BridgeDb_table <- NULL
  # }
  
  BridgeDb_mapping <- reactiveValues(BridgeDb_table = NULL)
  observeEvent(input$BridgeDb_get, {
    if(!is.null(BridgeDb_output())) {
      BridgeDb_mapping$BridgeDb_table <- req(
        DT::datatable(BridgeDb_output(),
                      options = list(orderClasses = TRUE,
                                     lengthMenu = c(10, 25, 50, 100),
                                     pageLength = 10)
        )
      )
    } 
  }, ignoreInit = TRUE)
  
  # Update output display
  output$BridgeDb_mapping_results <- renderDT({
    if (length(identifiersList()) != 0) {
      BridgeDb_mapping$BridgeDb_table
    } else {
      NULL
    }
  })
  
  ## Download results
  output$BridgeDb_download <- downloadHandler(
    filename = function() {
      paste0("IDMapper.", input$BridgeDb_download_format)
    },
    content = function(file) {
      if(!is.null(BridgeDb_output())) {
        write.table(
          BridgeDb_output(), file, row.names = FALSE, 
          sep = ifelse(input$BridgeDb_download_format == "tsv", "\t", ","),
          quote = FALSE
        )
      }
    }
  )
  observe({
    if(is.null(BridgeDb_output())) {
      shinyjs::disable("BridgeDb_download")
    } else {
      shinyjs::enable("BridgeDb_download")
    }
  })
  
  # Handle clearing of input and output
  observeEvent(input$BridgeDb_clear_list, {
    updateTextAreaInput(session, "BridgeDb_identifiers", value = "")
    BridgeDb_input_file(NULL) # Reset the file input
    # Reset file input appearance
    js_reset_file_input <- "$('#BridgeDb_input_file').val(null); $('.custom-file-label').html('Please upload file..');"
    session$sendCustomMessage(type = 'jsCode', message = js_reset_file_input)
    BridgeDb_mapping$BridgeDb_table <- NULL
  })
  
  
}

shinyApp(ui = ui, server = server)

