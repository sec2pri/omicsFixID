#Empty the R environment
rm(list = ls())

#Set your working environment to the location where your current source file is saved into.
setwd("F:/bridgeDb/GitHubRepositories/BridgeDb-Shiny/")

# Load required packages
library(dplyr)
library(httr)
library(ggplot2)
library(shiny)
# library(edgeR)
library(DT)
library(data.table)

#Reading the required files
dataSources <- data.table::fread("input/dataSources.tsv")

## HMDB
HMDB <- data.table::fread("input/hmdb_secIds.tsv")
primaryIDs_HMDB <- HMDB$primaryID
## ChEBI
ChEBI <- data.table::fread("input/ChEBI_secIds.tsv")
primaryIDs_ChEBI <- ChEBI$primaryID
## WikiData
WikiData <- data.table::fread("input/wikidata_secIds.tsv")
## HGNC
HGNC <- data.table::fread("input/hgnc_all_secIds.tsv")

options(rsconnect.max.bundle.files = 3145728000)

# Piechart
piechart_theme <- theme_minimal() +
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


Xref_gene_function <- function(query, inputSpecies = "Human",
                               inputSystemCode = "H", outputSystemCode = "All") {

  # Preparing the query link.
  ## Setting up the query url.
  url <- "https://webservice.bridgedb.org"
  if(outputSystemCode == "All") {
    query_link  <- paste(url, inputSpecies, "xrefs", inputSystemCode, query, sep = "/")
  } else {
    query_link  <- paste(url, inputSpecies, "xrefs", inputSystemCode, query, outputSystemCode, sep = "/")
  }
  

  # Getting the response to the query.
  q_res <- GET(query_link)

  # Extracting the content in the raw text format.
  dat <- content(q_res, as = "text")

  if(dat == "{}") {
    warning(paste0("The query did not return a response."))
  }

  # Processing the raw content to get a data frame.
  dat <- as.data.frame(strsplit(dat, ",")[[1]])
  dat <- unlist(apply(dat, 1, strsplit, '":"'))
  dat <- matrix(dat, ncol = 2, byrow = TRUE)
  dat <- gsub('^[^:]*:|[{}\\\\/""]', "", dat)
  dat <- data.frame (identifier = rep(query, nrow(dat)),
                     target = dat[,1],
                     source = dat[,2])

  return(dat)

}

Xref_metabolite_function <- function(query, 
                                     inputSystemCode = "Ch", outputSystemCode = "All") {
  
  # Preparing the query link.
  ## Setting up the query url.
  url <- "https://webservice.bridgedb.org/Human"
  if(outputSystemCode == "All") {
    query_link  <- paste(url, "xrefs", inputSystemCode, query, sep = "/")
  } else {
    query_link  <- paste(url, "xrefs", inputSystemCode, query, outputSystemCode, sep = "/")
  }
  
  
  # Getting the response to the query.
  q_res <- GET(query_link)
  
  # Extracting the content in the raw text format.
  dat <- content(q_res, as = "text")
  
  if(dat == "{}") {
    warning(paste0("The query did not return a response."))
  }
  
  # Processing the raw content to get a data frame.
  dat <- as.data.frame(strsplit(dat, ",")[[1]])
  dat <- unlist(apply(dat, 1, strsplit, '":"'))
  dat <- matrix(dat, ncol = 2, byrow = TRUE)
  dat <- gsub('^[^:]*:|[{}\\\\/""]', "", dat)
  dat <- data.frame (identifier = rep(query, nrow(dat)),
                     target = dat[,1],
                     source = dat[,2])
  
  return(dat)
  
}

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
               min-height: 650px;
             }
         </style>
      ')
    )
  ),# Set the page title and add a horizontal line separator
  # Add a title panel
  titlePanel(
    div(
      div(
        strong("BridgeDb-Shiny"),
        br(),
        "a user friendly application for identifier mapping",
        style = "float:left;justify-content: flex-end;"
      ),
      div(
        imageOutput("Maastricht_logo"),
        style = "display: flex; align-items: right; justify-content: flex-end;"
        
      ),
      style = "display:flex; justify-content: space-between;margin: 0px; height: 100px;"
      
    )
  ),
  # Add a tabset panel with three tabs
  navbarPage(
    title = "",
    # Tab 1: About
    tabPanel(
      "About", 
      icon = icon("book"),
      align = "justify",
      # Add a summary section
      h3(strong("Summary")),
      # onclick = "ga('send', 'event', 'click', 'link')",
      p("Biological entities such as genes, proteins, complexes, and metabolites often have diverse identifiers across various databases, which pose a challenge for data integration. To solve this problem, identifier mapping is required to convert identifiers from one database to corresponding entities from other databases."),
      imageOutput("bridgeDb_logo", height = "70px"),
      p("BridgeDb (bridgedb.org) is an open source tool introduced in 2010 that connects identifiers from various biological databases and related resources, facilitating data harmonization and multi-omics analysis. BridgeDb is an ELIXIR Recommended Interoperability Resource (RIR) and provides mappings for genes and proteins for 35 species, metabolites, metabolic reactions, diseases, complexes, human coronaviruses, and publications. It includes:",
        tags$ul(
          tags$li("a Java library for software integration,"),
          tags$li("a REST-API for programmatic access from any programming language,"),
          tags$li("a dedicated R package,"),
          tags$li("a Python package,"),
          tags$li("and example code for Matlab integration through the webservice."),
        )
      ),
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
      h4(strong("BridgeDb-Shiny")),
      p("To address the challenges of integrating data from different biological sources that contain secondary identifiers, we developed a user-friendly Shiny app called BridgeDb-Shiny, which provides two key functions:",
        tags$ul(
          tags$li(style = "list-style-type: decimal;",
                  HTML("<b><span style='font-size:15px;'>XRefBatch mapping:</span></b>"),
                  br(),
                  "uses BridgeDb's REST-API to convert identifiers."),
          tags$li(style = "list-style-type: decimal;",
                  HTML("<b><span style='font-size:15px;'>Secondary-to-Primary (sec2pri) mapping:</span></b>"),
                  br(),
                  "provides statistics on the percentage of secondary identifiers in the dataset and converts outdated secondary identifiers to current primary identifiers, if available.",
                  "The sec2pri mapping functionality currently covers secondary identifiers from",
                  HTML("<b>HGNC</b>"), ", ",
                  HTML("<b>HMDB</b>"), ", ",
                  HTML("<b>ChEBI</b>"), "and ",
                  HTML("<b>WikiData</b>"),
                  "which can be converted to the corresponding primary identifier from the initial database.",
                  "After this step, the XrefBatch mapping can be used to convert the primary-ID-enhanced dataset to any other database currently supported by BridgeDb:",
                  tags$ul(
                    tags$li(
                      "The full overview of supported databases is available on the BridgeDb website (bridgedb.org/pages/system-codes)."
                    )
                  )
          )
        )
      ),
      p("The metadata for the latest update of the mapping files is also available to users for queries within the app."),
      br(),
      p(HTML("<b>Future development </b>"),
        "entails updating the Secondary-to-Primary identifier mapping files regularly via GitHub Actions to ensure accuracy."),
      br(),
      hr(),
      # Add a citation section
      h4("How to Cite BridgeDb"),
      p("van Iersel, Martijn P et al. “The BridgeDb framework: standardized access to gene, protein and metabolite identifier mapping services.” BMC bioinformatics vol. 11 5. 4 Jan. 2010."),
      p("doi:10.1186/1471-2105-11-5")
    ),
    # Tab 2: XrefBatch
    tabPanel(
      "XRefBatch mapping", 
      icon = icon("table"),
      div(
        style = "margin-top: 30px;",
        # Add a sidebar layout
        sidebarLayout(
          # Add a sidebar panel with input controls
          sidebarPanel(
            # Render the input options for selecting a identifier type
            radioButtons ("type", "Select identifier type:", 
                          c ("Gene/Protein" = "gene", "Metabolites" = "metabolite"),
                          selected = "metabolite" 
                          ),
            # Render the input options for selecting a species
            conditionalPanel(
              condition = "input.type == 'gene'",
              uiOutput('inputSpecies')
            ),
            # Add a file input for uploading a text file containing identifiers
            fileInput(
              "XrefBatch_identifiers_file",
              "Upload identifiers File",
              accept = c(".csv", ".xlsx", ".xls", ".tsv", ".txt"),
              placeholder = "Please upload file.."
            ),
            # Add a text area input for entering identifiers
            textAreaInput(
              'XrefBatch_identifiers', 
              'or insert identifier(s) here', 
              value = "", 
              width = NULL, 
              placeholder = 'one identifier per row'
            ),
            # Render the input options for selecting a data source
            uiOutput('inputDataSource'),
            # Render the input options for selecting an output data source
            uiOutput('outputDataSource'),
            # Add buttons for performing the identifier mapping and clearing the list
            div(
              actionButton("XrefBatch_get", "Bridge"),
              actionButton("XrefBatch_clear_list", "Clear"),
              br(),
              br(),
              downloadButton(
                "XrefBatch_download", "Download results", 
                style = "color: white; background-color: gray; border-color: black"
              )
            ),
            width = 3
          ),
          # Add a main panel for displaying the bridge list
          mainPanel(
            div(DTOutput("XrefBatch_mapping_results")),
            width = 9
          )
        )
      )
    ),
    # Tab 3: Sec2pri
    tabPanel(
      "Sec2pri mapping", 
      icon = icon("table"),
      div(
        style = "margin-top: 30px;",
        # Add a sidebar layout
        sidebarLayout(
          # Add a sidebar panel with input controls
          sidebarPanel(
            # Add a file input for uploading a text file containing identifiers
            fileInput(
              "sec2pri_identifiers_file",
              "Upload identifiers File",
              accept = c(".csv", ".xlsx", ".xls", ".tsv", ".txt"),
              placeholder = "Please upload file.."
            ),
            # Add a text area input for entering identifiers
            textAreaInput(
              'sec2pri_identifiers', 
              'or insert identifier(s) here', 
              value = "", 
              width = NULL, 
              placeholder = 'one identifier per row'
            ),
            # Render the input options for selecting a data source
            uiOutput('dataSource'),
            # Add buttons for performing the identifier mapping and clearing the list
            div(
              actionButton("sec2pri_get", "Bridge"),
              actionButton("sec2pri_clear_list", "Clear"),
              br(),
              br(),
              downloadButton(
                "sec2pri_download", "Download results", 
                style = "color: white; background-color: gray; border-color: black"
              )
            ),
            width = 3
          ),
          # Add a main panel for displaying the bridge list
          mainPanel(
            div(plotOutput("sec2pri_piechart_results", height = "300px"), class = "my-plot"),
            div(DTOutput("sec2pri_mapping_results"), style = "margin-top: -100px;"),
            width = 9
          )
        )
      )
    ),
    # Tab 4: Contact us
    tabPanel(
      "Contact us",
      icon = icon("envelope"),
      div(
        style = "margin-top: 30px;",
        # Add a contact us section
        br(),
        p(strong("For questions and comments:")),
        p("Tooba Abbassi-Daloii: t.abbassidaloii@maastrichtuniversity.nl"),
        p("Ozan Cinar: XXX"),
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
      imageOutput("bridgeDb_logo_wide", height = "70px"),
      style = "background-color: white; text-align: center; padding: 10px;"
    )
  )
)

server <- function(input, output, session) {
  #Maastricht logo
  output$Maastricht_logo <- renderImage({
    list(src = "www/Maastricht.png",
         width = "280px",
         height = "70px", 
         contentType = "image/png")
  }, deleteFile = FALSE)
  
  #XrefBatch tab
  ##Define the input options based
  ### Species
  observe({
    if(input$type == "gene") {
      output$inputSpecies <- renderUI({
        # Render the input options for selecting a species
        selectInput(
          inputId = 'inputSpecies',
          label = 'Select species:',
          choices = c("Human", "Mouse", "Rat"),
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
          label = 'Select the input data source:',
          choices = dataSources$source[dataSources$type == "gene"],
          selected = "HGNC"
        )
      })
    } else if(input$type == "metabolite") {
      output$inputDataSource <- renderUI({
        # Render the input options for selecting a species
        selectInput(
          inputId = 'inputDataSource', 
          label = 'Select the input data source:',
          choices = dataSources$source[dataSources$type == "metabolite"],
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
          label = 'Select one or more output data source:', 
          choices = c("All", dataSources$source[dataSources$type == "gene"]),
          selected = "Ensembl"
        )
      })
    } else if(input$type == "metabolite") {
      output$outputDataSource <- renderUI({
        # Render the input options for selecting a species
        selectInput(
          inputId = 'outputDataSource', 
          label = 'Select one or more output data source:', 
          choices = c("All", dataSources$source[dataSources$type == "metabolite"]),
          selected = "HMDB"
        )
      })
    }
  })
  
  XrefBatch_input_file <- reactiveVal(NULL)
  observeEvent(input$XrefBatch_identifiers_file, {
    if(!is.null(input$XrefBatch_identifiers_file)){
      XrefBatch_input_file(input$XrefBatch_identifiers_file)
    }
  })
  
  # Function to make a vector for input identifiers
  identifiersList <- reactive({
    if(!is.null(XrefBatch_input_file())){
      print("Reading identifiers from file...")
      input_ids <- readLines(XrefBatch_input_file()$datapath)
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
    } else if(!is.null(input$XrefBatch_identifiers)){
      # Split identifiers entered in text area by newline, comma, or space
      input_ids <- as.character(input$XrefBatch_identifiers)
      input_ids <- unlist(strsplit(input_ids, '\n|,|\\s+', perl = TRUE))
      # Remove empty strings and return the list of identifiers
      input_ids[input_ids != ""]
      input_ids
    } else{
      NULL
    }
  })
  
  # Function to make the output table
  XrefBatch_output <- reactive({
    req(!is.null(identifiersList())) 
    if(input$type == "gene") {
      input_species <- input$inputSpecies
      input_data_source <- dataSources$systemCode[dataSources$source == input$inputDataSource]
      output_data_source <- input$outputDataSource
      XrefBatch_results <- lapply(identifiersList(), function(identifier) {
        Xref_gene_function(
          identifier, 
          inputSpecies = input_species, 
          inputSystemCode = input_data_source, 
          outputSystemCode = output_data_source)
      })
      return(do.call(rbind, XrefBatch_results))
      # return(NULL)
    } else if(input$type == "metabolite") {
        input_data_source <- dataSources$systemCode [dataSources$source == input$inputDataSource]
        output_data_source <- input$outputDataSource
        XrefBatch_results <- lapply(identifiersList(), function(identifier) {
          Xref_metabolite_function(
            identifier, 
            inputSystemCode = input_data_source, 
            outputSystemCode = output_data_source)
        })
        return(do.call(rbind, XrefBatch_results))
    }
  })
  
  XrefBatch_mapping <- reactiveValues(XrefBatch_table = NULL)
  observeEvent(input$XrefBatch_get, {
    if(!is.null(XrefBatch_output())) {
      XrefBatch_mapping$XrefBatch_table <- req(
        DT::datatable(XrefBatch_output(),
                      options = list(orderClasses = TRUE,
                                     lengthMenu = c(10, 25, 50, 100),
                                     pageLength = 10)
        )
      )
    } 
  }, ignoreInit = TRUE)
  
  # Update output display
  output$XrefBatch_mapping_results <- renderDT({
    XrefBatch_mapping$XrefBatch_table
  })
  
  ## Download results
  output$XrefBatch_download <- downloadHandler(
    filename = "XrefBatch_mapping_BridgeDB-Shiny.csv",
    content = function(file) {
      if(!is.null(XrefBatch_output())) {
        write.csv(XrefBatch_output(),
                  file, row.names = FALSE, sep = "\t")
      }
    }
  )
  observe({
    if(is.null(XrefBatch_output())) {
      shinyjs::disable("XrefBatch_download")
    } else {
      shinyjs::enable("XrefBatch_download")
    }
  })
  
  # Handle clearing of input and output
  observeEvent(input$XrefBatch_clear_list, {
    updateTextAreaInput(session, "XrefBatch_identifiers", value = "")
    XrefBatch_input_file(NULL) # Reset the file input
    # Reset file input appearance
    js_reset_file_input <- "$('#XrefBatch_input_file').val(null); $('.custom-file-label').html('Please upload file..');"
    session$sendCustomMessage(type = 'jsCode', message = js_reset_file_input)
    XrefBatch_mapping$XrefBatch_table <- NULL
  })
  
  #sec2pri tab
  #Define the input options based on data source
  output$dataSource <- renderUI({
    selectInput(
      inputId = 'sec2priDataSource', 
      label = 'Select the data source:',
      choices = c("ChEBI", "HMDB", "WikiData", "HGNC"),
      selected = "ChEBI"
    )
  })
  
  seq2pri_input_file <- reactiveVal(NULL)
  observeEvent(input$sec2pri_identifiers_file, {
    if(!is.null(input$sec2pri_identifiers_file)){
      seq2pri_input_file(input$sec2pri_identifiers_file)
    }
  })
  
  # Function to make a vector for input identifiers
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
      input_ids <- unlist(strsplit(input_ids, '\\"|\n|\t|,|\\s+', perl = TRUE))
      # Remove empty strings and return the list of identifiers
      input_ids[input_ids != ""]
      input_ids
    } else if(!is.null(input$sec2pri_identifiers)){
      # Split identifiers entered in text area by newline, comma, or space
      input_ids <- as.character(input$sec2pri_identifiers)
      input_ids <- unlist(strsplit(input_ids, '\n|,|\\s+', perl = TRUE))
      # Remove empty strings and return the list of identifiers
      input_ids[input_ids != ""]
      input_ids
    }
  })
  
  # Function to calculate the proportion of primary and secondary identifiers in the input table
  sec2pri_proportion <- reactive({
    req(input$sec2priDataSource) 
    if(!is.null(input$sec2pri_identifiers_file) | !is.null(input$sec2pri_identifiers)) {
      priID_list = get(paste0("primaryIDs_", input$sec2priDataSource))
      dataset = get(input$sec2priDataSource)
      proportion_table = data.frame(
        type = c("#input IDs", 
                 "#primary IDs",
                 "#secondary IDs", 
                 "#unknown"),
        no = c(length(unique(secIdentifiersList())),
               length(intersect(secIdentifiersList(), priID_list)),
               length(intersect(secIdentifiersList(), dataset$secondaryID)),
               length(unique(secIdentifiersList())) - 
                 (length(intersect(secIdentifiersList(), priID_list)) +
                    length(intersect(secIdentifiersList(), dataset$secondaryID)))
        )
      )# %>% mutate(prop = no/length(unique(secIdentifiersList())))
      return(proportion_table[proportion_table$no != 0, ])
    }
  })
  
  # Function to make the output table
  sec2pri_output <- reactive({
    req(input$sec2priDataSource)
    if(!is.null(input$sec2pri_identifiers_file) | !is.null(input$sec2pri_identifiers)) {
      dataset = get(input$sec2priDataSource) 
      seq2pri_table_output <- dataset %>% 
        filter(secondaryID %in% c(secIdentifiersList())) %>%
        select(secondaryID, primaryID) %>%
        rename(identifier = secondaryID, `primary ID` = primaryID)
      return(seq2pri_table_output)
    }
  })
  
  seq2pri_mapping <- reactiveValues(seq2pri_pieChart = NULL, seq2pri_table = NULL)
  
  observeEvent(input$sec2pri_get, {
    seq2pri_mapping$seq2pri_pieChart <- 
      # Function to draw the piechart
      ggplot(sec2pri_proportion() [c(-1), ],
             aes(x = type, y = no, fill = type)) +
        geom_col(position = "dodge") +
        scale_fill_brewer(palette = "Blues") +
        coord_flip() +
        piechart_theme + 
        ggtitle(paste0(sec2pri_proportion()$no[1], " (unique) input identifiers")) +
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
  output$sec2pri_mapping_results <- renderDT({
    seq2pri_mapping$seq2pri_table
  })
  
  ## Download results
  output$sec2pri_download <- downloadHandler(
    filename = "sec2pri_mapping_BridgeDB-Shiny.csv",
    content = function(file) {
      if(!is.null(sec2pri_output())) {
        write.csv(sec2pri_output(),
                  file, row.names = FALSE, sep = "\t")
      }
    }
  )
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
  
  # Handle clearing of input and output
  observeEvent(input$sec2pri_clear_list, {
    updateTextAreaInput(session, "sec2pri_identifiers", value = "")
    seq2pri_input_file(NULL) # Reset the file input
    # Reset file input appearance
    js_reset_file_input <- "$('#sec2pri_identifiers_file').val(null); $('.custom-file-label').html('Please upload file..');"
    session$sendCustomMessage(type = 'jsCode', message = js_reset_file_input)
    
    seq2pri_mapping$seq2pri_pieChart <- NULL
    seq2pri_mapping$seq2pri_table <- NULL
  })
  
  # add BridgeDb logo (in the text)
  output$bridgeDb_logo <- renderImage({
    list(src = "www/logo_BridgeDb.png",
         width = "120px",
         height = "70px")
  }, deleteFile = F)
  
  # add BridgeDb logo (page footer)
  output$bridgeDb_logo_wide <- renderImage({
    list(src = "www/logo_BridgeDb_footer.png",
         width = "100%",
         height = "70px")
  }, deleteFile = F)
  
}


shinyApp(ui = ui, server = server)

