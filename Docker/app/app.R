#Empty the R environment
rm(list = ls())
options(shiny.appmode = "shiny")

# Load required packages
if(!"dplyr" %in% installed.packages()){install.packages("dplyr")}
library(dplyr)
if(!"httr" %in% installed.packages()){install.packages("httr")}
library(httr)
if(!"ggplot2" %in% installed.packages()){install.packages("ggplot2")}
library(ggplot2)
if(!"shiny" %in% installed.packages()){install.packages("shiny")}
library(shiny)
if(!"DT" %in% installed.packages()){install.packages("DT")}
library(DT)
if(!"data.table" %in% installed.packages()){install.packages("data.table")}
library(data.table)
if(!"rjson" %in% installed.packages()){install.packages("rjson")}
library(rjson)

# Load required functions and data
source("functions.R")

options(rsconnect.max.bundle.files = 3145728000)

#### Shinny App
ui <- function() {
  fluidPage(
    shinyjs::useShinyjs(), # needed for download button to work
    tags$head(
      tags$style(
        HTML('<hr style = "border-color: #0088cc;">
             <style>
               .main-panel {
                 padding-top: 60px; /* adjust this value as needed */
               }
               .my-plot {
                 height: 200px; /* set the height of the plot */
                 margin-bottom: 10px; /* add a small margin at the bottom */
                 margin-left: 10px;
               }
               .my-table {
                 height: 500px; /* set the height of the table */
                 overflow-y: auto; /* add a vertical scrollbar if necessary */
               }
               .navbar {
                 margin-bottom: 0px !important;
                 margin-top: 0px !important!
               }
               .tab-content {
                 padding-top: 0px !important;
                 min-height: 570px;
               }
               .input-group {
                 margin-bottom: 0px;
               }
               .navbar-default {
                 background-color: #004578; 
               }
               
               .navbar-default .navbar-nav > .active > a,
               .navbar-default .navbar-nav > .active > a:focus,
               .navbar-default .navbar-nav > .active > a:hover {
                 background-color: #eff6fc;
                 color: #004578; 
                 font-weight: bold;
               }
               .well {
                 background-color: #deecf9;
               }
               .info-icon {
                 color: black; /* Custom icon color */
                 background-color: transparent; /* No background */
               }
               .info-icon:hover {
                 color: gray; /* Hover color */
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
            style = "float:left;justify-content: flex-end;color: #004578; background-color: white;"
          ),
          div(
            imageOutput("Maastricht_logo"),
            style = "display: flex; align-items: right; justify-content: flex-end;"
          ),
          style = "display:flex; justify-content: space-between;margin: 0px; height: 70px;"
        ),
        div(style = "margin-top: -15px"),
        div(
          h4("A user friendly dashboard for refining omics identifiers and cross-referencing mappings across different biological datasources."),
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
        h3(strong("Summary"), style = "color: #004578;"),
        # onclick = "ga('send', 'event', 'click', 'link')",
        p("Biological entities such as genes, proteins, complexes, and metabolites often have diverse identifiers across various databases, which pose a challenge for data integration. To solve this problem, identifier mapping is required to convert identifiers from one database to corresponding entities from other databases."),
        br(),
        h4(strong("The secondary identifier challenge"), style = "color: #004578;"),
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
          "These tools currently do not have an API or other form of programmatic access, leading to issues in big omics data analysis."),
        br(),
        h4(strong("Omics IDRefiner"), style = "color: #004578;"),
        p("To address the challenges of integrating data from different biological sources that contain secondary identifiers, we developed a user-friendly Shiny app called Omics IDRefiner, which provides two key functions:",
          tags$ul(
            tags$li(style = "list-style-type: decimal;",
                    HTML("<b><span style='font-size:15px;color: #004578;'>IDRefiner:</span></b>"),
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
                    HTML("<b><span style='font-size:15px;color: #004578;'>IDMapper:</span></b>"),
                    br(),
                    "uses BridgeDb's REST-API to convert identifiers.")
            
          )
        ),
        p("The metadata for the latest update of the mapping files is also available to users for queries within the app."),
        br(),
        p(HTML("<b>Future development </b>"),
          "entails updating the Secondary-to-Primary identifier mapping files regularly via GitHub Actions to ensure accuracy."),
        br(),
        hr()
        # Add a citation section
        # h4("How to Cite ..."),
        # p("")
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
              style = "width: 255px;",
              div(style = "margin-top: -10px"),
              # Render the input options for selecting a identifier type
              radioButtons(
                "type", 
                HTML(
                  paste("Mapping type&nbsp;<i class='fas fa-info-circle info-icon' data-toggle='tooltip' title='",
                        tooltips$description[tooltips$tooltip == "Mapping type"], "'></i> :")),
                inline = TRUE, 
                c("Identifier" = "identifierType", "Symbol/Name" = "symbolType"),
                selected = "identifierType" 
              ),
              div(style = "margin-top: -10px"),
              # Render the input options for selecting a data source
              uiOutput('dataSource'),
              # Add a file input for uploading a text file containing identifiers
              fileInput(
                "sec2pri_identifiers_file",
                HTML(paste("Upload identifiers file&nbsp;<i class='fas fa-info-circle info-icon' data-toggle='tooltip' title='", tooltips$description[tooltips$tooltip == "Identifier file"], "'></i> :")),
                accept = c(".csv", ".xlsx", ".xls", ".tsv", ".txt"),
                placeholder = "Please upload file.."
              ),
              div(style = "margin-top: -30px"),
              # Add a text area input for entering identifiers
              textAreaInput(
                'sec2pri_identifiers',
                HTML(paste("or insert identifier(s) here&nbsp;<i class='fas fa-info-circle info-icon' data-toggle='tooltip' title='", tooltips$description[tooltips$tooltip == "Identifiers"], "'></i> :")),
                value = "", 
                width = NULL, 
                placeholder = 'one identifier per row'
              ),
              # Add buttons for performing the identifier mapping and clearing the list
              div(style = "margin-top: -10px"),
              div(
                actionButton(
                  "sec2pri_get", "Refine IDs",
                  style = "color: white; background-color: #004578; border-color: #004578"),
                actionButton(
                  "sec2pri_clear_list", "Clear results",
                  style = "color: white; background-color: #004578; border-color: #004578"),
                br(),
                br(),
                div(style = "margin-top: -10px"),
                # Render the input options for selecting a download format
                uiOutput('downloadFormatUI'),
                div(style = "margin-top: -10px"),
                downloadButton(
                  outputId = "sec2pri_download",
                  label = "Download results", 
                  style = "color: white; background-color: #004578; border-color: #004578"
                ),
                div(style = "margin-top: -10px")
              )
            ),
            # Add a main panel for displaying the bridge list
            mainPanel(
              style = "margin-left: 0px; padding: 20px;",
              div(htmlOutput("sec2pri_metadata"), style = "margin-left: 10px;"),
              div(plotOutput("sec2pri_piechart_results", height = "200px"), class = "my-plot"),
              uiOutput("copyButtonUI"),
              div(DT::DTOutput("sec2pri_mapping_results"), style = "margin-top: 50px; margin-left: 10px;")
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
              style = "width: 250px;",
              div(style = "margin-top: -10px"),
              
              # Render the input options for selecting a identifier type
              radioButtons("type_BridgeDb", "Choose identifier type:", inline = TRUE,
                            c("Gene/Protein" = "gene", "Metabolites" = "metabolite"),
                            selected = "metabolite" 
              ),
              # Render the input options for selecting a species
              conditionalPanel(
                condition = "input.type_BridgeDb == 'gene'",
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
                  "BridgeDb_get", "Bridge IDs",
                  style = "color: white; background-color: #004578; border-color: #004578"),
                actionButton(
                  "BridgeDb_clear_list", "Clear results",
                  style = "color: white; background-color: #004578; border-color: #004578"),
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
                  style = "color: white; background-color: #004578; border-color: #004578"
                ),
                div(style = "margin-top: -10px")
              )
            ),
            # Add a main panel for displaying the bridge list
            mainPanel(
              style = "margin-left: 0px; padding: 20px;",
              div(DT::DTOutput("BridgeDb_mapping_results", height = "500px"))
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
          p(HTML("<b><span style='font-size:16px;color: #004578;'>For questions and comments:</span></b>")),
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
      div(style = "flex: 0;"
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
}

server <- function(input, output, session) {
  #Maastricht university logo
  output$Maastricht_logo <- renderImage({
    list(src = "www/Maastricht.png",
         width = "280px",
         height = "70px", 
         contentType = "image/png")
  }, deleteFile = FALSE)
  
  #sec2pri tab
  # Define the input options based on identifier or symbol/name
  observe({
    sec2pri_mapping$sec2pri_table <- NULL
    output$dataSource <- renderUI({
      id_type(type = input$type)
    })
  })

  # Update the TextArea based on the selected database
  observeEvent(input$sec2priDataSource, {
    # Clear the existing outputs
    sec2pri_mapping$sec2pri_pieChart <- NULL
    sec2pri_mapping$sec2pri_table <- NULL
    output$sec2pri_metadata <- NULL
    updateTextAreaInput(session, "sec2pri_identifiers",
                        value = text_value(input$sec2priDataSource))
  })

  # Check the input file
  sec2pri_input_file <- reactiveVal(NULL)
  observeEvent(input$sec2pri_identifiers_file, {
    if(!is.null(input$sec2pri_identifiers_file)){
      sec2pri_input_file(input$sec2pri_identifiers_file)
    }
  })
  
  # Function to make a vector for input identifiers
  secIdentifiersList <- reactive({
    output$sec2pri_metadata <- NULL
    input_ids <- create_id_list(
      inputFile = sec2pri_input_file()$datapath,
      inputText = input$sec2pri_identifiers,
      type = input$type)
  })
  
  # Function to read data related to the selected datasource  
  read_data_all <- reactive({
    req(input$sec2priDataSource) 
    mapping_table <- read_input(sec2priDataSource = input$sec2priDataSource)
  })
  
  # Function to read primary ids/symbols/names related to the selected datasource 
  read_data_primary <- reactive({
    req(input$sec2priDataSource)
    priID_list <- read_primary_input(sec2priDataSource = input$sec2priDataSource)
  })
  
  # Function to calculate the number of primary and secondary identifiers in the input table
  sec_pri_count <- reactive({
    req(input$sec2priDataSource) 
    # (1) calculate the number of primary and secondary identifiers in the input table
    if(!is.null(input$sec2pri_identifiers_file) | !is.null(input$sec2pri_identifiers)) {
      count_id_group(
        type = input$type,
        inputIdentifierList = secIdentifiersList(), 
        priID_list = read_data_primary(),
        mapping_table = read_data_all()
      )
    }
  })
  
  # Function to make the output table
  sec2pri_output <- reactive({
    req(input$sec2priDataSource, input$sec2pri_get)
    if(!is.null(input$sec2pri_identifiers_file) | !is.null(input$sec2pri_identifiers)) {
      create_output_table(
        sec2priDataSource = input$sec2priDataSource,
        inputIdentifierList = secIdentifiersList(),
        mapping_table = read_data_all())
    }
  })
  
  # Display metadata based on the selected datasource 
  observeEvent(input$sec2pri_get, {
    output$sec2pri_metadata <- NULL
    if(length(secIdentifiersList()) != 0) {
      output$sec2pri_metadata <- create_metadata(sec2priDataSource = input$sec2priDataSource)
    } else if(length(secIdentifiersList()) == 0){
      output$sec2pri_metadata <- renderText(HTML("<b>No input provided</b>"))
    }
  })

  # Display plot and output table 
  sec2pri_mapping <- reactiveValues(sec2pri_pieChart = NULL, sec2pri_table = NULL)
  observeEvent(input$sec2pri_get, {
    # Clear the existing outputs
    sec2pri_mapping$sec2pri_pieChart <- NULL
    sec2pri_mapping$sec2pri_table <- NULL
    
    if(!is.null(sec_pri_count())) {
      sec2pri_mapping$sec2pri_pieChart <- 
        create_plot(freq_table = sec_pri_count(), IDtype = input$type)
    }
    
    if(nrow(sec2pri_output()) != 0) {
      sec2pri_mapping$sec2pri_table <- req(
        DT::datatable(sec2pri_output(), 
                      options = list(orderClasses = TRUE,
                                     lengthMenu = c(10, 25, 50, 100),
                                     pageLength = 10
                      ),
                      rownames = FALSE,
                      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left; color:black; font-weight: bold;',
                                              "Only the mapping results for the secondary inputs are shown:")
        ) %>%
          formatStyle(
            columns = "input (secondary)",
            backgroundColor = "#7da190",
            color = "white"
          )
      )
    } 
  }, ignoreInit = TRUE)
  
  # Update output display
  output$sec2pri_piechart_results <- renderPlot({
    if(length(secIdentifiersList()) != 0) {
      sec2pri_mapping$sec2pri_pieChart
    } else {
      NULL
    }
  }, height = 200, width = 400)
  output$sec2pri_mapping_results <- 
    DT::renderDT({
      if(length(secIdentifiersList()) != 0) {
        sec2pri_mapping$sec2pri_table
      } else {
        NULL
      }
    })
  
   PriIDList <- reactive({
    if(nrow(sec2pri_output()) != 0) {
      primary_id_list(
        type = input$type,
        inputIdentifierList = secIdentifiersList(), 
        priID_list = read_data_primary(),
        mapping_table = read_data_all(),
        sec2pri_table = sec2pri_output())
    }
  })

   # Create a reactiveVal to track user selection
   user_selection <- reactiveVal("")
   
   # Update user_selection when the "Get Data" button is clicked
   observeEvent(input$sec2pri_get, {
     user_selection("sec2pri_get")
   })
   
   # Conditionally render the "Copy Primary IDs" button
   output$copyButtonUI <- renderUI({
     if(!is.null(sec2pri_mapping$sec2pri_table) & user_selection() == "sec2pri_get") {
       actionButton(
         "copyPrimaryID",
         "Copy primary IDs to IDMapper", 
         style = "color: #96b77d; background-color:#EEEEEE; border-color: #96b77d; float: right;")
     } 
   })
   

   # Copy all PrimaryID values to the text area when the button is clicked
   observeEvent(input$copyPrimaryID, {
    primary_id_text = PriIDList()
    primary_id_text <- paste(primary_id_text, collapse = "\n")
    updateTextAreaInput(session, "BridgeDb_identifiers", value = primary_id_text)
      
  })
  
  # Define output format
  output$downloadFormatUI <- renderUI({
    dataSource <- input$sec2priDataSource
    downloadFormats <- if(!is.null(dataSource) && grepl("synonym2name", dataSource)) {
      c("tsv", "sssom.tsv")
    } else {
      c("csv", "tsv", "sssom.tsv")
    }
    
    selectInput(
      inputId = "sec2pri_download_format",
      label = HTML(paste("Choose a download format:&nbsp;<i class='fas fa-info-circle info-icon' data-toggle='tooltip' title='", tooltips$description[tooltips$tooltip == "Format"], "'></i> :")),
      choices = downloadFormats,
      selected = "tsv"
    )
  })  
  
  # Download results
  output$sec2pri_download <- downloadHandler(
    filename = function() {
      paste0("IDRefiner_mapping_", gsub(" ", "_", input$sec2priDataSource), ".", input$sec2pri_download_format)
    },
    
    content = function(file) {
      if(!is.null(sec2pri_output())) {
          if(input$sec2pri_download_format %in% c("tsv", "csv")){
            
            output <- create_tsvORcsv_output(
              type = input$type,
              inputIdentifierList = secIdentifiersList(),
              sec2priDataSource = input$sec2priDataSource,
              priID_list = read_data_primary(),
              mapping_table = read_data_all(),
              sec2pri_table = sec2pri_output())
            print(output)
            print(sec2pri_output())
            write.table(
              output, file, row.names = FALSE,
              sep = ifelse(input$sec2pri_download_format == "csv", ",", "\t"),
              quote = FALSE
            )
        } else if(input$sec2pri_download_format == "sssom.tsv"){
          sourceVersion <- get_source_version(sec2priDataSource = input$sec2priDataSource)
          
          output <- create_sssom_output(
            type = input$type,
            inputIdentifierList = secIdentifiersList(),
            sec2priDataSource = input$sec2priDataSource,
            sourceVersion = sourceVersion,
            priID_list = read_data_primary(),
            mapping_table = read_data_all(),
            sec2pri_table = sec2pri_output())
            print(output)
            
          write_sssom_tsv(
            output, file,
            source = ifelse(grepl("HMDB|ChEBI|Wikidata", input$sec2priDataSource) && exists("sourceVersion"), sourceVersion, ""))
        }
      }
    }
  )
  
  
  # (In)Active download button
  observe({
    if(nrow(sec2pri_output()) == 0) {
      shinyjs::disable("sec2pri_download")
    } else {
      shinyjs::enable("sec2pri_download")
    }
  })
  
  # Handle clearing of input and output
  observeEvent(input$sec2pri_clear_list, {
    # updateTextAreaInput(session, "sec2pri_identifiers", value = "")
    # Reset file input appearance
    # sec2pri_input_file(NULL) # Reset the file input
    # js_reset_file_input <- "$('#sec2pri_identifiers_file').val(null); $('.custom-file-label').html('Please upload file..');"
    # session$sendCustomMessage(type = 'jsCode', message = js_reset_file_input)
    sec2pri_mapping$sec2pri_pieChart <- NULL
    output$sec2pri_metadata <- NULL 
    sec2pri_mapping$sec2pri_table <- NULL
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
  # Handle clearing of BridgeDb_identifiers
  observeEvent(c(input$type_BridgeDb, input$inputDataSource), {
    if(is.null(PriIDList())){
      updateTextAreaInput(session,
                        inputId = 'BridgeDb_identifiers',
                        value = '')  # Clear the text area
    }
  })
  
  ##Define the input options based
  ### Species
  observe({
    if(input$type_BridgeDb == "gene") {
      output$inputSpecies <- renderUI({
        # Render the input options for selecting a species
        selectInput(
          inputId = 'inputSpecies',
          label = 'Choose species:',
          choices = c("Human"),
          selected = "Human"
        )
      })
    } else if(input$type_BridgeDb == "metabolite") {
      output$inputSpecies <- renderUI({
        # Render an empty UI if identifier type is not gene
        NULL
      })
    }
  })
  ### Input data source
  observe({
    if(input$type_BridgeDb == "gene") {
      output$inputDataSource <- renderUI({
        BridgeDb_mapping$BridgeDb_table <- NULL
        
        # Render the input options for selecting a species
        selectInput(
          inputId = 'inputDataSource', 
          label = 'Choose the input data source:',
          choices = sort(dataSources$source[dataSources$type == "gene"]),
          selected = "HGNC alias2symbol"
        )
      })
    } else if(input$type_BridgeDb == "metabolite") {
      output$inputDataSource <- renderUI({
        BridgeDb_mapping$BridgeDb_table <- NULL
        
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
    if(input$type_BridgeDb == "gene") {
      BridgeDb_mapping$BridgeDb_table <- NULL
      
      output$outputDataSource <- renderUI({
        # Render the input options for selecting a species
        selectInput(
          inputId = 'outputDataSource', 
          label = 'Choose one or more output data source:', 
          choices = c("All", sort(dataSources$source[dataSources$type == "gene"])),
          selected = "Ensembl"
        )
      })
    } else if(input$type_BridgeDb == "metabolite") {
      output$outputDataSource <- renderUI({
        BridgeDb_mapping$BridgeDb_table <- NULL
        
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
    BridgeDb_mapping$BridgeDb_table <- NULL
    
    if(input$type_BridgeDb == "gene") {
      input_species <- input$inputSpecies
      input_data_source <- input$inputDataSource
      output_data_source <- input$outputDataSource
      BridgeDb_results <- Xref_function(
        identifiersList(), 
        inputSpecies = input_species, 
        inputSystemCode = input_data_source, 
        outputSystemCode = output_data_source) %>%
        unique()
      # return(NULL)
    } else if(input$type_BridgeDb == "metabolite") {
      input_data_source <- input$inputDataSource
      output_data_source <- input$outputDataSource
      BridgeDb_results <- Xref_function(
        identifiersList(), 
        inputSystemCode = input_data_source, 
        outputSystemCode = output_data_source) %>%
        unique()
    }
    return(BridgeDb_results)
    
  })
  
  BridgeDb_mapping <- reactiveValues(BridgeDb_table = NULL)
  observeEvent(input$BridgeDb_get, {
    BridgeDb_mapping$BridgeDb_table <- NULL
    
    if(!is.null(BridgeDb_output())) {
      BridgeDb_mapping$BridgeDb_table <- req(
        DT::datatable(BridgeDb_output(),
                      options = list(orderClasses = TRUE,
                                     lengthMenu = c(10, 25, 50, 100),
                                     pageLength = 10),
                      rownames = FALSE
        )
      )
    } 
  }, ignoreInit = TRUE)
  
  # Update output display
  output$BridgeDb_mapping_results <- DT::renderDT({
    if(length(identifiersList()) != 0) {
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
    # updateTextAreaInput(session, "BridgeDb_identifiers", value = "")
    # BridgeDb_input_file(NULL) # Reset the file input
    # Reset file input appearance
    # js_reset_file_input <- "$('#BridgeDb_input_file').val(null); $('.custom-file-label').html('Please upload file..');"
    # session$sendCustomMessage(type = 'jsCode', message = js_reset_file_input)
    BridgeDb_mapping$BridgeDb_table <- NULL
  })
  
  
}

shinyApp(ui = ui, server = server)

