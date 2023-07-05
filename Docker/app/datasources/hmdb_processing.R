# Clear environment and load necessary libraries
rm(list = ls())
library(xml2)

# Set variables
sourceName <- "hmdb"
DbVersion <- "1.0.0"
inputDir <- "datasources/input_raw_files/"

# Create output directory
outputDir <- "datasources/processed_mapping_files"
dir.create(outputDir, showWarnings = FALSE)

# Create output tsv mapping files

# Primary ID
listOfpri <- list()
# Secondary to primary ID
listOfsec2pri <- list()
# Name to synonyms
listOfname2symbol <- list()

counter <- 0
counter2 <- 0

# read HMDB XML files
folder_path <- paste0(inputDir, sourceName, "/hmdb_metabolites_split/hmdb/")
entries <- list.files(folder_path)

# Iterate through the folder
for (entry in entries) {
  if (entry != "hmdb_metabolites.xml") {
    document <- read_xml(paste0 (folder_path, entry))
      
    # Extract primary ID
    priIdNode <- "accession"
    priIdList <- xml_find_all(document, priIdNode)
    priId <- xml_text(priIdList)
    listOfpri <- c(listOfpri, priId)
      
    # Extract secondary IDs
    secIdNode <- "secondary_accessions"
    secIdList <- xml_find_all(document, secIdNode)
    secIdValues <- unlist(lapply(xml_children(secIdList), function(node) xml_text(node)))
      
    if (is.null(secIdValues)) {
      listOfsec2pri <- c(listOfsec2pri, list(c(priId, NA)))
      } else if (length(secIdValues) == 1) {
        listOfsec2pri <- c(listOfsec2pri, list(c(priId, secIdValues)))
        } else if (length(secIdValues) > 1){
          for (secID in secIdValues) {
            listOfsec2pri <- c(listOfsec2pri, list(c(priId, secID)))
          }
        }
    
    # Extract metabolite name
    priSymbolNode <- "name"
    priSymbolList <- xml_find_all(document, priSymbolNode)
    priSymbol <- xml_text(priSymbolList)
      
    # Extract synonyms
    secSymbolNode <- "synonyms"
    secSymbolList <- xml_find_all(document, secSymbolNode)
    secSymbolValues <- unlist(lapply(xml_children(secSymbolList), function(node) xml_text(node)))
      
    if (is.null(secSymbolValues)) {
      listOfname2symbol <- c(listOfname2symbol, list(c(priId, priSymbol, NA)))
      } else if (length(secSymbolValues) == 1) {
        listOfname2symbol <- c(listOfname2symbol, list(c(priId, priSymbol, secSymbolValues)))
        } else if (length(secSymbolValues) > 1){
          for (syn in secSymbolValues) {
            listOfname2symbol <- c(listOfname2symbol, list(c(priId, priSymbol, syn)))
          }
        }
    
    # Progress update at every 5000th iteration
    counter <- counter + 1
    if (counter == 5000) {
      counter2 <- counter2 + 1
      cat(paste("5k mark ", counter2, ": ", priId), "\n")
      counter <- 0
    }
  }
}

  
# Write output TSV files
output_pri_Tsv <- file.path(outputDir, paste(sourceName, "_priIDs_v", DbVersion, ".tsv", sep = ""))
write.table(do.call(rbind, listOfpri), output_pri_Tsv, sep = "\t", row.names = FALSE)
  
output_sec2pri_Tsv <- file.path(outputDir, paste(sourceName, "_secID2priID_v", DbVersion, ".tsv", sep = ""))
write.table(do.call(listOfsec2pri), output_sec2pri_Tsv, sep = "\t", row.names = FALSE)
  
output_name_Tsv <- file.path(outputDir, paste(sourceName, "_name2symbol_v", DbVersion, ".tsv", sep = ""))
write.table(do.call(listOfname2symbol), output_name_Tsv, sep = "\t", row.names = FALSE)
  
  