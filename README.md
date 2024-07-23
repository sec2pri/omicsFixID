## [omicsFixID](https://tabbassidaloii.shinyapps.io/omicsFixID/)

### Summary
Biological entities such as genes, proteins, complexes, and metabolites often have diverse identifiers across various databases, which pose a challenge for data integration. To solve this problem, identifier mapping is required to convert identifiers from one database to corresponding entities from other databases.

### The secondary identifier challenge
After mapping identifiers from one database to another, integrating data can remain a challenge due to the presence of retired, deleted, split, and/or merged identifiers currently present in databases and datasets alike. 

--------
#### Examples
Many biological databases split/merge or withdraw identifiers. 
Below you can see some examples from [HGNC](http://ftp.ebi.ac.uk/pub/databases/genenames/hgnc/tsv/withdrawn.txt):

HGNC_ID|STATUS|WITHDRAWN_SYMBOL|MERGED_INTO_REPORT(S) (i.e HGNC_ID/SYMBOL/STATUS)
:---|:---|:---|:--- 
HGNC:1531|Merged/Split|CBBM|HGNC:4206/OPN1MW/Approved, HGNC:9936/OPN1LW/Approved
HGNC:354|Merged/Split|AIH2|HGNC:3344/ENAM/Approved
HGNC:440|Entry Withdrawn|ALPPL1| 

##### Withdrawn entries (deleted ids)
Some molecular entries were withdrawn/deleted from a database and they won't exist anymore. `HGNC:440` is an example of withdrawn id from HGNC.

##### Split/merged ids
When an id is split or merged in a database, a new id(s) will be used for that specific entity. The new id(s) is called the primary id(s), while the split/merged id is the secondary id, which will not be used anymore.

secondary id|primary id
:---|:---
HGNC:1531|HGNC:4206
HGNC:1531|HGNC:9936
CBBM|OPN1MW
CBBM|OPN1LW
HGNC:354|HGNC:3344
AIH2|ENAM

The split ids may introduce one-to-multiple mapping issues which should be further evaluated.

##### Secondary ids vs duplicate ids
In some databases, multiple ids refer to the same entity. We define these ids as duplicate ids. Below you see an example from [HMDB](https://hmdb.ca/metabolites/HMDB0004160):

Version|Status|Creation Date|Update Date|HMDB ID|Secondary Accession Numbers
:---|:---|:---|:---|:---|:---
5.0|Detected and Quantified|2006-08-13 13:18:56 UTC|2021-09-14 14:59:00 UTC|HMDB0004160|HMDB0004159, HMDB0004161, HMDB04159, HMDB04160, HMDB04161

In this case, the id, currently used by the databases to refer to the entity, is the primary id.
duplicate id|primary id
:---|:---
HMDB0004159|HMDB0004160
HMDB0004161|HMDB0004160
HMDB04159|HMDB0004160
HMDB04160|HMDB0004160
HMDB04161|HMDB0004160
--------

These outdated identifiers are called “secondary” while the identifiers currently supported by databases are called “primary”. The presence of secondary identifiers in a used dataset or database can lead to information loss and hinders effective data integration. While some tools exist to convert secondary identifiers to current ones, these tools only support one type of data (either genes/proteins or metabolites):

- https://www.genenames.org/tools/multi-symbol-checker/
- https://www.metaboanalyst.ca/MetaboAnalyst/upload/ConvertView.xhtml

These tools currently do not have an API or other form of programmatic access, leading to issues in big OMICS data analysis.

--------
### [omicsFixID](https://tabbassidaloii.shinyapps.io/omicsFixID/) functionality:
To address the challenges of integrating data from different biological sources that contain secondary identifiers, we developed a user-friendly Shiny app called omicsFixID, which provides two key functions:

1. FixID:
provides statistics on the percentage of secondary identifiers in the dataset and converts outdated secondary identifiers to current primary identifiers, if available. The FixID functionality currently covers secondary identifiers from HGNC, NCBI, UniProt, HMDB, ChEBI and Wikidata which can be converted to the corresponding primary identifier from the initial database. After this step, the IDMapper can be used to convert the primary-ID-enhanced dataset to any other database currently supported by BridgeDb:
- The full overview of supported databases is available on the BridgeDb website (bridgedb.org/pages/system-codes).

2. CrossMapID:
uses BridgeDb's REST-API to convert identifiers.

The metadata for the latest update of the mapping files is also available to users for queries within the app.

--------
### **Building and running omics FixID Docker**

### Building the omics FixID Docker image
To build the omics FixID Docker image, follow these steps:

1. **Navigate to the project directory:**
   ```bash
   cd path/to/omicsFixID
   ```

2. **Build the Docker image:**
   Replace `<your_dockerhub_username>`, `<your_image_name>`, and `<version_name>` with your desired values.
   ```bash
   docker build -t <your_dockerhub_username>/<your_image_name>:<version_name> .
   ```
   For example: 
   ```bash
   docker build -t tabbassidaloii/omicsfixid:v2 .
   ```

### Running the omics FixID Docker image locally
To run the omics FixID Docker image locally:

1. **Run the container:**
   Replace `<your_dockerhub_username>`, `<your_image_name>`, and `<version_name>` with your desired values.
   ```bash
   docker run -p 4200:4200 -d <your_dockerhub_username>/<your_image_name>:<version_name>
   ```
   For example:
   ```bash
   docker run -p 4200:4200 -d tabbassidaloii/omicsfixid:v2
   ```
   This command maps port 4200 of the container to port 4200 on your local machine and runs the container in detached mode.

2. **Check running containers:**
   To see all running and stopped containers, use:
   ```bash
   docker ps -a
   ```

### Accessing the omics FixID application
Once the container is running, you can access the omics FixID application by opening a web browser and navigating to:
```
http://localhost:4200/
```
