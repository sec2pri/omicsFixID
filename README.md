### Summary
Biological entities such as genes, proteins, complexes, and metabolites often have diverse identifiers across various databases, which pose a challenge for data integration. To solve this problem, identifier mapping is required to convert identifiers from one database to corresponding entities from other databases.

### BridgeDb
BridgeDb (bridgedb.org) is an open source tool introduced in 2010 that connects identifiers from various biological databases and related resources, facilitating data harmonization and multi-omics analysis. BridgeDb is an ELIXIR Recommended Interoperability Resource (RIR) and provides mappings for genes and proteins for 35 species, metabolites, metabolic reactions, diseases, complexes, human coronaviruses, and publications. It includes:

- a Java library for software integration,
- a REST-API for programmatic access from any programming language,
- a dedicated R package,
- a Python package,
- and example code for Matlab integration through the webservice.

### The secondary identifier challenge
After mapping identifiers from one database to another, integrating data can remain a challenge due to the presence of retired, deleted, split, and/or merged identifiers currently present in databases and datasets alike. These outdated identifiers are called “secondary” while the identifiers currently supported by databases are referred to as “primary” . The presence of secondary identifiers in a used dataset or database can lead to information loss and hinders effective data integration. While some tools exist to convert secondary identifiers to current ones, these tools only support one type of data (either genes/proteins or metabolites):

- https://www.genenames.org/tools/multi-symbol-checker/
- https://www.metaboanalyst.ca/MetaboAnalyst/upload/ConvertView.xhtml

These tools currently do not have an API or other form of programmatic access, leading to issues in big OMICS data analysis.

### BridgeDb-Shiny: https://tabbassidaloii.shinyapps.io/BridgeDb-Shiny/
To address the challenges of integrating data from different biological sources that contain secondary identifiers, we developed a user-friendly Shiny app called BridgeDb-Shiny, which provides two key functions:

1. XRefBatch mapping:
uses BridgeDb's REST-API to convert identifiers.
2. Secondary-to-Primary (sec2pri) mapping:
provides statistics on the percentage of secondary identifiers in the dataset and converts outdated secondary identifiers to current primary identifiers, if available. The sec2pri mapping functionality currently covers secondary identifiers from HGNC , HMDB , ChEBI and WikiData which can be converted to the corresponding primary identifier from the initial database. After this step, the XrefBatch mapping can be used to convert the primary-ID-enhanced dataset to any other database currently supported by BridgeDb:
The full overview of supported databases is available on the BridgeDb website (bridgedb.org/pages/system-codes).
The metadata for the latest update of the mapping files is also available to users for queries within the app.


Future development entails updating the Secondary-to-Primary identifier mapping files regularly via GitHub Actions to ensure accuracy.

