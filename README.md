### Summary
Biological entities such as genes, proteins, complexes, and metabolites often have diverse identifiers across various databases, which pose a challenge for data integration. To solve this problem, identifier mapping is required to convert identifiers from one database to corresponding entities from other databases.

### The secondary identifier challenge
After mapping identifiers from one database to another, integrating data can remain a challenge due to the presence of retired, deleted, split, and/or merged identifiers currently present in databases and datasets alike. These outdated identifiers are called “secondary” while the identifiers currently supported by databases are referred to as “primary”. The presence of secondary identifiers in a used dataset or database can lead to information loss and hinders effective data integration. While some tools exist to convert secondary identifiers to current ones, these tools only support one type of data (either genes/proteins or metabolites):

- https://www.genenames.org/tools/multi-symbol-checker/
- https://www.metaboanalyst.ca/MetaboAnalyst/upload/ConvertView.xhtml

These tools currently do not have an API or other form of programmatic access, leading to issues in big OMICS data analysis.

### [Omics IDRefiner](https://tabbassidaloii.shinyapps.io/OmicsIDRefiner/):
To address the challenges of integrating data from different biological sources that contain secondary identifiers, we developed a user-friendly Shiny app called Omics IDRefiner, which provides two key functions:

1. IDRefiner:
provides statistics on the percentage of secondary identifiers in the dataset and converts outdated secondary identifiers to current primary identifiers, if available. The IDRefiner functionality currently covers secondary identifiers from HGNC , HMDB , ChEBI and Wikidata which can be converted to the corresponding primary identifier from the initial database. After this step, the IDMapper can be used to convert the primary-ID-enhanced dataset to any other database currently supported by BridgeDb:
- The full overview of supported databases is available on the BridgeDb website (bridgedb.org/pages/system-codes).

2. IDMapper:
uses BridgeDb's REST-API to convert identifiers.

The metadata for the latest update of the mapping files is also available to users for queries within the app.

Future development entails updating the Secondary-to-Primary identifier mapping files regularly via GitHub Actions to ensure accuracy.

