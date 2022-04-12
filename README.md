# gatoRs
## Suggested Workflow
### Download data
To download data using gators_download(), you will need a list of possible names for your species (including synonyms), with the first name in the list the accepted name if using the species code option for GBIF search. You will also need the path and name of your file, including the ".csv" ending.

Example: gators_download(synonyms_list = c("Asclepias curtissi", "Asclepias aceratoides", "Asclepias arenicola", "Oxypteryx arenicola", "Oxypteryx curtissii"), newFileName = "base_folder/other_folder/my_file.csv", filter = "fuzzy")
### Find redacted data
To find data that needs to be manually received by an institution via a permit (or removed from the dataset), use needed_records(). Use your downloaded data from the previous step as input.

Example: redacted_info <- needed_records(occurrence_records)
### Find data to georeference
To find data lacking coordinates but containing locality information, use need_to_georeference(). Use your downloaded data from the first step as input.

Example: to_georeference <- need_to_georeference(occurrence_records)
### Fix capitalization issues
To fix potential capitalization discrepancies with scientific names provided, use fix_names() to parse through the column and fix these issues. Use your downloaded data from the first step as input, along with the manually entered information for redacted records and coordinates from georeferencing.

Example: occurrence_records <- fix_names(occurrence_records)
### Find and remove flagged points
To find records that may have problematic coordinates, use find_flagged(). This will let you manually remove points deemed improper by viewing the points on a graph. Use your downloaded data from the previous step as input.

Example: occurrence_records <- find_flagged(occurrence_records)
### Final cleaning
....
