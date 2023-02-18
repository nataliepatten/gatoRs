<!-- badges: start -->
[![R-CMD-check](https://github.com/nataliepatten/gatoRs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nataliepatten/gatoRs/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<img align="right" src="man/figures/gators_hex_sticker.png" width=250>

# gatoRs: Geographic and Taxonomic Occurrence R-Based Scrubbing
**Natalie N. Patten, Michelle L. Gaynor, Douglas E. Soltis, and Pamela S. Soltis** 



## Suggested Workflow
### Download data
To download data using gators_download(), you will need a list of possible names for your species (including synonyms), with the first name in the list the accepted name if using the species code option for GBIF search. You will also need the path and name of your file, including the ".csv" ending.

Example:
```
gators_download(synonyms_list = c("Asclepias curtissii", "Asclepias aceratoides",
                                  "Asclepias arenicola", "Oxypteryx arenicola", 
                                  "Oxypteryx curtissii"), 
                newFileName = "base_folder/other_folder/my_file.csv")
```

Optional parameters include gbif_match and idigbio_filter. gbif_match allows you to search by fuzzy matching records to the scientific name (default, gbif_match = "fuzzy") or to search for the associated species key using GBIF’s backbone taxonomy system (gbif_match = "code").

If set to TRUE (default, recommended), idigbio_filter fuzzy matches taxonomic columns to provided taxon names. This filters the dataset for relevant data.

The function also generates scientificName, genus, specificEpithet, and infraspecificEpithet columns based on available data and parsing and then fixes incorrect capitalization of species names to give the data a cleaner look.

An overview of the returned data is summarized in the table below.

Data returned | Category | Darwin Core Link |
---| --- | --- |
scientificName | Taxonomic information | [http://rs.tdwg.org/dwc/terms/scientificName](http://rs.tdwg.org/dwc/terms/scientificName) |
genus | Taxonomic information |[https://dwc.tdwg.org/list/#dwc_genus](https://dwc.tdwg.org/list/#dwc_genus)|
specificEpithet | Taxonomic information |[https://dwc.tdwg.org/list/#dwc_specificEpithet](https://dwc.tdwg.org/list/#dwc_specificEpithet)|
infraspecificEpithet | Taxonomic information |[http://rs.tdwg.org/dwc/terms/infraspecificEpithet](http://rs.tdwg.org/dwc/terms/infraspecificEpithet)|
basisOfRecord | Event information |[http://rs.tdwg.org/dwc/terms/basisOfRecord](http://rs.tdwg.org/dwc/terms/basisOfRecord)|
eventDate | Event information |[http://rs.tdwg.org/dwc/terms/eventDate](http://rs.tdwg.org/dwc/terms/eventDate)|
institutionCode | Current record storage information |[http://rs.tdwg.org/dwc/terms/institutionCode](http://rs.tdwg.org/dwc/terms/institutionCode)|
collectionCode | Current record storage information |[http://rs.tdwg.org/dwc/terms/collectionCode](http://rs.tdwg.org/dwc/terms/collectionCode)|
collectionID | Current record storage information |[http://rs.tdwg.org/dwc/terms/collectionID](http://rs.tdwg.org/dwc/terms/collectionID)|
identificationID | Current record storage information |[http://rs.tdwg.org/dwc/terms/identificationID](http://rs.tdwg.org/dwc/terms/identificationID)|
informationWithheld | Current record storage information |[http://rs.tdwg.org/dwc/terms/informationWithheld](http://rs.tdwg.org/dwc/terms/informationWithheld) |
country | Original record location information |[http://rs.tdwg.org/dwc/terms/country](http://rs.tdwg.org/dwc/terms/country)|
county | Original record location information |[http://rs.tdwg.org/dwc/terms/county](http://rs.tdwg.org/dwc/terms/county)|
stateProvince | Original record location information |[http://rs.tdwg.org/dwc/terms/stateProvince](http://rs.tdwg.org/dwc/terms/stateProvince)|
locality | Original record location information |[http://rs.tdwg.org/dwc/terms/locality](http://rs.tdwg.org/dwc/terms/locality)|
latitude | Original record location information |[http://rs.tdwg.org/dwc/terms/decimalLatitude](http://rs.tdwg.org/dwc/terms/decimalLatitude)|
longitude | Original record location information |[http://rs.tdwg.org/dwc/terms/decimalLongitude](http://rs.tdwg.org/dwc/terms/decimalLongitude)|
coordinateUncertaintyInMeters | Original record location information |[http://rs.tdwg.org/dwc/terms/coordinateUncertaintyInMeters](	http://rs.tdwg.org/dwc/terms/coordinateUncertaintyInMeters)|
habitat | Original record location information |[http://rs.tdwg.org/dwc/terms/habitat](	http://rs.tdwg.org/dwc/terms/habitat)|


### Find redacted data
To find data that needs to be manually received by an institution via a permit (or removed from the dataset), use needed_records(). Use your downloaded data from the previous step as input. After receiving the data from herbaria, manually enter into the original dataset.

Example: 
```
redacted_info <- needed_records(occurrence_records)
```
### Find data to georeference
To find data lacking coordinates but containing locality information, use need_to_georeference(). Use your downloaded data from the first step as input. You should georeference these records and manually enter the information into the dataset.

Example: 
```
to_georeference <- need_to_georeference(occurrence_records)
```
### Filter for applicable taxonomic names
To find data containing scientific names corresponding to your desired species, use filter_select_name(). Use your downloaded data from the first step as input, as well as a synonyms list, the accepted name, and the filter option (exact, fuzzy, or interactive).

Example:
```
occurrence_records <- filter_select_name(occurrence_records, synonyms_list = c("Asclepias curtissii", "Asclepias aceratoides", "Asclepias arenicola", "Oxypteryx arenicola", "Oxypteryx curtissii"), filter = "interactive", accepted_name = "Asclepias curtissii")
```
### Find and remove flagged points
To find records that may have problematic coordinates, use find_flagged(). This will let you interactively manually remove points deemed improper by viewing the points on a graph. Use your downloaded data from the previous step as input.

Example: 
```
occurrence_records <- find_flagged(occurrence_records)
```
### Final cleaning
To remove duplicate points and reduce the clustering of points, use our final_clean() function. This function also offers an interactive method to look at (and remove, if desired) types of basis of record.

Example: 
```
occurrence_records <- final_clean(occurrence_records)
```
