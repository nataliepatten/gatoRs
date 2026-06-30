# Cite Data - Get GBIF citations

The `citation_bellow` function retrieves and returns the citation
information for records from GBIF, or where aggregator = "GBIF".

## Usage

``` r
citation_bellow(df, id = "ID", aggregator = "aggregator")
```

## Arguments

- df:

  Data frame of occurrence records returned from
  [`gators_download()`](https://nataliepatten.github.io/gatoRs/reference/gators_download.md).

- id:

  Default = "ID". The name of the id column in the data frame, which
  contains unique IDs defined from GBIF (keys) or iDigBio (UUID).

- aggregator:

  Default = "aggregator". The name of the column in the data frame that
  identifies the aggregator that provided the record. This is equal to
  iDigBio or GBIF.

## Value

Returns a list with citation information for the GBIF data downloaded.

## Details

This function requires the rgbif package.

## Examples

``` r
# \donttest{
citations <- citation_bellow(data)
#> Warning: gbif_citation() for datasetKeys is deprecated since rgbif 3.8.0. 
#> Use rgbif::derived_dataset() instead.
#> [[1]]
#> [[1]]$text
#> [1] "iNaturalist contributors, iNaturalist (2026). iNaturalist Research-grade Observations. iNaturalist.org. Occurrence dataset https://doi.org/10.15468/ab3s5x accessed via GBIF.org on 2026-06-30."
#> 
#> [[1]]$citationProvidedBySource
#> [1] FALSE
#> 
#> 
#> Warning: Unknown or uninitialised column: `rights`.
#> Warning: gbif_citation() for datasetKeys is deprecated since rgbif 3.8.0. 
#> Use rgbif::derived_dataset() instead.
#> [[1]]
#> [[1]]$text
#> [1] "iNaturalist contributors, iNaturalist (2026). iNaturalist Research-grade Observations. iNaturalist.org. Occurrence dataset https://doi.org/10.15468/ab3s5x accessed via GBIF.org on 2026-06-30."
#> 
#> [[1]]$citationProvidedBySource
#> [1] FALSE
#> 
#> 
#> Warning: Unknown or uninitialised column: `rights`.
#> Warning: gbif_citation() for datasetKeys is deprecated since rgbif 3.8.0. 
#> Use rgbif::derived_dataset() instead.
#> [[1]]
#> [[1]]$text
#> [1] "iNaturalist contributors, iNaturalist (2026). iNaturalist Research-grade Observations. iNaturalist.org. Occurrence dataset https://doi.org/10.15468/ab3s5x accessed via GBIF.org on 2026-06-30."
#> 
#> [[1]]$citationProvidedBySource
#> [1] FALSE
#> 
#> 
#> Warning: Unknown or uninitialised column: `rights`.
#> Warning: gbif_citation() for datasetKeys is deprecated since rgbif 3.8.0. 
#> Use rgbif::derived_dataset() instead.
#> [[1]]
#> [[1]]$text
#> [1] "iNaturalist contributors, iNaturalist (2026). iNaturalist Research-grade Observations. iNaturalist.org. Occurrence dataset https://doi.org/10.15468/ab3s5x accessed via GBIF.org on 2026-06-30."
#> 
#> [[1]]$citationProvidedBySource
#> [1] FALSE
#> 
#> 
#> Warning: Unknown or uninitialised column: `rights`.
#> Warning: gbif_citation() for datasetKeys is deprecated since rgbif 3.8.0. 
#> Use rgbif::derived_dataset() instead.
#> [[1]]
#> [[1]]$text
#> [1] "iNaturalist contributors, iNaturalist (2026). iNaturalist Research-grade Observations. iNaturalist.org. Occurrence dataset https://doi.org/10.15468/ab3s5x accessed via GBIF.org on 2026-06-30."
#> 
#> [[1]]$citationProvidedBySource
#> [1] FALSE
#> 
#> 
#> Warning: Unknown or uninitialised column: `rights`.
#> Warning: gbif_citation() for datasetKeys is deprecated since rgbif 3.8.0. 
#> Use rgbif::derived_dataset() instead.
#> [[1]]
#> [[1]]$text
#> [1] "University of Georgia Herbarium [GA] (2026). University of Georgia Herbarium. Occurrence dataset https://doi.org/10.15468/weknsr accessed via GBIF.org on 2026-06-30."
#> 
#> [[1]]$citationProvidedBySource
#> [1] FALSE
#> 
#> 
#> Warning: Unknown or uninitialised column: `rights`.
#> Warning: gbif_citation() for datasetKeys is deprecated since rgbif 3.8.0. 
#> Use rgbif::derived_dataset() instead.
#> [[1]]
#> [[1]]$text
#> [1] "Ramirez J, Watson K, Feder L, Gjieli E, Sessa E (2026). The New York Botanical Garden Herbarium (NY). Version 1.101. The New York Botanical Garden. Occurrence dataset https://doi.org/10.15468/6e8nje accessed via GBIF.org on 2026-06-30."
#> 
#> [[1]]$citationProvidedBySource
#> [1] FALSE
#> 
#> 
#> Warning: Unknown or uninitialised column: `rights`.
#> Warning: gbif_citation() for datasetKeys is deprecated since rgbif 3.8.0. 
#> Use rgbif::derived_dataset() instead.
#> [[1]]
#> [[1]]$text
#> [1] "Ramirez J, Watson K, Feder L, Gjieli E, Sessa E (2026). The New York Botanical Garden Herbarium (NY). Version 1.101. The New York Botanical Garden. Occurrence dataset https://doi.org/10.15468/6e8nje accessed via GBIF.org on 2026-06-30."
#> 
#> [[1]]$citationProvidedBySource
#> [1] FALSE
#> 
#> 
#> Warning: Unknown or uninitialised column: `rights`.
#> Warning: gbif_citation() for datasetKeys is deprecated since rgbif 3.8.0. 
#> Use rgbif::derived_dataset() instead.
#> [[1]]
#> [[1]]$text
#> [1] "Orrell T, Informatics and Data Science Center - Digital Stewardship (2026). NMNH Extant Specimen Records (USNM, US). Version 1.109. National Museum of Natural History, Smithsonian Institution. Occurrence dataset https://doi.org/10.15468/hnhrg3 accessed via GBIF.org on 2026-06-30."
#> 
#> [[1]]$citationProvidedBySource
#> [1] FALSE
#> 
#> 
#> Warning: Unknown or uninitialised column: `rights`.
#> Warning: gbif_citation() for datasetKeys is deprecated since rgbif 3.8.0. 
#> Use rgbif::derived_dataset() instead.
#> [[1]]
#> [[1]]$text
#> [1] "Orrell T, Informatics and Data Science Center - Digital Stewardship (2026). NMNH Extant Specimen Records (USNM, US). Version 1.109. National Museum of Natural History, Smithsonian Institution. Occurrence dataset https://doi.org/10.15468/hnhrg3 accessed via GBIF.org on 2026-06-30."
#> 
#> [[1]]$citationProvidedBySource
#> [1] FALSE
#> 
#> 
#> Warning: Unknown or uninitialised column: `rights`.
# }
```
