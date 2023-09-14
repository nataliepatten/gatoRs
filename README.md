<a href="https://nataliepatten.github.io/gatoRs/"><img align="right" src="man/figures/logo.png" width=250>

# gatoRs: Geographic and Taxonomic Occurrence R-Based Scrubbing
**Natalie N. Patten, Michelle L. Gaynor, Douglas E. Soltis, and Pamela S. Soltis** 
<!-- badges: start -->
[![R-CMD-check](https://github.com/nataliepatten/gatoRs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nataliepatten/gatoRs/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview
gatoRs (Geographic and Taxonomic Occurrence R-Based Scrubbing) provides users with tools for downloading and processing biodiversity data. Click [here](https://nataliepatten.github.io/gatoRs/) for the full user guide.

## Installation
```
install.packages("devtools")
devtools::install_github("nataliepatten/gatoRs")
```

**Due to our dependency on the package CoordinateCleaner, installation and loading of this package includes an R-spatial error message.  We are hopeful that the package maintainer for CoordinateCleaner will remove their dependency on rgdal prior to October 2023. However, if  CoordinateCleaner is not updated, we will revise our `process_flagged()` function and remove this error message.**

## Quick Start  
Our package aims to streamline downloading and processing of biodiversity specimen data. Here is a quick example of how to download and clean with our package.

Step 1: Download   

```
library(gatoRs)
galaxdf <- gators_download(synonyms.list = c("Galax urceolata", "Galax aphylla"), 
                write.file = FALSE,
                gbif.match = "fuzzy",
                idigbio.filter = TRUE)
```

Step 2: Clean   
  - We do not recommend jumping to our full clean function. See our extended introduction [here](https://nataliepatten.github.io/gatoRs/)!     
  
  
```
clean_data <- full_clean(galaxdf,
                         synonyms.list = c("Galax urceolata", "Galax aphylla"), 
                         digits = 3,
                         basis.list = c("Preserved Specimen","Physical specimen"), 
                         accepted.name = "Galax urceolata")
```


The gatoRs logo was created using the hexSticker package (https://github.com/GuangchuangYu/hexSticker).
