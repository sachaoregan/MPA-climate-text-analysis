# Climate change text analysis

**Author**: Sacha O'Regan\
**Affiliation**: MC Wright and Associates Ltd.

## Required inputs

1. `WDPA_English_Combined_2020-03-25_kg.xlsx`
2. `luCountryGroup.csv`
3. `ManagementPlans_R` folder containing PDF documents
4. `searchcomponents.csv` containing climate components
5. `search-scienceterms.csv` containing science terms

## Required packages

* ggplot2
* tidyr
* dplyr
* purrr
* future
* ggsidekick; to install:

```r
# install.packages("devtools")
devtools::install_github("seananderson/ggsidekick")
```

## Text Search 
### Scripts:

To begin, open `MPA-climate-text-analysis.Rproj`. Then, run the two scripts below. 

`1.countTerms_Setup.R`
This script must be run 1st to generate the data and figure output directories and the .rds files that feed into script 2. The .rds files it creates are `list-of-pdfs.rds`, which lists all PDF files, and `mpa-metadata.rds`, which contains the metadata associated with each PDF (excluding publication year). Does not need to be run again after the directories and .rds files are generated. 

Two PDFs were found to have bad OCR. This script excludes them from the text analysis entirely.

`2.countTerms_Search.R`
This script contains the text search code. First, the code searches all PDFs for one of five climate change related terms: "climate change", "global warming", "extreme events", "natural variability", "climate variability". PDFs that contain at least one of these terms are then searched for the component words provided in `searchcomponents.csv `(e.g., abundance, diversity, recreation, etc.). PDFs that contain at least one of the climate change terms are also searched for the science terms provided in `search-scienceterms.csv` (e.g., monitor, metric). 

This script also counts the total words in each PDF and obtains the publication year for each PDF. The year search code works by pulling out the first 4-number string in the PDF text. If the first 4-number string is preceded by the word "act", "regulation", "regulations", it takes the second 4-number string to be the publication year. The reason for this is that the first year on some PDF title pages is the year of enactment of legislation, not the publication year. 

The publication year could not be identified for six PDFs (beyond the two with bad OCR excluded in `1.countTerms_Setup.R`) for reasons noted in `missing-yrs.csv`. These PDFs were therefore excluded from plots where publication year was a dimension of interest. 

This script must be run before running the plotting scripts.

## Plotting
### Scripts:

`climate_change_term_plots.R`
Plots trends in the five climate change related terms ("climate change", "global warming", "extreme events", "natural variability", "climate variability") by geographical region (aka "Grouping" in the code) and publication year.

Plots publication year for all PDFs (with and without climate change related terms).

`climate_component_plots.R`
Plots trends in the climate change related components (e.g., abundance, diversity, recreation, etc.) and their dimensions (ecological, physical, sociological) by geographical region (aka "Grouping" in the code) and publication year.

`science_terms_plots.R`
Plots the science terms retrieved from the text search. This file also contains the code to filter the PDFs down to a final list of PDFs to be manually searched for evidence of climate change effects analysis, planning, and monitoring. Only PDFs that contained at least two of the words "Metric", "Indicator", "Transects", "Survey", "Target", "Threshold" and contained at least three instances of at least one of the two words were retained. The filter retained 221 PDFs to be searched manually (see the Manual PDF data pull methods below). The code assigns a random sort order to the PDFs.

`manual_pdf_search_plots.R`
Plots trends in climate change effects analysis, planning, and monitoring using the data pulled manually from the PDFs (data sourced from the file `manual-pdf-data-pull.csv`). 

## Manual PDF data pull
PDFs were searched manually (following the random sort order) for information pertaining to climate change effects analysis, planning, and monitoring. The data were collated in the file `manual-pdf-data-pull.csv`. 181 of the 221 PDFs were searched; the remaining PDFS were not searched due to budget. `The manual_pdf_search_plots.R` script can be re-run as more PDFs are searched. The meaning of the column headings in the file `manual-pdf-data-pull.csv` is described below:

`sort_order` - random sort order assigned to the PDFs in the script `science_terms_plots.R`. PDFs were searched in this order (excluding PDF 219 ["1024_Biscayne_National_Park.pdf"], which was searched prior to generating the random order numbers)

`disc_of_climate_effects_on_park_ecological` - did the PDF discuss any past, present, or future ecological effects of climate change on the park. Not discriminating on level of detail. Yes/No.

`disc_of_climate_effects_on_park_physical` -  did the PDF discuss any past, present, or future physical effects of climate change on the park. Not discriminating on level of detail. Yes/No.

`disc_of_climate_effects_on_park_sociological`	-  did the PDF discuss any past, present, or future sociological effects of climate change on the park. Not discriminating on level of detail. Yes/No.

`climate_objectives`	- did the PDF contain one or more objectives that explicitly mentioned climate change or one of its consequences (e.g., sea level rise). Yes/No.

`climate_strategies_actions`	- did the PDF contain one or more strategies that explicitly mentioned climate change or one of its consequences (e.g., sea level rise). Yes/No.

`detailed_methods`	- did the PDF contain detailed survey/monitoring methods. Yes/No.

`establishes_baseline_conditions`	- did the PDF discuss baseline conditions in the park(s) or state that they would be established in the future. Yes/No/Ongoing ("Ongoing" was entered if the PDF stated that some type of baseline monitoring had already begun).

`indicators`	- did the PDF list monitoring indicators or state that they would be established in the future. Yes/No/Planned ("Planned" was entered if the PDF stated that the park(s) intends to select indicators in the future).

`metrics`	- did the PDF list monitoring metrics for the indicators or state that they would be established in the future. Yes/No/Planned ("Planned" was entered if the PDF stated that they intend to select metrics in the future). Generally, it was assumed that PDFs with stated plans to establish indicators would also decide on metrics. 

`thresholds`	- did the PDF list targets or thresholds for the condition of the indicators or state that they would be established in the future. Yes/No/Planned ("Planned" was entered if the PDF stated that the park(s) intends to set targets or thresholds in the future).

`indicat_thresh_metrics_explicitly_linked_to_climate`	- Were the indicators/metrics/thresholds explicitly linked to climate change (i.e., do they directly track climate changes OR are "climate change" or "sea level" mentioned in the same sentence as the indicator/metric/threshold). Yes/No/NA ("NA was entered if there were no indicators listed or planned).

`commitment_to_climate_monitoring` - Did the PDF explicitly commit to monitoring or adapting to climate change. Yes/No.

`references_other_monitoring`	- Did the PDF mention other climate change monitoring or mitigation efforts being completed by agencies other than park staff. Yes/No.

`other_monitoring` - List of agencies (other than park staff) working on climate change monitoring or mitigation efforts that the park(s) will work with or rely upon. Or, other climate change initiatives (other than park-specific initiatives) that the park(s) will rely upon.

Note, the PDFs were not read in their entirety. For each PDF, I searched for the following key terms to take me to the sections relevant to each column: 

* climate change
* warm
* sea level
* objective
* strategies
* indicator
* threshold
* metric
* parameter
* baseline        
