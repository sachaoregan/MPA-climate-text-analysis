# Climate change text analysis

**Author**: Sacha O'Regan\
**Affiliation**: MC Wright and Associates Ltd.

## Required inputs

1. `WDPA_English_Combined_2021-07-16_so.xlsx`
2. `luCountryGroup.csv` 
3. `ManagementPlans_R` folder containing PDF documents
4. `SearchTerms/searchcomponents.csv` containing climate components
5. `SearchTerms/search-scienceterms.csv` containing science terms
6. `manual-pdf-data-pull.csv` containing manual PDF search results
7. `replace_ospar_w_new_desigs.csv` to switch out OSPAR designation for the MPAs' other designation in one of the figures.

`ManagementPlans_R` folder and `manual-pdf-data-pull.csv` available through corresponding author, Karen Hunter, Fisheries and Oceans Canada.

**For a summary table of all MPA management plans included in the analysis and the results, download `summary-table-climate-mpa-analysis-management-plans.csv`** 

## Required packages

* ggplot2
* readr
* readxl
* forcats
* tidyr
* plyr
* dplyr
* pdftools
* purrr
* furrr
* stringi
* stringr
* future
* viridis
* ggsidekick; to install:

```r
# install.packages("devtools")
devtools::install_github("seananderson/ggsidekick")
```
## Setup
To begin, open `MPA-climate-text-analysis.Rproj`. Then, either open `runall.R` to source all scripts in sequence OR open and run the text search and plotting scripts individually, as described in the following sections. 

## Text Search 
### Scripts:

`1.countTerms_Setup.R`
This script must be run 1st to generate the data and figure output directories and the .rds files that feed into script 2. The .rds files it creates are `list-of-pdfs.rds`, which lists all PDF files, and `mpa-metadata.rds`, which contains the metadata associated with each PDF (excluding publication year). Does not need to be run again after the directories and .rds files are generated. 

Two PDFs were found to have bad OCR. This script excludes them from the text analysis.

`2.countTerms_Search.R`
This script contains the text search code. First, the code searches all PDFs for one of five climate change related terms: "climate change", "global warming", "extreme events", "natural variability", "climate variability". PDFs that contain at least one of these terms are then searched for the component words provided in `searchcomponents.csv `(e.g., abundance, diversity, recreation, etc.). PDFs that contain at least one of the climate change terms are also searched for the science terms provided in `search-scienceterms.csv` (e.g., monitor, metric). 

This script also counts the total words in each PDF and obtains the publication year for each PDF. The year search code works by pulling out the first 4-number string in the PDF text. If the first 4-number string is preceded by the word "act", "act of", "regulation", "regulations", it takes the second 4-number string to be the publication year. The reason for this is that the first year on some PDF title pages is the year of enactment of legislation, not the publication year. 

This script must be run before running the plotting scripts.

## Plotting
### Scripts:

`climate_change_term_plots.R`
Plots trends in the five climate change related terms ("climate change", "global warming", "extreme events", "natural variability", "climate variability") by geographical region (aka "Grouping" in the code) and publication year.

Plots publication year for all PDFs (with and without climate change related terms).

`climate_component_plots.R`
Plots trends in the climate change related components (e.g., abundance, diversity, recreation, etc.) and their dimensions (ecological, physical, sociological) by geographical region (aka "Grouping" in the code) and publication year.

`science_terms_plots.R`
Plots the science terms retrieved from the text search. This file also contains the code to filter the PDFs down to a final list of PDFs to be manually searched for evidence of climate change effects analysis, planning, and monitoring. Only PDFs that contained at least two of the words "Metric", "Indicator", "Transects", "Survey", "Target", "Threshold" and contained at least three instances of at least one of the two words were retained. The filter retained 223 PDFs to be searched manually (see the Manual PDF data pull methods below).

`manual_pdf_search_plots.R`
Plots trends in climate change effects analysis, planning, and monitoring using the data pulled manually from the PDFs (data sourced from the file `manual-pdf-data-pull.csv`). 

## Manual PDF data pull
PDFs were searched manually for information pertaining to climate change effects analysis, planning, and monitoring. The data were collated in the file `manual-pdf-data-pull.csv`. 223 PDFs were searched. The meaning of the column headings in the file `manual-pdf-data-pull.csv` is described below:

`disc_of_climate_effects_on_park_ecological` - did the PDF discuss any past, present, or future ecological effects of climate change on the park. Not discriminating on level of detail. Yes/No.

`disc_of_climate_effects_on_park_physical` -  did the PDF discuss any past, present, or future physical effects of climate change on the park. Not discriminating on level of detail. Yes/No.

`disc_of_climate_effects_on_park_sociological`	-  did the PDF discuss any past, present, or future sociological effects of climate change on the park. Not discriminating on level of detail. Yes/No.

`climate_objectives`	- did the PDF contain one or more objectives that explicitly mentioned climate change or one of its consequences (e.g., sea level rise). Yes/No.

`climate_strategies_actions`	- did the PDF contain one or more strategies that explicitly mentioned climate change or one of its consequences (e.g., sea level rise). Yes/No.

`detailed_methods`	- did the PDF contain detailed survey/monitoring methods. Yes/No.

`establishes_baseline_conditions`	- did the PDF discuss baseline conditions in the park(s) or state that they would be established in the future. Yes/Ongoing/Planned/No (Ongoing was entered if the PDF stated that some type of baseline monitoring had already begun; Planned was entered if the plan stated that the MPA intends to complete baseline monitoring in the future).

`indicators`	- did the PDF list monitoring indicators or state that they would be established in the future. Yes/Planned/No (Planned was entered if the PDF stated that the park(s) intends to select indicators in the future).

`metrics`	- did the PDF list monitoring metrics for the indicators or state that they would be established in the future. Yes/Planned/No. Generally, it was assumed that PDFs with stated plans to establish indicators would also decide on metrics. 

`targets`	- did the PDF list targets for the condition of the indicators or state that they would be established in the future. Yes/Some/Planned/No (Some was entered if there were targets for some indicators but not all).

`thresholds`	- did the PDF list thresholds for the condition of the indicators or state that they would be established in the future. Yes/Some/Planned/No.

`indicat_thresh_metrics_explicitly_linked_to_climate`	- Were the indicators/metrics/threshold explicitly linked to climate change (i.e., do they directly track climate changes OR are "climate change" or "sea level" mentioned in the same sentence as the indicator/metric/threshold). Yes/No/NA (NA was entered if there were no indicators listed or planned).

`commitment_to_climate_monitoring` - Did the PDF explicitly commit to monitoring or adapting to climate change. Yes/No.

`references_other_monitoring`	- Did the PDF mention other climate change monitoring or mitigation efforts being completed by agencies other than park staff. Yes/No.

`other_monitoring` - List of agencies (other than park staff) working on climate change monitoring or mitigation efforts that the park(s) will work with or rely upon. Or, other climate change initiatives (other than park-specific initiatives) that the park(s) will rely upon.

Manual PDF search results were used to calculate climate change robustness scores. ‘No’ was assigned a score of 0, ‘Planned’ a score of ‘1’, and ‘Yes’ a score of 2. ‘Yes’ and ‘Ongoing’ were assigned an equivalent score of 2, and ‘Planned’ and ‘Some’ an equivalent score of 1. ‘NA’ was scored as 0. 
