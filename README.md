# Text Analysis - Marine Protected Area Management Plan Review

__Main author:__  Katie Gale  
__Affiliation:__  Fisheries and Oceans Canada (DFO)   
__Group:__        Marine Spatial Ecology and Analysis   
__Location:__     Institute of Ocean Sciences   
__Contact:__      e-mail: katie.gale@dfo-mpo.gc.ca | tel: 250-363-6411


- [Objective](#objective)
- [Summary](#summary)
  * [Required inputs](#required-inputs)
  * [Semi-optional inputs](#semioptional-inputs)
  * [Outputs](#outputs)
- [Contents](#contents)
  * [1.countTerms_Setup.R](#content-setup)
  * [2a.countTerms_fullMethod.R](#content-full)
  * [2b.countTerms_searchMethod.R](#content-search)
  * [3.countTerms_results.R](#content-results)
- [Methods](#methods)
  * [1.countTerms_Setup.R](#methods-setup)
  * [2a.countTerms_fullMethod.R](#methods-full)
  * [2b.countTerms_searchMethod.R](#methods-search)
  * [3.countTerms_results.R](#methods-results)
- [Caveats](#caveats)


## Objective
The objective of this code is to summarize the full text of a series of PDFs, and to search for a set of terms of interest.

## Summary
These scripts were created for searching marine protected area (MPA) management plans, but could be adapted for a variety of uses. As written, the script is not completely generalized (not plug and play for other uses), but could be pretty easily adapted. 
PDFs were obtained for marine protected areas listed in the World Database on Protected Areas (WDPA, https://www.protectedplanet.net/) as of February 2019.

#### Required inputs
- a folder of PDF documents with embedded text.
   - If the PDFs are simple scanned images, optical character recognition (OCR) must be run first. 
   - multi-document/multi-page plans should be combined into one PDF using program like Adobe Acrobat Pro.
- `searchTerms...csv`: a csv of terms to search for.

#### Semi-optional inputs
These are not actually required to carry out the text search, but are needed for summarizing the results. Currently the script fails without them, but it could be modified to skip those steps. 
- `WDPA_English_....xlsx`: a spreadsheet with the file names and associated attributes such as MPA name and country of origin.
- `luCountryGroup.csv`: a helper table to link country of origin to a higher-level grouping.

#### Outputs
- `FULL-full_outputMatrix.rds`: paper x term matrix from full analysis, listing each returned term.                 
- `FULL-root_outputMatrix.rds`: paper x term matrix from full analysis, rolled up for each root term searched.        
- `FULL_nWordsPerPaper.csv`: number of words present in each paper.                                 
- `FULL_termFreq.csv`: total count of each term across all papers.                                                                                         
- `SEARCH_countTermOcurrencePerPaper_2020-04-23.csv`:  Park x term matrix, including other park attributes. If PDFs refer to multiple parks, there will be duplicate lines for identical PDFs. **main output** 
- `SEARCH_countTermOcurrencePerPaperNoLookup_2020-04-23.csv`: Paper x term matrix. One line per PDF. **main output** 
- `SEARCH_outputMatrix.rds`: paper x term matrix from search analysis.
- `SEARCH_PDF_errorSummary_2020-04-23.csv`: error report from search analysis, listing papers that did not parse.                   
- `SEARCH_PDF_errorSummary_wPAName_2020-04-23.csv`: error report from search analysis, listing papers that did not parse as well as their respective parks.          
- `SEARCH_readPDFs_2020-04-23.txt`                   

- results/%method%
  - `_results_terms_by_Paper_2020-04-23.csv`: list of papers, with presence-absence for each term  
  - `_results_terms_by_Park_2020-04-23.csv`: list of protected areas, with presence-absence for each term     
  - `_results_terms_countPapers_2020-04-23.csv`: % and # of papers with each term
  - `_results_terms_countParks_2020-04-23.csv`: % and # of protected areas with each term 
  - `_TermsByPaper.png`: figure, % of papers with term                        
  - `_TermsByPaper_ByGrouping.png`: figure, % of papers with term, broken down by country of origin             
  - `_TermsByPark.png`: figure, % of protected areas with term                          
  - `_TermsByPark_ByGrouping.png`: figure, % of protected areas with term, broken down by country of origin        

## Contents
There are 4 scripts that must be run in order. Do not clear the environment after running the setup. If the set of PDFs to search is very large, R will run out of memory and will not be able to run script 2b directly after 2a. If this happens, close R completely, re-run script 1, and then run 2b. 
- The only output of 2a that is used in 2b is `FULL_nwordsPerPaper.csv`, which counts overall number of words in each pdf. This is useful to detect pdfs that did not parse correctly, or to do calculations on word prevalence. However, it's not fully necessary. So if you don't want the full corpus of each document, you could remove line 104 from script 2b, and then could skip 2a altogether.
- note that different search term lists are appropriate for FULL vs SEARCH. FULL can take wildcards but not multiword phrases, with SEARCH being the opposite.

### 1.countTerms_Setup.R
This script loads the packages, sets up the input and output directories, and defines the search terms of interest. It must be run before scripts 2a and 2b. 

### 2a.countTerms_fullMethod.R
Create full "corpus" (text body) and then search that for individual words and word roots. All terms in the PDFs are extracted and included in a term-document matrix. The matrix is searched using a list of root words (e.g., ecol, commun, integr). 
- The matches might be relevant (ecological, community, integrity), or not (ecologist, ecologische, communion, communicable,  communicates, integral, integrados, integrifolium). Irrelevant matches need to be manually removed.
- Can summarize results by pooling all matching terms for each root word or individually by each matching term. 
- Takes longer, but the output files are easy to query and can be used and subset in other analyses (if your search terms change, you don’t need to have all the PDFs to do another analysis).
- Can only match single words (not phrases/multi-word terms).
- Can see which terms are matching the roots (e.g., "^govern" matches "governor", but you might just want government and governance)
- Limited error reporting (i.e., papers that didn't parse)
- Because all terms are included, the intermediate objects are very large (gbs) and can bog down computer. 
- Requires manual input in an intermediate step to remove words that matched but that we don't want
- May be better at reading pdfs that have problems

### 2b.countTerms_searchMethod.R
- The PDFs are read, but only matching terms/phrases are extracted. This is faster, but means you need to re-run the search when you change your terms (need the PDFs).
- Can do multi-word phrases, but not wildcards.
- Cannot accept the same root word search list as the full method - need to specify whole words. 
- May be more sensitive to problems in the PDFs.

### 3.countTerms_results.R
- Summarize the results into some figures and tables. 

## Methods

### 1.countTerms_Setup.R
- Set the working directory and define the output directory
- Load the search terms and the spreadsheet with the list of PDFs, their respective parks, and their attributes.
- Check to ensure that all the PDFs you want to search (those in the spreadsheet) are in the folder, and vice versa. 
 
### 2a.countTerms_fullMethod.R
- Read each PDF and store the full text in a 'corpus' object (note: many errors will show up in the console - this is fine).
- create term-document matrix, which has all terms in each PDF.
- Count the number of terms in each PDF, and write to file (`FULL_nWordsPerPaper.csv`)
- Clean up term list, by removing terms only in one paper and cleaning up nonsense characters.
- Optional - print a word cloud of all terms in all PDFs.
- Search list of all terms in each PDF for the list of terms or roots (wildcards) defined in script 1.
- **important - a manual step is needed at line 104, to remove nonsense and bad words from the term frequency list.**
- summarize matching words into their roots (e.g., "ecologist" and "ecology" both came from root term "ecol*"
- write matrices of roots x PDF and matching words x PDF.

### 2b.countTerms_searchMethod.R
- Search each PDF using the list of terms defined in script 1.
- An error log (`SEARCH_PDF_errorSummary_DATE.csv` and `SEARCH_PDF_errorSummary_wPAName_DATE.csv`) is written to be able to identify PDFs that did not parse.
- Create matrix of term x PDF
- Add attributes, as well as number of words per paper (from 2a), and write results. 
  - note that if there are PDFs that correspond to multiple protected areas, the results WITH lookup will have duplicate lines.
  - write both lookup (duplicate lines for PDFs, but unique lines per protected area) and no lookup (1 line per PDF) versions ofthe results (`SEARCH_countTermOcurrencePerPaperNoLookup_DATE.csv` and `SEARCH_countTermOcurrencePerPaper_DATE.csv`)

### 3.countTerms_results.R
- User specifies which outputs to create graphs and tables for (one of "full" (all matching single terms), "root" (summed root/wildcards), or "search" (multi-word phrases)) 
- Terms are summarized by park, higher-level grouping, and paper
- Csv and figures are written showing summaries - e.g., counts of terms in each category by country of origin.

## Caveats
- Some PDFs with bad text encoding do not parse very well, and return unicode characters, nonsense terms, spacing issues, etc. In theory these terms could be fixed and replaced, but in practice that is too manually tedious and computationally intensive.  
- I have opted to just delete bad terms, since they generally occur very infrequently compared to the correctly-matched terms.

