# -------------------------------------------------------------------- #
# Text Analysis - Marine Protected Area Management Plan Review
#
# Script 1: Setup
#
# __Main author:__  Katie Gale
# __Affiliation:__  Fisheries and Oceans Canada (DFO)
# __Group:__        Marine Spatial Ecology and Analysis
# __Location:__     Institute of Ocean Sciences
# __Contact:__      e-mail: katie.gale@dfo-mpo.gc.ca | tel: 250-363-6411
#
# Objective: Summarize the full text of a series of PDFs, and optionally to search for a set of terms of interest.
#
# Version date: 23 April 2020
# -------------------------------------------------------------------- #


# ---- Instructions ----
# There are 4 scripts in this directory that must be run in order. Do not clear the environment between scripts, unless you run out of memory between 2a and 2b. If the set of PDFs to search is very large, R will run out of memory and will not be able to run script 2b directly after 2a. If this happens, close R completely, re-run script 1, and then run 2b.
#
#
# Notes
# - all documents must be PDFs with text. If image-based PDFs, must be OCR'd (optical character recognition))
# - multi-document/multi-page plans should be combined into one PDF
# - can use Adobe Acrobat Pro (not available in approved programs) for this.
# - reading the PDFs throws many errors/warnings, most of which aren't fatal to the read, just caused by some bad characters in the doc

# Limitations
# - Some PDFs with bad text encoding do not parse very well, and return unicode characters, nonsense terms, spacing issues, etc. In theory these terms could be fixed and replaced, but in practice that is too manually tedious and computationally intensive.
# - I have opted to just delete bad terms, since they generally occur very infrequently compared to the correctly-matched terms.

# Potential additions
# - compare which PDFs are getting parsed through full method compared to those failing on search method
# - test wildcard for search method. trailing * seem to search though in function.


# ---- Setup ----
# Load packages and set up
# library(tm)
# library(pdftools)
# library(pdfsearch)
# library(tidyverse)
# library(plyr)
# library(reshape2)
# library(tidyr)
# library(wordcloud)
# library(RColorBrewer)
# library(data.table)

Rpdf <- tm::readPDF(control = list(text = "-layout"))

# ---- Input Setup -----

# Set or load terms of interest
compoundTerms <- readr::read_csv("SearchTerms/searchterms_kg.csv")
compoundTerms <- c(compoundTerms$dimension, compoundTerms$attribute, compoundTerms$searchterm)
allTerms <- tolower(unique(compoundTerms))
allTerms <- allTerms[allTerms != ""]
allTerms <- as.character(na.omit(allTerms))

# ---- Load list of reports ----

# dat<-read.xlsx("WDPA_English_Combined_2020-03-25_kg.xlsx")
dat <- readxl::read_xlsx("WDPA_English_Combined_2020-03-25_kg.xlsx")
names(dat) <- gsub(" ", ".", names(dat))
luCountryGroup <- read.csv("luCountryGroup.csv")

dat2 <- dat[, names(dat) %in% c("NAME", "DESIG", "WDPAID", "Parent.Nation", "Saved.File.Name", "Found", "Simplified.comments")]
dat2$PA.Name <- paste(dat2$NAME, dat2$DESIG)
dat2$Found <- tolower(dat2$Found)
dat2$Simplified.comments <- tolower(dat2$Simplified.comments)
dat2$PA.Name <- paste(dat2$NAME, dat2$DESIG)
dat2$paper <- c(as.character(dat2$Saved.File.Name))
dat2 <- dat2[, !names(dat2) %in% c("Saved.File.Name")]

# #optional - look at categories
# table(dat2$Found[dat2$Simplified.comments!="duplicate"])
# table(dat2$Simplified.comments[dat2$Found=="not appropriate"])
# table(dat2$Simplified.comments[dat2$Found=="no"])
# table(dat2$Simplified.comments[dat2$Found=="yes"])

# split out lines for parks with multiple papers
dat2 <- tidyr::separate_rows(dat2, paper, sep = ";")
dat2$paper <- gsub("^\\s|\\s$", "", dat2$paper)
dat2$paper <- paste0(gsub(".pdf$", "", dat2$paper), ".pdf")
names(dat2)[names(dat2) == "Parent.Nation"] <- c("Country")
dat2$paper[dat2$paper == "NA.pdf"] <- NA
dat2 <- merge(dat2, luCountryGroup, by = "Country", all.x = T)

length(unique(dat2$PA.Name)) # 1449 parks
length(unique(dat2$paper[!is.na(dat2$paper)])) # 663

# make lookup table for country and country grouping
luPaper <- dat2[, !names(dat2) %in% c("Saved.File.Name", "Found", "Simplified.comments")]
luPaper <- luPaper[!is.na(luPaper$paper), ]
luPaper$Grouping <- as.character(luPaper$Grouping)
luPaper$Grouping[luPaper$paper == "California_MPAs.pdf"] <- "California_MPAN"
luPaper <- luPaper[!duplicated(luPaper), ]
length(unique(luPaper$paper)) # 663 unique papers

parkPaperSummary <- plyr::ddply(luPaper, c("paper"), plyr::summarize, nParks = length(unique(PA.Name)))
length(unique(parkPaperSummary$paper)) # 663 papers
nrow(parkPaperSummary[parkPaperSummary$nParks > 1, ]) # 86 papers refer to more than 1 park

paperParkSummary <- plyr::ddply(luPaper, c("PA.Name"), plyr::summarize, nPapers = length(unique(paper)))
length(unique(paperParkSummary$PA.Name)) # 935 parks
nrow(paperParkSummary[paperParkSummary$nPapers > 1, ]) # 9 parks have more than 1 paper

parkPaperByCountry <- plyr::ddply(luPaper, c("Country"), plyr::summarize, nParks = length(unique(PA.Name)), nPapers = length(unique(paper)))
parkPaperByGrouping <- plyr::ddply(luPaper, c("Grouping"), plyr::summarize, nParks = length(unique(PA.Name)), nPapers = length(unique(paper)))

# Get list of PDFs and read them in
dir <- "ManagementPlans_R/"
# list.of.pdfs<-paste0(dir,list.files(pattern="*.pdf$", recursive = TRUE))
list.of.pdfs <- list.files(dir, pattern = "*.pdf$", recursive = TRUE, full.names = TRUE)

# Compare spreadspeet to files
inXLS_butMissing <- unique(dat2$paper)[!unique(dat2$paper) %in% gsub(dir, "", list.of.pdfs)]
inXLS_butMissing <- inXLS_butMissing[order(inXLS_butMissing)]

haveFile_butNotInXLS <- gsub(dir, "", list.of.pdfs)[!gsub(dir, "", list.of.pdfs) %in% unique(dat2$paper)]
haveFile_butNotInXLS <- haveFile_butNotInXLS[order(haveFile_butNotInXLS)]

# look at these - make sure all the papers you want to be searching are in the list/folder
inXLS_butMissing
haveFile_butNotInXLS

saveRDS(list.of.pdfs, file = "data-generated/list-of-pdfs.rds")
