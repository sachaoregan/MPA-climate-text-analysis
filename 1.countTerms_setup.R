library(purrr)
library(dplyr)

dir.create("data-generated", showWarnings = FALSE)
dir.create("figs", showWarnings = FALSE)

# Load list of reports
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

# Split out lines for parks with multiple papers
dat2 <- tidyr::separate_rows(dat2, paper, sep = ";")
dat2$paper <- gsub("^\\s|\\s$", "", dat2$paper)
dat2$paper <- paste0(gsub(".pdf$", "", dat2$paper), ".pdf")
names(dat2)[names(dat2) == "Parent.Nation"] <- c("Country")
dat2$paper[dat2$paper == "NA.pdf"] <- NA
dat2 <- merge(dat2, luCountryGroup, by = "Country", all.x = T)

length(unique(dat2$PA.Name)) # 1449 parks
length(unique(dat2$paper[!is.na(dat2$paper)])) # 665

# Make lookup table for country and country grouping
luPaper <- dat2[, !names(dat2) %in% c("Saved.File.Name", "Found", "Simplified.comments")]
luPaper <- luPaper[!is.na(luPaper$paper), ]
luPaper$Grouping <- as.character(luPaper$Grouping)
luPaper$Grouping[luPaper$paper == "California_MPAs.pdf"] <- "California_MPAN"
luPaper <- luPaper[!duplicated(luPaper), ]
length(unique(luPaper$paper)) # 665 unique papers

parkPaperSummary <- plyr::ddply(luPaper, c("paper"), plyr::summarize, nParks = length(unique(PA.Name)))
length(unique(parkPaperSummary$paper)) # 665 papers
nrow(parkPaperSummary[parkPaperSummary$nParks > 1, ]) # 86 papers refer to more than 1 park

paperParkSummary <- plyr::ddply(luPaper, c("PA.Name"), plyr::summarize, nPapers = length(unique(paper)))
length(unique(paperParkSummary$PA.Name)) # 936 parks
nrow(paperParkSummary[paperParkSummary$nPapers > 1, ]) # 9 parks have more than 1 paper

parkPaperByCountry <- plyr::ddply(luPaper, c("Country"), plyr::summarize, nParks = length(unique(PA.Name)), nPapers = length(unique(paper)))
parkPaperByGrouping <- plyr::ddply(luPaper, c("Grouping"), plyr::summarize, nParks = length(unique(PA.Name)), nPapers = length(unique(paper)))

# Get list of PDFs and read them in
dir <- "ManagementPlans_R/"
list.of.pdfs <- list.files(dir, pattern = "*.pdf$", recursive = TRUE, full.names = TRUE)

# Compare spreadspeet to files
inXLS_butMissing <- unique(dat2$paper)[!unique(dat2$paper) %in% gsub(dir, "", list.of.pdfs)]
inXLS_butMissing <- inXLS_butMissing[order(inXLS_butMissing)]

haveFile_butNotInXLS <- gsub(dir, "", list.of.pdfs)[!gsub(dir, "", list.of.pdfs) %in% unique(dat2$paper)]
haveFile_butNotInXLS <- haveFile_butNotInXLS[order(haveFile_butNotInXLS)]

# Make sure all the papers you want to be searching are in the list/folder
inXLS_butMissing
haveFile_butNotInXLS

luPaper <- luPaper %>% distinct(paper, Grouping, Country) %>%
  filter(paper != "555536253_Southern_Waters_of_Gibraltar_Management_Scheme_2012.pdf" | Grouping != "UK") %>% #Database had two rows for this PDF, one with incorrectly specified grouping. Removed incorrect row b/c wanted only unique PDFs
  filter(!paper %in% inXLS_butMissing) %>%
  filter(!paper %in% c("101534_BoundaryBay_WMA.pdf", "900736_Elizabeth_and_Middleton_reefs_national_nature_reserve.pdf")) #Excluding 2 PDFs with bad OCR

saveRDS(luPaper, file = "data-generated/mpa-metadata.rds")
#write.csv(luPaper, file = "mpa-metadata.csv") # If you want, not required

todrop <- c("ManagementPlans_R/101534_BoundaryBay_WMA.pdf", "ManagementPlans_R/900736_Elizabeth_and_Middleton_reefs_national_nature_reserve.pdf")
list.of.pdfs <- purrr::map(list.of.pdfs, ~ discard(.x, ~ .x %in% todrop)) %>% purrr::compact() #Excluding 2 PDFs with bad OCR
saveRDS(list.of.pdfs, file = "data-generated/list-of-pdfs.rds")
