# -------------------------------------------------------------------- #
# Text Analysis - Marine Protected Area Management Plan Review
#
# Script 2a: "Search method" text search
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

# 1.countTerms_setup.R must have been run first.
# 2a.countTerms_fullMethod.R must have been run first if you want the number of words overall in each pdf. If you don't want this, you can remove line 104 in this script, and ignore script 2a.


dir.create("data-generated/pdftools", showWarnings = FALSE)

parse_pdfs <- function(.x) {
  cat(.x, "\n")
  tryCatch({
    f <- paste0(file.path(outdir, "pdftools",
      gsub("\\.pdf", "", gsub(dir, "", .x))), ".rds")
    if(!file.exists(f)) {
      x <- pdftools::pdf_text(.x)
      saveRDS(x, file = f)
    } else {
      x <- readRDS(f)
    }
  }, error = function(e) NA)
  x
}
my_corpus <- purrr::map(list.of.pdfs, parse_pdfs)

check_contains <- function(.x, vec = "test") {
  out <- tryCatch({
    result <- keyword_search(.x, keyword = vec, surround_lines = 0)
    nrow(result) > 0
  }, error = function() NA)
  out
}

library(future)
plan(multisession, workers = future::availableCores() / 2)

# includes_i <- purrr::map_lgl(my_corpus, check_contains,
#   vec = c('climate change', 'global warming'))
includes_i <- furrr::future_map_lgl(my_corpus, check_contains,
  vec = c('climate change', 'global warming'))

characters <- purrr::map_dbl(my_corpus, ~ sum(str_count(.x)))

# ----------- SEARCH method --------- #
start_time_search<-Sys.time()
#Process Files
sink(paste0(outdir, "/SEARCH_readPDFs_", Sys.Date(),".txt")) #this logs my "this is a bad pdf" message but not what the error is. Also supresses the progress bar in R.

tab<-data.frame(paper=NA, keyword=NA, pagenum=NA)
errors<-data.frame(paper=NA)
warnings<-data.frame(paper=NA)

for (i in 1:length(list.of.pdfs)){
  tryCatch(
    {
      message(cat("\n running doc ", i, " of ", length(list.of.pdfs), " ..... ^", gsub(dir, "",list.of.pdfs[i], "^")))
      result<-FALSE
      f <- paste0(file.path(outdir, "pdftools",
        gsub("\\.pdf", "", gsub(dir, "", list.of.pdfs[i]))), ".rds")
      if(!file.exists(f)) {
        x <- pdftools::pdf_text(list.of.pdfs[i])
        saveRDS(x, file = f)
      } else {
        x <- readRDS(f)
      }
      result <- keyword_search(x, keyword = allTerms, surround_lines = 0)
    },
    error=function(error_message){
      message(cat(paste0(gsub(dir, "^",list.of.pdfs[i])), "^ is a bad pdf - error"))
      message(error_message)
      return(NA)
    },
    warning=function(warning_message){
      message(cat(paste0(gsub(dir, "^",list.of.pdfs[i])), "^ is a bad pdf - warning"))
      message(warning_message)
      return(NA)
    }
  )

  if (!isFALSE(result)){if(nrow(result)>0){
    tab<-rbind.data.frame(tab,cbind.data.frame(paper=gsub(dir, "",list.of.pdfs[i]), keyword=result$keyword, pagenum=result$page_num))
  } else {tab<-rbind.data.frame(tab, cbind.data.frame(paper=gsub(dir, "",list.of.pdfs[i]), keyword=NA, pagenum=NA))}
  }
}

sink()
sink()

#Process log to get good and bad papers
log<-read.delim(paste0(outdir, "/SEARCH_readPDFs_", Sys.Date(),".txt"), sep="^", header=F)
names(log)<-c("paper", "error")
log$paper<-gsub("^\\s|\\s$", "", log$paper)
log$error<-gsub("^\\s|\\s$", "", log$error)

papers.loaded<-log[log$error!="is a bad pdf - warning",2]

errors<-log[log$error=="is a bad pdf - warning",1]
noerrors<-papers.loaded[!papers.loaded %in% errors]

length(papers.loaded) #648
length(errors)      #48
length(noerrors)    #600

write.csv(c(paste0("of ",  length(papers.loaded), " papers loaded, ", length(errors), " had errors and did not read, and ", length(noerrors), " were successfully read"),paste0("the files that did not read are listed below"), errors), paste0(outdir, "/SEARCH_PDF_errorSummary_", Sys.Date(),".csv"), row.names=F)

#
write.csv(merge(data.frame(paper=errors), luPaper, by="paper", all.x=T, all.y=F), paste0(outdir, "/SEARCH_PDF_errorSummary_wPAName_", Sys.Date(),".csv"), row.names=F)

names(tab)[names(tab)=="keyword"]<-"term"

#out the bat, this does not return paper-term combos that don't exist. So we do want to fill those in
#results by keyword with page number
tabx<-tab[!is.na(tab$paper),]
length(unique(tabx$paper)) #614 papers were parsed

### need matrix to include papers with 0 terms
# make matrix of terms by paper (fills back in blanks)
tabx2<-melt(tabx, id.vars=c("paper", "term"))
tabx2$value[!is.na(tabx2$value)]<-1 #set page numbers to "1" so sum is a count

termMatrixInt_search<-reshape::cast(tabx2, paper~term, fun="sum") #this is the count of each term in each paper
termMatrixInt_search<-termMatrixInt_search[,!names(termMatrixInt_search) %in% c("NA", "V1")]

head(termMatrixInt_search)
nrow(termMatrixInt_search) #600
nrow(new) #560

# add attributes for each paper (park name, grouping, etc.)
termMatrixInt_search<-merge(termMatrixInt_search, luPaper, by="paper", all.x=T)

length(unique(termMatrixInt_search$PA.Name)) #863 parks
length(unique(termMatrixInt_search$paper)) #600 papers. This matches how many papers were read w/o errors
length(noerrors)    #600

# add total number of words for each paper - this is the only output from script 2a used here. Remove if you don't want it.
nWordsPerPaper<-read.csv(paste0(outdir, "/FULL_nWordsPerPaper.csv"))
termMatrixInt_search<-merge(termMatrixInt_search, nWordsPerPaper, by="paper")
head(termMatrixInt_search)

termMatrixInt_search4<-rbind(data.table(termMatrixInt_search), data.table(merge(data.frame(paper=errors), luPaper, by="paper", all.x=T, all.y=F)),  fill=T)
head(termMatrixInt_search4)

#clean up and write results
nameOrder<-c("paper","WDPAID","PA.Name","NAME","DESIG","Country","Grouping","nWords")
nameOrder2<-c(names(termMatrixInt_search4)[!names(termMatrixInt_search4) %in% nameOrder])
termMatrixInt_search5<-termMatrixInt_search4[,c(nameOrder, nameOrder2), with=FALSE]

# note that if there are PDFs that correspond to multiple protected areas, the results WITH lookup will have duplicate lines.
# write both lookup (duplicate lines for pdfs, but unique lines per protected area) and no lookup (1 line per pdf) versions ofthe results.
write.csv(termMatrixInt_search,paste0(outdir, "/SEARCH_countTermOcurrencePerPaperNoLookup_", Sys.Date(),".csv"), row.names=F) #results with no lookup
write.csv(termMatrixInt_search5, paste0(outdir, "/SEARCH_countTermOcurrencePerPaper_", Sys.Date(),".csv"), row.names=F) #results with lookup

#write output
saveRDS(termMatrixInt_search, paste0(outdir, "/SEARCH_outputMatrix.rds"))

## -- end -- ##
end_time_search<-Sys.time()
cat("search method took",end_time_search-start_time_search, " minutes") #12 mins
