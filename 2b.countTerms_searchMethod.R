# library(future)
library(dplyr)
library(purrr)
library(future)
plan(multisession)
dir.create("data-generated/pdftools", showWarnings = FALSE)
outdir <- "data-generated"
dir <- "ManagementPlans_R"

list.of.pdfs <- readRDS("data-generated/list-of-pdfs.rds")

parse_pdfs <- function(.x) {
  cat(.x, "\n")
  tryCatch(
    {
      f <- paste0(file.path(
        outdir, "pdftools",
        gsub("\\.pdf", "", gsub(dir, "", .x))
      ), ".rds")
      if (!file.exists(f)) {
        x <- pdftools::pdf_text(file.path(dir, .x))
        saveRDS(x, file = f)
      } else {
        x <- readRDS(f)
      }
    },
    error = function(e) NA
  )
  x
}

my_corpus <- gsub(paste0(dir, "/"), "", list.of.pdfs) %>%
  set_names() %>%
  map(parse_pdfs)

get_total_words <- function(.x) {
  .x <- unlist(.x)
  .x <- paste(.x, collapse = " ")
  .x <- gsub("\\\n", " ", .x)
  .x <- gsub("\\s+", " ", stringr::str_trim(.x))
  total <- stringr::str_count(.x, "\\w+")
  tibble(total_words = total)
}
total_words <- furrr::future_map_dfr(my_corpus, get_total_words,
  .id = "report", .progress = TRUE)

get_count <- function(.x, .s) {
  .x <- unlist(.x)
  .x <- paste(.x, collapse = " ")
  .x <- gsub("\\\n", " ", .x)
  .x <- gsub("\\s+", " ", stringr::str_trim(.x))
  term_count <- stringi::stri_count_fixed(.x, .s,
    opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE))
  tibble(term = .s, count = term_count)
}

vec <- c("climate change", "global warming", "extreme events",
  "natural variability", "climate variability")

out <- furrr::future_map_dfr(my_corpus, get_count, .s = vec,
  .id = "report", .progress = TRUE)

metadata <- readr::read_csv("mpa_metadata.csv")
metadata <- metadata %>% rename(report = paper)

climate_terms_w_meta <- left_join(metadata, out, by = "report")
write.csv(climate_terms_w_meta, file = "climate_terms_w_meta_rpt.csv")

tokeep <- group_by(out, report) %>%
  summarise(keep_this = sum(count) > 0)

mean(tokeep$keep_this)
corpus_selected <- keep(my_corpus, tokeep$keep_this)

characters <- purrr::map_dbl(my_corpus, ~ sum(stringr::str_count(.x)))
characters

components <- readr::read_csv("SearchTerms/searchcomponents.csv")
vec <- components$component
vec[vec == "pH"] <- "pH "

out <- furrr::future_map_dfr(corpus_selected, get_count, .s = vec,
  .id = "report", .progress = TRUE)

out <- left_join(out, total_words, by = "report")

components <- components %>% rename(term = component)
out <- left_join(components, out, by = "term") %>%
  select(-term) %>%
  mutate(prop = count/total_words)

write.csv(out, file = "components_rpt.csv")
saveRDS(out, file = "data-generated/search-results.rds")

components_w_meta <- left_join(metadata, out, by = "report")
write.csv(components_w_meta, file = "components_w_meta_rpt.csv")

library(ggplot2)
library(ggsidekick)

tot_term_count <- components_w_meta %>% mutate(root_word = stringr::str_to_sentence(root_word), dimension = stringr::str_to_sentence(dimension)) %>%
  group_by(dimension, root_word) %>%
  summarise(tot.count = sum(count))

term_prop <- components_w_meta %>% mutate(root_word = stringr::str_to_sentence(root_word), dimension = stringr::str_to_sentence(dimension)) %>%
  group_by(dimension, root_word) %>%
  summarise(mean.prop = mean(prop))

# ggplot(filter(tot_term_count, !is.na(root_word)), aes(x = forcats::fct_reorder(root_word, tot.count), y = tot.count, fill = dimension)) +
#   geom_col() +
#   facet_wrap(~dimension, ncol = 1, scales = "free") +
#   scale_fill_manual(values = c("#31a354", "#8856a7", "#fd8d3c")) +
#   theme_sleek() +
#   theme(legend.position = "none") +
#   labs(x = "", y = "Count") + coord_flip(expand = FALSE)

ggplot(filter(tot_term_count, !is.na(root_word)), aes(x = forcats::fct_reorder(root_word, tot.count), y = tot.count)) +
  geom_segment( aes(xend = root_word, yend = 0)) +
  geom_point(shape = 21, size = 2, colour = "black", aes(fill = dimension)) +
  facet_wrap(~dimension, ncol = 1, scales = "free") +
  scale_colour_manual(values = c("#31a354", "#8856a7", "#fd8d3c")) +
  theme_sleek() +
  theme(legend.position = "none") +
  labs(x = "", y = "Count") + coord_flip(expand = FALSE)

ggsave("comps_v2.png", width = 5, height = 9)

ggplot(filter(term_prop, !is.na(root_word)), aes(x = forcats::fct_reorder(root_word, mean.prop), y = mean.prop)) +
  geom_segment( aes(xend = root_word, yend = 0)) +
  geom_point(shape = 21, size = 2, colour = "black", aes(fill = dimension)) +
  facet_wrap(~dimension, ncol = 1, scales = "free") +
  scale_colour_manual(values = c("#31a354", "#8856a7", "#fd8d3c")) +
  theme_sleek() +
  theme(legend.position = "none") +
  labs(x = "", y = "Proportion") + coord_flip(expand = FALSE)

ggsave("comps_v3.png", width = 5, height = 9)

term_prop2 <- components_w_meta %>% mutate(root_word = stringr::str_to_sentence(root_word), dimension = stringr::str_to_sentence(dimension)) %>%
  group_by(dimension, root_word)

ggplot(filter(term_prop2, !is.na(root_word)), aes(x = forcats::fct_reorder(root_word, prop), y = prop)) +
  geom_violin() + geom_jitter(height = 0, width = 0.1, alpha = 0.1) +
  facet_wrap(~dimension, ncol = 1, scales = "free") +
  scale_colour_manual(values = c("#31a354", "#8856a7", "#fd8d3c")) +
  theme_sleek() +
  theme(legend.position = "none") +
  labs(x = "", y = "Proportion") + coord_flip(expand = FALSE)

ggsave("comps_v4.png", width = 5, height = 9)

ggplot(tot_term_count, aes(x = term, y = tot.count)) +
  geom_bar(stat = "identity", x = forcats::fct_infreq(term)) +
  theme_sleek() +
  labs(x = "Term", y = "Count") +
  theme(axis.text.x = element_text(angle = 90))

tot_by_grouping<- components_w_meta %>% group_by(Grouping, term) %>%
  summarise(tot.count = sum(count))

ggplot(tot_term_count, aes(x = term, y = tot.count)) +
  geom_bar(stat = "identity", x = forcats::fct_infreq(term)) +
  theme_sleek() +
  labs(x = "Term", y = "Count") +
  theme(axis.text.x = element_text(angle = 90))

# future::plan(future::sequential)

# # ----------- SEARCH method --------- #
# start_time_search<-Sys.time()
# # sink(paste0(outdir, "/SEARCH_readPDFs_", Sys.Date(),".txt")) #this logs my "this is a bad pdf" message but not what the error is. Also supresses the progress bar in R.
#
# tab<-data.frame(paper=NA, keyword=NA, pagenum=NA)
# errors<-data.frame(paper=NA)
# warnings<-data.frame(paper=NA)
#
# for (i in 1:length(list.of.pdfs)){
#   tryCatch(
#     {
#       message(cat("\n running doc ", i, " of ", length(list.of.pdfs), " ..... ^", gsub(dir, "",list.of.pdfs[i], "^")))
#       result<-FALSE
#       f <- paste0(file.path(outdir, "pdftools",
#         gsub("\\.pdf", "", gsub(dir, "", list.of.pdfs[i]))), ".rds")
#       if(!file.exists(f)) {
#         x <- pdftools::pdf_text(list.of.pdfs[i])
#         saveRDS(x, file = f)
#       } else {
#         x <- readRDS(f)
#       }
#       result <- keyword_search(x, keyword = allTerms, surround_lines = 0)
#     },
#     error=function(error_message){
#       message(cat(paste0(gsub(dir, "^",list.of.pdfs[i])), "^ is a bad pdf - error"))
#       message(error_message)
#       return(NA)
#     },
#     warning=function(warning_message){
#       message(cat(paste0(gsub(dir, "^",list.of.pdfs[i])), "^ is a bad pdf - warning"))
#       message(warning_message)
#       return(NA)
#     }
#   )
#
#   if (!isFALSE(result)){if(nrow(result)>0){
#     tab<-rbind.data.frame(tab,cbind.data.frame(paper=gsub(dir, "",list.of.pdfs[i]), keyword=result$keyword, pagenum=result$page_num))
#   } else {tab<-rbind.data.frame(tab, cbind.data.frame(paper=gsub(dir, "",list.of.pdfs[i]), keyword=NA, pagenum=NA))}
#   }
# }
#
# sink()
# sink()
#
# #Process log to get good and bad papers
# # log<-read.delim(paste0(outdir, "/SEARCH_readPDFs_", Sys.Date(),".txt"), sep="^", header=F)
# # names(log)<-c("paper", "error")
# # log$paper<-gsub("^\\s|\\s$", "", log$paper)
# # log$error<-gsub("^\\s|\\s$", "", log$error)
# #
# # papers.loaded<-log[log$error!="is a bad pdf - warning",2]
#
# # errors<-log[log$error=="is a bad pdf - warning",1]
# # noerrors<-papers.loaded[!papers.loaded %in% errors]
#
# # length(papers.loaded) #648
# # length(errors)      #48
# # length(noerrors)    #600
#
# # write.csv(c(paste0("of ",  length(papers.loaded), " papers loaded, ", length(errors), " had errors and did not read, and ", length(noerrors), " were successfully read"),paste0("the files that did not read are listed below"), errors), paste0(outdir, "/SEARCH_PDF_errorSummary_", Sys.Date(),".csv"), row.names=F)
#
# #
# # write.csv(merge(data.frame(paper=errors), luPaper, by="paper", all.x=T, all.y=F), paste0(outdir, "/SEARCH_PDF_errorSummary_wPAName_", Sys.Date(),".csv"), row.names=F)
#
# names(tab)[names(tab)=="keyword"]<-"term"
#
# #out the bat, this does not return paper-term combos that don't exist. So we do want to fill those in
# #results by keyword with page number
# tabx<-tab[!is.na(tab$paper),]
# length(unique(tabx$paper)) #614 papers were parsed
#
# ### need matrix to include papers with 0 terms
# # make matrix of terms by paper (fills back in blanks)
# tabx2<-melt(tabx, id.vars=c("paper", "term"))
# tabx2$value[!is.na(tabx2$value)]<-1 #set page numbers to "1" so sum is a count
#
# termMatrixInt_search<-reshape::cast(tabx2, paper~term, fun="sum") #this is the count of each term in each paper
# termMatrixInt_search<-termMatrixInt_search[,!names(termMatrixInt_search) %in% c("NA", "V1")]
#
# head(termMatrixInt_search)
# nrow(termMatrixInt_search) #600
# nrow(new) #560
#
# # add attributes for each paper (park name, grouping, etc.)
# termMatrixInt_search<-merge(termMatrixInt_search, luPaper, by="paper", all.x=T)
#
# length(unique(termMatrixInt_search$PA.Name)) #863 parks
# length(unique(termMatrixInt_search$paper)) #600 papers. This matches how many papers were read w/o errors
# length(noerrors)    #600
#
# # add total number of words for each paper - this is the only output from script 2a used here. Remove if you don't want it.
# nWordsPerPaper<-read.csv(paste0(outdir, "/FULL_nWordsPerPaper.csv"))
# termMatrixInt_search<-merge(termMatrixInt_search, nWordsPerPaper, by="paper")
# head(termMatrixInt_search)
#
# termMatrixInt_search4<-rbind(data.table(termMatrixInt_search), data.table(merge(data.frame(paper=errors), luPaper, by="paper", all.x=T, all.y=F)),  fill=T)
# head(termMatrixInt_search4)
#
# #clean up and write results
# nameOrder<-c("paper","WDPAID","PA.Name","NAME","DESIG","Country","Grouping","nWords")
# nameOrder2<-c(names(termMatrixInt_search4)[!names(termMatrixInt_search4) %in% nameOrder])
# termMatrixInt_search5<-termMatrixInt_search4[,c(nameOrder, nameOrder2), with=FALSE]
#
# # note that if there are PDFs that correspond to multiple protected areas, the results WITH lookup will have duplicate lines.
# # write both lookup (duplicate lines for pdfs, but unique lines per protected area) and no lookup (1 line per pdf) versions ofthe results.
# write.csv(termMatrixInt_search,paste0(outdir, "/SEARCH_countTermOcurrencePerPaperNoLookup_", Sys.Date(),".csv"), row.names=F) #results with no lookup
# write.csv(termMatrixInt_search5, paste0(outdir, "/SEARCH_countTermOcurrencePerPaper_", Sys.Date(),".csv"), row.names=F) #results with lookup
#
# #write output
# saveRDS(termMatrixInt_search, paste0(outdir, "/SEARCH_outputMatrix.rds"))
#
# ## -- end -- ##
# end_time_search<-Sys.time()
# cat("search method took",end_time_search-start_time_search, " minutes") #12 mins
