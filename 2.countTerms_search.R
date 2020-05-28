library(dplyr)
library(purrr)
library(future)
plan(multisession)
dir.create("data-generated/pdftools", showWarnings = FALSE)
outdir <- "data-generated"
dir <- "ManagementPlans_R"

# Read in list of 650 PDFs and create a clean text corpus

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

clean_text <- function(.x) {
  .x <- unlist(.x)
  .x <- paste(.x, collapse = " ")
  .x <- gsub("\\\n", " ", .x)
  gsub("\\s+", " ", tolower(stringr::str_trim(.x)))
}

my_corpus <- my_corpus %>% map(clean_text)

# Get publication year for all PDFs by searching for 1st 4-number string. Take the 2nd 4-number string to be the publication year if the 1st 4-number string is preceded by the words "act", "regulation", "regulations".

get_years <- function(.x) {
  .x <- stringr::str_sub(.x, 1, 10000)
  yr_pattern <- "\\b19[7-9]+[0-9]+\\b|\\b20[0-2]+[0-9]+\\b"
  .yrs <- stringr::str_extract_all(.x, pattern = yr_pattern)
  pos <- stringr::str_locate_all(.x, pattern = yr_pattern)
  tibble(years = .yrs[[1]], position = pos[[1]][, "end"])
}
yrs <- map_dfr(my_corpus, get_years, .id = "report")

get_years_act <- function(.x) {
  .x <- stringr::str_sub(.x, 1, 10000)
  yr_pattern <- "(act|regulation|regulations) (19[7-9]+[0-9]+\\b|\\b20[0-2]+[0-9]+\\b)"
  .yrs <- stringr::str_extract_all(.x, pattern = yr_pattern)
  pos <- stringr::str_locate_all(.x, pattern = yr_pattern)
  tibble(act_years = .yrs[[1]], position = pos[[1]][, "end"])
}
act_yrs <- map_dfr(my_corpus, get_years_act, .id = "report")

first_yrs <- anti_join(yrs, act_yrs) %>%
  group_by(report) %>%
  summarise(first_yr = years[1])

saveRDS(first_yrs, file = "data-generated/MPAplan-pub-year.rds")

# Get the total number of words in each PDF

get_total_words <- function(.x) {
  total <- stringr::str_count(.x, "\\w+")
  tibble(total_words = total)
}
total_words <- furrr::future_map_dfr(my_corpus, get_total_words,
  .id = "report", .progress = TRUE)

saveRDS(total_words, file = "data-generated/total-words.rds")

# The search term count function

get_count <- function(.x, .s) {
  term_count <- stringi::stri_count_fixed(.x, .s,
    opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE))
  tibble(term = .s, count = term_count)
}

# Search corpus for the climate change terms

vec <- c("climate change", "global warming", "extreme events",
  "natural variability", "climate variability")

out <- furrr::future_map_dfr(my_corpus, get_count, .s = vec,
  .id = "report", .progress = TRUE)

# Join on metadata for each PDF to the results of the climate change term search and create RDS for future plotting and analysis

metadata <- readRDS("data-generated/mpa-metadata.rds")
metadata <- metadata %>% rename(report = paper)

climate_terms_w_meta <- left_join(metadata, out, by = "report")
saveRDS(climate_terms_w_meta, file = "data-generated/climate-search-results.rds")

# Keep only those 289 PDFs that contained at least one of the climate change terms and search these for the climate components

tokeep <- group_by(out, report) %>%
  summarise(keep_this = sum(count) > 0)

mean(tokeep$keep_this)
corpus_selected <- keep(my_corpus, tokeep$keep_this)

components <- readr::read_csv("SearchTerms/searchcomponents.csv")
vec <- components$component
vec[vec == "pH"] <- "pH "

terms <- furrr::future_map_dfr(corpus_selected, get_count, .s = vec,
  .id = "report", .progress = TRUE)

# Join climate component search results with the other columns from the searchcomponents.csv and join column of total words in each PDF. Calculate frequency of climate components per 10,000 words. Join on the metadata for each PDF. Create RDS for future plotting and analysis

components <- components %>% rename(term = component)
out <- left_join(components, terms) %>%
  filter(report != "NA") %>%
  left_join(total_words) %>%
  mutate(count = ifelse(!is.na(count), count, 0)) %>%
  select(-term) %>%
  mutate(freq = count/total_words * 10000)

components_w_meta <- left_join(out, metadata)

saveRDS(components_w_meta, file = "data-generated/component-search-results-w-meta-rpt.rds")

# Search those 289 PDFs that contained at least one of the climate change terms and search these for the science terms

scienceterms <- readr::read_csv("SearchTerms/search-scienceterms.csv")
vec <- scienceterms$scienceterm

out <- furrr::future_map_dfr(corpus_selected, get_count, .s = vec,
  .id = "report", .progress = TRUE)

# Join column of total words in each PDF and the metadata for each PDF onto the science term search results. Calculate frequency of science terms per 10,000 words. Create RDS for future plotting and analysis

scienceterms_w_meta <- left_join(out, total_words) %>%
  left_join(metadata) %>%
  mutate(freq = count/total_words * 10000)

saveRDS(scienceterms_w_meta, file = "data-generated/scienceterms-search-results-w-meta-rpt.rds")


