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

clean_text <- function(.x) {
  .x <- unlist(.x)
  .x <- paste(.x, collapse = " ")
  .x <- gsub("\\\n", " ", .x)
  gsub("\\s+", " ", tolower(stringr::str_trim(.x)))
}

my_corpus <- my_corpus %>% map(clean_text)

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

saveRDS(first_yrs, file = "data-generated/MPAplan_pub_year.rds")

set.seed(42)
sampled_reports <- sample_frac(first_yrs, 0.05)
readr::write_csv(sampled_reports, path = "sampled-reports.csv")

get_total_words <- function(.x) {
  total <- stringr::str_count(.x, "\\w+")
  tibble(total_words = total)
}
total_words <- furrr::future_map_dfr(my_corpus, get_total_words,
  .id = "report", .progress = TRUE)

saveRDS(total_words, file = "data-generated/total_words.rds")

get_count <- function(.x, .s) {
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
saveRDS(climate_terms_w_meta, file = "data-generated/climate-search-results.rds")

tokeep <- group_by(out, report) %>%
  summarise(keep_this = sum(count) > 0)

mean(tokeep$keep_this)
corpus_selected <- keep(my_corpus, tokeep$keep_this)

characters <- purrr::map_dbl(my_corpus, ~ sum(stringr::str_count(.x)))
characters

components <- readr::read_csv("SearchTerms/searchcomponents.csv")
vec <- components$component
vec[vec == "pH"] <- "pH "

terms <- furrr::future_map_dfr(corpus_selected, get_count, .s = vec,
  .id = "report", .progress = TRUE)

components <- components %>% rename(term = component)
out <- left_join(components, terms) %>%
  filter(report != "NA") %>%
  left_join(total_words) %>%
  mutate(count = ifelse(!is.na(count), count, 0)) %>%
  select(-term) %>%
  mutate(prop = count/total_words * 10000)

components_w_meta <- left_join(out, metadata)

write.csv(components_w_meta, file = "components_w_meta_rpt.csv")
saveRDS(components_w_meta, file = "data-generated/component-search-results-w-meta-rpt.rds")

scienceterms <- readr::read_csv("SearchTerms/search_scienceterms.csv")
vec <- scienceterms$scienceterm

out <- furrr::future_map_dfr(corpus_selected, get_count, .s = vec,
  .id = "report", .progress = TRUE)

scienceterms_w_meta <- left_join(out, total_words) %>%
  left_join(metadata) %>%
  mutate(prop = count/total_words * 10000)

write.csv(scienceterms_w_meta, file = "scienceterms_w_meta_rpt.csv")
saveRDS(scienceterms_w_meta, file = "data-generated/scienceterms-search-results-w-meta-rpt.rds")


