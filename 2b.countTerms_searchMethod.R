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

# out <- left_join(out, total_words, by = "report")

components <- components %>% rename(term = component)
out <- left_join(components, terms) %>%
  filter(report != "NA") %>%
  left_join(total_words) %>%
  mutate(count = ifelse(!is.na(count), count, 0)) %>%
  select(-term) %>%
  mutate(prop = count/total_words * 1)

write.csv(out, file = "components_rpt.csv")
saveRDS(out, file = "data-generated/component-search-results.rds")

components_w_meta <- left_join(out, metadata)
write.csv(components_w_meta, file = "components_w_meta_rpt.csv")

library(ggplot2)
library(ggsidekick)

tot_term_count <- components_w_meta %>%
  mutate(root_word = stringr::str_to_sentence(root_word),
  dimension = stringr::str_to_sentence(dimension)) %>%
  group_by(dimension, root_word) %>%
  summarise(
    tot.count = sum(count),
    mean.prop = mean(prop)
    # lwr.prop = tryCatch(plogis(confint(glm(prop~1, family = binomial()))[[1]]), error = function(e) 0),
    # upr.prop = tryCatch(plogis(confint(glm(prop~1, family = binomial()))[[2]]), error = function(e) 0)
  )

# term_prop <- components_w_meta %>% mutate(root_word = stringr::str_to_sentence(root_word), dimension = stringr::str_to_sentence(dimension)) %>%
#   group_by(dimension, root_word) %>%
#   summarise(mean.prop = mean(prop))

# ggplot(filter(tot_term_count, !is.na(root_word)), aes(x = forcats::fct_reorder(root_word, tot.count), y = tot.count, fill = dimension)) +
#   geom_col() +
#   facet_wrap(~dimension, ncol = 1, scales = "free") +
#   scale_fill_manual(values = c("#31a354", "#8856a7", "#fd8d3c")) +
#   theme_sleek() +
#   theme(legend.position = "none") +
#   labs(x = "", y = "Count") + coord_flip(expand = FALSE)

ggplot(filter(tot_term_count, !is.na(root_word)),
  aes(x = forcats::fct_reorder(root_word, tot.count), y = tot.count)) +
  geom_segment( aes(xend = root_word, yend = 0)) +
  geom_point(shape = 21, size = 2, colour = "black", aes(fill = dimension)) +
  facet_wrap(~dimension, ncol = 1, scales = "free") +
  scale_colour_manual(values = c("#31a354", "#8856a7", "#fd8d3c")) +
  theme_sleek() +
  theme(legend.position = "none") +
  labs(x = "", y = "Count") + coord_flip(expand = FALSE)

ggsave("comps_v2.png", width = 5, height = 9)

ggplot(filter(tot_term_count, !is.na(root_word)),
  aes(x = forcats::fct_reorder(root_word, mean.prop), y = mean.prop)) +
  geom_segment(aes(xend = root_word, yend = 0)) +
  geom_point(shape = 21, size = 2, colour = "black", aes(fill = dimension)) +
  facet_wrap(~dimension, ncol = 1, scales = "free") +
  scale_colour_manual(values = c("#31a354", "#8856a7", "#fd8d3c")) +
  theme_sleek() +
  theme(legend.position = "none") +
  scale_y_discrete(expand = c(0, 0, 0.03, 0)) +
  scale_x_discrete(expand = c(0.04, 0, 0.04, 0)) +
  labs(x = "", y = "Proportion") +
  coord_flip()

# plot_terms <- function(dat, title) {
#   ggplot(dat, aes(x = forcats::fct_reorder(root_word, mean.prop), y = mean.prop)) +
#   geom_segment( aes(xend = root_word, yend = 0)) +
#   geom_point(shape = 21, size = 2, colour = "black") +
#   scale_colour_manual(values = c("#31a354", "#8856a7", "#fd8d3c")) +
#   theme_sleek() +
#     ggtitle(title) +
#   theme(legend.position = "none") +
#   labs(x = "", y = "Proportion") + coord_flip(expand = FALSE)
# }
# g <- group_by(term_prop, dimension) %>%
#   group_split() %>%
#   purrr::map(plot_terms, title = "dsa")
# cowplot::plot_grid(plotlist = g)

ggsave("comps_v3.png", width = 5, height = 9)

components_w_meta$Grouping <-  recode(components_w_meta$Grouping, California_MPAN = "USA")

components_by_region <- components_w_meta %>%
  mutate(root_word = stringr::str_to_sentence(root_word), dimension = stringr::str_to_sentence(dimension)) %>%
  group_by(Grouping, dimension, root_word) %>%
  summarise(proportion = mean(count > 0))
  #filter(term!= "NA")

ggplot(components_by_region, aes(x = forcats::fct_reorder(root_word, proportion), y = proportion, fill = Grouping)) +
  geom_col() +
  facet_grid(dimension~Grouping, scales = "free") +
  theme_sleek() +
  theme(legend.position = "none", panel.spacing.x = unit(10, "pt"), plot.margin = margin(11/2, 11/2+5, 11/2, 11/2)) +
  scale_y_continuous(breaks = c(0, 0.25, .50, 0.75, 1), labels = c("0", "0.25", "0.50", "0.75", "1.0")) +
  labs(x = "", y = "Proportion of MPAs") + coord_flip(expand = FALSE) +
  scale_fill_brewer(palette = "Set3")

ggsave("comps_by_region.png", width = 14, height = 12)

term_prop2 <- components_w_meta %>% mutate(root_word = stringr::str_to_sentence(root_word), dimension = stringr::str_to_sentence(dimension)) %>%
  group_by(dimension, root_word)

ggplot(filter(term_prop2, !is.na(root_word)), aes(x = forcats::fct_reorder(root_word, prop), y = prop)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~dimension, ncol = 1, scales = "free") +
  scale_colour_manual(values = c("#31a354", "#8856a7", "#fd8d3c")) +
  theme_sleek() +
  theme(legend.position = "none") +
  labs(x = "", y = "Proportion") + coord_flip(expand = FALSE, ylim = c(0, 10))

ggplot(filter(term_prop2, !is.na(root_word)), aes(x = forcats::fct_reorder(root_word, prop), y = prop)) +
  geom_jitter(height = 0, width = 0.1, alpha = 0.01) +
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


