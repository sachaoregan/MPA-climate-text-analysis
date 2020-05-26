library(ggplot2)
library(ggsidekick)
library(dplyr)

pdf_data_pull <- readr::read_csv("manual_pdf_data_pull.csv")
metadata <- readr::read_csv("mpa_metadata.csv")
metadata <- metadata %>% rename(report = paper)

pub_years <- readRDS("data-generated/MPAplan_pub_year.rds")
total_words <- readRDS(file = "data-generated/total_words.rds")

pdf_data_pull <- left_join(pdf_data_pull, metadata, by = "report")
pdf_data_pull <- left_join(pdf_data_pull, pub_years, by = "report")
pdf_data_pull <- left_join(pdf_data_pull, total_words, by = "report")

pdf_data_pull$Grouping <-  recode(pdf_data_pull$Grouping, California_MPAN = "USA", ABNJ = "Antarctica")

pdf_data_pull <- select(pdf_data_pull, -X1) %>%
  filter(disc_of_climate_effects_on_park_ecological != "NA")
