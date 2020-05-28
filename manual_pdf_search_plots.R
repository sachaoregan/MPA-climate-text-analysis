library(ggplot2)
library(ggsidekick)
library(dplyr)
library(tidyr)

pdf_data_pull <- readr::read_csv("manual_pdf_data_pull.csv")
metadata <- readr::read_csv("mpa_metadata.csv")
metadata <- metadata %>% rename(report = paper)

pub_years <- readRDS("data-generated/MPAplan_pub_year.rds")
total_words <- readRDS(file = "data-generated/total_words.rds")

pdf_data_pull <- left_join(pdf_data_pull, metadata, by = "report")
pdf_data_pull <- left_join(pdf_data_pull, pub_years, by = "report")
pdf_data_pull <- left_join(pdf_data_pull, total_words, by = "report")

pdf_data_pull$Grouping <-  recode(pdf_data_pull$Grouping, California_MPAN = "USA", ABNJ = "Antarctica")

pdf_data_pull <- select(pdf_data_pull, -sort_order, -other_monitoring, -X1) %>%
  filter(disc_of_climate_effects_on_park_ecological != "NA")

pdf_data_pull <- pdf_data_pull %>% pivot_longer(
  cols = disc_of_climate_effects_on_park_ecological:references_other_monitoring,
  names_to = "variable",
  values_to = "value"
)

d <-tribble(~variable, ~dimension,
"disc_of_climate_effects_on_park_ecological", "Review of climate effects on park",
"disc_of_climate_effects_on_park_physical",  "Review of climate effects on park",
"disc_of_climate_effects_on_park_sociological",  "Review of climate effects on park",
"climate_objectives", "Climate objectives and strategies",
"climate_strategies_actions", "Climate objectives and strategies",
"detailed_methods", NA,
"establishes_baseline_conditions", NA,
"indicators", "Monitoring",
"thresholds", "Monitoring",
"metrics",  "Monitoring",
"indicat_thresh_metrics_explicitly_linked_to_climate", "Monitoring",
"commitment_to_climate_monitoring", "Monitoring",
"references_other_monitoring", NA)

pdf_data_pull <- left_join(pdf_data_pull, d, by = "variable")

pdf_data_pull$variable <- recode(pdf_data_pull$variable,
  disc_of_climate_effects_on_park_ecological = "Ecological", disc_of_climate_effects_on_park_physical = "Physical", disc_of_climate_effects_on_park_sociological = "Sociological",
  climate_objectives = "Objectives",
  climate_strategies_actions = "Strategies or actions",
  indicators = "Indicators",
  thresholds = "Thresholds",
  metrics = "Metrics",
  indicat_thresh_metrics_explicitly_linked_to_climate = "Indicators linked to climate change",
  commitment_to_climate_monitoring = "Commitment to climate monitoring")

pdf_data_pull <- mutate(pdf_data_pull, value = stringr::str_to_sentence(value))

pdf_data_by_region <-  pdf_data_pull %>% group_by(Grouping, dimension, variable, value) %>%
  tally() %>%
  group_by(Grouping, variable) %>%
  mutate(proportion = n / sum(n)) %>% ungroup()


mypalette <- c("#fb6a4a", "#41b6c4")
mypalette2 <- c("#bdbdbd", "#fb6a4a", "#a8ddb5", "#41b6c4")


ggplot(filter(pdf_data_by_region, dimension == "Review of climate effects on park"), aes(x = variable, y = proportion, fill = value)) +
  geom_col(position = "stack") +
  facet_wrap(~Grouping, nrow = 2) +
  theme_sleek() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 40, hjust = 1)) +
  labs(x = "", y = "Proportion", title= "Review of climate effects on park") +
  scale_fill_manual(values = mypalette)

ggsave("manual_pdf_search_climate_effects.png", width = 8, height = 4)

ggplot(filter(pdf_data_by_region, dimension == "Climate objectives and strategies"), aes(x = variable, y = proportion, fill = value)) +
  geom_col(position = "stack") +
  facet_wrap(~Grouping,  nrow = 2) +
  theme_sleek() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = "", y = "Proportion", title= "Climate change planning") +
  scale_fill_manual(values = mypalette)

ggsave("manual_pdf_search_climate_planning.png", width = 9, height = 4)

ggplot(filter(pdf_data_by_region, dimension == "Monitoring", variable != "Metrics"), aes(x = variable, y = proportion, fill = value)) +
  geom_col(position = "stack") +
  facet_wrap(~Grouping,  nrow = 2) +
  theme_sleek() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Proportion", title= "Climate monitoring") +
  scale_x_discrete(labels = c("Commitment to\nclimate monitoring", "Indicators", "Indicators linked\n to climate change", "Thresholds")) +
  scale_fill_manual(values = mypalette2)

ggsave("manual_pdf_search_climate_monitoring.png", width = 9, height = 4)
