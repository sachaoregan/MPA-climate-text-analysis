library(ggplot2)
library(ggsidekick)
library(dplyr)

climate_change_terms <- readRDS("data-generated/climate-search-results.rds")
pub_years <- readRDS("data-generated/MPAplan_pub_year.rds")
total_words <- readRDS(file = "data-generated/total_words.rds")

climate_change_terms <- left_join(climate_change_terms, pub_years, by = "report")
climate_change_terms <- left_join(climate_change_terms, total_words, by = "report")

climate_change_terms$Grouping <-  recode(climate_change_terms$Grouping, California_MPAN = "USA")
write.csv(climate_change_terms, file = "climate_change_terms_w_pub_years_rpt.csv")

climate_change_terms$binned_year <-
  seq(1970, 2020, 5)[findInterval(as.numeric(climate_change_terms$first_yr),
  vec = seq(1970, 2020, 5))]

climate_change_terms <- climate_change_terms %>%
  mutate(binned_year_chr = paste0(binned_year, "-", binned_year + 5))

climate_terms_region <- climate_change_terms %>%
  group_by(Grouping, term) %>%
  summarise(proportion = mean(count > 0)) %>%
  filter(term!= "NA") %>%
  mutate(term = stringr::str_to_sentence(term))

ggplot(climate_terms_region, aes(x = forcats::fct_reorder(term, proportion), y = proportion, fill = Grouping)) +
  geom_col(position = position_dodge()) +
  theme_sleek() +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Proportion of MPAs") + coord_flip(expand = FALSE) +
  scale_fill_brewer(palette = "Set3")

ggsave("climate_by_region_v1.png", width = 5, height = 9)

ggplot(climate_terms_region, aes(x = forcats::fct_reorder(term, proportion), y = proportion, fill = Grouping)) +
  geom_col() +
  facet_wrap(~Grouping, nrow = 2) +
  theme_sleek() +
  theme(legend.position = "none", panel.spacing.x = unit(10, "pt"), plot.margin = margin(11/2, 11/2+5, 11/2, 11/2)) +
  scale_y_continuous(breaks = c(0, 0.25, .50, 0.75, 1), labels = c("0", "0.25", "0.50", "0.75", "1.0")) +
  labs(x = "", y = "Proportion of MPAs") + coord_flip(expand = FALSE) +
  scale_fill_brewer(palette = "Set3")

ggsave("climate_by_region_v2.png", width = 8, height = 3)

climate_change_terms_yr <- climate_change_terms %>%
  filter(first_yr != "NA") %>%
  group_by(report) %>%
  mutate(count = ifelse(!is.na(count), count, 0)) %>%
  mutate(prop = count/total_words * 10000)

ggplot(climate_change_terms_yr, aes(x = as.numeric(first_yr), y = prop, colour = Grouping)) +
  geom_point(alpha = 0.7) +
 # facet_wrap(~Grouping, nrow = 2) +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "MPA plan publication year", y = "Proportion (term count/10,000 words in MPA plan)") +
  scale_colour_brewer(palette = "Set3")

ggsave("climate_terms_time.png", width = 8, height = 4)

climate_change_terms_yr_bygroup <- climate_change_terms %>% group_by(Grouping, first_yr, term) %>%
  summarize(tot_count = sum(count), tot_words = sum(total_words), prop = tot_count/tot_words * 10000)

ggplot(climate_change_terms_yr_bygroup, aes(x = as.numeric(first_yr), y = prop, colour = Grouping)) +
  geom_line(aes(colour = Grouping)) +
  facet_wrap(~term, nrow = 2, scales = "free_y") +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "MPA plan publication year", y = "Proportion (term count/10,000 words in MPA plan)") +
  scale_colour_brewer(palette = "Set3")

ggsave("climate_terms_time_bygroup.png", width = 8, height = 4)

ggplot(climate_change_terms_yr_bygroup, aes(x = as.numeric(first_yr), y = prop)) +
  geom_line() +
  facet_grid(Grouping~term, scales = "free_y") +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "MPA plan publication year", y = "Proportion (term count/10,000 words in MPA plan)") +
  scale_colour_brewer(palette = "Set3")

ggsave("climate_terms_time_bygroup_v2.png", width = 10, height = 10)

ggplot(climate_change_terms_yr_bygroup, aes(x = as.numeric(first_yr), y = prop)) +
  geom_line() +
  facet_grid(term~Grouping, scales = "free_y") +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "MPA plan publication year", y = "Proportion (term count/10,000 words in MPA plan)") +
  scale_colour_brewer(palette = "Set3")

ggsave("climate_terms_time_bygroup_v3.png", width = 15, height = 10)

mpa_total_words <- climate_change_terms %>% group_by(Grouping, first_yr, report) %>%
  summarise(tot_words = unique(total_words)) %>%
  group_by(Grouping, first_yr) %>%
  summarise(tot_words = sum(tot_words))

ggplot(mpa_total_words, aes(x = as.numeric(first_yr), y = prop)) +
  geom_line() +
  facet_grid(term~Grouping, scales = "free_y") +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "MPA plan publication year", y = "Proportion (term count/10,000 words in MPA plan)") +
  scale_colour_brewer(palette = "Set3")

ggsave("mpa_total_words.png", width = 15, height = 10)


