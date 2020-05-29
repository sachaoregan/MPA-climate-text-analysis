library(ggplot2)
library(ggsidekick)
library(dplyr)

components_w_meta <- readRDS("data-generated/component-search-results-w-meta-rpt.rds")
pub_years <- readRDS("data-generated/MPAplan-pub-year.rds")

components_w_meta <- left_join(components_w_meta, pub_years, by = "report")

components_w_meta <- mutate(components_w_meta, root_word = stringr::str_to_sentence(root_word),  dimension = stringr::str_to_sentence(dimension))

components_w_meta$Grouping <-  recode(components_w_meta$Grouping, California_MPAN = "USA", ABNJ = "Antarctica")
write.csv(components_w_meta, file = "data-generated/components-w-meta-w-pub-years-rpt.csv")

components_w_meta$binned_year <-
  seq(1970, 2020, 5)[findInterval(as.numeric(components_w_meta$first_yr),
    vec = seq(1970, 2020, 5))]

components_w_meta <- components_w_meta %>%
  mutate(binned_year_chr = paste0(binned_year, "-", binned_year + 5))

tot_term_count <- components_w_meta %>%
  group_by(dimension, root_word) %>%
  summarise(
    tot.count = sum(count),
    mean.freq = mean(freq)
    )

components_by_region <- components_w_meta %>%
  mutate(root_word = stringr::str_to_sentence(root_word), dimension = stringr::str_to_sentence(dimension)) %>%
  group_by(Grouping, dimension, root_word) %>%
  summarise(proportion = mean(count > 0))

components_w_meta_yr <- components_w_meta %>%
  filter(first_yr != "NA") %>%
  group_by(report) %>%
  mutate(count = ifelse(!is.na(count), count, 0)) %>%
  mutate(prop = count/total_words * 10000)

components_w_meta$first_yr <- components_w_meta$binned_year  #comment on or off for binning
components_w_meta_yr_bygroup <- components_w_meta %>% filter(Grouping != "Antarctica") %>%
  group_by(Grouping, first_yr, dimension, root_word) %>%
  summarize(tot_count = sum(count), tot_words = sum(total_words), prop = tot_count/tot_words * 10000)

mypalette <- c("#41ae76", "#ffeda0", "#9e9ac8", "#d53e4f", "#4292c6", "#fe9929", "#91cf60", "#fa9fb5", "#969696", "#8c6bb1")


# Plot of the mean frequency of the climate components per 10,000 words by dimension.

ggplot(filter(tot_term_count, !is.na(root_word)),
  aes(x = forcats::fct_reorder(root_word, mean.freq), y = mean.freq, fill = dimension)) +
  geom_col() +
  facet_wrap(~dimension, ncol = 1, scales = "free") +
  scale_fill_manual(values = c("#31a354", "#8856a7", "#fd8d3c")) +
  theme_sleek() +
  theme(legend.position = "none") +
  labs(x = "", y = "Mean frequency per 10,000 words") +
  coord_flip(expand = FALSE)

ggsave("figs/components.png", width = 5, height = 9)


# Plot of the proportion of MPA plans by region that contain the climate components.

ggplot(components_by_region, aes(x = forcats::fct_reorder(root_word, proportion), y = proportion, fill = Grouping)) +
  geom_col() +
  facet_grid(dimension~Grouping, scales = "free") +
  theme_sleek() +
  theme(legend.position = "none", panel.spacing.x = unit(10, "pt"), plot.margin = margin(11/2, 11/2+5, 11/2, 11/2)) +
  scale_y_continuous(breaks = c(0, 0.25, .50, 0.75, 1), labels = c("0", "0.25", "0.50", "0.75", "1.0")) +
  labs(x = "", y = "Proportion of MPA plans") + coord_flip(expand = FALSE) +
  scale_fill_brewer(palette = "Set3")

ggsave("figs/component-props-by-region.png", width = 14, height = 12)


# Plot of the total frequency of the climate components per 10,000 words in MPA plans by region and MPA plan publication year. Each dot is an MPA plan/PDF.

ggplot(components_w_meta_yr, aes(x = as.numeric(first_yr), y = prop, colour = Grouping)) +
  geom_point(alpha = 0.7) +
  # facet_wrap(~Grouping, nrow = 2) +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "MPA plan publication year", y = "Frequencey per 10,000 words in MPA plan") +
  scale_color_manual(values = mypalette)

ggsave("figs/components-time.png", width = 8, height = 4)


# Plots of the frequency of the individual climate components in each dimension per 10,000 words in MPA plans by region and MPA plan publication year. Years are binned to reduce noise in the trend lines.

ggplot(filter(components_w_meta_yr_bygroup, dimension == "Ecological"), aes(x = as.numeric(first_yr), y = prop, colour = Grouping)) +
  geom_line(aes(colour = Grouping)) +
  facet_wrap(~root_word, nrow = 4, scales = "free_y") +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "MPA plan publication year", y = "Frequency per 10,000 words in MPA plan") +
  scale_color_manual(values = mypalette)

ggsave("figs/components-time-bygroup-ecological_binned.png", width = 16, height = 9)

ggplot(filter(components_w_meta_yr_bygroup, dimension == "Physical"), aes(x = as.numeric(first_yr), y = prop, colour = Grouping)) +
  geom_line(aes(colour = Grouping)) +
  facet_wrap(~root_word, nrow = 3, scales = "free_y") +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "MPA plan publication year", y = "Frequency per 10,000 words in MPA plan") +
  scale_color_manual(values = mypalette)

ggsave("figs/components-time-bygroup-physical-binned.png", width = 16, height = 9)

ggplot(filter(components_w_meta_yr_bygroup, dimension == "Sociological", root_word != "Traditional diet"), aes(x = as.numeric(first_yr), y = prop, colour = Grouping)) +
  geom_line(aes(colour = Grouping)) +
  facet_wrap(~root_word, nrow = 3, scales = "free_y") +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "MPA plan publication year", y = "Frequency per 10,000 words in MPA plan") +
  scale_color_manual(values = mypalette)

ggsave("figs/components-time-bygroup-sociological-binned.png", width = 16, height = 9)


# Plots of the frequency of the individual climate components in each dimension per 10,000 words in MPA plans by region and MPA plan publication year. Years are binned to reduce noise in the trend lines. Same data, different view from previous three plots.

ggplot(filter(components_w_meta_yr_bygroup, dimension == "Ecological", Grouping != "Asia"), aes(x = as.numeric(first_yr), y = prop)) +
  geom_line() +
  facet_grid(root_word~Grouping, scales = "free_y") +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "MPA plan publication year", y = "Frequency per 10,000 words in MPA plan")

ggsave("figs/components-time-bygroup-ecological-binned-v2.png", width = 20, height = 27)

ggplot(filter(components_w_meta_yr_bygroup, dimension == "Physical", Grouping != "Asia"), aes(x = as.numeric(first_yr), y = prop)) +
  geom_line() +
  facet_grid(root_word~Grouping, scales = "free_y") +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "MPA plan publication year", y = "Frequency per 10,000 words in MPA plan")

ggsave("figs/components-time-bygroup-physical-binned-v2.png", width = 20, height = 20)

ggplot(filter(components_w_meta_yr_bygroup, dimension == "Sociological", Grouping != "Asia"), aes(x = as.numeric(first_yr), y = prop)) +
  geom_line() +
  facet_grid(root_word~Grouping, scales = "free_y") +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "MPA plan publication year", y = "Frequency per 10,000 words in MPA plan")

ggsave("figs/components-time-bygroup-sociological-binned-v2.png", width = 20, height = 26)
