library(ggplot2)
library(ggsidekick)
library(dplyr)

climate_change_terms <- readRDS("data-generated/climate-search-results.rds")
pub_years <- readRDS("data-generated/MPAplan-pub-year.rds")
total_words <- readRDS(file = "data-generated/total-words.rds")

climate_change_terms <- left_join(climate_change_terms, pub_years, by = "report")
climate_change_terms <- left_join(climate_change_terms, total_words, by = "report")

climate_change_terms <- mutate(climate_change_terms, term = stringr::str_to_sentence(term))

climate_change_terms$Grouping <-  recode(climate_change_terms$Grouping, California_MPAN = "USA", ABNJ = "Antarctica")
write.csv(climate_change_terms, file = "data-generated/climate-change-terms-w-pub-years-rpt.csv")

climate_change_terms$binned_year <-
  seq(1970, 2020, 5)[findInterval(as.numeric(climate_change_terms$pub_yr),
  vec = seq(1970, 2020, 5))]

climate_change_terms <- climate_change_terms %>%
  mutate(binned_year_chr = paste0(binned_year, "-", binned_year + 5))

climate_terms_region <- climate_change_terms %>%
  group_by(Grouping, term) %>%
  summarise(proportion = mean(count > 0), numerator=sum(count > 0), denominator=n()) %>%
  filter(term!= "NA")

climate_change_terms_yr <- climate_change_terms %>%
  filter(pub_yr != "NA") %>%
  group_by(report) %>%
  mutate(count = ifelse(!is.na(count), count, 0)) %>%
  mutate(freq = count/total_words * 10000)

climate_change_terms_yr_bygroup <- climate_change_terms %>% filter(Grouping != "Antarctica") %>%
  group_by(Grouping, pub_yr, term) %>%
  summarize(tot_count = sum(count), tot_words = sum(total_words), freq = tot_count/tot_words * 10000)

climate_change_terms$pub_yr <- climate_change_terms$binned_year  #Turns on binning
climate_change_terms_yr_bygroup_binned <- climate_change_terms %>% filter(Grouping != "Antarctica") %>%
  group_by(Grouping, pub_yr, term) %>%
  summarize(tot_count = sum(count), tot_words = sum(total_words), freq = tot_count/tot_words * 10000)

mypalette <- c("#41ae76", "#ffeda0", "#9e9ac8", "#d53e4f", "#4292c6", "#fe9929", "#91cf60", "#fa9fb5", "#969696", "#8c6bb1")

# Plot of the proportion of MPA plans by region that contain the climate change terms.

.blue <- RColorBrewer::brewer.pal(6, "Blues")[5]

g <- ggplot(climate_terms_region, aes(x = forcats::fct_reorder(term, proportion), y = proportion)) +
  geom_col(fill = .blue) +
  facet_wrap(~Grouping, nrow = 2) +
  theme_sleek() +
  theme(legend.position = "none", panel.spacing.x = unit(10, "pt"), plot.margin = margin(11/2, 11/2+5, 11/2, 11/2)) +
  scale_y_continuous(breaks = c(0, .50,1), labels = c("0", "0.50", "1.0")) +
  labs(x = "", y = "Proportion of MPA Plans") + coord_flip(expand = FALSE) +
  geom_text(aes(label = paste0(numerator, "/", denominator)), y = 0.97, col = "black", size = 2.8,
    hjust = 1)
  # scale_fill_brewer(palette = "Set3")

ggsave("figs/prop-climate-terms-by-region.png", width = 7.5, height = 2.8)
ggsave("figs/prop-climate-terms-by-region.pdf", width = 7.5, height = 2.8)


# Plot of the total frequency of the climate change terms per 10,000 words in MPA plans by region and MPA plan publication year. Each dot is an MPA plan/PDF.

# smooth_data <- filter(climate_change_terms_yr, Grouping %in% c('Canada','USA', 'UK'))

palette_OkabeIto <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000")

g <- ggplot(filter(climate_change_terms_yr, !Grouping %in% "Antarctica"),
  aes(x = as.numeric(pub_yr), y = freq, colour = Grouping)) +
  geom_jitter(alpha = 0.5, width = 0.25) +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "MPA plan publication year", y = "Frequency per 10,000 words in MPA plan") +
  scale_color_manual(values = palette_OkabeIto) +
  # scale_colour_brewer(palette = "Set3") +
  coord_cartesian(expand = FALSE, ylim = c(-0.15, max(climate_change_terms_yr$freq) * 1.03))
  # geom_smooth(data=smooth_data)
ggsave("figs/climate-terms-time.png", width = 8, height = 5)
ggsave("figs/climate-terms-time.pdf", width = 8, height = 5)

ggplot(filter(climate_change_terms_yr, !Grouping %in% "Antarctica"),
  aes(x = as.numeric(pub_yr), y = freq)) +
  facet_wrap(~Grouping) +
  geom_jitter(alpha = 0.5, width = 0.25) +
  theme_sleek() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.x = unit(10, units = "pt")) +
  labs(x = "MPA plan publication year", y = "Frequency per 10,000 words in MPA plan") +
  scale_color_manual(values = palette_OkabeIto) +
  # scale_colour_brewer(palette = "Set3") +
  coord_cartesian(expand = FALSE, ylim = c(-0.15, max(climate_change_terms_yr$freq) * 1.03))
# geom_smooth(data=smooth_data)
ggsave("figs/climate-terms-time-panelled.png", width = 8, height = 5)
ggsave("figs/climate-terms-time-panelled.pdf", width = 8, height = 5)


# Plot of the frequency of the individual climate change terms per 10,000 words in MPA plans by region and MPA plan publication year. Years are binned to reduce noise in the trend lines.

ggplot(climate_change_terms_yr_bygroup_binned, aes(x = as.numeric(pub_yr), y = freq, colour = Grouping)) +
  geom_line(aes(colour = Grouping)) +
  facet_wrap(~term, nrow = 2, scales = "free_y") +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "MPA plan publication year", y = "Frequency per 10,000 words in MPA plan") +
  scale_color_manual(values = mypalette)

ggsave("figs/climate-terms-time-bygroup-binned.png", width = 8, height = 4)


# Plot of the frequency of the individual climate change terms per 10,000 words in MPA plans by region and MPA plan publication year. Years are binned to reduce noise in the trend lines. Same data, different view from previous plot ("climate-terms-time-bygroup-binned.png").

ggplot(climate_change_terms_yr_bygroup_binned, aes(x = as.numeric(pub_yr), y = freq)) +
  geom_line() +
  facet_grid(term~Grouping, scales = "free_y") +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "MPA plan publication year", y = "Frequency per 10,000 words in MPA plan")

ggsave("figs/climate-terms-time-bygroup-binned-v2.png", width = 10, height = 8)


# Plot of the number of MPS plans published in each region per five year interval.

total_plans <- climate_change_terms %>% group_by(Grouping, pub_yr) %>%
  filter(pub_yr != "NA") %>%
  summarise(tot_plans = length(unique(report)))

ggplot(total_plans, aes(x = as.numeric(pub_yr), y = tot_plans)) +
  geom_col() +
  facet_wrap(~Grouping, nrow = 2) +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Year", y = "Number of MPA plans published")

ggsave("figs/pub-yr-all-plans-binned.png", width = 10, height = 5)


# Plot of the total frequency of each of the climate change terms per 100,000 words in the MPA plans per five year interval and across all regions.

climate_change_terms_yr_globe <- climate_change_terms %>%
  filter(pub_yr != "NA") %>%
  group_by(Grouping, pub_yr, term) %>%
  summarize(tot_count = sum(count), tot_words = sum(total_words)) %>%
  group_by(pub_yr, term) %>%
  summarise(tot_count = sum(tot_count), tot_words = sum(tot_words), freq = tot_count/tot_words * 100000)

ggplot(climate_change_terms_yr_globe, aes(x = as.numeric(pub_yr), y = freq)) +
  geom_col() +
  theme_sleek() +
  facet_wrap(~term, scales = "free_y") +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Year", y = "Frequency per 100,000 words in MPA plan")

ggsave("figs/climate-terms-time-binned-globe.png", width = 10, height = 5)

# Plot of the frequency of the term "climate change" per 10,000 words in MPA plans by region and MPA plan publication year. Note, years are not binned.
g <- ggplot(filter(climate_change_terms_yr_bygroup, term == "Climate change", Grouping != "Asia"), aes(x = as.numeric(pub_yr), y = freq, colour = Grouping)) +
  geom_point(colour = .blue) +
  geom_line(color=.blue,alpha = 0.5, lwd = 0.5) +
  facet_wrap(~Grouping, nrow = 2) +
  theme_sleek() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey96")) +
  labs(x = "MPA plan publication year", y = "Frequency per 10,000 words") +
  # scale_color_manual(values = mypalette) +
  scale_y_continuous(expand = expansion(mult=c(0.01,.1))) +
  coord_cartesian(xlim = c(2000, 2020))
ggsave("figs/'climate-change'-bygroup-2000-pres.png", width = 8, height = 4)
ggsave("figs/'climate-change'-bygroup-2000-pres.pdf", width = 8, height = 4)

# # Plot of the frequency of the all climate change terms per 10,000 words in MPA plans by region and MPA plan publication year. Note, years are not binned.
# g <- ggplot(filter(climate_change_terms_yr_bygroup, Grouping != "Asia"), aes(x = as.numeric(pub_yr), y = freq, colour = Grouping)) +
#   geom_point(colour = .blue) +
#   geom_line(color=.blue,alpha = 0.5, lwd = 0.5) +
#   facet_wrap(~Grouping, nrow = 2) +
#   theme_sleek() +
#   theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
#     panel.grid.major = element_line(color = "grey96")) +
#   labs(x = "MPA plan publication year", y = "Frequency per 10,000 words") +
#   # scale_color_manual(values = mypalette) +
#   scale_y_continuous(expand = expansion(mult=c(0.01,.1))) +
#   coord_cartesian(xlim = c(2000, 2020))
# ggsave("figs/all-climate-terms-bygroup-2000-pres.png", width = 8, height = 4)
# ggsave("figs/all-climate-terms-bygroup-2000-pres.pdf", width = 8, height = 4)
