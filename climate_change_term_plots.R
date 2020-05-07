library(ggplot2)
library(ggsidekick)
library(dplyr)

climate_change_terms <- readRDS("data-generated/climate-search-results.rds")
climate_change_terms$Grouping <-  recode(climate_change_terms$Grouping, California_MPAN = "USA")

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

ggsave("climte_by_region_v1.png", width = 5, height = 9)

ggplot(climate_terms_region, aes(x = forcats::fct_reorder(term, proportion), y = proportion, fill = Grouping)) +
  geom_col() +
  facet_wrap(~Grouping, nrow = 2) +
  theme_sleek() +
  theme(legend.position = "none", panel.spacing.x = unit(10, "pt"), plot.margin = margin(11/2, 11/2+5, 11/2, 11/2)) +
  scale_y_continuous(breaks = c(0, 0.25, .50, 0.75, 1), labels = c("0", "0.25", "0.50", "0.75", "1.0")) +
  labs(x = "", y = "Proportion of MPAs") + coord_flip(expand = FALSE) +
  scale_fill_brewer(palette = "Set3")

ggsave("climte_by_region_v2.png", width = 8, height = 3)
