library(ggplot2)
library(ggsidekick)
library(dplyr)

scienceterms_w_meta <- readRDS("data-generated/scienceterms-search-results-w-meta-rpt.rds")
pub_years <- readRDS("data-generated/MPAplan_pub_year.rds")

scienceterms_w_meta <- left_join(scienceterms_w_meta, pub_years, by = "report")

scienceterms_w_meta <- mutate(scienceterms_w_meta, term = stringr::str_to_sentence(term))

scienceterms_w_meta$Grouping <-  recode(scienceterms_w_meta$Grouping, California_MPAN = "USA", ABNJ = "Antarctica")
write.csv(scienceterms_w_meta, file = "scienceterms_w_meta_w_pub_years_rpt.csv")

scienceterms_w_meta$binned_year <-
  seq(1970, 2020, 5)[findInterval(as.numeric(scienceterms_w_meta$first_yr),
    vec = seq(1970, 2020, 5))]

scienceterms_w_meta <- scienceterms_w_meta %>%
  mutate(binned_year_chr = paste0(binned_year, "-", binned_year + 5))

########################

scienceterm_prop_bygroup <- scienceterms_w_meta %>%
  group_by(Grouping, term) %>%
summarise(proportion = mean(count > 0))

ggplot(scienceterm_prop_bygroup,
  aes(x = forcats::fct_reorder(term, proportion), y = proportion, fill = Grouping)) +
  geom_col() +
  facet_wrap(~Grouping, nrow = 1) +
  theme_sleek() +
  theme(legend.position = "none", panel.spacing.x = unit(10, "pt"), plot.margin = margin(11/2, 11/2+5, 11/2, 11/2)) +
  scale_y_continuous(breaks = c(0, 0.25, .50, 0.75, 1), labels = c("0", "0.25", "0.50", "0.75", "1.0")) +
  labs(x = "", y = "Proportion of MPAs") +
  coord_flip(expand = FALSE) +
  scale_fill_brewer(palette = "Set3")

ggsave("scienceterms_by_region.png", width = 20, height = 4)

ggplot(filter(scienceterms_w_meta, !term %in% c("Trade-off", "Early warning system")), aes(x = count)) +    #filter out Trade-off and Early warning system b/c they almost never or never appeared in any PDF
  geom_histogram(colour="black", fill="#99d8c9") +
  facet_wrap(~term, scales = "free_x") +
  theme_sleek() +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Count per MPA plan", y = "")

ggsave("scienceterms_count_histogram.png", width = 10, height = 10)


for_manual_search <- scienceterms_w_meta %>% filter(term %in% c("Metric", "Indicator", "Transects", "Survey", "Target", "Threshold") & count > 3) %>%
  group_by(report) %>%
  filter(length(unique(term)) > 1)

set.seed(6)
manual_pdf_search <- tibble(report = sample(unique(for_manual_search$report), size = length(unique(for_manual_search$report))))

#readr::write_csv(manual_pdf_search, "manual_pdf_search.csv")

