library(ggplot2)
library(ggsidekick)
library(dplyr)
library(tidyr)

# Read in csv containing data from the manual PDF search. Join on the metadata, publication years, and total words.

pdf_data_pull <- readr::read_csv("manual-pdf-data-pull.csv", na = c("", "na"))
pdf_data_pull <- pdf_data_pull %>% filter(!is.na(disc_of_climate_effects_on_park_ecological))
metadata <- readRDS("data-generated/mpa-metadata.rds")
metadata <- metadata %>% rename(report = paper)

pub_years <- readRDS("data-generated/MPAplan-pub-year.rds")
total_words <- readRDS(file = "data-generated/total-words.rds")

pdf_data_pull <- left_join(pdf_data_pull, metadata, by = "report")
pdf_data_pull <- left_join(pdf_data_pull, pub_years, by = "report")
pdf_data_pull <- left_join(pdf_data_pull, total_words, by = "report")

pdf_data_pull$Grouping <-  recode(pdf_data_pull$Grouping, California_MPAN = "USA", ABNJ = "Antarctica")

write.csv(pdf_data_pull, "pdf_data_pull_w_metadata.csv")

pdf_data_pull <- select(pdf_data_pull, -sort_order, -other_monitoring) %>%
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
"targets", "Monitoring",
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

# Make these NAs Nos:
pdf_data_pull <- mutate(pdf_data_pull, value = ifelse(variable == "Indicators linked to climate change" & is.na(value), "No", value))

pdf_data_by_region <-  pdf_data_pull %>% group_by(Grouping, dimension, variable, value) %>%
  tally() %>%
  group_by(Grouping, variable) %>%
  mutate(proportion = n / sum(n)) %>% ungroup()


mypalette <- c("#fb6a4a", "#41b6c4")
mypalette2 <- c("#fb6a4a", "#a8ddb5", "#c7eae5", "#41b6c4")


# Plots of the proportion of the MPA plans/PDFs manually searched that review climate effects on the parks, have climate objectives and strategies, and are undertaking or plan to undertake monitoring.

ggplot(filter(pdf_data_by_region, dimension == "Review of climate effects on park"), aes(x = variable, y = proportion, fill = value)) +
  geom_col(position = "stack") +
  facet_wrap(~Grouping, nrow = 2) +
  theme_sleek() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 40, hjust = 1)) +
  labs(x = "", y = "Proportion", title= "Review of climate effects on park") +
  scale_fill_manual(values = mypalette)
ggsave("figs/manual-pdf-search-climate-effects-v2.png", width = 8, height = 4)

ggplot(filter(pdf_data_by_region, dimension == "Climate objectives and strategies"), aes(x = variable, y = proportion, fill = value)) +
  geom_col(position = "stack") +
  facet_wrap(~Grouping,  nrow = 2) +
  theme_sleek() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = "", y = "Proportion", title= "Climate change planning") +
  scale_fill_manual(values = mypalette)
ggsave("figs/manual-pdf-search-climate-planning-v2.png", width = 6.5, height = 3.5)

# tigure versions: -----------------------------------------------------

make_panel <- function(component) {
  dat <- filter(pdf_data_by_region, dimension == component, value == 'Yes')
  dat2 <- filter(pdf_data_by_region, dimension == component)
  lu <- distinct(select(dat2, Grouping, variable))
  dat <- left_join(lu, dat)
  dat$proportion <- ifelse(is.na(dat$proportion), 0, dat$proportion)
  ggplot(dat, aes(x = Grouping, y = variable, fill = proportion)) +
    theme_sleek() +
    geom_tile(colour = "grey70") +
    geom_text(aes(label= gsub("0.00", "", sprintf("%.2f", round(proportion, 2)), "")),
      color='grey10', size = 2.5)+
    scale_fill_distiller(palette = "Blues", direction = 1, limits = c(0, 1.2)) +
    labs(y = "", x = "", fill = "Proportion") +
    scale_x_discrete(position = "top") +
    coord_cartesian(expand = FALSE) +
    guides(fill = FALSE) +
    theme(axis.text.x.top = element_text(angle = 45, hjust = 0))
  }
g1 <- make_panel('Climate objectives and strategies')
g2 <- make_panel('Review of climate effects on park')
g <- cowplot::plot_grid(g2,g1, ncol = 1L, labels = c("A: Review of climate effects on MPA","B: Climate objectives and strategies"), label_fontface = "plain", label_x = -0.12, label_y = 0.94, align = "v", label_size = 10)
ggsave("figs/manual-search-tigure.pdf", width=7, height=4)
ggsave("figs/manual-search-tigure.png", width=7, height=4)
ggsave("figs/manual-search-tigure.jpeg", dpi = 300, units = "cm", width = 16, height = 10.5)
# ---------------------------------------------

pdf_data_pull %>% group_by(Grouping, dimension, variable, value) %>%
  tally() %>%
  group_by(Grouping, variable) %>%
  mutate(proportion = n / sum(n)) %>% ungroup()

mypalette2 <- viridis::viridis(4, begin = 0.05, end = 0.92, direction = 1)
pdf_data_by_region %>%
  filter(dimension == "Monitoring", variable != "Metrics") %>%
  mutate(variable = gsub("Indicators linked", "Indicators linked\n", variable)) %>%
  mutate(variable = gsub("Commitment to", "Commitment to\n", variable)) %>%
  mutate(variable = gsub("targets", "Targets", variable)) %>%
  ggplot(aes(x = forcats::fct_rev(variable), y = proportion, fill = value)) +
  geom_col(position = "stack") +
  facet_wrap(~Grouping,  nrow = 2) +
  theme_sleek() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "top", legend.margin = margin(t = 0, r = 0, b = -5, l = 0, unit = "pt"), legend.justification = c(0, 0), panel.spacing.x = unit(15, "pt")) +
  labs(x = "", y = "Proportion") +
 scale_fill_manual(values = mypalette2, guide = guide_legend(reverse = TRUE)) +
  coord_flip(expand = FALSE) +
  scale_y_continuous(breaks = c(0, 0.5, 1))

ggsave("figs/manual-pdf-search-climate-monitoring-v3.png", width = 9, height = 4)
ggsave("figs/manual-pdf-search-climate-monitoring-v3.pdf", width = 9, height = 4)
ggsave("figs/manual-pdf-search-climate-monitoring-v3.jpeg", dpi = 300, units = "cm", width = 20, height = 9.5)

# Plot of the climate change robustness indices across regions.

d <- readr::read_csv("manual-pdf-data-pull.csv", na = c("", "na"))
convert012 <- function(x) {
  case_when(
    x == "yes" ~ 2,
    x == "no" ~ 0,
    x == "na" ~ 0,
    x == "planned" ~ 1,
    x == "some" ~ 1,
    x == "ongoing" ~ 2,
    TRUE ~ NA_real_
  )
}
d_scored <- d %>%
  mutate(across(disc_of_climate_effects_on_park_ecological:other_monitoring, convert012))
d_scored$total <- apply(d_scored[-c(1:3)], 1, sum, na.rm = TRUE)

pdf_scored <- left_join(d_scored, metadata, by = "report")
pdf_scored <- left_join(pdf_scored, pub_years, by = "report")
pdf_scored$Grouping <-  recode(pdf_scored$Grouping, California_MPAN = "USA", ABNJ = "Antarctica")
write.csv(pdf_scored, "manual_pdf_search_cc_robustness_index_v2.csv")

xx <- group_by(pdf_scored, Grouping, pub_yr, total) %>% summarise(n = n(), .groups = "drop_last") %>%
  arrange(total)
#all <- expand_grid(total = seq(0, max(y$total)), Grouping = unique(d$Grouping))
#xx <- left_join(all, xx) %>%
#  mutate(n = replace_na(n, 0))

xx <- xx %>% group_by(Grouping) %>%
  mutate(prop = n / sum(n))

ggplot(xx, aes(x = total, y = n)) +
  geom_col() +
  facet_wrap(~Grouping,  nrow = 2) +
  theme_sleek() +
  theme(legend.title = element_blank()) +
  coord_cartesian(expand = FALSE, xlim = c(0, 24), ylim = c(0, 16)) +
  theme(panel.spacing.x = grid::unit(10, "pt")) +
  scale_y_continuous(breaks = seq(0, 16, 2)) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Climate change robustness score", y = "Number of management plans")

ggsave("figs/climate_robustness_index_v2.png", width = 10, height = 4)
ggsave("figs/climate_robustness_index_v2.pdf", width = 10, height = 4)
ggsave("figs/climate_robustness_index_v2.jpeg", dpi = 300, units = "cm", width = 18, height = 8)

plans_with_park_ids <- readRDS("data-generated/plans_with_mpa_ids_for_map.rds") %>%
  select(report, NAME, DESIG, WDPAID)
pdf_scored_w_park_desig_and_yr <- left_join(pdf_scored, plans_with_park_ids, by = "report")

pdf_scored_w_park_desig_and_yr$DESIG <- plyr::revalue(pdf_scored_w_park_desig_and_yr$DESIG, c("Site of Community Importance (Habitats Directive)" = "Special Area of Conservation (Habitats Directive)"))

desig_replace <- readr::read_csv("replace_ospar_w_new_desigs.csv")
pdf_scored_w_park_desig_and_yr <- desig_replace %>% right_join(pdf_scored_w_park_desig_and_yr, by=c("NAME","DESIG")) %>%
mutate(DESIG = ifelse(!is.na(NEW_DESIG), NEW_DESIG, DESIG)) %>%
  select(-NEW_DESIG)

unique(pdf_scored_w_park_desig_and_yr$DESIG) %>% sort() %>% dput()

parks <- c("National Wildlife Refuge", "National Estuarine Research Reserve", "Special Area of Conservation (Habitats Directive)", "Special Protection Area (Birds Directive)", "Ramsar Site, Wetland of International Importance", "Marine Protected Area", "National Park")

parks_lu <- tibble(DESIG = parks, DESIG_ABBREV = c("NWR", "NERR", "SAC", "SPA", "RS WII", "MPA", "NP"))

fit_dat <- pdf_scored_w_park_desig_and_yr %>%
  filter(DESIG %in% parks, Grouping %in% c("Canada", "Oceania", "UK", "USA")) %>%
  left_join(parks_lu) %>%
  group_by(Grouping, DESIG_ABBREV)

out <- fit_dat %>% group_split() %>% purrr::map_dfr(function(x) {
  if (nrow(x) > 4) {
    m <- MASS::glm.nb(total ~ as.numeric(pub_yr), data = x)
    nd <- data.frame(pub_yr = seq(min(x$pub_yr), max(x$pub_yr), length.out = 100))
    p <- predict(m, newdata = nd, se.fit = TRUE)
    nd$log_est <- p$fit
    nd$est <- exp(p$fit)
    nd$se <- p$se.fit
    nd$lwr <- exp(nd$log_est - 2 * nd$se)
    nd$upr <- exp(nd$log_est + 2 * nd$se)
    nd$DESIG_ABBREV <- x$DESIG_ABBREV[1]
    nd$Grouping <- x$Grouping[1]
    nd
  }
})


lab_dat_lu <- tribble(
  ~Grouping, ~DESIG_ABBREV, ~pub_yr,
  "Canada","MPA",2005,
  "Canada","NP",2016,
  "Oceania","RS WII",2016,
  "UK","SAC",2019,
  "UK","RS WII",2000,
  "UK","SPA",2006,
  "USA","NERR",2019,
  "USA","NWR", 2016,
  "USA","RS WII", 1996)

lab_dat <- left_join(lab_dat_lu, out)

pdf_scored_w_park_desig_and_yr <- left_join(pdf_scored_w_park_desig_and_yr , parks_lu)
pdf_scored_w_park_desig_and_yr <- mutate(pdf_scored_w_park_desig_and_yr, legend_lab = paste0(DESIG, " (", DESIG_ABBREV, ")"))

out <- left_join(out, parks_lu)
out <- mutate(out, legend_lab = paste0(DESIG, " (", DESIG_ABBREV, ")"))

pdf_scored_w_park_desig_and_yr$DESIG[pdf_scored_w_park_desig_and_yr$DESIG_ABBREV == "MPA" & pdf_scored_w_park_desig_and_yr$Grouping == "UK" & !is.na(pdf_scored_w_park_desig_and_yr$DESIG_ABBREV == "MPA")] <- "IGNORE ME"
pdf_scored_w_park_desig_and_yr$DESIG[pdf_scored_w_park_desig_and_yr$DESIG_ABBREV == "MPA" & pdf_scored_w_park_desig_and_yr$Grouping == "USA" & !is.na(pdf_scored_w_park_desig_and_yr$DESIG_ABBREV == "MPA")] <- "IGNORE ME"
pdf_scored_w_park_desig_and_yr$DESIG[pdf_scored_w_park_desig_and_yr$DESIG_ABBREV == "NP" & pdf_scored_w_park_desig_and_yr$Grouping == "USA" & !is.na(pdf_scored_w_park_desig_and_yr$DESIG_ABBREV == "MPA")] <- "IGNORE ME"

library(ggrepel)
pdf_scored_w_park_desig_and_yr %>% filter(DESIG %in% parks, Grouping %in% c("Canada", "Oceania", "UK", "USA")) %>%
  ggplot(aes(x = as.numeric(pub_yr), y = total)) +
  geom_jitter(aes(colour = legend_lab), width = 0.2, height = 0, alpha = 0.9) +
  geom_jitter(data = pdf_scored_w_park_desig_and_yr %>% filter(!DESIG %in% parks, Grouping %in% c("Canada", "Oceania", "UK", "USA")), width = 0.2, height = 0, colour = "grey50", alpha = 0.6) +
 # geom_smooth(aes(colour = DESIG_ABBREV, fill = DESIG_ABBREV), method = MASS::glm.nb, alpha = 0.1) +
 #  geom_smooth(aes(colour = DESIG_ABBREV, fill = DESIG_ABBREV), method = MASS::glm.nb, se= FALSE) +
  geom_ribbon(aes(fill = legend_lab, x = pub_yr, y = est, ymin = lwr, ymax = upr), data = out, inherit.aes = FALSE, colour = NA, alpha = 0.1) +
  geom_line(aes(colour = legend_lab, x = pub_yr, y = est), data = out, inherit.aes = FALSE) +
  facet_wrap(~Grouping,  nrow = 2) +
  theme_sleek() +
  theme(panel.spacing.x = unit(20, "pt")) +
  scale_y_continuous(breaks = seq(0, 24, 4)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "MPA plan publication year", y = "Climate change robustness score", colour = "MPA Designation", fill = "MPA Designation") +
  ggrepel::geom_text_repel(aes(x = pub_yr, y = est, label = DESIG_ABBREV), data = lab_dat, inherit.aes = FALSE, min.segment.length = 0.15, size = 3.5, colour = "black")
  # geom_text(aes(label = DESIG_ABBREV, colour = DESIG_ABBREV), data = out)

# gd <- ggplot_build(g)
# gd$data[[3]] %>% group_by(group) %>% filter(x == max(x)) %>%


ggsave("figs/climate_robustness_index_over_time_v2.png", width = 10, height = 5)
ggsave("figs/climate_robustness_index_over_time_v2.pdf", width = 10, height = 5)
ggsave("figs/climate_robustness_index_over_time_v2.jpeg", dpi = 300, units = "cm", width = 23, height = 11)

write.csv(pdf_scored_w_park_desig_and_yr, "pdf_scored_w_park_desig_and_yr.csv")

