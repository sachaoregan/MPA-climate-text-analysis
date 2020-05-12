library(ggplot2)
library(ggsidekick)

components_w_meta <- readRDS("data-generated/component-search-results_w_meta_rpt.rds")
pub_years <- readRDS("data-generated/MPAplan_pub_year.rds")
total_words <- readRDS(file = "data-generated/total_words.rds")

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
