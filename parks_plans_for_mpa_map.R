library(dplyr)

pub_years <- readRDS("data-generated/MPAplan-pub-year.rds")
dat <- readxl::read_xlsx("WDPA_English_Combined_2021-07-16_so.xlsx")

names(dat) <- gsub(" ", ".", names(dat))
luCountryGroup <- read.csv("luCountryGroup.csv")

dat2 <- dat[, names(dat) %in% c("NAME", "DESIG", "WDPAID", "Parent.Nation", "Saved.File.Name", "Found", "Simplified.comments")]
dat2$PA.Name <- paste(dat2$NAME, dat2$DESIG)
dat2$Found <- tolower(dat2$Found)
dat2$Simplified.comments <- tolower(dat2$Simplified.comments)
dat2$PA.Name <- paste(dat2$NAME, dat2$DESIG)
dat2$paper <- c(as.character(dat2$Saved.File.Name))
dat2 <- dat2[, !names(dat2) %in% c("Saved.File.Name")]

# Split out lines for parks with multiple papers
dat2 <- tidyr::separate_rows(dat2, paper, sep = ";")
dat2$paper <- gsub("^\\s|\\s$", "", dat2$paper)
dat2$paper <- paste0(gsub(".pdf$", "", dat2$paper), ".pdf")
names(dat2)[names(dat2) == "Parent.Nation"] <- c("Country")
dat2$paper[dat2$paper == "NA.pdf"] <- NA

dat2 <- rename(dat2, report = paper) %>%
  filter(report != "NA")

mpa_plan_list <- left_join(pub_years, dat2)

write.csv(mpa_plan_list, "plans_with_mpa_ids_for_map.csv")
saveRDS(mpa_plan_list, file = "data-generated/plans_with_mpa_ids_for_map.rds")
