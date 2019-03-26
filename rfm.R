# Install and load needed packages
needed_packages <- c("tidyverse",
                     "xtable",
                     "Jmisc",
                     "reshape2",
                     "rfm",
                     "cowplot",
                     "plotly",
                     "GGally",
                     "ggmosaic")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Load scripts and functions
source("dataset_preparation.R")
Jmisc::sourceAll(file.path(getwd(), "Helpers", fsep="/"))

## RFM
rfm_results = rfm::rfm_table_order(data           = transactions_unique,
                                   customer_id    = customer_id,
                                   order_date     = invoice_date,
                                   revenue        = invoice_total,
                                   analysis_date  = as.Date("2011-11-30"),
                                   recency_bins   = 5,
                                   frequency_bins = 5,
                                   monetary_bins  = 5)

# Segments
rfm_df = rfm_results$rfm %>%
  dplyr::rename(recency   = recency_days,
                frequency = transaction_count,
                monetary  = amount) %>%
  dplyr::mutate(segment = ifelse(recency_score   %in% seq(4, 5) & 
                                 frequency_score %in% seq(4, 5) & 
                                 monetary_score  %in% seq(4, 5),
                                 "Champions",
                          ifelse(recency_score   %in% seq(3, 5) & 
                                 frequency_score %in% seq(3, 5) & 
                                 monetary_score  %in% seq(3, 5) &
                                 !(recency_score   == 3 & 
                                   frequency_score == 3 & 
                                   monetary_score  == 3),
                                 "Loyal customers",
                          ifelse(recency_score   %in% seq(3, 5) & 
                                 frequency_score %in% seq(1, 3) & 
                                 monetary_score  %in% seq(1, 3) &
                                 !(recency_score   == 3 & 
                                   frequency_score == 3 & 
                                   monetary_score  == 3) &
                                 !(recency_score   %in% seq(3, 4) & 
                                   frequency_score == 1 & 
                                   monetary_score  == 1),
                                 "Potential loyalists",
                          ifelse(recency_score   %in% seq(4, 5) & 
                                 frequency_score == 1 & 
                                 monetary_score  %in% seq(1, 5),
                                 "New customers",
                          ifelse(recency_score   %in% seq(3, 4) & 
                                 frequency_score == 1 & 
                                 monetary_score  %in% seq(1, 2),
                                 "Promising",
                          ifelse(recency_score   %in% seq(2, 3) & 
                                 frequency_score %in% seq(2, 3) & 
                                 monetary_score  %in% seq(2, 3) &
                                 !(recency_score   == 2 & 
                                   frequency_score == 2 & 
                                   monetary_score  == 2),
                                 "Need attention",
                          ifelse(recency_score   == 2 & 
                                 frequency_score == 2 & 
                                 monetary_score  == 2,
                                 "About to sleep",
                          ifelse(recency_score   %in% seq(1, 2) & 
                                 frequency_score %in% seq(2, 5) & 
                                 monetary_score  %in% seq(2, 5) &
                                 !(recency_score   %in% seq(1, 2) & 
                                   frequency_score == 5 & 
                                   monetary_score  == 5),
                                "At risk",
                          ifelse(recency_score   %in% seq(1, 2) & 
                                 frequency_score == 5 & 
                                 monetary_score  == 5,
                                 "Can't lose them",
                          ifelse(recency_score   %in% seq(1, 2) & 
                                 frequency_score %in% seq(1, 2) & 
                                 monetary_score  %in% seq(1, 2) &
                                 !(recency_score   == 1 & 
                                   frequency_score == 1 & 
                                   monetary_score  == 1),
                                 "Hibernating",
                          ifelse(recency_score   == 1 & 
                                 frequency_score == 1 & 
                                 monetary_score  == 1,
                                 "Lost", "Average customers"))))))))))) %>%
                  as.factor() %>%
                  fct_relevel("Champions", "Loyal customers", "Potential loyalists", "New customers",
                              "Promising", "Need attention", "About to sleep", "At risk",
                              "Can't lose them", "Hibernating", "Lost", "Average customers"))

# First purchase
first_purchase = transactions_unique %>%
  group_by(customer_id) %>%
  summarize(first_purchase = difftime(as.Date("2011-11-30"), min(invoice_date)) %>%
                                time_length(unit = "months") %>%
                                round(1))

# Join
rfm_df = rfm_df %>%
  dplyr::inner_join(first_purchase, by = "customer_id")

## REMOVE UNNECESSARY OBJECTS
rm("first_purchase", "test")

