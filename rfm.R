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
                                   recency_bins   = 4,
                                   frequency_bins = 4,
                                   monetary_bins  = 4)
rfm_df = rfm_results$rfm %>%
  rename(recency   = recency_days,
         frequency = transaction_count,
         monetary  = amount) %>%
  dplyr::mutate(segment = ifelse(rfm_score == 444, "Best customer",
                   ifelse(rfm_score %in% c(441, 442, 443, 341, 342, 343, 344, 431, 432, 433, 434,
                                           331, 332, 333, 334), "Loyal customer",
                   ifelse(rfm_score %in% c(414, 424, 413, 423), "Promising new customer",
                   ifelse(rfm_score %in% c(144, 143, 134, 133, 244, 243, 234, 233), "Churned best customer",
                   ifelse(rfm_score %in% c(111, 112, 121, 122, 211, 221, 212, 222), "Lost customer",
                                                "Average customer"))))) %>%
                  as.factor() %>%
                  fct_relevel("Best customer", "Loyal customer", "Promising new customer",
                              "Churned best customer", "Lost customer", "Average customer"))
# Max transaction date
# invoice_date_max = transactions %>%
#   summarize(max = max(invoice_date)) %>%
#   t() %>%
#   as.Date()
# 
# rfm_values = transactions %>%
#   group_by(customer_id) %>%
#   summarize(recency   = invoice_date_max - max(invoice_date),
#             frequency = n_distinct(invoice_id),
#             monetary  = sum(tot_product_price))

  

