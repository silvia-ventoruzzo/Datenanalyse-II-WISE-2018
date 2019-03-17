# Install and load needed packages
needed_packages <- c("tidyverse",
                     "xtable",
                     "Jmisc",
                     "reshape2",
                     "rfm",
                     "cowplot",
                     "plotly")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Load scripts and functions
source("exploratory_data_analysis.R")
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
rfm_df = rfm_results$rfm %>%
  rename(recency   = recency_days,
         frequency = transaction_count,
         monetary  = amount)

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

# Histograms
rfm::rfm_histograms(rfm_results)

# Recency vs Frequency
rfm::rfm_rf_plot(rfm_results)

# Recency vs Monetary
rfm::rfm_rm_plot(rfm_results)

# Descriptive statistics
rfm_df %>%
  select(recency, frequency, monetary) %>%
  descriptive_statistics() %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)

# Boxplots rfm values
plot_recency <- ggplot(data = rfm_df) +
  geom_boxplot(aes(x = "recency", y = recency), fill = "red",
               outlier.colour = "red", outlier.shape = 1) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

plot_frequency <- ggplot(data = rfm_df) +
  geom_boxplot(aes(x = "frequency", y = frequency), fill = "blue",
               outlier.colour = "blue", outlier.shape = 1) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

plot_monetary <- ggplot(data = rfm_df) +
  geom_boxplot(aes(x = "monetary", y = monetary), fill = "darkgreen",
               outlier.colour = "darkgreen", outlier.shape = 1) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

plot_grid(plot_recency, plot_frequency, plot_monetary,
          labels = NULL, ncol = 3)

dev.copy2pdf(file = "../Paper/rfm_values_boxplots.pdf")
dev.off()

# Boxplots rfm scores
plot_recency <- ggplot(data = rfm_df) +
  geom_boxplot(aes(y = recency_score), fill = "red",
               outlier.colour = "red", outlier.shape = 1)

plot_frequency <- ggplot(data = rfm_df) +
  geom_boxplot(aes(y = frequency_score), fill = "blue",
               outlier.colour = "blue", outlier.shape = 1)

plot_monetary <- ggplot(data = rfm_df) +
  geom_boxplot(aes(y = monetary_score), fill = "darkgreen",
               outlier.colour = "darkgreen", outlier.shape = 1)

plot_grid(plot_recency, plot_frequency, plot_monetary,
          labels = NULL, ncol = 3)

dev.copy2pdf(file = "../Paper/rfm_scores_boxplots.pdf")
dev.off()

# 3D Scatterplot
plot_ly(data = rfm_df,
        x = ~recency,
        y = ~frequency, 
        z = ~monetary,
        type = "scatter3d",
        mode="markers",
        size = 1)

