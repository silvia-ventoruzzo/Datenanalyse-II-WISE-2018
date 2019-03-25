# Install and load needed packages
needed_packages <- c("tidyverse",
                     "lubridate",
                     "readxl",
                     "xtable",
                     "Jmisc",
                     "mvoutlier",
                     "MASS")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Source script and functions
source("rfm.R")
Jmisc::sourceAll(file.path(getwd(), "Helpers", fsep="/"))

# Descriptive statistics of distinct products and total value of invoices
transactions_unique %>%
  filter(invoice_type == "order") %>%
  dplyr::select(distinct_products, invoice_total) %>%
  descriptive_statistics() %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)


# Distribution of transactions across the week
transactions_unique %>%
  group_by(invoice_dow, invoice_type) %>%
  summarize(count = n()/nrow(.)) %>%
  ggplot() +
  geom_bar(aes(x = invoice_dow, y = count, fill = invoice_type), stat = "identity") +
  theme_bw() +
  labs(x    = "Day of the week",
       y    = "Percentage of transactions",
       fill = "Invoice type") +
  theme(legend.position = "bottom")

# dev.copy2pdf(file = "../Paper/transactions_week.pdf")
# dev.off()

# Distribution of transactions across the day
transactions_unique %>%
  group_by(invoice_tod, invoice_type) %>%
  summarize(count = n()/nrow(.)) %>%
  ggplot() +
  geom_bar(aes(x = invoice_tod, y = count, fill = invoice_type), stat = "identity") +
  theme_bw() +
  labs(x    = "Time of the day",
       y    = "Percentage of transactions",
       fill = "Invoice type") +
  theme(legend.position = "bottom")

# dev.copy2pdf(file = "../Paper/transactions_day.pdf")
# dev.off()

# Time series plot
transactions_unique %>%
  group_by(invoice_date, invoice_type) %>%
  summarize(count = n()) %>%
  ggplot() +
  geom_line(aes(x = invoice_date, y = count, color = invoice_type)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b/%Y") +
  labs(x = "Month",
       y = "Quantity of transactions",
       color = "Transaction type") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = c(rep("black", 12), "transparent"),
                                   angle = 45, hjust = -0.3, vjust = -0.1),
        legend.position = "bottom")

# dev.copy2pdf(file = "../Paper/time_series.pdf")
# dev.off()

## RFM MODEL
# Descriptive statistics
rfm_df %>%
  dplyr::select(recency, frequency, monetary) %>%
  descriptive_statistics() %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)

# Boxplots rfm values
plot_recency <- ggplot(data = rfm_df) +
  geom_boxplot(aes(x = "recency", y = recency), fill = "red",
               outlier.colour = "red", outlier.shape = 1) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.text.x  = element_text(size = rel(1.2)),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())

plot_frequency <- ggplot(data = rfm_df) +
  geom_boxplot(aes(x = "frequency", y = frequency), fill = "blue",
               outlier.colour = "blue", outlier.shape = 1) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.text.x  = element_text(size = rel(1.2)),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())

plot_monetary <- ggplot(data = rfm_df) +
  geom_boxplot(aes(x = "monetary", y = monetary), fill = "darkgreen",
               outlier.colour = "darkgreen", outlier.shape = 1) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.text.x  = element_text(size = rel(1.2)),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())

plot_grid(plot_recency, plot_frequency, plot_monetary,
          labels = NULL, nrow = 3)

# dev.copy2pdf(file = "../Paper/rfm_values_boxplots.pdf")
# dev.off()

# Scatterplots
rfm_df %>%
  dplyr::select(recency, frequency, monetary) %>%
  ggpairs(lower = list(continuous = wrap("points", colour = "red", size = 0.5)),
          diag  = list(continuous = wrap("densityDiag", fill = "red")),
          upper = list(combo = wrap("dot", colour = "red"))) +
  theme_bw()

# dev.copy2pdf(file = "../Paper/scatterplots.pdf")
# dev.off()

# Mosaic plot of RFM Scores
mosaicplot(table(target_data))


# Distribution of Segments
ggplot(rfm_df) +
  geom_bar(aes(x = gsub(" ", "\n", segment), y = (..count..)/sum(..count..)), fill = "blue") +
  theme_bw() +
  labs(x = "Segment",
       y = "Percentage of customers") +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.text.x  = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)),
        axis.text.y  = element_text(size = rel(1.2)))

# dev.copy2pdf(file = "../Paper/segmentdistribution.pdf")
# dev.off()


# Mosaic plot of RFM Scores
ggplot(rfm_df) +
  ggmosaic::geom_mosaic(aes(x = product(recency_score, frequency_score))) +
  facet_grid(monetary_score~.) +
  theme_bw() +
  labs(x = "Recency Score",
       y = "Frequency Score")


## OUTLIERS

# MVE estimators of centroid and covariance matrix
set.seed(784353765)
mve = cov.mve(x     = rfm_df %>% dplyr::select(recency, frequency, monetary),
              nsamp = "best")

# Chi-Square threshold
outlier_chisq_cutoff = qchisq(p = 0.975, df = 3) %>% sqrt()

rfm_df = rfm_df %>%
  dplyr::mutate(md            = mahalanobis(x      = rfm_df %>% dplyr::select(recency, frequency, monetary),
                                            center = mve$center,
                                            cov    = mve$cov) %>% sqrt(),
                outlier_chisq = ifelse(md > outlier_chisq_cutoff, TRUE, FALSE))

sum(rfm_df$outlier_chisq)/nrow(rfm_df) # 0.2839991

# Elbow method
rfm_df %>%
  dplyr::arrange(desc(md)) %>%
  ggplot() +
  geom_line(aes(x = as.numeric(rownames(rfm_df)), y = md), color = "black", alpha = 0.5) +
  geom_point(aes(x = as.numeric(rownames(rfm_df)), y = md), color = "red", alpha = 0.5) +
  labs(x     = "Ordered customers",
       y     = "Mahalanobis distance") +
  scale_y_continuous(breaks = seq(0, 800, by = 100)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.text.x  = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)),
        axis.text.y  = element_text(size = rel(1.2)))

# dev.copy2pdf(file = "../Paper/elbowmethod.pdf")
# dev.off()

rfm_df = rfm_df %>%
  dplyr::mutate(outlier_elbow = ifelse(md > 120, TRUE, FALSE),
                outlier_both  = ifelse(outlier_chisq & outlier_elbow, "Both methods",
                                ifelse(outlier_chisq == FALSE & outlier_elbow == FALSE, "No",
                                       "Only with Chi-Square method")))

sum(rfm_df$outlier_elbow)/nrow(rfm_df) # 0.004617871

ggplot() +
  geom_point(data = rfm_df, aes(x = customer_id, y = md, color = outlier_both), alpha = 0.5) +
  geom_hline(yintercept = outlier_chisq_cutoff, color = "blue", linetype = 2, size = 0.5) +
  annotate("text",
           x = min(rfm_df$customer_id) + 200, y = outlier_chisq_cutoff + 20, 
           label = "Chi-Square", color = "blue", size = 4) +
  geom_hline(yintercept = 120, color = "blue", linetype = 2, size = 0.5) +
  annotate("text", 
           x = min(rfm_df$customer_id) + 300, y = 120 + 20,
           label = "Elbow method", color = "blue", size = 4) +
  theme_bw() +
  labs(x     = "Customers",
       y     = "Mahalanobis distance",
       color = "Is it an outlier?") +
  scale_color_manual(values=c("red", "green", "orange")) +
  theme(axis.title.x = element_text(size = rel(1.2)),
        axis.text.x  = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)),
        axis.text.y  = element_text(size = rel(1.2)),
        legend.position = c(0.2, 0.8))

# dev.copy2pdf(file = "../Paper/outliers.pdf")
# dev.off()

## REMOVED UNNECESSARY OBJECTS
rm("mve", "plot_frequency", "plot_monetary", "plot_recency", "outlier_chisq_cutoff")
