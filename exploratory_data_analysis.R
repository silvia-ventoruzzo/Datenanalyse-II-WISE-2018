# Install and load needed packages
needed_packages <- c("tidyverse",
                     "lubridate",
                     "readxl",
                     "xtable",
                     "Jmisc")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Source script and functions
source("dataset_preparation.R")
Jmisc::sourceAll(file.path(getwd(), "Helpers", fsep="/"))

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