# Install and load needed packages
needed_packages <- c("tidyverse",
                     "lubridate",
                     "readxl")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Load dataframe
unzip(zipfile = file.path(getwd(), "Data", "online_retail.xlsx.zip", fsep="/"),
      files   = "online_retail.xlsx",
      exdir   = ".")
transactions <- read_excel("online_retail.xlsx")

# Rename variables
transactions <- transactions %>%
  rename(invoice_id       = InvoiceNo,
         product_id       = StockCode,
         product_name     = Description,
         product_quantity = Quantity,
         invoice_datetime = InvoiceDate,
         product_price    = UnitPrice,
         customer_id      = CustomerID,
         customer_country = Country)

# New variables
transactions <- transactions %>%
  mutate(product_value     = product_quantity*product_price,
         product_name      = tolower(product_name),
         invoice_type      = ifelse(product_quantity < 0, "cancellation", "order"),
         invoice_date      = lubridate::date(invoice_datetime),
         invoice_day       = lubridate::day(invoice_datetime),
         invoice_month     = lubridate::month(invoice_datetime),
         invoice_year      = lubridate::year(invoice_datetime),
         invoice_dow       = lubridate::wday(invoice_datetime, label = TRUE, abbr = TRUE) %>% 
                                fct_relabel(tolower),
         invoice_tow       = ifelse(invoice_dow == "sat" | invoice_dow == "sun", "weekend",
                                    "workday"),
         invoice_time      = strftime(invoice_datetime,"%H:%M:%S", tz = "Europe/London") %>%
           chron::times(),
         invoice_hour      = lubridate::hour(invoice_datetime),
         invoice_tod       = ifelse(invoice_hour >= 6  & invoice_hour < 12, "morning",
                                    ifelse(invoice_hour >= 12 & invoice_hour < 18, "afternoon",
                                           ifelse(invoice_hour >= 18 & invoice_hour < 24, "evening",
                                                  "night"))),
         invoice_tow_tod   = paste(invoice_tow, invoice_tod, sep = "_")) %>%
  arrange(customer_id, invoice_year, invoice_month, invoice_day, invoice_hour) %>%
  group_by(customer_id) %>%
  mutate(invoice_number = dense_rank(invoice_id)) %>%
  ungroup()

# Keep only transactions with customer_id
transactions = transactions %>%
  filter(!is.na(customer_id))

# Keep only transactions of one year
transactions = transactions %>%
  filter(invoice_date <= as.Date("2011-11-30", format = "%Y-%m-%d"))

# Fix time of invoice within two "times of day"
diff_times = transactions %>%
  group_by(invoice_id) %>%
  summarize(count = n_distinct(invoice_tod)) %>%
  arrange(desc(count))
diff_times = transactions %>%
  filter(invoice_id == 549245) %>%
  dplyr::select(invoice_id, invoice_datetime, invoice_date, invoice_time, invoice_tod)

transactions = transactions %>%
  mutate(invoice_tod = ifelse(invoice_id == "549245", "morning", invoice_tod) %>%
           factor(levels = c("morning", "afternoon", "evening", "night")))

## UNIQUE TRANSACTIONS
transactions_unique = transactions %>%
  group_by(invoice_id) %>%
  summarize(customer_id       = unique(customer_id),
            invoice_date      = unique(invoice_date),
            invoice_day       = unique(invoice_day),
            invoice_dow       = unique(invoice_dow),
            invoice_tod       = unique(invoice_tod),
            invoice_number    = unique(invoice_number),
            invoice_total     = sum(product_value),
            invoice_type      = unique(invoice_type),
            distinct_products = n())

## REMOVE UNNECESSARY OBJECTS
rm("diff_times")