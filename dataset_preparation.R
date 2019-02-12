# Install and load needed packages
needed_packages <- c("NbClust",
                     "cluster",
                     "corrplot",
                     "tidyverse")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Load dataframe
purchases <- read.csv("online_retail.csv", sep = ";", dec = ",")

# Rename variables
purchases <- purchases %>%
  rename(invoice_id       = InvoiceNo,
         product_id       = StockCode,
         product_name     = Description,
         product_quantity = Quantity,
         invoice_datetime = InvoiceDate,
         product_price    = UnitPrice,
         customer_id      = CustomerID,
         customer_country = Country)

# Check for missing values
apply(purchases, 2, function(x) any(is.na(x)))

# Since we want to cluster customers, we cannot have have purchases without the customer information
# We can however check if it was present in another row of the same invoice
invoices_na_customer <- purchases %>%
  filter(is.na(customer_id)) %>%
  distinct(invoice_id) %>%
  pull()
purchases %>%
  filter(invoice_id %in% invoices_na_customer,
         !is.na(customer_id))

# Apparently 3710 invoices do not have the relative customer_id
# And we cannot derive it in a secure way without risking to ruining the dataset for clusterin
# Therefore we will simply delete these rows
purchases <- purchases %>%
  filter(!is.na(customer_id))
rm("invoices_na_customer")

# New variables
purchases <- purchases %>%
  mutate(tot_product_price = product_quantity*product_price,
         product_name      = tolower(product_name),
         customer_country  = fct_relabel(customer_country, tolower),
         invoice_datetime  = lubridate::dmy_hm(invoice_datetime, tz = "Europe/London"),
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
                                                  "night"))) %>%
           factor(levels = c("morning", "afternoon", "evening", "night")),
         invoice_tow_tod   = paste(invoice_tow, invoice_tod, sep = "_")) %>%
  arrange(customer_id, invoice_year, invoice_month, invoice_day, invoice_hour) %>%
  group_by(customer_id) %>%
  mutate(invoice_number = dense_rank(invoice_id)) %>%
  ungroup()