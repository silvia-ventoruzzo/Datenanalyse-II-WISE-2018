# Install and load needed packages
needed_packages <- c("NbClust",
                     "cluster",
                     "corrplot",
                     "tidyverse",
                     "readxl")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Load dataframe
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
test <- transactions %>%
  mutate(product_value     = product_quantity*product_price,
         product_name      = tolower(product_name),
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

# Keep only transactions of one year