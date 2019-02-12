# Load previous script
source("dataset_preparation.R")

## Create products dataframe

# Distinguish free products from the rest
purchases <- purchases %>%
  mutate(product_name = ifelse(product_price == 0, 
                               paste("free", product_name, sep = " "),
                               product_name))

# Count the number of distinct products
purchases %>%
  distinct(product_id, product_name) %>%
  count() %>%
  pull() # 3951

# Calculate the average price of each product
products_mean_prices <- purchases %>%  
  group_by(product_id, product_name) %>%
  distinct(product_price) %>%
  summarize(product_mean_price = mean(product_price) %>% round(2)) %>%
  ungroup()

# Calculate inn which time of day and day of the week combination the products are being bought
products_purchases_time <- purchases %>%
  filter(product_quantity > 0) %>%
  select(product_id, product_name, invoice_tow_tod) %>%
  rowid_to_column(var = "i") %>%
  mutate(yesno  = 1) %>%
  spread(invoice_tow_tod, yesno, fill = 0) %>%
  select(-i) %>%
  group_by(product_id, product_name) %>%
  summarize_all(sum) %>%
  ungroup()

# Calculate the percentage of times products have been returned
products_returned <- purchases %>%
  select(invoice_id, product_id, product_name, product_quantity,
         invoice_datetime, customer_id) %>%
  group_by(product_id, product_name) %>%
  summarize(times_purchased = n_distinct(invoice_id),
            times_returned = sum(product_quantity <= 0),
            percentage_returned = round(times_returned/times_purchased*100, 2)) %>%
  select(-times_returned, -times_purchased) %>%
  ungroup()

# Claculate the percentage of times the products have been reordered (not counting products that have been returned)
products_reordered <- purchases %>%
  filter(product_quantity > 0) %>%
  group_by(customer_id, product_id, product_name) %>%
  mutate(count = n()-1) %>%
  group_by(product_id, product_name) %>%
  summarize(times_purchased = n_distinct(invoice_id),
            times_reordered = sum(unique(count)),
            percentage_reordered = round(times_reordered/times_purchased*100, 2)) %>%
  select(-times_purchased, -times_reordered) %>%
  ungroup()

# Join dataframes
products <- products_mean_prices %>%
  left_join(products_purchases_time, by = c("product_id", "product_name")) %>%
  left_join(products_returned, by = c("product_id", "product_name")) %>%
  left_join(products_reordered, by = c("product_id", "product_name")) %>%
  replace(is.na(.), 0) %>%
  arrange(product_id, product_name)

# Correlation plot
products %>%
  dplyr::select(-product_id, -product_name) %>%
  stats::cor() %>%
  corrplot::corrplot()

rm("products_mean_prices", "products_purchases_time", "products_returned", "products_reordered")

## Calculate number of clusters

# Scale dataframe
products_scaled <- products %>%
  select(-product_id, -product_name) %>%
  scale()

# Calculate distance
products_dist <- dist(products_scaled)
# Hierarchical clustering
products_hclust <- hclust(products_dist)
# Dendogramm
plot(products_hclust) # 3

# Total variance explained
tve <- rep(NA, 15)
for (k in 2:15) {
  clk <- kmeans(products_scaled, k)
  tve[k] <- 1-clk$tot.withinss/clk$totss
}
plot(tve, type="b") # 5 or 7

# Indexes
indexes <- c("ch", "duda", "cindex", "beale")
results <- vector(mode = "list", length = length(indexes))
nc <- vector(length = length(indexes))
for (i in 1:length(indexes)) {
  results[[i]] <- try(NbClust(products_scaled, method = "kmeans", index = indexes[i]))
  nc[i] <- results[[i]]$Best.nc[1]
  print(paste("Index", indexes[i], "done", sep = " "))
}
nc # 8 3 7 2

# Majority rule: 3 and 7

rm("i", "indexes", "nc", "results", "products_dist", "products_hclust", "tve", "clk", "k")


## Products clustering
set.seed(1)
products_kmeans <- kmeans(products_scaled, centers = 3)

# Plot
p <- products %>%
  select(product_id, product_name) %>%
  cbind(products_scaled)
centers <- products_kmeans$centers
clusters <- as.factor(products_kmeans$cluster)
ggplot() + 
  geom_point(data = p, 
             aes(x = percentage_returned, y = percentage_reordered, color = clusters)) + 
  geom_point(data = as.data.frame(products_kmeans$centers), 
             aes(x = percentage_returned, y = percentage_reordered, color = "purple")) +
  geom_point(data = as.data.frame(products_kmeans$centers), 
             aes(x = percentage_returned, y = percentage_reordered, color = "purple"),
             size = 52, alpha = 0.3, show.legend = FALSE)

# Assign corresponding cluster to the products
products <- products %>%
  select(product_id, product_name) %>%
  mutate(product_cluster = paste("product", "cluster", products_kmeans$cluster, sep = "_"))

rm("p", "centers", "clusters", "products_scaled", "products_kmeans")

# Add product clusters info to main dataframe
purchases <- purchases %>%
  left_join(products, by = c("product_id", "product_name"))

rm("products")
