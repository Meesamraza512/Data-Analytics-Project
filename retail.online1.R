
# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Load the Excel file (update the file path if needed)
data=read_excel(file.choose())

# Display first few rows of the dataset
head(data)


# check the data structure
str(data)

# check and count the missing value or NA value in dataset
sum(is.na(data))



# drop the NA or Missing value
data=na.omit(data)


# show the dataset in table
View(data)

# 1. Revenue Analysis (Calculate revenue per product and find top products)
data <- data %>%
  mutate(Revenue = Quantity * UnitPrice) # Add Revenue column
data

# top 10 products by revenue
top_products <- data %>%
  group_by(Description) %>%
  summarise(Total_Revenue = sum(Revenue, na.rm = TRUE)) %>%
  arrange(desc(Total_Revenue)) %>%
  head(10)

print("Top 10 products by revenue:")
print(top_products)

# 2. Geographic Insights (Total sales by country)
sales_by_country <- data %>%
  group_by(Country) %>%
  summarise(Total_Sales = sum(Revenue, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))

print("Sales by country:")
print(sales_by_country)

# 3. Customer Segmentation (Total revenue by customer)
customer_revenue <- data %>%
  group_by(CustomerID) %>%
  summarise(Total_Revenue = sum(Revenue, na.rm = TRUE)) %>%
  arrange(desc(Total_Revenue))

print("Top customers by revenue:")
print(customer_revenue)

# 4. Inventory Management (Top-selling products by quantity)
top_selling_products <- data %>%
  group_by(Description) %>%
  summarise(Total_Quantity = sum(Quantity, na.rm = TRUE)) %>%
  arrange(desc(Total_Quantity)) %>%
  head(10)

print("Top 10 products by quantity sold:")
print(top_selling_products)



# 5. Top-Selling Products Visualization
ggplot(top_products, aes(x = reorder(Description, -Total_Revenue), y = Total_Revenue)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Products by Revenue", x = "Products", y = "Revenue") +
  theme_minimal()

# 6. Seasonality Trends (Sales over time)
sales_over_time <- data %>%
  mutate(InvoiceDate = as.Date(InvoiceDate)) %>%
  group_by(InvoiceDate) %>%
  summarise(Daily_Revenue = sum(Revenue, na.rm = TRUE))

ggplot(sales_over_time, aes(x = InvoiceDate, y = Daily_Revenue)) +
  geom_line(color = "blue") +
  labs(title = "Sales Trends Over Time", x = "Date", y = "Daily Revenue") +
  theme_minimal()

# 7. Customer Behavior (High-value customers)
high_value_customers <- customer_revenue %>%
  filter(Total_Revenue > quantile(customer_revenue$Total_Revenue, 0.9))

print("High-value customers:")
print(high_value_customers)

# 8. Market Expansion (Country sales distribution)
ggplot(sales_by_country, aes(x = reorder(Country, -Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Sales by Country", x = "Country", y = "Total Sales") +
  theme_minimal()
