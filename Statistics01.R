# installo il pacchetto moments per indici di forma
# Installing moments package for the shape indices
install.packages("moments")
install.packages("ggplot2")
install.packages("vctrs")
install.packages("dplyr")

library(moments)
library(ggplot2)
library(dplyr)

# carico il file
# upload file
dataset <- read.csv("realestate_texas.csv")

# media
# mean
mean_sales <- mean(dataset$sales, na.rm = TRUE)
mean_volume <- mean(dataset$volume, na.rm = TRUE)
mean_median_price <- mean(dataset$median_price, na.rm = TRUE)
mean_listings <- mean(dataset$listings, na.rm = TRUE)
mean_months_inventory <- mean(dataset$months_inventory, na.rm = TRUE)


# mediana
# median
median_sales <- median(dataset$sales, na.rm = TRUE)
median_volume <- median(dataset$volume, na.rm = TRUE)
median_median_price <- median(dataset$median_price, na.rm = TRUE)
median_listings <- median(dataset$listings, na.rm = TRUE)
median_months_inventory <- median(dataset$months_inventory, na.rm = TRUE)

# quartile
# quartile
quartiles_sales <- quantile(dataset$sales, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
quartiles_volume <- quantile(dataset$volume, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
quartiles_median_price <- quantile(dataset$median_price, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
quartiles_listings <- quantile(dataset$listings, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
quartiles_months_inventory <- quantile(dataset$months_inventory, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# deviazione standard 
# standard deviation
sd_sales <- sd(dataset$sales, na.rm = TRUE)
sd_volume <- sd(dataset$volume, na.rm = TRUE)
sd_median_price <- sd(dataset$median_price, na.rm = TRUE)
sd_listings <- sd(dataset$listings, na.rm = TRUE)
sd_months_inventory <- sd(dataset$months_inventory, na.rm = TRUE)

# max variabilità maggiore (dev standard), posso usare anche sapply
vector_sd <- c(sales = sd_sales, volume = sd_volume, median_price = sd_median_price, 
               listings = sd_listings, months_inventory = sd_months_inventory)
max_sd_var <- names(which.max(vector_sd))
print(paste("La variabile con la variabilità più alta è: ", max_sd_var," e vale", round(max(vector_sd), 2)))

# coeffciente di variazione
# posso calcolarlo utilizzando la formula per ognuna delle varibili
# qui ho scelto la funzione sapply, come segue
# variation coefficient, using sapply for a rapid function

variables <- c("sales", "volume", "median_price", "listings", "months_inventory")
cv_values <- sapply(variables, function(var) {
  (sd(dataset[[var]], na.rm = TRUE) / mean(dataset[[var]], na.rm = TRUE)) * 100
})

print(cv_values)

# Skewness
skewness_sales <- skewness(dataset$sales, na.rm = TRUE)
skewness_volume <- skewness(dataset$volume, na.rm = TRUE)
skewness_median_price <- skewness(dataset$median_price, na.rm = TRUE)
skewness_listings <- skewness(dataset$listings, na.rm = TRUE)
skewness_months_inventory <- skewness(dataset$months_inventory, na.rm = TRUE)

# Asimmetria, uso sapply
# Asymmetry, by using sapply
skewness_values <- sapply(variables, function(var) skewness(dataset[[var]], na.rm = TRUE))
max_skewness_var <- names(which.max(abs(skewness_values)))

print(paste("The most asymmetric variable is: ", max_skewness_var))

# Curtosi 
kurtosis_sales <- kurtosis(dataset$sales, na.rm = TRUE)
kurtosis_volume <- kurtosis(dataset$volume, na.rm = TRUE)
kurtosis_median_price <- kurtosis(dataset$median_price, na.rm = TRUE)
kurtosis_listings <- kurtosis(dataset$listings, na.rm = TRUE)
kurtosis_months_inventory <- kurtosis(dataset$months_inventory, na.rm = TRUE)

# distribuzione di frequenza
# frequency distribution
table_city <- table(dataset$city)
table_year <- table(dataset$year)
table_months <- table(dataset$months)
print(table_city)
print(table_year)
print(table_months)

# RICHIESTA N. 5
classes <- cut(dataset$median_price, breaks = 7, labels = FALSE, include.lowest = TRUE)
freq_table <- table(classes)
print(freq_table)

freq_df <- as.data.frame(freq_table)
colnames(freq_df) <- c("Class", "Frequency")

ggplot(freq_df, aes(x = as.factor(Class), y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Frequency Distributions per Median Price", x = "Class of Price", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

gini_coefficient <- function(x) {
  x <- sort(x)
  n <- length(x)
  gini <- sum((2 * (1:n) - n - 1) * x) / (n * sum(x))
  return(1 - gini)
}

gini_index <- gini_coefficient(dataset$median_price)
print(paste("Gini index for median_price:", round(gini_index, 3)))

# RICHIESTA N. 6

city_counts <- as.numeric(table(dataset$city))
gini_city <- gini_coefficient(city_counts)
print(paste("Gini index for the city counts:", round(gini_city, 3)))

# RICHIESTA N. 7
prob_beaumont <- sum(dataset$city == "Beaumont") / nrow(dataset)
prob_july <- sum(dataset$month == 7) / nrow(dataset)
prob_dec_2012 <- sum(dataset$month == 12 & dataset$year == 2012) / nrow(dataset)

print(paste("Probability the city is Beaumont:", round(prob_beaumont * 100, 2), "%"))
print(paste("Probability the month is Luglio:", round(prob_july * 100, 2), "%"))
print(paste("Probability that is Dicembre 2012:", round(prob_dec_2012 * 100, 2), "%"))

# RICHIESTA N. 8
dataset$average_price <- (dataset$volume * 1e6) / dataset$sales
head(dataset)


# RICHIESTA N. 9
dataset$efficacy_ratio <- dataset$sales / dataset$listings
head(dataset)
# voglio anche vedere le 3 città con la maggior efficacia degli annunci
# I want to show the top 3 cities considering the effectiveness of the advertising
top3_cities <- dataset %>%
  group_by(city) %>%
  summarise(avg_efficacy = mean(efficacy_ratio, na.rm = TRUE)) %>%
  arrange(-avg_efficacy) %>%
  head(3)

print(top3_cities)

# RICHIESTA 10

# Boxplot per il prezzo mediano tra le varie città
# Boxplot for the median price among the cities
ggplot(dataset, aes(x = city, y = median_price)) +
  geom_boxplot() + # aggiungo il boxplot
  theme_minimal() + # scelgo il tema
  # titoli ed etichette degli assi
  # titles and labels for the axes 
  labs(title = "Cities Median Price Distribution", y = "Median Price", x = "Cities")

# Boxplot del valore totale delle vendite per città e anno
# Boxplot for the total sales - year and city
ggplot(dataset, aes(x = city, y = volume, fill = as.factor(year))) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Total Sales (year and city) Distribution", y = "Total Sales", x = "City")

# Grafico a barre sovrapposte per le vendite nei vari mesi
dataset %>%
  group_by(city, month, year) %>% #raggruppo il dataset
  summarise(total_sales = sum(sales)) %>% # calcolo le vendite totali per ogni gruppo
  ggplot(aes(x = as.factor(month), y = total_sales, fill = city)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~year) +
  theme_minimal() +
  labs(title = "Total Sales (month and city)", y = "Total Sales", x = "Month")

# Creazione di un vettore di colori per le città, ogni città avrà un colore
city_colors <- unique(dataset$city)
colors <- scales::hue_pal()(length(city_colors))
color_mapping <- setNames(colors, city_colors)

# Line chart per il prezzo mediano nel tempo per ciascuna città
# Line chart for the median price for the cities, along the year
dataset$date <- as.Date(paste(dataset$year, dataset$month, "01", sep = "-"))
dataset %>%
  group_by(city, date) %>%
  summarise(avg_median_price = mean(median_price, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = avg_median_price, color = city)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Median Price Evolution", y = "Median Price", x = "Date") +
  scale_color_manual(values = color_mapping)
head(dataset)


