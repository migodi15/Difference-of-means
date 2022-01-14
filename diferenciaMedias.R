library(nortest)
library(ggplot2)
library(dplyr)

#### Dataset
data <- read.csv("ChildCarSeats_clean.csv", stringsAsFactors = TRUE)
str(data)
summary(data)

#### Shops in USA vs. outside USA
sales_in_US <- data %>% filter(US == "Yes") %>% pull(Sales)
sales_out_US <- data %>% filter(US == "No") %>% pull(Sales)

# Means
sales_mean <- data.frame(Mean = c(mean(sales_in_US), mean(sales_out_US)))
sales_mean$Case <- c("En USA", "Fuera de USA")
ggplot(data = sales_mean, aes(x = Case, y = Mean, label = round(Mean, 2))) +
  geom_bar(stat = "identity", width = 0.5, fill = c("darksalmon", "burlywood4")) +
  geom_text(vjust = 2, colour = "white") +
  labs(title = "Medias de ventas", x = "Caso", y = "Media") +
  theme_bw()

# Histogram
par(mfrow = c(1,2))
hist(sales_in_US, main = "Ventas en USA", col = "darksalmon", 
     xlab = "Ventas", ylab = "Frecuencia", xlim = c(0,20), ylim = c(0, 60),
     border = "white")
hist(sales_out_US, main = "Ventas fuera de USA", col = "burlywood4", 
     xlab = "Ventas", ylab = "Frecuencia", xlim = c(0,20), ylim = c(0, 60),
     border = "white")

# Shapiro test to check normality
shapiro.test(sales_in_US) 
shapiro.test(sales_out_US)

# Comparing the variances
var.test(sales_in_US, sales_out_US)

# Student's t-Test
t.test(sales_in_US, sales_out_US, paired=FALSE, var.equal = FALSE)

#### Rural vs. urban zones
sales_rural <- data %>% filter(Urban == "No") %>% pull(Sales)
sales_urban <- data %>% filter(Urban == "Yes") %>% pull(Sales)

# Means
sales_mean <- data.frame(Mean = c(mean(sales_urban), mean(sales_rural)))
sales_mean$Case <- c("Zona urbana", "Zona rural")
ggplot(data = sales_mean, aes(x = Case, y = Mean, label = round(Mean, 2))) +
  geom_bar(stat = "identity", width = 0.5, fill = c("azure4", "deepskyblue3")) +
  geom_text(vjust = 2, colour = "white") +
  labs(title = "Medias de ventas", x = "Caso", y = "Media") +
  theme_bw()

# Histogram
par(mfrow = c(1,2))
hist(sales_rural, main = "Ventas en zona rural", col = "deepskyblue3", 
     xlab = "Ventas", ylab = "Frecuencia", xlim = c(0,20), ylim = c(0, 80),
     border = "white")
hist(sales_urban, main = "Ventas en zona urbana", col = "azure4", 
     xlab = "Ventas", ylab = "Frecuencia", xlim = c(0,20), ylim = c(0, 80),
     border = "white")

# Shapiro test to check normality
shapiro.test(sales_rural)
shapiro.test(sales_urban) 

# Comparing the variances
var.test(sales_rural, sales_urban)

# Student's t-Test
t.test(sales_urban, sales_rural, paired=FALSE, var.equal = TRUE)

#### Good, medium and bad shelves
sales_good_shelve <- data %>% filter(ShelveLoc == "Good") %>% pull(Sales)
sales_medium_shelve <- data %>% filter(ShelveLoc == "Medium") %>% pull(Sales)
sales_bad_shelve <- data %>% filter(ShelveLoc == "Bad") %>% pull(Sales)

# Means
sales_mean <- data.frame(Mean = c(mean(sales_good_shelve), mean(sales_medium_shelve),
                                  mean(sales_bad_shelve)))
sales_mean$Case <- c("Estantería buena", "Estantería media", "Estantería mala")
ggplot(data = sales_mean, aes(x = reorder(Case, -Mean), y = Mean, 
                              label = round(Mean, 2))) +
  geom_bar(stat = "identity", width = 0.5, fill = c("darkgreen", "orange", "red")) +
  geom_text(vjust = 2, colour = "white") +
  labs(title = "Medias de ventas", x = "Caso", y = "Media") +
  theme_bw()

# Histogram
par(mfrow = c(1,3))
hist(sales_good_shelve, main = "Ventas en estantería buena", col = "darkgreen", 
     xlab = "Ventas", ylab = "Frecuencia", xlim = c(0,20), ylim = c(0, 70),
     border = "white")
hist(sales_medium_shelve, main = "Ventas en estantería media", col = "orange", 
     xlab = "Ventas", ylab = "Frecuencia", xlim = c(0,20), ylim = c(0, 70),
     border = "white")
hist(sales_bad_shelve, main = "Ventas en estantería mala", col = "red", 
     xlab = "Ventas", ylab = "Frecuencia", xlim = c(0,20), ylim = c(0, 70),
     border = "white")

# Shapiro test to check normality
shapiro.test(sales_good_shelve) 
shapiro.test(sales_medium_shelve)
shapiro.test(sales_bad_shelve)

# Comparing the variances
bartlett.test(Sales ~ ShelveLoc, data)

# ANOVA test
result <- aov(Sales ~ ShelveLoc, data)
summary(result)