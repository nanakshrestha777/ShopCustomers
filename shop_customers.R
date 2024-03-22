library(tidyverse)
library(GGally)
library(dplyr)
library(tidyr)
library(magrittr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(cluster)
library(factoextra)
library(plotly)
theme_set(theme_minimal())

customers_data <- read.csv("Customers.csv")
head(customers_data)

str(customers_data)
summary(customers_data)

customers_data <- na.omit(customers_data)
cat("number of data after removing the rows with missing values are:", nrow(customers_data), "\n")

summary(customer_data)

customer_data <- customer_data[customer_data$Profession != "", , drop = FALSE]
profession_counts <- table(customer_data$Profession)
profession_percentages <- prop.table(profession_counts) * 100
print(profession_percentages)


filtered_data <- subset(customers_data, Age >= 18)
filtered_data <- customers_data[customers_data$Age >= 18, ]
view(filtered_data)

plot_ly(customer_data,
  x = ~Spending.Score..1.100.,
  y = ~Annual.Income....,
  z = ~Age,
  color = ~Age,
  marker = list(size = 5, opacity = 0.7)
) %>%
  layout(scene = list(
    xaxis = list(title = "Spending Score"),
    yaxis = list(title = "Annual Income"),
    zaxis = list(title = "Age")
  )) %>%
  add_markers() %>%
  layout(title = "High-Value Customers")


customers_data$Gender <- factor(customers_data$Gender)
gender_distribution <- customers_data %>%
  group_by(Gender) %>%
  summarise(count = n())
gender_color <- c("Male" = "dodgerblue", "Female" = "deeppink")
ggplot(gender_distribution, aes(x = Gender, y = count, fill = Gender)) +
  geom_col(color = "black", width = 0.7) +
  scale_fill_manual(values = gender_color) +
  labs(title = "Customers by Gender") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1),
    plot.background = element_rect(fill = "lightgray")
  )

customers_data <- customers_data %>%
  filter(Age > 0)
customers_data$Age[customers_data$Age == 0] <- mean(customers_data$Age, na.rm = TRUE)

customers_data <- customers_data[!(customers_data$Profession %in% c("Unknown", "")), ]

common_professions <- customers_data %>%
  group_by(Profession) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
ggplot(head(common_professions, 10), aes(x = reorder(Profession, -count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Most Common Professions", x = "Profession", y = "Count")
ggplot(customer_data, aes(x = Profession, y = Spending.Score..1.100., fill = Profession)) +
  geom_boxplot() +
  ggtitle("Profession vs. Spending Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(customers_data, aes(x = Profession, y = Annual.Income....)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Income Distribution by Profession", x = "Profession", y = "Annual Income") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



customers_data$SpendingSegment <- cut(customers_data$Spending.Score..1.100.,
  breaks = c(-Inf, 20, 80, Inf),
  labels = c("Low-Spending", "Medium-Spending", "High-Spending"), include.lowest = TRUE, right = FALSE
)

ggplot(customers_data, aes(x = SpendingSegment, fill = SpendingSegment)) +
  geom_bar(position = "dodge") +
  labs(title = "Segments on the basis of customers spending", x = "Spending Segment", y = "Count") +
  scale_fill_manual(values = c("Low-Spending" = "blue", "Medium-Spending" = "green", "High-Spending" = "red")) +
  theme_minimal() +
  facet_wrap(~Gender, scales = "free")


average_spending_by_gender <- customers_data %>%
  group_by(Gender) %>%
  summarize(AverageSpending = mean(Spending.Score..1.100., na.rm = TRUE))

ggplot(average_spending_by_gender, aes(x = Gender, y = AverageSpending, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Spending Score by Gender", x = "Gender", y = "Average Spending Score") +
  theme_minimal()


plot_ly(customer_data, x = ~Age, y = ~Profession, color = ~Profession, type = "box") %>%
  layout(title = "Age Distribution across Professions")


wcss_values <- numeric()

for (i in 1:10) {
  kmeans_model <- kmeans(scale(clustering_data), centers = i, nstart = 25)
  wcss_values[i] <- kmeans_model$tot.withinss
}


plot(1:10, wcss_values,
  type = "b", pch = 19, frame = FALSE, col = "blue",
  xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares (WCSS)",
  main = "Elbow Method for Optimal k"
)

diff_wcss <- diff(wcss_values)
optimal_k <- which(diff_wcss == min(diff_wcss[-1])) + 1


cat("Optimal number of clusters (k):", optimal_k, "\n")



clustering_data <- customers_data %>%
  select(Annual.Income...., Spending.Score..1.100.)
scaled_data <- scale(clustering_data)

k <- 3
kmeans_model <- kmeans(scaled_data, centers = k, nstart = 25)
customers_data$Cluster <- kmeans_model$cluster
cat("Cluster Assignments:\n")
cat("CustomerID\tCluster\n")
cat(paste(customers_data$CustomerID, customers_data$Cluster, sep = "\t"), "\n")
ggplot(customers_data, aes(x = Annual.Income...., y = Spending.Score..1.100., color = factor(Cluster))) +
  geom_point() +
  labs(
    title = "Customer Segmentation",
    x = "Annual Income",
    y = "Spending Score"
  ) +
  theme_minimal()


cat("Cluster Assignments:\n")
cat("CustomerID\tCluster\n")
cat(paste(customers_data$CustomerID, customers_data$Cluster, sep = "\t"), "\n")

cat("\nCount of Customers in Each Cluster:\n")
print(table(customers_data$Cluster))



ggplot(customers_data, aes(x = factor(Cluster))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Customers in Each Cluster",
    x = "Cluster",
    y = "Number of Customers"
  ) +
  theme_minimal()

customers_data$SpendingSegment <- cut(customers_data$Spending.Score..1.100.,
  breaks = c(0, 33, 66, 100),
  labels = c("Low-Spending", "Medium-Spending", "High-Spending")
)

segment_counts <- table(customers_data$Cluster, customers_data$SpendingSegment)
print("Distribution of Spending Segments within Clusters:")
print(segment_counts)

ggplot(customers_data, aes(x = factor(Cluster), fill = SpendingSegment)) +
  geom_bar(position = "stack") +
  labs(
    title = "Distribution of Spending Segments within Clusters",
    x = "Cluster",
    y = "Number of Customers",
    fill = "Spending Segment"
  ) +
  theme_minimal()

colnames(customers_data)

colors <- c("cornflowerblue", "chartreuse3", "mediumorchid2", "slategray2")

ggplot(customers_data, aes(x = Cluster, fill = cut(Annual.Income...., breaks = c(0, 50000, 100000, 200000, Inf), labels = c("Low", "Medium", "High", "Very High")))) +
  geom_bar(position = "stack", color = "white", size = 1) +
  scale_fill_manual(values = colors) +
  labs(title = "Customers by Annual Income from Clusters ", fill = "Annual Income") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


ggplot(customers_data, aes(x = factor(Cluster), fill = Profession)) +
  geom_bar(position = "stack") +
  labs(
    title = "Distribution of Professions in Each Cluster",
    x = "Cluster",
    y = "Number of Customers",
    fill = "Profession"
  ) +
  theme_minimal()
