#libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(dbscan)
#read the file
CustomerSegmentation <- read.csv("customerSegmentation.csv")
#-------------------------YUMNA-------------------------#
summary(CustomerSegmentation)
head(CustomerSegmentation)
#rename the columns
CustomerSegmentation <- rename(CustomerSegmentation, c("Annual Income" = "Annual.Income..k.."))
CustomerSegmentation <- rename(CustomerSegmentation, c("Spending Score" = "Spending.Score..1.100."))
#remove duplicates
unique(CustomerSegmentation$CustomerID)
#replace the null values in the columns with the median 
CustomerSegmentation <- CustomerSegmentation %>% mutate(Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age))
CustomerSegmentation <- CustomerSegmentation %>% mutate(`Annual Income` = ifelse(is.na(`Annual Income`), median(`Annual Income`, na.rm = TRUE), `Annual Income`))
CustomerSegmentation <- CustomerSegmentation %>% mutate(`Spending Score` = ifelse(is.na(`Spending Score`), median(`Spending Score`, na.rm = TRUE), `Spending Score`))
#replace the zeroes and the negative values in the columns with the median
CustomerSegmentation <- CustomerSegmentation %>% mutate(Age = ifelse(Age <= 0, median(Age[Age != 0]), Age))
CustomerSegmentation <- CustomerSegmentation %>% mutate(`Annual Income` = ifelse(`Annual Income` <= 0, median(`Annual Income`[`Annual Income` != 0]), `Annual Income`))
CustomerSegmentation <- CustomerSegmentation %>% mutate(`Spending Score` = ifelse(`Spending Score` <= 0, median(`Spending Score`, na.rm = TRUE), `Spending Score`))
corr0<- cor(CustomerSegmentation$`Spending Score`,CustomerSegmentation$`Annual Income`)
summary(CustomerSegmentation)
corr0 #Very weak positive or no association
corr1<- cor(CustomerSegmentation$`Spending Score`,CustomerSegmentation$Age)
corr1 #Weak negative association
corr2<- cor(CustomerSegmentation$Age,CustomerSegmentation$`Annual Income`)
corr2 #Very weak positive or no association
#-------------------------RAWAN-------------------------#
#Pie chart for gender
ggplot(CustomerSegmentation, aes(x = "", fill = Gender)) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Gender Distribution")
#Histogram for age
hist(CustomerSegmentation$Age, col = "skyblue",main = "Age Distribution",xlab = "Age",ylab = "Frequency",border = "white")
#Boxplot for annual income
boxplot(CustomerSegmentation$`Annual Income`,col = "lightblue",main = "Annual Income Box Plot", ylab = "Annual Income",border = "black")
#Boxplot for spending salary
boxplot(CustomerSegmentation$`Spending Score`,col = "lightblue",main = "Spending Score Box Plot", ylab = "Spending Score",border = "black")
#Grouped bar chart for gender and age
ggplot(CustomerSegmentation, aes(x = Gender, y = Age, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Grouped Bar Chart of Age by Gender", x = "Gender", y = "Age") +
  theme_minimal()
#-------------------------MAI-------------------------#
#Bar chart for annual income by gender
ggplot(CustomerSegmentation, aes(x =Gender, y = `Annual Income`, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  labs(title = "Annual Income by Gender", x = "Gender", y = "Annual Income") +
  theme_minimal()
#Bar chart for spending score by gender
ggplot(CustomerSegmentation, aes(x =Gender, y = `Spending Score`, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  labs(title = "Annual Income by Gender", x = "Gender", y = "Annual Income") +
  theme_minimal()
#Scatter plot between age and annual income
ggplot(CustomerSegmentation, aes(x = Age, y = `Annual Income`)) +
  geom_point() +
  labs(title = "Scatter Plot of Annual Salary vs. Age", x = "Age", y = "Annual Salary") +
  theme_minimal()
#Scatter plot between age and spending score
ggplot(CustomerSegmentation, aes(x = Age, y = `Spending Score`)) +
  geom_point() +
  labs(title = "Scatter Plot of Spending Score vs. Age", x = "Age", y = "Spending Score") +
  theme_minimal()
#Scatter plot between annual income and spending score
ggplot(CustomerSegmentation, aes(x = `Annual Income`, y = `Spending Score`)) +
  geom_point() +
  labs(title = "Scatter Plot of Annual Income vs. Spending Score", x = "Annual Income", y = "Spending Score") +
  theme_minimal()
#-------------------------NOUR--------------------------#
# k-means
km <- CustomerSegmentation [c("CustomerID","Spending Score")] # variable to hold k-means columns
View(km)
# Cluster identification for each observation
wss=numeric(16)   
for(i in 1:16) wss[i]=sum(kmeans(km,i)$withinss)
plot(1:16,wss,type='b',xlab='number of cluster',ylab='within grp sum square')
k<- kmeans(km, centers = 3, nstart = 20)
k
k$cluster
k$totss
#-------------------------AHMED--------------------------#
#DbScan
Dbscan <- dbscan(km, eps = 1, MinPts = 3)
Dbscan$minPts
plot(km ,col=Dbscan$cluster)
Dbscan
#clusters matrix
Dbscan$cluster
dbscan::kNNdistplot(km, k =  5)
abline(h = 14, lty = 3)
abline(h = 19, lty = 3)
#Hierarchical Clustering
clusters <- hclust(dist(km), method = 'average')
plot(clusters)
coph_coef <- cophenetic(clusters)
print(cor(coph_coef, as.dist(dist(data_matrix))))

