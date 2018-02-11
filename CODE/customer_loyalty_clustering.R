#install.packages("caret")
#install.packages("NbClust")
library(NbClust)
library(caret)

str(shpInfo)

id.frequency <- as.data.frame(table(shpInfo$ID))

colnames(id.frequency) <- c('ID', 'freq')
str(id.frequency)

customer.loyalty <- merge(customerRating, id.frequency, by = 'ID')
str(customer.loyalty)
customer.loyalty[, 2:3] <- scale(customer.loyalty[, 2:3])

summary(customer.loyalty)





customer.loyalty.kmeans <- kmeans(customer.loyalty[-1], centers = 4)
table(customer.loyalty.kmeans$cluster)
customer.loyalty$cluster <- as.factor(customer.loyalty.kmeans$cluster)
qplot(BUY, freq, colour = cluster, data = customer.loyalty)



hc <- hclust(dist(customer.loyalty[2:3]), method = "ave")
plot(hc, hang = -1, labels = customer.loyalty$ID)
