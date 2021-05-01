# read in data
cereals.df <- read.csv("C:/Users/valay/OneDrive/Desktop/Ivey/Winter Term 2021/Big Data Analytics/Class 8/Assignment/Cereals.csv")

#remove any data that is na or blank

cereals.df = cereals.df[-which(is.na(cereals.df$carbo)),]
cereals.df = cereals.df[-which(is.na(cereals.df$potass)),]
cereals.df

# Make row names the names of the utilities, remove numerical labeling of rows
row.names(cereals.df) <- cereals.df[,1]

#Before we can normalize the variables, all variables must have only numeric values
#So we can remove the first two columns since they are not numerical
#the minus 1 removes a column in the cereals file
cereals.df <- cereals.df[,-1]
cereals.df
#removes mfr column
cereals.df <- cereals.df[,-1]
cereals.df
#removes type column
cereals.df <- cereals.df[,-1]
cereals.df


# normalize input variables
# sapply changes everything to standardized numbers
cereals.df.norm <- sapply(cereals.df, scale)

# add row names to cereals.df
row.names(cereals.df.norm) <- row.names(cereals.df)
#you can only run the above line once; can't put row names once row names have already been established

# compute normalized distance based on all variables
d.norm <- dist(cereals.df.norm, method = "euclidean")



# in hclust() set argument method =
# to "ward.D", "single", "complete", "average", "median", or "centroid"

# plot an empty scatter plot
km<-kmeans(cereals.df.norm, 6)
#the 6 means 6 clusters

plot(c(0), xaxt = 'n', ylab = "", type = "l", ylim = 
       c(min(km$centers), max(km$centers)), xlim = c(0, 8))

# label x-axes
axis(1, at = c(1:13), labels = names(cereals.df))

# plot centroids
for (i in c(1:6))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),"black", "dark grey"))

# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:6)))

#averages within clusters for each variable
km$centers

#distance between points within clusters
km$withinss

#how many data points are within each cluster is the size of the cluster
km$size

#1-6 clusters
#wss is within sum of squares meaning the distance between points within each cluster
#the higher the number of clusters, the lower the variance b/w points within clusters
wss <- numeric(6)

#mean of distance between points within a cluster vs number of clusters
for (k in 1:6) wss[k] <- mean(kmeans(cereals.df.norm, centers = k, nstart = 25)$withinss)
plot(1:6, wss, type="b",xlab="Number of Clusters", ylab="Within Sum of Squares")




#SINGLE CLUSTER
hc1 <- hclust(d.norm, method = "single")
hc1

plot(hc1, hang = -1, ann = FALSE)

#the graph (dendrogram) shows us the shortest distance NY is to anything else is 3.6
#closest Idaho and Puget are to anything else is 2.3

cutree(hc1,k=6)
#this shows just 6 clusters; number of clusters is 6 clusters

cutree (hc1, k=10)
#this shows 10 clusters

#h will be cut the tree based on a number
#if h is 3 then it clusters everything below 3 as one cluster and for everything above 3 will have their
#own seperate cluster





#COMPLETE CLUSTER
hc2 <- hclust(d.norm, method = "complete")
hc2

plot(hc2, hang = -1, ann = FALSE)

#the graph (dendrogram) shows us the shortest distance NY is to anything else is 3.6
#closest Idaho and Puget are to anything else is 2.3

cutree(hc2,k=2)
#this shows just 2 clusters; number of clusters is 2 clusters

cutree (hc2, k=5)
#this shows 6 clusters

#h will be cut the tree based on a number
#if h is 3 then it clusters everything below 3 as one cluster and for everything above 3 will have their
#own seperate cluster
