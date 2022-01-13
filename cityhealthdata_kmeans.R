library(tidyverse)
library(cluster)
library(dendextend)
library(factoextra)

# Cityhealth dataset

ch <- read_csv("cityhealth.csv")
chs <- scale(ch[,-1])
rownames(chs) <- ch$city.state

get_clust_tendency(chs, n = 50, gradient = list(low = "red",
                                                mid = "white",
                                                high = "blue"))

fviz_nbclust(chs, kmeans, method = "gap_stat")

km.res <- kmeans(chs, 7, nstart = 25)
windows()
fviz_cluster(km.res, data = chs, frame.type = "convex") +
  theme_minimal()

res.km <- eclust(chs, "kmeans")
fviz_silhouette(res.km, label = FALSE)
sil <- silhouette(res.km$cluster, dist(chs))
sil.sum <- summary(sil)
sil.sum$avg.width

means <- aggregate(ch[-1], by = list(cluster = res.km$cluster), mean)
view(means)









