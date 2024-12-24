#installpackages
install.packages("arules")
install.packages("arulesViz")

#load packages
library(arules)
library(arulesViz) 

# Import the dataset
RT
#i used the "Import Dataset" button in "Global Environment"

#investigate and review data
class(RT)
View(RT)

RT[1:10] <- lapply(RT[1:10], factor)
transactions <- as(RT, "transactions")
arules::summary(transactions)
class(transactions)
inspect(transactions)
inspect(head(transactions))
inspect(tail(transactions))
size(head(transactions))        
size(tail(transactions))
LIST(head(transactions))
LIST(tail(transactions))

frequent.items <- eclat(transactions)
frequent.items <- eclat(transactions, parameter = list(support=.05))
inspect(head(frequent.items))
inspect(tail(frequent.items))
frequent.items.count <- sort(frequent.items, by = "count")
inspect(frequent.items.count)
frequent.items.support <- sort(frequent.items, by = "support", decreasing = FALSE)
inspect(head(frequent.items.support,3)) 
inspect(tail(frequent.items.support,3)) 


#visualize the data
itemFrequencyPlot(transactions) 
itemFrequencyPlot(transactions, topN = 10)
itemFrequencyPlot(transactions, topN=10, type = "absolute") 

# generate association rules
rules <- apriori(transactions, parameter = list(support=.1, confidence=.4))
rules 
rules <- apriori(transactions, parameter = list(support=.01, confidence=.4))
rules 
inspect(rules)

rules.support <- sort(rules, by="support")
inspect(rules.support)
inspect(head(rules.support))
rules.count <- sort(rules, by="count")
inspect(rules.count)
inspect(head(rules.count))
rules.confidence <- sort(rules, by="confidence")
inspect(rules.confidence)
inspect(head(rules.confidence))

plot(rules)
plot(rules, method = 'graph')

#target specific itemsets in the rules
burger_rules <- subset(rules, subset = lhs %in% "Burger=1" | rhs %in% "Burger=1",
                       confidence >.55)
inspect(sort(burger_rules, by = "support"))

Burger_1 <- "Burger=1"
rules.Burger_1.rhs <- apriori(transactions, parameter = list(support=.01, confidence=.4),
                         appearance = list(default = "lhs", rhs='Burger=1'))
inspect(sort(rules.Burger_1.rhs, by = "support"))

plot(rules.Burger_1.rhs, method = 'graph')




###Clustering Analysis###




#install packages
install.packages("cluster")
install.packages("factoextra")
install.packages("pastecs")
install.packages("clustMixType")

library(cluster)
library(factoextra)
library(pastecs)
library(clustMixType)

#import data into R and investigate it
data("votes.repub")
v = votes.repub

#preprocessing
sum(is.na(v))
v = na.omit(v)

#investigate the data
names(v)
summary(v)
str(v)
pastecs::stat.desc(v)
lapply(v, sd)
lapply(v, mean)
lapply(v, range)


#visualize data distances
dist = factoextra::get_dist(v, method = 'pearson')
fviz_dist(dist)

#determine the best k
fviz_nbclust(v, kmeans, method = 'wss')

find kmeans clusters ####

#k4
set.seed(12)
k4 = kmeans(v, centers = 4)
k4$tot.withinss
k4$size

#k5
set.seed(12)
k5 = kmeans(v, centers = 5)
k5$tot.withinss
k5$size

#k6
set.seed(12)
k6 = kmeans(v, centers = 6)
k6$tot.withinss
k6$size

#compare the total wss for k 4, 5, and 6
k4$tot.withinss
k5$tot.withinss
k6$tot.withinss

#4) visualize the kmeans clusters ####
fviz_cluster(k5, data = v)
clusplot(v, k5$cluster)

#5) learn about each cluster and its attributes' values
c1 = v[k5$cluster==1,]
c2 = v[k5$cluster==2,]
c3 = v[k5$cluster==3,]
c4 = v[k5$cluster==4,]
c5 = v[k5$cluster==5,]


#hierarchical clustering
v.single = agnes(v, method = 'single')
v.complete = agnes(v, method = 'complete')
v.average = agnes(v, method = 'average')

#compare their ACs
v.single$ac
v.complete$ac
v.average$ac


#visualize the hierarchical clustering (dendrograms)
plot(v.complete)
