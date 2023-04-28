library(dplyr)
dataset <- read.table('dataset2.csv', header = T, sep = ";", dec = ",")

#####Values for analysis
data1 <- subset(dataset[, -c(2,17,18,19,22,23,25,26,27,29,30,32,33,34)])

#####Replacing NAs#####
data1[is.na(data1)] = 0
data1[,c(26,27)] <- na_if((data1[,c(26,27)]), 0)

#####Adding NPI sum value#####
data1$NPI.total <- rowSums(data1[,4:15])

#####Transforming value format#####
data1[,2] <- as.factor(data1[,2])
data1[,16] <- as.factor(data1[,16])
data1[,17] <- as.factor(data1[,17])
data1[,20] <- as.factor(data1[,20])

#####Subsetting final data#####
final.data <- data1[,c(2,16,17,19,20,21,22,23,24,25,26,27,28)]
names(final.data) <- c("VARIANT", "SEX", "HERED", "AGE", "DEM", "LE", "a1PI", 
                       "PII", "S100", "OBM", "CRP", "IL", "NPI")

#####Basic statistics#####
summary(final.data)
mean(final.data$LE, trim = 0.1)

#####Data visualization#####
hist(final.data$LE)
hist(final.data$LE, breaks = 18)

hist(final.data$a1PI)
hist(final.data$a1PI, breaks = 18)
hist(final.data$a1PI, breaks = 36)

hist(final.data$AGE)
hist(final.data$AGE, breaks = 18)

hist(final.data$S100)
hist(final.data$S100, breaks = 18)

hist(final.data$OBM)
hist(final.data$OBM, breaks = 18)
hist(final.data$OBM, breaks = 36)

barplot(final.data$LE, beside = T, legend.text = T, ylim = c(0,300),
        ylab = "нмоль/мин*мл",main = 'Лейкоцитарная эластаза')
barplot(final.data$a1PI, beside = T, legend.text = T, ylim = c(0,80),
        ylab = "ИЕ/мл",main = 'Альфа-1 ПИ')
barplot(final.data$S100, beside = T, legend.text = T, ylim = c(0,1.5),
        ylab = "ЕОП",main = 'S100')
barplot(final.data$OBM, beside = T, legend.text = T, ylim = c(0,1.5),
        ylab = "ЕОП",main = 'ОБМ')


#####By FTD variants#####
plot(final.data$LE, final.data$a1PI, ylab="Альфа-1 ПИ", xlab = "ЛЭ", type = "n",
     main = "Активность ЛЭ и ПИ")
points(final.data$LE[final.data$VARIANT==1],
       final.data$a1PI[final.data$VARIANT==1], pch=0)
points(final.data$LE[final.data$VARIANT==2],
       final.data$a1PI[final.data$VARIANT==2], pch=1)

plot(final.data$S100, final.data$OBM, ylab="ОБМ", xlab = "S100-b", type = "n",
     main = "Антитела")
points(final.data$S100[final.data$VARIANT==1],
       final.data$OBM[final.data$VARIANT==1], pch=0)
points(final.data$S100[final.data$VARIANT==2],
       final.data$OBM[final.data$VARIANT==2], pch=1)

#####By dementia stage#####
plot(final.data$LE, final.data$a1PI, ylab="Альфа-1 ПИ", xlab = "ЛЭ", type = "n",
     main = "Активность ЛЭ и ПИ")
points(final.data$LE[final.data$DEM==1],
       final.data$a1PI[final.data$DEM==1], pch=0)
points(final.data$LE[final.data$DEM==2],
       final.data$a1PI[final.data$DEM==2], pch=1)
points(final.data$LE[final.data$DEM==3],
       final.data$a1PI[final.data$DEM==3], pch=2)

plot(final.data$S100, final.data$OBM, ylab="ОБМ", xlab = "S100-b", type = "n",
     main = "Антитела")
points(final.data$S100[final.data$DEM==1],
       final.data$OBM[final.data$DEM==1], pch=0)
points(final.data$S100[final.data$DEM==2],
       final.data$OBM[final.data$DEM==2], pch=1)
points(final.data$S100[final.data$DEM==3],
       final.data$OBM[final.data$DEM==3], pch=2)



#####Preparing data for cluster analysis with CRP & IL-6#####

final.nona <- na.omit(final.data)

maxs.l <- apply(final.nona[6], 2, max)
mins.l <- apply(final.nona[6], 2, min)
standart.data <- scale(final.nona[6], center = mins.l, scale = maxs.l - mins.l)
standart.data <- as.data.frame(standart.data)
maxs.p <- apply(final.nona[7], 2, max)
mins.p <- apply(final.nona[7], 2, min)
standart.data$PE <- scale(final.nona[7], center = mins.p, 
                          scale = maxs.p - mins.p)
maxs.s <- apply(final.nona[9], 2, max)
mins.s <- apply(final.nona[9], 2, min)
standart.data$s100 <- scale(final.nona[9], center = mins.s, 
                            scale = maxs.s - mins.s)
maxs.o <- apply(final.nona[10], 2, max)
mins.o <- apply(final.nona[10], 2, min)
standart.data$OBM <- scale(final.nona[10], center = mins.o, 
                           scale = maxs.o - mins.o)
maxs.c <- apply(final.nona[11], 2, max)
mins.c <- apply(final.nona[11], 2, min)
standart.data$CRP <- scale(final.nona[11], center = mins.c, 
                           scale = maxs.c - mins.c)
maxs.i <- apply(final.nona[12], 2, max)
mins.i <- apply(final.nona[12], 2, min)
standart.data$IL <- scale(final.nona[12], center = mins.i, 
                           scale = maxs.i - mins.i)

dist.data <- dist(standart.data)

clust.data <- hclust(dist.data, "ward.D")

plot(clust.data, cex = 0.5, axes = F, las = 3, 
     main='Кластерная дендрограмма \n активности иммунологических
     показателей c ЦРБ и ИЛ-6', 
     xlab = '', ylab = '')
rect.hclust(clust.data, k=2, border="red") 

plot(clust.data$height, type = 'b')

groups.1 <- cutree(clust.data, k=2)

final.nona$Cluster <- groups.1


#####Preparing data for cluster analysis w/o CRP & IL-6#####
final.data.less <- subset(final.data[,-c(11,12)])
final.nona.less <- na.omit(final.data.less)

maxs.le <- apply(final.nona.less[6], 2, max)
mins.le <- apply(final.nona.less[6], 2, min)
standart.data.less <- scale(final.nona.less[6], center = mins.le, scale = maxs.le - mins.le)
standart.data.less <- as.data.frame(standart.data.less)
maxs.pi <- apply(final.nona.less[7], 2, max)
mins.pi <- apply(final.nona.less[7], 2, min)
standart.data.less$PE <- scale(final.nona.less[7], center = mins.pi, 
                          scale = maxs.pi - mins.pi)
maxs.s1 <- apply(final.nona.less[9], 2, max)
mins.s1 <- apply(final.nona.less[9], 2, min)
standart.data.less$s100 <- scale(final.nona.less[9], center = mins.s1, 
                            scale = maxs.s1 - mins.s1)
maxs.ob <- apply(final.nona.less[10], 2, max)
mins.ob <- apply(final.nona.less[10], 2, min)
standart.data.less$OBM <- scale(final.nona.less[10], center = mins.ob, 
                           scale = maxs.ob - mins.ob)

dist.data.2 <- dist(standart.data.less)

clust.data.2 <- hclust(dist.data.2, "ward.D")

plot(clust.data.2, cex = 0.5, axes = F, las = 3, 
     main='Кластерная дендрограмма \n активности иммунологических
     показателей без ЦРБ и ИЛ-6', 
     xlab = '', ylab = '')
rect.hclust(clust.data.2, k=3, border="red") 

plot(clust.data.2$height, type = 'b')

groups.2 <- cutree(clust.data.2, k=3)

final.nona.less$Cluster <- groups.2

#####Single linkage method#####
clust.data.3 <- hclust(dist.data.2, "single")

plot(clust.data.3, cex = 0.5, axes = F, las = 3, 
     main='Кластерная дендрограмма (б.с.) \n активности иммунологических
     показателей без ЦРБ и ИЛ-6', 
     xlab = '', ylab = '', hang = -1)
rect.hclust(clust.data.3, k=3, border="red") 

plot(clust.data.3$height, type = 'b')


#####Cluster interpretation#####

data1$Cluster <- groups.2
data1$Cluster <- as.factor(data1$Cluster)

colMeans(data1[data1$Cluster=='1',c(3,18,19,21,22,24,25,28)])
colMeans(data1[data1$Cluster=='2',c(3,18,19,21,22,24,25,28)])
colMeans(data1[data1$Cluster=='3',c(3,18,19,21,22,24,25,28)])


0median(data1[data1$Cluster=='1',c(26)], na.rm = T)
median(data1[data1$Cluster=='2',c(26)], na.rm = T)
median(data1[data1$Cluster=='3',c(26)], na.rm = T)
median(data1[data1$Cluster=='1',c(27)], na.rm = T)
median(data1[data1$Cluster=='2',c(27)], na.rm = T)
median(data1[data1$Cluster=='3',c(27)], na.rm = T)

plot(data1$Cluster, data1$Поведени.1..Речевой.2,  xlab="Номер кластера", ylab = "Вариант ЛВД")
title("Распределение вариантов ЛВД по кластерам",
      "\n 1 - поведенческий, 2 - ППА, 3 - логопенический")

#####k-means method#####

summ.1 <- kmeans(standart.data.less, 3, iter.max = 100)

summ.1$cluster
summ.1$centers
t(summ.1$centers) 

options(digits=2)

summ.1$withinss

summ.1$tot.withinss

summ.1$totss

summ.1$size
data1[summ.1$cluster==1,1]
data1[summ.1$cluster==2,1]
data1[summ.1$cluster==3,1]
#####3 clusters#####
wss <- (nrow(standart.data.less)-1)*sum(apply(standart.data.less,2,var))
for (i in 2:15) {
  wss[i] <- kmeans(standart.data.less, 
                   centers=i)$tot.withinss
}
plot(1:15, wss, type="b", xlab="Количество кластеров",
     ylab="Сумма квадратов расстрояний в группе") 

cex.1 <- 0.2
plot(standart.data.less, col="blue", pch=19,main="Iteration 0", cex = cex.1)

#####2 clusters#####
summ.2 <- kmeans(standart.data.less, 2, iter.max = 100)
wss.2 <- (nrow(standart.data.less)-1)*sum(apply(standart.data.less,2,var))

for (i in 2:15) {
  wss.2[i] <- kmeans(standart.data.less,centers=i)$tot.withinss
}

#####Comparison#####
table(summ.1$cluster, summ.2$cluster)

#####Visualization#####
col.1 <- c("red","blue", "green")
plot(standart.data.less, col=col.1[summ.1$cluster], pch=19, main="Попытка 1",
     cex=cex.1)

#####Data interpretation#####
colMeans(data1[data1$Cluster==1,4:15])
colMeans(data1[data1$Cluster==2,4:15])
colMeans(data1[data1$Cluster==3,4:15])

data1[data1$Cluster==1,1]
data1[data1$Cluster==2,1]
data1[data1$Cluster==3
      ,1]
median(data1[data1$Cluster=='1',19])
median(data1[data1$Cluster=='2',19])
median(data1[data1$Cluster=='3',19])


data1[summ.1$cluster==1,1]

cluster.h.1 <- subset(data1, Cluster==1)
cluster.h.2 <- subset(data1, Cluster==2)
cluster.h.3 <- subset(data1, Cluster==3)

cluster.k.1 <- subset(data1[summ.1$cluster==1,])
cluster.k.2 <- subset(data1[summ.1$cluster==2,])
cluster.k.3 <- subset(data1[summ.1$cluster==3,])

sum.h.1 <- summary(cluster.h.1)
sum.h.2 <- summary(cluster.h.2)
sum.h.3 <- summary(cluster.h.3)
sum.k.1 <- summary(cluster.k.1)
sum.k.2 <- summary(cluster.k.2)
sum.k.3 <- summary(cluster.k.3)


library("writexl")

write.table(x = sum.h.1,file = "C:\\Users\\Darcy\\Desktop\\sumh1.txt",sep = ",")
write.table(x = sum.h.2,file = "C:\\Users\\Darcy\\Desktop\\sumh2.txt",sep = ",")
write.table(x = sum.h.3,file = "C:\\Users\\Darcy\\Desktop\\sumh3.txt",sep = ",")
write.table(x = sum.k.1,file = "C:\\Users\\Darcy\\Desktop\\sumk1.txt",sep = ",")
write.table(x = sum.k.2,file = "C:\\Users\\Darcy\\Desktop\\sumk2.txt",sep = ",")
write.table(x = sum.k.3,file = "C:\\Users\\Darcy\\Desktop\\sumk3.txt",sep = ",")

