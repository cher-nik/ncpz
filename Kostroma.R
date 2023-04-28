setwd("D:/R/Kostroma")
library(readr)
df <- read_csv("Kostroma2.csv", col_types = cols(No = col_skip(), NAME = col_skip(), FAM = col_factor(levels = c("0", "1", "2")), 
                                                NUM = col_skip(), S100b=col_double(), PII=col_double(), GMP=col_double(), COGg = col_factor(levels = c("1", "2")),
                                                MBIg10=col_factor(levels=c("1","2","3")), MBIg6=col_factor(levels=c("1","2","3"))
                                                )
               )
View(df)  
PII_median <- median(df$PII)
for (i in 1:nrow(df)){
  if (df$PII[i] > PII_median){
    df$PII_group[i] <- '1'
  } else df$PII_group[i] <- '2'
}
df1 <- na.omit(df[,c(8,9)])
library(pROC)
library(randomForest)
MBI6 <- df$MBIg6
MBI10 <- df$MBIg10
COG <- df1$COGg
LE <- df1$LE

plot(x=LE,y=COG)
glm.fit=glm(COG ~ LE, family=binomial)
lines(LE, glm.fit$fitted.values)
roc(COG, glm.fit$fitted.values, plot=TRUE)
roc(COG, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4)
roc.info <- roc(COG, glm.fit$fitted.values, legacy.axes=TRUE)
str(roc.info)
roc.df <- data.frame(
  tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
  thresholds=roc.info$thresholds)
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]
roc(COG, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

rf.model <- randomForest(factor(COG) ~ LE)

## ROC for random forest
roc(COG, rf.model$votes[,1], plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)
roc(COG, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

plot.roc(COG, rf.model$votes[,1], percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
legend("bottomright", legend=c("Logisitic Regression", "Random Forest"), col=c("#377eb8", "#4daf4a"), lwd=4)

df2 <- na.omit(df[,c(7,9,10,11,12,13,19)])
df2$S100b <- as.numeric(df2$S100b)
df2$GMP <- as.numeric(df2$GMP)
df2cor <- corr.test(df2, method = "pearson")
df2cor$r
df2cor$p.adj
plot(x=df2$LE, y=df2$FCSRT3)
ggplot(df2, aes(LE, FCSRT3))+
  geom_point(size=4)

df2lm <- lm(FCSRT3~LE, df2)
summary(df)
df2lm <- ?lm(FCSRT3~a1PI, df2)
summary(df2lm)
df2lm <- lm(FCSRT3~S100b, df2)
summary(df2lm)
df2lm <- lm(FCSRT3~GMP, df2)
summary(df2lm)
df2lm <- lm(FCSRT3~MBIt, df2)
summary(df2lm)
df2lm <- lm(MBIt~FCSRT3, df2)
summary(df2lm)
df2lm <- lm(LE~MBIt, df2)
summary(df2lm)
df2lm <- lm(LE~FCSRT3, df2)
summary(df2lm)

library(ggplot2)
library(ggpubr)
my_comparisons <- list(c("affective", "impulsive"), c("impulsive", "non-MBI"), c("affective", "non-MBI"))
my_comparisons <- list(c("1", "2"))
p1 <- ggboxplot(na.omit(df[,c("MBIg6","PII")]), x = "MBIg6", y = "PII",
                 color = "MBIg6", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                 add = "jitter", shape = "MBIg6", 
)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test",
                     symnum.args = list(
                       cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                       symbols = c("****", "***", "**", "*", "ns")
                     )
  ) + 
  stat_compare_means(label.y = 8)
ggpar(p1, main = " ",
      xlab = " ", 
      ylab = " ",
      legend.title = " ")

p2 <- ggboxplot(na.omit(df[,c("MBIg6","LE")]), x = "MBIg6", y = "LE",
                color = "MBIg6", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                add = "jitter", shape = "MBIg6", 
)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test",
                     symnum.args = list(
                       cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                       symbols = c("****", "***", "**", "*", "ns")
                     )
  ) + 
  stat_compare_means(label.y = 300)
ggpar(p2, main = " ",
      xlab = " ", 
      ylab = " ",
      legend.title = " ")

p3 <- ggboxplot(na.omit(df[,c("MBIg6","a1PI")]), x = "MBIg6", y = "a1PI",
                color = "MBIg6", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                add = "jitter", shape = "MBIg6", 
)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test",
                     symnum.args = list(
                       cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                       symbols = c("****", "***", "**", "*", "ns")
                     )
  ) + 
  stat_compare_means(label.y = 50)
ggpar(p3, main = " ",
      xlab = " ", 
      ylab = " ",
      legend.title = " ")

p4 <- ggboxplot(na.omit(df[,c("MBIg6","S100b")]), x = "MBIg6", y = "S100b",
                color = "MBIg6", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                add = "jitter", shape = "MBIg6", 
)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test",
                     symnum.args = list(
                       cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                       symbols = c("****", "***", "**", "*", "ns")
                     )
  ) + 
  stat_compare_means(label.y = 2)
ggpar(p4, main = " ",
      xlab = " ", 
      ylab = " ",
      legend.title = " ")
library(tidyverse)
library(psych)
summary(df)
describe(df, fast=F)
quantile(dataset$MBI)
quantile(dataset$MBIa) #Q3=5
quantile(dataset$MBId) #Q3=4
quantile(dataset$MBIi) #Q3=2
quantile(dataset$MBIs) #too little observ
quantile(dataset$MBIp) #too little observ

tb <- as_tibble(df[-c(28,22,11,10,21,25,48,50),])
tb <- mutate(tb, MBI=cut(tb$MBIt, c(-Inf, 13, Inf), labels = c('No', 'Yes')))
tb <- mutate(tb, MBIa=cut(tb$MBIa, c(-Inf,5, Inf), labels = c('No', 'Yes')))
tb <- mutate(tb, MBId=cut(tb$MBId, c(-Inf,4, Inf), labels = c('No', 'Yes')))
tb <- mutate(tb, MBIi=cut(tb$MBIi, c(-Inf,2, Inf), labels = c('No', 'Yes')))

####Density plots####
ggdensity(tb, x = "LE",
          add = "mean", rug = TRUE,
          color = "MBIg10", fill = "MBIg10",
          palette = c("#B5D99C", "#0F0326","#E65F5C"))

ggdensity(na.omit(tb), x = "a1PI",
          add = "mean", rug = TRUE,
          color = "MBIg10", fill = "MBIg10",
          palette = c("#B5D99C", "#0F0326","#E65F5C"))

ggdensity(tb, x = "S100b",
          add = "mean", rug = TRUE,
          color = "MBIg10", fill = "MBIg10",
          palette = c("#B5D99C", "#0F0326","#E65F5C"))

ggdensity(tb, x = "GMP",
          add = "mean", rug = TRUE,
          color = "MBIg10", fill = "MBIg10",
          palette = c("#B5D99C", "#0F0326","#E65F5C"))

####Point plots####
plot(tb$LE, tb$a1PI, ylab="LE", xlab = "a1PI", type = "n",main = "Enzymes")
points(tb$LE[tb$MBI=="No"], tb$a1PI[tb$MBI=="No"], pch=0) #sq
points(tb$LE[tb$MBI=="Yes"],tb$a1PI[tb$MBI=="Yes"], pch=1) #ci

plot(tb$S100b, tb$GMP, ylab="S100b", xlab = "GMP", type = "n",main = "Antibod")
points(tb$S100b[tb$MBI=="No"], tb$GMP[tb$MBI=="No"], pch=0) #sq
points(tb$S100b[tb$MBI=="Yes"],tb$GMP[tb$MBI=="Yes"], pch=1) #ci

####Cluster analysis with immunology and clinical####
#preparing data#

maxs.l <- apply(tb[9], 2, max)
mins.l <- apply(tb[9], 2, min)
standart.data <- scale(tb[9], center = mins.l, scale = maxs.l - mins.l)
standart.data <- as.data.frame(standart.data)
maxs.p <- apply(tb[10], 2, max)
mins.p <- apply(tb[10], 2, min)
standart.data$PE <- scale(tb[10], center = mins.p,
                          scale = maxs.p - mins.p)
maxs.s <- apply(tb[12], 2, max)
mins.s <- apply(tb[12], 2, min)
standart.data$s100 <- scale(tb[12], center = mins.s, 
                            scale = maxs.s - mins.s)
maxs.o <- apply(tb[13], 2, max)
mins.o <- apply(tb[13], 2, min)
standart.data$OBM <- scale(tb[13], center = mins.o, 
                           scale = maxs.o - mins.o)
maxs.m <- apply(tb[19], 2, max)
mins.m <- apply(tb[19], 2, min)
standart.data <- scale(tb[19], center = mins.m, 
                       scale = maxs.m - mins.m)
standart.data <- as.data.frame(standart.data)
maxs.pi <- apply(tb[11], 2, max)
mins.pi <- apply(tb[11], 2, min)
standart.data$pii <- scale(tb[11], center = mins.pi, 
                           scale = maxs.pi - mins.pi)
maxs.f <- apply(tb[7], 2, max)
mins.f <- apply(tb[7], 2, min)
standart.data$fcsrt3 <- scale(tb[7], center = mins.f, 
                           scale = maxs.f - mins.f)

dist.data <- dist(standart.data)

###performing analysis Ward method##

clust.data <- hclust(dist.data, "ward.D")

plot(clust.data, cex = 0.5, axes = F, las = 3, 
     main='', 
     xlab = '', ylab = '')
#visualizing the clusterisation curve#
plot(clust.data$height, type = 'b')
#deciding how many clusters to be
plot(clust.data, cex = 0.5, axes = F, las = 3, 
     main='', 
     xlab = '', ylab = '')
rect.hclust(clust.data, k=2, border="red") 

group1 <- cutree(clust.data, k=2)
tb$ClusterW <- group1
tb$ClusterW <- as.factor(tb$ClusterW)
df$Cluster <- group1

####Cluster analysis with clinic####
#preparing data#
maxs.a <- apply(tb[3], 2, max)
mins.a <- apply(tb[3], 2, min)
standart.data$AGE <- scale(tb[3], center = mins.a, scale = maxs.a - mins.a)
maxs.mb <- apply(tb[11], 2, max)
mins.mb <- apply(tb[11], 2, min)
standart.data$MBI <- scale(tb[11], center = mins.mb,
                           scale = maxs.mb - mins.mb)
maxs.mc <- apply(tb[4], 2, max)
mins.mc <- apply(tb[4], 2, min)
standart.data$MOCA <- scale(tb[4], center = mins.mc, 
                            scale = maxs.mc - mins.mc)
maxs.f <- apply(tb[5], 2, max)
mins.f <- apply(tb[5], 2, min)
standart.data$FCSRT <- scale(tb[5], center = mins.f, 
                             scale = maxs.f - mins.f)

dist.data2 <- dist(standart.data)

###performing analysis Ward method##

clust.data2 <- hclust(dist.data2, "ward.D")

plot(clust.data2, cex = 0.5, axes = F, las = 3, 
     main='', 
     xlab = '', ylab = '')
#visualizing the clusterisation curve#
plot(clust.data2$height, type = 'b')
#deciding how many clusters to be
plot(clust.data2, cex = 0.5, axes = F, las = 3, 
     main='', 
     xlab = '', ylab = '')
rect.hclust(clust.data2, k=2, border="red") 

group2 <- cutree(clust.data2, k=2)
tb$ClusterC <- group2
dataset$Cluster2 <- group2

####Cluster analysis with k-means method####
#Using raw data
summ.1 <- kmeans(tb[,9:13], 2, iter.max = 100)
summ.1$centers #LE too big 
tb$ClusterK <- summ.1$cluster
describeBy(tb, group = tb$ClusterK)

#Using stardart data
summ.2 <- kmeans(standart.data[,1:3], 2, iter.max = 100)
summ.2$centers
tb$ClusterK2 <- summ.2$cluster
describeBy(tb, group = tb$ClusterK2)
tb$ClusterK2 <- as.factor(tb$ClusterK2)
####Analysing clusters####
options(digits=2)
#identifying cluster parameters#
colMeans(standart.data[group1==1])*100
colMeans(standart.data[group1==2])*100

#identifying immunological parameters#
colMeans(subset(df, Cluster=="1", select=c(9:13)))
colMeans(subset(df, Cluster=="2", select=c(9:13)))

##Statistics on group differences##
x <- subset(df, Cluster=="1")
y <- subset(df, Cluster=="2")
x[1]
# immunology between groups
?wilcox.test(x$LE, y$LE,alternative = "g", conf.int=T) #-
wilcox.test(x$PII, y$PII, alternative = "l",conf.int=T) #+ W = 228 p=0.02 
wilcox.test(x$a1PI, y$a1PI, alternative = "g", conf.int=T) #+ W = 503 p=0.001 
wilcox.test(x$S100b, y$S100b, alternative = "l", conf.int=T) #+ W = 11 p<0.001
wilcox.test(x$GMP, y$GMP, alternative = "l") #+ W = 103 p<0.001
wilcox.test(x$AGE, y$AGE, alternative = "g") #- 
wilcox.test(x$FCSRT3, y$FCSRT3, alternative = "l") #- 
wilcox.test(x$MBIt, y$MBIt, alternative = "g") #- 
wilcox.test(x$MOCA, y$MOCA, alternative = "g") #+ w = 393, p=0.05 



dataset_stats <- df %>%
  group_by(Cluster) %>%
  get_summary_stats(type = "full")

####testing groups Wilcox####
library(rstatix)

#k-means cluster groups
test.k.le <- tb %>% 
  wilcox_test(LE ~ ClusterK2) %>%
  add_significance()
test.k.pi <- tb %>% 
  wilcox_test(a1PI ~ ClusterK2) %>%
  add_significance()
test.k.pii <- tb %>% 
  wilcox_test(PII ~ ClusterK2) %>%
  add_significance()
test.k.s100 <- tb %>% 
  wilcox_test(S100b ~ ClusterK2) %>%
  add_significance()
test.k.gmp <- tb %>% 
  wilcox_test(GMP ~ ClusterK2) %>%
  add_significance()
test.k.age <- tb %>% 
  wilcox_test(AGE ~ ClusterK2) %>%
  add_significance()
test.k.moca <- tb %>% 
  wilcox_test(MOCA ~ ClusterK2) %>%
  add_significance()
test.k.fcsrt1 <- tb %>% 
  wilcox_test(FCSRT1 ~ ClusterK2) %>%
  add_significance()
test.k.fcsrt2 <- tb %>% 
  wilcox_test(FCSRT2 ~ ClusterK2) %>%
  add_significance()
test.k.fcsrt3 <- tb %>% 
  wilcox_test(FCSRT3 ~ ClusterK2) %>%
  add_significance()
test.k.mbia <- tb %>% 
  wilcox_test(MBIa ~ ClusterK2) %>%
  add_significance()
test.k.mbid <- tb %>% 
  wilcox_test(MBId ~ ClusterK2) %>%
  add_significance()
test.k.mbii <- tb %>% 
  wilcox_test(MBIi ~ ClusterK2) %>%
  add_significance()
test.k.mbis <- tb %>% 
  wilcox_test(MBIs ~ ClusterK2) %>%
  add_significance()
test.k.mbip <- tb %>% 
  wilcox_test(MBIp ~ ClusterK2) %>%
  add_significance()
test.k.mbit <- tb %>% 
  wilcox_test(MBIt ~ ClusterK2) %>%
  add_significance()
test.k.gca <- tb %>% 
  wilcox_test(GCA ~ ClusterK2) %>%
  add_significance()

#Mixed cluster groups
test.le <- tb %>% 
  wilcox_test(LE ~ ClusterW) %>%
  add_significance()
test.pi <- tb %>% 
  wilcox_test(a1PI ~ ClusterW) %>%
  add_significance()
test.pii <- tb %>% 
  wilcox_test(PII ~ ClusterW) %>%
  add_significance()
test.s100 <- tb %>% 
  wilcox_test(S100b ~ ClusterW) %>%
  add_significance()
test.gmp <- tb %>% 
  wilcox_test(GMP ~ ClusterW) %>%
  add_significance()
test.age <- tb %>% 
  wilcox_test(AGE ~ ClusterW) %>%
  add_significance()
test.moca <- tb %>% 
  wilcox_test(MOCA ~ ClusterW) %>%
  add_significance()
test.fcsrt1 <- tb %>% 
  wilcox_test(FCSRT1 ~ ClusterW) %>%
  add_significance()
test.fcsrt2 <- tb %>% 
  wilcox_test(FCSRT2 ~ ClusterW) %>%
  add_significance()
test.fcsrt3 <- tb %>% 
  wilcox_test(FCSRT3 ~ ClusterW) %>%
  add_significance()
test.mbia <- tb %>% 
  wilcox_test(MBIa ~ ClusterW) %>%
  add_significance()
test.mbid <- tb %>% 
  wilcox_test(MBId ~ ClusterW) %>%
  add_significance()
test.mbii <- tb %>% 
  wilcox_test(MBIi ~ ClusterW) %>%
  add_significance()
test.mbis <- tb %>% 
  wilcox_test(MBIs ~ ClusterW) %>%
  add_significance()
test.mbip <- tb %>% 
  wilcox_test(MBIp ~ ClusterW) %>%
  add_significance()
test.mbit <- tb %>% 
  wilcox_test(MBIt ~ ClusterW) %>%
  add_significance()
test.gca <- tb %>% 
  wilcox_test(GCA ~ ClusterW) %>%
  add_significance()
describeBy(tb, group = tb$ClusterW)

#PII_groups
test.le <- df %>% 
  wilcox_test(LE ~ PII_group) %>%
  add_significance()
test.pi <- df %>% 
  wilcox_test(a1PI ~ PII_group) %>%
  add_significance()
test.pii <- df %>% 
  wilcox_test(PII ~ PII_group) %>%
  add_significance()
test.s100 <- df %>% 
  wilcox_test(S100b ~ PII_group) %>%
  add_significance()
test.gmp <- df %>% 
  wilcox_test(GMP ~ PII_group) %>%
  add_significance()
test.age <- df %>% 
  wilcox_test(AGE ~ PII_group) %>%
  add_significance()
test.moca <- df %>% 
  wilcox_test(MOCA ~ PII_group) %>%
  add_significance()
test.fcsrt1 <- df %>% 
  wilcox_test(FCSRT1 ~ PII_group) %>%
  add_significance()
test.fcsrt2 <- df %>% 
  wilcox_test(FCSRT2 ~ PII_group) %>%
  add_significance()
test.fcsrt3 <- df %>% 
  wilcox_test(FCSRT3 ~ PII_group) %>%
  add_significance()
test.mbia <- df %>% 
  wilcox_test(MBIa ~ PII_group) %>%
  add_significance()
test.mbid <- df %>% 
  wilcox_test(MBId ~ PII_group) %>%
  add_significance()
test.mbii <- df %>% 
  wilcox_test(MBIi ~ PII_group) %>%
  add_significance()
test.mbis <- df %>% 
  wilcox_test(MBIs ~ PII_group) %>%
  add_significance()
test.mbip <- df %>% 
  wilcox_test(MBIp ~ PII_group) %>%
  add_significance()
test.mbit <- df %>% 
  wilcox_test(MBIt ~ PII_group) %>%
  add_significance()
test.gca <- df %>% 
  wilcox_test(GCA ~ PII_group) %>%
  add_significance()

#MBIg10_groups
test2.le <- df %>% 
  wilcox_test(LE ~ MBIg10) %>%
  add_significance()
test2.pi <- df %>% 
  wilcox_test(a1PI ~ MBIg10) %>%
  add_significance()
test2.pii <- df %>% 
  wilcox_test(PII ~ MBIg10) %>%
  add_significance()
test2.s100 <- df %>% 
  wilcox_test(S100b ~ MBIg10) %>%
  add_significance()
test2.gmp <- df %>% 
  wilcox_test(GMP ~ MBIg10) %>%
  add_significance()
test2.age <- df %>% 
  wilcox_test(AGE ~ MBIg10) %>%
  add_significance()
test2.moca <- df %>% 
  wilcox_test(MOCA ~ MBIg10) %>%
  add_significance()
test2.fcsrt1 <- df %>% 
  wilcox_test(FCSRT1 ~ MBIg10) %>%
  add_significance()
test2.fcsrt2 <- df %>% 
  wilcox_test(FCSRT2 ~ MBIg10) %>%
  add_significance()
test2.fcsrt3 <- df %>% 
  wilcox_test(FCSRT3 ~ MBIg10) %>%
  add_significance()
test2.mbia <- df %>% 
  wilcox_test(MBIa ~ MBIg10) %>%
  add_significance()
test2.mbid <- df %>% 
  wilcox_test(MBId ~ MBIg10) %>%
  add_significance()
test2.mbii <- df %>% 
  wilcox_test(MBIi ~ MBIg10) %>%
  add_significance()
test2.mbis <- df %>% 
  wilcox_test(MBIs ~ MBIg10) %>%
  add_significance()
test2.mbip <- df %>% 
  wilcox_test(MBIp ~ MBIg10) %>%
  add_significance()
test2.mbit <- df %>% 
  wilcox_test(MBIt ~ MBIg10) %>%
  add_significance()
test2.gca <- df %>% 
  wilcox_test(GCA ~ MBIg10) %>%
  add_significance()

#COGg_groups
test3.le <- df %>% 
  wilcox_test(LE ~ COGg) %>%
  add_significance()
test3.pi <- df %>% 
  wilcox_test(a1PI ~ COGg) %>%
  add_significance()
test3.pii <- df %>% 
  wilcox_test(PII ~ COGg) %>%
  add_significance()
test3.s100 <- df %>% 
  wilcox_test(S100b ~ COGg) %>%
  add_significance()
test3.gmp <- df %>% 
  wilcox_test(GMP ~ COGg) %>%
  add_significance()
test3.age <- df %>% 
  wilcox_test(AGE ~ COGg) %>%
  add_significance()
test3.moca <- df %>% 
  wilcox_test(MOCA ~ COGg) %>%
  add_significance()
test3.fcsrt1 <- df %>% 
  wilcox_test(FCSRT1 ~ COGg) %>%
  add_significance()
test3.fcsrt2 <- df %>% 
  wilcox_test(FCSRT2 ~ COGg) %>%
  add_significance()
test3.fcsrt3 <- df %>% 
  wilcox_test(FCSRT3 ~ COGg) %>%
  add_significance()
test3.mbia <- df %>% 
  wilcox_test(MBIa ~ COGg) %>%
  add_significance()
test3.mbid <- df %>% 
  wilcox_test(MBId ~ COGg) %>%
  add_significance()
test3.mbii <- df %>% 
  wilcox_test(MBIi ~ COGg) %>%
  add_significance()
test3.mbis <- df %>% 
  wilcox_test(MBIs ~ COGg) %>%
  add_significance()
test3.mbip <- df %>% 
  wilcox_test(MBIp ~ COGg) %>%
  add_significance()
test3.mbit <- df %>% 
  wilcox_test(MBIt ~ COGg) %>%
  add_significance()
test3.gca <- df %>% 
  wilcox_test(GCA ~ COGg) %>%
  add_significance()

#MBIg6_groups
test4.le <- df %>% 
  wilcox_test(LE ~ MBIg6) %>%
  add_significance()
test4.pi <- df %>% 
  wilcox_test(a1PI ~ MBIg6) %>%
  add_significance()
test4.pii <- df %>% 
  wilcox_test(PII ~ MBIg6) %>%
  add_significance()
test4.s100 <- df %>% 
  wilcox_test(S100b ~ MBIg6) %>%
  add_significance()
test4.gmp <- df %>% 
  wilcox_test(GMP ~ MBIg6) %>%
  add_significance()
test4.age <- df %>% 
  wilcox_test(AGE ~ MBIg6) %>%
  add_significance()
test4.moca <- df %>% 
  wilcox_test(MOCA ~ MBIg6) %>%
  add_significance()
test4.fcsrt1 <- df %>% 
  wilcox_test(FCSRT1 ~ MBIg6) %>%
  add_significance()
test4.fcsrt2 <- df %>% 
  wilcox_test(FCSRT2 ~ MBIg6) %>%
  add_significance()
test4.fcsrt3 <- df %>% 
  wilcox_test(FCSRT3 ~ MBIg6) %>%
  add_significance()
test4.mbia <- df %>% 
  wilcox_test(MBIa ~ MBIg6) %>%
  add_significance()
test4.mbid <- df %>% 
  wilcox_test(MBId ~ MBIg6) %>%
  add_significance()
test4.mbii <- df %>% 
  wilcox_test(MBIi ~ MBIg6) %>%
  add_significance()
test4.mbis <- df %>% 
  wilcox_test(MBIs ~ MBIg6) %>%
  add_significance()
test4.mbip <- df %>% 
  wilcox_test(MBIp ~ MBIg6) %>%
  add_significance()
test4.mbit <- df %>% 
  wilcox_test(MBIt ~ MBIg6) %>%
  add_significance()
test4.gca <- df %>% 
  wilcox_test(GCA ~ MBIg6) %>%
  add_significance()

####testing groups chisq####
attach(df)
chisq.test(table(MBIg10,COGg),correct=FALSE)
chisq.test(table(MBIg6,COGg),correct=FALSE)
chisq.test(table(MBIg10,PII_group),correct=FALSE)
chisq.test(table(MBIg6,PII_group),correct=FALSE)
chisq.test(table(PII_group,COGg),correct=FALSE)

## effect sizes ##
library(coin)
df %>% wilcox_effsize(a1PI ~ Cluster)
dataset %>% wilcox_effsize(S.100b ~ Cluster2)

## visualizations ##
plot.le <- ggboxplot(
  df, x = "Cluster", y = "LE", 
  ylab = "LE", xlab = "Groups", add = "jitter"
)

plot.pi <- ggboxplot(
  df, x = "Cluster", y = "a1PI", 
  ylab = "??????????-1 ????, ????/????", xlab = "???????????????????????????????? ????????????????", add = "jitter"
)

test.pi <- test.pi %>% add_xy_position(x = "Cluster")
plot.pi +
  stat_pvalue_manual(test.pi, tip.length = 0) +
  labs(subtitle = get_test_label(test.pi, detailed = TRUE))

plot.pii <- ggboxplot(
  dataset, x = "Cluster2", y = "PII", 
  ylab = "PII", xlab = "Groups", add = "jitter"
)

plot.s100 <- ggboxplot(
  dataset, x = "Cluster2", y = "S.100b", 
  ylab = "S100b", xlab = "Groups", add = "jitter"
)

test.s100 <- test.s100 %>% add_xy_position(x = "Cluster2")
plot.s100 +
  stat_pvalue_manual(test.s100, tip.length = 0) +
  labs(subtitle = get_test_label(test.s100, detailed = TRUE))

plot.gmp <- ggboxplot(
  dataset, x = "Cluster2", y = "GMP", 
  ylab = "GMP", xlab = "Groups", add = "jitter"
)

#1 group 'cognitive': younger, better in MoCA and FCSRT, less MBI
#AGE   MBI  MOCA FCSRT      LE     PI    PII S.100b    GMP 
#49    18    52    29   233.49  48.31   4.92   0.81   0.70 
#2 group 'behavioral': older, worse in cognitive performance (MoCA & FCSRT), higher MBI scores
#AGE   MBI  MOCA FCSRT     LE     PI    PII S.100b    GMP 
#64    34    58   100  214.71  47.78   4.49   0.66   0.65 
ds <- dataset
ds$FCSRT <- as.factor(ds$FCSRT)
ds$Cluster2 <- as.factor(ds$Cluster2)
ggplot(ds, aes(Cluster2, fill=FCSRT)) +
  geom_bar(position = "dodge")

#### Logical try ####
df$COGg <- as.factor(df$COGg)

df_stats <- df %>%
  group_by(MBIg10) %>%
  get_summary_stats(type = "full")

test.le12 <- subset(df, MBIg10!="3") %>% 
  rstatix::wilcox_test(MBIt ~ MBIg10) %>%
  add_significance()
test.le13 <- subset(df, MBIg10!="2") %>% 
  rstatix::wilcox_test(MBIt ~ MBIg10) %>%
  add_significance()
test.le23 <- subset(df, MBIg10!="1") %>% 
  rstatix::wilcox_test(MBIt ~ MBIg10) %>%
  add_significance()

test.pi12 <- subset(df, MBIg10!="3") %>% 
  rstatix::wilcox_test(a1PI ~ MBIg10) %>%
  add_significance()
test.pi13 <- subset(df, MBIg10!="2") %>% 
  rstatix::wilcox_test(a1PI ~ MBIg10) %>%
  add_significance()
test.pi23 <- subset(df, MBIg10!="1") %>% 
  rstatix::wilcox_test(a1PI ~ MBIg10) %>%
  add_significance()

test.pii12 <- subset(df, MBIg10!="3") %>% 
  rstatix::wilcox_test(PII ~ MBIg10,
                       alternative = "less") %>%
  add_significance()
test.pii13 <- subset(df, MBIg10!="2") %>% 
  rstatix::wilcox_test(PII ~ MBIg10) %>%
  add_significance()
test.pii23 <- subset(df, MBIg10!="1") %>% 
  rstatix::wilcox_test(PII ~ MBIg10,
                       alternative = "greater") %>%
  add_significance()


test.s10012 <- subset(df, MBIg10!="3") %>% 
  rstatix::wilcox_test(S100b ~ MBIg10) %>%
  add_significance()
test.gmp <- df %>% 
  rstatix::wilcox_test(GMP ~ MBIg10 
                       alternative = "greater") %>%
  add_significance()

# + only in PII between 1 and 3 group

## effect sizes ##
library(coin)
subset(df, MBIg10!="1") %>% wilcox_effsize(PII ~ MBIg10, alternative = "greater")
df %>% wilcox_effsize(S100b ~ Cluster)

## visualizations ##
plot.le <- ggboxplot(
  df, x = "MBIg10", y = "LE", 
  ylab = "LE", xlab = "Groups", add = "jitter"
)

plot.pi <- ggboxplot(
  df, x = "MBIg10", y = "a1PI", 
  ylab = "A1PI", xlab = "Groups", add = "jitter"
)

plot.pii <- ggboxplot(
  df, x = "MBIg10", y = "PII", 
  ylab = "PII", xlab = "Groups", add = "jitter"
)
test.pii23 <- test.pii23 %>% add_xy_position(x = "MBIg10")
plot.pii +
  stat_pvalue_manual(test.pii23, tip.length = 0) +
  labs(subtitle = get_test_label(test.pii23, detailed = TRUE))

plot.s100 <- ggboxplot(
  df, x = "MBIg10", y = "S.100b", 
  ylab = "S100b", xlab = "Groups", add = "jitter"
)

plot.gmp <- ggboxplot(
  df, x = "MBIg10", y = "GMP", 
  ylab = "GMP", xlab = "Groups", add = "jitter"
)

s1 <- ggscatter(tb, x = "LE", y = "a1PI", color = "COGg",
                size = 5)+
  color_palette(c("#00AFBB","#FC4E07" , "#E7B800"))
s1p <- ggpar(s1, title = "???????????? ?????????????????????? ?????????????????????? ???????????????????? ???? ?? ??1-????",
             subtitle = "?? '??????????????????????' ??????????????",
             xlab = "????, ???????????????????????????? ????????????????????, ??????????/??????*????",
             ylab = "a1-????, ???????????????????????????? ????????????????????, ????/????",
             legend.title = "????????????:")
s2 <- ggscatter(tb, x = "LE", y = "a1PI", color = "MBIg6",
                size = 5)+
  color_palette(c("#00AFBB","#FC4E07" , "#E7B800"))
s2p <- ggpar(s2, title = "???????????? ?????????????????????? ?????????????????????? ???????????????????? ???? ?? ??1-????",
             subtitle = "?? '??????????????????????????' ??????????????",
             xlab = "????, ???????????????????????????? ????????????????????, ??????????/??????*????",
             ylab = "a1-????, ???????????????????????????? ????????????????????, ????/????",
             legend.title = "????????????:")