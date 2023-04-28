setwd("D:/MHC/STATISTICS/NDS-2023")

library(readr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(psych)
library(dplyr)
library(rstatix)

CSV_3 <- read_csv("CSV-3.csv", col_types = cols(
  NAME = col_skip(),
  SEX = col_factor(levels = c("f", "m")),
  DEM = col_factor(levels = c("0", "1")), 
  GROUP = col_factor(levels = c("1", "2", "3")), 
  FAM = col_factor(levels = c("0", "1","2")), 
  NUM = col_skip(), 
  COGg = col_factor(levels = c("1","2")), 
  PII = col_double(), 
  S100b = col_double(), 
  GMP = col_double(), 
  MBIg10 = col_factor(levels = c("1", "2", "3")), 
  MBIg6 = col_factor(levels = c("1", "2", "3"))
  ))

df <- as_tibble(CSV_3)
df %>%
  mutate_at(vars(PERIOD,
                 MOCA,
                 FCSRT1,
                 FCSRT2,
                 FCSRT3,
                 MBIa,
                 MBId,
                 MBIi,
                 MBIs,
                 MBIp,
                 MBIt,
                 GCA,
                 WML,
                 MTA,
                 PCA,
                 FA),
            as.integer)
df1 <- subset(df, AGE>60)
df1$WML <- as.factor(df1$WML)

count(df, COGg, MBIg6)
count(df, AGE<60)
count(df1, COGg, MBIg6)
count(df1, SEX)
median(df1$AGE)
quantile(df1$AGE)

p <- ggplot(df1)
p.age <- p + geom_histogram(
  mapping = aes(AGE),
  binwidth = 3,
  alpha = 0.5)
p.age + labs(
  x="Возраст, лет",
  y="Количество обследованных",
  title = "Распределение по возрасту")
  
hist(df1$AGE, breaks = 6, xlim = c(60,90), col=rgb(1,0,0,0.5),xlab = "Возраст, лет",
     ylab = "Количество обследованных", main = "Распределение по возрасту")

plot(factor(df1$SEX, labels = c("Женщины", "Мужчины")), col=rgb(1,0,0,0.5), xlab = "",
     ylab = "Количество обследованных", ylim = c(0, 55), main = "Распределение по полу")

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(df$AGE , horizontal=TRUE , ylim=c(50,90), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(df$AGE, breaks = 6, xlim = c(50,90), col=rgb(1,0,0,0.5),xlab = "Возраст, лет",
     ylab = "Количество обследованных", main = "Распределение по возрасту")

ggdensity(df1, x = "LE",
          add = "mean", rug = TRUE,
          color = "MBIg6", fill = "MBIg6",
          palette = c("#B5D99C", "#0F0326","#E65F5C"))

ggdensity(df1, x = "a1PI",
          add = "mean", rug = TRUE,
          color = "MBIg6", fill = "MBIg6",
          palette = c("#B5D99C", "#0F0326","#E65F5C"))

ggdensity(df1, x = "LE",
          add = "mean", rug = TRUE,
          color = "COGg", fill = "COGg",
          palette = c("#B5D99C", "#0F0326","#E65F5C"))

ggdensity(df1, x = "GCA",
          add = "mean", rug = TRUE,
          color = "MBIg6", fill = "MBIg6",
          palette = c("#B5D99C", "#0F0326","#E65F5C"))

test.gca <- df1 %>% 
  wilcox_test(GCA ~ MBIg6) %>%
  add_significance()
df.stats <- df1 %>%
  get_summary_stats(type = "full")

COGg.stats <- df1 %>%
  group_by(COGg) %>%
  get_summary_stats(type = "full")
MBIg6.stats <- df1 %>%
  group_by(MBIg6) %>%
  get_summary_stats(type = "full")

# Differences between non-cogntive groups
attach(df1)
table(MBIg6,GCA)
prop.table(table(MBIg6,GCA),1)
chisq.test(table(MBIg6,GCA),correct=FALSE)
table(MBIg6,WML)
prop.table(table(MBIg6,WML),1)
chisq.test(table(MBIg6,WML),correct=FALSE)
table(MBIg6,MTA)
prop.table(table(MBIg6,MTA),1)
chisq.test(table(MBIg6,MTA),correct=FALSE)
table(MBIg6,PCA)
prop.table(table(MBIg6,PCA),1)
chisq.test(table(MBIg6,PCA),correct=FALSE)
table(MBIg6,FA)
prop.table(table(MBIg6,FA),1)
chisq.test(table(MBIg6,FA),correct=FALSE)

# Differences between cogntive groups
table(COGg,GCA)
prop.table(table(COGg,GCA),1)
chisq.test(table(COGg,GCA),correct=FALSE)
table(COGg,WML)
prop.table(table(COGg,WML),1)
chisq.test(table(COGg,WML),correct=FALSE)
table(COGg,MTA)
prop.table(table(COGg,MTA),1)
chisq.test(table(COGg,MTA),correct=FALSE)
table(COGg,PCA)
prop.table(table(COGg,PCA),1)
chisq.test(table(COGg,PCA),correct=FALSE)
table(COGg,FA)
prop.table(table(COGg,FA),1)
chisq.test(table(COGg,FA),correct=FALSE)

ggdensity(df1, x = "WML",
          add = "mean", rug = TRUE,
          color = "MBIg6", fill = "MBIg6",
          palette = c("#B5D99C", "#0F0326","#E65F5C"))


p.wml <- ggplot(na.omit(df1[,c("WML","MBIg6")]), aes(MBIg6, fill=WML)) +
  geom_bar(position = "fill")+ 
  bgcolor("#FFFFFF")+ 
  scale_fill_discrete(name = "Градация по Fazekas",labels = c("0 ст", "1 ст", "2 ст", "3 ст", "E"))+
  grids(linetype = "dashed")
  
ggpar(p.wml, main = "Распределение групп по показателю поражения белого вещества",
      xlab = "Клинические группы: 1 - депрессивные, 2 - лобные, 3 - контроль", 
      ylab = "Доля от общего количества пациентов в группе",
      caption = "Pearson's Chi-squared test (X-squared = 18.35, df = 6)\np = 0.005")

p.dem <- ggplot(df1[,c("DEM","MBIg6")], aes(MBIg6, fill=DEM)) +
  geom_bar(position = "dodge")+ 
  bgcolor("#FFFFFF")+ 
  scale_fill_discrete(name = "Исход",labels = c("Сохранные", "Конвертеры", "2 ст", "3 ст", "E"))+
  grids(linetype = "dashed")

ggpar(p.dem, main = "Число случаев деменции по данным \n1-летнего катамнеза в клинических группах",
      xlab = "Клинические группы: 1 - депрессивные, 2 - лобные, 3 - контроль", 
      ylab = "Число пациентов в группе")