df <- MCI_1

str(df)

colnames(df)

names(df)[names(df) == "62"] <- "Age"
names(df)[names(df) == "MCI"] <- "Diagnosis"
names(df)[names(df) == "3"] <- "ApoE"
names(df)[names(df) == "30"] <- "MMSE"
names(df)[names(df) == "6.3"] <- "Mem_test"
names(df)[names(df) == "5"] <- "Recall_test"
names(df)[names(df) == "27"] <- "MOCA"
names(df)[names(df) == "1...8"] <- "Vis_mem"
names(df)[names(df) == "0"] <- "Vis_spat"
names(df)[names(df) == "1...10"] <- "Prax"
names(df)[names(df) == "1...11"] <- "Regul"
names(df)[names(df) == "1...12"] <- "Verb_mem"

dim(df)

#создал номинативные переменные
df$Diagnosis <- as.factor(df$Diagnosis)
levels(df$Diagnosis) <- c("Control", "MCI")

df$ApoE <- as.factor(df$ApoE)
levels(df$ApoE) <- c("e3", "e4")

df_MCI <- df[df$Diagnosis=='MCI',]
df_control <- df[df$Diagnosis=='Control',]

#описательная статистика
library(psych)

stats_df <- describe(df[,-c(2,3)])
stats_df <- (round(stats_df[,-1], 2)) #убирает знаки после зпт
                 
write.table(stats_df, file = "stats.txt", sep = ",", 
            quote = FALSE, row.names = T) 

stats_df_MCI <- describe(df_MCI)
stats_df_MCI <- (round(stats_df_MCI[,-c(1,6,7,11,12)], 2))
write.table(stats_df_MCI, file = "stats_MCI.txt", sep = ",", 
            quote = FALSE, row.names = T) 

stats_df_control <- describe(df_control)
stats_df_control <- round(stats_df_control[,-c(1,6,7,11,12)], 2)
write.table(stats_df_control, file = "stats_control.txt", sep = ",", 
            quote = FALSE, row.names = T) 

library(stargazer)
stargazer(df, type = 'latex', out = '1.doc')

DS1 <- aggregate(cbind(Age, MMSE, Mem_test, Recall_test, MOCA)~Diagnosis, 
                 df, mean)
DS1 <- as.matrix(DS1)
stargazer(DS1, type="html", out="stat1.htm")

DS2 <- aggregate(cbind(Age, MMSE, Mem_test, Recall_test, MOCA)~Diagnosis, 
                 df, sd)
DS2 <- as.matrix(DS2)
stargazer(DS2, type="html", out="stat2.htm")

DS3 <- aggregate(cbind(Age, MMSE, Mem_test, Recall_test, MOCA)~Diagnosis, 
                 df, median)
DS3 <- as.matrix(DS3)
stargazer(DS3, type="html", out="stat3.htm")

statmat1 <- describeBy(df[,-c(1,2,6)], group = df$Diagnosis,
                       mat = T, fast = T, digits = 1)
statmat1 <- as.matrix(statmat1)
stargazer(statmat1, type="html", out="statmat1.htm")

statmat2 <- describeBy(df[,-c(1,2,6)], group = df$ApoE,
                       mat = T, fast = T, digits = 1)
statmat2 <- as.matrix(statmat2)
stargazer(statmat2, type="html", out="statmat2.htm")

#группы по диагнозу
describeBy(x=df[,-c(1,2,6)], group = df$Diagnosis)
Ds_group_stats <- describeBy(x=df[,-c(1,2,6)], group = df$Diagnosis, 
                             mat = T, digits = 1)
#на случай списка
Ds_group_stats$Control
Ds_group_stats$MCI

MCI_group_stats <- describeBy(x=df[,-c(1,2,6)], group = df$Diagnosis=='MCI',
                              mat = T, digits = 1, fast = T)


#группы по генотипу
describeBy(x=df[,-c(1,2,6)], group = df$ApoE)
Genotype_group_stats <- describeBy(x=df[,-c(1,2,6)], group = df$ApoE, 
                                   mat = T, digits = 1)

#обобщенная статистика по обеим группам
describeBy(df$MOCA, group = list(df$Diagnosis, df$ApoE), mat = T, digits = 1,
           fast = T)


#проверка на НД - 41. Добавить аргумент na.rm = T
sum(is.na(df))


#гистограммы распределения значений переменных
#Age/Diagnosis
hist(df$Age)

ggplot(df, aes(x=Age))+
  geom_histogram(fill = 'white', col = 'black', binwidth = 3)+
  facet_grid(Diagnosis ~ .)

#MMSE/diagnosis
ggplot(df, aes(x=MMSE), stat='count')+
  geom_histogram(fill = 'white', col = 'black', binwidth = 1)+
  facet_grid(Diagnosis ~ .) #не работает с фактором в х, а как сделать наоборот?

library(ggplot2)

ggplot(df, aes(Diagnosis, MMSE))+
  geom_boxplot()

#Memtest/Diagnosis
ggplot(df, aes(x=Mem_test))+
  geom_histogram(fill = 'white', col = 'black', binwidth = 0.5)+
  facet_grid(Diagnosis ~ .)

ggplot(df, aes(Mem_test, fill = Diagnosis ))+
  geom_density(alpha = 0.5)

ggplot(df, aes(Diagnosis, Mem_test))+
  geom_boxplot()

#Recall/Diagnosis
ggplot(df, aes(x=Recall_test))+
  geom_histogram(fill = 'white', col = 'black', binwidth = 1)+
  facet_grid(Diagnosis ~ .)

ggplot(df, aes(Recall_test, fill = Diagnosis ))+
  geom_density(alpha = 0.5)

ggplot(df, aes(Diagnosis, Recall_test))+
  geom_boxplot()

#MOCA/Diagnosis
ggplot(df, aes(x=MOCA))+
  geom_histogram(fill = 'white', col = 'black', binwidth = 1)+
  facet_grid(Diagnosis ~ .)

ggplot(df, aes(MOCA, fill = Diagnosis ))+
  geom_density(alpha = 0.5)

ggplot(df, aes(Diagnosis, MOCA))+
  geom_boxplot()

#проверка выброса значений
ggplot(df, aes(Diagnosis, Age))+
  geom_boxplot()


#проверка нормальности (гомогенности) распределения \
shapiro.test(df$Age)

shapiro.test(df$Age[df$Diagnosis=="MCI"])
shapiro.test(df$Age[df$Diagnosis=="Control"])

#t-критерий
t.test(Age~Diagnosis, df)

ggplot(df, aes(Diagnosis, Age))+
  stat_summary(fun.data = mean_cl_boot, 
               geom = "errorbar", width = 0.2)+
  stat_summary(fun = mean, geom = "point", size = 4)

#Wilcox (Mann-Whitney) для не(очень)нормального распределения НЕ КОРРЕЛЯЦИЯ
#Diagnosis ОТЛИЧАЮТСЯ ЛИ ГРУППЫ И ПО КАКИМ ПОКЛЯМ ДОСТОВЕРНО
wilcox.test(Age~Diagnosis, df) #достоверно отличаются

wilcox.test(MMSE~Diagnosis, df) #достоверно отличаются

wilcox.test(Mem_test~Diagnosis, df)#достоверно отличаются

wilcox.test(Recall_test~Diagnosis, df)#достоверно отличаются

wilcox.test(MOCA~Diagnosis, df)#достоверно отличаются
#ApoE
wilcox.test(Age~ApoE, df) #no = случайны

wilcox.test(MMSE~ApoE, df) #no

wilcox.test(Mem_test~ApoE, df) #no

wilcox.test(Recall_test~ApoE, df) #no

wilcox.test(MOCA~ApoE, df) #no

#визуально
ggplot(df, aes(Diagnosis, Age))+
  geom_boxplot()

ggplot(df, aes(Diagnosis, MMSE))+
  geom_boxplot()

ggplot(df, aes(Diagnosis, Mem_test))+
  geom_boxplot()

ggplot(df, aes(Diagnosis, Recall_test))+
  geom_boxplot()

ggplot(df, aes(Diagnosis, MOCA))+
  geom_boxplot()

ggplot(df_no_na, aes(ApoE, Age))+
  geom_count()
ggplot(df_no_na, aes(ApoE, MMSE))+
  geom_count()
ggplot(df_no_na, aes(ApoE, Mem_test))+
  geom_count()
ggplot(df_no_na, aes(ApoE, Recall_test))+
  geom_count()
ggplot(df_no_na, aes(ApoE, MOCA))+
  geom_count()



#ранговый коэффициент Спирмена (Kendall)
cor(df$Age, df$MMSE, use =  "pairwise.complete.obs",
    method = "kendall")

cor.test(~Age+MMSE, x1)

cor.test(~Age+MMSE, x2)

#распределение MMSE/Age в зависимости от диагноза
ggplot(df, aes(Age, MMSE, col = factor(Diagnosis)))+
  geom_point(size=4)






#таблица линейной регрессии (только для параметрических выборок) на экспорт


m1 <- lm(MMSE~df$Diagnosis=="MCI", data=df)
m2 <- lm(MMSE~df$Diagnosis=="Control", data=df)
m3 <- lm(MMSE~(df$Diagnosis=="MCI")+(df$ApoE=="e3"), data=df)
m4 <- lm(MMSE~(df$Diagnosis=="MCI")+(df$ApoE=="e4")+df$Age, data=df)
stargazer(m1, m2, m3, m4, type="html",
          dep.var.labels=c("MMSE"),
          covariate.labels=c("MCI","Control",
                             "MCI+ApoEe3",
                             "MCI+ApoEe4",
                             "Age"), 
          out="models.htm")
?lm
#построение матрицы

df_no_na <- na.omit(df)
attach(df_no_na)

install.packages('Hmisc')
library(Hmisc)
rcorr(as.matrix(df[,-c(1,2,6)]), type='spearman')
mat1 <- rcorr(as.matrix(df[,-c(1,2,6)]), type='spearman')

cor(df_no_na[df_no_na$Diagnosis=='MCI',-c(1,2,6)], 
    df_no_na[df_no_na$Diagnosis=='Control',-c(1,2,6)], method = 'spearman'
)

mat1$P <- as.data.frame(mat1)

#построение графиков рассеивания 

plot(df$MMSE, df$Mem_test, col=factor(df$Diagnosis), 
     xlab = 'MMSE', ylab = "10 word Memo Test")


plot(df$MMSE, df$Mem_test, col=factor(df$ApoE), 
     xlab = 'MMSE', ylab = "10 word Memo Test")

kruskal.test(MMSE[Diagnosis=='MCI']~MMSE[Diagnosis=='Control'],df)

#поправка на множественное сравнение
?p.adjust
p.adjust(mat1$P,
         method = 'bonferroni'
)
mat_adj <- p.adjust(mat1$P,
                    method = 'bonferroni',
)



image(1:ncol(mat1$P), 1:nrow(mat1$P), mat1$P,
      col = heat.colors(22), axes=FALSE, xlab="", ylab="",
)
# Подписи к осям:
axis(1, at=1:ncol(mat1$P), labels=abbreviate(colnames(mat1$P)))
axis(2, at=1:nrow(mat1$P), labels=abbreviate(rownames(mat1$P)),
     las = 2)

mat1$P

?image

#значимость корреляции

#сравнение групп

x1 <- df[df$Diagnosis=='MCI',-c(1,2,6)]
x2 <- df[df$Diagnosis=='Control',-c(1,2,6)]

x1 <- na.omit(x1)
x2 <- na.omit(x2)

y1 <- df[df$ApoE=='3',-c(1,2,6)]
y2 <- df[df$ApoE=='4',-c(1,2,6)]

y1 <- na.omit(y1)
y2 <- na.omit(y2)

sapply(1:nrow(x1), function(i) cor(df1[i,], df2[i,]))

diag(cor(t(x1), t(x2)))

X1 <- as.matrix(x1)
X2 <- as.matrix(x2)

cor(X1, 
    X2, method = c("pearson", "kendall", "spearman")
)



# проверка на нормальность переменной 
# Sepal.Length в трех разных группах в соответствии с переменной Species
by(iris$Sepal.Length, INDICES = iris$Species, shapiro.test) 



#межгрупповая корреляция
rcorr(as.matrix(x1), type='spearman')
rcorr(as.matrix(x2), type='spearman')

b1 <- rcorr(as.matrix(x1), type='spearman')
b2 <- rcorr(as.matrix(x2), type='spearman')

p.adjust(b1$P,
         method = 'bonferroni'
)
p.adjust(b2$P,
         method = 'bonferroni'
)
b1_adj <- p.adjust(b1$P,
                   method = 'bonferroni'
)
b2_adj <- p.adjust(b2$P,
                   method = 'bonferroni'
)
bon1 <- matrix(b1_adj, ncol=5, byrow=TRUE)
bon2 <- matrix(b2_adj, ncol=5, byrow=TRUE)

rcorr(as.matrix(y1), type='spearman')
rcorr(as.matrix(y2), type='spearman')

d1 <- rcorr(as.matrix(y1), type='spearman')
d2 <- rcorr(as.matrix(y2), type='spearman')

d1_adj <- p.adjust(d1$P,
                   method = 'bonferroni'
)
d2_adj <- p.adjust(d2$P,
                   method = 'bonferroni'
)
bon3 <- matrix(d1_adj, ncol=5, byrow=TRUE, colnames(
  c('MMSE', 'Mem_test', 'Recall_test', 'MOCA', 'Age')))
bon4 <- matrix(d2_adj, ncol=5, byrow=TRUE)

#вывод данных
#без бонферрони
stargazer(b1$P, b2$P, d1$P, d2$P, type="html",
          out="models2.htm")
#с бонферрони
stargazer(bon1, bon2, bon3, bon4, type="html",
          out="models3.htm")
#визуально
image(1:ncol(bon1), 1:nrow(bon1), bon1,
      col = heat.colors(3), axes=FALSE, xlab="", ylab="",
)

axis(1, at=1:ncol(bon1), labels=abbreviate(colnames(mat1$P)))
axis(2, at=1:nrow(bon1), labels=abbreviate(rownames(mat1$P)),
     las = 2)
