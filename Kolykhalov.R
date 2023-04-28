####без иммуноглобулинов####
## Импорт данных
dataset <- read.csv("dataset.csv", sep = ";")
library(tidyverse)

## Сабсетинг
df <- dataset[1:30, c(2,3,9,10,11,12,13,14,21,22,23,24,25)]
df <- df[,-c(5,8)]
df$CRP <- dataset[1:30, 7]

## Преобразование значений
df.num1 <- as.numeric(sub(",", ".", df$ФРН, fixed = TRUE))
df.num2 <- as.numeric(sub(",", ".", df$ЛЭ, fixed = TRUE))
df.num3 <- as.numeric(sub(",", ".", df$а1.ПИ, fixed = TRUE))
df.num4 <- as.numeric(sub(",", ".", df$ЛЭ.а1.ПИ, fixed = TRUE))
df.num5 <- as.numeric(sub(",", ".", df$CRP, fixed = TRUE))
df$ФРН <- df.num1
df$ЛЭ <- df.num2
df$а1.ПИ <- df.num3
df$ЛЭ.а1.ПИ <- df.num4
df$CRP <- df.num5
df$M.P2 <- cut(df$M.P2, 3, labels=c("Неуд", "Уд", "Хор"))


## Визуализация

ggplot(data = subset(df, !is.na(M.P2))) +
  stat_summary(
    mapping=aes(x=M.P2, y=ЛЭ),
    fun.ymin=min,
    fun.ymax=max,
    fun.y=median
  ) +
  ggtitle("Соотношение активности ЛЭ с динамикой когнитивных показателей") +
  labs(y= "Активность ЛЭ", x = "Динамика результата MMSE на 3 визите (в баллах)")
    
ggplot(data = subset(df, !is.na(M.P2))) +
  stat_summary(
    mapping=aes(x=M.P2, y=ФРН),
    fun.ymin=min,
    fun.ymax=max,
    fun.y=median
  ) +
  ggtitle("Соотношение активности ФРН с динамикой когнитивных показателей") +
  labs(y= "ФРН", x = "Динамика результата MMSE на 3 визите (в баллах)")

ggplot(data = subset(df, !is.na(M.P2))) +
  stat_summary(
    mapping=aes(x=M.P2, y=а1.ПИ),
    fun.ymin=min,
    fun.ymax=max,
    fun.y=median
  ) +
  ggtitle("Соотношение активности а1-ПИ с динамикой когнитивных показателей") +
  labs(y= "Активность а1-ПИ", x = "Динамика результата MMSE на 3 визите (в баллах)")

ggplot(data = subset(df, !is.na(M.P2))) +
  stat_summary(
    mapping=aes(x=M.P2, y=ЛЭ.а1.ПИ),
    fun.ymin=min,
    fun.ymax=max,
    fun.y=median
  ) +
  ggtitle("Соотношение ПИИ с динамикой когнитивных показателей") +
  labs(y= "ПИИ", x = "Динамика результата MMSE на 3 визите (в баллах)")

ggplot(data = subset(df, !is.na(M.P2)), aes(MMSE1, ЛЭ, col = factor(M.P2))) +
  geom_point(size=3, position = "jitter") +
  scale_colour_discrete(name="Эффективность\nкурса") +
  ggtitle("Соотношение активности ЛЭ с динамикой\nкогнитивных показателей") +
  labs(y= "Активность ЛЭ", x = "Исходные результаты MMSE (в баллах)")

ggplot(data = subset(df, !is.na(M.P2)), aes(MMSE1, CRP, col = factor(M.P2))) +
  geom_point(size=3, position = "jitter") +
  scale_colour_discrete(name="Эффективность\nкурса") +
  ggtitle("Соотношение активности ЦРБ с динамикой\nкогнитивных показателей") +
  labs(y= "Активность ЦРБ", x = "Исходные результаты MMSE (в баллах)")

ggplot(data = subset(df, !is.na(M.P2)), aes(MMSE1, ЛЭ, fill=M.P2)) + 
  geom_boxplot()+
  ggtitle("Соотношение активности ЛЭ с динамикой\nкогнитивных показателей") +
  labs(y= "Активность ЛЭ", x = "Исходные результаты MMSE (в баллах)")+
  scale_fill_discrete(name="Эффективность\nкурса")

ggplot(data = subset(df, !is.na(M.P2)), aes(MMSE1, CRP, fill=M.P2)) + 
  geom_boxplot()+
  ggtitle("Соотношение активности ЦРБ с динамикой\nкогнитивных показателей") +
  labs(y= "Активность ЦРБ", x = "Исходные результаты MMSE (в баллах)")+
  scale_fill_discrete(name="Эффективность\nкурса")

ggplot(data = subset(df, !is.na(M.P2)), aes(MMSE1, а1.ПИ, fill=M.P2)) + 
  geom_boxplot()+
  ggtitle("Соотношение активности а1-ПИ с динамикой\nкогнитивных показателей") +
  labs(y= "Активность а1-ПИ", x = "Исходные результаты MMSE (в баллах)")+
  scale_fill_discrete(name="Эффективность\nкурса")

ggplot(data = subset(df, !is.na(M.P2)), aes(MMSE1, ФРН, fill=M.P2)) + 
  geom_boxplot()+
  ggtitle("Соотношение активности ФРН с динамикой\nкогнитивных показателей") +
  labs(y= "Активность ФРН", x = "Исходные результаты MMSE (в баллах)")+
  scale_fill_discrete(name="Эффективность\nкурса")

ggplot(data = subset(df, !is.na(M.P2)), aes(MMSE1, df[-18, 6], fill=M.P2)) + 
  geom_boxplot()+
  ggtitle("Соотношение активности ПИИ с динамикой\nкогнитивных показателей") +
  labs(y= "Активность ПИИ", x = "Исходные результаты MMSE (в баллах)")+
  scale_fill_discrete(name="Эффективность\nкурса")


## Проверка гипотез
# ПИИ и MMSE по исходам
wilcox.test(df$ЛЭ.а1.ПИ~M.P2!='Неуд', df)#-
wilcox.test(df$ЛЭ.а1.ПИ~M.P2!='Уд', df)#-
wilcox.test(df$ЛЭ.а1.ПИ~M.P2!='Хор', df)#-
# ЛЭ и MMSE по исходам
wilcox.test(df$ЛЭ~M.P2!='Неуд', df)#-
wilcox.test(df$ЛЭ~M.P2!='Уд', df)#-
wilcox.test(df$ЛЭ~M.P2!='Хор', df)#-
# а1.ПИ и MMSE по исходам
wilcox.test(df$а1.ПИ~M.P2!='Неуд', df)#-
wilcox.test(df$а1.ПИ~M.P2!='Уд', df)#-
wilcox.test(df$а1.ПИ~M.P2!='Хор', df)#-
# СРБ и MMSE по исходам
wilcox.test(df$CRP~M.P2!='Неуд', df)#-
wilcox.test(df$CRP~M.P2!='Уд', df)#+ p-value = 0.04262
wilcox.test(df$CRP~M.P2!='Хор', df)#-
# ФРН и MMSE по исходам
wilcox.test(df$ФРН~M.P2!='Неуд', df)#-
wilcox.test(df$ФРН~M.P2!='Уд', df)#-
wilcox.test(df$ФРН~M.P2!='Хор', df)#-

####с иммуноглобулинами####
## Импорт данных
setwd("C:/Users/Darcy/Desktop/R/Kolykhalov")
dataset <- read.csv("dataset_ig.csv", sep = ";")
library(tidyverse)

## Сабсетинг
df <- dataset[1:30, -c(1, 6, 7, 8, 9, 11, 12, 13, 14, 27, 30, 34, 40, 45, 51, 56,
                       61, 66)
              ]

##Переименования
old_names <- colnames(df)
new_names <- 1:49
compare <- c(rbind(new_names, old_names))
colnames(df) <- c("index","age","sex","apoe","le","iga_1","igm_1","igg_1",
                  "cic_1","iga_2","igm_2","igg_2","cic_2","mri","hach","haml",
                  "gds","mmse_1","mmse_2","mmse_3","mmse_r","sld_1","sld_2",
                  "sld_3","sld_r","tenw_1","tenw_2","tenw_3","tenw_r","dere_1",
                  "dere_2","dere_3", "dere_r","auas_1","auas_2","auas_3","auas_r",
                  "caas_1","caas_2","caas_3","caas_r","trch_1","trch_2","trch_3",
                  "trch_r","bost_1","bost_2","bost_3","bost_r")

## Преобразование значений
summary(df)

df_num1 <- as.numeric(sub(",", ".", df$le, fixed = TRUE))
df_num2 <- as.numeric(sub(",", ".", df$iga_1, fixed = TRUE))
df_num3 <- as.numeric(sub(",", ".", df$igm_1, fixed = TRUE))
df_num4 <- as.numeric(sub(",", ".", df$iga_2, fixed = TRUE))
df_num5 <- as.numeric(sub(",", ".", df$tenw_1, fixed = TRUE))
df_num6 <- as.numeric(sub(",", ".", df$tenw_2, fixed = TRUE))
df_num7 <- as.numeric(sub(",", ".", df$tenw_3, fixed = TRUE))
df_num8 <- as.numeric(sub(",", ".", df$tenw_r, fixed = TRUE))

df$le <- df_num1
df$iga_1 <- df_num2
df$igm_1 <- df_num3
df$iga_2 <- df_num4
df$tenw_1 <- df_num5
df$tenw_2 <- df_num6
df$tenw_3 <- df_num7
df$tenw_r <- df_num8

df$apoe <- cut(df$apoe, 2, labels=c("Good", "Bad"))
str(df$apoe)
df$sex <- cut(df$sex, 2, labels=c("M", "F"))
str(df$sex)
df$mmse_r <- cut(df$mmse_r, 3, labels=c("Неуд", "Уд", "Хор"))
df$tenw_r <- cut(df$tenw_r, 3, labels=c("Неуд", "Уд", "Хор"))
df$dere_r <- cut(df$dere_r, 3, labels=c("Неуд", "Уд", "Хор"))
df$auas_r <- cut(df$auas_r, 3, labels=c("Неуд", "Уд", "Хор"))
df$caas_r <- cut(df$caas_r, 3, labels=c("Неуд", "Уд", "Хор"))
df$bost_r <- cut(df$bost_r, 3, labels=c("Неуд", "Уд", "Хор"))

## Визуализация

ggplot(data = subset(df, !is.na(mmse_r)), aes(mmse_1, le, fill=mmse_r)) + 
  geom_boxplot()+
  ggtitle("Соотношение активности ЛЭ с динамикой\nкогнитивных показателей") +
  labs(y= "Активность ЛЭ", x = "Исходные результаты MMSE (в баллах)")+
  scale_fill_discrete(name="Эффективность\nкурса")

ggplot(data = subset(df[-c(4,20,16,26),], !is.na(mmse_r)), 
       aes(mmse_1, df[-c(4,20,16,26),6], fill=mmse_r)) + 
  geom_boxplot()+
  ggtitle("Соотношение исходного уровня антител IgA\n с динамикой когнитивных показателей") +
  labs(y= "Уровень антител", x = "Исходные результаты MMSE (в баллах)")+
  scale_fill_discrete(name="Эффективность\nкурса")
hist(df[-c(4,16,26),6],breaks=20,probability = T)
hist(df[-c(4,16,26),6],breaks=20,probability = T,xlim = c(150,400), ylim = c(0,0.01))
lines(density(df[,6]), lwd = 2, col = "red")
hist(log(df[,6]),breaks=20,probability = T)
lines(density(log(df[,6])), lwd = 2, col = "red")


ggplot(data = subset(df[-c(4,20,23,5),], !is.na(mmse_r)), 
       aes(mmse_1, df[-c(4,20,23,5),7], fill=mmse_r)) + 
  geom_boxplot()+
  ggtitle("Соотношение исходного уровня антител IgM\n с динамикой когнитивных показателей") +
  labs(y= "Уровень антител", x = "Исходные результаты MMSE (в баллах)")+
  scale_fill_discrete(name="Эффективность\nкурса")

ggplot(data = subset(df, !is.na(mmse_r)), 
       aes(mmse_1, igg_1, fill=mmse_r)) + 
  geom_boxplot()+
  ggtitle("Соотношение исходного уровня антител IgG\n с динамикой когнитивных показателей") +
  labs(y= "Уровень антител", x = "Исходные результаты MMSE (в баллах)")+
  scale_fill_discrete(name="Эффективность\nкурса")



ggplot(data = subset(df, !is.na(M.P2)), aes(MMSE1, CRP, fill=M.P2)) + 
  geom_boxplot()+
  ggtitle("Соотношение активности ЦРБ с динамикой\nкогнитивных показателей") +
  labs(y= "Активность ЦРБ", x = "Исходные результаты MMSE (в баллах)")+
  scale_fill_discrete(name="Эффективность\nкурса")

ggplot(data = subset(df, !is.na(M.P2)), aes(MMSE1, а1.ПИ, fill=M.P2)) + 
  geom_boxplot()+
  ggtitle("Соотношение активности а1-ПИ с динамикой\nкогнитивных показателей") +
  labs(y= "Активность а1-ПИ", x = "Исходные результаты MMSE (в баллах)")+
  scale_fill_discrete(name="Эффективность\nкурса")

ggplot(data = subset(df, !is.na(M.P2)), aes(MMSE1, ФРН, fill=M.P2)) + 
  geom_boxplot()+
  ggtitle("Соотношение активности ФРН с динамикой\nкогнитивных показателей") +
  labs(y= "Активность ФРН", x = "Исходные результаты MMSE (в баллах)")+
  scale_fill_discrete(name="Эффективность\nкурса")

ggplot(data = subset(df, !is.na(M.P2)), aes(MMSE1, df[-18, 6], fill=M.P2)) + 
  geom_boxplot()+
  ggtitle("Соотношение активности ПИИ с динамикой\nкогнитивных показателей") +
  labs(y= "Активность ПИИ", x = "Исходные результаты MMSE (в баллах)")+
  scale_fill_discrete(name="Эффективность\nкурса")


## Проверка гипотез
# ПИИ и MMSE по исходам
wilcox.test(df$ЛЭ.а1.ПИ~M.P2!='Неуд', df)#-
wilcox.test(df$ЛЭ.а1.ПИ~M.P2!='Уд', df)#-
wilcox.test(df$ЛЭ.а1.ПИ~M.P2!='Хор', df)#-
# ЛЭ и MMSE по исходам
wilcox.test(df$ЛЭ~M.P2!='Неуд', df)#-
wilcox.test(df$ЛЭ~M.P2!='Уд', df)#-
wilcox.test(df$ЛЭ~M.P2!='Хор', df)#-
# а1.ПИ и MMSE по исходам
wilcox.test(df$а1.ПИ~M.P2!='Неуд', df)#-
wilcox.test(df$а1.ПИ~M.P2!='Уд', df)#-
wilcox.test(df$а1.ПИ~M.P2!='Хор', df)#-
# СРБ и MMSE по исходам
wilcox.test(df$CRP~M.P2!='Неуд', df)#-
wilcox.test(df$CRP~M.P2!='Уд', df)#+ p-value = 0.04262
wilcox.test(df$CRP~M.P2!='Хор', df)#-
# ФРН и MMSE по исходам
wilcox.test(df$ФРН~M.P2!='Неуд', df)#-
wilcox.test(df$ФРН~M.P2!='Уд', df)#-
wilcox.test(df$ФРН~M.P2!='Хор', df)#-