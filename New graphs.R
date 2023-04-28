## Импорт данных
setwd("C:/Users/Darcy/Desktop/R/Kolykhalov")
install.packages("ggpubr")
library(ggplot2)
library(ggpubr)
library(tidyverse)

set.seed(1234)
data <- data.frame(
  sex = factor(rep(c("F", "M"), each=200)),
  weight = c(rnorm(200, 55), rnorm(200, 58)))
head(data, 4)

#### Density plot with mean lines and marginal rug ####
# Change outline and fill colors by groups ("sex")
ggdensity(data, x = "weight",
          add = "mean", rug = TRUE,
          color = "sex", fill = "sex",
          palette = c("#00AFBB", "#E7B800"))

# Bars
gghistogram(data, x = "weight",
            add = "mean", rug = TRUE,
            color = "sex", fill = "sex",
            palette = c("#00AFBB", "#E7B800"))


#### Boxplots and p-value comparing grouping ####
data("ToothGrowth")
df <- ToothGrowth
ggboxplot(df, x = "dose", y = "len",
          color = "dose", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
          add = "jitter", shape = "dose")

# Add p-values comparing groups
# Specify the comparisons you want
my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )
ggboxplot(df, x = "dose", y = "len",
          color = "dose", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
          add = "jitter", shape = "dose") + 
  stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)                   # Add global p-value and place on y-axis = 50

# Violin plots with box plots inside
# Change fill color by groups: dose
# add boxplot with white fill color

ggviolin(df, x = "dose", y = "len", fill = "dose",
         palette = c("#00AFBB", "#E7B800", "#FC4E07"),
         add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ # Add significance levels
  stat_compare_means(label.y = 50)                                      # Add global the p-value 

#### My trial ####
setwd("C:/Users/Darcy/Desktop/R/Kolykhalov")
dataset <- read.csv("dataset_ig.csv", sep = ";")
df <- dataset[1:30, 
              -c(1, 6, 7, 8, 9, 11, 12, 13, 14, 27, 
                 30, 34, 40, 45, 51, 56, 61, 66)
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
df$haml <- cut(df$haml, 3, labels=c("Хор", "Уд", "Неуд"))

# Return to monke
df$bost_r <- dataset[,32]
df$mmse_r <- dataset[,47]
df$dere_r <- dataset[,67]

## Visualisations
# Change outline and fill colors by groups
ggdensity(df, x = "le",
          add = "mean", rug = TRUE,
          color = "apoe", fill = "apoe",
          palette = c("#00AFBB", "#E7B800"))

# Bars
gghistogram(df, x = "le", bins = 8,
            add = "mean", rug = TRUE,
            color = "apoe", fill = "apoe",
            palette = c("#00AFBB", "#E7B800"))

# Boxplots
ggboxplot(df, x = "mmse_r", y = "le",
          color = "mmse_r", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
          add = "jitter", shape = "mmse_r", na.omit = T)

# Le x MMSE
my_comparisons <- list(c("Неуд", "Уд"), c("Уд", "Хор"), c("Неуд", "Хор"))
p1 <- ggboxplot(na.omit(df[,c("mmse_r","le")]), x = "mmse_r", y = "le",
          color = "mmse_r", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
          add = "jitter", shape = "mmse_r", 
           ) + 
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test") + 
  stat_compare_means(label.y = 100)
  ggpar(p1, main = "Соотношение активности ЛЭ с динамикой\nкогнитивных показателей",
        xlab = "Эффективность курса", ylab = "Активность ЛЭ",
        legend.title = "")
  
  
# Le x Apoe
  my_comparisons2 <- list(c("Good", "Bad"))
  p2 <- ggboxplot(na.omit(df[,c("apoe","le")]), x = "apoe", y = "le",
                  color = "apoe", palette =c("#00AFBB", "#FC4E07"),
                  add = "jitter", shape = "apoe", 
  ) + 
    stat_compare_means(comparisons = my_comparisons2, method = "wilcox.test") + 
    stat_compare_means(label.y = 140)
  ggpar(p2, main = "Соотношение активности ЛЭ с вариантом\nгенотипа АроЕ",
        xlab = "Вариант генотипа", ylab = "Активность ЛЭ",
        legend.title = "")

  
# Le x Hamilton
  p3 <- ggboxplot(na.omit(df[,c("haml","le")]), x = "haml", y = "le",
                  color = "haml", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                  add = "jitter", shape = "haml", 
  ) + 
    stat_compare_means(comparisons = my_comparisons, method = "wilcox.test") + 
    stat_compare_means(label.y = 100)
  ggpar(p3, main = "Соотношение активности ЛЭ с выраженностью\nдепрессивных симптомов",
        xlab = "Оценка депрессивного статуса", ylab = "Активность ЛЭ",
        legend.title = "")
  
  # Le x 10 words (immediate recall)
  p4 <- ggboxplot(na.omit(df[,c("tenw_r","le")]), x = "tenw_r", y = "le",
                  color = "tenw_r", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                  add = "jitter", shape = "tenw_r", 
  ) + 
    stat_compare_means(comparisons = my_comparisons, method = "wilcox.test") + 
    stat_compare_means(label.y = 100)
  ggpar(p4, main = "Соотношение активности ЛЭ с результативностью\nзапоминания 10 слов",
        xlab = "Результативность запоминания 10 слов", ylab = "Активность ЛЭ",
        legend.title = "")
  
  # Le x 10 words (delayed recall)
  p5 <- ggboxplot(na.omit(df[,c("dere_r","le")]), x = "dere_r", y = "le",
                  color = "dere_r", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                  add = "jitter", shape = "dere_r", 
  ) + 
    stat_compare_means(comparisons = my_comparisons, method = "wilcox.test") + 
    stat_compare_means(label.y = 100)
  ggpar(p5, main = "Соотношение активности ЛЭ с результативностью\nотсроченного воспроизведения 10 слов",
        xlab = "Результативность отсроченного воспроизведения 10 слов", ylab = "Активность ЛЭ",
        legend.title = "")
  
  # Le x auditory associations
  p6 <- ggboxplot(na.omit(df[,c("auas_r","le")]), x = "auas_r", y = "le",
                  color = "auas_r", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                  add = "jitter", shape = "auas_r", 
  ) + 
    stat_compare_means(comparisons = my_comparisons, method = "wilcox.test") + 
    stat_compare_means(label.y = 100)
  ggpar(p6, main = "Соотношение активности ЛЭ с изменением количества\nлитеральных ассоциаций",
        xlab = "Динамика количества литеральных ассоциаций", ylab = "Активность ЛЭ",
        legend.title = "")
  
  # Le x categorical associations
  p7 <- ggboxplot(na.omit(df[,c("caas_r","le")]), x = "caas_r", y = "le",
                  color = "caas_r", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                  add = "jitter", shape = "caas_r", 
  ) + 
    stat_compare_means(comparisons = my_comparisons, method = "wilcox.test") + 
    stat_compare_means(label.y = 100)
  ggpar(p7, main = "Соотношение активности ЛЭ с изменением количества\nкатегориальных ассоциаций",
        xlab = "Динамика количества категориальных ассоциаций", ylab = "Активность ЛЭ",
        legend.title = "")
  
  # Le x boston test
  p8 <- ggboxplot(na.omit(df[,c("bost_r","le")]), x = "bost_r", y = "le",
                  color = "bost_r", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                  add = "jitter", shape = "bost_r", 
  ) + 
    stat_compare_means(comparisons = my_comparisons, 
                       method = "wilcox.test", label = "p.signif") + 
    stat_compare_means(label.y = 140)
  ggpar(p8, main = "Соотношение активности ЛЭ с динамикой показателей\nбостонского теста",
        xlab = "Динамика показателей бостонского теста", ylab = "Активность ЛЭ",
        legend.title = "")
  
  
  ## Paired Wilcox and t-tests  ####
  library(rstatix)
# MMSE
  df.mmse <- df %>%
  gather(key = "gr_mmse", value = "mmse", mmse_1, mmse_3) #Create long data

df.mmse %>%
  group_by(gr_mmse) %>%
  get_summary_stats(mmse, type = "mean_sd")               #Get summary stats

d <- with(df.mmse,
          mmse[gr_mmse == "mmse_1"] - mmse[gr_mmse == "mmse_3"]) #Check for normality
shapiro.test(d) # p-value 0.03 and is less than 0.05, which means data is NOT normally distributed.

stat.test <- wilcox.test(mmse~gr_mmse, data=subset(df.mmse[-c(20,50),]), 
                         paired = T, alternative="less")  #Calculate p-value with alternative hypothesis that M1<M2
stat.test$p.value #P-value < 0.0001

df.mmse %>% cohens_d(mmse~gr_mmse, paired = T) #Negative ES means M1<M2

bxp <- ggpaired(df.mmse, x="gr_mmse", y="mmse",
                order=c("mmse_1","mmse_3"),
                ylab="MMSE", xlab="Groups",
                color = "gr_mmse", palette =c("#00AFBB", "#FC4E07"))


# boston test
df.bost <- df %>%
  gather(key = "gr_bost", value = "bost", bost_1, bost_3) #Create long data

df.bost %>%
  group_by(gr_bost) %>%
  get_summary_stats(bost, type = "mean_sd")               #Get summary stats

d <- with(df.bost,
          bost[gr_bost == "bost_1"] - bost[gr_bost == "bost_3"]) #Check for normality
shapiro.test(d) # p-value 0.8 and is greater than 0.05, which means data is NORMALLY distributed.

stat.test <- df.bost %>%
  t_test(bost~gr_bost, paired = T, alternative = "less")  #Calculate p-value with alternative hypothesis that M1<M2
stat.test$p #P-value < 0.0001

df.bost %>% cohens_d(bost~gr_bost, paired = T) #Negative ES means M1<M2

bxp <- ggpaired(df.bost, x="gr_bost", y="bost",
                order=c("bost_1","bost_3"),
                ylab="Boston test", xlab="Groups",
                color = "gr_bost", fill = "grey", palette =c("#00AFBB", "#FC4E07"))
stat.test <- stat.test %>% add_xy_position(x = "gr_bost")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed= TRUE))


# ШЛД
df.sld <- df %>%
  gather(key = "gr_sld", value = "sld", sld_1, sld_3) 

df.sld %>%
  group_by(gr_sld) %>%
  get_summary_stats(sld, type = "mean_sd")               

d <- with(df.sld,
          sld[gr_sld == "sld_1"] - sld[gr_sld == "sld_3"]) 
shapiro.test(d)             #p-value < 0.05, data is NOT normally distributed

stat.test <- wilcox.test(sld~gr_sld, data=subset(df.sld[-c(29,59),]), 
                         paired = T, alternative="less")  #alternative hypothesis M1<M2
stat.test$p.value #P-value = 0.18 NS

df.sld %>% cohens_d(sld~gr_sld, paired = T) #Small effect size means data differencies are weak


# ШЛД
df.tenw <- df %>%
  gather(key = "gr_tenw", value = "tenw", tenw_1, tenw_3) 

df.tenw %>%
  group_by(gr_tenw) %>%
  get_summary_stats(tenw, type = "mean_sd")               

d <- with(df.tenw,
          tenw[gr_tenw == "tenw_1"] - tenw[gr_tenw == "tenw_3"]) 
shapiro.test(d)             #p-value > 0.05, data is NORMALLY distributed

stat.test <- df.tenw %>%
  t_test(tenw~gr_tenw, paired = T, alternative = "less")  #alternative hypothesis M1<M2
stat.test$p #P-value < 0.001

df.tenw %>% cohens_d(tenw~gr_tenw, paired = T) #Moderate ES

bxp <- ggpaired(df.tenw, x="gr_tenw", y="tenw",
                order=c("tenw_1","tenw_3"),
                ylab="10 words test", xlab="Groups",
                color = "gr_tenw", fill = "grey", palette =c("#00AFBB", "#FC4E07"))
stat.test <- stat.test %>% add_xy_position(x = "gr_tenw")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed= TRUE))


# Delayed recall
df.dere <- df %>%
  gather(key = "gr_dere", value = "dere", dere_1, dere_3) 

df.dere %>%
  group_by(gr_dere) %>%
  get_summary_stats(dere, type = "mean_sd")               

d <- with(df.dere,
          dere[gr_dere == "dere_1"] - dere[gr_dere == "dere_3"]) 
shapiro.test(d) #p-value < 0.05, means data is NOT normally distributed

stat.test <- wilcox.test(dere~gr_dere, data=subset(df.dere[-c(20,50),]), 
                         paired = T, alternative="less")  #alternative hypothesis M1<M2
stat.test$p.value #P-value < 0.001

df.dere %>% cohens_d(dere~gr_dere, paired = T) #large ES
bxp <- ggpaired(df.dere, x="gr_dere", y="dere",
                order=c("dere_1","dere_3"),
                ylab="Delayed recall", xlab="Groups",
                color = "gr_mmse", palette =c("#00AFBB", "#FC4E07"))


# Auditory associations
df.auas <- df %>%
  gather(key = "gr_auas", value = "auas", auas_1, auas_3) 

df.auas %>%
  group_by(gr_auas) %>%
  get_summary_stats(auas, type = "mean_sd")               

d <- with(df.auas,
          auas[gr_auas == "auas_1"] - auas[gr_auas == "auas_3"]) 
shapiro.test(d) #p-value > 0.5 means data is NORMALLY distributed

stat.test <- df.auas %>%
  t_test(auas~gr_auas, paired = T, alternative = "less")  #alternative hypothesis M1<M2
stat.test$p #P-value < 0.0001

df.auas %>% cohens_d(auas~gr_auas, paired = T) #Large ES

bxp <- ggpaired(df.auas, x="gr_auas", y="auas",
                order=c("auas_1","auas_3"),
                ylab="Auditory associations", xlab="Groups",
                color = "gr_auas", fill = "grey", palette =c("#00AFBB", "#FC4E07"))
stat.test <- stat.test %>% add_xy_position(x = "gr_auas")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed= TRUE))


# Categorical associations
df.caas <- df %>%
  gather(key = "gr_caas", value = "caas", caas_1, caas_3) 

df.caas %>%
  group_by(gr_caas) %>%
  get_summary_stats(caas, type = "mean_sd")               

d <- with(df.caas,
          caas[gr_caas == "caas_1"] - caas[gr_caas == "caas_3"]) 
shapiro.test(d) #p-value < 0.05 means data is NOT normally distributed

stat.test <- wilcox.test(caas~gr_caas, data=df.caas, 
                         paired = T, alternative="less")  #alternative hypothesis M1<M2
stat.test$p.value #P-value < 0.001

df.caas %>% cohens_d(caas~gr_caas, paired = T) #large ES
bxp <- ggpaired(df.caas, x="gr_caas", y="caas",
                order=c("caas_1","caas_3"),
                ylab="Categorical associations", xlab="Groups",
                color = "gr_caas", palette =c("#00AFBB", "#FC4E07"))


# trch
df.trch <- df %>%
  gather(key = "gr_trch", value = "trch", trch_1, trch_3) 

df.trch %>%
  group_by(gr_trch) %>%
  get_summary_stats(trch, type = "mean_sd")               

d <- with(df.trch,
          trch[gr_trch == "trch_1"] - trch[gr_trch == "trch_3"]) 
shapiro.test(d) #p-value < 0.05 means data is NOT normally distributed

stat.test <- wilcox.test(trch~gr_trch, data=df.trch, 
                         paired = T, alternative="less")  #alternative hypothesis M1<M2
stat.test$p.value #P-value < 0.05

df.trch %>% cohens_d(trch~gr_trch, paired = T) #small ES
bxp <- ggpaired(df.trch, x="gr_trch", y="trch",
                order=c("trch_1","trch_3"),
                ylab="", xlab="Groups",
                color = "gr_trch", palette =c("#00AFBB", "#FC4E07"))


# New group conditions ####

df$gr_cond <- ifelse(
  df$mmse_r > 0 &
  df$dere_r > 0 & 
  df$bost_r >= 3, "Уд", "Неуд") #too few cases

cond_1 <- df$mmse_r > 0 & df$dere_r > 0
cond_2 <- df$dere_r > 0 & df$bost_r >= 3
cond_3 <- df$mmse_r > 0 & df$bost_r >= 3

df$gr_cond_2 <- ifelse(cond_1 | cond_2 | cond_3, "Уд", "Неуд")


# LE
my_comparisons3 <- list(c("Уд", "Неуд"))
p3 <- ggboxplot(na.omit(df[,c("gr_cond_2","le")]), x = "gr_cond_2", y = "le",
                color = "gr_cond_2", palette =c("#00AFBB", "#FC4E07"),
                add = "jitter", shape = "gr_cond_2", 
) + 
  stat_compare_means(comparisons = my_comparisons3, method = "wilcox.test") + 
  stat_compare_means(label.y = 140)
ggpar(p3, main = "Соотношение активности ЛЭ с вариантом\nисхода после терапии",
      xlab = "Вариант исхода", ylab = "Активность ЛЭ",
      legend.title = "")


#PI
df$pi <- as.numeric(sub(",", ".", dataset[,12], fixed = TRUE))
df$pile <- as.numeric(sub(",", ".", dataset[,13], fixed = TRUE))

p4 <- ggboxplot(na.omit(df[,c("gr_cond_2","pi")]), x = "gr_cond_2", y = "pi",
                color = "gr_cond_2", palette =c("#00AFBB", "#FC4E07"),
                add = "jitter", shape = "gr_cond_2", 
) + 
  stat_compare_means(comparisons = my_comparisons3, method = "wilcox.test") + 
  stat_compare_means(label.y = 68)
ggpar(p4, main = "Соотношение активности ПИ с вариантом\nисхода после терапии",
      xlab = "Вариант исхода", ylab = "Активность ПИ",
      legend.title = "")

#PII
p5 <- ggboxplot(na.omit(df[,c("gr_cond_2","pile")]), x = "gr_cond_2", y = "pile",
                color = "gr_cond_2", palette =c("#00AFBB", "#FC4E07"),
                add = "jitter", shape = "gr_cond_2", 
) + 
  stat_compare_means(comparisons = my_comparisons3, method = "wilcox.test") + 
  stat_compare_means(label.y = 6)
ggpar(p5, main = "Соотношение ингибиторного индекса с вариантом\nисхода после терапии",
      xlab = "Вариант исхода", ylab = "Ингибиторный индекс",
      legend.title = "")

wilcox.test(df[,53]~df[,51])
