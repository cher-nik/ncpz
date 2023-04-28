library(ggplot2)
library(ggpubr)
library(tidyverse)
library(psych)
setwd("D:/MHC/STATISTICS/MRI")
df <- read.csv("MRI.csv.csv", sep = ",", dec = ".")
df <- subset(df[,-c(1,2,5,6,23,35,37,38,39,40,41)])

colnames(df) <- c("sex","age","group","mmse","moca_tot","moca_vs","moca_n",
                  "moca_at","moca_cal","moca_sp","moca_ab","moca_mem","moca_or",
                  "gca","wml","mta","pca","fro","mrs_a_cr","mrs_a_naa","mrs_a_glx",
                  "mrs_a_ins","mrs_a_cho","mrs_a_naami","mrs_r_naacr","mrs_r_gxcr",
                  "mrs_r_micr","mrs_r_chocr","mrs_r_naamicr","cbf")

df[,'group'] <- as.factor(as.character(df[,'group']))
df[,'wml'] <- as.factor(as.character(df[,'wml']))
df[,'mta'] <- as.factor(as.character(df[,'mta']))
df[,'pca'] <- as.factor(as.character(df[,'pca']))
df[,'fro'] <- as.factor(as.character(df[,'fro']))

dataset <- read.csv("totaldata.csv", sep = ";", dec = ",")
df2 <- subset(dataset[,-2])

df2$mbi_gr10f <- factor(df2$mbi_gr10, levels = list("1","2","3"))
df2$mbi_gr6f <- factor(df2$mbi_gr6, levels = list("1","2","3"))
df2$fcsrt_grf <- factor(df2$fcsrt_gr, levels = list("1","2","3"))
df2$sexf <- factor(df2$sex, levels = list("f","m"), labels = c("Female","Male"))
df2$control <- 0
df3 <- read.csv("immunology control.csv", sep = ";", dec = ",")
df3$control <- 1
df4 <- merge(df2,df3,all=T)
## Defining comparison groups ##
#Clinical
my_comparisons <- list(c("1", "2"), c("2", "3"), c("1", "3"))
#Sex
my_comparisons2 <- list(c("Female", "Male"))

##### Visualization of MRI data #####
# GCA(Pasquier) between groups
p1 <- ggboxplot(na.omit(df[,c("group","gca")]), x = "group", y = "gca",
                color = "group", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                add = "jitter", shape = "group", 
)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test",
                     symnum.args = list(
                       cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                       symbols = c("****", "***", "**", "*", "ns")
                       )
                     ) + 
  stat_compare_means(label.y = 25)
ggpar(p1, main = "Показатели шкалы глобальной корковой атрофии по группам",
      xlab = "Группы, 1 - Контроль, 2 - MCI, 3 - Деменция\n* - p < 0.05; **** - p < 0.0001", 
      ylab = "Балл GCA",
      legend.title = "Группы:")

# WML(Fazekas) between groups
# Two categorical variables need chi-square test
# Preparing contingency table

attach(df)
table(group,wml)
prop.table(table(group,wml),1)
chisq.test(table(group,wml),correct=FALSE)
p2 <- ggplot(na.omit(df[,c("wml","group")]), aes(wml, fill=group)) +
  geom_bar(position = "fill")+ 
  bgcolor("#FFFFFF")+ 
  grids(linetype = "dashed")
ggpar(p2, main = "Показатели шкалы поражения белого вещества (Fazekas) по группам",
      xlab = "Градация по Fazekas", 
      ylab = "% от общего количества",
      legend.title = "Группы:",
      caption = "Pearson's Chi-squared test (X-squared = 18.29, df = 6)\np = 0.005")

# MTA(Scheltens) between groups
# Two categorical variables need chi-square test
# Preparing contingency table

table(group,mta)
prop.table(table(group,mta),1)
chisq.test(table(group,mta),correct=FALSE)
p3 <- ggplot(na.omit(df[,c("mta","group")]), aes(mta, fill=group)) +
  geom_bar(position = "fill")+ 
  bgcolor("#FFFFFF")+ 
  grids(linetype = "dashed")
ggpar(p3, main = "Показатели выраженности височной атрофии (Scheltens) по группам",
      xlab = "Градация по Scheltens", 
      ylab = "% от общего количества",
      legend.title = "Группы:",
      caption = "Pearson's Chi-squared test (X-squared = 24.932, df = 6)\np = 0.0003")

# PCA(Koedam) between groups
# Two categorical variables need chi-square test
# Preparing contingency table

table(group,pca)
prop.table(table(group,pca),1)
chisq.test(table(group,pca),correct=FALSE)
p4 <- ggplot(na.omit(df[,c("pca","group")]), aes(pca, fill=group)) +
  geom_bar(position = "fill")+ 
  bgcolor("#FFFFFF")+ 
  grids(linetype = "dashed")
ggpar(p4, main = "Показатели выраженности теменной атрофии (Koedam) по группам",
      xlab = "Градация по Koedam", 
      ylab = "% от общего количества",
      legend.title = "Группы:",
      caption = "Pearson's Chi-squared test (X-squared = 15.203, df = 6)\np = 0.01")

# Frontal atrophy between groups
# Two categorical variables need chi-square test
# Preparing contingency table

table(group,fro)
prop.table(table(group,fro),1)
chisq.test(table(group,fro),correct=FALSE)
p5 <- ggplot(na.omit(df[,c("fro","group")]), aes(fro, fill=group)) +
  geom_bar(position = "fill")+ 
  bgcolor("#FFFFFF")+ 
  grids(linetype = "dashed")
ggpar(p5, main = "Показатели выраженности лобной атрофии по группам",
      xlab = "Степени лобной атрофии 0-отс., 1-слабо, 2-умер., 3-выраж.", 
      ylab = "% от общего количества",
      legend.title = "Группы:",
      caption = "Pearson's Chi-squared test (X-squared = 17.377, df = 6)\np = 0.007")

# CBF in GM between groups
p6 <- ggboxplot(na.omit(df[,c("group","cbf")]), x = "group", y = "cbf",
                color = "group", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                add = "jitter", shape = "group", 
)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test",
                     symnum.args = list(
                       cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                       symbols = c("****", "***", "**", "*", "ns")
                     )
  ) + 
  stat_compare_means(label.y = 95)
ggpar(p6, main = "Показатели скорости церебрального кровотока в сером веществе\nсреди обследованных",
      xlab = "Группы, 1 - Контроль, 2 - MCI, 3 - Деменция\nns- p > 0.05; ** - p < 0.01", 
      ylab = "CBF in GM (ml/(min*100g tissue)",
      legend.title = "Группы:")

#### Visualization of full data ####

# MBI and amnestic groups # FAILED
# Two categorical variables need chi-square test
# Preparing contingency table

attach(df2)
table(mbi_gr6f,fcsrt_grf)
prop.table(table(mbi_gr10f,fcsrt_grf),1)
chisq.test(table(mbi_gr10f,fcsrt_grf),correct=FALSE)
p7 <- ggplot(na.omit(df2[,c("mbi_gr10f","fcsrt_grf")]), 
             aes(mbi_gr10f, fill=fcsrt_grf)) +
  geom_bar(position = "dodge")+ 
  bgcolor("#FFFFFF")+ 
  grids(linetype = "dashed")
ggpar(p7, main = "Соотношение поведенческих нарушений с амнестическим типом MCI",
      xlab = "MCI: 1-неамнест., 2-амнест., 3-погран. ", 
      ylab = "MBI: 1-аффект., 2-импульс., 3-отсут.",
      legend.title = "Группы:",
      caption = "Pearson's Chi-squared test (X-squared = 17.377, df = 6)\np = 0.007")

# MBI groups and FCSRToi3 #FAILED
p8 <- ggviolin(df2, x = "mbi_gr10f", y = "oi3", fill = "mbi_gr10f",
         palette = c("#00AFBB", "#E7B800", "#FC4E07"),
         add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ # Add significance levels
  stat_compare_means(label.y = 50)  
# MBI groups and FCSRTsp3 #FAILED
p9 <- ggviolin(df2, x = "mbi_gr10f", y = "sp3", fill = "mbi_gr10f",
               palette = c("#00AFBB", "#E7B800", "#FC4E07"),
               add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ # Add significance levels
  stat_compare_means(label.y = 100)  
# MBI groups and FCSRTip #FAILED
p10 <- ggviolin(df2, x = "mbi_gr10f", y = "ip", fill = "mbi_gr10f",
               palette = c("#00AFBB", "#E7B800", "#FC4E07"),
               add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ # Add significance levels
  stat_compare_means(label.y = 1) 

## Neuroimmunology ##
# LE in MBI #FAILED
p11 <- ggboxplot(na.omit(df2[,c("mbi_gr6f","le")]), x = "mbi_gr6f", y = "le",
                color = "mbi_gr6f", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                add = "jitter", shape = "mbi_gr6f", 
)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test",
                     symnum.args = list(
                       cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                       symbols = c("****", "***", "**", "*", "ns")
                     )
  ) + 
  stat_compare_means(label.y = 300)
ggpar(p11, main = "Показатели активности лейкоцитарной эластазы в группах MBI",
      xlab = "MBI: 1-аффект., 2-импульс., 3-отсут.", 
      ylab = "Энзиматическая активность ЛЭ, нмоль/мин*мл",
      legend.title = "Группы:")

# LE in MCI #FAILED
p12 <- ggboxplot(na.omit(df2[,c("fcsrt_grf","le")]), x = "fcsrt_grf", y = "le",
                 color = "fcsrt_grf", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                 add = "jitter", shape = "fcsrt_grf", 
)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test",
                     symnum.args = list(
                       cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                       symbols = c("****", "***", "**", "*", "ns")
                     )
  ) + 
  stat_compare_means(label.y = 300)
ggpar(p12, main = "Показатели активности лейкоцитарной эластазы в группах MCI",
      xlab = "MCI: 1 - неамнест., 2 - амнест., 3 - погран.", 
      ylab = "Энзиматическая активность ЛЭ, нмоль/мин*мл",
      legend.title = "Группы:")

# A1PI in MBI #FAILED
p13 <- ggboxplot(na.omit(df2[,c("mbi_gr6f","a1pi")]), x = "mbi_gr6f", y = "a1pi",
                 color = "mbi_gr6f", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                 add = "jitter", shape = "mbi_gr6f", 
)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test",
                     symnum.args = list(
                       cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                       symbols = c("****", "***", "**", "*", "ns")
                     )
  ) + 
  stat_compare_means(label.y = 50)
ggpar(p13, main = "Показатели активности а1-протеиназного ингибитора в группах MBI",
      xlab = "MBI: 1-аффект., 2-импульс., 3-отсут.", 
      ylab = "Функциональная активность а1-ПИ, ИЕ/мл",
      legend.title = "Группы:")

# a1pi in MCI #FAILED
p14 <- ggboxplot(na.omit(df2[,c("fcsrt_grf","a1pi")]), x = "fcsrt_grf", y = "a1pi",
                 color = "fcsrt_grf", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                 add = "jitter", shape = "fcsrt_grf", 
)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test",
                     symnum.args = list(
                       cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                       symbols = c("****", "***", "**", "*", "ns")
                     )
  ) + 
  stat_compare_means(label.y = 50)
ggpar(p14, main = "Показатели активности а1-протеиназного ингибитора в группах MCI",
      xlab = "MCI: 1 - неамнест., 2 - амнест., 3 - погран.", 
      ylab = "Функциональная активность а1-ПИ, ИЕ/мл",
      legend.title = "Группы:")

# pii in MBI #FAILED
p15 <- ggboxplot(na.omit(df2[,c("mbi_gr6f","pii")]), x = "mbi_gr6f", y = "pii",
                 color = "mbi_gr6f", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                 add = "jitter", shape = "mbi_gr6f", 
)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test",
                     symnum.args = list(
                       cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                       symbols = c("****", "***", "**", "*", "ns")
                     )
  ) + 
  stat_compare_means(label.y = 6)
ggpar(p15, main = "Показатели ПИИ в группах MBI",
      xlab = "MBI: 1-аффект., 2-импульс., 3-отсут.", 
      ylab = "ПИИ, ед.",
      legend.title = "Группы:")

# pii in MCI #FAILED
p16 <- ggboxplot(na.omit(df2[,c("fcsrt_grf","pii")]), x = "fcsrt_grf", y = "pii",
                 color = "fcsrt_grf", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                 add = "jitter", shape = "fcsrt_grf", 
)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test",
                     symnum.args = list(
                       cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                       symbols = c("****", "***", "**", "*", "ns")
                     )
  ) + 
  stat_compare_means(label.y = 6)
ggpar(p16, main = "Показатели ПИИ в группах MCI",
      xlab = "MCI: 1 - неамнест., 2 - амнест., 3 - погран.", 
      ylab = "ПИИ, ед.",
      legend.title = "Группы:")

# S100b in MBI #FAILED
p17 <- ggboxplot(na.omit(df2[,c("mbi_gr6f","s100b")]), x = "mbi_gr6f", y = "s100b",
                 color = "mbi_gr6f", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                 add = "jitter", shape = "mbi_gr6f", 
)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test",
                     symnum.args = list(
                       cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                       symbols = c("****", "***", "**", "*", "ns")
                     )
  ) + 
  stat_compare_means(label.y = 6)
ggpar(p17, main = "Показатели уровня аутоантител S100b в группах MBI",
      xlab = "MBI: 1-аффект., 2-импульс., 3-отсут.", 
      ylab = "Показатель оптической плотности, ЕОП",
      legend.title = "Группы:")

# s100b in MCI #FAILED
p18 <- ggboxplot(na.omit(df2[,c("fcsrt_grf","s100b")]), x = "fcsrt_grf", y = "s100b",
                 color = "fcsrt_grf", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                 add = "jitter", shape = "fcsrt_grf", 
)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test",
                     symnum.args = list(
                       cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                       symbols = c("****", "***", "**", "*", "ns")
                     )
  ) + 
  stat_compare_means(label.y = 6)
ggpar(p18, main = "Показатели ПИИ в группах MCI",
      xlab = "MCI: 1 - неамнест., 2 - амнест., 3 - погран.", 
      ylab = "Показатель оптической плотности, ЕОП",
      legend.title = "Группы:")

# GMP in MBI #FAILED
p17 <- ggboxplot(na.omit(df2[,c("mbi_gr6f","gmp")]), x = "mbi_gr6f", y = "gmp",
                 color = "mbi_gr6f", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                 add = "jitter", shape = "mbi_gr6f", 
)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test",
                     symnum.args = list(
                       cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                       symbols = c("****", "***", "**", "*", "ns")
                     )
  ) + 
  stat_compare_means(label.y = 2)
ggpar(p17, main = "Показатели уровня аутоантител gmp в группах MBI",
      xlab = "MBI: 1-аффект., 2-импульс., 3-отсут.", 
      ylab = "Показатель оптической плотности, ЕОП",
      legend.title = "Группы:")

# GMP in MCI #FAILED
p18 <- ggboxplot(na.omit(df2[,c("fcsrt_grf","gmp")]), x = "fcsrt_grf", y = "gmp",
                 color = "fcsrt_grf", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                 add = "jitter", shape = "fcsrt_grf", 
)+
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test",
                     symnum.args = list(
                       cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                       symbols = c("****", "***", "**", "*", "ns")
                     )
  ) + 
  stat_compare_means(label.y = 2)
ggpar(p18, main = "Показатели уровня аутоантител GMP в группах MCI",
      xlab = "MCI: 1 - неамнест., 2 - амнест., 3 - погран.", 
      ylab = "Показатель оптической плотности, ЕОП",
      legend.title = "Группы:")

#### Point graphs ####
# In MCI

s1 <- ggscatter(df2, x = "le", y = "a1pi", color = "fcsrt_grf",
                size = 5)+
 color_palette(c("#00AFBB","#FC4E07" , "#E7B800"))
s1p <- ggpar(s1, title = "График рассеивания показателей активности ЛЭ и а1-ПИ",
             subtitle = "в 'когнитивных' группах",
             xlab = "ЛЭ, Энзиматическая активность, нмоль/мин*мл",
             ylab = "a1-ПИ, Функциональная активность, ИЕ/мл",
             legend.title = "Группы:")
s2 <- ggscatter(df2, x = "s100b", y = "gmp", color = "fcsrt_grf",
                size = 5)+
  color_palette(c("#00AFBB","#FC4E07" , "#E7B800"))
s2p <- ggpar(s2, title = "График рассеивания показателей уровня аутоантител\n к S-100B и ОБМ",
             subtitle = "в 'когнитивных' группах",
             xlab = "S100b, Показатель оптической плотности, ЕОП",
             ylab = "GMP, Показатель оптической плотности, ЕОП",
             legend.title = "Группы:")
# In MBI

s3 <- ggscatter(df2, x = "le", y = "a1pi", color = "mbi_gr10f",
                size = 5)
s3p <- ggpar(s3, title = "График рассеивания показателей активности ЛЭ и а1-ПИ",
             subtitle = "в 'поведенческих' группах",
             xlab = "ЛЭ, Энзиматическая активность, нмоль/мин*мл",
             ylab = "a1-ПИ, Функциональная активность, ИЕ/мл",
             legend.title = "Группы:",
             palette = "Dark2")
s4 <- ggscatter(df2, x = "s100b", y = "gmp", color = "mbi_gr10f",
                size = 5)
s4p <- ggpar(s4, title = "График рассеивания показателей уровня аутоантител\n к S-100B и ОБМ",
             subtitle = "в 'поведенческих' группах",
             xlab = "S100b, Показатель оптической плотности, ЕОП",
             ylab = "GMP, Показатель оптической плотности, ЕОП",
             legend.title = "Группы:",
             palette = "Dark2")

#### Descriptive stats ####
describe(df2)
describeBy(df2, group = df2$mbi_gr6f)
describeBy(df2, group = df2$mbi_gr10f)
describeBy(df2, group = df2$fcsrt_grf)
#### TRYHARD ANALYSIS ####
library(rstatix)
tb <- as_tibble(df2)
tb2 <- as_tibble(df4)
tb_stats <- tb %>%
  group_by(mbi_gr6f) %>%
  get_summary_stats(type = "full")
## MBI6 ##
# See below #
test.le12 <- subset(tb, mbi_gr6f!="3") %>% 
  rstatix::wilcox_test(le ~ mbi_gr6f) %>%
  add_significance()
# Better to use whole tb %>% #
test.mbi6.le <- tb %>%
  rstatix::wilcox_test(le ~ mbi_gr6f) %>%
  add_significance()
test.mbi6.pi <- tb %>%
  rstatix::wilcox_test(a1pi ~ mbi_gr6f) %>%
  add_significance()
test.mbi6.pii <- tb %>%
  rstatix::wilcox_test(pii ~ mbi_gr6f) %>%
  add_significance()
test.mbi6.s100b <- tb %>%
  rstatix::wilcox_test(s100b ~ mbi_gr6f) %>%
  add_significance()
test.mbi6.gmp <- tb %>% 
  rstatix::wilcox_test(gmp ~ mbi_gr6f) %>%
  add_significance()
test.mbi6.age <- tb %>% 
  rstatix::wilcox_test(age ~ mbi_gr6f) %>%
  add_significance()
test.mbi6.cbf <- tb %>% 
  rstatix::wilcox_test(cbf ~ mbi_gr6f) %>%
  add_significance()
test.mbi6.year <- tb %>% 
  rstatix::wilcox_test(yearssick ~ mbi_gr6f) %>%
  add_significance()
test.mbi6.sp3 <- tb %>% 
  rstatix::wilcox_test(sp3 ~ mbi_gr6f) %>%
  add_significance()

## MBI10 ##
test.mbi10.le <- tb %>%
  rstatix::wilcox_test(le ~ mbi_gr10f) %>%
  add_significance()
test.mbi10.pi <- tb %>%
  rstatix::wilcox_test(a1pi ~ mbi_gr10f) %>%
  add_significance()
test.mbi10.pii <- tb %>%
  rstatix::wilcox_test(pii ~ mbi_gr10f) %>%
  add_significance()
test.mbi10.s100b <- tb %>%
  rstatix::wilcox_test(s100b ~ mbi_gr10f) %>%
  add_significance()
test.mbi10.gmp <- tb %>% 
  rstatix::wilcox_test(gmp ~ mbi_gr10f) %>%
  add_significance()
test.mbi10.age <- tb %>% 
  rstatix::wilcox_test(age ~ mbi_gr10f) %>%
  add_significance()
test.mbi10.cbf <- tb %>% 
  rstatix::wilcox_test(cbf ~ mbi_gr10f) %>%
  add_significance()
test.mbi10.year <- tb %>% 
  rstatix::wilcox_test(yearssick ~ mbi_gr10f) %>%
  add_significance()
test.mbi10.sp3 <- tb %>% 
  rstatix::wilcox_test(sp3 ~ mbi_gr10f) %>%
  add_significance()

## FCSRT ##
test.fcsrt.le <- tb %>%
  rstatix::wilcox_test(le ~ fcsrt_grf) %>%
  add_significance()
test.fcsrt.pi <- tb %>%
  rstatix::wilcox_test(a1pi ~ fcsrt_grf) %>%
  add_significance()
test.fcsrt.pii <- tb %>%
  rstatix::wilcox_test(pii ~ fcsrt_grf) %>%
  add_significance()
test.fcsrt.s100b <- tb %>%
  rstatix::wilcox_test(s100b ~ fcsrt_grf) %>%
  add_significance()
test.fcsrt.gmp <- tb %>% 
  rstatix::wilcox_test(gmp ~ fcsrt_grf) %>%
  add_significance()
test.fcsrt.age <- tb %>% 
  rstatix::wilcox_test(age ~ fcsrt_grf) %>%
  add_significance()
test.fcsrt.cbf <- tb %>% 
  rstatix::wilcox_test(cbf ~ fcsrt_grf) %>%
  add_significance()
test.fcsrt.year <- tb %>% 
  rstatix::wilcox_test(yearssick ~ fcsrt_grf) %>%
  add_significance()
test.fcsrt.sp3 <- tb %>% 
  rstatix::wilcox_test(sp3 ~ fcsrt_grf) %>%
  add_significance()
test.fcsrt.mbi <- tb %>% 
  rstatix::wilcox_test(mbi_tot ~ fcsrt_grf) %>%
  add_significance()
test.fcsrt.cbf <- tb %>% 
  rstatix::wilcox_test(cbf ~ fcsrt_grf) %>%
  add_significance()

## control tests ##
test.le1c <- subset(tb2, mbi_gr6f=="1") %>% 
  rstatix::wilcox_test(le ~ control) %>%
  add_significance()