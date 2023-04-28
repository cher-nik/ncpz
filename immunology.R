immun <- read.csv("immunology.csv")
sum.immun <- summary(immun[,3:7])
# 1 - 1 кластер, 2-NA (риск), 3- 2 кластер, 4 - NA
immun$risk <- c(2,1,2,1,4,3,3,4,4,2,4,1)
immun$risk <- as.factor(immun$risk)

hist(immun$LE, breaks = 5, 
     main = "Гистограмма показателей лейкоцитарной эластазы",
     xlab = "Энзиматическая активность ЛЭ, нмоль/мин*мл",
     ylab = "Частота, к-во набл.")
hist(immun$О.1.PI, breaks = 5,
     main = "Гистограмма показателей а1-протеиназного ингибитора",
     xlab = "Функциональная активность а1-ПИ, ИЕ/мл",
     ylab = "Частота, к-во набл.")
hist(immun$S.100b, breaks = 5,
     main = "Гистограмма уровня аутоантител к S-100B",
     xlab = "Показатель оптической плотности, ЕОП",
     ylab = "Частота, к-во набл.")
hist(immun$GMP, breaks = 5,
     main = "Гистограмма уровня аутоантител к основному белку 
     миелина",
     xlab = "Показатель оптической плотности, ЕОП",
     ylab = "Частота, к-во набл.")

plot(immun$LE, immun$О.1.PI, type = 'n', main = "График рассеивания
     показателей активности ЛЭ и а1-ПИ
     Нолики - группа риска по FCSRT",
     xlab = 'ЛЭ',
     ylab = "a1-ПИ")
points(immun$LE[immun$risk==1], 
       immun$О.1.PI[immun$risk==1], pch=3)
points(immun$LE[immun$risk==2], 
       immun$О.1.PI[immun$risk==2], pch=1)
points(immun$LE[immun$risk==3], 
       immun$О.1.PI[immun$risk==3], pch=2)
points(immun$LE[immun$risk==4], 
       immun$О.1.PI[immun$risk==4], pch=4)
abline(h=49.3, col="red")
# 1 - нолик - 1 кластер, 2-треуг-NA (риск), 3-плюс- 2 кластер, 4 -крест- NA

plot(immun$S.100b, immun$GMP, type = 'n', main = "График рассеивания
     показателей уровня аутоантител к S-100B и ОБМ
     Нолики - группа риска по FCSRT",
     xlab = 'S-100B',
     ylab = "ОБМ", axes = T)
points(immun$S.100b[immun$risk==0], 
       immun$GMP[immun$risk==0], pch=3)
points(immun$S.100b[immun$risk==1], 
       immun$GMP[immun$risk==1], pch=1)
abline(h=0.68, col="red")
abline(v=0.7, col="blue")
polygon(с(0:0.7, 0, 0), c(0:0.68, 0, 0), col = "blue")

plot(immun$risk, immun$LE, xlab="Группы", ylab = "ЛЭ")
plot(immun$risk, immun$О.1.PI, xlab="Группы", ylab = "ПИ")
plot(immun$risk, immun$PII, xlab="Группы", ylab = "Индекс")
plot(immun$risk, immun$S.100b, xlab="Группы", ylab = "S-100B")
plot(immun$risk, immun$GMP, xlab="Группы", ylab = "ОБМ")

# 1 - 1 кластер, 2-NA (риск), 3- 2 кластер, 4 - NA
