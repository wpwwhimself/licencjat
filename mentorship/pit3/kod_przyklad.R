setwd("~/Pobrane/kody_dane_WP")

source("kod_testy.R")

dane <- read.table('sauce/Orthosis.txt', header = TRUE)
head(dane)

# warunki 1 i 3
dane1 <- dane[dane$Subject  ==  1, ]
dane21 <- data.frame(rbind(dane1[dane1$Condition  ==  1 & dane1$Replication  ==  1, ]$Moment))
for(i in 2:10) dane21 <- data.frame(rbind(dane21, dane1[dane1$Condition  ==  1 & dane1$Replication  ==  i, ]$Moment))

dane2 <- dane[dane$Subject  ==  2, ]
dane22 <- data.frame(rbind(dane2[dane2$Condition  ==  1 & dane2$Replication == 1, ]$Moment))
for(i in 2:10) dane22 <- data.frame(rbind(dane22, dane2[dane2$Condition == 1 & dane2$Replication == i, ]$Moment))

dane3 <- dane[dane$Subject == 3, ]
dane23 <- data.frame(rbind(dane3[dane3$Condition == 1 & dane3$Replication == 1, ]$Moment))
for(i in 2:10) dane23 <- data.frame(rbind(dane23, dane3[dane3$Condition == 1 & dane3$Replication == i, ]$Moment))

dane4 <- dane[dane$Subject == 4, ]
dane24 <- data.frame(rbind(dane4[dane4$Condition == 1 & dane4$Replication == 1, ]$Moment))
for(i in 2:10) dane24 <- data.frame(rbind(dane24, dane4[dane4$Condition == 1 & dane4$Replication == i, ]$Moment))

dane5 <- dane[dane$Subject == 5, ]
dane25 <- data.frame(rbind(dane5[dane5$Condition == 1 & dane5$Replication == 1, ]$Moment))
for(i in 2:10) dane25 <- data.frame(rbind(dane25, dane5[dane5$Condition == 1 & dane5$Replication == i, ]$Moment))

dane6 <- dane[dane$Subject == 6, ]
dane26 <- data.frame(rbind(dane6[dane6$Condition == 1 & dane6$Replication == 1, ]$Moment))
for(i in 2:10) dane26 <- data.frame(rbind(dane26, dane6[dane6$Condition == 1 & dane6$Replication == i, ]$Moment))

dane7 <- dane[dane$Subject == 7, ]
dane27 <- data.frame(rbind(dane7[dane7$Condition == 1 & dane7$Replication == 1, ]$Moment))
for(i in 2:10) dane27 <- data.frame(rbind(dane27, dane7[dane7$Condition == 1 & dane7$Replication == i, ]$Moment))

yy1 <- data.frame(rbind(colMeans(dane21), 
                        colMeans(dane22), 
                        colMeans(dane23), 
                        colMeans(dane24), 
                        colMeans(dane25), 
                        colMeans(dane26), 
                        colMeans(dane27)))

dane1 <- dane[dane$Subject == 1, ]
dane21 <- data.frame(rbind(dane1[dane1$Condition == 3 & dane1$Replication == 1, ]$Moment))
for(i in 2:10) dane21 <- data.frame(rbind(dane21, dane1[dane1$Condition == 3 & dane1$Replication == i, ]$Moment))

dane2 <- dane[dane$Subject == 2, ]
dane22 <- data.frame(rbind(dane2[dane2$Condition == 3 & dane2$Replication == 1, ]$Moment))
for(i in 2:10) dane22 <- data.frame(rbind(dane22, dane2[dane2$Condition == 3 & dane2$Replication == i, ]$Moment))

dane3 <- dane[dane$Subject == 3, ]
dane23 <- data.frame(rbind(dane3[dane3$Condition == 3 & dane3$Replication == 1, ]$Moment))
for(i in 2:10) dane23 <- data.frame(rbind(dane23, dane3[dane3$Condition == 3 & dane3$Replication == i, ]$Moment))

dane4 <- dane[dane$Subject == 4, ]
dane24 <- data.frame(rbind(dane4[dane4$Condition == 3 & dane4$Replication == 1, ]$Moment))
for(i in 2:10) dane24 <- data.frame(rbind(dane24, dane4[dane4$Condition == 3 & dane4$Replication == i, ]$Moment))

dane5 <- dane[dane$Subject == 5, ]
dane25 <- data.frame(rbind(dane5[dane5$Condition == 3 & dane5$Replication == 1, ]$Moment))
for(i in 2:10) dane25 <- data.frame(rbind(dane25, dane5[dane5$Condition == 3 & dane5$Replication == i, ]$Moment))

dane6 <- dane[dane$Subject == 6, ]
dane26 <- data.frame(rbind(dane6[dane6$Condition == 3 & dane6$Replication == 1, ]$Moment))
for(i in 2:10) dane26 <- data.frame(rbind(dane26, dane6[dane6$Condition == 3 & dane6$Replication == i, ]$Moment))

dane7 <- dane[dane$Subject == 7, ]
dane27 <- data.frame(rbind(dane7[dane7$Condition == 3 & dane7$Replication == 1, ]$Moment))
for(i in 2:10) dane27 <- data.frame(rbind(dane27, dane7[dane7$Condition == 3 & dane7$Replication == i, ]$Moment))

yy2 <- data.frame(rbind(colMeans(dane21), 
                        colMeans(dane22), 
                        colMeans(dane23), 
                        colMeans(dane24), 
                        colMeans(dane25), 
                        colMeans(dane26), 
                        colMeans(dane27)))

yy <- data.frame(cbind(yy1, yy2))

# wykres
par(mar = c(4, 4, 2, 0.2))
matplot(t(yy), type = 'l', col = 1:7, lty = 1:7, 
        main = 'without orthosis versus with spring 1', 
        xlab = expression(italic(t)), xaxt = "n", ylab = 'Moment', axes = F, lwd = 2)
axis(1, at = c(1, 256, 512), labels = c('0', '1', '2'), las=1, lwd = 0.5)
legend('bottomright', legend = 1:7, col = 1:7, lty = 1:7, ncol = 2, 
       box.lwd = 0.5, lwd = 1.5)
axis(side = 2, lwd = 0.5)
box(lwd = 0.5)

set.seed(1234)
fpp1.test(yy)
# $A
# [1] 0.001
# $B
# [1] 0
# $P
# [1] 0
# $Z
# [1] 0.0008123766

## [0.8, 1]
dane1 <- dane[dane$Subject == 1, ]
dane21 <- data.frame(rbind(dane1[dane1$Condition == 1 & dane1$Replication == 1 & dane1$Time>=0.8, ]$Moment))
for(i in 2:10) dane21 <- data.frame(rbind(dane21, dane1[dane1$Condition == 1 & dane1$Replication == i & dane1$Time>=0.8, ]$Moment))

dane2 <- dane[dane$Subject == 2, ]
dane22 <- data.frame(rbind(dane2[dane2$Condition == 1 & dane2$Replication == 1 & dane2$Time>=0.8, ]$Moment))
for(i in 2:10) dane22 <- data.frame(rbind(dane22, dane2[dane2$Condition == 1 & dane2$Replication == i & dane2$Time>=0.8, ]$Moment))

dane3 <- dane[dane$Subject == 3, ]
dane23 <- data.frame(rbind(dane3[dane3$Condition == 1 & dane3$Replication == 1 & dane3$Time>=0.8, ]$Moment))
for(i in 2:10) dane23 <- data.frame(rbind(dane23, dane3[dane3$Condition == 1 & dane3$Replication == i & dane3$Time>=0.8, ]$Moment))

dane4 <- dane[dane$Subject == 4, ]
dane24 <- data.frame(rbind(dane4[dane4$Condition == 1 & dane4$Replication == 1 & dane4$Time>=0.8, ]$Moment))
for(i in 2:10) dane24 <- data.frame(rbind(dane24, dane4[dane4$Condition == 1 & dane4$Replication == i & dane4$Time>=0.8, ]$Moment))

dane5 <- dane[dane$Subject == 5, ]
dane25 <- data.frame(rbind(dane5[dane5$Condition == 1 & dane5$Replication == 1 & dane5$Time>=0.8, ]$Moment))
for(i in 2:10) dane25 <- data.frame(rbind(dane25, dane5[dane5$Condition == 1 & dane5$Replication == i & dane5$Time>=0.8, ]$Moment))

dane6 <- dane[dane$Subject == 6, ]
dane26 <- data.frame(rbind(dane6[dane6$Condition == 1 & dane6$Replication == 1 & dane6$Time>=0.8, ]$Moment))
for(i in 2:10) dane26 <- data.frame(rbind(dane26, dane6[dane6$Condition == 1 & dane6$Replication == i & dane6$Time>=0.8, ]$Moment))

dane7 <- dane[dane$Subject == 7, ]
dane27 <- data.frame(rbind(dane7[dane7$Condition == 1 & dane7$Replication == 1 & dane7$Time>=0.8, ]$Moment))
for(i in 2:10) dane27 <- data.frame(rbind(dane27, dane7[dane7$Condition == 1 & dane7$Replication == i & dane7$Time>=0.8, ]$Moment))

yy1 <- data.frame(rbind(colMeans(dane21), 
                       colMeans(dane22), 
                       colMeans(dane23), 
                       colMeans(dane24), 
                       colMeans(dane25), 
                       colMeans(dane26), 
                       colMeans(dane27)))

dane1 <- dane[dane$Subject == 1, ]
dane21 <- data.frame(rbind(dane1[dane1$Condition == 3 & dane1$Replication == 1 & dane1$Time>=0.8, ]$Moment))
for(i in 2:10) dane21 <- data.frame(rbind(dane21, dane1[dane1$Condition == 3 & dane1$Replication == i & dane1$Time>=0.8, ]$Moment))

dane2 <- dane[dane$Subject == 2, ]
dane22 <- data.frame(rbind(dane2[dane2$Condition == 3 & dane2$Replication == 1 & dane2$Time>=0.8, ]$Moment))
for(i in 2:10) dane22 <- data.frame(rbind(dane22, dane2[dane2$Condition == 3 & dane2$Replication == i & dane2$Time>=0.8, ]$Moment))

dane3 <- dane[dane$Subject == 3, ]
dane23 <- data.frame(rbind(dane3[dane3$Condition == 3 & dane3$Replication == 1 & dane3$Time>=0.8, ]$Moment))
for(i in 2:10) dane23 <- data.frame(rbind(dane23, dane3[dane3$Condition == 3 & dane3$Replication == i & dane3$Time>=0.8, ]$Moment))

dane4 <- dane[dane$Subject == 4, ]
dane24 <- data.frame(rbind(dane4[dane4$Condition == 3 & dane4$Replication == 1 & dane4$Time>=0.8, ]$Moment))
for(i in 2:10) dane24 <- data.frame(rbind(dane24, dane4[dane4$Condition == 3 & dane4$Replication == i & dane4$Time>=0.8, ]$Moment))

dane5 <- dane[dane$Subject == 5, ]
dane25 <- data.frame(rbind(dane5[dane5$Condition == 3 & dane5$Replication == 1 & dane5$Time>=0.8, ]$Moment))
for(i in 2:10) dane25 <- data.frame(rbind(dane25, dane5[dane5$Condition == 3 & dane5$Replication == i & dane5$Time>=0.8, ]$Moment))

dane6 <- dane[dane$Subject == 6, ]
dane26 <- data.frame(rbind(dane6[dane6$Condition == 3 & dane6$Replication == 1 & dane6$Time>=0.8, ]$Moment))
for(i in 2:10) dane26 <- data.frame(rbind(dane26, dane6[dane6$Condition == 3 & dane6$Replication == i & dane6$Time>=0.8, ]$Moment))

dane7 <- dane[dane$Subject == 7, ]
dane27 <- data.frame(rbind(dane7[dane7$Condition == 3 & dane7$Replication == 1 & dane7$Time>=0.8, ]$Moment))
for(i in 2:10) dane27 <- data.frame(rbind(dane27, dane7[dane7$Condition == 3 & dane7$Replication == i & dane7$Time>=0.8, ]$Moment))

yy2 <- data.frame(rbind(colMeans(dane21), 
                       colMeans(dane22), 
                       colMeans(dane23), 
                       colMeans(dane24), 
                       colMeans(dane25), 
                       colMeans(dane26), 
                       colMeans(dane27)))

yy <- data.frame(cbind(yy1, yy2))

set.seed(1234)
fpp1.test(yy)
# $A
# [1] 0.201
# $B
# [1] 0.204
# $P
# [1] 0.241
# $Z
# [1] 0.2368321
