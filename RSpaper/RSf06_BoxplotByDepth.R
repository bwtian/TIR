### p1402fig5 f05box2
### boxplot by depth
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
hkdSample  <- readRDS("~/Dropbox/2data/dataProduct/hkd/hkd_profiles_140806_164333.Rds")
names(hkdSample)
breaks1  <- 0:15 *100 + 50
# breaks2  <- c(1850, 2200)
# (breaks  <- c(0,breaks1, breaks2))
breaks  <- c(0,breaks1)
#labels  <- as.character(c(50, seq(100, 1500, 100),1700,2000))
labels  <- as.character(c(50, seq(100, 1500, 100)))
hkdSample$cutDepth  <- cut(hkdSample$Depths, breaks = breaks, labels = labels, right = TRUE)
hkdSample$cut100  <- cut(hkdSample$Depths, breaks = 0:22 * 100, labels = as.character(1:22 *100), right = TRUE)
table(hkdSample$cutDepth)
table(hkdSample$cut100)
hkdS  <- hkdSample[complete.cases(hkdSample), ]  #28210 - 28476 = 266


# summary(hkdS)
# stripchart(hkdSample$Temperature)
# boxplot(hkdSample$Temperature,horizontal=TRUE, xlab = , col = "green",outlier.colour = "red")
# hist(Hokkaido_xy$Depth,horizontal=TRUE, xlab = "Depth (m)", col = "green")
# stripchart(Hokkaido_xy$Depth, add=TRUE, pch=19, method = "jitter")
# stripchart(Hokkaido_xy$Tem, add=TRUE, pch=19, method = "jitter")

hkdBox2  <- ggplot(hkdS, aes(factor(cutDepth), Temperature)) +
  geom_boxplot(fill = "green",outlier.colour = "red") +
  stat_summary(fun.y="mean",geom="point",color="blue", shape = 18, size=4) +
  stat_summary(aes(y = Temperature,group = 1), fun.y="mean",geom="line",color="blue") +
  scale_y_continuous(breaks = c(seq(0, 400,by = 50))) +
  ylab(expression(Temperature~(degree*C))) +
  xlab("Depth (m)") +
  theme_bw(base_size = 12, base_family = "Times") +
  theme(axis.title.x = element_text(vjust = -0.5))

ggsave(plot =hkdBox2, "hkdBox2.pdf", width = 7, height = 4)
#ge.ggsave(f05box2)

#
# bhp_df$ColTemp  <- round(bhi_df$Temperature)
# bhp_df$ColTemp[bhp_df$ColTemp <= 0]  <- 1   # rgl color need all value are postive
# summary(bhp_df)
# bhp3d <- bhp_df[,c(3,4,9)]
# bhp4d <- bhp_df[,c(3,4,9,11)]
# ```
