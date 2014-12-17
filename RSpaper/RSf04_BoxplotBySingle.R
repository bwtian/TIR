# f03
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
hkdBH  <- readRDS("~/Dropbox/2data/dataProduct/hkd/hkd_profiles_140806_164333.Rds")
hkd100  <- subset(hkdBH, (Depths >=100 ))
hkdxyz  <-  unique(hkdBH[,c(1:3,13)])
hkdBox1   <-
  ggplot(hkdxyz, aes(factor(0),TD)) +
  geom_boxplot(fill = "green",outlier.colour = "red") +
  #geom_boxplot(fill = "green", outlier.size = 0 ) +
  geom_jitter(shape = ".", size  = 1) +
  stat_summary(fun.y="mean",geom="point",color="blue", shape = 4) +
  ylab("Total length (depth of the bottom hole from the orifice, m)") +
  scale_y_continuous(breaks = c(seq(200, 2200,by = 200))) +
  xlab("") +

  theme_bw(base_size = 12, base_family = "Times") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_text(vjust = - 0.5),
        aspect.ratio = 1/8) +
  coord_flip()
hkdBox1
ggsave(plot =hkdBox1, "hkdBox1.pdf", width = 7, height = 1.5)
# ge.ggsave(hkdBox1)
getwd()
