# Generate data
pp <- function (n,r=4) {
        x <- seq(-r*pi, r*pi, len=n)
        df <- expand.grid(x=x, y=x)
        df$r <- sqrt(df$x^2 + df$y^2)
        df$z <- cos(df$r^2)*exp(-df$r/6)
        df
}
p <- ggplot(pp(20), aes(x=x,y=y))

p + geom_tile() #pretty useless!

# Add aesthetic mappings
p + geom_tile(aes(fill=z))

# Change scale
p + geom_tile(aes(fill=z)) + scale_fill_gradient(low="green", high="red")

# Use qplot instead
qplot(x, y, data=pp(20), geom="tile", fill=z)
qplot(x, y, data=pp(100), geom="tile", fill=z)

# Missing values
p <- ggplot(pp(20)[sample(20*20, size=200),], aes(x=x,y=y,fill=z))
p + geom_tile()

# Input that works with image
image(t(volcano)[ncol(volcano):1,])
library(reshape2) # for melt
ggplot(melt(volcano), aes(x=Var1, y=Var2, fill=value)) + geom_tile()

# inspired by the image-density plots of Ken Knoblauch
cars <- ggplot(mtcars, aes(y=factor(cyl), x=mpg))
cars + geom_point()
cars + stat_bin(aes(fill=..count..), geom="tile", binwidth=3, position="identity")
cars + stat_bin(aes(fill=..density..), geom="tile", binwidth=3, position="identity")

cars + stat_density(aes(fill=..density..), geom="tile", position="identity")
cars + stat_density(aes(fill=..count..), geom="tile", position="identity")

# Another example with with unequal tile sizes
x.cell.boundary <- c(0, 4, 6, 8, 10, 14)
example <- data.frame(
        x = rep(c(2, 5, 7, 9, 12), 2),
        y = factor(rep(c(1,2), each=5)),
        z = rep(1:5, each=2),
        w = rep(diff(x.cell.boundary), 2)
)

qplot(x, y, fill=z, data=example, geom="tile")
qplot(x, y, fill=z, data=example, geom="tile", width=w)
qplot(x, y, fill=factor(z), data=example, geom="tile", width=w)

# You can manually set the colour of the tiles using
# scale_manual
col <- c("darkblue", "blue", "green", "orange", "red")
qplot(x, y, fill=col[z], data=example, geom="tile", width=w, group=1) +
        scale_fill_identity(labels=letters[1:5], breaks=col)

[Package ggplot2 version 1.0.0 Index]

### Plot

p0 <- ggplot(data=df,aes(x=Distance,y=Elevation)) +
        xlab("Distance(Km)") +
        ylab("Elevation(m)") +
        theme_classic() +
        scale_x_continuous(expand = c(0,0), breaks = seq(0,3000000,500000),labels = function(x) x/1000) +
        scale_y_continuous(expand = c(0,0), limit = c(0,6000),breaks = seq(0,6000,1000)) +
        coord_fixed(ratio = 100)


p12  <- p0 + geom_area(fill="cyan") +
        geom_line(aes(group=1),size = 0,colour="sienna2") #sienna2

p12
p10  <- p0 + geom_point(size =1, colour="red")
p10
p11  <- p0 + geom_line(aes(group=1),colour="red")
p11

p14  <- p0 + geom_step()
p14
p20  <- p11 + stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max")
p20
source("~/Dropbox/workbox/R/")

p  <- p12
ggsave(p, file="curen-wuhan6.png",dpi = 600, unit = "cm")
png("test.png")
plot(p)
dev.off()
### Control the Graphy
p <- p + geom_line(aes(fill = "Elevation"))
p
p <- p + geom_smooth()
p


library(ggplot2)
ggplot(df, aes(x = Distance)) +
        geom_ribbon(aes(ymin = 0, # change this to match your min below
                        ymax = 6000),
                    fill = "#1B9E77") + # put your altitude variable here if not using moving averages
        labs(x = "Miles",
             y = "Elevation")
+
        scale_y_continuous(limits = c(600,1200)) # change this to limits appropriate for your region





# Geometry Layers
geom_line() +
        # Smooth Zone and Curve
        geom_smooth()
# Title
# labs(title = "willing")+
# X,Y Labels
xlab(expression(paste("Temperature (",degree,"C)"))) +
        coord_cartesian(xlim=c(10.5, 23)) +
        scale_x_continuous(breaks=seq(10.5, 23, by = 2.5)) +
        ylab(expression(paste("NPP (g C ",m^-2," ",yr^-1,")"))) +
        coord_cartesian(ylim=c(600, 850)) +
        scale_y_continuous(breaks=seq(600, 850, by = 50))

# Theme with white background, grey grid, and Font
p <- p + theme_bw(base_size = 12, base_family = "Times New Roman")

# Eliminates baground, gridlines, and chart border
p <- p + theme(
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.background = element_blank())
## Axes:
# Axes:draws x and y axis line
p <- p + theme(axis.line = element_line(color = 'black'))

# Axes:Scales of X and Y


