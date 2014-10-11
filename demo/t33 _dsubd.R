dsubd  <- function(data, sub){
        out  <- list() # a list of dataframe
        for (i in 1:nrow(sub)){
                xmin  <- sub[i,]$xmin
                xmax  <- sub[i,]$xmax
                ymin  <- sub[i,]$ymin
                ymax  <- sub[i,]$ymax
                out[[i]]  <-  data[x >= xmin & x <= xmax & y  >= ymin & y <= ymax,]
        }
        return(out)

}

set.seed(1011)
x  <- rnorm(100,mean  =50, sd = 25)
y  <- rnorm(100,mean  =50, sd = 25)
data  <- as.data.frame(cbind(x,y))
data
### get rectangle map
sub  <- data.frame(rbind(c(20,60,30,70),
                         c(80,90,80,90)))
sub
names(sub)  <- c("xmin","xmax","ymin","ymax")
library(ggplot2)
g1  <- ggplot(data) + geom_point(aes(x =x,y =y)) +
        geom_rect(data = sub, aes(xmin= xmin, xmax =xmax, ymin = ymin, ymax =ymax),
                  col ="red", fill = NA)

# subset
g1 + xlim(80,90) + ylim(80,90)
g1 + scale_x_continuous(limits = c(80, 90)) + scale_y_continuous(limits = c(80, 90))
## zoom out
g1 + coord_cartesian(xlim = c(80,90), ylim = c(80,90))
data[sub[1,]]
# subdata
out  <- dsubd(data,sub)
lapply(out,class)
library(gridExtra)
gl  <- lapply(out, function(df){
        ggplot(df) + geom_point(aes(x =x,y =y))
})
grid.arrange(gl[[1]], gl[[2]], ncol =2)
do.call(grid.arrange, gl)
