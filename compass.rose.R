### This function inserts a Compass Rose in Maps developed with 
### ggplot2 R package...
compass.rose <- function(loc, size, bearing = 0, cex = 1, w.subplot,
        	             h.subplot, ...) {
    ### Loading required packages...
    library(ggplot2)
    library(gridExtra)
    ### Empty theme for ggplot2...
    theme_nothing <- function(base_size = 12, base_family = "Helvetica")
    {
        theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(
            rect = element_blank(),
            line = element_blank(),
            text = element_blank(),
            legend.position = "none",
            axis.ticks.margin = unit(0, "lines")
            )
    }
    ### Checking arguments...
    if(missing(loc)) stop("Argument loc is missing")
    if(missing(size)) stop("Argument size is missing")
    ### Setting color scheme...
    cols <- rep(c("white", "black"), 8)
    ### Calculating polygons coordinates...
    rad <- rep(size/c(1, 4, 2, 4), 4)
    x <- rad[(0:15)+1]*cos((0:15)*pi/8+bearing)+loc[1]
    y <- rad[(0:15)+1]*sin((0:15)*pi/8+bearing)+loc[2]
    out <- list(NULL)
    for(i in 1:15) {
        x1 <- c(x[i], x[i+1], loc[1])
        y1 <- c(y[i], y[i+1], loc[2])
        out[i] <- list(data.frame(x = x1, y = y1, col = cols[i]))
    }
    out[16] <- list(data.frame(x = c(x[16], x[1], loc[1]),
                               y = c(y[16], y[1], loc[2]),
                               col = rep(cols[16], 3)))
    df <- as.data.frame(do.call(rbind, out))
    ### Preparing subwindom for Compass Rose drawing...
    vp <- viewport(width = w.subplot, height = h.subplot,
                   x = loc[1],
                   y = loc[2])
    wr <- ggplot(data = df, aes(x = x, y = y, fill = col, colour = col)) +
        geom_polygon() + coord_equal() +
            annotate(geom = "text", x = mean(df$x), y = max(df$y)+.1,
                     label = "N", size = 4) +
            scale_fill_manual(values = c("white", "black")) +
                scale_colour_manual(values = c("black", "black")) +
                theme_nothing()
    print(wr, vp = vp)
}