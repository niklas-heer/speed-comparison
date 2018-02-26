require(ggplot2)

df <- read.csv("results/data.csv", header=TRUE, sep=",")


p <- ggplot(
        data        = df,
        position    = "dodge",
        aes(
            x   = reorder(name, -median),
            y   = median,
            fill = accuracy)) +
    geom_bar(
        stat        = "identity",
        color       = "black",
        position    = "dodge") +
    geom_text(aes(
                x       = reorder(name, -median),
                y       = median + 0.3 * sign(median),
                label   = format(df$median, digits=2)),
                hjust   = -0.13,
                size    = 3) +
    coord_flip() +
    scale_fill_gradient(
        low     = "tomato1",
        high    = "dodgerblue",
        guide   = guide_legend(
                    title           = "Accuracy (%)",
                    title.position  = "right",
                    title.hjust     = 0.5,
                    title.theme     = element_text(
                                        size = 10,
                                        angle = -90),
                    label.position  = "left",
                    label.hjust     = -5)) +
    labs(
        title       = "Speed comparison of various porgramming languages",
        subtitle    = "Method: calculating π through the Leibniz formula x times",
        caption     = "https://github.com/niklas-heer/speed-comparison",
        x           = "Languages",
        y           = "Median time (ms)") +
    theme(
        plot.title          = element_text(hjust = 0.5),
        plot.subtitle       = element_text(hjust = 0.5),
        plot.caption        = element_text(hjust = 1.18),
        panel.background    = element_blank(),
        panel.grid.minor    = element_blank(),
        axis.ticks          = element_blank(),
        axis.line           = element_line(colour=NA))

ggsave("results/plot.png", width=10, height = 7)
