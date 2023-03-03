library(ggpubr)

yield <- read.csv("Co_borylation_yield.csv", check.names = FALSE, header = FALSE)

plotOut <- gghistogram(yield, x = "V1",
                     ggtheme = theme_pubr(),
                     color = "orchid3",
                     fill = "orchid3",
                     alpha = 0.5,
                     #bins = 50,
                     binwidth = 5,
                     add = "median",
                     add.params = list(color = "black", linetype = "dashed"),
                     #legend = "none",
                     ylab = "Reactions count",
                     xlab = "Yield (%)"
) +
scale_x_continuous(breaks = seq(from = 0, to = 100, by = 20)) +
scale_y_continuous(breaks = seq(from = 0, to = 120, by = 20)) +
font("xlab", size = 18, color = "black") +
font("ylab", size = 18, color = "black") +
font("xy.text", size = 18, color = "black") +
border(size = 0.5)

ggsave(file = "Co_yieldHistogram.svg", 
       plot = plotOut, width = 5, height = 5)
ggsave(file = "Co_yieldHistogram.png", 
       plot = plotOut, width = 5, height = 5, dpi = 600)

saveRDS(plotOut,"Co_yieldHistogram.rds")