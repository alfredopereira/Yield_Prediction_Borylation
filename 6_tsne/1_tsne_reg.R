################################################################################

library(Rtsne)
library(ggpubr)
library(caret)

################################################################################

tsnePlot <- function(dataset, namePlot, colors){
  
  data <- read.csv(dataset, check.names = FALSE, stringsAsFactors = FALSE)
  yield <- data$yield
  
  data <- as.data.frame(scale(data[, 2:(ncol(data)-1)]))
  
  set.seed(7)
  
  tsne <- Rtsne(data, max_iter = 2000, perplexity = 80, check_duplicates = FALSE, 
                pca = FALSE, pca_scale = FALSE, pca_center = FALSE, 
                normalize = TRUE, num_threads = 3, theta = 0)
  
  dataTsne <- as.data.frame(cbind(tsne$Y,yield))
  # dataTsne$V1 <- as.numeric(as.character(dataTsne$V1))
  # dataTsne$V2 <- as.numeric(as.character(dataTsne$V2))
  # dataTsne$yield <- as.numeric(dataTsne$yield)
  
  tsne.plot <- ggscatter(dataTsne, x = "V1", y = "V2", color = "yield", 
                         shape = 19, size = 2, alpha = 0.5)
  
  plot <-
    ggpar(
      tsne.plot,
      ggtheme = theme_pubr(),
      title = "",
      legend = "top",
      legend.title = "Yield (%)",
      xlab = "tSNE1",
      ylab = "tSNE2",
      font.title = c(0, "plain", "black"),
      font.legend = c(15, "plain", "black"),
      font.x = c(18, "plain", "black"),
      font.y = c(18, "plain", "black"),
      font.tickslab = c(18, "plain", "black")
    ) +
    gradient_color(colors) +
    border(size = 0.5)
  
  ggsave(file = paste(namePlot, "tnse.svg", sep = "_"), 
         plot = plot, width = 5, height = 5)
  
  ggsave(file = paste(namePlot, "tnse.png", sep = "_"),  
         plot = plot, width = 5, height = 5, dpi = 600)
  
  saveRDS(plot, paste(namePlot, "tnse.rds", sep = "_"))
}

################################################################################

colors_3 <-  c("#005bbc","gray50","#ffd600")

################################################################################

dataset <- "../5_datasets/Co_dataset_reg.csv"
namePlot <- "Co_dataset_reg"
tsnePlot(dataset,namePlot,colors_3)

################################################################################

dataset <- "../5_datasets/Ni_dataset_reg.csv"
namePlot <- "Ni_dataset_reg"
tsnePlot(dataset,namePlot,colors_3)

################################################################################