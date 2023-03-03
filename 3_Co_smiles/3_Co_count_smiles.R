library(ggpubr)

################################################################################
dir.create("count")

countPlot <- function(dataset, name, limite){
  
  df1 <- read.csv(dataset, check.names = F, header = F)
  
  setwd("count")

  data1 <- as.data.frame(table(df1$V1))
  data1$Var1 <- as.character(data1$Var1)
  data1 <- data1[order(-data1$Freq),]
  
  data1 <- data1[1:3,]
  write.csv(data1,paste("Co", name, ".csv", sep = ""),)
  
  plotOut <- ggdotchart(data1, x = "Var1", y = "Freq",
                        ggtheme = theme_pubr(),
                        color = "orchid2",
                        add.params = list(color = "orchid3", size = 1),
                        sorting = "descending",
                        add = "segments",
                        rotate = TRUE,
                        dot.size = 15,
                        label = data1$Freq,
                        font.label = list(color = "black", 
                                          size = 18, vjust = 0.5),
                        title = name
  ) +
  scale_x_discrete(breaks = NULL, expand = c(0.1,0)) +
  scale_y_continuous(breaks = NULL, limits = c(0,limite)) +
  font("xlab", size = 80, color = "#FDEDFC") +
  font("ylab", size = 0, color = "black") +
  font("xy.text", size = 0, color = "black") +
  font("title", size = 18, color = "black", face = c("bold")) +
  border(size = 0) +
  bgcolor("#FDEDFC") +
  theme(plot.background = element_rect(fill = "#FDEDFC", linetype = "solid",
                                       colour = "white", size = 3))
  
  ggsave(file = paste("Co", name, ".svg", sep = ""), 
         plot = plotOut, width = 2.5, height = 5)
  ggsave(file = paste("Co", name, ".png", sep = ""),
         plot = plotOut, width = 2.5, height = 5, dpi = 600)
  
  saveRDS(plotOut,paste("Co", name, ".rds", sep = ""))
  
  setwd("../")
}

################################################################################

dataset <- "Co_borylation_RCTAsmiles.smi"
name <- "ReactantA"
limite <- 673 + 50
countPlot(dataset,name,limite)

################################################################################

dataset <- "Co_borylation_RCTBsmiles.smi"
name <- "ReactantB"
limite <- 12 + 1
countPlot(dataset,name,limite)

################################################################################

dataset <- "Co_borylation_PROsmiles.smi"
name <- "Product"
limite <- 9 + 1
countPlot(dataset,name,limite)

################################################################################

dataset <- "Co_borylation_CATsmiles.smi"
name <- "Catalyst"
limite <- 264 + 20
countPlot(dataset,name,limite)

################################################################################

dataset <- "Co_borylation_SOLsmiles.smi"
name <- "Solvent"
limite <- 348 + 30
countPlot(dataset,name,limite)

################################################################################

dataset <- "Co_borylation_ADDsmiles.smi"
name <- "Additive"
limite <- 110 + 10
countPlot(dataset,name,limite)

################################################################################
  