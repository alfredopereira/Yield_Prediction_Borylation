library("MLeval")
library("ggpubr")
library("svglite")

########################################################################
rocPlot <- function(folder,classes){
  
  predictedDf <- readRDS(paste(folder,"/predictedDf.rds", sep = ""))
  predictedDfGeneral <- readRDS(paste(folder,"/predictedDfGeneral.rds", sep = ""))
  
  colors <- c("darkgoldenrod1","orchid3","gray50","aquamarine3","steelblue2")
  outDir <- paste("ROC_",folder, sep = "")
  dir.create(outDir)
  positiveClass <- "H"
  
  if(classes == 2){
    evalOut <- evalm(predictedDf, cols = colors, rlinethick = 0.8, 
                     dlinecol = "black", dlinethick = 0.3, positive = positiveClass)
  }
  if(classes == 3){
    evalOut <- evalm(predictedDf[,c(1,2,4,5)], cols = colors, rlinethick = 0.8, 
                     dlinecol = "black", dlinethick = 0.3, positive = positiveClass)
  }
  
  roc.plot <-
    ggpar(
      evalOut$roc,
      ggtheme = theme_pubr(),
      font.title = c(0, "plain", "black"),
      font.legend = c(15, "plain", "black"),
      font.x = c(18, "plain", "black"),
      font.y = c(18, "plain", "black"),
      font.tickslab = c(18, "plain", "black"),
      xlab = "1 - Specificity",
      ylab = "Sensitivity"
    ) +
    border(size = 0.5)
  roc.plot <- roc.plot + theme(legend.position = c(0.7, 0.32),
                   legend.title = element_blank(),
                   legend.background = element_rect(fill = "white", color = "black", size = 0.25))
  
  ggsave(file = paste(outDir, "/ROC.svg", sep = ""), 
         plot = roc.plot, width = 5, height = 5)
  
  ggsave(file = paste(outDir, "/ROC.png", sep = ""),  
         plot = roc.plot, width = 5, height = 5, dpi = 600)
  
  return(roc.plot)
}

###############################################################################

CoD4 <- rocPlot("ML_Co_50dataset_2class",2)
CoD3 <- rocPlot("ML_Co_33dataset_2class",2)
CoD2 <- rocPlot("ML_Co_33dataset_3class",3)

NiD4 <- rocPlot("ML_Ni_50dataset_2class",2)
NiD3 <- rocPlot("ML_Ni_33dataset_2class",2)
NiD2 <- rocPlot("ML_Ni_33dataset_3class",3)

plot2 <- ggarrange(CoD2, CoD3, CoD4,
                   NiD2, NiD3, NiD4,
                   labels = c("A","B","C","D","E","F"),
                   font.label = list(size = 20),
                   ncol = 3, nrow = 2)


ggsave(file = "Fig_6.svg", 
       plot = plot2, width = 15, height = 10)

ggsave(file = "Fig_6.png",   
       plot = plot2, width = 15, height = 10, dpi = 600)
