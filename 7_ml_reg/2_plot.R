library("ggpubr")
library("svglite")

########################################################################
regPlot <- function(dataset,name,color){
  
  dataPlot <- read.csv(dataset, check.names = FALSE)

  regPlot <- ggscatter(dataPlot, x = "actual", y = "predicted", 
                       add = "reg.line", color = color,
                       shape = 19, size = 2, alpha = 0.5) +
    stat_cor(aes(label = ..rr.label..), color = "black", geom = "text", size = 6)+
    scale_x_continuous(
                       #limits = c(0, 100), 
                       breaks = seq(from = 0, to = 100, by = 10)) +
    scale_y_continuous(
                       #limits = c(0, 100), 
                       breaks = seq(from = 0, to = 100, by = 10))

  plot <-
    ggpar(
      regPlot,
      ggtheme = theme_pubr(),
      font.title = c(0, "plain", "black"),
      font.x = c(18, "plain", "black"),
      font.y = c(18, "plain", "black"),
      font.tickslab = c(18, "plain", "black"),
      xlab = "Experimental yield (%)",
      ylab = "Predicted yield (%)"
    ) +
    border(size = 0.5)

  ggsave(file = paste(name,".svg", sep = ""), 
         plot = plot, width = 5, height = 5)
  
  ggsave(file = paste(name,".png", sep = ""),   
         plot = plot, width = 5, height = 5, dpi = 600)
  
  return(plot)
}

###############################################################################

co1 <- regPlot("ML_Co_dataset_reg/predictions_fold1.csv", "Co_reg_1", "orchid3")
co2 <- regPlot("ML_Co_dataset_reg/predictions_fold2.csv", "Co_reg_2", "orchid3")
co3 <- regPlot("ML_Co_dataset_reg/predictions_fold3.csv", "Co_reg_3", "orchid3")
co4 <- regPlot("ML_Co_dataset_reg/predictions_fold4.csv", "Co_reg_4", "orchid3")
co5 <- regPlot("ML_Co_dataset_reg/predictions_fold5.csv", "Co_reg_5", "orchid3")

ni1 <- regPlot("ML_Ni_dataset_reg/predictions_fold1.csv", "Ni_reg_1", "aquamarine3")
ni2 <- regPlot("ML_Ni_dataset_reg/predictions_fold2.csv", "Ni_reg_2", "aquamarine3")
ni3 <- regPlot("ML_Ni_dataset_reg/predictions_fold3.csv", "Ni_reg_3", "aquamarine3")
ni4 <- regPlot("ML_Ni_dataset_reg/predictions_fold4.csv", "Ni_reg_4", "aquamarine3")
ni5 <- regPlot("ML_Ni_dataset_reg/predictions_fold5.csv", "Ni_reg_5", "aquamarine3")

plot2 <- ggarrange(co5,ni2,
                   labels = c("A","B"),
                   font.label = list(size = 20),
                   ncol = 2, nrow = 1)

ggsave(file = "Fig_5.svg", 
       plot = plot2, width = 10, height = 5)

ggsave(file = "Fig_5.png",   
       plot = plot2, width = 10, height = 5, dpi = 600)
