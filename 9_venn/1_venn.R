library("ggpubr")
library("svglite")
library("ggVennDiagram")

########################################################################

dataCo <- list(
  Fold1 = read.csv("../8_ml_class/ML_Co_50dataset_2class/varImp_fold1.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),3],
  Fold2 = read.csv("../8_ml_class/ML_Co_50dataset_2class/varImp_fold2.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),3],
  Fold3 = read.csv("../8_ml_class/ML_Co_50dataset_2class/varImp_fold3.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),3],
  Fold4 = read.csv("../8_ml_class/ML_Co_50dataset_2class/varImp_fold4.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),3],
  Fold5 = read.csv("../8_ml_class/ML_Co_50dataset_2class/varImp_fold5.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),3]
)

plot1 <- 
  ggVennDiagram(dataCo[1:5], label_alpha = 0, label = "count",
                edge_size = 0, label_size = 6, set_size = 6) +
  scale_fill_gradientn(colours = c("steelblue2","#ffd600")) +
  scale_color_manual(values = c("black","black","black","black", "black")) +
  theme(legend.position = "none")

ggsave(file = "Co_venn.svg", 
       plot = plot1, width = 6, height = 6)

saveRDS(plot1, "venn1.rds")

topCo <- Reduce(intersect, dataCo)

########################################################################

dataNi <- list(
  Fold1 = read.csv("../8_ml_class/ML_Ni_50dataset_2class/varImp_fold1.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),3],
  Fold2 = read.csv("../8_ml_class/ML_Ni_50dataset_2class/varImp_fold2.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),3],
  Fold3 = read.csv("../8_ml_class/ML_Ni_50dataset_2class/varImp_fold3.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),3],
  Fold4 = read.csv("../8_ml_class/ML_Ni_50dataset_2class/varImp_fold4.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),3],
  Fold5 = read.csv("../8_ml_class/ML_Ni_50dataset_2class/varImp_fold5.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),3]
)

plot2 <- 
  ggVennDiagram(dataNi[1:5], label_alpha = 0, label = "count",
                edge_size = 0, label_size = 6, set_size = 6) +
  scale_fill_gradientn(colours = c("steelblue2","#ffd600")) +
  scale_color_manual(values = c("black","black","black","black", "black")) +
  theme(legend.position = "none")

ggsave(file = "Ni_venn.svg", 
       plot = plot2, width = 6, height = 6)

saveRDS(plot2, "venn2.rds")

topNi <- Reduce(intersect, dataNi)

########################################################################
