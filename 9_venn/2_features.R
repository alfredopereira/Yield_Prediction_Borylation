library("ggpubr")
library("svglite")
library("ggVennDiagram")
library("dplyr")

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

topNi <- Reduce(intersect, dataNi)

########################################################################

dataCo <- list(
  Fold1 = read.csv("../8_ml_class/ML_Co_50dataset_2class/varImp_fold1.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),c(2,3)],
  Fold2 = read.csv("../8_ml_class/ML_Co_50dataset_2class/varImp_fold2.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),c(2,3)],
  Fold3 = read.csv("../8_ml_class/ML_Co_50dataset_2class/varImp_fold3.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),c(2,3)],
  Fold4 = read.csv("../8_ml_class/ML_Co_50dataset_2class/varImp_fold4.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),c(2,3)],
  Fold5 = read.csv("../8_ml_class/ML_Co_50dataset_2class/varImp_fold5.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),c(2,3)]
)

dataCo2 <- lapply(dataCo, function(x) x[x$Features %in% topCo,])
dataCo3 <- lapply(dataCo2, function(x) x[order(x$Features),])

dataCo4 <- bind_rows(dataCo3) %>%
           group_by(Features) %>%
           summarise(mean = mean(MeanDecreaseGini, na.rm = TRUE),
                     sd = sd(MeanDecreaseGini, na.rm = TRUE),
                     .groups = 'drop')

dataCo5 <- cbind(dataCo4[order(dataCo4$Features),], 
                 A = c(rep("CAT", 30),
                       rep("PRO", 10),
                       rep("RCTB", 5)))


varImpPlot <- ggplot(dataCo5, aes(x = reorder(Features, +mean), y = mean, fill = A)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                width = 0.2, position = position_dodge(0.9)) + 
  coord_flip() +
  scale_fill_manual(values = c("orchid3","firebrick2","gray50")) 

plot1 <-
  ggpar(
    varImpPlot,
    legend = "right",
    legend.title = "",
    ggtheme = theme_pubr(),
    xlab = "Features", ylab = "MeanDecreaseGini",
    font.x = c(18, "plain", "black"),
    font.y = c(18, "plain", "black"),
    font.tickslab = c(15, "plain", "black"),
    font.legend = c(18, "plain", "black"),
  ) +
  border(size = 0.5)

ggsave(file = "varImpPlot_Co.svg", 
       plot = plot1, width = 10, height = 12)

########################################################################

dataNi <- list(
  Fold1 = read.csv("../8_ml_class/ML_Ni_50dataset_2class/varImp_fold1.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),c(2,3)],
  Fold2 = read.csv("../8_ml_class/ML_Ni_50dataset_2class/varImp_fold2.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),c(2,3)],
  Fold3 = read.csv("../8_ml_class/ML_Ni_50dataset_2class/varImp_fold3.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),c(2,3)],
  Fold4 = read.csv("../8_ml_class/ML_Ni_50dataset_2class/varImp_fold4.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),c(2,3)],
  Fold5 = read.csv("../8_ml_class/ML_Ni_50dataset_2class/varImp_fold5.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)[c(1:100),c(2,3)]
)

dataNi2 <- lapply(dataNi, function(x) x[x$Features %in% topNi,])
dataNi3 <- lapply(dataNi2, function(x) x[order(x$Features),])

dataNi4 <- bind_rows(dataNi3) %>%
  group_by(Features) %>%
  summarise(mean = mean(MeanDecreaseGini, na.rm = TRUE),
            sd = sd(MeanDecreaseGini, na.rm = TRUE),
            .groups = 'drop')

dataNi5 <- cbind(dataNi4[order(dataNi4$Features),], 
                 A = c(rep("PRO", 19),
                       rep("RCTA", 5),
                       rep("RCTB", 23)))


varImpPlot <- ggplot(dataNi5, aes(x = reorder(Features, +mean), y = mean, fill = A)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                width = 0.2, position = position_dodge(0.9)) + 
  coord_flip() +
  scale_fill_manual(values = c("firebrick2","peru","gray50"))

plot2 <-
  ggpar(
    varImpPlot,
    legend = "right",
    legend.title = "",
    ggtheme = theme_pubr(),
    xlab = "Features", ylab = "MeanDecreaseGini",
    font.x = c(18, "plain", "black"),
    font.y = c(18, "plain", "black"),
    font.tickslab = c(15, "plain", "black"),
    font.legend = c(18, "plain", "black"),
  ) +
  border(size = 0.5)

ggsave(file = "varImpPlot_Ni.svg", 
       plot = plot2, width = 10, height = 12)

################

plotA <- readRDS("venn1.rds")
plotB <- readRDS("venn2.rds")

plot3 <- ggarrange(plotA, plotB, plot1, plot2,
                   labels = c("A", "B", "C", "D"),
                   font.label = list(size = 20),
                   ncol = 2, nrow = 2,
                   heights = c(1,2))

ggsave(file = "Fig_8.svg", 
       plot = plot3, width = 20, height = 18)

ggsave(file = "Fig_8.png", 
       plot = plot3, width = 20, height = 18, dpi = 600)

