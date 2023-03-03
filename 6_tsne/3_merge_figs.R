library("ggpubr")
library("svglite")

########################################################################

Co1 <- readRDS("Co_dataset_reg_tnse.rds")
Co2 <- readRDS("Co_33dataset_3class_tnse.rds")
Co3 <- readRDS("Co_33dataset_2class_tnse.rds")
Co4 <- readRDS("Co_50dataset_2class_tnse.rds")

Ni1 <- readRDS("Ni_dataset_reg_tnse.rds")
Ni2 <- readRDS("Ni_33dataset_3class_tnse.rds")
Ni3 <- readRDS("Ni_33dataset_2class_tnse.rds")
Ni4 <- readRDS("Ni_50dataset_2class_tnse.rds")

plot2 <- ggarrange(Co1,Co2,Co3,Co4,
                   Ni1,Ni2,Ni3,Ni4,
                   labels = c("A","B","C","D","E","F","G","H"),
                   font.label = list(size = 20),
                   ncol = 4, nrow = 2)


ggsave(file = "Fig_4.svg", 
       plot = plot2, width = 20, height = 10)

ggsave(file = "Fig_4.png",   
       plot = plot2, width = 20, height = 10, dpi = 600)
