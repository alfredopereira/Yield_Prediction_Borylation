################################################################################
################################################################################

library(caret)

################################################################################
################################################################################

data <- read.csv("../4_Co_descriptors/Co_borylation_desc_yields.csv", 
                 check.names = FALSE)

id <- c(1:nrow(data))
data <- cbind(id, data)

dummy <- dummyVars(" ~ .", data = data)
dataML <- data.frame(predict(dummy, newdata = data))
summary(dataML$yield)

badCols <- nearZeroVar(dataML)
dataML <- dataML[, -badCols]
dataML <- dataML[complete.cases(dataML), ]

write.csv(dataML, file = "Co_dataset_reg.csv", row.names = FALSE, 
          quote = FALSE)

################################################################################

zone33 <- quantile(dataML$yield, probs = c(0.33, 0.66))
zone33
zone50 <- quantile(dataML$yield, probs = c(0.25, 0.75))
zone50

yield33 <- cut(dataML$yield , breaks = c(-Inf,zone33[1],zone33[2],Inf), 
               labels=c("L","M","H"))
summary(yield33)

dataML33 <- cbind(dataML[,1:(ncol(dataML)-1)], yield = yield33)
write.csv(dataML33, file = "Co_33dataset_3class.csv", row.names = FALSE, 
          quote = FALSE)
dataML33 <- dataML33[dataML33$yield=="L" | dataML33$yield=="H",]
write.csv(dataML33, file = "Co_33dataset_2class.csv", row.names = FALSE, 
          quote = FALSE)

yield50 <- cut(dataML$yield , breaks = c(-Inf,zone50[1],zone50[2],Inf), 
               labels=c("L","M","H"))
summary(yield50)

dataML50 <- cbind(dataML[,1:(ncol(dataML)-1)], yield = yield50)
# write.csv(dataML50, file = "Co_50dataset_3class.csv", row.names = FALSE, 
#           quote = FALSE)
dataML50 <- dataML50[dataML50$yield=="L" | dataML50$yield=="H",]
write.csv(dataML50, file = "Co_50dataset_2class.csv", row.names = FALSE, 
          quote = FALSE)

################################################################################
################################################################################

data <- read.csv("../4_Ni_descriptors/Ni_borylation_desc_yields.csv", 
                 check.names = FALSE)

id <- c(1:nrow(data))
data <- cbind(id, data)

dummy <- dummyVars(" ~ .", data = data)
dataML <- data.frame(predict(dummy, newdata = data))
summary(dataML$yield)

badCols <- nearZeroVar(dataML)
dataML <- dataML[, -badCols]
dataML <- dataML[complete.cases(dataML), ]

write.csv(dataML, file = "Ni_dataset_reg.csv", row.names = FALSE,
          quote = FALSE)

################################################################################

zone33 <- quantile(dataML$yield, probs = c(0.33, 0.66))
zone33
zone50 <- quantile(dataML$yield, probs = c(0.25, 0.75))
zone50

yield33 <- cut(dataML$yield , breaks = c(-Inf,zone33[1],zone33[2],Inf), 
               labels=c("L","M","H"))
summary(yield33)

dataML33 <- cbind(dataML[,1:(ncol(dataML)-1)], yield = yield33)
write.csv(dataML33, file = "Ni_33dataset_3class.csv", row.names = FALSE,
          quote = FALSE)
dataML33 <- dataML33[dataML33$yield=="L" | dataML33$yield=="H",]
write.csv(dataML33, file = "Ni_33dataset_2class.csv", row.names = FALSE, 
          quote = FALSE)

yield50 <- cut(dataML$yield , breaks = c(-Inf,zone50[1],zone50[2],Inf), 
               labels=c("L","M","H"))
summary(yield50)

dataML50 <- cbind(dataML[,1:(ncol(dataML)-1)], yield = yield50)
# write.csv(dataML50, file = "Ni_50dataset_3class.csv", row.names = FALSE,
#           quote = FALSE)
dataML50 <- dataML50[dataML50$yield=="L" | dataML50$yield=="H",]
write.csv(dataML50, file = "Ni_50dataset_2class.csv", row.names = FALSE, 
          quote = FALSE)

################################################################################
################################################################################
