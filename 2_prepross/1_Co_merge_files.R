################################################################################

filenames <- list.files("../1_Co_reactions", pattern = "cas.csv",
                        full.names = TRUE)

listCas <- lapply(filenames,function(i){
  read.csv(i, check.names = FALSE, stringsAsFactors = FALSE)[,-1]
  
})

cas <- do.call(cbind.data.frame, listCas)
cas <- cas[, colSums(is.na(cas)) != nrow(cas)]

################################################################################

smiles <- read.csv("../1_Co_reactions/Co_borylation.smiles",
                   check.names = FALSE, header = FALSE, stringsAsFactors = FALSE)

info <- read.csv("../1_Co_reactions/Co_borylation_Info.csv", 
                   check.names = FALSE, stringsAsFactors = FALSE)

smilesRXN <- c()
smilesID <- 0

for ( i in info$scheme ){
  smilesID <- smilesID + 1
  smilesRXN[smilesID] <- smiles[i,1]
}

################################################################################

yield <- read.csv("../1_Co_reactions/Co_borylation_Yield.csv", 
                  check.names = FALSE, stringsAsFactors = FALSE)[,-1]

yield <- yield[, colSums(is.na(yield)) != nrow(yield)]

################################################################################

data1 <- cbind(info[,c(-2,-3,-6)], smilesRXN = smilesRXN, cas, yield)
data2 <- data1[data1$nRCT==2 & data1$nPRO==1,]

data3 <- data2[, colSums(is.na(data2)) != nrow(data2)]

write.csv(data3[,c(-2,-3)],file = "Co_borylation_data.csv", 
          row.names = FALSE, quote = FALSE)
