################################################################################

library(stringr)

################################################################################

data <- read.csv("../2_prepross/Ni_borylation_smiles.csv", 
                 check.names = FALSE, stringsAsFactors = FALSE)

################################################################################

reactantSmiles <- data.frame(smiles = str_split_fixed(data$smilesRXN, ">>", n = 2)[,1])
reactantSmilesA <- data.frame()
reactantSmilesB <- data.frame()

for ( i in 1:nrow(reactantSmiles) ) {
 
  reactantSmilesTempA <- str_split_fixed(reactantSmiles[i,1],"[.]", n = 2)[,1]
  reactantSmilesTempB <- str_split_fixed(reactantSmiles[i,1],"[.]", n = 2)[,2]
  
  if (str_detect(reactantSmilesTempA, "B") == T & 
      str_detect(reactantSmilesTempA, "B[[:lower:]]") != T) {
    
    reactantSmilesA[i,1] <- reactantSmilesTempA
    reactantSmilesB[i,1] <- reactantSmilesTempB
  
    } else {
    
    reactantSmilesA[i,1] <- reactantSmilesTempB
    reactantSmilesB[i,1] <- reactantSmilesTempA
    
  }
}

for (i in 1:nrow(reactantSmilesA)) {
  if (str_detect(reactantSmilesA[i,1], "[.]") == TRUE |
      str_detect(reactantSmilesA[i,1], "BH|Br-|Cl-|F-|I-|C-|H-|H2-|H3-|B-|Si-") == TRUE) {
    reactantSmilesA[i,1] <- NA
  }
}

for (i in 1:nrow(reactantSmilesB)) {
  if (str_detect(reactantSmilesB[i,1], "[.]") == TRUE |
      str_detect(reactantSmilesB[i,1], "Br-|Cl-|F-|I-|C-|H-|H2-|H3-|B-|Si-") == TRUE) {
    reactantSmilesB[i,1] <- NA
  }
}

################################################################################

productSmiles <- data.frame(smiles = str_split_fixed(data$smilesRXN, ">>", n = 2)[,2])

for (i in 1:nrow(productSmiles)) {
  if (str_detect(productSmiles[i,1], "[.]") == TRUE |
      str_detect(productSmiles[i,1], "Br-|Cl-|F-|I-|C-|H-|H2-|H3-|B-|Si-") == TRUE) {
    productSmiles[i,1] <- NA
  }
}

################################################################################

NiCatalysts <- c()

for (i in 1:nrow(data)) {
  if (str_detect(data[i,3], "Ni") == TRUE) {
    NiCatalysts[i] <- data[i,3]
  } else {
    if (str_detect(data[i,4], "Ni") == TRUE) {
      NiCatalysts[i] <- data[i,4]
    } else {
      if (str_detect(data[i,5], "Ni") == TRUE) {
        NiCatalysts[i] <- data[i,5]
      } else {
        if (str_detect(data[i,6], "Ni") == TRUE) {
          NiCatalysts[i] <- data[i,6]
        } else {
          if (str_detect(data[i,7], "Ni") == TRUE) {
            NiCatalysts[i] <- data[i,7]
          }
        }
      }
    }
  }
}

NiCatalysts <- as.data.frame(NiCatalysts)

for (i in 1:nrow(NiCatalysts)) {
  if (str_detect(NiCatalysts[i,1], "C-|H-|H2-|H3-|B-|Si-") == TRUE) {
    NiCatalysts[i,1] <- NA
  }
}

################################################################################

additives <- c()

for (i in 1:nrow(data)) {
  tempSmiles <- data[i,3:14]
  additives[i] <- paste(tempSmiles[!is.na(tempSmiles) & str_detect(tempSmiles, "Ni") == FALSE], collapse = '.')
  if (additives[i]==""){
    additives[i] <- "None"
  }
}

additives <- as.data.frame(additives)

################################################################################

solvents <- c()

for (i in 1:nrow(data)) {
  tempSmiles <- data[i,15:19]
  solvents[i] <- paste(tempSmiles[!is.na(tempSmiles)], collapse = '.')
  if (solvents[i]==""){
    solvents[i] <- "None"
  }
}

solvents <- as.data.frame(solvents)

################################################################################

data2 <- cbind(data$id,reactantSmilesA,reactantSmilesB,NiCatalysts,additives,solvents,productSmiles, data$y1)
colnames(data2) <- c("id","RCTA","RCTB","CAT","ADD","SOL","PRO","yield")

data3 <- data2[complete.cases(data2), ]

write.table(data3$RCTA, file = "Ni_borylation_RCTAsmiles.smi", 
            row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(data3$RCTB, file = "Ni_borylation_RCTBsmiles.smi", 
            row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(data3$CAT, file = "Ni_borylation_CATsmiles.smi",
            row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(data3$ADD, file = "Ni_borylation_ADDsmiles.smi",
            row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(data3$SOL, file = "Ni_borylation_SOLsmiles.smi",
            row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(data3$PRO, file = "Ni_borylation_PROsmiles.smi", 
            row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(data3$yield, file = "Ni_borylation_yield.csv", 
            row.names = FALSE, quote = FALSE, col.names = FALSE)
