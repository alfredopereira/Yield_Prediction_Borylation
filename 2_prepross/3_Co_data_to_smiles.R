################################################################################

data <- read.csv("Co_borylation_data.csv", 
                 check.names = FALSE, stringsAsFactors = FALSE)

chemNames <- read.csv("cas_names_smiles_curated.csv", check.names = FALSE)

for (i in 1:nrow(chemNames)) {
  if(is.na(chemNames$ChemNames[i])){
    chemNames$ChemNames[i] <- chemNames$CAS_n[i]
  }
}

for (i in 3:14) {
  data[,i] <- chemNames$SMILES[match(data[,i], chemNames$CAS_n)]
}

write.csv(data, file = "Co_borylation_smiles.csv", row.names = FALSE, 
          quote = TRUE)