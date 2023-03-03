library(webchem)

################################################################################

df1 <- read.csv("Co_borylation_data.csv", 
                check.names = FALSE, stringsAsFactors = FALSE)

df2 <- c(as.matrix.data.frame(df1[,3:(ncol(df1)-1)]))
df3 <- unique(df2)
Co_CAS <- df3[df3 != ""]

################################################################################

df1 <- read.csv("Ni_borylation_data.csv", 
                check.names = FALSE, stringsAsFactors = FALSE)

df2 <- c(as.matrix.data.frame(df1[,3:(ncol(df1)-1)]))
df3 <- unique(df2)
Ni_CAS <- df3[df3 != ""]

################################################################################

chemNames <- data.frame(CAS_n = unique(c(Co_CAS,Ni_CAS)))

count <- 1

for (i in chemNames$CAS_n) {
  cid <- get_cid(i, from = "xref/registryid", match = "first")
  chemNames[count,2] <- "NO"
  if(is.na(cid$cid)){
    cid <- get_cid(i, from = "xref/rn", match = "first")
    chemNames[count,2] <- "YES"
  }
  if(!is.na(cid$cid)){
    result <- pc_sect(cid$cid, "Canonical SMILES")
    chemNames[count,3] <- result$Name
    chemNames[count,4] <- result$Result
  }
  count <- count + 1
}

colnames(chemNames) <- c("CAS_n","Check?","ChemNames", "SMILES")

write.csv(chemNames, file = "cas_names_smiles.csv", row.names = FALSE, 
          quote = TRUE)
saveRDS(chemNames, "cas_names_smiles.rds")

