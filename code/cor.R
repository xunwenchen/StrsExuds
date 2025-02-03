# remove Ctrl group of the phylseq object pl_unra
pl_unra.f = subset_samples(pl_unra, group != "Ctrl")

# subset group = Nutrient data and cd = 0
pl_unra.n0  <-  subset_samples(pl_unra, group == "Nutrient" & cd == 0)
pl_unra.n10 <- subset_samples(pl_unra, group == "Nutrient" & cd == 10)
pl_unra.n100 <- subset_samples(pl_unra, group == "Nutrient" & cd == 100)

# take the otu table of pl_unra.n0 and filter out the otus (rows) with sum reads < 10
otu_unra.n0 = otu_table(pl_unra.n0)
dim(otu_unra.n0)
library(phyloseq)

otu_unra.n0 = otu_unra.n0[rowSums(otu_unra.n0) >= 10,]
dim(otu_unra.n0)

# conduct otu pairwise correlation using pearson method, but use c++ in R for faster calculation

library(Rcpp)
sourceCpp("code/cor.cpp")

otu_unra.n0.mt <- as.matrix(otu_unra.n0)

# Calculate pairwise correlations
cor_matrix.n0 <- pairwise_correlation(otu_unra.n0.mt)




