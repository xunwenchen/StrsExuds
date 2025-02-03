# use permanova to test if there is interactive effect between group and cd
# Calculate distance matrix using the specified method
dist <- phyloseq::distance(pl, method = 'wunifrac')
  
# Extract sample data
sample_data <- data.frame(sample_data(pl))
str(sample_data)
  
# Conduct adonis2 test
adonis.rs <- adonis(dist ~ cd * group, data = sample_data, permutations = 999)
print(adonis.rs)
adonis.rs$aov.tab

adonis.rs2 <- adonis2(dist ~ cd * group, data = sample_data, permutations = 999)
summary(adonis.rs2)
# check if interactive effects
interaction_term <- interaction(sample_data$cd, sample_data$group)
pairwise_result <- pairwise.adonis(dist, interaction_term)
  

