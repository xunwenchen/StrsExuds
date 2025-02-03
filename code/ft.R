# import ft data
unique <- read_xlsx('data/ft.xlsx', sheet = 3) # sheet: SPLIST

install.packages("ComplexUpset")
library(ComplexUpset)
us_p <- upset(unique, intersect = c("Me10", "To10", "To100", "Ma10", "Ma100"), 
      name = "Intersection", 
      width_ratio = 0.1, 
      min_size = 1)
  
us_p

if(save){
  ggsave('out/upset_plot.jpg', plot = us_p, 
         w = 88*2.8, h = 66*1.5, units = 'mm', dpi = 300)
  ggsave('out/upset_plot.pdf', plot = us_p, 
         w = 88*2.8, h = 66*1.5, units = 'mm', dpi = 300)
  
}

# import ft data
ft <- read_xlsx('data/ft.xlsx', sheet = 1)
# calculate diversity of certain columns
head(ft)
library(vegan)
# select only these columns: c("Medic-CK", "Medic-cd10", "Tomato-CK", "Tomato-Cd10", "Tomato-cd100", "Maize-CK", "Maize-cd10", "Maize-cd100")
ft <- ft[, 10:17]
# assign 1 to the last number as chem_ID. Chem_ID as a new column
ft$chem_ID <- 1:nrow(ft)
# assign chem_ID as rownames
rownames(ft) <- ft$chem_ID
# remove chem_ID column
ft <- ft[, -ncol(ft)]


# calculate diversity
div <- vegan::diversity(t(ft), "shannon")
# use bootstrapping to calculate the diversity


div <- vegan::diversity(t(ft), "shannon", index = TRUE)
# convert to data frame

div <- as.data.frame(div)
# convert row names to column
div$sample <- rownames(div)
# set sample order: c("Medic-CK", "Medic-cd10", "Tomato-CK", "Tomato-Cd10", "Tomato-cd100", "Maize-CK", "Maize-cd10", "Maize-cd100")
div$sample <- factor(div$sample, levels = c("Medic-CK", "Medic-Cd10", "Tomato-CK", "Tomato-Cd10", "Tomato-Cd100", "Maize-CK", "Maize-Cd10", "Maize-Cd100"))

ggplot(div, aes(x = sample, y = div)) +
  geom_bar(stat = "identity") +
  xlab("Sample") +
  ylab("Chemical diversity (Shannon)") +
  # tilt x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 




