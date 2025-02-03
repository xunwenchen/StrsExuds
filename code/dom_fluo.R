# PCA for DOC chemical data (fluorescent, indices, pH ...)
# load data
doc_flu <- read_xlsx('data/doc_flu.xlsx')

# set factor
doc_flu$sp <- factor(doc_flu$sp, levels = c('Medick', 'Tomato', 'Maize'))
doc_flu$cd <- factor(doc_flu$cd)
head(doc_flu)

# conduct PCA analysis
# install.packages("factoextra")
library(factoextra)

pca_res <- prcomp(doc_flu[, -c(1, 2)], scale = TRUE)
summary(pca_res)
pca_df <- data.frame(PC1 = pca_res$x[,1], PC2 = pca_res$x[,2], cd = doc_flu$cd, sp = doc_flu$sp)
explained_variance <- summary(pca_res)$importance[2, 1:2] * 100

# pca_score_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = cd, shape = sp)) +
#   geom_point(size = 3) +
#   scale_color_gradient(low = "lightblue", high = "darkred") +
#   theme_bw() +
#   labs(title = "PCA Score Plot", 
#        x = paste0("PC1 (", round(explained_variance[1], 2), "%)"), 
#        y = paste0("PC2 (", round(explained_variance[2], 2), "%)")) +
#   theme(legend.position = 'right') # + stat_ellipse(aes(group = sp), type = "norm", level = 0.95, linetype = 2)
# 
# pca_score_plot

# Define color palettes for each species
palette_maize <- c("#f2ecce", "#dfd084", "#BFA100")
palette_medick <- c("#e5f5f9", "#99d8c9", "#2ca25f")
palette_tomato <- c("#fee0d2", "#fc9272", "#de2d26")

# Create a mapping for colors based on species and cd levels
pca_df$color <- with(pca_df, ifelse(sp == "Maize", palette_maize[as.numeric(cd)],
                                    ifelse(sp == "Medick", palette_medick[as.numeric(cd)],
                                           palette_tomato[as.numeric(cd)])))

# Plot PCA results
pca_score_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = color, shape = sp)) +
  geom_point(size = 4, stroke = 1) +
  scale_color_identity() +
  theme_bw() +
  labs(title = "PCA Score Plot", 
       x = paste0("PC1 (", round(explained_variance[1], 2), "%)"), 
       y = paste0("PC2 (", round(explained_variance[2], 2), "%)")) +
  theme(legend.position = 'right') +
# + stat_ellipse(aes(group = sp), type = "norm", level = 0.95, linetype = 2) +
# sp legend using hollow symbols
  scale_shape_manual(values = c(21, 22, 23))+
  # set legend to the right low corner
  theme(legend.position = c(0.83, 0.225))+
  theme(legend.spacing.y = unit(0.5, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        legend.text = element_text(margin = margin(t = 0.1, b = 0.1)))+
  # remove grid
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  # remove legend title
  theme(legend.title = element_blank())

pca_score_plot

if(save){
  ggsave('out/pca_score_plot.jpg', plot = pca_score_plot, 
         w = 88, h = 88, units = 'mm', dpi = 300)
  ggsave('out/pca_score_plot.pdf', plot = pca_score_plot, 
         w = 88, h = 88, units = 'mm', dpi = 300)
}
