# rarefy the phyloseq object pl_unra based on the min read

pl <- rarefy_even_depth(pl_unra, sample.size = min(sample_sums(pl_unra)), rngseed = 123)

# # remove the group Ctrl and Nutrient for the all groups
# pl_unra_rm <- subset_samples(pl_unra, group != "Ctrl" & group != "Nutrient")
# # rarefy the phyloseq object pl_unra_rm based on the min read
# pl_rm <- rarefy_even_depth(pl_unra_rm, sample.size = min(sample_sums(pl_unra_rm)), rngseed = 123)

sample_sums(pl)
sample_data(pl)

# sample_sums(pl_rm)
# sample_data(pl_rm)

# remove Ctrl (should be soil without exudates and nutrient solution)
pl_f <- subset_samples(pl, group != 'Ctrl')
sample_data(pl_f) # 'Ctrl' removed

# subset group Ctrl in pl
pl_cd0 <- subset_samples(pl_f, cd == 0)

# plot PCoA based on weighted unifrac distance of pl_cd0, group by group
pcoa_cd0_uf <- ordinate(pl_cd0, method = "PCoA", distance = "unifrac", weighted = TRUE)
pcoa_plot_cd0 <- plot_ordination(pl_cd0, pcoa_cd0_uf, color = "group", shape = "group") + 
  geom_point(size = 3) + 
  stat_ellipse(aes(group = group), type = "norm", level = 0.95)+ 
  ggtitle("Cd = 0")+
  theme_bw()+
  scale_color_manual(values = c("lightblue", "yellowgreen", "tomato", "#BFA100"))+
  scale_shape_manual(values = c(15, 16, 17, 18))+
  # remove legend title and move legend in the figure
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.2))+
  # reduce line spacing in the legend
  theme(legend.spacing.y = unit(0.2, 'cm'),
        legend.key.height = unit(0.3, 'cm'),
        legend.text = element_text(margin = margin(t = 0, b = 0)))+
  # remove grid
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  

pcoa_plot_cd0
# plot PCoA based on bray-curtis distance of pl_cd0, group by group
pcoa_bc <- ordinate(pl_cd0, method = "PCoA", distance = "bray", weighted = TRUE)
pcoa_plot_bc <- plot_ordination(pl_cd0, pcoa_bc, color = "group", shape = "group") + 
  geom_point(size = 3) + 
  theme_minimal() + 
  ggtitle("Cd = 0")+
  theme_bw()+ 
  stat_ellipse(aes(group = group), type = "norm", level = 0.95)+
  scale_color_manual(values = c("lightblue", "yellowgreen", "tomato", "#BFA100"))+
  scale_shape_manual(values = c(15, 16, 17, 18))+
  # remove legend
  theme(legend.position = "none")+
  # remove grid
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pcoa_plot_bc



# do the same for pl_cd100
pl_cd100 <- subset_samples(pl_f, cd == 100)
pcoa_cd100 <- ordinate(pl_cd100, method = "PCoA", distance = "unifrac", weighted = TRUE)
pcoa_plot_cd100 <- plot_ordination(pl_cd100, pcoa_cd100, color = "group", shape = "group") + 
  geom_point(size = 3) + 
  stat_ellipse(aes(group = group), type = "norm", level = 0.95)+
  ggtitle("Cd = 100")+
  theme_bw()+
  scale_color_manual(values = c("lightblue", "tomato", "#BFA100"))+
  scale_shape_manual(values = c(15, 17, 18))+
  # remove legend
  theme(legend.position = "none")+
  # remove grid
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pcoa_plot_cd100

# do the same for pl_Cd10
pl_cd10 <- subset_samples(pl_f, cd == 10)
pcoa_cd10 <- ordinate(pl_cd10, method = "PCoA", distance = "unifrac", weighted = TRUE)
pcoa_plot_cd10 <- plot_ordination(pl_cd10, pcoa_cd10, color = "group", shape = "group") + 
  geom_point(size = 3) + 
  stat_ellipse(aes(group = group), type = "norm", level = 0.95)+
  ggtitle("Cd = 10")+
  theme_bw()+
  scale_color_manual(values = c("lightblue", "yellowgreen", "tomato", "#BFA100"))+
  scale_shape_manual(values = c(15, 16, 17, 18))+
  # remove legend
  theme(legend.position = "none")+
  # remove grid
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pcoa_plot_cd10


pcoa_p <- ggarrange(pcoa_plot_cd0, pcoa_plot_cd10, pcoa_plot_cd100, ncol = 2, nrow = 2, labels = c('a', 'b', 'c'))

pcoa_p

# save
ggsave('out/pcoa_plot.jpg', plot = pcoa_p, 
       w = 88*2, h = 66*2, units = 'mm', dpi = 300)
ggsave('out/pcoa_plot.pdf', plot = pcoa_p, 
       w = 88*2, h = 66*2, units = 'mm', dpi = 300)



adonis2_pairwise(pl_cd0, 'group')
adonis2_pairwise(pl_cd10, 'group')
adonis2_pairwise(pl_cd100, 'group', method = 'bray')

# remove the group Ctrl and Nutrient for the Cd = 0 group
pl_cd0_rm <- subset_samples(pl_cd0, group != "Ctrl" & group != "Nutrient")

adonis2_pairwise(pl_cd0_rm, 'group', 'bray')


# remove the group Ctrl and Nutrient for the Cd = 10 group
pl_cd10_rm <- subset_samples(pl_cd10, group != "Ctrl" & group != "Nutrient")

adonis2_pairwise(pl_cd10_rm, 'group', method = 'bray')

# remove the group Ctrl and Nutrient for the Cd = 100 group
pl_cd100_rm <- subset_samples(pl_cd100, group != "Ctrl" & group != "Nutrient")

adonis2_pairwise(pl_cd100_rm, 'group')



# subset group Ctrl in pl_rm
pl_rm_cd0 <- subset_samples(pl_rm, cd == 0)
sample_data(pl_rm_cd0)

adonis2_pairwise(pl_rm_cd0, 'group', method = 'euclidean')

# subset group Ctrl in pl_rm
pl_rm_cd10 <- subset_samples(pl_rm, cd == 10)
sample_data(pl_rm_cd10)
adonis2_pairwise(pl_rm_cd10, 'group', method = 'euclidean')

# subset group Ctrl in pl_rm
pl_rm_cd100 <- subset_samples(pl_rm, cd == 100)
sample_data(pl_rm_cd100)
adonis2_pairwise(pl_rm_cd100, 'group', method = 'euclidean')


plot_pcoa(pl_rm_cd0, distance_method = "unifrac", weighted = TRUE, color_var = "group", shape_var = "group", title = "PCoA plot (w.UniFrac) (Cd = 0)")

plot_pcoa(pl_rm_cd10, distance_method = "unifrac", weighted = TRUE, color_var = "group", shape_var = "group", title = "PCoA plot (w.UniFrac) (Cd = 10)")

plot_pcoa(pl_rm_cd100, distance_method = "unifrac", weighted = TRUE, color_var = "group", shape_var = "group", title = "PCoA plot (w.UniFrac) (Cd = 100)")
