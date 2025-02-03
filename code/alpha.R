# use phyloseq package to plot alpha diversity using pl_unra object. note the pl_unra object is a phyloseq object with unrarefied data.

library(phyloseq)
library(ggplot2)
library(vegan)
library(gridExtra)
library(agricolae)


# Calculate alpha diversity using phyloseq::estimate_richness ----
a_div <- estimate_richness(pl_unra) 
# can use measures = c("Shannon", "Simpson", "Chao1", "ACE", "Observed") to specify the diversity indices

head(a_div)
metadata <- sample_data(pl_unra)



# merge a_div and metadata by rownames
a_div <- cbind(a_div, metadata)

# remove the Ctrl group
a_div.f <- a_div %>% filter(group != "Ctrl")

# plot alpha diversity ----

library(ggplot2)

# Assuming you have a data frame called 'a_div.f'
p_shannon <- ggplot(a_div.f, aes(x = group, y = Shannon, fill = cd)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.1), alpha = 0.33) +
  labs(x = "Exudate type added to soil", y = "Shannon Index") +
  scale_fill_manual(values = c("white", "lightgrey", "darkgrey")) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), linetype = "dashed", color = "darkgrey")+
  # remove grid
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  # remove legend
  theme(legend.position = 'none')
p_shannon 

p_observed <- ggplot(a_div.f, aes(x = group, y = Observed, fill = cd)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.1), alpha = 0.33) +
  labs(x = "Exudate type added to soil", y = "Observed no. of ASVs") +
  scale_fill_manual(values = c("white", "lightgrey", "darkgrey")) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), linetype = "dashed", color = "darkgrey")+
  # remove grid
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = c(0.14, 0.15))+
  # remove legend backgroud
  theme(legend.background = element_blank())
p_observed

p_chao1 <- ggplot(a_div.f, aes(x = group, y = Chao1, fill = cd)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.1), alpha = 0.33) +
  labs(x = "Exudate type added to soil", y = "Chao1 index") +
  scale_fill_manual(values = c("white", "lightgrey", "darkgrey")) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), linetype = "dashed", color = "darkgrey")+
  # remove grid
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  # remove legend
  theme(legend.position = 'none')
p_chao1

p_ACE <- ggplot(a_div.f, aes(x = group, y = Chao1, fill = cd)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.1), alpha = 0.33) +
  labs(x = "Exudate type added to soil", y = "ACE index") +
  scale_fill_manual(values = c("white", "lightgrey", "darkgrey")) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), linetype = "dashed", color = "darkgrey")+
  # remove grid
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  # remove legend
  theme(legend.position = 'none')
p_ACE

p_fisher <- ggplot(a_div.f, aes(x = group, y = Fisher, fill = cd)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.1), alpha = 0.33) +
  labs(x = "Exudate type added to soil", y = "Fisher index") +
  scale_fill_manual(values = c("white", "lightgrey", "darkgrey")) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), linetype = "dashed", color = "darkgrey")+
  # remove grid
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  # remove legend
  theme(legend.position = 'none')
p_fisher

p_simpson <- ggplot(a_div.f, aes(x = group, y = Simpson, fill = cd)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.1), alpha = 0.33) +
  labs(x = "Exudate type added to soil", y = "Simpson index") +
  scale_fill_manual(values = c("white", "lightgrey", "darkgrey")) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), linetype = "dashed", color = "darkgrey")+
  # remove grid
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  # remove legend
  theme(legend.position = 'none')
p_simpson

p_invsimpson <- ggplot(a_div.f, aes(x = group, y = InvSimpson, fill = cd)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_jitter(position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.1), alpha = 0.33) +
  labs(x = "Exudate type added to soil", y = "Inverse Simpson index") +
  scale_fill_manual(values = c("white", "lightgrey", "darkgrey")) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), linetype = "dashed", color = "darkgrey")+
  # remove grid
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  # remove legend
  theme(legend.position = 'none')
p_invsimpson

# combine the plots and labels a and b ----

a_div_new <- ggarrange(p_observed, p_shannon, 
                       ncol = 2, 
                       labels = c("a", "b"))
a_div_new

if(save){
  ggsave('out/alpha_div.jpg', plot = a_div_new, 
         w = 66*2.5, h = 88*1.5, units = 'mm', dpi = 300)
  ggsave('out/alpha_div.pdf', plot = a_div_new, 
         w = 66*2.5, h = 88*1.5, units = 'mm', dpi = 300)
}

# anova and tukey test for observed ----
a_div.f.mut <- a_div.f %>% 
  mutate(group2 = paste(group, cd, sep = "_"))
a_div.f.mut.nut <- a_div.f.mut %>% 
  filter(group == "Nutrient")
a_div.f.mut.med <- a_div.f.mut %>% 
  filter(group == "Medick")
a_div.f.mut.tom <- a_div.f.mut %>% 
  filter(group == "Tomato")
a_div.f.mut.mai <- a_div.f.mut %>%
  filter(group == "Maize")

# build aov function ----
aov2 <- function(data, index, grouping) {
  # Create the formula
  formula <- as.formula(paste(index, "~", grouping))
  
  # Perform ANOVA
  aov_result <- stats::aov(formula, data = data)
  
  # Tukey test using agricolae
  tukey <- agricolae::HSD.test(aov_result, trt = grouping, group = TRUE, console = TRUE)
  
  return(tukey)
}

# usage
aov2(a_div.f.mut.nut, 'Observed', 'cd')
aov2(a_div.f.mut.med, 'Observed', 'cd') # *
aov2(a_div.f.mut.tom, 'Observed', 'cd')
aov2(a_div.f.mut.mai, 'Observed', 'cd')

aov2(a_div.f.mut.nut, 'Shannon', 'cd')# *
aov2(a_div.f.mut.med, 'Shannon', 'cd')
aov2(a_div.f.mut.tom, 'Shannon', 'cd')# *
aov2(a_div.f.mut.mai, 'Shannon', 'cd')

aov2(a_div.f.mut.nut, 'Chao1', 'cd')
aov2(a_div.f.mut.med, 'Chao1', 'cd')# *
aov2(a_div.f.mut.tom, 'Chao1', 'cd')
aov2(a_div.f.mut.mai, 'Chao1', 'cd')

aov2(a_div.f.mut.nut, 'ACE', 'cd')
aov2(a_div.f.mut.med, 'ACE', 'cd')# *
aov2(a_div.f.mut.tom, 'ACE', 'cd')
aov2(a_div.f.mut.mai, 'ACE', 'cd')

aov2(a_div.f.mut.nut, 'Fisher', 'cd')# *
aov2(a_div.f.mut.med, 'Fisher', 'cd')# *
aov2(a_div.f.mut.tom, 'Fisher', 'cd')# *
aov2(a_div.f.mut.mai, 'Fisher', 'cd')

aov2(a_div.f.mut.nut, 'Simpson', 'cd')# *
aov2(a_div.f.mut.med, 'Simpson', 'cd')
aov2(a_div.f.mut.tom, 'Simpson', 'cd')# *
aov2(a_div.f.mut.mai, 'Simpson', 'cd')

aov2(a_div.f.mut.nut, 'InvSimpson', 'cd')# *
aov2(a_div.f.mut.med, 'InvSimpson', 'cd')
aov2(a_div.f.mut.tom, 'InvSimpson', 'cd')# *
aov2(a_div.f.mut.mai, 'InvSimpson', 'cd')

# two-way ANOVA using factors cd and group. the dataframe a_div.f.mut contains factor columns 'group' and 'cd'. The valuables to test is Obeserved, Shannon, Chao1, Simpson, Fisher, and ACE ----
# Load necessary libraries
library(car)

# Define a function to perform two-way ANOVA
two_way_anova <- function(df, response, factor1, factor2) {
  formula <- as.formula(paste(response, "~", factor1, "*", factor2))
  aov_result <- stats::aov(formula, data = df)
  summary(aov_result)
}

# List of variables to test
variables <- c("Observed", "Shannon", "Chao1", "Simpson", "Fisher", "ACE")

# Perform two-way ANOVA for each variable
results <- lapply(variables, function(var) {
  two_way_anova(a_div.f.mut, var, "cd", "group")
})

# Print results
names(results) <- variables
results


# Extract relevant information from the summary objects
extract_summary <- function(summary_obj) {
  summary_df <- as.data.frame(summary_obj[[1]])
  return(summary_df)
}

# Apply the extraction function to each result
results_df_list <- lapply(results, extract_summary)

# Combine the results into a single data frame
combined_results_df <- do.call(rbind, results_df_list)

# Save the combined results to a CSV file
write.csv(combined_results_df, "out/two-way_anova_results.csv", row.names = TRUE)

# combine all alpha diversity with labels "a" to "f" using ggarrrange ----
a_div_all <- ggarrange(p_observed, p_shannon, p_chao1, p_ACE, p_fisher, p_simpson, 
                       nrow = 3, ncol = 2, 
                       labels = c("a", "b", "c", "d", "e", "f", "g"))
a_div_all



if(save){
  ggsave('out/alpha_div_all.jpg', plot = a_div_all, 
         w = 66*2.2, h = 88*3.4, units = 'mm', dpi = 300)
  ggsave('out/alpha_div_all.pdf', plot = a_div_all, 
         w = 66*2.2, h = 88*3.4, units = 'mm', dpi = 300)
}



# Since we expect that abiotic stress would change the chemical profile of the root exudates and subsequently the associated bacterial communities, we compare bacterial alpha diversity across different Cd concentrations. 

plot_richness(subset_samples(pl_unra, group != "Ctrl"), x = "group", color = "cd", measures = c("Observed", "Shannon", "Chao1"))+
  # set size of boxplot to 0.5
  geom_boxplot() + 
  geom_jitter(width = 0.2, alpha = 0.5)+
  theme(legend.position = "none")
  
  

df_ctrl <- a_div %>% 
  filter(group == "Ctrl") 

p_ctrl1 <- plot_box(df_ctrl, 'Shannon', 'cd')+
  # remove x lab 
  xlab('')+
  # revise legend title as "Cd conc.", and put the legend on the right top corner c(80, 80) within the figure frame
  theme(legend.position = "none")
  
  
p_ctrl2 <- plot_box(df_ctrl, 'InvSimpson', 'cd')+
  xlab('')+
  # remove legend 
  theme(legend.position = "none")

p_ctrl3 <- plot_box(df_ctrl, 'ACE', 'cd')+
  xlab('Cd conc. used to sculpt root exudates (µM)')+
  # remove legend 
  theme(legend.position = "none")

p_ctrl4 <- plot_box(df_ctrl, 'Fisher', 'cd')+
  xlab('Cd conc. used to sculpt root exudates (µM)')+
  # remove legend 
  theme(legend.position = "none")


# combine and add plot title 'Control'
alpha_ctrl <- grid.arrange(p_ctrl1, p_ctrl2, p_ctrl3, p_ctrl4, ncol = 2, nrow = 2, top = "Control")
alpha_ctrl

# save the plot

if(save){
  ggsave('out/alpha_ctrl.jpg', plot = alpha_ctrl, 
         w = 88*2, h = 66*2, units = 'mm', dpi = 300)
  ggsave('out/alpha_ctrl.pdf', plot = alpha_ctrl, 
         w = 88*2, h = 66*2, units = 'mm', dpi = 300)
}






df_nut <- a_div %>% 
  filter(group == "Nutrient") 

p_nut1 <- plot_box(df_nut, 'Shannon', 'cd')+
  xlab('')+
  theme(legend.position = "none")

p_nut2 <- plot_box(df_nut, 'InvSimpson', 'cd')+
  xlab('')+
  theme(legend.position = "none")

p_nut3 <- plot_box(df_nut, 'ACE', 'cd')+
  xlab('Cd conc. used to sculpt root exudates (µM)')+
  theme(legend.position = "none")

p_nut4 <- plot_box(df_nut, 'Fisher', 'cd')+
  xlab('Cd conc. used to sculpt root exudates (µM)')+
  theme(legend.position = "none")

alph_nut <- grid.arrange(p_nut1, p_nut2, p_nut3, p_nut4, ncol = 2, nrow = 2, top = "Nutrient")

if(save){
  ggsave('out/alpha_nut.jpg', plot = alph_nut, 
         w = 88*2, h = 66*2, units = 'mm', dpi = 300)
  ggsave('out/alpha_nut.pdf', plot = alph_nut, 
         w = 88*2, h = 66*2, units = 'mm', dpi = 300)
}



df_medick <- a_div %>% 
  filter(group == "Medick") 

p_medick1 <- plot_box(df_medick, 'Shannon', 'cd')+
  xlab('')+
  theme(legend.position = "none")

p_medick2 <- plot_box(df_medick, 'InvSimpson', 'cd')+
  xlab('')+
  theme(legend.position = "none")

p_medick3 <- plot_box(df_medick, 'ACE', 'cd')+
  xlab('Cd conc. used to sculpt root exudates (µM)')+
  theme(legend.position = "none")

p_medick4 <- plot_box(df_medick, 'Fisher', 'cd')+
  xlab('Cd conc. used to sculpt root exudates (µM)')+
  theme(legend.position = "none")

alph_med <- grid.arrange(p_medick1, p_medick2, p_medick3, p_medick4, ncol = 2, nrow = 2, top = "Medick")

if(save){
  ggsave('out/alpha_med.jpg', plot = alph_med, 
         w = 88*2, h = 66*2, units = 'mm', dpi = 300)
  ggsave('out/alpha_med.pdf', plot = alph_med, 
         w = 88*2, h = 66*2, units = 'mm', dpi = 300)
}



df_tom <- a_div %>% 
  filter(group == "Tomato")

p_tom1 <- plot_box(df_tom, 'Shannon', 'cd')+
  xlab('')+
  theme(legend.position = "none")

p_tom2 <- plot_box(df_tom, 'InvSimpson', 'cd')+
  xlab('')+
  theme(legend.position = "none")

p_tom3 <- plot_box(df_tom, 'ACE', 'cd')+
  xlab('Cd conc. used to sculpt root exudates (µM)')+
  theme(legend.position = "none")

p_tom4 <- plot_box(df_tom, 'Fisher', 'cd')+
  xlab('Cd conc. used to sculpt root exudates (µM)')+
  theme(legend.position = "none")

alpha_tom <- grid.arrange(p_tom1, p_tom2, p_tom3, p_tom4, ncol = 2, nrow = 2, top = "Tomato")
if(save){
  ggsave('out/alpha_tom.jpg', plot = alpha_tom, 
         w = 88*2, h = 66*2, units = 'mm', dpi = 300)
  ggsave('out/alpha_tom.pdf', plot = alpha_tom, 
         w = 88*2, h = 66*2, units = 'mm', dpi = 300)
}

df_mai <- a_div %>% 
  filter(group == "Maize") 

p_mai1 <- plot_box(df_mai, 'Shannon', 'cd')+
  xlab('')+
  theme(legend.position = "none")

p_mai2 <- plot_box(df_mai, 'InvSimpson', 'cd')+
  xlab('')+
  theme(legend.position = "none")

p_mai3 <- plot_box(df_mai, 'ACE', 'cd')+
  xlab('Cd conc. used to sculpt root exudates (µM)')+
  theme(legend.position = "none")

p_mai4 <- plot_box(df_mai, 'Fisher', 'cd')+
  xlab('Cd conc. used to sculpt root exudates (µM)')+
  theme(legend.position = "none")

alpha_mai <- grid.arrange(p_mai1, p_mai2, p_mai3, p_mai4, ncol = 2, nrow = 2, top = "Maize")
if(save){
  ggsave('out/alpha_mai.jpg', plot = alpha_mai, 
         w = 88*2, h = 66*2, units = 'mm', dpi = 300)
  ggsave('out/alpha_mai.pdf', plot = alpha_mai, 
         w = 88*2, h = 66*2, units = 'mm', dpi = 300)
}

# END ----









