head(a_div)
head(div)

# subset groups what contain Medick, Tomato, and Maize
a_div_f <- a_div %>% 
  filter(group == "Medick" | group == "Tomato" | group == "Maize")
a_div_f

# mutation and generate a column group2 by add group and cd
a_div_f_mean <- a_div_f %>% 
  mutate(group2 = paste(group, cd, sep = "_")) %>% 
  group_by(group2) %>%
  summarise(across(c(Shannon, InvSimpson, ACE, Fisher), mean))


# set factor levels medick 0, 10, tomato 0, 10, 100, maize 0, 10, 100
a_div_f_mean$group2 <- factor(a_div_f_mean$group2, levels = c("Medick_0", "Medick_10", "Tomato_0", "Tomato_10", "Tomato_100", "Maize_0", "Maize_10", "Maize_100"))
a_div_f_mean

div


# add group2 to div with "Medick_0", "Medick_10", "Tomato_0", "Tomato_10", "Tomato_100", "Maize_0", "Maize_10", "Maize_100"
div$group2 <- c("Medick_0", "Medick_10", "Tomato_0", "Tomato_10", "Tomato_100", "Maize_0", "Maize_10", "Maize_100")

# merge div and a_div_f_mean
df <- merge(div, a_div_f_mean, by = "group2")

# correlate between df$div and df$Shannon; df$div and df$ACE; df$div and df$Fisher; df$div and df$InvSimpson; and plot
cor(df$div, df$Shannon)
cor(df$div, df$ACE)
cor(df$div, df$Fisher)
cor(df$div, df$InvSimpson)

# show significance level
cor.test(df$div, df$Shannon)
cor.test(df$div, df$ACE)
cor.test(df$div, df$Fisher)
cor.test(df$div, df$InvSimpson)


# plot
p1 <- ggplot(df, aes(x = div, y = Shannon))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE)+
  theme(legend.position = "none")+
  labs(x = "Chemodiversity (Shannon)", y = "Shannon")

p2 <- ggplot(df, aes(x = div, y = ACE))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE)+
  theme(legend.position = "none")+
  labs(x = "Chemodiversity (Shannon)", y = "ACE")

p3 <- ggplot(df, aes(x = div, y = Fisher))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE)+
  theme(legend.position = "none")+
  labs(x = "Chemodiversity (Shannon)", y = "Fisher")

p4 <- ggplot(df, aes(x = div, y = InvSimpson))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE)+
  theme(legend.position = "none")+
  labs(x = "Chemodiversity (Shannon)", y = "InvSimpson")

# arrange plot and add labels a, b, c, and d, using ggarrange
p.all <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, labels = c('a', 'b', 'c', 'd'))
p.all
# save plot
if(save){
  ggsave('out/cor_div.jpg', plot = p.all, 
         w = 88*2, h = 66*2, units = 'mm', dpi = 300)
  ggsave('out/cor_div.pdf', plot = p.all, 
         w = 88*2, h = 66*2, units = 'mm', dpi = 300)
}


