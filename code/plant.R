plant <- read_xlsx('data/plant.xlsx')
toc_tn <- read_xlsx('data/toc_tn.xlsx')

plant$sp <- factor(plant$sp, levels = c('Medick', 'Tomato', 'Maize'))
plant$cd <- factor(plant$cd)

toc_tn$sp <- factor(toc_tn$sp, levels = c('Medick', 'Tomato', 'Maize'))
toc_tn$cd <- factor(toc_tn$cd)


p_rt_ms <- plot_plant(plant, "sp", "rt_wet_ms", "cd")+
  xlab('Plant species')+
  ylab('Root fresh mass (g/plant)')+
  labs(fill = 'Stress level') +
  theme(legend.position = c(0.3, 0.7))

p_sht_ms <- plot_plant(plant, "sp", "sht_wet_ms", "cd")+
  xlab('Plant species')+
  ylab('Shoot fresh mass (g/plant)')+
  theme(legend.position = 'none')

p_rt_l <- plot_plant(plant, "sp", "rt_length", "cd")+
  xlab('Plant species')+
  ylab('Root length (cm)')+
  theme(legend.position = 'none')

p_height <- plot_plant(plant, "sp", "height", "cd")+
  xlab('Plant species')+
  ylab('Plant height (cm)')+
  theme(legend.position = 'none')


p_plant <- ggarrange(p_rt_ms, p_sht_ms,
          p_rt_l, p_height,
          labels = c('a', 'b', 'c', 'd'), 
          ncol = 2, nrow = 2)
p_plant

if(save){
  ggsave('out/plant_plot.jpg', plot = p_plant, 
         w = 88*1.5, h = 66*2, units = 'mm', dpi = 300)
  
}

rm(p_rt_ms, p_sht_ms,
   p_rt_l, p_height)
