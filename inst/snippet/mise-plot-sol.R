ggplot(aes(y = mise, x = adj, colour = kernel, group = kernel), 
       data = results %>% mutate(adj = factor(round(adjust, 2)))) +
  geom_line(alpha = 0.7) +
  facet_grid( dist ~ size, scales = "free_y")       

