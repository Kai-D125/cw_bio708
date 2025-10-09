pacman::p_load(tidyverse,
               patchwork,
               here,
               pwr)

# 11.5.1 ------------------------------------------------------------------

#install data
plant_growth<-PlantGrowth

#visuals
plant_growth %>% 
  ggplot(aes(x = group,
             y = weight))+
  geom_violin(draw_quantiles = 0.5, 
              alpha = 0.5,
              fill = "skyblue") + 
  geom_jitter(width = 0.1, 
              alpha = 1) +
  theme_bw()

#ANOVA
a<-aov(weight ~ group, 
       data = plant_growth)
summary(a)

#Discussion
#the p-value, f-value, and degree of freedom 
#would be the most important values to report in a scientific article

# 11.5.2 ------------------------------------------------------------------

#pwr
pwr.anova.test(k = 3, 
               f = 0.5,
               sig.level = 0.05,
               power = 0.8)

#change in k
pwr.anova.test(k = 6, 
               f = 0.5,
               sig.level = 0.05,
               power = 0.8)

pwr.anova.test(k = 2, 
               f = 0.5,
               sig.level = 0.05,
               power = 0.8)

#change in f
pwr.anova.test(k = 3, 
               f = 0.1,
               sig.level = 0.05,
               power = 0.8)

pwr.anova.test(k = 3, 
               f = 0.75,
               sig.level = 0.05,
               power = 0.8)

#change in power
pwr.anova.test(k = 3, 
               f = 0.5,
               sig.level = 0.05,
               power = 0.95)

pwr.anova.test(k = 3, 
               f = 0.5,
               sig.level = 0.05,
               power = 0.5)

##calculating power

#changing n
pwr.anova.test(k = 3, 
               n = 5,
               f = 0.5,
               sig.level = 0.05)

pwr.anova.test(k = 3, 
               n = 2,
               f = 0.5,
               sig.level = 0.05)

pwr.anova.test(k = 3, 
               n = 10,
               f = 0.5,
               sig.level = 0.05)

#changing k
pwr.anova.test(k = 30, 
               n = 5,
               f = 0.5,
               sig.level = 0.05)

pwr.anova.test(k = 2, 
               n = 5,
               f = 0.5,
               sig.level = 0.05)

#changing f
pwr.anova.test(k = 3, 
               n = 5,
               f = 0.75,
               sig.level = 0.05)

pwr.anova.test(k = 3, 
               n = 5,
               f = 0.25,
               sig.level = 0.05)
