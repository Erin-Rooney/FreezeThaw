# Erin Rooney
# August 3 2020
# Pore throat statistics

source("code/0-packages.R")

# Load Data ---------------------------------------------------------------------

# breadthdata_csv = read.csv("processed/ftc_porethroatdist_july312020_2.csv") 
breadthdata_csv = read.csv("processed/5bin_breadth_nov22020.csv")
eqradiusdata_csv = read.csv("processed/405016eqradius.csv")
alldata_csv = read.csv("processed/405016_allmeasures.csv")
compiled_csv=read.csv("processed/fulldata_ftc_xct.csv")
feret_width=read.csv("processed/diameter_ferettest_longwidth.csv")
feret_breadth=read.csv("processed/diameter_ferettest_longbreadth.csv")



plot(breadthdata_csv)

eqradiusdata_csv %>% 
  ggplot(aes(x= breadth_um, y=freq2))+
  geom_point(alpha = 0.5, size = 4, color = "blue")+
  geom_line(color = "gray30")+
  scale_fill_manual(values = c("#b0986c")) +
  scale_y_continuous(labels = (scales::percent),
                     limits = c(0.00,0.20),
                     breaks = seq(0.00,0.20,0.05))+
  labs(y = "frequency",
       x = "Eq Radius, μm")+
  ylim(0, 0.15)+
  theme_er1()
  
  


breadthdata_csv %>%
  filter(sample == '40-50-16' & trmt == "before") %>% 
  ggplot(aes(x= breadth_um, y=freq))+
  geom_point(alpha = 0.5, size = 4, color = "purple")+
  geom_line(color = "gray30")+
  #scale_fill_manual(values = c("#b0986c")) +
  scale_y_continuous(labels = (scales::percent),
                     limits = c(0.00,0.20),
                     breaks = seq(0.00,0.20,0.05))+
  labs(y = "frequency",
       x = "Breadth, μm")+
  ylim(0, 0.15)+
theme_er1()
  

alldata_csv %>%
  ggplot(aes(x= area_um, y=eqradius_um))+
  geom_point()+
  theme_er1()

alldata_csv %>%
  ggplot(aes(x= channellength_um, y=eqradius_um))+
  geom_point()+
  theme_er1()

alldata_csv %>%
  ggplot(aes(x= channellength_um, y=eqradius_um))+
  geom_point()+
  theme_er1()

compiled_csv %>%
  ggplot(aes(x= pore_shape_factor, y=eq_diam_mm, color = ftc))+
  geom_point()+
  theme_er1()+
  facet_grid(moisture~bottom)

# manual test

library(nlme)
l = lme(um ~ type, random = ~1|pore_throat, na.action = na.omit, data = feret)
summary(l)
print(l)
anova(l)

# multiple linear regression example

breadth_wider = 
  feret_breadth %>% 
  pivot_wider(values_from = 'um', names_from = 'type')

fit_breadth <- lm(Expected ~ Breadth, data = breadth_wider)

summary(fit_breadth)

plot(Expected ~ Breadth, data = breadth_wider)+
  abline(lm(Expected ~ Breadth, data = breadth_wider))

library(ggpmisc)


ggscatter(
  breadth_wider, x = 'Expected', y = 'Breadth', 
  add = 'reg.line'
  )+
  stat_cor(label.y = 400)+
  stat_regline_equation(label.y = 420)+
  labs(x = "Manually calculated pore throat diameter, μm",
       y = "Feret Breadth, μm")
  
  



width_wider = 
  feret_width %>% 
  pivot_wider(values_from = 'um', names_from = 'type')

fit_width <- lm(Expected ~ Width, data = width_wider)

summary(fit_width)

plot(Expected ~ Width, data = width_wider)+
  abline(lm(Expected ~ Width, data = width_wider))


ggscatter(
  width_wider, x = 'Expected', y = 'Width', 
  add = 'reg.line'
)+
  stat_cor(label.y = 400)+
  stat_regline_equation(label.y = 420)+
  labs(x = "Manually calculated pore throat diameter, μm",
       y = "Feret Width, μm")
  


