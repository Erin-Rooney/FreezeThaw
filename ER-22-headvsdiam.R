# Erin Rooney
# Aug 5 2020
# conn vs unconn pore volumes

setwd("~/R/R/R Datasets/introductoryR-master")

head_diam = read.csv("processed/head_porediam_theory.csv") 
diameter_um = head_diam[head_diam$diameter_um,]
h_cm = head_diam[head_diam$h_cm,]
theory = head_diam[head_diam$site=="theory",]

x <- diameter_um
y <- h_cm

#p = plot(x=diameter_um, y=h_cm, main = "Water Retention by Pore Size", xlab = "pore throat diameter", ylab = "head", col=5)

#p = plot(as.numeric(diameter_um), as.numeric(h_cm), main = "Water Retention by Pore Size", xlab = "pore throat diameter", ylab = "head", col=5)

#plot(as.numeric(diameter_um), as.numeric(h_cm))

#p

library(ggplot2)

ggplot(theory, aes(x=diameter_um, y=h_cm)) +
  geom_dotplot(binwidth=1)

plot(x, y, main = "Main title",
     pch = 19, frame = FALSE)


plot(diameter_um, h_cm)

lines(lowess(x, y), col = "blue")

#dotchart(theory, aes(as.numeric(diameter_um), as.numeric(h_cm)))




