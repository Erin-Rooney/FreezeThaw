# Erin Rooney
# November 2 2020
# Pore throat statistics

source("code/0-packages.R")

# Load Data ---------------------------------------------------------------------
 
breadthdata_csv = read.csv("processed/prebinningxctbreadth.csv")

# Specialized theme--------------------------------------------------------------

theme_er <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=2, fill = NA),
          
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
          axis.text = element_text(size = 12, color = "black"),
          axis.title = element_text(size = 12, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}

# Order data and change sample names----------------------------------------------

levels(as.factor(breadthdata_csv$sample))

breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(sample = recode(sample, "40-50-16" = "Aggregate-1"))

breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(sample = recode(sample, "40-50-28" = "Aggregate-2"))

breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(sample = recode(sample, "28-38-12" = "Aggregate-3"))

breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(sample = recode(sample, "28-38-28" = "Aggregate-4"))

breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(sample = recode(sample, "41-50-16" = "Aggregate-5"))

breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(sample = recode(sample, "41-50-28" = "Aggregate-6"))

breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(sample = factor(sample, levels = c("Aggregate-1", "Aggregate-2", "Aggregate-3", "Aggregate-4", "Aggregate-5", "Aggregate-6")))

breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(trmt = factor(trmt, levels = c("before", "after")))

tool = breadthdata_csv %>% 
  filter(site=="tool")

# Binning-----------------------------------------------------------------------

tool %>% 
  dplyr::mutate(0.005 = if_else(breadth<0.005, paste0("0.005", 0.005)))
