library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggsci)
# sensitivity for length parameters
senalbi <- read_excel("SenLBSPR19.19.xlsx", sheet = 2)
senalbi
senlen <- senalbi %>% select(1,2, 4:6)
senlen <- senalbi %>% dplyr::select(1, 2, 4:6)

senlen$Code <- as.factor(senlen$Code)

senlen$Par <- factor(senlen$Par,
                     levels = unique(senlen$Par))

senlen %>% 
  #select(2:5) %>%  
  pivot_longer(cols = 3:5, names_to = "range", values_to = "value") %>% 
  ggplot(aes(Code, value, fill = Par))+
  geom_boxplot(show.legend = F)+
  facet_wrap(~Par, scales = "free_y")+
  scale_color_npg()+
  theme_bw()
