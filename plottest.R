data <- read.csv(file.choose())
data$edmom <- as.factor(data$edmom)
levels(data$edmom) <- c("Less than high school completed", "High school diploma or equivalent", "Some college, vocational, or trade school", "Bachelors degree", "Masters degree", "Professional degree", "Doctorate", "Not applicable")
data$racem <- as.factor(data$racem)
levels(data$racem) <- c("Asian", "AI_AN", "Black", "White", "NH_OPI", "MR")
library(ggplot2)
library(tidyverse)

png("~/Desktop/plot.png",width=1000,height=800)
ggplot(data = data, aes(x = edmom, y = ugloanr, fill = edmom)) +
       geom_bar(stat = "identity") + 
       theme(plot.margin = unit(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), "cm")) +
       coord_flip() +
       scale_y_reverse() +
       scale_x_reverse(breaks=c(1:8))  
       labels(c("Less than high school completed", "High school diploma or equivalent", "Some college", "Bachelors degree", "Masters degree", "Professional degree", "Doctorate", "Not applicable")) +
       geom_text(aes(label = round(ugloanr_pct,0)))
       position = position_stack(vjust = 0.5) +
         labs(title ="Undergraduate Loan Amount of Millenials by Father's Edcuation Level")+
         theme(axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               axis.ticks.x = element_blank(),
               axis.ticks.y = element_blank(),
               axis.text.x = element_blank(),
               panel.background = element_blank(),
               strip.background = element_blank(),
               legend.position = c(0.8, 0.3))+
         facet_wrap(~data$racem)
         
       
       

