# loading packages ----
library(tidyverse)
library(ggplot2)
library(dplyr)

# loading data ----

filename <- "~/Documents/GIThub/Anusha_FT/Files/20181107_Anusha_exp_results.csv"
data_original <- read_csv(filename)


# prepare data ----

data_w_total<- data_original %>% mutate(total_leaves = rosette_leaves + cauline_leaves) %>% mutate(hexcolor = as.numeric())
data_w_total <- data_w_total %>% mutate(num_pch = 0)


for (r in 1:nrow(data_w_total)) 
  {
  if (data_w_total$TEV_line[r] != "none")
    {
    data_w_total$num_pch[r] = 1
    }
  if (data_w_total$FT_line[r] != "none")
    {
    data_w_total$num_pch[r] = 2
    }
  if (data_w_total$FT_line[r] != "none" & data_w_total$TEV_line[r] != "none")
    {
    data_w_total$num_pch[r] = 4
    }
  }
# PCA-like graph ----

#add column for line/colors.
data_w_total$hexcolor <- c()

col0 <- "000000"
ft10 <- "919191"
jv268 <- "00C3C9"
fk491 <- "0216E8"
jv268x8 <- "9CD41C"
jv268x9 <- "C974C8"
fk491x8 <- "0E7021"
fk491x9 <- "571370"
fk8 <- "E8A100"
fk9 <- "BD0009"



for (r in 1:nrow(data_w_total))
{
  
  
  if (data_w_total$line[r] == "pJV268xpFK428#9")
  {
    data_w_total$hexcolor[r] = jv268x9
  }

}


# plotting ----
data_w_total %>% ggplot(aes(dtf, total_leaves, color = line, shape = num_pch)) + geom_jitter() + 
  scale_color_manual(values = c("#000000", "#919191", "#E8A100", "#BD0009", "#0216E8", "#0E7021", "#835EBD", "#00C3C9", "#9CD41C", "#C974C8")) + scale_shape_identity()
  
# would it be interesting to add the mean (or median) to the data? Probably only if they are equally split.

mean(data_w_total$dtf) #24.65
median(data_w_total$dtf) #23

mean(data_w_total$total_leaves) #22.35
median(data_w_total$total_leaves) #19
# no it doesnt make any sense.
