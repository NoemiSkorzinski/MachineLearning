#First tries in machine learning using "Anusha" dataset

#load libraries ----
library(caret) #includes packages "lattice" and "ggplot2"
library(dplyr)
library(tidyverse)


# loading and preparing data ----
filename <- "~/Documents/GIThub/Anusha_FT/Files/20181107_Anusha_exp_results.csv"
FTTEVdata <- read_csv(filename)
FTTEVdata<- FTTEVdata %>% mutate(total_leaves = rosette_leaves + cauline_leaves)


  # test 1: using controls for model building and crosses for "testing". ----
  
  mysplit <- split(FTTEVdata, FTTEVdata$TEV_line != "none" & FTTEVdata$FT_line != "none")
  controls <- mysplit[[1]]
  # unique(controls$line)
  crosses <- mysplit[[2]]
  #unique(crosses$line)

# adding "early" "late" information to the controls. (For supervised learning.)
FTTEVdata <- FTTEVdata %>% mutate(flowering = NA)
for (r in 1:nrow(FTTEVdata))
{
  if (FTTEVdata$line[r] == "ft-10")
  {
    FTTEVdata$flowering[r] = "late"
  }
  else if (FTTEVdata$line[r] == "Col-0")
  {
    FTTEVdata$flowering[r] = "early"
  }
  else if (FTTEVdata$line[r] == "pJV268"| FTTEVdata$line[r] == "pFK491")
  {
    FTTEVdata$flowering[r] = "late"
  }
  else if (FTTEVdata$line[r] == "pFK428#8"| FTTEVdata$line[r] == "pFK428#9")
  {
    FTTEVdata$flowering[r] = "early"
  }
}


# preparing data for plotting:
# pictogram
FTTEVdata <- FTTEVdata %>% mutate(num_pch = 0)


for (r in 1:nrow(FTTEVdata)) 
{
  if (FTTEVdata$TEV_line[r] != "none")
  {
    FTTEVdata$num_pch[r] = 1
  }
  if (FTTEVdata$FT_line[r] != "none")
  {
    FTTEVdata$num_pch[r] = 2
  }
  if (FTTEVdata$FT_line[r] != "none" & FTTEVdata$TEV_line[r] != "none")
  {
    FTTEVdata$num_pch[r] = 4
  }
}

#colour #doesn't work... :-/
  # FTTEVdata<- FTTEVdata %>% mutate(hexcolor = NA)
  # for (r in 1:nrow(FTTEVdata))
  # {
  #   if (FTTEVdata$line[r] == "Col-0")
  #   {
  #     FTTEVdata$hexcolor[r] = "#000000"
  #   }
  #   else if (FTTEVdata$line[r] == "ft-10")
  #   {
  #     FTTEVdata$hexcolor[r] = "#919191"
  #   }
  #   else if (FTTEVdata$line[r] == "pJV268")
  #   {
  #     FTTEVdata$hexcolor[r] = "#00C3C9"
  #   }
  #   else if (FTTEVdata$line[r] == "pFK491")
  #   {
  #     FTTEVdata$hexcolor[r] = "#0216E8"
  #   }
  #   else if (FTTEVdata$line[r] == "pFK428#8")
  #   {
  #     FTTEVdata$hexcolor[r] = "#E8A100"
  #   }
  #   else if (FTTEVdata$line[r] == "pFK428#9")
  #   {
  #     FTTEVdata$hexcolor[r] = "#BD0009"
  #   }
  #   else if (FTTEVdata$line[r] == "pFK491xpFK428#8")
  #   {
  #     FTTEVdata$hexcolor[r] = "#0E7021"
  #   }
  #   else if (FTTEVdata$line[r] == "pFK491xpFK428#9")
  #   {
  #     FTTEVdata$hexcolor[r] = "#571370"
  #   }
  #   else if (FTTEVdata$line[r] == "pJV268xpFK428#8")
  #   {
  #     FTTEVdata$hexcolor[r] = "#9CD41C"
  #   }
  #   else if (FTTEVdata$line[r] == "pJV268xpFK428#9")
  #   {
  #     FTTEVdata$hexcolor[r] = "#C974C8"
  #   }
  # }

#subsetting the control dataset into Training and Testing
#set.seed makes the (pseudo-)random selection reproducible!
set.seed(1)
inTrainingSet <- createDataPartition(controls$line, p = 0.8 , list = FALSE)
controlsTrain <- controls[ inTrainingSet,]
controlsTest <- controls[-inTrainingSet,]

#setting predictors (Basically all variables except the "flowering" ("and maybe the line...)) #Does not work, because not all values are numerical!
#predictors <- names(controls) [names(controls) != "line" & names(controls) != "flowering"]

#doesn't work yet. Maybe because I skipped the preprocessing step...
# works if I only take numeric factors into account. Still warning "Setting row names on a tibble is deprecated"
numerics <- c("tray", "position", "dtf", "rosette_leaves" ,"cauline_leaves", "total_leaves")
procValues <- preProcess(controls8Train[,numerics], method = c("center", "scale", "YeoJohnson"))
trainScaled <- predict(procValues, controls8Train[,numerics])

#training the model. "y = " supervised learning.
gbmTest <- train(x = controlsTest[, numerics], y = controlsTest$flowering, method = "svmLinear2", verbose = FALSE)
gbmTest #display summary
ggplot(gbmTest) #summary graph

#test model
gbmPred <- predict(gbmTest, controlsTest)
gbmProbe <- predict(gbmTest, controlsTest, typ = "prob")
str(gbmPred)
str(gbmProbe)
#look at the data
head(gbmProbe)
gbmProbe
gbmPred

confusionMatrix(gbmPred, controlsTest$flowering)

for (x in 1:length(gbmPred))
{
  if (gbmPred[x] != controlsTest$flowering[x])
  {
    print(x, ": False")
  }
}

#
gbmCrosses <- predict(gbmTest, crosses)
str(gbmCrosses)
gbmCrosses

crosses <- cbind(crosses, gbmCrosses)
crosses %>% filter(crosses$gbmCrosses == "late")

crosses %>% filter(crosses$line == "pFK491xpFK428#9")


#----test 1B: Supervised learning, with crosses for prediction, splitting set8 and set9 data. (overlapping controls) ----

#splitting out pFK428-8 (set9)
split9 <- split(FTTEVdata, FTTEVdata$FT_line == "pFK428#8")
set9 <- split9[[1]]
set9 <- set9 %>% filter(set9$total_leaves > 10)

#splitting out pFK428-9 (set8)
split8 <- split(FTTEVdata, FTTEVdata$FT_line == "pFK428#9")
set8 <- split8[[1]]
set8 <- set8 %>% filter(set8$total_leaves > 10)

#plotting
set8 %>% ggplot(aes(dtf, total_leaves, color = line, shape = num_pch)) + geom_jitter() + scale_shape_identity() + 
  scale_color_manual(values = c("#000000", "#919191", "#E8A100", "#0216E8", "#0E7021", "#00C3C9", "#9CD41C"))
set9 %>% ggplot(aes(tray, total_leaves, color = line, shape = num_pch)) + geom_jitter() + scale_shape_identity() + 
  scale_color_manual(values = c("#000000", "#919191", "#BD0009", "#0216E8", "#835EBD", "#00C3C9", "#C974C8"))


#splitting into control and crosses datasets
mysplit <- split(set8, set8$TEV_line != "none" & set8$FT_line != "none")
controls8 <- mysplit[[1]]
crosses8 <- mysplit[[2]]

mysplit <- split(set9, set9$TEV_line != "none" & set9$FT_line != "none")
controls9 <- mysplit[[1]]
crosses9 <- mysplit[[2]]

#subsetting the control dataset into Training and Testing
inTrainingSet <- createDataPartition(controls8$line, p = 0.9 , list = FALSE)
controls8Train <- controls8[ inTrainingSet,]
controls8Test <- controls8[-inTrainingSet,]

inTrainingSet <- createDataPartition(controls9$line, p = 0.9 , list = FALSE)
controls9Train <- controls9[ inTrainingSet,]
controls9Test <- controls9[-inTrainingSet,]

numerics <- c("rosette_leaves" ,"cauline_leaves", "total_leaves", "dtf")
#--- set 8 ---#
#Training and testing the models

gbmTest8 <- train(x = controls8Train[,numerics], y = controls8Train$flowering, method = "gbm", verbose = FALSE)
gbmTest8 #display summary
ggplot(gbmTest8) #summary graph

#test model
gbmPred8 <- predict(gbmTest8, controls8Test)
gbmProbe8 <- predict(gbmTest8, controls8Test, typ = "prob")
str(gbmPred8)
str(gbmProbe8)
#look at the data
head(gbmProbe8)
gbmProbe8
gbmPred8
controls8Test$flowering

gbmCrosses8 <- predict(gbmTest8, crosses8)
str(gbmCrosses8)
gbmCrosses8


#--- set 9 ---#
#Training and testing the models

gbmTest9 <- train(x = controls9Train[,numerics], y = controls9Train$flowering, method = "gbm", verbose = FALSE)
gbmTest9 #display summary
ggplot(gbmTest9) #summary graph

#test model
gbmPred9 <- predict(gbmTest9, controls9Test)
gbmProbe9 <- predict(gbmTest9, controls9Test, typ = "prob")
str(gbmPred9)
str(gbmProbe9)
#look at the data
head(gbmProbe9)
gbmProbe9
gbmPred9
controls9Test$flowering

gbmCrosses9 <- predict(gbmTest9, crosses9)
str(gbmCrosses9)
gbmCrosses9

crosses9$gbmCrosses9 <- gbmCrosses9
crosses9 %>% filter(crosses9$gbmCrosses9 == "late")

crosses9 %>% select(line, sample, gbmCrosses9)

table(crosses9$gbmCrosses9)
filtered9 <- filter(crosses9, crosses9$line == "pJV268xpFK428#9")
table(filtered9$gbmCrosses9)

for (r in 1:length(gbmPred9))
{
  print(crosses9$line[r])
  print(crosses9$sample[r])
  print(gbmPred9[r])
}

as.factor(controls9Test$flowering)
confusionMatrix(table(gbmPred9, controls9Test$flowering))

gbmPred9

