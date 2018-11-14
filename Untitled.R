filename <- "~/Documents/Projects/Trehalose6phosphate/EMSscreen/EMS_candidates_rephenotyping.csv"
mydata <- read.csv(filename, sep = ";", stringsAsFactors = FALSE)

selection <- mydata %>% filter(gene.ID =="At5g44800") %>% arrange(X...line)

ggplot(data = selection, aes(x = X...line, y = phenotype)) + geom_jitter()
data(mtcars)
ggplot(mtcars, aes(x = mpg)) + geom_dotplot()
