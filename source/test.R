library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

automobile <- read_csv("data/automobile.csv")
count(automobile)
ncol(automobile)


by_origin <- automobile %>% 
  group_by(origin) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(freq = n/sum(n),
         hi = cumsum(freq))


labels = c("USA", "Europa", "Japón")
pct=round(table(automobile$origin)/sum(table(automobile$origin))*100) 
labs=paste(labs, pct);labs=paste(labs, "%", sep = " ")
table(automobile$origin)
pie(table(automobile$origin), labels, 
    main = "Procedencia", 
    col = c("#023047", "#f4d35e", "#ee964b"))


barplot(automobile$model_year)

