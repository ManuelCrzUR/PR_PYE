install.packages("dplyr")
library(dplyr)

twins <- read.csv("C:/Users/manue/Desktop/PYE_Final/twins.txt", header = FALSE)

db_twins <- select(twins, HRWAGEH, HRWAGEL)
