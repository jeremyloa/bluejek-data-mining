# Qualif Data Mining JM22-1
# 1. Introduction
score <- read.csv('Score.csv', fileEncoding = 'UTF-8-BOM')
scoreweight <- read.csv('ScoreWeight.csv', fileEncoding = 'UTF-8-BOM')

score <- score[score$Course.Code %in% scoreweight$Course.Code,]
scoreweight <- scoreweight[scoreweight$Course.Code %in% score$Course.Code,]

score <- score[order(score$Course.Code),]
scoreweight <- scoreweight[order(scoreweight$Course.Code),]

merged <- merge(score, scoreweight, by = 'Course.Code')
merged$total <- round((merged$Assignment.x * merged$Assignment.y) + (merged$Mid.Exam.x * merged$Mid.Exam.y) + (merged$Final.Exam.x * merged$Final.Exam.y))

grade.intervals <- c(66,71,76,81,86,90,100)
grade.labels    <- c("D","C","B-","B","B+","A-","A")

for (i in 1:length(merged$total)) {
  if (merged$total[i] < grade.intervals[1]) {
    merged$grade[i] <- grade.labels[1]
  } else if (merged$total[i] < grade.intervals[2]) {
    merged$grade[i] <- grade.labels[2]
  } else if (merged$total[i] < grade.intervals[3]) {
    merged$grade[i] <- grade.labels[3]
  } else if (merged$total[i] < grade.intervals[4]) {
    merged$grade[i] <- grade.labels[4]
  } else if (merged$total[i] < grade.intervals[5]) {
    merged$grade[i] <- grade.labels[5]
  } else if (merged$total[i] < grade.intervals[6]) {
    merged$grade[i] <- grade.labels[6]
  } else {
    merged$grade[i] <- grade.labels[7]
  }
}

write.csv(
  data.frame(Student.Name = merged$Student.Name,
             Total.Score = merged$total,
             Grade = merged$grade),
  "./Result.csv", row.names = FALSE
)

# 2. Data Visualization
anime <- read.csv('anime.csv', encoding = 'UTF-8-BOM')
anime <- na.omit(anime)

# Pie Chart: Sources of Anime
pie (
  table(anime$source),
  col = rainbow(10),
  main = strwrap("Sources of Anime")
)

legend(
  "right",
  cex = 0.7,
  legend = as.data.frame(table(anime$source))$Var1,
  fill = rainbow(10)
)

# Bar Plot: Types of Anime
barplot(
  table(anime$type),
  col = rainbow(6),
  main = strwrap("Types of Anime")
)

legend(
  "top",
  cex = 0.7,
  legend = as.data.frame(table(anime$type))$Var1,
  fill = rainbow(6)
)  

# Histogram: 
hist(
  x = table(anime$score),
  main = strwrap("Anime Score"),
)

# Plot: Anime Aired from Year
plot(
  table(anime$aired_from_year),
  main = strwrap("Anime Aired from Year"),
  type = "h"
)

# 3. Data Description and Frequent Pattern Analysis
header <- read.csv('Header.csv', fileEncoding = 'UTF-8-BOM')
detail <- read.csv('Detail.csv', fileEncoding = 'UTF-8-BOM')
items <- read.csv('Items.csv', fileEncoding = 'UTF-8-BOM')
header <- na.omit(header)
detail <- na.omit(detail)
items <- na.omit(items)

trans = merge(header, detail, by.x = "header_id", by.y = "transaction_id")
trans = merge(trans, items, by.x = "header_id", by.y = "id")
trans <- trans[complete.cases(trans), ]
summary(trans)
install.packages('arules')
library(arules)
rules <- apriori(split(trans$name, trans$user_id), parameter = list(support = 0.05), target = 'frequent itemsets')
inspect(rules)
help(lhs)

assoc <- ruleInduction(rules, confidence = 0.5)
inspect(assoc)

