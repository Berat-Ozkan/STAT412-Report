data = read.csv("C:/Users/Lenovo/Desktop/STAT_412_Report/student_habits_performance.csv")
head(data)
summary(data)


install.packages("missMethods")
library(missMethods)

set.seed(412)
#adding %10 missing value
data_with_missing_value = delete_MCAR(ds = data, p = 0.08)


colSums(is.na(data_with_missing_value))

write.csv(data_with_missing_value, 'C:/Users/Lenovo/Desktop/STAT_412_Report/student_habits_performance_missing.csv', row.names = FALSE)


