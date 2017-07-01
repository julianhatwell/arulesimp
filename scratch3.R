
# more mice
iris.mis <- synth_missing(iris, prob = 0.25)

#Check missing values introduced in the data
summary(iris.mis)
str(iris.mis)

