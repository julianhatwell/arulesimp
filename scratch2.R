subrules <- rules[quality(rules)$lift > 2]
plot(subrules)


operacars <- extract_cars(cars[["Opera"]])

chemcars <- extract_cars(cars[["Chemistry"]])
rows_to_impute <- which(is.na(responses[["Chemistry"]]))

