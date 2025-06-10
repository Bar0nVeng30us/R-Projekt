library(ggplot2)
# b
data_dir <- "data/weight-test-2025.txt"

weight_test <- read.table(file=data_dir,
                         header=TRUE,
                         sep=" ",
                         as.is=TRUE)

# c
n_row <- nrow(weight_test)
n_hühnchen <- length(unique(weight_test$Chick))

#d

n_diff_alter <- table(weight_test$Time)
diff_alter <- names(n_diff_alter)
n_diff_alter <- matrix(n_diff_alter)

# e
weight_test[weight_test == 1060] <- 106
weight_test[weight_test == "Plan_5"] <- "Plan_4"

#f
durchschnittsgewicht <- tapply(weight_test$weight, weight_test$Time, mean)

#Ausgaben
cat("Der Datensatz enthält:\n", n_hühnchen, "Hüchnchen\n",
            n_row, "Gesamtbeobachtungen\n\n ")

cat(paste(diff_alter, "Tage nach dem Schlüpfen wurden",
          n_diff_alter, "Messungen gemacht.\n"))

cat(paste("\nDas Durchschnittsgewicht nach", diff_alter,
          "Tagen ist:", durchschnittsgewicht, "g."))

# g
print(ggplot(
  data=weight_test,
  mapping=aes(x=Time,
              y=weight,
              color=factor(Diet)))+
    geom_point())

# h
qqnorm(scale(weight_test$weight)) #To scale() or not to scale()? That is the Question.
qqline(scale(weight_test$weight))

cat("\n\nDen Ergebnissen aus dem Q-Q-Plot ist zu entnehmen,
    dass die Gewichtsdaten nicht normalverteilt sind.")