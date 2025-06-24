finddefectvalues <- function(data, ExpectedIDCount, ExpectedObservEnd)
{
    #Fehlerhaftes Gewicht abfangen

    #Falschen Ernährungsplan abfangen

    #Falsche ID abfangen

    #Falsche Messzeit abfangen

    #Falls die Zeit außerhalb des Zeitraums ist in dem wir gemessen haben, löschen wir die betroffenen Daten.
  
}


library(ggplot2)
# b
data_dir <- "/home/779072/Dokumente/weight-test-2025.txt"

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
# Wir führen hier eine lineare Regression durch, mit dem Gewicht als Zielvariable Y und der Zeit als abhängigen Variable
# Für die Regression verwenden wir nur die Werte, die in unserem Fehlertest keine Auffälligkeiten gezeigt haben
defectvalues <- finddefectvalues(weight_test)
lm(weight_test[!defectvalues])#Hier ist die Fehlermeldung

# Die Fehlerhaften Werte setzen wir dann neu auf den Wert der Regressionsgeraden zum passenden Zeitpunkt
weight_test[weight_test == 1060] <- lm(weight_test, n_diff_alter)

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

# Aufgabe 2 ---------------------------------------------------------------------------------------------------

#a)
# dummykodierung für Ernährungsplan
lin_mod <- lm(weight ~ Time, data = weight_test)










