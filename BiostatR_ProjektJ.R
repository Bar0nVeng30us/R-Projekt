adjust_weight <- function(v, abweichung)
{
  adjusted_weight <- v
  
  # Linke und rechte Nachbarn
  day_before  <- c(0, v[-length(v)])
  day_after <- c(v[-1], 100000)
  
  #Berechne der Mittelwert der umliegenden Tage
  neighbour_mean <- rowMeans(cbind(day_before, day_after), na.rm = TRUE)
  
  # Ist das Gewicht kleiner als am Vortag, oder größer als am darauffolgenden Tag, dann:
  extreme_vals <- (v < day_before | v > day_after)
  
  adjusted_weight[extreme_vals] <- neighbour_mean[extreme_vals]
  
  return(adjusted_weight)
}

finddefectvalues <- function(data, expected_id_count = 50, expected_observ_end = 22)
{
    #-------------------------------------#Falsche Messzeit abfangen------------------------------------------------------------------------------
  
  #Überprüfe, ob die EIngetragene Zeit außerhalb des Messraums liegt, oder ungerade ist (es wurde ja nur an geraden Tagen gemessen)
  correct_time_intervall <- (0<=data$Time) & (22>=data$Time)
  correct_time_phase <- data$Time %%2 == 0
  
  #Falls die Zeit außerhalb des Zeitraums ist in dem wir gemessen haben, löschen wir die betroffenen Daten.
  data <- data[correct_time_intervall & correct_time_phase,]

  #------------------------------------------------------------------------------------------------------------------------------------------------------
  
 #-------------------------------------Falschen Ernährungsplan abfangen---------------------S-------------------------------------------------------------
  i <- 1
  
  cat(nrow(data)) 
  #str(data)
  #cat(table(data$Chick))
  cat("Chick" %in% names(data))
  
  split_data <- split(data, data$Chick)
    
  bereinigte_daten <- lapply(split_data, 
                             function(chicken){haeufigste_diet <- names(sort(table(chicken$Diet), decreasing = TRUE))[1]
                                               chicken$Diet <- haeufigste_diet
                                               chicken <- chicken[order(chicken$Time),]
                                               chicken$weight <- adjust_weight(chicken$weight)
                                               return(chicken)})
  bereinigte_daten <- do.call(rbind, bereinigte_daten)
  return(bereinigte_daten)

}



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
# Wir führen hier eine lineare Regression durch, mit dem Gewicht als Zielvariable Y und der Zeit als abhängigen Variable
# Für die Regression verwenden wir nur die Werte, die in unserem Fehlertest keine Auffälligkeiten gezeigt haben
weight_test <- finddefectvalues(weight_test)
#lm(weight_test[!defectvalues])#Hier ist die Fehlermeldung

# Die Fehlerhaften Werte setzen wir dann neu auf den Wert der Regressionsgeraden zum passenden Zeitpunkt
#weight_test[weight_test == 1060] <- lm(weight_test, n_diff_alter)

#weight_test[weight_test == "Plan_5"] <- "Plan_4"

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










