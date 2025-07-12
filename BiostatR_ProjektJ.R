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



finddefectvalues <- function(data, ExpectedIDCount, ExpectedObservEnd)
{
   #-------------------------------------Falsche Messzeit abfangen------------------------------------------------------------------------------
  
  #Überprüfe, ob die EIngetragene Zeit außerhalb des Messraums liegt, oder ungerade ist (es wurde ja nur an geraden Tagen gemessen)
  correct_time_intervall <- (0<=weight_test$Time) & (22>=weight_test$Time)
  correct_time_phase <- weight_test$Time %%2 == 0
  
  #Falls die Zeit außerhalb des Zeitraums ist in dem wir gemessen haben, löschen wir die betroffenen Daten.
  weight_test <- weight_test[pmin(correct_time_intervall, correct_time_phase)]

  ------------------------------------------------------------------------------------------------------------------------------------------------------
  
  i <- 1
  split_data <- split(weight_test, weight_test$Chick)
  
  for (chickdata in split_data) #Verboten, oder nicht verboten- das ist hier nicht die Frage
    { 
      
#-------------------------------------Falschen Ernährungsplan abfangen----------------------------------------------------------------------------------
      #Spalte die Daten des Huhns nach den verschiedenen Ernährungsplänen auf
      diets <- split_data[[i]]$Diet
      #Erhalte den Ernährungsplan, der am häufigsten verwendet wurde
      most_used_name <- names(sort(table(diets),decreasing=TRUE)[1])
      #Setze auf den am häufigsten verwendeten Plan
      split_data[[i]]$Diet <- most_used_name
      i <- i+1

#-------------------------------------Falsches Gewicht abfangen---------------------------------------------------------------------------------------------
     adjusted_weight <- adjust_weight(chickdata$weight)
    }
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










