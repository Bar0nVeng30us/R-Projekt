adjust_weight <- function(v, tolerated_deviation = 10)
{
  #Funktion sorgt dafür, dass die Gewicht mit Toleranzfaktor "tolerated_deviation" monoton steigend sind.
  
  #ACHTUNG: Diese Funktion kann nur Außreißer korrigieren, die eine Umgebung von nicht-Außreißern haben
  #Bei zwei oder mehr extremen Werte hintereinander wird davon ausgegangen, dass dies kein Fehler mehr ist, den man korrigieren sollte,
  #sondern, dass die Daten manuell auf Tauglichkeit untersucht werden sollten.
  
  adjusted_weight <- v
  
  #Erzeuge Vektoren, die zum Vergleich mit Vortag bzw. Nachtag dienen
  day_before  <- c(0, v[-length(v)])
  day_after <- c(v[-1], 100000)
  
  #Berechne der Mittelwert der beiden umliegenden Tage
  neighbour_mean <- rowMeans(cbind(day_before, day_after), na.rm = TRUE)
  
  # Ist das Gewicht kleiner als am Vortag, oder größer als am darauffolgenden Tag, dann wird das Gewicht angepasst.
  extreme_vals <- ((v + tolerated_deviation) < day_before | v > (day_after + tolerated_deviation))
  adjusted_weight[extreme_vals] <- neighbour_mean[extreme_vals]
  print(adjusted_weight)
  
  #Korrigierten Werte werden zurückgegeben
  return(adjusted_weight)
}

finddefectvalues <- function(data, expected_observ_end = 22, tolerated_deviation = 0)
{
  #-------------------------------------#Falsche Messzeit abfangen------------------------------------------------------------------------------
  
  #Überprüfe, ob die EIngetragene Zeit außerhalb des Messraums liegt, oder ungerade ist (es wurde ja nur an geraden Tagen gemessen)
  correct_time_intervall <- (0<=data$Time) & (expected_observ_end>=data$Time)
  correct_time_phase <- data$Time %%2 == 0
  
  #Falls die Zeit außerhalb des Zeitraums ist in dem wir gemessen haben, löschen wir die betroffenen Daten.
  data <- data[correct_time_intervall & correct_time_phase,]

 #-------------------------------------Falschen Ernährungsplan abfangen---------------------S-------------------------------------------------------------
  cat(nrow(data)) 
  #str(data)
  #cat(table(data$Chick))
  cat("Chick" %in% names(data))
  
  split_data <- split(data, data$Chick)
    
  bereinigte_daten <- lapply(split_data, 
                             function(chicken){haeufigste_diet <- names(sort(table(chicken$Diet), decreasing = TRUE))[1]
                                               chicken$Diet <- haeufigste_diet
                                               chicken <- chicken[order(chicken$Time),]
                                               chicken$weight <- adjust_weight(chicken$weight, tolerated_deviation)
                                               return(chicken)})
  bereinigte_daten <- do.call(rbind, bereinigte_daten)
  return(bereinigte_daten[])

}



library(ggplot2)
# b
data_dir <- "data/weight-test-2025.txt"

weight_test <- read.table(file=data_dir,
                          header=TRUE,
                          sep=" ",
                          as.is=TRUE)

# e
#Die Daten werden bereinigt. ACHTUNG: Dabei werden unplausible Einträge nicht gelöscht, sondern verändert. 
#Für eine kurze Beschreibung der Veränderung siehe Funktionsbeschreibung und für ausführliche Dokumentation siehe Anhang.
cleanded_data <- finddefectvalues(weight_test, tolerated_deviation = 10)


split_cleaned_data <- split(cleanded_data, cleanded_data$Chick)









