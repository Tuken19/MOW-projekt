AllDonors <- function(nazwa, winLen){
  # Funkcja, której celem jest wykrycie wszystkich donorów prawdziwych i fa³szywych z listy wszystkich ³añcuchów.
  # Wejscie:
  #   nazwa - nazwa pliku z danymi
  #   winLen - to parametr okreœlaj¹cy d³ugoœæ donora
  # Wyjœcie:
  #   donors - data frame, której wiersze zawieraj¹ sekwencje znalezionych donorów, a w ostatniej kolumnie
  #            jest 0 - jeœli donor jest prawdziwy i 1 jesli jest fa³szywy
  
  dane <- LoadData(nazwa)
  
  donors1 <- data.frame(matrix(nrow = 0, ncol = (winLen + 1)))
  
  zakres <- floor(length(dane)/3)
  
  for (i in 1:zakres) {
    temp <- GetDonors(dane[[3*i-2]], dane[[3*i]], winLen)
    donors <- rbind.data.frame(donors1, temp)
    print(c("Postêp = ", 100*i/zakres, "%"))
  }
  
  return(donors)
}
