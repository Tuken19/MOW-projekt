AllDonors <- function(nazwa, winLen){
  # Funkcja, kt�rej celem jest wykrycie wszystkich donor�w prawdziwych i fa�szywych z listy wszystkich �a�cuch�w.
  # Wejscie:
  #   nazwa - nazwa pliku z danymi
  #   winLen - to parametr okre�laj�cy d�ugo�� donora
  # Wyj�cie:
  #   donors - data frame, kt�rej wiersze zawieraj� sekwencje znalezionych donor�w, a w ostatniej kolumnie
  #            jest 0 - je�li donor jest fa�szywy i 1 jesli jest prawdziwy
  
  dane <- LoadData(nazwa)
  
  donors <- data.frame(matrix(nrow = 0, ncol = (winLen + 1)))
  
  zakres <- floor(length(dane)/3)
  
  for (i in 1:zakres) {
    temp <- GetDonors(dane[[3*i-2]], dane[[3*i]], winLen)
    donors <- rbind.data.frame(donors, temp)
    print(c("Post�p = ", 100*i/zakres, "%"))
  }
  
  return(donors)
}
