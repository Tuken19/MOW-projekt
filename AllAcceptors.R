AllAcceptors <- function(nazwa, winLen){
  # Funkcja, kt�rej celem jest wykrycie wszystkich akceptorow prawdziwych i fa�szywych z listy wszystkich �a�cuch�w.
  # Wejscie:
  #   nazwa - nazwa pliku z danymi
  #   winLen - to parametr okre�laj�cy d�ugo�� akceptora
  # Wyj�cie:
  #   acceptors - data frame, kt�rej wiersze zawieraj� sekwencje znalezionych akceptor�w, a w ostatniej kolumnie
  #            jest 0 - je�li akceptor jest fa�szywy i 1 jesli jest prawdziwy
  
  dane <- LoadData(nazwa)
  
  acceptors <- data.frame(matrix(nrow = 0, ncol = (winLen + 1)))
  
  zakres <- floor(length(dane)/3)
  
  for (i in 1:zakres) {
    temp <- GetAcceptors(dane[[3*i-2]], dane[[3*i]], winLen)
    acceptors <- rbind.data.frame(acceptors, temp)
    print(c("Post�p = ", 100*i/zakres, "%"))
  }
  
  return(acceptors)
}
