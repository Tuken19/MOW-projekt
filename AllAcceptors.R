AllAcceptors <- function(nazwa, winLen){
  # Funkcja, której celem jest wykrycie wszystkich akceptorow prawdziwych i fa³szywych z listy wszystkich ³añcuchów.
  # Wejscie:
  #   nazwa - nazwa pliku z danymi
  #   winLen - to parametr okreœlaj¹cy d³ugoœæ akceptora
  # Wyjœcie:
  #   acceptors - data frame, której wiersze zawieraj¹ sekwencje znalezionych akceptorów, a w ostatniej kolumnie
  #            jest 0 - jeœli akceptor jest fa³szywy i 1 jesli jest prawdziwy
  
  dane <- LoadData(nazwa)
  
  acceptors <- data.frame(matrix(nrow = 0, ncol = (winLen + 1)))
  
  zakres <- floor(length(dane)/3)
  
  for (i in 1:zakres) {
    temp <- GetAcceptors(dane[[3*i-2]], dane[[3*i]], winLen)
    acceptors <- rbind.data.frame(acceptors, temp)
    print(c("Postêp = ", 100*i/zakres, "%"))
  }
  
  return(acceptors)
}
