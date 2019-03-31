GetAcceptors <- function(introns, dane, winLen){
  # Funkcja, której celem jest wykrycie wszystkich akceptorów prawdziwych i fa³szywych z jednego ³añcucha danych.
  # Wejscie:
  #   introns - wektor wspó³rzêdnych intronów
  #   dane - lista sekwencji liter DNA, w której chcemy zanleŸæ akceptory
  #   winLen - to parametr okreœlaj¹cy d³ugoœæ akceptora
  # Wyjœcie:
  #   acceptors - data frame, której wiersze zawieraj¹ sekwencje znalezionych akceptorów, a w ostatniej kolumnie
  #            jest 0 - jeœli akceptor jest fa³szywy i 1 jesli jest prawdziwy
  
  library(zoo)      # Biblioteka, z której potrzebujemy funkcjê rollapply
  
  acceptors <- data.frame(matrix(nrow = 0, ncol = (winLen + 1)))
  name <- names(acceptors)
  coord <- which(rollapply(dane, 2, identical, c("A", "G")))  # Sprawdzam gdzie wystêpuj¹ sekwencje GT
  coord <- coord + 1                                     # coord zawieraa wspó³rzêdn¹ litery A, a potrzebujemy wspó³rzêdnej litery G
  introns_b <- introns[seq(2, length(introns), 2)]       # Wybieram co drug¹ wartoœæ z wektora introns zaczynaj¹c od drugiej
  
  for(i in coord){
    if(i < winLen){
      # pomiñ bo akceptor koñcz¹cy siê w tym miejscu by³by za krótki
    }
    else if(i %in% introns_b){
      d <- dane[(i+1-winLen):i]
      d <- append(d, "1")
      temp <- as.data.frame.list(d)
      names(temp) <- name
      acceptors <- rbind.data.frame(acceptors, temp)
    }
    else{
      d <- dane[(i+1-winLen):i]
      d <- append(d, "0")
      temp <- as.data.frame.list(d)
      names(temp) <- name
      acceptors <- rbind.data.frame(acceptors, temp)
    }
  }
  return (acceptors)
}