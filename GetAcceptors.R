GetAcceptors <- function(introns, dane, winLen){
  # Funkcja, kt�rej celem jest wykrycie wszystkich akceptor�w prawdziwych i fa�szywych z jednego �a�cucha danych.
  # Wejscie:
  #   introns - wektor wsp�rz�dnych intron�w
  #   dane - lista sekwencji liter DNA, w kt�rej chcemy zanle�� akceptory
  #   winLen - to parametr okre�laj�cy d�ugo�� akceptora
  # Wyj�cie:
  #   acceptors - data frame, kt�rej wiersze zawieraj� sekwencje znalezionych akceptor�w, a w ostatniej kolumnie
  #            jest 0 - je�li akceptor jest fa�szywy i 1 jesli jest prawdziwy
  
  library(zoo)      # Biblioteka, z kt�rej potrzebujemy funkcj� rollapply
  
  acceptors <- data.frame(matrix(nrow = 0, ncol = (winLen + 1)))
  name <- names(acceptors)
  coord <- which(rollapply(dane, 2, identical, c("A", "G")))  # Sprawdzam gdzie wyst�puj� sekwencje GT
  coord <- coord + 1                                     # coord zawieraa wsp�rz�dn� litery A, a potrzebujemy wsp�rz�dnej litery G
  introns_b <- introns[seq(2, length(introns), 2)]       # Wybieram co drug� warto�� z wektora introns zaczynaj�c od drugiej
  
  for(i in coord){
    if(i < winLen){
      # pomi� bo akceptor ko�cz�cy si� w tym miejscu by�by za kr�tki
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