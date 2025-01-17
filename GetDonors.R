GetDonors <- function(introns, dane, winLen){
  # Funkcja, kt�rej celem jest wykrycie wszystkich donor�w prawdziwych i fa�szywych z jednego �a�cucha danych.
  # Wejscie:
  #   introns - wektor wsp�rz�dnych intron�w
  #   dane - lista sekwencji liter DNA, w kt�rej chcemy zanle�� donory
  #   winLen - to parametr okre�laj�cy d�ugo�� donora
  # Wyj�cie:
  #   donors - data frame, kt�rej wiersze zawieraj� sekwencje znalezionych donor�w, a w ostatniej kolumnie
  #            jest 0 - je�li donor jest fa�szywy i 1 jesli jest prawdziwy
  
  library(zoo)      # Biblioteka, z kt�rej potrzebujemy funkcj� rollapply
  
  donors <- data.frame(matrix(nrow = 0, ncol = (winLen + 1)))
  name <- names(donors)
  coord <- which(rollapply(dane[c(-(length(dane)-winLen):-length(dane))], 2, identical, c("G", "T")))  # Sprawdzam gdzie wyst�puj� sekwencje GT
  introns_b <- introns[seq(1, length(introns), 2)]       # Wybieram co drug� warto�� z wektora introns zaczynaj�c od pierwszej
  
  for(i in coord){
    if(i %in% introns_b){
      d <- dane[i:(i+winLen-1)]
      d <- append(d, "1")
      temp <- as.data.frame.list(d)
      names(temp) <- name
      donors <- rbind.data.frame(donors, temp)
    }
    else{
      d <- dane[i:(i+winLen-1)]
      d <- append(d, "0")
      temp <- as.data.frame.list(d)
      names(temp) <- name
      donors <- rbind.data.frame(donors, temp)
    }
  }
  return (donors)
}