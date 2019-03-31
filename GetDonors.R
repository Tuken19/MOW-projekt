GetDonors <- function(introns, dane, winLen){
  # Funkcja, której celem jest wykrycie wszystkich donorów prawdziwych i fa³szywych z jednego ³añcucha danych.
  # Wejscie:
  #   introns - wektor wspó³rzêdnych intronów
  #   dane - lista sekwencji liter DNA, w której chcemy zanleŸæ donory
  #   winLen - to parametr okreœlaj¹cy d³ugoœæ donora
  # Wyjœcie:
  #   donors - data frame, której wiersze zawieraj¹ sekwencje znalezionych donorów, a w ostatniej kolumnie
  #            jest 0 - jeœli donor jest fa³szywy i 1 jesli jest prawdziwy
  
  library(zoo)      # Biblioteka, z której potrzebujemy funkcjê rollapply
  
  donors <- data.frame(matrix(nrow = 0, ncol = (winLen + 1)))
  name <- names(donors)
  coord <- which(rollapply(dane[c(-(length(dane)-winLen):-length(dane))], 2, identical, c("G", "T")))  # Sprawdzam gdzie wystêpuj¹ sekwencje GT
  introns_b <- introns[seq(1, length(introns), 2)]       # Wybieram co drug¹ wartoœæ z wektora introns zaczynaj¹c od pierwszej
  
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