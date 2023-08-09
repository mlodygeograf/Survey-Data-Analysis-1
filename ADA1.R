
library(dplyr)
library(vcd)
library(epiDisplay)

dane <- read.csv2("C:/Users/DAMIANEK/Downloads/personel.csv")

tabA1 <- dane %>%
  group_by(A1) %>%
  count()

tabW1 <- dane %>%
  group_by(W1) %>%
  count()


tab1(dane['A1'])
tab1(dane['W1'])

tabWP <- structable(W1 ~ P, data = dane)
tabWS <- structable(W1 ~ S, data = dane)
tabAD <- structable(A1 ~ D, data = dane)

wykW1_kol <- pie(table(dane$W1))
wykW2_kol <- pie(table(dane$W2))
wykW1_slup <- barplot(table(dane$W1))
wykW2_slup <- barplot(table(dane$W2))

mozaika1 <- mosaic(~ D + A1, data = dane, highlighting = "A1", highlighting_fill = c("blue", "red"))
mozaika2 <- mosaic(~ D + W1, data = dane, highlighting = "W1", highlighting_fill = c("blue", "red"))
mozaika3 <- mosaic(~ S + P, data = dane, highlighting = "P", highlighting_fill = c("blue", "red"))
mozaika4 <- mosaic(~ P + W1, data = dane, highlighting = "W1", highlighting_fill = c("blue", "red"))
