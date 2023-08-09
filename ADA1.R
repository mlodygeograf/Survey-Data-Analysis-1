library(vcdExtra)
library(dplyr)
library(vcd)
library(epiDisplay)
library(stats)
library(likert)
library(binom)

#Część 1
dane <- read.csv2("C:/Users/lab/Downloads/personel.csv")

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

#Część 2
zwr1 <- sample(nrow(dane), 0.1*nrow(dane), replace = TRUE)
zwr0 <- sample(nrow(dane), 0.1*nrow(dane), replace = FALSE)

dane$A1_l <- as.factor(dane$A1)
dane$A2_l <- as.factor(dane$A2)

X <- likert(dane["A1_l"])
summary(X)
likert.density.plot(X)
likert.bar.plot(X)
Y <- likert(dane["A2_l"])
summary(Y)
likert.density.plot(Y)
likert.bar.plot(Y)

Clopper <- function(alpha, x, n) {
  l <- qbeta(alpha/2, x, n-x+1)
  u <- qbeta(1-alpha/2, x+1, n-x)
  if (x==0) {
    L <- 0
  } else {
    L <- l
  }
  if (x==n) {
    P <- 1
  } else {
    P <- u
  }
  paste("Dolna granica: ", L, "Górna granica: ", P)
}

y <- 0
i <- 1
while (i<=200) {
  if (dane$W1[i] == 1 | dane$W1[i] == 2) {
    y <- y+1
    i <- i+1
  } else {
    i <- i+1
  }
}
y

x <- y
n <- 200
alpha <- 0.05
binom.confint(x, n, conf.level=1-alpha, methods="exact")
Clopper(alpha,x,n)


