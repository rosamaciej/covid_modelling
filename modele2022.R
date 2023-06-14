require(coda)
require(scales)
require(sarima)
require(gapminder)
require(patchwork)
require(tidyverse)
require(mvtnorm)
require(manipulate)
require(readxl)
require(zoo)
require(corrplot)
if(!require("mvtnorm")) {install.packages("mvtnorm"); library(mvtnorm)}
if(!require("coda")) {install.packages("coda"); library(coda)} #Pakiet służący do wyliczenia przedziałów HPD (Plummer et al., 2006)
require(nlme)
require(ggplot2)
#Wybór scieżki oraz danych
setwd("C:/Users/mjros/Desktop/4_semestr/praca_magisterska")

dane_zmienne <- read_excel("zmienne2.xlsx")

#model

model2022 <- lm(log(dane_zmienne$deahts_per_person_second_model) ~ log(dane_zmienne$vaccination_per_100_people) + log(dane_zmienne$GDP_for_HC) + log(dane_zmienne$urbanization) + log(dane_zmienne$mean_temperature) + log(dane_zmienne$population_density) + log(dane_zmienne$number_of_physicians) + log(dane_zmienne$Average_household_size)  )
summary(model2022)


dane_zmienne2 <- dane_zmienne[-1]
stand_dane_zmienne <- scale(dane_zmienne2[-1])
rm(dane_zmienne2)
stand_dane_zmienne <- as.data.frame(stand_dane_zmienne)
model2022_stand <- lm(stand_dane_zmienne$deahts_per_person_second_model ~ stand_dane_zmienne$vaccination_per_100_people  + stand_dane_zmienne$mean_temperature  + stand_dane_zmienne$GDP_for_HC + stand_dane_zmienne$Average_household_size )
summary(model2022_stand)

#2. Regresja wieloraka (OLS) i statystyki dostateczne

y <- as.matrix((stand_dane_zmienne)[1:26, c('deahts_per_person_second_model')])
N.data <- length(y)
X <- cbind(as.matrix(rep(1, N.data)), 
           as.matrix(stand_dane_zmienne[1:26, c('vaccination_per_100_people','mean_temperature','GDP_for_HC','Average_household_size')]))
Beta.ols.data <- model2022_stand$coefficients
v.data <- model2022_stand$df.residual
XTX.data <- t(X) %*% X
s2.data <- sum((model2022_stand$residuals) ^ 2) / v.data

#3. Parametry a priori

Beta.prior <- c(0.1, -1, -0.5, -0.5, 0.5)
sm2.prior <- 4 # s2.prior = 0.25, s.prior = 0.5
U.prior <- 0.1 * 4 * diag(5) # zerowe kowariancje i wariancja (co do wartości oczekiwanej) 0.1 * 4 * (1/4) = 0.1 (tzn. odchylenie standardowe (0.1)^0.5 = 0.31)
U.prior[1, 1] <-200
v.prior <- 40
k <- ncol(X)
N <- length(y)

#4. Parametry a posteriori

Beta.posterior <- solve(solve(U.prior) + XTX.data) %*% (solve(U.prior) %*% Beta.prior + XTX.data %*% Beta.ols.data)
U.posterior <- solve(solve(U.prior) + XTX.data)
v.posterior <- v.prior + N.data
vs2.posterior <- v.prior / sm2.prior + v.data * s2.data + t(Beta.ols.data-Beta.prior) %*% solve(U.prior + solve(XTX.data)) %*% (Beta.ols.data - Beta.prior)
sm2.posterior <- 1 / (vs2.posterior / v.posterior)

#5. Dane do wykresów - gęstość a priori i a posteriori
beta.space <- seq(from = -2, to = 2, by = 0.01)
n_eval_points <- length(beta.space)
n_parameters <- length(Beta.posterior)
prior.marg.dens.beta <- matrix(NA, nrow = n_parameters, ncol = n_eval_points)
posterior.marg.dens.beta <- matrix(NA, nrow = n_parameters, ncol = n_eval_points)
for(ii in 1:length(Beta.posterior)) {
  prior.marg.dens.beta[ii, ] <- apply(as.matrix(beta.space), 1, dmvt,
                                      delta = Beta.prior[ii], sigma = as.matrix(U.prior[ii, ii] / sm2.prior), df = v.prior, log = FALSE)
  posterior.marg.dens.beta[ii, ] <- apply(as.matrix(beta.space), 1, dmvt,
                                          delta = Beta.posterior[ii], sigma = as.matrix(U.posterior[ii, ii] / sm2.posterior), df = v.posterior, log = FALSE)
}

grey_area <- rgb(160, 160, 160, 80, names = NULL, maxColorValue = 255)
grey_line <- rgb(80, 80, 80, 160, names = NULL, maxColorValue = 255)
green_area <- rgb(24, 121, 104, 80, names = NULL, maxColorValue = 255)
green_line <- rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)

#Polecenie, które tworzy wykres składający się z 4 paneli (2x2)
par(mfrow = c(2, 2))
for(ii in 2:length(Beta.posterior)) {
  plot(beta.space, prior.marg.dens.beta[ii, ], las = 1, lwd = 2, bty = "n", col = grey_area,
       ylim = c(0, max(c(max(prior.marg.dens.beta[ii, ]),max(posterior.marg.dens.beta[ii, ]))) + 1), type = "l", ylab = "gęstość", main = colnames(X)[ii])
  polygon(c(beta.space, rev(beta.space)), c(prior.marg.dens.beta[ii, ], 
                                            rep(0, length(beta.space))), col = grey_area, border = NA)
  abline(v = Beta.prior[ii], col = grey_line, lwd = 3)
  text(Beta.prior[ii], max(prior.marg.dens.beta[ii, ]) + 0.4, paste("E(beta) a priori = ", Beta.prior[ii]), col = grey_line)
  abline(v = Beta.ols.data[ii], col = rgb(0, 0, 0, 1), lwd = 3)
  text(Beta.ols.data[ii], max(posterior.marg.dens.beta[ii, ]) + 0.2, paste("parametr OLS = ", round(Beta.ols.data[ii], 4)), col = rgb(0, 0, 0, 1))
  lines(beta.space, posterior.marg.dens.beta[ii, ], lwd = 2, col = green_line)
  polygon(c(beta.space, rev(beta.space)), c(posterior.marg.dens.beta[ii, ], 
                                            rep(0, length(beta.space))), col = green_area, border = NA)
  abline(v = Beta.posterior[ii], col = green_line, lwd = 3)
  text(Beta.posterior[ii], max(posterior.marg.dens.beta[ii, ]) + 0.6, paste("E(beta) a posteriori = ", round(Beta.posterior[ii], digits = 4)), col = green_line)
}

# 6. Szkicujemy HPDI wokół wybranego współczynnika kierunkowego równania regresji
###  Wybierz:
###  2 <- elastyczność cenowa popytu własna
###  3 <- elastyczność cenowa popytu krzyżowa względem nowych samochodów
###  4 <- elastyczność cenowa popytu krzyżowa względem używanych samochodów
###  5 <- elastyczność dochodowa popytu
ii <- 2

grey_area <- rgb(160, 160, 160, 80, names = NULL, maxColorValue = 255)
grey_line <- rgb(80, 80, 80, 160, names = NULL, maxColorValue = 255)
green_area <- rgb(24, 121, 104, 80, names = NULL, maxColorValue = 255)
green_line <- rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)
red_area <- rgb(255, 100, 123, 80, names = NULL, maxColorValue = 255)
red_line <- rgb(200, 0, 30, 160, names = NULL, maxColorValue = 255)

par(mfrow = c(1, 1))
manipulate( 
  {#Tworzymy zmienną binarną wskazującą, gdzie będzie HPDI - tzn. o najwyżej gęstości a posteriori ponad zadany poziom
    credible_set_indicator <- as.vector(as.integer(posterior.marg.dens.beta[ii, ] > line_level))
    credible_set_begin <- match(1, credible_set_indicator)
    credible_set_end <- length(credible_set_indicator) - match(1, rev(credible_set_indicator))
    #Lewy i prawy brzeg HPDI
    x1 <- beta.space[credible_set_begin]
    x2 <- beta.space[credible_set_end]
    #Na potrzeby wykresu tworzymy wektor, który przyjmuje wartość gęstości a posteriori w HPDI i zero poza nim
    posterior.cs <- posterior.marg.dens.beta[ii, ] * credible_set_indicator
    #Poziom ufności
    HPDI_probab <- sum(posterior.cs) * 0.01
    #Wykres gęstości a posteriori
    plot(beta.space, posterior.marg.dens.beta[ii, ], las = 1, lwd = 2, bty = "n", col = green_line,
         ylim = c(0, max(posterior.marg.dens.beta[ii, ] + 1)), type = "l", ylab = "gęstość", main = colnames(X)[ii])
    polygon(c(beta.space, rev(beta.space)), 
            c(posterior.marg.dens.beta[ii, ], rep(0, length(beta.space))), 
            col = green_area, border = NA)
    text(Beta.posterior[ii], max(posterior.marg.dens.beta[ii, ]) + 0.6, paste("E(beta) a posteriori = ", round(Beta.posterior[ii], digits = 4)), col = green_line)
    abline(v = Beta.posterior[ii], col = green_line, lwd = 3)
    #Linia pozioma odcinająca "najwyższe" gęstości a posteriori (HPD)
    abline(h = line_level, col = red_line, lwd = 3)
    #Pole oznaczające gęstość a posteriori w przedziale ufności HPDI
    polygon(c(beta.space, rev(beta.space)), 
            c(posterior.cs, rep(0, length(beta.space))), 
            col = red_area, border = NA)
    
    #Wyświetl poziom ufności i granice przedziału
    text(0, max(posterior.marg.dens.beta[ii, ]) + 0.2, paste(round(HPDI_probab * 100, digits = 1), "% przedział HPDI: (", round(x1, digits = 2), " , ", round(x2, digits = 2), ")"), col = red_line)
  },
  line_level = slider(0, max(posterior.marg.dens.beta[ii, ]) + 0.002, step = 0.001, initial = max(posterior.marg.dens.beta[ii, ]) + 0.001)
)




########
#model2022


model2022 <- lm(log(dane_zmienne$deahts_per_person_second_model) ~ log(dane_zmienne$vaccination_per_100_people) + log(dane_zmienne$population_density) + log(dane_zmienne$number_of_physicians) + log(dane_zmienne$Average_household_size) )
summary(model2022)



#Ustalamy parametry rozkładu a posteriori ze skrajnie nieinformacyjnym rozkładem a priori N-G
ols.beta <- model2022$coefficients
ols.sigma <- vcov(model2022)
ols.sum.sq.residuals <- sum(model2022$residuals ^ 2)

v.posterior <- nrow(dane_zmienne) - length(ols.beta) 
beta.posterior <- ols.beta
U.posterior <- ols.sigma / (ols.sum.sq.residuals / v.posterior)
s2.posterior <- ols.sum.sq.residuals / v.posterior

#Próbkowanie z funkcji q
library(mvtnorm)
S <- 10 ^ 5
set.seed(100)
losowanie.beta <- rmvt(S, sigma = s2.posterior * U.posterior,
                       df = v.posterior, delta = beta.posterior,
                       type = "shifted")
colnames(losowanie.beta) <- rownames(summary(model2022)$coefficients)
head(losowanie.beta) #Ten sam współczynnik występuje z różnymi znakami

beta0 <- losowanie.beta[, 1] # stała
beta1 <- losowanie.beta[, 2] # vaccination_per_100_people
beta2 <- losowanie.beta[, 3] # ln(population density)
beta3 <- losowanie.beta[, 4] # number_of_physians
beta4 <- losowanie.beta[, 5] # Average_household_size


t(t(apply(losowanie.beta,2,mean)))

#Importance sampling
important.beta <- losowanie.beta[beta1 < 0 & beta2 > 0 & beta3 < 0 & beta4 > 0 , ]

#Prawdopodobieństwo a posteriori spełnienia restrykcji

#################################################################

#Metoda 1
nrow(important.beta)/nrow(losowanie.beta)

#Metoda 2
R <- diag(c(-1,1,-1,1))
dim(R)
p.restrykcji <- as.numeric(pmvt(lower = rep(0,4),
                                sigma = s2.posterior * (R %*% U.posterior[c(2:5), c(2:5)] %*% t(R)),
                                df = v.posterior, 
                                delta = as.vector(R %*% beta.posterior[c(2:5)]), type = "shifted"))
p.restrykcji

##################################################################

par(mfrow=c(2,1))

#Ilustracja gęstości a posteriori dla parametru liczby lekarzy na 100000 mieszkańców
important.beta3 <- important.beta[, 4]
restricted.h <- hist(important.beta3, breaks = (- 500:500) / 100, 
                     col = "gray", 
                     xlab = "Liczba lekarzy na 100000 mieszkańców: parametr", 
                     main = "Rozkład a posteriori przy uciętym rozkładzie a priori")
unrestricted.h <- hist(beta3, breaks = (- 500:500) / 100, col = "gray", 
                       xlab = "Liczba lekarzy na 100000 mieszkańców: parametr", 
                       main = "Rozkład a posteriori przy skrajnie nieinformacyjnym rozkładzie a priori")
par(mfrow=c(1,1))
plot(x = (-499:500) / 100, y = restricted.h$density, col = green_line, type = "l", lwd = 3, 
     main = "Rozkłady a posteriori parametru ",
     xlab = "wartość parametru number_of_physians", ylab = "gęstość")
lines(x = (- 499:500) / 100, y = unrestricted.h$density, col = "red", lwd = 3)
legend(x = "left", fill = c("red", green_line), legend = c("nieinformacyjny rozkład N-G", "restrykcje narzucone a priori"))
abline(v = mean(important.beta3), col = green_line, lwd = 3, lty = 3)
abline(v = mean(beta3), col = "red", lwd = 1, lty = 3)

#Ilustracja gęstości a posteriori dla parametru szczepień
important.beta1 <- important.beta[, 2]
restricted.h <- hist(important.beta1, breaks = (- 600:600) / 100, col = "green", 
                     xlab = "Liczba szczepień na sto mieszkańców: parametr", main = "Ucięty rozkład a priori")
unrestricted.h <- hist(beta1, breaks = (-600:600) / 100, col = "gray", 
                       xlab = "Liczba szczepień na sto mieszkańców: parametr", main = "Skrajnie nieinformacyjny rozkład a priori")
plot(x = (-599:600) / 100, y = restricted.h$density, type = "l",col = green_line, lwd = 2, 
     main = "Rozkłady a posteriori parametru vaccination_per_100_people", xlab = "wartość parametru vaccination_per_100_people", ylab = "gęstość")
lines(x = (- 599:600) / 100, y = unrestricted.h$density, col = "red", lwd = 2)
legend(x = "right", fill = c("red", green_line), legend = c("nieinformacyjny rozkład N-G", "restrykcje narzucone a priori"))
abline(v = mean(important.beta1), col = green_line, lwd = 3, lty = 2)
abline(v = mean(beta1), col = "red", lwd = 1, lty = 2)

# Wyliczanie przedziałów HPD

beta.restricted <- apply(important.beta, 2, mean)
hpd.restricted <- HPDinterval(mcmc(important.beta)) #funkcja licząca bayesowskie przedziały HPD (95%)
restricted.results <- cbind(hpd.restricted[,1], beta.restricted, hpd.restricted[,2])
rownames(restricted.results) <- names(ols.beta)
colnames(restricted.results) <- c("Dolna granica HPD","Oszacowanie z ograniczeniami","Górna granica HPD")
round(restricted.results, digits = 3)

beta.unrestricted <- apply(losowanie.beta, 2, mean)
hpd.unrestricted <- HPDinterval(mcmc(losowanie.beta)) 
unrestricted.results <- cbind(hpd.unrestricted[,1],beta.unrestricted, hpd.unrestricted[,2])
rownames(unrestricted.results) <- names(ols.beta)
colnames(unrestricted.results) <- c("Dolna granica HPD","Oszacowanie bez ograniczeń", "Górna granica HPD");
round(unrestricted.results, digits = 3)

frequentist.results <- cbind(confint(model2022)[,1],model2022$coefficients,confint(model2022)[,2])
colnames(frequentist.results) <- c("Dolna granica 95% p.u.","Oszacowanie bez ograniczeń", "Górna granica 95% p.u.");
round(frequentist.results, digits=3)

#Który model jest lepszy?

colnames()
dane_short <- dane_zmienne[, c( "deahts_per_person_second_model","vaccination_per_100_people",
                                "population_density","number_of_physicians", "Average_household_size")]

# Długości próby testowej
O <- c(1, 5, 10, 15, 20)
# Ile razy maksymalnie dzielimy naszą próbę (losowo)
K_max <- 50

#Ile razy losujemy z rozkładu a posteriori 
S <- 10^5
final.results <- matrix(NA, length(O), 5)
rownames(final.results) <- O
colnames(final.results) <- c("OLS RMSE", "OLS Calculation Errors", "Bayesian RMSE", "Bayesian Calculation Errors", "Probability")
set.seed(1)
iteracja <- 0



#Następny fragment kodu (podwójna pętla) jest dość długi, prosimy nie panikować :)
for(i in 1:length(O)){
  o <- O[i]
  
  #Na ile sposobów dzielimy próbę:
  K <- min(choose(nrow(dane_short), o), K_max)
  forecast.errors <- matrix(NA, K, 5)
  for(k in 1:K){
    
    #Liczymy iterację
    iteracja <- iteracja + 1
    print(paste("Iteracja ", iteracja, " spośród ", (length(O) - 1) * K_max + nrow(dane_short), ".", sep = ""))
    outofsample <- dane_short[sample(1:nrow(dane_short), o), ]
    insample <- dane_short[-as.numeric(rownames(outofsample)), ]
    y_empirical <- outofsample$deahts_per_person_second_model
    
    #Ucinamy zmienna objasniana
    outofsample <- outofsample[, -1]
    outofsample <- cbind(1, as.matrix(outofsample))
    ols <- lm(log(dane_zmienne$deahts_per_person_second_model) ~ log(dane_zmienne$vaccination_per_100_people) + log(dane_zmienne$population_density) + log(dane_zmienne$number_of_physicians) + log(dane_zmienne$Average_household_size) )
    
    ols.beta <- ols$coefficients
    ols.sigma <- vcov(ols)
    v <- nrow(insample) - ncol(insample)
    
    #Przybliżenie Monte Carlo - generujemy wektory oszacowan z rozkladu a posteriori
    #bez restrykcji
    #Rzadki przypadek - wylosuje się tak, że któraś ze zmiennych binarnych będzie współliniowa.
    losowanie.beta <- tryCatch(rmvt(S, sigma = ols.sigma, df = v, delta = ols.beta, type = "shifted"),
                               error = function(e) matrix(NA, S, ncol(insample)))
    
    #shifted daje odpowiedni rozklad dla symulacji z rozkladu a posteriori
    beta0 <- losowanie.beta[, 1] # stała
    beta1 <- losowanie.beta[, 2] # vaccination_per_100_people
    beta2 <- losowanie.beta[, 3] # ln(population density)
    beta3 <- losowanie.beta[, 4] # number_of_physians
    beta4 <- losowanie.beta[, 5] # Average_household_size
    
    important.beta <- losowanie.beta[beta1 < 0 & beta2 > 0 & beta3 < 0 & beta4 > 0 , ]
    #Czasami żadne beta nie spełniają restrykcji. Musimy mimo to zagwarantować dalszy przebieg pętli.
    bayes.beta <- tryCatch(apply(important.beta, 2, mean), error = function(e) rep(NA, ncol(insample)))
    bayes.prob <- nrow(important.beta) / nrow(losowanie.beta) 
    
    #Liczymy prognozy
    #Doliczamy logarytmy dla próby testowej
    outofsample[, 2:5] <- log(outofsample[, 2:5])
    ols.predict <- outofsample %*% ols.beta
    bayes.predict <- outofsample %*% bayes.beta
    rmse.ols <- sqrt(mean((y_empirical - exp(ols.predict)) ^ 2, na.rm = TRUE))
    rmse.bayes <- sqrt(mean((y_empirical - exp(bayes.predict)) ^ 2, na.rm = TRUE))
    
    #Ile razy pętla musiała być puszczona pomimo błędu
    errors.ols <- sum(is.na(ols.predict))
    errors.bayes <- sum(is.na(bayes.predict))
    
    #Średnie prawdopodobieństwo a posteriori spełnienia wszystkich restrykcji
    prob.bayes <- mean(bayes.prob, na.rm = TRUE)
    
    #Wypełniamy tabelkę ze średnimi błędami prognoz dla każdej wylosowanej próby testowej
    forecast.errors[k, ] <- c(rmse.ols, errors.ols, rmse.bayes, errors.bayes, prob.bayes)
  }
  #Uśredniamy wyniki tak, by otrzymać sumaryczne miary dla każdej wielkości próby testowej
  final.results[i, ] <- apply(forecast.errors, 2, mean, na.rm = TRUE)
}

rmse.ratio <- final.results[, 3] / final.results[, 1]
final.results <- cbind(final.results, rmse.ratio)
colnames(final.results)[6] <- "RMSE ratio"
final.results
