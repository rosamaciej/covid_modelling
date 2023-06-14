#Wyczyszczenie worksplace, wykresóW oraz konsoli

#rm(list=ls())
#if(!is.null(dev.list())) dev.off()
#cat("\014")

#Załadowanie potrzebnych pakietów
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
dane_zgony <- read_excel("dane_zgony.xlsx")
dane_zgony <- na.omit(dane_zgony)
#names(dane_zgony)[names(dane_zgony) == "Country name"] <- 12
#colnames(dane_zgony) <- as.Date(names(dane_zgony))

#kolumny <- colnames(dane_zgony)
#kolumny <- as.Date(as.numeric(kolumny), origin = "1899-12-30")

#colnames(dane_zgony) <- kolumny
#names(dane_zgony)[names(dane_zgony) == "1900-01-11"] <- "Country names"
#rm(kolumny)
dane_zgony <- dane_zgony[-1,]
#dane_zgony <- dane_zgony[,-2]

dane_zakazenia <- read_excel("dane_zakazenia.xlsx")
dane_zakazenia <- na.omit(dane_zakazenia)
dane_zgony$Date <- as.Date(dane_zgony$Date)
dane_zakazenia$Date <- as.Date(dane_zakazenia$Date)


dane_zmienne <- read_excel("zmienne2.xlsx")

dane_google <- read_excel("dane_google.xlsx")

dane_zgony_eu <- read_excel("dane_zgony_eu.xlsx")
eu_zgony_tran <- read_excel("dane_zgony_eu_transpozycja.xlsx")

merged <- merge(eu_zgony_tran,dane_zmienne,by= "Country_name")


UK.results <- lm(dane_zgony$`United Kingdom` ~ dane_zakazenia$`United Kingdom` + dane_zgony$Date)
summary(UK.results)
plot(dane_zakazenia$Date,log(dane_zakazenia$Poland),main = "Zgony i zakażenia w Polsce od 22 lutego 2020 do 30 marca 2021",type = "l",col = "blue",xlab = "Time", ylab="Value") +
lines(dane_zgony$Date, log(dane_zgony$Poland),col = "red") 
legend("topleft", legend=c("Zakażenia", "Zgony"),col=c("blue","red"), lty=1:2, cex=0.8)

dane_polska <- data_frame(dane_zakazenia$Date,dane_zgony$Poland,dane_zakazenia$Poland)
colnames(dane_polska) <- c("Data", "Zgony", "Zakażenia")

dane_europa <- data_frame(dane_zmienne$Date,dane_zmienne$P,dane_zakazenia$Poland)
colnames(dane_polska) <- c("Data", "Zgony", "Zakażenia")


dane_all <- data_frame(dane_zgony$Date, dane_zgony$All_deaths, dane_zakazenia$All_cases)
colnames(dane_all) <- c("Data","Zgony","Przypadki")

#gapminder %>%

##### Wykresy i histogramy  
legend("topleft",c("Zgonów", "Przypadków"),
         col=c("red", "green"), lty=1, cex=0.8,
         title="Liczba", text.font=4, bg='white')

legend("topleft",c("Zgonów", "Przypadków"),
       col=c("red", "green"), lty=1, cex=0.8,
       title="Logarytm liczby", text.font=4, bg='white')

  
plot_pl_log <- ggplot(dane_polska, aes(x= Data,y=log(Zgony))) +
  geom_line(color="red", size=2)+
  geom_line(aes(y=log(Zakażenia)),color="green",size=2)+
  xlab("Data")+
  ylab("Logartmy liczby zakażeń i zgonów w Polsce")
print(plot_pl_log)

#plot(x=dane_polska$Data,y=dane_polska$Zgony,
  #main="Logartmy liczby zakażeń i zgonów w Polsce",
  #type="l",
  #col="red",
  #lines(dane_polska$Zakażenia,col="green"),
  #legend(1, 95, legend=c("Zgony", "Przypadki"),
         #col=c("red", "green"), lty=1:2, cex=0.8,
         #title="Liczba", text.font=4, bg='lightblue'))
     

  
plot_pl <- ggplot(dane_polska, aes(x= Data,y=Zgony)) +
  geom_line(color="red", size=2)+
  geom_line(aes(y=Zakażenia),color="green",size=2)+
  xlab("Data")+
  ylab("Liczby zakażeń i zgonów w Polsce")
print(plot_pl)


lp1 <- ggplot(dane_polska, aes(x=Data, y=Zgony, group=Variables, colour=Variables)) + geom_line() + geom_point()
options(scipen=10000)

plot_all <- ggplot(dane_all, aes(x=Data,y=Zgony)) +
  geom_line(color="red", size=2)+
  geom_line(aes(y=Przypadki),color="green",size=2)+
  xlab("Data")+
  ylab("Ilość zakażeń i zgonów na świecie")

print(plot_all)
require(scales)


plot_all_log  <- ggplot(dane_all, aes(x=Data,y=log(Zgony))) +
  geom_line(color="red", size=2)+
  geom_line(aes(y=log(Przypadki)),color="green",size=2)+
  xlab("Data")+
  ylab("Liczby zakażeń i zgonów na świecie")

print(plot_all_log)

hist(dane_zmienne$deaths_per_milion_april21, main="Śmiertleność na milion mieszkańców", 
     xlab="Liczba przypadków śmiertelnych na milion mieszkańców",
     ylab="Liczba Państw",
     border="black", 
     col="red",
     xlim=c(0,2500),
     las=0, 
     breaks=8)

hist(dane_zmienne$cases_per_thousand, main="Zachorowalność na tysiąc mieszkańców", 
     xlab="Liczba zachorowań na tysiąc mieszkańców",
     ylab="Liczba Państw",
     border="black", 
     col="green",
     xlim=c(0,200),
     las=0)

p+scale_x_date(date_labels = "%Y %b %d")

summary(dane_zakazenia$Poland)

cor(dane_zgony$`United Kingdom`,dane_zakazenia$`United Kingdom`)
cor(dane_zgony$Belgium,dane_zakazenia$Belgium)
cor(dane_zgony$Poland,dane_zakazenia$Poland)


print(summary(dane_zmienne[,-1]))

model1 <- lm(log(dane_zmienne$deaths_per_milion_april21) ~ log(dane_zmienne$GDP_for_HC) + log(dane_zmienne$mean_temperature) + log(dane_zmienne$population_density)  + log(dane_zmienne$Average_household_size), data = dane_zmienne)
summary(model1)

model2 <- lm((dane_zmienne$deaths_per_milion_april21) ~ dane_zmienne$DTP_1980 + dane_zmienne$GDP_for_HC + dane_zmienne$mean_temperature + dane_zmienne$population_density + dane_zmienne$number_of_physicians + dane_zmienne$people65 , data = dane_zmienne)
summary(model2)
plot(model2)
model3 <- lm(dane_zmienne$deaths_per_milion_april21 ~  dane_zmienne$number_of_physicians + dane_zmienne$population_density, data = dane_zmienne)
summary(model3)
plot(model3)
model4 <- lm(log(dane_zmienne$deaths_per_milion_april21) ~ log(dane_zmienne$number_of_physicians) + log(dane_zmienne$population_density)  + dane_zmienne$DTP_1980, data = dane_zmienne)
summary(model4)

par(mfrow=c(2,2))
plot(model4)

polski_model <- lm(dane_zgony$Poland ~ dane_zakazenia$Poland)




dane_model_google <- dane_google[,-1]
model_google_2 <- lm(dane_model_google$cases_per_thousand_2021 ~ dane_model_google$population_density + dane_model_google$transit_stations + dane_model_google$residential + dane_model_google$metro_area + dane_model_google$workplaces)
summary(model_google_2)



plot(log(dane_zgony_eu$all_countries))
plot(dane_zgony_eu$Poland)

#korelacje

cormat_dane <- read_excel("dane_korelacja.xlsx")
cormat <- cor(cormat_dane)

model_ogolny <- lm(cormat_dane$deaths_per_milion_april21 ~ DTP_1980 + GDP_for_HC + mean_temperature + people65 + population_density + number_of_physicians + Average_household_size + urbanization,data = cormat_dane)
summary(model_ogolny)

model_ogolny_2022 <- lm(cormat_dane$deahts_per_person_second_model ~ vaccination_per_100_people + DTP_1980 + GDP_for_HC + mean_temperature + people65 + population_density + number_of_physicians + Average_household_size + urbanization,data = cormat_dane)
summary(model_ogolny_2022)

corrplot(cormat, method="circle")
corrplot(cormat, method="pie")
corrplot(cormat, method="color")

cor.mtest = function(mat, ...) {
  n = ncol(mat)
  p.mat= matrix(NA, n, n)
  diag(p.mat) = 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp = cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] = p.mat[j, i] = tmp$p.value
    }
  }
  colnames(p.mat) = rownames(p.mat) = colnames(mat)
  mat = as.matrix(mat)
  p.mat
}

p.mat = cor.mtest(cormat)

corrplot(cormat, type="upper", order="hclust", p.mat = p.mat ,sig.level = 0.1)

rm(act,ARMA.coef,arma.CSS,arma.CSS_ML,arma.ML,arma0,arma1,B,bmat,dmARMA,dmVAR,e,errARMA,errRW,errVAR,fcst)

cor(dane_zmienne$cases_per_thousand,dane_zmienne$deaths_per_milion_april21)
cor.test(dane_zmienne$cases_per_thousand,dane_zmienne$deaths_per_milion_april21)
cor()

library(Hmisc)
res <- rcorr(as.matrix(cormat))
round(res$P, 3)


#korelacja europa zgony

cormat_zgony <- subset(dane_zgony_eu,select =c("Austria","Belgium","Poland","Spain","United Kingdom","Italy","Netherlands","France","Germany","Portugal","Sweden","all_countries"))
cormat_zgony <- cor(cormat_zgony)
corrplot(cormat_zgony,type="upper",order="hclust",p.mat=p.mat,sig.level = 0.05)

myvars <- names(dane_zgony_eu) %in% c("Austria","Belgium","Poland","Spain","United Kingdom","Italy","Netherlands","France","Germany","Portugal","Sweden","Malta","Danmark","Finland","Latvia","Slovakia","Slovenia","Date")
cormat_zgony2 <- dane_zgony_eu[!myvars]
cormat_zgony2 <- cor(cormat_zgony2)
corrplot(cormat_zgony2,type="upper",order="hclust",p.mat=p.mat,sig.level = 0.05)

par(mfrow = c(1,1))
plot(dane_zgony_eu$all_countries,type="l",x=dane_zgony_eu$Date)

merge(dane_zmienne$Country_name)   

########## korelacja miedzy przypadkami a zgonami

cor(dane_all$Zgony,dane_all$Przypadki)
cor.test(dane_all$Zgony,dane_all$Przypadki,
          alternative = c("two.sided", "less", "greater"),
          method =  "spearman",
          exact = NULL, conf.level = 0.95, continuity = FALSE)

ks.test(dane_all$Zgony,pnorm(q))
shapiro.test(dane_all$Przypadki)
ks.test(dane_all$Zgony, "pnorm", mean=mean(dane_all$Zgony), sd=sd(dane_all$Zgony))

acf(dane_all$Zgony,dane_all$Przypadki,main="ACF",ylab="", sub="", xlab="")
ccf(dane_all$Zgony,dane_all$Przypadki,main="CCF",lag=100)




##################### Bayes




#Ustalamy parametry rozkładu a posteriori ze skrajnie nieinformacyjnym rozkładem a priori N-G
ols.beta <- model4$coefficients
ols.sigma <- vcov(model4)
ols.sum.sq.residuals <- sum(model4$residuals ^ 2)

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
colnames(losowanie.beta) <- rownames(summary(model4)$coefficients)
head(losowanie.beta) #Ten sam współczynnik występuje z różnymi znakami

beta0 <- losowanie.beta[, 1] # stała
beta1 <- losowanie.beta[, 2] # ln(number_of_physians
beta2 <- losowanie.beta[, 3] # ln(population density)
beta3 <- losowanie.beta[, 4] # DTP1980


t(t(apply(losowanie.beta,2,mean)))

#Importance sampling
important.beta <- losowanie.beta[beta1 < 0 & beta2 > 0 & beta3 > 0, ]

#Prawdopodobieństwo a posteriori spełnienia restrykcji

#################################################################

#Metoda 1
nrow(important.beta)/nrow(losowanie.beta)

#Metoda 2
R <- diag(c(-1,1,1))
dim(R)
p.restrykcji <- as.numeric(pmvt(lower = rep(0,3),
                                sigma = s2.posterior * (R %*% U.posterior[c(2:4), c(2:4)] %*% t(R)),
                                df = v.posterior, 
                                delta = as.vector(R %*% beta.posterior[c(2:4)]), type = "shifted"))
p.restrykcji

##################################################################

par(mfrow=c(2,1))

#Ilustracja gęstości a posteriori dla parametru liczby lekarzy na 100000 mieszkańców
important.beta1 <- important.beta[, 2]
restricted.h <- hist(important.beta1, breaks = (- 500:500) / 100, 
                     col = "gray", 
                     xlab = "Liczba lekarzy na 100000 mieszkańców: parametr", 
                     main = "Rozkład a posteriori przy uciętym rozkładzie a priori")
unrestricted.h <- hist(beta1, breaks = (- 500:500) / 100, col = "gray", 
                       xlab = "Liczba lekarzy na 100000 mieszkańców: parametr", 
                       main = "Rozkład a posteriori przy skrajnie nieinformacyjnym rozkładzie a priori")
par(mfrow=c(1,1))
plot(x = (-499:500) / 100, y = restricted.h$density, col = green_line, type = "l", lwd = 3, 
     main = "Rozkłady a posteriori parametru ",
     xlab = "wartość parametru number_of_physians", ylab = "gęstość")
lines(x = (- 499:500) / 100, y = unrestricted.h$density, col = "red", lwd = 3)
legend(x = "left", fill = c("red", green_line), legend = c("nieinformacyjny rozkład N-G", "restrykcje narzucone a priori"))
abline(v = mean(important.beta1), col = green_line, lwd = 3, lty = 3)
abline(v = mean(beta1), col = "red", lwd = 1, lty = 3)

#Ilustracja gęstości a posteriori dla parametru przy wydadtkach na ochrone zdrowia
important.beta1 <- important.beta[, 2]
windows(width = 1000, height = 800)
restricted.h <- hist(important.beta1, breaks = (- 600:600) / 100, col = "gray", 
                     xlab = "Wydatki na ochorone: parametr", main = "Ucięty rozkład a priori")
unrestricted.h <- hist(beta1, breaks = (-600:600) / 100, col = "gray", 
                       xlab = "Wydatkich na ochrone zdrowia: parametr", main = "Skrajnie nieinformacyjny rozkład a priori")
plot(x = (-599:600) / 100, y = restricted.h$density, type = "l", lwd = 3, 
     main = "Rozkłady a posteriori parametru beta4 (Mean temperature)", xlab = "wartość parametru beta4", ylab = "gęstość")
lines(x = (- 599:600) / 100, y = unrestricted.h$density, col = grey_line, lwd = 1)
legend(x = "left", fill = c(grey_line, green_line), legend = c("nieinformacyjny rozkład N-G", "restrykcje narzucone a priori"))
abline(v = mean(important.beta1), col = green_line, lwd = 3, lty = 2)
abline(v = mean(beta1), col = grey_line, lwd = 1, lty = 2)

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

frequentist.results <- cbind(confint(model4)[,1],model4$coefficients,confint(model4)[,2])
colnames(frequentist.results) <- c("Dolna granica 95% p.u.","Oszacowanie bez ograniczeń", "Górna granica 95% p.u.");
round(frequentist.results, digits=3)

#Który model jest lepszy?

colnames()
dane_short <- dane_zmienne[, c( "deaths_per_milion_april21","number_of_physicians",
                       "population_density","DTP_1980")]

# Długości próby testowej
O <- c(1, 5, 10, 15, 20)
# Ile razy maksymalnie dzielimy naszą próbę (losowo)
K_max <- 70

#Ile razy losujemy z rozkładu a posteriori (mniej niż wcześniej, żeby przyspieszyć)
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
    y_empirical <- outofsample$deaths_per_milion_april21
    
    #Ucinamy zmienna objasniana
    outofsample <- outofsample[, -1]
    outofsample <- cbind(1, as.matrix(outofsample))
    ols <- lm(log(dane_zmienne$deaths_per_milion_april21) ~  log(dane_zmienne$number_of_physicians) + log(dane_zmienne$population_density) + dane_zmienne$DTP_1980, data = dane_zmienne)

    ols.beta <- ols$coefficients
    ols.sigma <- vcov(ols)
    v <- nrow(insample) - ncol(insample)
    
    #Przybliżenie Monte Carlo - generujemy wektory oszacowan z rozkladu a posteriori
    #bez restrykcji
    #Rzadki przypadek - wylosuje się tak, że któraś ze zmiennych binarnych będzie współliniowa.
    losowanie.beta <- rmvt(S, sigma = s2.posterior * U.posterior,
                           df = v.posterior, delta = beta.posterior,
                           type = "shifted")
    
    #shifted daje odpowiedni rozklad dla symulacji z rozkladu a posteriori
    beta0 <- losowanie.beta[, 1] # stała
    beta1 <- losowanie.beta[, 2] # ln(number_of_physians
    beta2 <- losowanie.beta[, 3] # ln(population density)
    beta3 <- losowanie.beta[, 4] # DTP1980
    
    important.beta <- losowanie.beta[beta1 < 0 & beta2 > 0 & beta3 > 0, ]
    #Czasami żadne beta nie spełniają restrykcji. Musimy mimo to zagwarantować dalszy przebieg pętli.
    bayes.beta <- tryCatch(apply(important.beta, 2, mean), error = function(e) rep(NA, ncol(insample)))
    bayes.prob <- nrow(important.beta) / nrow(losowanie.beta) 
    
    #Liczymy prognozy
    #Doliczamy logarytmy dla próby testowej
    outofsample[, 2:4] <- log(outofsample[, 2:4])
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


### Modele dla zakażeń

model_cases <- lm(log(dane_zmienne$cases_per_thousand) ~ log(dane_zmienne$people65) + log(dane_zmienne$number_of_physicians) , data = dane_zmienne)
summary(model_cases)

######## Model 2022

model2022 <- lm(log(dane_zmienne$deahts_per_person_2022) ~ log(dane_zmienne$vaccination_per_100_people) + log(dane_zmienne$population_density) + log(dane_zmienne$number_of_physicians)   )
summary(model2022)

dane_zmienne2 <- dane_zmienne[-1]
stand_dane_zmienne <- scale(dane_zmienne2[-1])
rm(dane_zmienne2)
stand_dane_zmienne <- as.data.frame(stand_dane_zmienne)
model2022_stand <- lm(stand_dane_zmienne$deahts_per_person_2022 ~ stand_dane_zmienne$vaccination_per_100_people + stand_dane_zmienne$population_density + stand_dane_zmienne$number_of_physicians)
summary(model2022_stand)

########
#Model Gamma


#2. Regresja wieloraka (OLS) i statystyki dostateczne

y <- as.matrix((stand_dane_zmienne)[1:26, c('deahts_per_person_2022')])
N.data <- length(y)
X <- cbind(as.matrix(rep(1, N.data)), 
           as.matrix(stand_dane_zmienne[1:26, c('vaccination_per_100_people','population_density','number_of_physicians')]))
Beta.ols.data <- model2022_stand$coefficients
v.data <- model2022_stand$df.residual
XTX.data <- t(X) %*% X
s2.data <- sum((model2022_stand$residuals) ^ 2) / v.data

#3. Parametry a priori

Beta.prior <- c(100, -0.5, 0.5, -0.5)
sm2.prior <- 4 # s2.prior = 0.25, s.prior = 0.5
U.prior <- 0.1 * 4 * diag(4) # zerowe kowariancje i wariancja (co do wartości oczekiwanej) 0.1 * 4 * (1/4) = 0.1 (tzn. odchylenie standardowe (0.1)^0.5 = 0.31)
U.prior[1, 1] <- 200
v.prior <- 100
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


model2022 <- lm(log(dane_zmienne$deahts_per_person_2022) ~ log(dane_zmienne$vaccination_per_100_people) + log(dane_zmienne$population_density) + log(dane_zmienne$number_of_physicians) + log(dane_zmienne$Average_household_size) )
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
restricted.h <- hist(important.beta1, breaks = (- 500:500) / 100, 
                     col = "gray", 
                     xlab = "Liczba lekarzy na 100000 mieszkańców: parametr", 
                     main = "Rozkład a posteriori przy uciętym rozkładzie a priori")
unrestricted.h <- hist(beta1, breaks = (- 500:500) / 100, col = "gray", 
                       xlab = "Liczba lekarzy na 100000 mieszkańców: parametr", 
                       main = "Rozkład a posteriori przy skrajnie nieinformacyjnym rozkładzie a priori")
par(mfrow=c(1,1))
plot(x = (-499:500) / 100, y = restricted.h$density, col = green_line, type = "l", lwd = 3, 
     main = "Rozkłady a posteriori parametru ",
     xlab = "wartość parametru number_of_physians", ylab = "gęstość")
lines(x = (- 499:500) / 100, y = unrestricted.h$density, col = "red", lwd = 3)
legend(x = "left", fill = c("red", green_line), legend = c("nieinformacyjny rozkład N-G", "restrykcje narzucone a priori"))
abline(v = mean(important.beta1), col = green_line, lwd = 3, lty = 3)
abline(v = mean(beta1), col = "red", lwd = 1, lty = 3)

#Ilustracja gęstości a posteriori dla parametru szczepień
important.beta1 <- important.beta[, 2]
windows(width = 1000, height = 800)
restricted.h <- hist(important.beta1, breaks = (- 600:600) / 100, col = "gray", 
                     xlab = "Wydatki na ochorone: parametr", main = "Ucięty rozkład a priori")
unrestricted.h <- hist(beta1, breaks = (-600:600) / 100, col = "gray", 
                       xlab = "Liczba szczepień na sto mieszkańców: parametr", main = "Skrajnie nieinformacyjny rozkład a priori")
plot(x = (-599:600) / 100, y = restricted.h$density, type = "l", lwd = 3, 
     main = "Rozkłady a posteriori parametru vaccination_per_100_people", xlab = "wartość parametru vaccination_per_100_people", ylab = "gęstość")
lines(x = (- 599:600) / 100, y = unrestricted.h$density, col = grey_line, lwd = 1)
legend(x = "left", fill = c(grey_line, green_line), legend = c("nieinformacyjny rozkład N-G", "restrykcje narzucone a priori"))
abline(v = mean(important.beta1), col = green_line, lwd = 3, lty = 2)
abline(v = mean(beta1), col = grey_line, lwd = 1, lty = 2)

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

frequentist.results <- cbind(confint(model2022)[,1],model4$coefficients,confint(model2022)[,2])
colnames(frequentist.results) <- c("Dolna granica 95% p.u.","Oszacowanie bez ograniczeń", "Górna granica 95% p.u.");
round(frequentist.results, digits=3)

#Który model jest lepszy?

colnames()
dane_short <- dane_zmienne[, c( "deaths_per_milion_april21","number_of_physicians",
                                "population_density","DTP_1980")]

# Długości próby testowej
O <- c(1, 5, 10, 15, 20)
# Ile razy maksymalnie dzielimy naszą próbę (losowo)
K_max <- 1

#Ile razy losujemy z rozkładu a posteriori (mniej niż wcześniej, żeby przyspieszyć)
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
    y_empirical <- outofsample$deaths_per_milion_april21
    
    #Ucinamy zmienna objasniana
    outofsample <- outofsample[, -1]
    outofsample <- cbind(1, as.matrix(outofsample))
    ols <- lm(log(dane_zmienne$deahts_per_person_2022) ~ log(dane_zmienne$vaccination_per_100_people) + log(dane_zmienne$population_density) + log(dane_zmienne$number_of_physicians) + log(dane_zmienne$Average_household_size) )
    
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
