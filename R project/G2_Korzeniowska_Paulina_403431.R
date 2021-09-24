library(dplyr)
library(EnvStats)
install.packages(dplyr)
install.packages(EnvStats)
install.packages("tseries") 
library(tseries)

#OBROBKA DANYCH

dane <- subset(matchinfo, select = c("Address","League","Year","gamelength","blueTeamTag","bResult","rResult","redTeamTag","blueJungle","redJungle","blueMiddle","redMiddle","blueADCChamp","redADCChamp"))


g1<- select(gold, "Address","min_15","Type")
g1<-g1%>%
  filter(Type=="goldblueADC")
g2<- select(gold, "Address","min_15","Type")
g2<-g2%>%
  filter(Type=="goldblueSupport")
gg<- merge(x=g1,y=g2,by="Address")

dane<- merge(x=dane,y=gg,by="Address")

#wybieram dane z 2017 roku
dane <- filter(dane,Year==2017)  

#wybieram dane z 3 regionów Ameryka, Europa i Korea
dane <- filter(dane, League =="NALCS" | League=="EULCS" | League=="LCK")

#utworzenie probki losowej 1000 elementów
proba <- dane[sample(1:1448, 1000),]

#obrobka missing values poprzez usuniecie
sum(is.na(proba))
proba <- na.omit(proba)

#usuwanie zbytnio odstajacych danych
boxplot(proba$gamelength)
boxplot(proba$min_15.x)
boxplot(proba$min_15.y)

#OSTATECZNE DANE
probaf <- proba %>%
  filter(gamelength<55)
probaf <- probaf %>%
  filter(min_15.x<5800)
probaf <- probaf %>%
  filter(min_15.x>3800)
probaf <- probaf %>%
  filter(min_15.y<3700)

boxplot(probaf$gamelength)
boxplot(probaf$min_15.x)
boxplot(probaf$min_15.y)

probaf_Korea <- probaf %>%
  filter(League=="LCK")

probaf_Europa <- probaf %>%
  filter(League=="EULCS")

probaf_Ameryka <- probaf %>%
  filter(League=="NALCS")

#TESTY

#wskazniki
v<- var(probaf$gamelength)
sd<-sd(probaf$gamelength)
m<-mean(probaf$gamelength)

v
sd
m
vgadc <- var(probaf$min_15.x)
vgsup <- var(probaf$min_15.y)

#histogramy
par(mfrow=c(1,2))
hist(probaf$gamelength, probability = T, col = "lightgrey", main="Histogram dla czasu gry")
lines(density(probaf$gamelength), col="red", lwd=2)

par(mfrow=c(1,2))
hist(probaf$min_15.x, probability = T, col = "lightgrey", main="Histogram dla zota dla ADC ")
lines(density(probaf$min_15.x), col="red", lwd=2)

par(mfrow=c(1,2))
hist(probaf$min_15.y, probability = T, col = "lightgrey", main="Histogram dla zota dla Supporta")
lines(density(probaf$min_15.y), col="red", lwd=2)



#test normalnosci danych czasu gry

shapiro.test(probaf_Europa$gamelength)
shapiro.test(probaf_Ameryka$gamelength)
shapiro.test(probaf_Korea$gamelength)


par(mfrow=c(1,2))
hist(probaf_Europa$gamelength, probability = T, col = "lightgrey", main="Histogram dla czaau gry w Europie")
lines(density(probaf_Europa$gamelength), col="red", lwd=2)

par(mfrow=c(1,2))
hist(probaf_Korea$gamelength, probability = T, col = "lightgrey", main="Histogram dla czasu gry w Korei")
lines(density(probaf_Korea$gamelength), col="red", lwd=2)

#test normalnosci danych goldaadc
shapiro.test(probaf$min_15.x)

#test normalnosci danych goldasupport
shapiro.test(probaf$min_15.y)

#1. TEST ISTOTNOSCI WAARTOSCI OCZEKIWANEJ DLA CZASU GRY
hip1<- t.test(probaf$gamelength,mu=36.1, conf.level = 0.95)
hip1
pvaluehip1 <- hip1$p.value

#przedzial ufnoci
hip1$conf.int

#2.TEST ISTOTNOSCI WARTOSCI OCZEKIWANEJ DLA CZASU GRY W KOREI I EUROPIE
hip2<- t.test(probaf_Europa$gamelength, probaf_Korea$gamelength, alternative="greater")
hip2
pvaluehip2 <-hip2$p.value

#przedzial ufnoci
hip2$conf.int

#4. TEST NORMALNOSCI DANYH CZASU GRY
# Jarque-Bera
jarque.bera.test(probaf$gamelength)

#wybieranie danych dla postaci xayah i caitlyn

xayahf <- select(probaf, "blueADCChamp","redADCChamp")
xayahf$blueADCChamp[xayahf$blueADCChamp != "Xayah"] <- 0
xayahf$redADCChamp[xayahf$redADCChamp != "Xayah"] <- 0
xayahf$blueADCChamp[xayahf$blueADCChamp == "Xayah"] <- 1
xayahf$redADCChamp[xayahf$redADCChamp == "Xayah"] <- 1


xayahb <- xayahf %>%
  count(xayahf$blueADCChamp)

xayahr <- xayahf %>%
  count(xayahf$redADCChamp)

xayah <- data.frame(xayahb, xayahr)

xayah <- xayah %>%
  filter(xayahf.blueADCChamp==1)%>%
  mutate(suma=n+n.1)%>%
  mutate(nc=length(xayahf$blueADCChamp))

Caitlyn <- select(probaf, "blueADCChamp","redADCChamp")
Caitlyn$blueADCChamp[Caitlyn$blueADCChamp != "Caitlyn"] <- 0
Caitlyn$redADCChamp[Caitlyn$redADCChamp != "Caitlyn"] <- 0
Caitlyn$blueADCChamp[Caitlyn$blueADCChamp == "Caitlyn"] <- 1
Caitlyn$redADCChamp[Caitlyn$redADCChamp == "Caitlyn"] <- 1


Caitlynb <- Caitlyn %>%
  count(Caitlyn$blueADCChamp)

Caitlynr <- Caitlyn %>%
  count(Caitlyn$redADCChamp)

caitlyn <- data.frame(Caitlynb, Caitlynr)

caitlyn <- caitlyn %>%
  filter(Caitlyn.blueADCChamp==1)%>%
  mutate(suma=n+n.1)%>%
  mutate(nc=length(Caitlyn$blueADCChamp))


#3. test dla frakcji 
prop.test(x=c(xayah$suma,caitlyn$suma),n=c(xayah$nc,caitlyn$nc))


#wybieranie danych dla Jankosa i Perkza

jankos <- select(probaf, "blueJungle","redJungle")
jankos$blueJungle[jankos$blueJungle != "Jankos"] <- 0
jankos$redJungle[jankos$redJungle != "Jankos"] <- 0
jankos$blueJungle[jankos$blueJungle == "Jankos"] <- 1
jankos$redJungle[jankos$redJungle == "Jankos"] <- 1


jankosb <- jankos %>%
  count(jankos$blueJungle)

jankosr <- jankos %>%
  count(jankos$redJungle)

Jankos <- data.frame(jankosb, jankosr)

Jankos <- Jankos %>%
  filter(jankos.blueJungle==1)%>%
  mutate(suma=n+n.1)%>%
  mutate(nc=length(jankos$blueJungle))

Perkz <- select(probaf, "blueMiddle","redMiddle")
Perkz$blueMiddle[Perkz$blueMiddle != "Perkz"] <- 0
Perkz$redMiddle[Perkz$redMiddle != "Perkz"] <- 0
Perkz$blueMiddle[Perkz$blueMiddle == "Perkz"] <- 1
Perkz$redMiddle[Perkz$redMiddle == "Perkz"] <- 1


Perkzb <- Perkz %>%
  count(Perkz$blueMiddle)

Perkzr <- Perkz %>%
  count(Perkz$redMiddle)

perkz <- data.frame(Perkzb, Perkzr)

perkz <- perkz %>%
  filter(Perkz.blueMiddle==1)%>%
  mutate(suma=n+n.1)%>%
  mutate(nc=length(Perkz$blueMiddle))

#4. test dla frakcji 
prop.test(x=c(Jankos$suma,perkz$suma),n=c(Jankos$nc,perkz$nc),alternative="greater")


 
#7. ANNOVA

# ANOVA- ANALIZA WARIANCJI (H0: wartości oczekiwane w podgrupach są sobie równe, H1: przynajmniej jedna jest różna)
res.aov <- aov(gamelength ~ League, data = probaf)
# PODSUMOWANIE ANOVY - bez tego nie dostaniemy się do p-value
summary(res.aov)

# TEST POST-HOC DLA ANOVY- test pozwalający zweryfikować w których dokładnie grupach zachodzą istotne różnice
TukeyHSD(res.aov)  # (H0: m1=m2, H1: są różne )


#TESTOWANIE ZAŁOŻENIA O NORMALNOŚCI ROZKŁADU RESZT ANOVY
aov_residuals <- residuals(object = res.aov )
# test Shapiro- Wilka (H0: normalność rozkładu)
shapiro.test(x = aov_residuals)


#KIEDY ZAŁOŻENIA ANOVY NIE SĄ SPEŁNIONE STOSUJEMY JEJ NIEPARAMETRYCZNY ODPOWIEDNIK
#test Kruskala-Wallisa
kruskal.test(gamelength ~ League, data = probaf)





#7. Test wariancji
var.test(probaf$min_15.x,probaf$min_15.y, alternative="greater")

