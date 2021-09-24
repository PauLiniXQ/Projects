library('zoo')
library(dplyr)
library(ggplot2)

odrzucone<-0
df <- 1  ##stopnie swobody
licznosc <- 200 ## liczebnosc probki
klasy <- 1## liczba klas
j<-0

wynikiklasy<-as.data.frame(0)
wynikistopnie<-as.data.frame(klasy)
wynikilicznpsc<-as.data.frame(klasy)

for (i in 1:8) {  # petla zmieniajca licznosc
  klasy<- 4
  df <- 3
for (i in 1:7) { # petla zmieniajca st swobody
  klasy <- 4
for (i in 1:8) { ## petla mzieniajca klasy
for (i in 1:1000) { ## petla wykonujaca 1000 powtorzen badania
  
probka <- sort(rt(licznosc, df)) # generowanie probki z rozkladu t studenta 

#boxplot(probka) ## wykres pudelkowy

## ODRZUCAMY WARTOSCI ODSTAJACE
k = 1.5
quantiles <- quantile(probka, c(0.25, 0.5, 0.75))  
diff <- k * (quantiles[3] - quantiles[1])
lb <- quantiles[1] - diff 
lb<- as.numeric(lb)
ub <- quantiles[3] + diff
ub <- as.numeric(ub)

probka <- as.data.frame(probka)
probka <- filter(probka, probka >= lb) 
probka <- filter(probka, probka <= ub)

#boxplot(probka)

## -----------------------

p1 <- hist(probka$probka, breaks=klasy, plot = FALSE)  ## histogram podzielony na iles klas

breaks_cdf <- pt(p1$breaks, df) ## zapisywanie prawdopodobienstw potrzebnych do testu chi kwadrat
null.probs <- rollapply(breaks_cdf, 2, function(probka) probka[2]-probka[1])

a<- chisq.test(p1$counts, p=null.probs, rescale.p = TRUE, simulate.p.value = TRUE)


  if(a$p.value<0.05)
{
    print("odrzucam")
  odrzucone <- odrzucone +1 
}

}
odsetek <- 0
print(odrzucone) ## liczba odrzuconych hipotez glownych
odsetek <- odrzucone / 1000
print(odsetek)  ## odsetek odrzucen hipotezy glownej
odrzucone<-0
print("POZYCJA: ")
print(j)

## zapisywanie danych od tablicy 

wynikiklasy<- as.data.frame(odsetek)
wynikiklasy<- cbind(wynikiklasy, klasy)
wynikiklasy<- cbind(wynikiklasy, df)
wynikiklasy<- cbind(wynikiklasy, licznosc)

if(j==0){
  for(i in 1:4){
wynikilicznpsc[1,i] <- as.data.frame(wynikiklasy[1,i])
  }
  names(wynikilicznpsc) <- c("odsetek","klasy","df","licznosc")
}
else if(j!=0)
{
wynikilicznpsc <- rbind(wynikilicznpsc, wynikiklasy)
}
wynikiklasy<- NULL
wynikiklasy<-as.data.frame(0)

## --------------------

klasy <- klasy + 1 
j<-j+1

}
  df <- df +1
}
  licznosc <- licznosc + 50 

}


## -------------------- WYKRESY --------------------------------


wykresy1 <- as.data.frame(wynikilicznpsc)
wykresy1 <- cbind.data.frame(wykresy1, c(1:448))
names(wykresy1) <- c("odsetek","klasy","df","licznosc","numer")


  ggplot(data = wykresy1, aes(x=numer, y=odsetek,size = 2, col="blue")) +
  geom_point(alpha=0.5) +
  xlab("Numer kombinacji klasy, licznosci i stopni swobody") +
  ylab("Odsetek") +
  ggtitle("Odsetek odrzuconych hipotez glownych") +
  theme_light()
  
  #
  

  
  
  
wykres2 <-select(wykresy1, odsetek, klasy) %>%
  group_by(klasy)%>%
  summarise(
    odsetek=mean(odsetek)
  )


ggplot(wykres2, aes(x=klasy, y=odsetek, size = 2,col="pink")) +
  geom_point(alpha=0.7)+
  xlab("Liczba klas") +
  ylab("Odsetek") +
  ggtitle("Odsetek odrzuconych hipotez glownych") +
  stat_smooth(method = "lm", col = "pink", size = 1,alpha=0.3)+
  theme_light()


# ---------


wykres3 <-select(wykresy1, odsetek, df) %>%
  group_by(df)%>%
  summarise(
    odsetek=mean(odsetek)
  )


ggplot(data = wykres3, aes(x=df, y=odsetek, size = 2, col="blue")) +
  geom_point(alpha=0.7) +
  xlab("Liczba stopni swobody") +
  ylab("Odsetek") +
  ggtitle("Odsetek odrzuconych hipotez glownych") +
  theme_light()

## od 3 stopni w wzwyz prawdopodbienstwo odrzucenia hipotezy glownej maleje


wykres4 <-select(wykresy1, odsetek, licznosc) %>%
  group_by(licznosc)%>%
  summarise(
    odsetek=mean(odsetek)
  )


ggplot(data = wykres4, aes(x=licznosc, y=odsetek,size = 2, col="blue")) +
  geom_point() +
  xlab("Liczba obserwacji") +
  ylab("Odsetek") +
  ggtitle("Odsetek odrzuconych hipotez glownych") +
  theme_light()


## rosnie prawdpodobenstwo odrzucenia h0


