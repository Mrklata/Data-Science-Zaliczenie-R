#LIBRARIES

library(ggplot2)
library(tidyverse)
library(zoo)

#DATA

tabela<-read.csv(file="data.csv",sep=";",dec=",",encoding = "UTF-8")

#ORDERING DATA

names(tabela)[3:16] = paste0("Bułka_Pszenna", c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tabela)[17:30] = paste0("Mięso_Wieprzowe_Bez_Kości", c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tabela)[31:44] = paste0("Kiełbasa_Wędzona", c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tabela)[45:58] = paste0("Filety_Z_Morszczuka_Mrożone",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tabela)[59:72] = paste0("Karp_Świeży", c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tabela)[73:86] = paste0("Podkoszulek_Męski_Bawełniany_Bez_Rękawa",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tabela)[87:100] = paste0("Rajstopy_Damskie_Gładkie_15Den",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tabela)[101:114] = paste0("Spodnie_Jeans", c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tabela)[115:128] = paste0("Czyszczenie_Chemiczne_Garnituru_Męskiego",c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
names(tabela)[129:142] = paste0("Olej_Napędowy", c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))

#CREATING TABELS AND VECTORS WITH MONTHS ETC

woj_one = c(tabela[,2])
woj = rep(tabela[,2],14)
woj_rep = rep(woj,12)
rok = rep(c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),each=17)
rok_rep = rep(rok,12)
length(rok_rep)
mies = rep(c("sty","lut","mar","kwi","maj","cze","lip","sie","wrz","paź","lis","gru"),each=238)

#CREATING VECTORS OF PRICES

Bulka_Pszenna = c()
Mięso_Wieprzowe_Bez_Kości = c()
Kielbasa_Wędzona= c()
Filety_Z_Morszczuka_Mrożone = c()
Karp_Świeży = c()
Podkoszulek_Męski_Bawełniany_Bez_Rękawa = c()
Rajstopy_Damskie_Gładkie_15Den = c()
Spodnie_Jeans = c()
Czyszczenie_Chemiczne_Garnituru_Męskiego = c()
Olej_Napędowy = c()

for (n in c(0, 140, 140*11)) {
  for (i in 3:16) {
    Bulka_Pszenna = c(Bulka_Pszenna, tabela[,i+n])
  }
  for (i in 17:30) {
    Mięso_Wieprzowe_Bez_Kości = c(Mięso_Wieprzowe_Bez_Kości, tabela[,i+n])
  }
  for (i in 31:44) {
    Kielbasa_Wędzona = c(Kielbasa_Wędzona, tabela[,i+n])
  }
  for (i in 45:58) {
    Filety_Z_Morszczuka_Mrożone = c(Filety_Z_Morszczuka_Mrożone, tabela[,i+n])
  }
  for (i in 59:72) {
    Karp_Świeży = c(Karp_Świeży, tabela[,i+n])
  }
  for (i in 73:86) {
    Podkoszulek_Męski_Bawełniany_Bez_Rękawa = c(Podkoszulek_Męski_Bawełniany_Bez_Rękawa, tabela[,i+n])
  }
  for (i in 87:100) {
    Rajstopy_Damskie_Gładkie_15Den = c(Rajstopy_Damskie_Gładkie_15Den, tabela[,i+n])
  }
  for (i in 101:114) {
    Spodnie_Jeans = c(Spodnie_Jeans, tabela[,i+n])
  }
  for (i in 115:128) {
    Czyszczenie_Chemiczne_Garnituru_Męskiego = c(Czyszczenie_Chemiczne_Garnituru_Męskiego, tabela[,i+n])
  }
  for (i in 129:142) {
    Olej_Napędowy = c(Olej_Napędowy, tabela[,i+n])
  }
}

#CREATING DATA FRAME

data = data.frame(woj = woj_rep, rok = rok_rep, mies = mies, Bulka_Pszenna = Bulka_Pszenna, Mięso_Wieprzowe_Bez_Kości = Mięso_Wieprzowe_Bez_Kości,
                 Kielbasa_Wędzona = Kielbasa_Wędzona, Filety_Z_Morszczuka_Mrożone = Filety_Z_Morszczuka_Mrożone, Karp_Świeży = Karp_Świeży,
                 Podkoszulek_Męski_Bawełniany_Bez_Rękawa = Podkoszulek_Męski_Bawełniany_Bez_Rękawa, Rajstopy_Damskie_Gładkie_15Den = Rajstopy_Damskie_Gładkie_15Den,
                 Spodnie_Jeans = Spodnie_Jeans, Czyszczenie_Chemiczne_Garnituru_Męskiego = Czyszczenie_Chemiczne_Garnituru_Męskiego, Olej_Napędowy = Olej_Napędowy)

#REPLACING ALL NA WITH 0

data[is.na(data)] = 0

#COLLECTING AVREAGE AND STD FOR ALL DATA

for(i in 4:13){
  print(tapply(data[,i],data[,2],mean))
}

for(i in 4:13){
  print(tapply(data[,i],data[,2],sd))
}

#COLLECTTING DATA FOR POLAND

poland = data[data$woj == "POLSKA",]

#ORDERING DATA FOR POLAND

poland$data = as.yearmon(paste(poland$mies, poland$rok)) 
poland = poland[order(poland$mies),]

#COLLECTING AVERAGES, SD FOR WOJ

AV_Bułka_Pszenna = tapply(data[,4], data[,1], mean)
AV_Mięso_Wieprzowe_Bez_Kości = tapply(data[,5], data[,1], mean)
AV_Kiełbasa_Wędzona = tapply(data[,6], data[,1], mean, na.rm=T)
AV_Filety_Z_Morszczuka_Mrożone = tapply(data[,7], data[,1], mean)
AV_Karp_Świeży = tapply(data[,8], data[,1],mean)
AV_Podkoszulek_Męski_Bawełniany_Bez_Rękawa = tapply(data[,9], data[,1], mean)
AV_Rajsstopy_Damskie_Gładkie_15Den = tapply(data[,10], data[,1], mean)
AV_Spodnie_Jeans = tapply(data[,11], data[,1], mean)
AV_Czyszczenie_Chemiczne_Garnituru_Męskiego = tapply(data[,12], data[,1], mean)
AV_Olej_Napędowy = tapply(data[,13], data[,1], mean)

Sd_Bułka_Pszenna = tapply(data[,4], data[,1], sd)
Sd_Mięso_Wieprzowe_Bez_Kości = tapply(data[,5], data[,1], sd)
Sd_Kiełbasa_Wędzona = tapply(data[,6], data[,1], sd, na.rm=T)
Sd_Filety_Z_Morszczuka_Mrożone = tapply(data[,7], data[,1], sd)
Sd_Karp_Świeży = tapply(data[,8], data[,1], sd)
Sd_Podkoszulek_Męski_Bawełniany_Bez_Rękawa = tapply(data[,9], data[,1], sd)
Sd_Rajstopy_Damskie_Gładkie_15Den = tapply(data[,10], data[,1], sd)
Sd_Spodnie_Jeans = tapply(data[,11], data[,1], sd)
Sd_Czyszczenie_Chemiczne_Garnituru_Męskiego = tapply(data[,12], data[,1], sd)
Sd_Olej_Napędowy = tapply(data[,13], data[,1], sd)

#CREATING AVERAGE, SD DATA FRAMES

average = data.frame(woj = as.factor(sort(tabela[,1])), AV_Bułka_Pszenna = AV_Bułka_Pszenna, AV_Mięso_Wieprzowe_Bez_Kości = AV_Mięso_Wieprzowe_Bez_Kości,
                    AV_Kiełbasa_Wędzona = AV_Kiełbasa_Wędzona, AV_Filety_Z_Morszczuka_Mrożone = AV_Filety_Z_Morszczuka_Mrożone, AV_Karp_Świeży = AV_Karp_Świeży,
                    AV_Podkoszulek_Męski_Bawełniany_Bez_Rękaw = AV_Podkoszulek_Męski_Bawełniany_Bez_Rękawa, AV_Rajsstopy_Damskie_Gładkie_15Den = AV_Rajsstopy_Damskie_Gładkie_15Den,
                    Sr_Spodnie_Jeans = AV_Spodnie_Jeans, AV_Czyszczenie_Chemiczne_Garnituru_Męskiego = AV_Czyszczenie_Chemiczne_Garnituru_Męskiego,
                    AV_Olej_Napędowy = AV_Olej_Napędowy)

deviation = data.frame(average, Sd_Bułka_Pszenna=Sd_Bułka_Pszenna, Sd_Mięso_Wieprzowe_Bez_Kości = Sd_Mięso_Wieprzowe_Bez_Kości,
                       Sd_Kiełbasa_Wędzona = Sd_Kiełbasa_Wędzona, Sd_Filety_Z_Morszczuka_Mrożone = Sd_Filety_Z_Morszczuka_Mrożone,
                       Sd_Karp_Świeży = Sd_Karp_Świeży, Sd_Podkoszulek_Męski_Bawełniany_Bez_Rękawa = Sd_Podkoszulek_Męski_Bawełniany_Bez_Rękawa,
                       Sd_Rajstopy_Damskie_Gładkie_15Den = Sd_Rajstopy_Damskie_Gładkie_15Den, Sd_Spodnie_Jeans = Sd_Spodnie_Jeans,
                       Sd_Czyszczenie_Chemiczne_Garnituru_Męskiego = Sd_Czyszczenie_Chemiczne_Garnituru_Męskiego, Sd_Olej_Napędowy = Sd_Olej_Napędowy)

#ANALYZING AVERAGE FOR WOJ

average_for_product_point = function(x) {
  ggplot(data = average, 
         aes(x = woj_one, 
             y = x)) + 
    geom_point(color = 'blue', size = 2, shape = 20)+
    theme(axis.text.x = element_text(angle = 80, hjust = 1))+
    xlab("Województwa")+
    ylab("Cena")

}
average_for_product_bar = function(x){
  ggplot(data = average, 
         aes(x = woj_one, 
             y = x)) +
    geom_bar(stat = 'identity', fill = "lightgreen", colour = "black") +
    theme(axis.text.x = element_text(angle = 80, hjust = 1))+
    xlab("Województwa")+
    ylab("Cena")
}

#ANALYZING SD FOR WOJ

Bul_data = c(AV_Bułka_Pszenna, Sd_Bułka_Pszenna) 
Mięs_data = c(AV_Mięso_Wieprzowe_Bez_Kości, Sd_Mięso_Wieprzowe_Bez_Kości)
Kiel_data = c(AV_Kiełbasa_Wędzona, Sd_Kiełbasa_Wędzona)
Fil_data = c(AV_Filety_Z_Morszczuka_Mrożone, Sd_Filety_Z_Morszczuka_Mrożone)
Karp_data = c(AV_Karp_Świeży, Sd_Karp_Świeży)
Podk_data = c(AV_Podkoszulek_Męski_Bawełniany_Bez_Rękawa, Sd_Podkoszulek_Męski_Bawełniany_Bez_Rękawa)
Raj_data = c(AV_Rajsstopy_Damskie_Gładkie_15Den, Sd_Rajstopy_Damskie_Gładkie_15Den)
Spod_data = c(AV_Spodnie_Jeans, Sd_Spodnie_Jeans)
Czyszcz_data = c(AV_Czyszczenie_Chemiczne_Garnituru_Męskiego, Sd_Czyszczenie_Chemiczne_Garnituru_Męskiego)
Olej_data = c(AV_Olej_Napędowy, Sd_Olej_Napędowy)

sd_for_woj_point = function(x) {
  ggplot(deviation, aes(x = woj_one, y = x[1:17]))+ 
    geom_errorbar(aes(ymin = x[1:17]-x[18:34], ymax = x[1:17]+x[18:34]), width = .1, col = "black")+
    geom_point(color = "blue", size = 2, shape = 20)+ 
    theme(axis.text.x = element_text(angle = 80, hjust = 1))+
    xlab("Województwa")+
    ylab("Cena")
}

sd_for_woj_bar = function(x) {
  ggplot(data = deviation, aes(x = woj_one, y = x[1:17]))+
    geom_bar(stat = "identity", fill = "lightgreen", colour = "black")+
    geom_errorbar(aes(ymin = x[1:17] - x[18:34], ymax = x[1:17] + x[18:34]), width = .2, col = "blue")+ 
    theme(axis.text.x = element_text(angle = 80, hjust = 1))+
    xlab("Województwa")+
    ylab("Cena")
}

#ANALYZING DATA FOR POLAND

poland_price_by_all_years = function(a) {
  ggplot(data = poland, aes(x = data, y = a, color = rok))+ 
    geom_point(size = 5, shape = 20)+
    ggtitle("Cena produktu na przestrzeni lat")+
    scale_x_yearmon()+
    theme(axis.text.x = element_text(angle = 80, hjust = 1))+
    xlab("Miesiące")+
    ylab("Cena")
}

poland_prce_by_year_box = function(a) {
  ggplot(poland, aes(x = mies, y = a, fill = mies)) + 
    geom_boxplot(alpha = 0.6) 
}

poland_price_by_month = function(a) {
  ggplot(data = poland, 
         aes(x = mies, y = a, color = rok, group = rok)) + 
    geom_point(size = 5, shape = 20) +
    ggtitle("Ceny w roku na przzestrzeni miesięcy") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    xlab("Miesiące")+
    ylab("Cena")
}
