#Saubere Darstellung der Datenaufbereitung
library(dynlm); library(stargazer); library(car); library(lmtest) 
#Importieren des Datensatzes 
library(readxl)
AirbnbBerlin_Data <- read_excel("AirbnbBerlin Data Cedric Andrä.xlsx")
View(AirbnbBerlin_Data)
#######################
#Säuberung des Datensatzes
Airbnb<-na.omit(AirbnbBerlin_Data)
Airbnb$price<-as.numeric(Airbnb$Price) 
Airbnb$superhost<-as.numeric(Airbnb$Superhost)
Airbnb$reviewpm<-as.numeric(Airbnb$reviews_per_month)
Airbnb$location<-as.factor(Airbnb$neighbourhood_group_cleansed)
Airbnb$location<-relevel(Airbnb$location, "Mitte")
Airbnb$type<-as.factor(Airbnb$room_type)
Airbnb$instantb<-as.factor(Airbnb$instant_bookable)
Airbnb$foreign<-as.numeric(Airbnb$`Host nicht aus Berlin`)
#Entfernen der Wert von "price" die gleich 0 sind
Airbnb2<-subset(Airbnb, price!="0")
Airbnb2$verified<-as.factor(Airbnb2$host_identity_verified)
Airbnb2$nongerman<-as.factor(Airbnb2$`Non-German`)

summary(Airbnb2)
#Die Daten sind bis hierhin gesäubert
#Anfangen mit einfachen Modell: Elastizität von Price und Review Score
M0<-lm(log(price*0.9079)~log(review_scores_rating), data=Airbnb2)
summary(M0)
#Hinweis: Die Daten wurden in US-Dollar erhoben. Um den Preis in Euro umzurechnen, wird mit dem Kurs des Erhebungstages multipliziert
#Hinzufügen von Variablen zur Qualität der Unterkunft
M0.a<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm, data=Airbnb2)
#hinzufügen von Variablen zu Charakteristika des Hosts
M0.b<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+HostProfilPic+nongerman+verified, data=Airbnb2)
#Um das Modell zu verbessern werden weitere Kontrollvariablen zu Lage und Art der Unterkunft hinzugefügt 
M<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+type+HostProfilPic+instantb+nongerman+verified, data=Airbnb2)
#Die Daten zur Größe der Unterkünfte sind fehlerhaft; deswegen muss ein Proxy verwendet werden
#"guests_included" als Proxy für die Größe:
M1<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+type+guests_included+HostProfilPic+instantb+nongerman+verified, data=Airbnb2)
#"accomodates" als Proxy für die Größe:
M2<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+type+accommodates+HostProfilPic+instantb+nongerman+verified, data=Airbnb2)
stargazer(M1, M2, type="text")
#Beide sind sehr signifikant; Das Modell "M2", das "accomodates enhält, weißt jedoch einen deutlich höheren Adjusted R^2 auf (bei gleicher Variablenzahl)
#Zudem folgt aus der logischen Argumentation, dass "accomodates" der bessere Proxy ist
stargazer(M0, M0.a, M0.b, M, M2, type="text")
#Überprüfen der MLR

#MLR1: Linear in Parameters
#siehe Argumentation
#MLR2: Random Sampling
#siehe Argumentation
#MLR3: No perfect collinearity
vif(M2)
#Da die VIFs von M2 1.76 nicht übersteigen, scheint Mulitcollinearity kein Problem zu sein

#MLR4: Zero Conditional Mean
#siehe Argumentation
#MLR5: Homoskedaszität (Varianz von u für alle xi konstant)
#siehe unten
#MLR6: Normalverteilung des Fehlertermns
#siehe Argumenation


#MLR5: Homoskedazität
bptest(M2)
#Es scheint starke Heteroskedaszität vorzuliegen
#Deswegen
#FGLS-Schätzung des Modells
logu2<-log(resid(M2)^2)
varreg<-lm(logu2~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+type+accommodates+HostProfilPic+instantb+nongerman+verified, data=Airbnb2)
w<-1/exp(fitted(varreg))
M3<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+type+accommodates+HostProfilPic+instantb+nongerman+verified,weight=w, data=Airbnb2)
summary(M3)
stargazer(M2,M3, type="text")
#Im Falle der WLS-Schätzung kann es sein, dass die Funktion, die das Gewicht beschreibt, falsch ist
#deswegen zur Sicherheit nochmal Heteroskedastie-robuste Schätzer
coeftest(M3, vcov=hccm)

#Untersuchung von verschiedenen Subsets
M3.PR<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+accommodates+HostProfilPic+instantb+nongerman+verified,weight=w, subset=(Airbnb2$type=="Private room"), data=Airbnb2)
M3.EHA<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+accommodates+HostProfilPic+instantb+nongerman+verified,weight=w, subset=(Airbnb2$type=="Entire home/apt"), data=Airbnb2)
M3.SH<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+accommodates+HostProfilPic+instantb+nongerman+verified,weight=w, subset=(Airbnb2$type=="Shared room"), data=Airbnb2)
stargazer(M3.PR, M3.EHA, M3.SH, type="text")

#Subsets zu Preis pro Größeneinheit
M3.EX<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+accommodates+HostProfilPic+instantb+nongerman+verified,weight=w, subset=((Airbnb2$price*0.9079/Airbnb2$accommodates)>22.6), data=Airbnb2)
M3.CH<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+accommodates+HostProfilPic+instantb+nongerman+verified,weight=w, subset=((Airbnb2$price*0.9079/Airbnb2$accommodates)<22.6), data=Airbnb2)
stargazer(M3.EX, M3.CH, type="text")

#Subsets zu Hosts
M3.G<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+accommodates+HostProfilPic+instantb+verified,weight=w, subset=(Airbnb2$nongerman=="0"), data=Airbnb2)
M3.F<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+accommodates+HostProfilPic+instantb+verified,weight=w, subset=(Airbnb2$nongerman=="1"), data=Airbnb2)
stargazer(M3.G, M3.F, type="text")
#Ist Koeffizient von HostProfilPic für nicht-deutsche Host signifikant von deutschen Host verschieden?
a <- (exp(0.279)-1)-1.96*0.095
b <- (exp(0.279)-1)+1.96*0.095
a;b
#Ja, ist er
#Ist Koeffizient von Superhost für nicht-deutsche Host signifikant von deutschen Host verschieden?
c <- (exp(0.129)-1)-1.96*0.022
d <- (exp(0.129)-1)+1.96*0.022
c;d
(exp(0.086)-1)
#Ja, ist er

#Funktionale Form: Interaktionsterm für "location" und "nongerman"
M2.a<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+type+accommodates+HostProfilPic+instantb+nongerman+verified+nongerman:location, data=Airbnb2)
summary(M2.a)
#Durch die Homoskedastizität sind die Daten allerdings noch verzerrt. Deswegen muss ein neues FGLS-Modell mit Interaktionsterm geschätzt werden
logu2.a<-log(resid(M2.a)^2)
varreg.a<-lm(logu2~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+type+accommodates+HostProfilPic+instantb+nongerman+verified+nongerman:location, data=Airbnb2)
w.a<-1/exp(fitted(varreg.a))
M3.a<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+type+accommodates+HostProfilPic+instantb+nongerman+verified+nongerman:location,weight=w.a, data=Airbnb2)
summary(M3.a)
linearHypothesis(M3.a, c("locationSpandau+locationSpandau:nongerman1=0"))
#Der gemeinsame Effekt von "locationSpandau" "locationSpandau:nongerman1" ist signifikant von 0 verschieden

#Funktionale Form: Interaktionsterm für "review_scores_rating" und "nongerman"
M2.b<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+type+accommodates+HostProfilPic+instantb+nongerman+verified+nongerman:log(review_scores_rating), data=Airbnb2)
summary(M2.b)
#Durch die Homoskedastizität sind die Daten allerdings noch verzerrt. Deswegen muss ein neues FGLS-Modell mit Interaktionsterm geschätzt werden
logu2.b<-log(resid(M2.b)^2)
varreg.b<-lm(logu2~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+type+accommodates+HostProfilPic+instantb+nongerman+verified+nongerman:log(review_scores_rating), data=Airbnb2)
w.b<-1/exp(fitted(varreg.b))
M3.b<-lm(log(price*0.9079)~log(review_scores_rating)+log(number_of_reviews)+reviewpm+superhost+location+type+accommodates+HostProfilPic+instantb+nongerman+verified+nongerman:log(review_scores_rating),weight=w.b, data=Airbnb2)
summary(M3.b)
linearHypothesis(M3.b, c("log(review_scores_rating)+log(review_scores_rating):nongerman1=0"))
#Der gemeinsame Effekt der beiden Variablen ist signifikant von 0 verschieden
#Koeffizient von log(review_scores_rating) ohne interaktionstermn: 
(exp(M3$coefficients[2])-1)
linearHypothesis(M3.b, c("log(review_scores_rating)+log(review_scores_rating):nongerman1=0.2922"))
M3.b$coefficients[2]+M3.b$coefficients[25]
confint(M3)
#Der Effekt von einem Anstieg des Review scores um 1% ist für Nicht-deutsche Hosts schwächer als für Deutsche


#Überprüfen, ob ich für meine eigene Buchung zu viel gezahlt habe:
cvalues<-data.frame(review_scores_rating=95.8, number_of_reviews=14,reviewpm=1.145, superhost=0, location="Neukölln", type="Private room", accommodates=2, HostProfilPic="1", instantb="f", nongerman="0", verified="f")
exp(predict(M3, cvalues, interval="confidence"))









