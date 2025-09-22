rm(list=ls())
library(foreign)
library(arm)


# salario annuale in migliaia di euro
# livello istruzione in anni di formazione
# livello esperienza in anni di occupazione
# genere 1 se è uomo 0 se è donna


load("dati_salari.RData")
wagedisc$maschio=wagedisc$Sesso

# wagedisc$Sesso <- NULL

attach(wagedisc)

par(mfrow=c(2,2))
boxplot(Salario~Sesso, col=c("pink", "blue"),  medcol = "white",
        xlab=expression("Genere"), ylab=expression("Salari"), xaxt="n")
yrs <- c(1, 2)
axis(1, yrs, tick = T, col.axis = 'black', lab=c("F","M"))

colors <- ifelse (maschio==1, "blue", "pink")
plot(jitter(maschio, 0.8), jitter(Salario,0.8), col=colors, pch=20,xaxt="n",
     xlab="Genere", ylab="Salario")
yrs <- c(0, 1)
axis(1, yrs, tick = T, col.axis = 'black', lab=c("F","M"))


colors <- ifelse (maschio==1, "blue", "pink")
plot(Livello.di.Istruzione, Salario, col=colors, pch=20)

legend("bottomright",               
       legend = c("uomini", "donne"), 
       col = c("blue", "pink"), 
       pch = 19,
       bty="n")


colors <- ifelse (maschio==1, "blue", "pink")
plot(Esperienza, Salario, col=colors, pch=20)

legend("bottomright",               
       legend = c("uomini", "donne"), 
       col = c("blue", "pink"), 
       pch = 19,
       bty="n")

par(mfrow=c(1,1))

hist(Salario[maschio==1], freq=F, xlab="salario", 
     main = "Distribuzione dei salari per genere", col="blue", ylim=c(0,0.30))
lines(density(Salario[maschio==1], kernel="gaussian"), col="blue")


hist(Salario[maschio==0], freq=F, xlab="salario", 
     main = "Distribuzione dei salari per genere", col="pink", ylim=c(0,0.30))
lines(density(Salario[maschio==0], kernel="gaussian"), col="pink")


par(mfrow=c(1,2))

boxplot(Salario~Sesso, col=c("pink", "blue"),  medcol = "white",
        xlab=expression("Genere"), ylab=expression("Salari"), xaxt="n")
yrs <- c(1, 2)
axis(1, yrs, tick = T, col.axis = 'black', lab=c("F","M"))

plot(density(Salario[maschio==1], kernel="gaussian"), col="blue", ylim=c(0,0.30),
     main="distribuzione salari per genere", xlab="salari", lwd=2)
lines(density(Salario[maschio==0], kernel="gaussian"), col="pink", lwd=2)


par(mfrow=c(1,1))


# la distribuzione dei salari è fortemente asimmetrica

# in generale possiamo dire che gli uomini guadagnano più delle donne, la distribuzione
# dei salari degli uomini tende ad assumere valori più alti, la mediana e la media sono più alte e ha
# maggiore variabilità, con un baffo superiore più lungo di quello inferiore, ciò vuol dire 
# che si presentano con maggiore variabilità valori superiori a Q3 piuttosto che valori inferiori
# a Q1. La distribuzione del salario delle donne invece è concentrata su valori più bassi 
# e presenta minore variabilità. Valori di salario che per gli uomini sono considerati 
# nel baffo superiore per le donne sono considerati outliers.
# Dunque vi è un evidente differenza di genere

# la maggior parte delle donne del data set (58 su 69) hanno esperienza lavorativa pari a 0, mentre
# non ci sono uomini con esperienza lavorativa pari a zero.

# in generale le donne guadagnano meno degli uomini a parità di livello di istruzione, soprattutto
# dopo 5 anni di istruzione


mean(Salario) # 22.41
mean(Salario[maschio==1]) #24.11
mean(Salario[maschio==0]) #21.64
# come si poteva notare dal boxplot

table(Sesso)  # 69 donne e 31 uomini
table(Esperienza) # 58 persone con 0 anni di esperienza che sono tutte donne
table(Livello.di.Istruzione)




# è OVVIO CHE LE DONNE HANNO SALARI PIU BASSI PERCHE HANNO ESPERIENZA
# ED ISTRUZIONE PIU BASSA DEGLI UOMINI

# LA MAGGIOR PARTE DELLE PERSONE NON HANNO ISTRUZIONE E LE DONNE HANNO
# ESPERIENZA 0

######################################################
fit.6=lm(Salario~maschio, data=wagedisc)
display(fit.6)
summary(fit.6)


# 2.47 è statisticamente significativo perchè è più lontano di +- 2sigma
# è economicamente significativo perchè dall'analisi dei dati vediamo che
# la distribuzione degli uomini è spostata verso destra rispetto a quella 
# delle donne, cioè gli uomini hanno un salario più elevato

# 2.47 (migliaia di euro) è la differenza di salario tra uomini e donne
# questi 2.47 a cosa sono dovuti? a maggiore esperienza degli uomini?
# a maggiore istruzione degli uomini?

# miglioriamo il modello con il log dei salari

# mi aspetto da questo modello che i residui non siano simmetrici,
# perchè la distribuzione dei salari è fortemente asimmetrica, ecco
# perchè usiamo il log

# usiampo il log per parlare di tasso di crescita

# usiamo il log per garantirci la poistività dell'outcome
#####################################################

# i salari si distribuiscono come una log-normale

fit.1 = lm(log(Salario)~maschio, data=wagedisc)
display(fit.1)
summary(fit.1)


# 0.11 è positivo, in concordanza con quello che mi aspetto guardando i dati


# in media, il log(salario) atteso per una femmina è 3.07, quindi
exp(3.07) # 21.54 è il salario atteso di una donna

# tra un maschio e una femmina ci si aspetta una differenza di salario pari a circa l' 11%
exp(0.11)-1  # 11.62%

# quindi da questo modello si evince una differenza di genere

# i coeff sono statisticamente significativi e anche economicamente, perchè 
# stiamo dicendo che tra un uomo e una donna ci sta il 11% di differena di salari


residui <- resid(fit.1)
hist(residui, breaks=50, freq=F, ylab="densità", main="Istogramma dei residui")  
lines(density(residui))

y.hat<-fit.1$fitted.values
plot (y.hat, residui, xlab="valori stimati", ylab="residui")
abline(h=0)
abline(h=-sigma.hat(fit.1), lty=2) 
abline(h=+sigma.hat(fit.1), lty=2)



##############################################

fit.2 = lm(log(Salario)~maschio+rescale(Livello.di.Istruzione)+rescale(Esperienza), 
           data=wagedisc)

display(fit.2)
summary(fit.2)


#(se le variabili sono categoriche con piu di due categorie vanno messi con factor)

# il coeff di uomini è negativo, ha senso?
# il segno negativo è controintuitivo, e il coeff NON e statisticamente significativo 
# e in piu è praticamente nullo, quindi sebbene sembrerebbe esserci una grande
# differenza di genere, in realtà quando controlliamo per istruzione
# ed esperienza questa disparità si annulla.

# dunqe ha senso fare l'interazione?? SI è L'UNICO MODO PER RISPONDERE ALLA DOMANDA
# perchè se mi interessa la differenza di genere e il coeff di genere non è significativo, allora
# bisogna per forza vederlo con un'interazione

# non posso togliere il coeff di genere, perchè altrimenti come risponderei alla domanda?

# una femmina con livello di istruzione medio e esperienza media ha un log(salario)=3.10
# quindi un salario atteso di 22.197
exp(3.10)

# considerando un maschio e una femmina con liv ist e esperienza media non c'è
# nessuna differenza attesa perchè il coefficiente di maschio è quasi nullo

# il coefficiente del genere non è statisticamente sign

# prese due persone a parità di condizioni, a fronte di una variazione di 2sigma di livello
# di istruzione, ci si aspetta che chi ha 2sigma in più, abbia un salario atteso superiore
# di circa 15.26%
exp(0.1526)-1
2*sd(Livello.di.Istruzione) #7.13


# prese due persone a parità di condizioni, a fronte di una variazione di 2sigma di anni
# di esperienza, ci si aspetta che chi ha 2sigma in più, abbia un salario atteso superiore
# di circa 14.37%
exp(0.1437)-1
2*sd(Esperienza) #4.72

# i coefficienti sono economicamente significativi 
# dal modello si evince che ad avere influenza sul salario sono il livello di istruzione
# e l'esperienza piuttosto che il genere

# ricordiamoci però che nel data set le donne hanno quasi tutte esperienza=0
# e livello di istruzione più basso degli uomini


table(Esperienza,maschio)
table(Livello.di.Istruzione, maschio)



###############################################

fit.3 = lm(log(Salario)~maschio+rescale(Livello.di.Istruzione)+rescale(Esperienza)
           + maschio*rescale(Esperienza), 
           data=wagedisc)

display(fit.3)
summary(fit.3)


# il coeff di interazione è sia alto che statisticamente significativo

# quindi possiamo riformulare la domanda iniziale così:
# QUALE è L'INFLUENZA DELL'ESPERIENZA NEGLI UOMINI E NELLE DONNE?
# è L'ESPERIENZA CHE FA LA DIFFERENZA??
# usiamo il modello: ci sta l'interazione e studiamo l'effetto complessivo dell'esperienza
# effetto complessivo esperienza:
# 0.30-0.16 = 0.14 maschio
# quindi presi due uomini a parità di condizioni, quello che ha 2sigma in più di 
# esperienza in più guadagna circa il 14% in più
# 0.30 femmine
# quindi prese due donne a parità di condizioni, quella che ha 2sigma in più di esperienza
# guadagna circa il 30% in più

# l'influenza dell'esperienza è molto più importante nelle donne che negli uomini
# è quasi il doppio

# il coeff di interazione è molto forte, se avessimo tolto il genere avremmo fatto un errore
# grandissimo


# DUNQUE
# l'istruzione e l'esperienza hanno un effetto positivo sui salari, sono entrambi positivi
# quello dell'esperienza è il doppio (posso dirlo perchè sono sulla stessa scala) 
# di quello dell'istruzione, ecco perchè faccio l'interazione con esperienza


# se facessimo l'interazione tra genere e istruzione vedremo che è zero


# per una donna con esperienza e livello di istruzione medi ci si aspetta
# un log(salario) atteso pari a 3.13, quindi
exp(3.138) #23.057

# effetto complessivo sesso:
# (-0.03-0.16*rescale(esperienza))*maschio

prop.table(table(Esperienza))
quantile(Esperienza)  # mediana = Q1 = 0
quantile(rescale(Esperienza))

table(Esperienza,maschio)  
# ad avere esperienza 0 sono 58 donne e 0 uomini
# ad avere esperienza 1 sono 10 donne e 10 uomini
# ad avere esperienza >2 sono solo uomini

# rescale(0)= -0.2516
# rescale(1)= -0.04


# considero 1 anno esperienza
(-0.03-0.16*(-0.04))
# questo è l'effetto complessivo del genere considerando un esperienza=1
# quindi presi un uomo e una donna a parità di condizioni, entrambi con 1 anno di
# esperienza ci si aspetta un salario atteso degli uomini minore del 2% di quello delle donne


m=mean(Esperienza)
s=sd(Esperienza)

(2-m)/(2*s)  # 0.17
(3-m)/(2*s)  # 0.38
(4-m)/(2*s)  # 0.59

# considero 4 anni esperienza
(-0.03-0.16*(0.59))
# quindi l'effetto complessivo del genere considerando 4 anni di esperienza è -0.12*maschio
# quindi ci si aspetta che gli uomini abbiamo un salario inferiore del 12% rispetto alle donne


# effetto complessivo esperienza:
# 0.30-0.16*maschio
# 0.30 femmine
# quindi considerando due donne a parità di condizioni, quella che ha
# 2sigma in più di esperienza ha un salario più alto di circa il 30%

# 0.14 maschio
# quindi considerando due uomini a parità di condizioni, quello che ha
# 2sigma in più di esperienza ha un salario più alto di circa il 14%



# l'esperienza incide sul salario maggiormente sulle donne che sugli uomini

# dunque -0.16 è la differenza dell'effetto che l'esperienza ha sul salario
# tra uomini e donne 


curve (cbind (1, 1, 0, rescale(x), 1*rescale(x)) %*% coef(fit.3), col="blue",
       ylim=c(2,4), xlim=c(-1,3) ,ylab="log(salario)", xlab="Esperienza riscalata")
curve (cbind (1, 0, 0, rescale(x), 0*rescale(x)) %*% coef(fit.3), col="pink", add=TRUE)

legend("bottomright",               
       legend = c("uomini", "donne"), 
       col = c("blue", "pink"), 
       pch = 19,
       bty="n")

# per esperienza bassa gli uomini guadagnano di più delle donne,
# dopo il contrario, a confermare quello che dice il modello, cioè che 
# l'esperienza ha una fortissima influenza sulle donne invece che sugli uomoni


coefplot(fit.3, intercept=TRUE)
# mar per i margini


residui <- resid(fit.3)
hist(residui, breaks=50, freq=F, ylab="densità", main="Istogramma dei residui")  
lines(density(residui))

y.hat<-fit.3$fitted.values
plot (y.hat, residui, xlab="valori stimati", ylab="residui")
abline(h=0)
abline(h=-sigma.hat(fit.3), lty=2) 
abline(h=+sigma.hat(fit.3), lty=2)


# i residui ci sono più a sinistra che a destra semplicemente perchè
# abbiamo più dati

##Un po' di diagnostica
par(mfrow=c(2,2))
plot(fit.3$fitted.values, fit.3$model[, 1], xlab = "Valori Stimati", ylab = "log(salario)", cex = 0.6)
+ lines(fit.3$fitted.values, fit.3$fitted.values)

plot(fit.3$fitted.values, fit.3$residuals, xlab = "Valori Stimati", ylab = "Residui", cex = 0.6)
abline(h = 0) 
abline(h=-sigma.hat(fit.3), lty=2)
abline(h=+sigma.hat(fit.3), lty=2)

qqnorm(fit.3$residuals, cex = 0.6, main = "", xlab = "Quantili", ylab = "Residui")
qqline(fit.3$residuals)

hist(fit.3$residuals, freq = FALSE, col = "gray", main = "", xlab = "Residui",ylab = "Densita'", ylim=c(0, 10)) 
lines(density(fit.4$residuals))
curve(dnorm(x, 0, sigma.hat(fit.3)), -3 * sigma.hat(fit.3), 4 * sigma.hat(fit.3), add = TRUE, col="red")


##curva d U dell'istruzione (fare con esperienza, segno non atteso e non significativo)
fit.5 <- lm(log(Salario) ~ rescale(Esperienza) + rescale(Livello.di.Istruzione) +  I(rescale(Livello.di.Istruzione)^2) + maschio + rescale(Esperienza):maschio)
summary(fit.5)
display(fit.5)

plot (range(rescale(Esperienza)), range(log(Salario)), xlab="Esperienza (riscalata)", ylab="Salario (in log)", pch=20, type="n", main="")

curve (coef(fit.5)[1] + coef(fit.5)[2] * 0 +coef(fit.5)[3]*rescale(x)+ coef(fit.5)[4] * I(rescale(x)^2)
       + coef(fit.5)[5]*1 + coef(fit.5)[6]*0* rescale(x), col="blue", lty=2, ylim=c(2.8,3.2),
       xlim=c(0,2))

curve (coef(fit.5)[1] + coef(fit.5)[2] * 0 +coef(fit.5)[3]*rescale(x)+ coef(fit.5)[4] * I(rescale(x)^2)
       + coef(fit.5)[5]*0 + coef(fit.5)[6]*0* rescale(x), col="pink", lty=2, ylim=c(2.8,3.2),
       xlim=c(0,2), add=TRUE)


## PREVISIONE

# come si traduce l'importanza dele done sugli uomini?
# vediamo il salario di donne e uomini a parità di condizioni

# Calcolare il salario atteso separatamente per uomini e donne con con 20 anni di
# esperienza e 15 anni di istruzione.

me=mean(Esperienza)
se=sd(Esperienza)

z.esp = (20-me)/(2*se)  #3.97

mi=mean(Livello.di.Istruzione)
si=sd(Livello.di.Istruzione)

z.liv=(15-mi)/(2*si) #1.69


pred.m = coef(fit.3)[1]+coef(fit.3)[2]+coef(fit.3)[3]*z.liv+coef(fit.3)[4]*z.esp+coef(fit.3)[5]*z.esp
pred.m
exp(pred.m)  #50 mila


pred.f = coef(fit.3)[1]+coef(fit.3)[3]*z.liv+coef(fit.3)[4]*z.esp
pred.f
exp(pred.f)  # 99 mila


# vediam ora con 5 anni di esperienza

me=mean(Esperienza)
se=sd(Esperienza)

z.esp = (5-me)/(2*se)  

mi=mean(Livello.di.Istruzione)
si=sd(Livello.di.Istruzione)

z.liv=(15-mi)/(2*si) 


pred.m = coef(fit.3)[1]+coef(fit.3)[2]+coef(fit.3)[3]*z.liv+coef(fit.3)[4]*z.esp+coef(fit.3)[5]*z.esp
pred.m
exp(pred.m)  # 32 mila


pred.f = coef(fit.3)[1]+coef(fit.3)[3]*z.liv+coef(fit.3)[4]*z.esp
pred.f
exp(pred.f)  # 38 mila

# più aumenta l'esperienza più aumeta la differenza tra uomini e donne







######################################################


# curva ad U dell'istruzione

# anche l'esperienza ha tradizionalmente una curva ad U

par(mfrow=c(1,1))
fit.7=lm(log(Salario)~rescale(Esperienza)+rescale(Livello.di.Istruzione)+
                 I(rescale(Esperienza^2)) + maschio+
                 rescale(Esperienza):maschio)
display(fit.7)

# il coef del quadrato è pari a zero quindi la relazione quadratica non ci dice nulla

curve (cbind (1, 0, rescale(x), I(rescale(x)^2), 1, 1*rescale(x)) %*% coef(fit.7), col="blue",
       ylim=c(2,4), xlim=c(-1,3) ,ylab="log(salario)", xlab="Esperienza riscalata")
curve (cbind (1, 0, rescale(x), I(rescale(x)^2), 0, 0*rescale(x)) %*% coef(fit.7), col="pink", add=TRUE)


###########################################################

