# zo open je dus csv files
#gegevens <- read.csv(file = file.choose(),header = TRUE, dec = ",",sep = ";")

# zo lees je txt bestand
#gegevens <- read.table(file = file.choose(),header = TRUE)

# aanmaken van het dataset
# kleur <- c("D","D","L","L","R","R")
# geslacht <- c("M","V","M","V","M","V")
# aantal <- c(9,14,12,11,3,7)
# gegevens <- data.frame(kleur,geslacht,aantal)
# gegevens

#save stuk  code door
#save(kleur,gegevens,file = "naam.Ragain.data")




#PROBLEEM 1 => scatterplot maken om verband te zien tussen scores van middelbar (dependent) met 1ste ba (independent)

# laad de txt file
gegevens <- read.table(file=file.choose(),header=TRUE)
# score van 1BA als y en score van middelbaar als x as
#plot(gegevens$middelbaar,gegevens$bachelor,type = "p",xlab = "Score middelbaaar",ylab = "Score 1ste bachelor",main = "Score 1e bachelor = f(Score  middelbaar)")

#PROBLEEM 2
# Ga na of jongens groter zijn dan meisjes door de mediaan en het gemiddelde te berekenen van de variabele lengte

lengteM <- gegevens$lengte[gegevens$geslacht=="M"] # lengte van mannen

meanM <- mean(lengteM)
medM <- median(lengteM)

lengteV <- gegevens$lengte[gegevens$geslacht=="V"] # lengte van vrouwen

meanV <- mean(lengteV)
medV <- median(lengteV)

# Antwoord voor probleem 2
print(paste("Mean of mannen ",toString(meanM)))
print(paste("Median of mannen ",toString(medM)))
print(paste("Mean of vrouwen ",toString(meanV)))
print(paste("Median of vrouwen ",toString(medV)))

#aanmaken van histogram
par(mfcol=c(1,2)) # maakt matrix van 1 rij en 2 kolommen
h_M <- hist(lengteM)
h_V <- hist(lengteV)

# PROBLEM 3
# Bereken de variantie, het maximum en het minimum, de range (maximum - minimum), IQR
# (Inter Quartile Range = verschil tussen 75% en 25% kwantiel) en de standaarddeviatie voor
# de variabele lengte in het algemeen.

lengte <- gegevens$lengte

var_lengte <- var(lengte) # variantie
min_max_lengte <- range(lengte) # waar max = min_max_lengte[2] en voor min [1]
range_lengte <- min_max_lengte[2] - min_max_lengte[1] # range
iqr_lengte <- IQR(lengte) # IQR AFSTAND
std_dev_lengte <- sd(lengte) # STANDARD DEVIATIE

# ANTWOORD VOOR 3
print(paste("Variantie van lengte",toString(var_lengte)))
print(paste("Maximum van lengte",toString(min_max_lengte[2])))
print(paste("Minimum van lengte ",toString(min_max_lengte[1])))
print(paste("Range van lengte",toString(range_lengte)))
print(paste("IQR van lengte ",toString(iqr_lengte)))
print(paste("Standard Deviation van lengte",toString(std_dev_lengte)))


# AANMAKEN VAN BOXPLOT
boxplot(lengte,ylab="lengte(in cm)",main="Boxplot van de variabele lengte")

# BOXPLOT VOOR LENGTE VAN JONGENS EN MEISJES
boxplot(list(lengteM,lengteV),ylab="lengte (in cm)",names= c("jongens","meisjes"), main="Boxplot van de lengte van de jongens en de meisjes apart")

# PROBLEM  4

verschil_ba_mid <- gegevens$middelbaar - gegevens$bachelor

mean_vershil_ba_mid <- mean(verschil_ba_mid)
med_verschil_ba_mid <- median(verschil_ba_mid)

#mean en median voor verschil
print(paste("Mean of verschil",toString(mean_vershil_ba_mid)))
print(paste("Median of verschil",toString(mean_vershil_ba_mid)))
#boxplot maken voor het verschil tussen die 2
boxplot(verschil_ba_mid,ylab = "",main = "Boxplot voor vershil tussen middelbaar en first bachelor")


get_bachelor <- gegevens$bachelor

mean_bach <- mean(get_bachelor)
med_bach <- median(get_bachelor)

print(paste("Mean of bachelor",toString(mean_bach)))
print(paste("Median of bachelor",toString(med_bach)))


richting_b <- gegevens$bachelor[gegevens$studierichting == "B"]
boxplot(richting_b,ylab = "cijfer voor b",main= "Boxplot voor richting B")
richting_s <- gegevens$bachelor [gegevens$studieriching == "S"]
boxplot(richting_b,ylab = "cijfer voor s",main= "Boxplot voor richting S")
richting_g <- gegevens$bachelor [gegevens$studieriching == "G"]
boxplot(richting_b,ylab = "cijfer voor g",main= "Boxplot voor richting G")