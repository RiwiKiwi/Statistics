# Load the file in
gegevens <- read.csv(file = file.choose(),header = TRUE, dec = ",",sep = ";")
# 500 rijen
#cat("Number of rows before:", nrow(gegevens), "\n")

#verify if it reads correctly
#str(gegevens)

#Studentnummer: s0210700
i <- 7
j <- 0
k <- 0

# verwijder die rijen zoals gezegd op het opgave
rows_to_remove <- c(k+1, j+1, i+1, j*k+1, i*j+1, i*k+1, i*j*k+1, i+j+k+1)
gegevens <- gegevens[-rows_to_remove, ]

cat("Number of rows after:", nrow(gegevens), "\n")

# haal los eruit
los <- gegevens$los
# inplaats van dat ik hele extra codes ga noteren heb ik gwn summary gebruikt om alle spreidingskenmerken te halen
los_summary <- summary(los)
#Minima
los_min <- los_summary["Min."]
# eerste kwartiel
los_q1 <- los_summary["1st Qu."]
# median
los_median <- los_summary["Median"]
# gemiddelde
los_mean <- los_summary["Mean"]
# 3de kwartiel
los_q3 <- los_summary["3rd Qu."]
# maxima
los_max <- los_summary["Max."]

#setup histogram
hist(los,ylab = "#patients",xlab = "Duur ziekenhuisbezoek (in days)",main = "Histogram voor Los")
# setup qqplot
qqnorm(los,main = "QQ-plot voor Los", ylab = "Los (Data quantiles) (ziekenhuisverblijf (in days)) ",xlab = "(Theoretical quantiles)")
qqline(los,col="red")
# Shapiro-Wilk test voor we proberen normale verdeling te benaderen
shapiro_wilk_before <- shapiro.test(los)
shapiro_wilk_before


# TIME TO BENADER NORMALE VERDELING
# pagina 40 van cursus => INVERSE RELATIE VAN DE QQ-PLOT EENS TE ZIEN
qqnorm(los, main = "QQ-plot voor Los", xlab = "Los (Data quantiles) (ziekenhuisverblijf (in days))", ylab = "(Theoretical quantiles)", plot.it = FALSE)
qqpoints <- qqnorm(los, main = "QQ-plot voor Los", xlab = "Los (Data quantiles) (ziekenhuisverblijf (in days))", ylab = "(Theoretical quantiles)", plot.it = FALSE)
plot(qqpoints$y, qqpoints$x, main = "QQ-plot voor Los", xlab = "Los (Data quantiles) (ziekenhuisverblijf (in days))", ylab = "(Theoretical quantiles)")
# Inverse heeft log verband dus ik doe dit (wil geen log(0) cuz undefined)
los_after <- log(los[los!=0])
# SAME ding copy en paste van bove
los_after_summary <- summary(los_after)
los_after_summary
#Minima
los_after_min <- los_after_summary["Min."]
# eerste kwartiel
los_after_q1 <- los_after_summary["1st Qu."]
# median
los_after_median <- los_after_summary["Median"]
# gemiddelde
los_after_mean <- los_after_summary["Mean"]
# 3de kwartiel
los_after_q3 <- los_after_summary["3rd Qu."]
# maxima
los_after_max <- los_after_summary["Max."]

#setup histogram
hist(los_after,ylab = "#patients",xlab = "Duur ziekenhuisbezoek (in days)",main = "Histogram voor Log(Los)")
# setup qqplot
qqnorm(los_after,main = "QQ-plot voor Log(Los)", ylab = "Log(Los) (Data quantiles) (ziekenhuisverblijf (in days)) ",xlab = "(Theoretical quantiles)")
qqline(los_after,col="green")
# Shapiro-Wilk test erna
shapiro_wilk_after <- shapiro.test(los_after)
shapiro_wilk_after

############################################################################# VRAAG 2 ###############################################################################

#levend en geen aanwezigheid van de golven
alive_0 <- nrow(subset(gegevens, dstat == "0" & mitype == "0"))
#levend en wel aanwezigheid van golven
alive_1 <- nrow(subset(gegevens, dstat == "0" & mitype == "1"))
# dood en geen aanwezigheid  van van golven
dead_0 <- nrow(subset(gegevens, dstat == "1" & mitype == "0"))
# dood en  wel aanwezigheid van golven
dead_1 <- nrow(subset(gegevens, dstat == "1" & mitype == "1"))

#2 types vectoren per kolom
alive <- c(alive_0, alive_1)
dead <- c(dead_0, dead_1)

ctable <- data.frame(alive, dead)
rownames(ctable) <- c("Geen aanwezigheid van golven", "Wel aanwezigheid van golven")
#ctable
ChiSq <- chisq.test(ctable)
ChiSq
ChiSq$observed
ChiSq$expected

############################################################################# VRAAG 3 ###############################################################################
# eerst ff kijken of er een lineair verband bestaat ofniet adhv scatterplot grafisch
plot(gegevens$age, gegevens$bmi, main = "Scatterplot of Age && Bmi", xlab = "Age", ylab = "Bmi", col = "blue")
abline(lm(bmi ~ age, data = gegevens), col = "red")

#pearson's corr test
tested <- cor.test(gegevens$age,gegevens$bmi,method = "pearson")
tested

# get the bestfit line by least squares method => In R was da fitting lineair models functie lm()
model <- lm(bmi ~ age, data = gegevens)
# alle gegevens van dit lineaire regression model
model_summary <- summary(model)
print(model_summary)