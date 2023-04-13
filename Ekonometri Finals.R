library(readxl)

join1fix_finalproject_ekon_miskin <- read_excel("join1fix_finalproject_ekon_miskin.xlsx")
data <- join1fix_finalproject_ekon_miskin

#data cleaning
data$Binary <- as.factor(data$Binary)
data$provinsi <- as.factor(data$provinsi)
data$jumlah_penduduk <- data$`Jumlah Penduduk (Jiwa) kabkot_x`
data$kepadatan_penduduk <- data$`Kepadatan Penduduk (Jiwa/Km2) KabKot_x`

#statistika deskriptif
summary(data)

counts <- table(data$Binary)
counts
pie_labels <- paste0(round(100 * counts/sum(counts), 2), "%")
pie(counts, labels = pie_labels, main = "Distribusi Variabel Target")
legend("topleft", legend = c("0", "1"),
       fill =  c("white", "lightblue"))


counts2 <- table(data$provinsi)
counts2
barplot(counts2)


#modelbuilding
model<-glm(Binary~provinsi+garis_kemiskinan+pengangguran+pengeluaran_perkapita+
             jumlah_penduduk+kepadatan_penduduk+akses_internet, data=data, family=binomial(link='probit'))
summary(model)
corrplot(model)

modeltest<-update(model,.~.-provinsi)
anova(model,modeltest)

data$pengangguran

modelbaru <- glm(Binary~provinsi+garis_kemiskinan+ 
                   kepadatan_penduduk+akses_internet, data=data, family=binomial(link='probit'))
summary(modelbaru)

#multicollinearity check
library(car)
vif(modelbaru)

#confusion matrix
b<-data$Binary
cm <- table(round(fitted.values(modelbaru)),b)
cm
accuracy<- (1 - (19/119))*100
accuracy 

#prediksi
predict(modelbaru,type='response')[1]






(9.069*10^(-2))+8.912*10^(-6)*298201-4.478*10^(-4)*380-9.342*10^(-2)*28.72124871




PseudoR2(model, which="McFadden")
nullmod <- glm(Binary~1, data=data, family=binomial(link='probit'))
1-logLik(modelbaru)/logLik(nullmod)
1-logLik(model)/logLik(nullmod)

install.packages("DescTools")
library(DescTools)

bptest(modelbaru)
bptest(model)
data$provinsi

data

View(data)
vif(model)

anova(model, test="Chisq")


model<-glm(Binary~provinsi+garis_kemiskinan+pengeluaran_perkapita+
             milik_rumah+kepadatan_penduduk+akses_internet, data=data, family=binomial(link='probit'))
164.961- 80.052
chi 18.30703805

pfit<- fitted.values(model)

a<-round(fitted.values(model))

cbind(join1fix_finalproject_ekon_miskin$Binary,a)
library(caret)

install.packages('caret')
confusionMatrix(join1fix_finalproject_ekon_miskin$Binary,a)

cm <- table(a, b)
a
b<-join1fix_finalproject_ekon_miskin$Binary
cm
1 - 16/119


library(lmtest)
bptest(model)

summary(model)

vif(model)

library(mctest)
install.packages('mctest')

model.vif
