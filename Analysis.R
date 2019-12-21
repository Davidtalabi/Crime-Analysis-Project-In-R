rm(list=ls(all=TRUE))


load("crime.rdata")

ViolentCrimes = x[,c(128)] #Setting variable ViolentCrimes to have the violent crime values


# for col 102 - 118 and 122,123,124,125 and 127, we will fill in NAs with the
# mean value of the non-NA entries in this column
# (imputing the missing values with the mean)
x[is.na(x[,102]),102] = mean(x[,102],na.rm=TRUE)
x[is.na(x[,103]),103] = mean(x[,103],na.rm=TRUE)
x[is.na(x[,104]),104] = mean(x[,104],na.rm=TRUE)
x[is.na(x[,105]),105] = mean(x[,105],na.rm=TRUE)
x[is.na(x[,106]),106] = mean(x[,106],na.rm=TRUE)
x[is.na(x[,107]),107] = mean(x[,107],na.rm=TRUE)
x[is.na(x[,108]),108] = mean(x[,108],na.rm=TRUE)
x[is.na(x[,109]),109] = mean(x[,109],na.rm=TRUE)
x[is.na(x[,110]),110] = mean(x[,110],na.rm=TRUE)
x[is.na(x[,111]),111] = mean(x[,111],na.rm=TRUE)
x[is.na(x[,112]),112] = mean(x[,112],na.rm=TRUE)
x[is.na(x[,113]),113] = mean(x[,113],na.rm=TRUE)
x[is.na(x[,114]),114] = mean(x[,114],na.rm=TRUE)
x[is.na(x[,115]),115] = mean(x[,115],na.rm=TRUE)
x[is.na(x[,116]),116] = mean(x[,116],na.rm=TRUE)
x[is.na(x[,117]),117] = mean(x[,117],na.rm=TRUE)
x[is.na(x[,118]),118] = mean(x[,118],na.rm=TRUE)
x[is.na(x[,122]),122] = mean(x[,122],na.rm=TRUE)
x[is.na(x[,123]),123] = mean(x[,123],na.rm=TRUE)
x[is.na(x[,124]),124] = mean(x[,124],na.rm=TRUE)
x[is.na(x[,125]),125] = mean(x[,125],na.rm=TRUE)
x[is.na(x[,127]),127] = mean(x[,127],na.rm=TRUE)



#model 1.

shufflerows = sample(c(1:nrow(x)))
rowsperfold = floor(nrow(x)/10)
model1mse = numeric(length=10)

for (i in c(1:10))
{
  si = rowsperfold*(i-1) + 1
  ei = rowsperfold*i
  # when i = 1, si:ei = 1:199
  # when i = 2, si:ei = 200:399
  testrows = shufflerows[c(si:ei)]
  trainrows = shufflerows[-c(si:ei)]
  xtrain = x[trainrows,]
  xtest = x[testrows,]
  mymod1 = lm(ViolentCrimesPerPop ~ PctIlleg + MalePctNevMarr, data = xtrain)
  
  ypred = predict(mymod1, newdata=xtest)
  model1mse[i] = mean((ypred - xtest$ViolentCrimesPerPop)^2)
}
model1mse = (mean(model1mse))
model1rse = summary(mymod1)$sigma



malesnevermarried = x[,c(45)] #never married males
parentsnevermarried = x[,c(56)] #parents never married
malenevermarriedanparentsnevermarried = malesnevermarried*parentsnevermarried
Cormalenevermarriedanparentsnevermarriedtoviolentcrime = cor(ViolentCrimes,malenevermarriedanparentsnevermarried)
cat("Model 1 MSE:",model1mse,"Model 1 RSE:",model1rse,"Model 1 Correlation:",Cormalenevermarriedanparentsnevermarriedtoviolentcrime,"(Violent Crime with Unmarried males with unmarried parents)","\n","\n")
plot(ViolentCrimes , malenevermarriedanparentsnevermarried , main="Violent crimes (Unmarried Males and their Parents never married)" , abline(lm(ViolentCrimes~malenevermarriedanparentsnevermarried),lwd=8 ,  col="red"))




#Model2 using percentage of population that doesnt speak enlish and percent of population under poverty line as a predictor for Violent Crime in a population
shufflerows = sample(c(1:nrow(x)))
rowsperfold = floor(nrow(x)/10)
model2mse = numeric(length=10)

for (i in c(1:10))
{
  si = rowsperfold*(i-1) + 1
  ei = rowsperfold*i
  # when i = 1, si:ei = 1:199
  # when i = 2, si:ei = 200:399
  testrows = shufflerows[c(si:ei)]
  trainrows = shufflerows[-c(si:ei)]
  xtrain = x[trainrows,]
  xtest = x[testrows,]
  mymod2 = lm(ViolentCrimesPerPop ~ PctNotSpeakEnglWell + PctPopUnderPov, data = xtrain)
  ypred = predict(mymod2, newdata=xtest)
  model2mse[i] = mean((ypred - xtest$ViolentCrimesPerPop)^2)
}
model2mse = (mean(model2mse))
model2rse = summary(mymod2)$sigma

noenglishpopulation = x[,c(67)] #bad english
populationpctunderpoverty = x[,c(34)] #pct under pov
nonenglishtimesunderpov = noenglishpopulation*populationpctunderpoverty
Corunderpovnoenglish = cor(ViolentCrimes,nonenglishtimesunderpov)
cat("Model 2 MSE:",model2mse,"Model 2 RSE:",model2rse,"Model 2 Correlation:",Corunderpovnoenglish,"(Violent Crime with Non English Speaker times UnderPov)","\n","\n")
plot(ViolentCrimes , nonenglishtimesunderpov , main="Violent crimes (Non English Speaker * Under Pov)" , abline(lm(ViolentCrimes~nonenglishtimesunderpov),lwd=8 ,  col="red"))



#Model3
shufflerows = sample(c(1:nrow(x)))
rowsperfold = floor(nrow(x)/10)
model3mse = numeric(length=10)

for (i in c(1:10))
{
  si = rowsperfold*(i-1) + 1
  ei = rowsperfold*i
  # when i = 1, si:ei = 1:199
  # when i = 2, si:ei = 200:399
  testrows = shufflerows[c(si:ei)]
  trainrows = shufflerows[-c(si:ei)]
  xtrain = x[trainrows,]
  xtest = x[testrows,]
  mymod3 = lm(ViolentCrimesPerPop ~ PctEmploy + PctPopUnderPov, data = xtrain)
  ypred = predict(mymod3, newdata=xtest)
  model3mse[i] = mean((ypred - xtest$ViolentCrimesPerPop)^2)
}
model3mse = (mean(model3mse))
model3rse = summary(mymod3)$sigma

employedpopulation = x[,c(39)] #pct population employed
populationpctunderpoverty = x[,c(34)] #pct under pov
employedtimesunderpov = employedpopulation*populationpctunderpoverty
Coremployedtimesunderpov = cor(ViolentCrimes,employedtimesunderpov)
cat("Model 3 MSE:",model3mse,"Model 3 RSE:",model3rse,"Model 3 Correlation:",Coremployedtimesunderpov,"(Violent Crime with Employed people Under Poverty Line)","\n","\n")
plot(ViolentCrimes , employedtimesunderpov , main="Violent crimes (Employed * Under Pov)" , abline(lm(ViolentCrimes~employedtimesunderpov),lwd=8 ,  col="red"))

shufflerows = sample(c(1:nrow(x)))
rowsperfold = floor(nrow(x)/10)
model4mse = numeric(length=10)

for (i in c(1:10))
{
  si = rowsperfold*(i-1) + 1
  ei = rowsperfold*i
  # when i = 1, si:ei = 1:199
  # when i = 2, si:ei = 200:399
  testrows = shufflerows[c(si:ei)]
  trainrows = shufflerows[-c(si:ei)]
  xtrain = x[trainrows,]
  xtest = x[testrows,]
  mymod2 = lm(ViolentCrimesPerPop ~ PctPopUnderPov + racepctblack, data = xtrain)
  ypred = predict(mymod2, newdata=xtest)
  model4mse[i] = mean((ypred - xtest$ViolentCrimesPerPop)^2)
}
model4mse = (mean(model4mse))
model4rse = summary(mymod2)$sigma

PctPopUnderPov = x[,c(34)]
RacePctBlack = x[,c(8)]
BlackntimesUnderPov=PctPopUnderPov*RacePctBlack
CorPopUnderPovtimesblack = cor(ViolentCrimes,BlackntimesUnderPov)
cat("Model 4 MSE:",model4mse,"Model 4 RSE:",model4rse,"Model 4 Correlation:",CorPopUnderPovtimesblack,"(Violent Crime with Black population and UnderPov)","\n","\n")
plot(ViolentCrimes , BlackntimesUnderPov , main="Violent crimes (Black * Under Pov)" , abline(lm(ViolentCrimes~BlackntimesUnderPov),lwd=8 ,  col="red"))

shufflerows = sample(c(1:nrow(x)))
rowsperfold = floor(nrow(x)/10)
model5mse = numeric(length=10)

for (i in c(1:10))
{
  si = rowsperfold*(i-1) + 1
  ei = rowsperfold*i
  # when i = 1, si:ei = 1:199
  # when i = 2, si:ei = 200:399
  testrows = shufflerows[c(si:ei)]
  trainrows = shufflerows[-c(si:ei)]
  xtrain = x[trainrows,]
  xtest = x[testrows,]
  mymod3 = lm(ViolentCrimesPerPop ~ MalePctNevMarr + pctUrban, data = xtrain)
  ypred = predict(mymod3, newdata=xtest)
  model5mse[i] = mean((ypred - xtest$ViolentCrimesPerPop)^2)
}
model5mse = (mean(model5mse))
model5rse = summary(mymod3)$sigma

PctMaleNeverMarr = x[,c(45)]
PctUrban = x[,c(17)]
NeverMarrtimesUrban=PctMaleNeverMarr*PctUrban
CorNeverMarrtimesUrban = cor(ViolentCrimes,NeverMarrtimesUrban)
cat("Model 5 MSE:",model5mse,"Model 5 RSE:",model5rse,"Model 5 Correlation:",CorNeverMarrtimesUrban,"(Violent Crime with Men never married and Urban Neighborhoods)","\n","\n")
plot(ViolentCrimes , NeverMarrtimesUrban , main="Violent crimes (Never Married * Urban)" , abline(lm(ViolentCrimes~NeverMarrtimesUrban),lwd=8 ,  col="red"))

shufflerows = sample(c(1:nrow(x)))
rowsperfold = floor(nrow(x)/10)
model6mse = numeric(length=10)

for (i in c(1:10))
{
  si = rowsperfold*(i-1) + 1
  ei = rowsperfold*i
  # when i = 1, si:ei = 1:199
  # when i = 2, si:ei = 200:399
  testrows = shufflerows[c(si:ei)]
  trainrows = shufflerows[-c(si:ei)]
  xtrain = x[trainrows,]
  xtest = x[testrows,]
  mymod4 = lm(ViolentCrimesPerPop ~ RentLowQ + PctEmploy, data = xtrain)
  ypred = predict(mymod4, newdata=xtest)
  model6mse[i] = mean((ypred - xtest$ViolentCrimesPerPop)^2)
}
model6mse = (mean(model6mse))
model6rse = summary(mymod4)$sigma

RentLowQuart = x[,c(88)]
PercentEmploy = x[,c(39)]
RentLowtimesPctEmploy=RentLowQuart*PercentEmploy
CorRentLowtimesPctEmploy = cor(ViolentCrimes,RentLowtimesPctEmploy)
cat("Model 6 MSE:",model6mse,"Model 6 RSE:",model6rse,"Model 6 Correlation:",CorRentLowtimesPctEmploy,"(Violent Crime with Low Rent and Employment)","\n","\n")
plot(ViolentCrimes , RentLowtimesPctEmploy , main="Violent crimes (Rent Low * Employed)" , abline(lm(ViolentCrimes~RentLowtimesPctEmploy),lwd=8 ,  col="red"))

#Model7
shufflerows = sample(c(1:nrow(x)))
rowsperfold = floor(nrow(x)/10)
model7mse = numeric(length=10)

for (i in c(1:10))
{
  si = rowsperfold*(i-1) + 1
  ei = rowsperfold*i
  # when i = 1, si:ei = 1:199
  # when i = 2, si:ei = 200:399
  testrows = shufflerows[c(si:ei)]
  trainrows = shufflerows[-c(si:ei)]
  xtrain = x[trainrows,]
  xtest = x[testrows,]
  mymod7 = lm(ViolentCrimesPerPop ~ pctUrban + PolicBudgPerPop, data = xtrain)
  ypred = predict(mymod7, newdata=xtest)
  model7mse[i] = mean((ypred - xtest$ViolentCrimesPerPop)^2)
}
model7mse = (mean(model7mse))
model7rse = summary(mymod7)$sigma

Urban = x[,c(17)] #Percentage of People Living in Urban Areas
Budget = x[,c(127)] #Police Operating Budget per Population
UrbanTimesBudget = Urban*Budget
CorUrbanTimesBudget = cor(ViolentCrimes,UrbanTimesBudget)
cat("Model 7 MSE:",model7mse,"Model 7 RSE:",model7rse,"Model 7 Correlation:",CorUrbanTimesBudget,"(Violent Crime with Police Budget Per Population in Urban Areas)","\n","\n")
plot(ViolentCrimes , UrbanTimesBudget , main="Violent crimes (Urban * Budget)" , abline(lm(ViolentCrimes~UrbanTimesBudget),lwd=8 ,  col="red"))

#Model8
shufflerows = sample(c(1:nrow(x)))
rowsperfold = floor(nrow(x)/10)
model8mse = numeric(length=10)

for (i in c(1:10))
{
  si = rowsperfold*(i-1) + 1
  ei = rowsperfold*i
  # when i = 1, si:ei = 1:199
  # when i = 2, si:ei = 200:399
  testrows = shufflerows[c(si:ei)]
  trainrows = shufflerows[-c(si:ei)]
  xtrain = x[trainrows,]
  xtest = x[testrows,]
  mymod8 = lm(ViolentCrimesPerPop ~ PctWorkMom + PctPopUnderPov, data = xtrain)
  ypred = predict(mymod8, newdata=xtest)
  model8mse[i] = mean((ypred - xtest$ViolentCrimesPerPop)^2)
}
model8mse = (mean(model8mse))
model8rse = summary(mymod8)$sigma

Moms = x[,c(54)] #Percentage of moms of kids under 18 in labor force
UnderPov = x[,c(34)] #pct under pov
MomsTimesUnderPov = Moms*UnderPov
CorMomsTimesUnderPov = cor(ViolentCrimes,MomsTimesUnderPov)
cat("Model 8 MSE:",model8mse,"Model 8 RSE:",model8rse,"Model 8 Correlation:",CorMomsTimesUnderPov,"(Violent Crime with Working Mothers Under Poverty Line)","\n","\n")
plot(ViolentCrimes , MomsTimesUnderPov , main="Violent crimes (Moms * UnderPov)" , abline(lm(ViolentCrimes~MomsTimesUnderPov),lwd=8 ,  col="red"))

#Model9
shufflerows = sample(c(1:nrow(x)))
rowsperfold = floor(nrow(x)/10)
model9mse = numeric(length=10)

for (i in c(1:10))
{
  si = rowsperfold*(i-1) + 1
  ei = rowsperfold*i
  # when i = 1, si:ei = 1:199
  # when i = 2, si:ei = 200:399
  testrows = shufflerows[c(si:ei)]
  trainrows = shufflerows[-c(si:ei)]
  xtrain = x[trainrows,]
  xtest = x[testrows,]
  mymod9 = lm(ViolentCrimesPerPop ~ PersPerRentOccHous + MedRent, data = xtrain)
  ypred = predict(mymod9, newdata=xtest)
  model9mse[i] = mean((ypred - xtest$ViolentCrimesPerPop)^2)
}
model9mse = (mean(model9mse))
model9rse = summary(mymod9)$sigma

Occupants = x[,c(72)] #Mean persons per rental household
Rent = x[,c(91)] #Median gross rent 
OccupantsTimesRent = Occupants*Rent
CorOccupantsTimesRent = cor(ViolentCrimes,OccupantsTimesRent)
cat("Model 9 MSE:",model9mse,"Model 9 RSE:",model9rse,"Model 9 Correlation:",CorOccupantsTimesRent,"(Violent Crime with Mean Persons Per Rental Household and Their Median Gross Rent)","\n")
plot(ViolentCrimes , OccupantsTimesRent , main="Violent crimes (Mean persons per rental household * Median Gross Rent)" , abline(lm(ViolentCrimes~OccupantsTimesRent),lwd=8 ,  col="red"))
