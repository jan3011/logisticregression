
data<-read.csv(file.choose())
g=glm(data$Admission ~ data$TOEFLScore+data$GREScore,family=binomial)
print(g)
y=data$TOEFLScore
x=data$GREScore
print("Predict")
for(i in 1:length(x))
{
  z=0.009909*y[i]+0.133760*x[i]-43.044983
  if(z<0.5)
    print("Not Admitted")
  else{
    print("admitted")
  }
}

newdat <- data.frame(hp=seq(min(data$TOEFLScore+data$GREScore), max(data$TOEFLScore+data$GREScore),len=500))
newdat$Admission = predict(g, newdata=newdat, type="response")
plot(data$Admission ~ data$TOEFLScore+data$GREScore, col="red4")
ablines(data$Admission ,data$TOEFLScore+data$GREScore,newdat, col="green4", lwd=2)


