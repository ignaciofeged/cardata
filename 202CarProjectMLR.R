###Load Data#######
library(tidyverse)
library(scales)
library(leaps)
library(HH)
library(MASS)
library(olsrr)
library(DAAG)
cardata<-read.csv("/Users/ignaciofeged/Downloads/data 2.csv")
###Clean Data#######
cardata <- cardata[ -c(10) ]
cardata$Make<-as.factor(cardata$Make)
cardata$Model<-as.factor(cardata$Model)
cardata$Engine.Fuel.Type<-as.factor(cardata$Engine.Fuel.Type)
cardata$Transmission.Type<-as.factor(cardata$Transmission.Type)
cardata$Driven_Wheels<-as.factor(cardata$Driven_Wheels)
cardata$Vehicle.Size<-as.factor(cardata$Vehicle.Size)
cardata<-cardata%>%
  filter(Year>2013)
cardata$Engine.Cylinders<-with(cardata,
                               ifelse(`Engine.Fuel.Type`== "electric", 0, 
                                      cardata$Engine.Cylinders))
cardata <- cardata[complete.cases(cardata[ , c('Number.of.Doors', 'Engine.HP',"Engine.Cylinders")]), ] 
cardata$Vehicle.Style<-gsub("2dr Hatchback","Hatchback",cardata$Vehicle.Style)
cardata$Vehicle.Style<-gsub("2dr SUV","SUV",cardata$Vehicle.Style)
cardata$Vehicle.Style<-gsub("4dr SUV","SUV",cardata$Vehicle.Style)
cardata$Vehicle.Style<-gsub("4dr Hatchback","Hatchback",cardata$Vehicle.Style)
cardata$Vehicle.Style<-gsub("Cargo Minivan","Minivan",cardata$Vehicle.Style)
cardata$Vehicle.Style<-gsub("Cargo Van","Van",cardata$Vehicle.Style)
cardata$Vehicle.Style<-gsub("Convertible SUV","Convertible",cardata$Vehicle.Style)
cardata$Vehicle.Style<-gsub("Crew Cab Pickup","Pickup",cardata$Vehicle.Style)
cardata$Vehicle.Style<-gsub("Extended Cab Pickup","Pickup",cardata$Vehicle.Style)
cardata$Vehicle.Style<-gsub("Passenger Minivan","Minivan",cardata$Vehicle.Style)
cardata$Vehicle.Style<-gsub("Passenger Van","Van",cardata$Vehicle.Style)
cardata$Vehicle.Style<-gsub("Regular Cab Pickup","Pickup",cardata$Vehicle.Style)
cardata$Vehicle.Style<-as.factor(cardata$Vehicle.Style)
cardata$Year<-as.numeric(cardata$Year)
cardata$MSRP<-as.numeric(cardata$MSRP)
##Feedback Additions######
cardata$MSRP<-ifelse(cardata$Year==2014,cardata$MSRP*1.30,
                     ifelse(cardata$Year==2015,cardata$MSRP*1.30,
                            ifelse(cardata$Year==2016,cardata$MSRP*1.28,
                                   cardata$MSRP*1.26)))
cardata$Popularity <- rescale(cardata$Popularity)
cardata$Popularity <-cardata$Popularity*100    
cardata$Engine.Fuel.Type<-gsub("electric",NA,cardata$Engine.Fuel.Type)
cardata<-na.omit(cardata)                     
cardata$Engine.Fuel.Type<-as.factor(cardata$Engine.Fuel.Type)

cardata$Vehicle.Style<-ifelse(cardata$Vehicle.Style=="Convertible","Sports_Cars",
                              ifelse(cardata$Vehicle.Style=="Coupe","Sports_Cars",
                                     ifelse(cardata$Vehicle.Style=="Hatchback","Sedan",
                                            ifelse(cardata$Vehicle.Style=="Wagon","SUV",
                                                   ifelse(cardata$Vehicle.Style=="Minivan","Minivan/van",
                                                          ifelse(cardata$Vehicle.Style=="Pickup","Pickup",
                                                                 ifelse(cardata$Vehicle.Style=="Sedan","Sedan",
                                                                        ifelse(cardata$Vehicle.Style=="SUV","SUV",
                                                                               "Minivan/van"))))))))
cardata$Vehicle.Style<-as.factor(cardata$Vehicle.Style)
cardata$highway.MPG<-gsub(354,34,cardata$highway.MPG)
cardata$highway.MPG<-as.numeric(cardata$highway.MPG)
cardata$Number.of.Doors<-as.factor(cardata$Number.of.Doors)

##Potential Variable Changes####
cardata$Engine.Fuel.Type<-ifelse(cardata$Engine.Fuel.Type=="diesel","diesel",
                                 ifelse(cardata$Engine.Fuel.Type=="flex-fuel (premium unleaded recommended/E85)","premium",
                                        ifelse(cardata$Engine.Fuel.Type=="flex-fuel (premium unleaded required/E85)","premium",
                                               ifelse(cardata$Engine.Fuel.Type=="flex-fuel (unleaded/E85)","regular",
                                                      ifelse(cardata$Engine.Fuel.Type=="natural gas","regular",
                                                             ifelse(cardata$Engine.Fuel.Type=="premium unleaded (recommended)","premium",
                                                                    ifelse(cardata$Engine.Fuel.Type=="premium unleaded (required)","premium",
                                                                           "regular")))))))
cardata$Engine.Fuel.Type<-as.factor(cardata$Engine.Fuel.Type)    
summary(cardata$Engine.Fuel.Type)
cardata$Engine.Fuel.Type <- relevel(cardata$Engine.Fuel.Type, ref = "regular")

cardata$Transmission.Type<-ifelse(cardata$Transmission.Type=="AUTOMATIC","AUTOMATIC","MANUAL")
cardata$Transmission.Type<-as.factor(cardata$Transmission.Type)
summary(cardata$Transmission.Type)
cardata$Transmission.Type <- relevel(cardata$Transmission.Type, ref = "AUTOMATIC")

cardata$Driven_Wheels<-gsub("all wheel drive","four wheel drive",cardata$Driven_Wheels)
cardata$Driven_Wheels<-as.factor(cardata$Driven_Wheels)
summary(cardata$Driven_Wheels)
cardata$Driven_Wheels <- relevel(cardata$Driven_Wheels, ref = "four wheel drive")


categorize_make <- function(make) {
  super_luxury <- c("Ferrari", "Lamborghini", "Rolls-Royce", "Aston Martin", "Bentley", "Maserati", "McLaren")
  luxury <- c("BMW", "Mercedes-Benz", "Lexus", "Audi", "Porsche", "Lotus", "Land Rover", "Alfa Romeo", "Cadillac")
  mainstream <- c("Ford", "Chevrolet", "Chrysler", "Dodge", "GMC", "Buick", "Lincoln","FIAT", "Mazda", "Nissan", "Toyota", "Honda", "Hyundai", "Kia", "Volkswagen", "Subaru", "Mitsubishi", "Volvo", "Genesis", "Infiniti", "Acura", "Scion")

  if (make %in% super_luxury) {
    return("Super-Luxury")
  } else if (make %in% luxury) {
    return("Luxury")
  } else if (make %in% mainstream) {
    return("mainstream")
  } else {
    return("ERROR")
  }
}

cardata$make <- sapply(cardata$Make, categorize_make)
cardata$make<-as.factor(cardata$make)
summary(cardata$make)
cardata$make <- relevel(cardata$make, ref = "mainstream")

summary(cardata$Number.of.Doors)
cardata$Number.of.Doors <- relevel(cardata$Number.of.Doors, ref = "4")

summary(cardata$Vehicle.Style)
cardata$Vehicle.Style <- relevel(cardata$Vehicle.Style, ref = "Sedan")
cardata$Vehicle.Size <- relevel(cardata$Vehicle.Size, ref = "Midsize")

##Variable Selection####

# Function to add correlation coefficients
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}

modelsubset<-subset(cardata,select = c(Year, Engine.HP,Engine.Cylinders,highway.MPG,city.mpg,Popularity,MSRP))
pairs(modelsubset,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) # Smoothed regression lines
#muilticolinariy for Engine Hp x cylinders and the 2 mgps--> went with the variable with higher cor. to MSRP
#So Engine Hp and Highway
ggplot(cardata,aes(x=Engine.Fuel.Type,y=MSRP))+
  geom_boxplot()
ggplot(cardata,aes(x=Year,y=MSRP,color=Engine.Fuel.Type))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=Engine.HP,y=MSRP,color=Engine.Fuel.Type))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=highway.MPG,y=MSRP,color=Engine.Fuel.Type))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=Popularity,y=MSRP,color=Engine.Fuel.Type))+
  geom_point()+
  geom_smooth(method="lm") 
#Possibly add interaction term with Year and/or highway.MPG

ggplot(cardata,aes(x=Transmission.Type,y=MSRP))+
  geom_boxplot()
ggplot(cardata,aes(x=Year,y=MSRP,color=Transmission.Type))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=Engine.HP,y=MSRP,color=Transmission.Type))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=highway.MPG,y=MSRP,color=Transmission.Type))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=Popularity,y=MSRP,color=Transmission.Type))+
  geom_point()+
  geom_smooth(method="lm") 
#Possibly add interaction term with highway.MPG

ggplot(cardata,aes(x=Driven_Wheels,y=MSRP))+
  geom_boxplot()
ggplot(cardata,aes(x=Year,y=MSRP,color=Driven_Wheels))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=Engine.HP,y=MSRP,color=Driven_Wheels))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=highway.MPG,y=MSRP,color=Driven_Wheels))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=Popularity,y=MSRP,color=Driven_Wheels))+
  geom_point()+
  geom_smooth(method="lm") 

#Possibly add interaction term with Engine.HP and/or popularity

ggplot(cardata,aes(x=Number.of.Doors,y=MSRP))+
  geom_boxplot()
ggplot(cardata,aes(x=Year,y=MSRP,color=Number.of.Doors))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=Engine.HP,y=MSRP,color=Number.of.Doors))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=highway.MPG,y=MSRP,color=Number.of.Doors))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=Popularity,y=MSRP,color=Number.of.Doors))+
  geom_point()+
  geom_smooth(method="lm") 
#Possibly add interaction term with Engine.HP and/or highway.MPG


ggplot(cardata,aes(x=Vehicle.Size ,y=MSRP))+
  geom_boxplot()
ggplot(cardata,aes(x=Year,y=MSRP,color=Vehicle.Size))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=Engine.HP,y=MSRP,color=Vehicle.Size))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=highway.MPG,y=MSRP,color=Vehicle.Size))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=Popularity,y=MSRP,color=Vehicle.Size))+
  geom_point()+
  geom_smooth(method="lm") 
#Possibly add interaction term with Popularity

ggplot(cardata,aes(x=Vehicle.Style ,y=MSRP))+
  geom_boxplot()
ggplot(cardata,aes(x=Year,y=MSRP,color=Vehicle.Style))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=Engine.HP,y=MSRP,color=Vehicle.Style))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=highway.MPG,y=MSRP,color=Vehicle.Style))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=Popularity,y=MSRP,color=Vehicle.Style))+
  geom_point()+
  geom_smooth(method="lm") 
#Possibly add interaction term with Engine.HP,highway.MPG, and/or popularity

ggplot(cardata,aes(x=make ,y=MSRP))+
  geom_boxplot()
ggplot(cardata,aes(x=Year,y=MSRP,color=make))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=Engine.HP,y=MSRP,color=make))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=highway.MPG,y=MSRP,color=make))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=Popularity,y=MSRP,color=make))+
  geom_point()+
  geom_smooth(method="lm") 
#Possibly add interaction term with Engine.HP and/or highway.MPG

modelsubset2<-subset(cardata,select = c(MSRP, Year, Engine.HP,highway.MPG,Popularity,Engine.Fuel.Type
                                        , Transmission.Type , Driven_Wheels , Number.of.Doors ,
                                        Vehicle.Size, Vehicle.Style , make))
pairs(modelsubset2,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) # Smoothed regression lines
##Initial Model--> no interactions###

intialmodel<-lm(MSRP~Year + Engine.HP + highway.MPG + Popularity +Engine.Fuel.Type
                + Transmission.Type + Driven_Wheels + Number.of.Doors +
                  Vehicle.Size+ Vehicle.Style + make, data = cardata)
summary(intialmodel)


plot(cardata$Year, intialmodel$residuals, pch=19, xlab="Year", ylab="Residuals",
     main="Residual versus X")
abline(h=0, lty=2, col=4, lwd=2)
plot(cardata$Engine.HP, intialmodel$residuals, pch=19, xlab="Engine.HP", ylab="Residuals",
     main="Residual versus X")
abline(h=0, lty=2, col=4, lwd=2)
plot(cardata$highway.MPG, intialmodel$residuals, pch=19, xlab="highway.MPG", ylab="Residuals",
     main="Residual versus X")
abline(h=0, lty=2, col=4, lwd=2)
plot(cardata$Popularity, intialmodel$residuals, pch=19, xlab="Popularity", ylab="Residuals",
     main="Residual versus X")
abline(h=0, lty=2, col=4, lwd=2)

plot(intialmodel$fitted.values, intialmodel$residuals, pch=19, xlab="Predicted Values", ylab="Residuals",
     main="Residual versus Predicted")
abline(h=0, lty=2, col=4, lwd=2)
hist(intialmodel$residuals, main="Histogram of Residuals")
qqnorm(intialmodel$residuals, pch=19)
qqline(intialmodel$residuals)

boxcox(intialmodel)
cardata$MSRP2<-cardata$MSRP^(-1/4)
transintialmodel<-lm(MSRP2~Year + Engine.HP + highway.MPG + Popularity +Engine.Fuel.Type
                     + Transmission.Type + Driven_Wheels + Number.of.Doors +
                       Vehicle.Size+ Vehicle.Style + make, data = cardata)
summary(transintialmodel)
boxcox(transintialmodel)
plot(cardata$Year, transintialmodel$residuals, pch=19, xlab="Year", ylab="Residuals",
     main="Residual versus X")
abline(h=0, lty=2, col=4, lwd=2)
plot(cardata$Engine.HP, transintialmodel$residuals, pch=19, xlab="Engine.HP", ylab="Residuals",
     main="Residual versus X")
abline(h=0, lty=2, col=4, lwd=2)
plot(cardata$highway.MPG, transintialmodel$residuals, pch=19, xlab="highway.MPG", ylab="Residuals",
     main="Residual versus X")
abline(h=0, lty=2, col=4, lwd=2)
plot(cardata$Popularity, transintialmodel$residuals, pch=19, xlab="Popularity", ylab="Residuals",
     main="Residual versus X")
abline(h=0, lty=2, col=4, lwd=2)
plot(cardata$Popularity, transintialmodel$residuals, pch=19, xlab="Popularity", ylab="Residuals",
     main="Residual versus X")
abline(h=0, lty=2, col=4, lwd=2)

plot(transintialmodel$fitted.values, transintialmodel$residuals, pch=19, xlab="Predicted Values", ylab="Residuals",
     main="Residual versus Predicted")
abline(h=0, lty=2, col=4, lwd=2)
hist(transintialmodel$residuals, main="Histogram of Residuals")
qqnorm(transintialmodel$residuals, pch=19)
qqline(transintialmodel$residuals)


##Variable Selection###
lm.lower<-lm(MSRP2~0, data = cardata)
step(transintialmodel,direction = "both",data=cardata)
step(transintialmodel, scope=list(lower=lm.lower, upper=transintialmodel), direction="backward", data=cardata)
step(lm.lower, scope=list(upper=transintialmodel), direction="forward", data=cardata)

ftestDOOR<-lm(MSRP2~Year + Engine.HP + highway.MPG + Popularity +Engine.Fuel.Type
              + Transmission.Type + Driven_Wheels  +
                Vehicle.Size+ Vehicle.Style + make, data = cardata)
anova(ftestDOOR,transintialmodel)

ftestYEAR<-lm(MSRP2~Engine.HP + highway.MPG + Popularity +Engine.Fuel.Type
              + Transmission.Type + Driven_Wheels + Number.of.Doors +
                Vehicle.Size+ Vehicle.Style + make, data = cardata)
anova(ftestYEAR,transintialmodel)

ftestMPG<-lm(MSRP2~Engine.HP  + Popularity +Engine.Fuel.Type
             + Transmission.Type + Driven_Wheels + Number.of.Doors +
               Vehicle.Size+ Vehicle.Style + make, data = cardata)
anova(ftestMPG,transintialmodel)

##Model Evaluation####
vif(transintialmodel) #muilticolinearity concerns with Vehicle.StyleSports_Cars (Sedan) and highway.MPG (Engine.HP)
finalmodel<-lm(MSRP2~Year + Engine.HP + Popularity +Engine.Fuel.Type
               + Transmission.Type + Driven_Wheels + Number.of.Doors +
                 Vehicle.Size + make, data = cardata)
summary(finalmodel)
step(finalmodel,direction = "both",data=cardata)
step(lm.lower, scope=list(upper=finalmodel), direction="forward", data=cardata)

regfit_full = regsubsets(MSRP2~Year + Engine.HP + Popularity +Engine.Fuel.Type
                         + Transmission.Type + Driven_Wheels + Number.of.Doors +
                           Vehicle.Size + make, nvmax=15, data = cardata)
(reg_summary<-summaryHH(regfit_full))

finalrevised<-lm(MSRP2~ Engine.HP + Engine.Fuel.Type
                 + Transmission.Type + Driven_Wheels + make, data = cardata)
summary(finalrevised)

finalrevised2<-lm(MSRP2~ Engine.HP + Engine.Fuel.Type
                  + Popularity + Transmission.Type + make, data = cardata)

#####CONLCUSION MFS#############
summary(finalrevised2)
vif(finalrevised2)
boxcox(finalrevised2)
confint(finalrevised2)

(predict(finalrevised2, newdata=data.frame(Engine.HP=192,Engine.Fuel.Type="regular",Popularity=38,Transmission.Type="AUTOMATIC",make="mainstream"), interval="prediction"))^-4
(confint(finalrevised2))^-4


ggplot(cardata,aes(x=Engine.HP,y=MSRP))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=Popularity,y=MSRP))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=make,y=MSRP))+
  geom_boxplot()
ggplot(cardata,aes(x=Transmission.Type,y=MSRP))+
  geom_boxplot()
ggplot(cardata,aes(x=Engine.Fuel.Type,y=MSRP))+
  geom_boxplot()


ggplot(cardata,aes(x=Engine.HP,y=MSRP,color=make))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(cardata,aes(x=Popularity,y=MSRP,color=Engine.Fuel.Type))+
  geom_point()+
  geom_smooth(method="lm") 


restab<-as_tibble(finalrevised2$residuals)
restab$id<-1:6502
restab
cardata$id<-1:6502

cardata2<-merge(restab,cardata,by="id")

sumtable<-aggregate(cardata2$value,by=list(Make=cardata2$Make), FUN=sum)
sumtable$count<-count(cardata2$Make)

sumtable<-sumtable %>% 
  rename(
    residuals = x,)

ggplot(sumtable,aes(x=fct_infreq(Make),y=residuals))+
  geom_boxplot()


df <- data.frame (Make  = c("Volvo", "Porsche", "Lincoln","Dodge","Kia"),
                  avgresidual = c(-3.896047e-03, -4.304561e-03, -3.535379e-03,3.045257e-03, 1.726381e-03
)
)
ggplot(df,aes(x=Make,y=avgresidual,fill=Make))+
  geom_bar(stat="identity")

df2<-cardata2%>%
  count(Make)

fuckingwork<-merge(sumtable,df2,by="Make")
fuckingwork$avgresidual<-fuckingwork$residuals/fuckingwork$n
