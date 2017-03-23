#We used lm() linear regression model to fill in missing values, and were found to be more precise. 

e2<-as.numeric(gsub(",","",GDP[,2])) # convert to numeric
e4<-GDP[,4][!is.na(GDP[,4])] # removing NAs
e4<-as.numeric(gsub(",","",e4)) # convert to numeric

w1<-as.numeric(gsub(",","",GDP[,3])) #convert to numeric
cor(w1[1:61],e4) # 0.9998814
#So,
q<-lm(e4~w1[1:61],GDP) 
z<-coef(q)[1]+coef(q)[2]*w1[62] # 639357.8

w9<-GDP[,9] #convert to numeric
w10<-GDP[,10][!is.na(GDP[,10])]
cor(w2[1:61],w3) #0.9992064
#So,
model<-lm(w3~w2[1:61],GDP) 
z<-coef(model)[1]+coef(model)[2]*w2[62] # 11.89985

w16<-GDP[,16] #convert to numeric
w17<-GDP[,17][!is.na(GDP[,17])]
cor(w16[1:61],w17) #0.997642
#So,
model1<-lm(w17~w16[1:61],GDP) 
z<-coef(model)[1]+coef(model1)[2]*w16[62] # 2.980251