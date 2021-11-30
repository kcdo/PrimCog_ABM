########Testing file#######



#Basic checks---------------------------------
#Checking if code functions/has functioned properly

#Check1---------------------------------
#####checks if the total number of fruits eaten does not exceed the total number of fruits created. (After running abm)
if(sum(Trees[,10]) > (Primate_agent[1,4])){"Nice"}else{"Problem!"} #total number of fruits created - total number of fruits eaten. 


#Check2---------------------------------
#####check if scalarproperty function is functioning properly.#y_values gaan tot max 100.
high_b<-NULL
low_b<-NULL
zero_b<-NULL
for(i in 1:10000){
  high_b<-c(high_b,scalarpropertyfun(y_value=30, b_value=(100/3120),ripeningsteps = 3120 ,bwaarde=0.2))
  low_b<-c(low_b,scalarpropertyfun(y_value=30, b_value=(100/3120),ripeningsteps = 3120 , bwaarde=0.1))
  zero_b<-c(zero_b,scalarpropertyfun(y_value=30, b_value=(100/3120),ripeningsteps = 3120 , bwaarde=0.0))
}

# de spreiding zou hoger moeten zijn wanneer er hogere b_waardes worden gebruikt. 
if(sd(high_b)>sd(low_b)){"Nice"}else{"Problem!"}

#de spreading zou nul moeten zijn bij b_waarde van 0
if(sd(zero_b)==0){"Nice"}else{"Problem!"}



#Check3---------------------------------
#####check if scalarproperty function is functioning properly. if y-value is 95 it should be almost zero 
if(round(scalarpropertyfun(y_value=95,bwaarde=0.0), digits = 6)==0){"Nice"}else{"Problem!"}




#Check4---------------------------------
#####check if decay function works properly
valueA<-100
valueB<-100
valueC<-100
for(i in 1:10){
  valueA<-decay(valueA,decaysteps=10)
  valueB<-decay(valueB,decaysteps=8)
  valueC<-decay(valueC,decaysteps=12)
}
if(valueA==0){"Nice"}else{"Problem!"}
if(valueB<0){"Nice"}else{"Problem!"}
if(valueC>0){"Nice"}else{"Problem!"}





#Check5---------------------------------
#####check if fruitdecay function works properly

#create dataset with only 4 trees and max 10 fruits
nTree<-5
Fruit_max<-10
FruitC<-matrix(rep(0,nTree*Fruit_max), nrow=nTree, ncol=Fruit_max) #fruit creating
FruitC[3,]<-c(0,1,1,0,1,0,1,1,0,1)
FruitC[4,]<-c(0,1,1,0,1,0,1,1,0,1)

FruitR<- matrix(rep(1,nTree*Fruit_max), nrow=nTree, ncol=Fruit_max) #ripening
FruitR[3,]<-c(1,95,95,1,95,1,95,95,1,95)
FruitR[4,]<-c(1,95,95,1,95,1,95,95,1,95)
FruitD<- matrix(rep(100,nTree*Fruit_max), nrow=nTree, ncol=Fruit_max) #decay

#this one should be deleted
FruitD[3,]<-c(100,0,0,100,0,100,0,0,100,0)
Fruits<- array(c(FruitC,FruitR,FruitD), c(nTree,Fruit_max,3))

Fruit_p= rtruncnorm(1, a=0,b=1, mean = Fruit_p_mean, sd=Fruit_p_sd)
Trees<-NULL

######CREATING Food_trees######
#Create Trees throughout map
for(i in 1:nTree){
  Fruit_p= rtruncnorm(1, a=0,b=1, mean = Fruit_p_mean, sd=Fruit_p_sd)
  Tree2 <- data.frame(TreeNo = i, Xcoord = runif(1,env_xmin,env_xmax),Ycoord= runif(1,env_ymin,env_ymax), Regenaration=round(rnorm(1, mean =  AverageRegenSteps, sd=SdRegenSteps)), 
                      fruit_prob = Fruit_p, Fruit_total_90= 0, Regen_counter=0, Fruit_total_95=NA, Unripe=NA,Fruits_created=0)
  Tree2[1,7] <- Tree2[1,4]-sample(x=1:Tree2[1,4],1,replace=T)
  Trees <- rbind(Trees, Tree2)
}

decaylist<-fruitdecay(Trees,Fruits)
Trees <- decaylist[[1]]
Fruits <- decaylist[[2]]
if(sum(Fruits[4,,3])<1000){"Nice"}else{"Problem, the decaying process works!"} 
if(sum(Fruits[3,,3])==1000){"Nice"}else{"Problem, if the fruit has fully decayed it does not get removed/updated"} 

