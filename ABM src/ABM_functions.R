
######Function for finding the distance between two locations######
#a_x= x coordinates of a 
#a_y= y coordinates of a 
#b_x= x coordinates of b 
#b_y= y coordinates of b 
distance <- function(x1, y1, x2, y2){sqrt((x1-x2)^2 + (y1-y2)^2)}



######Function for determining the angle between two locations######
anglefun <- function(x1,y1,x2,y2,bearing=TRUE,as.deg=TRUE){
  ## calculates the compass bearing of the line between two points
  ## xx and yy are the differences in x and y coordinates between two points
  ## Options:
  ## bearing = FALSE returns +/- pi instead of 0:2*pi
  ## as.deg = TRUE returns degrees instead of radians
  xx<-x1-x2
  yy<-y1-y2
  c = 1
  if (as.deg){
    c = 180/pi
  }
  
  b<-sign(xx)
  b[b==0]<-1  #corrects for the fact that sign(0) == 0
  tempangle = b*(yy<0)*pi+atan(xx/yy)
  if(bearing){
    #return a compass bearing 0 to 2pi
    #if bearing==FALSE then a heading (+/- pi) is returned
    tempangle[tempangle<0]<-tempangle[tempangle<0]+2*pi
  }
  return(tempangle*c)
}





######Function for ripening of fruit######

#het moet een s-curve worden die van 0 naar de 100 gaat. 
#bij x=0 -> y= 1
#y=c/ (1+ae^-bx)
#In 10 dagen bereikt het een waarde van 95  -> 13*4*10 = 540 stappen

#y=bx
#b=100/3120
#in 60 dagen wordt de 100 waarde bereikt. 
#value_to_ripen<-100


ripen<-function(y_value, b_value=(100/steps_to_ripen), ripeningsteps=steps_to_ripen){
  x_value<-NULL
  x_value   <- y_value/b_value
  x_new     <- x_value+1
  
  y_value   <-  b_value*x_new
  
  return(y_value);
}




fruitripening <-function(food, fruits, xloc_food=2,  
                         fruit_dim_creation=1,
                         fruit_dim_ripeness=2,
                         fruit_dim_decay=3,
                         ripeningsteps=steps_to_ripen, 
                         ripe=edibility,
                         ripe_prim=edibilityPrim,
                         unripe_prim= unripe_threshold,
                         fruit_col_agent= 6,
                         fruit_col_prim= 8,
                         fruit_unripe=9){
  
  #ripening dimension. I change the ripening dimenions in the 3d-array first to a vector to then after using the ripening vector back to 2-dimensions
  #this should be done with indexing/logical arrays but don't seem to be able to index with with an logical array of 2 dimensions.
  dropdimension<-as.vector(fruits[,,fruit_dim_ripeness])
  dropdimension[as.vector(fruits[,,fruit_dim_creation]==1)]<-ripen(dropdimension[as.vector(fruits[,,fruit_dim_creation]==1)]) #if thefruit is in the process of being created (i.e. col1 is 1 ) than it should ripen.
  dropdimension<-array(dropdimension,   dim =c(length(fruits[,1,1]),length(fruits[1,,1]))     )
  fruits[,,fruit_dim_ripeness]<-dropdimension
  
  #the total number of fruits that are edible for normal agents, for primate agents and the unripe fruits or memory should be recounted
  #food[,fruit_col_agent] <- apply(fruits[,,fruit_dim_ripeness]>ripe,1, sum)
  food[,fruit_col_prim] <- apply(fruits[,,fruit_dim_ripeness]>ripe_prim,1, sum)
  food[,fruit_unripe] <-apply(fruits[,,fruit_dim_ripeness]>unripe_prim & fruits[,,fruit_dim_ripeness]<ripe_prim,1, sum)
  
  return(list(food,fruits));
}











#decaysteps  number of steps after which the fruit is completely inedibile. Niet meer eetbaar na 4 dagen van het rijp worden. Dus na 13*4*4= 208 stappen
decay<-function(y_value, decaysteps=steps_to_decay){
  a_value  <- (-100/decaysteps);
  b_value  <- 100;
  #formula is y=(-100/208)*x+100
  x_value <- (y_value-b_value)/a_value
  x_new   <- x_value+1 
  y_value <- (a_value*x_new) + b_value
  #y_value<- y_value*(-100/decaysteps)
  
  return(y_value);
}






######Function for decay of fruit######

#decay_col1,2,3, are the column numbers for the first fruit of a tree in the dataframe.
fruitdecay <- function(food, fruits, xloc_food=2, 
                       fruit_dim_creation=1,
                       fruit_dim_ripeness=2,
                       fruit_dim_decay=3,
                       decay_str=decay_start,
                       ripe=edibility,
                       ripe_prim=edibilityPrim,
                       unripe_prim= unripe_threshold,
                       fruit_col_agent= 6,
                       fruit_col_prim= 8,
                       fruit_unripe=9){                                                        
  
  #decay process
  fruitsvectornumbers<-which((fruits[, , fruit_dim_creation]>0 & fruits[, , fruit_dim_ripeness] >= decay_str)==1) #determine which fruits should start the decaying process:  i.e.  if a fruit is in the process of being created (i.e. col1 is 1 ) and after fruit has ripened i.e. 95>
  fruitsdecayvector<-as.vector(fruits[,,fruit_dim_decay]) #make a vector out of the decay levels 
  fruitsdecayvector[fruitsvectornumbers]<-decay(fruitsdecayvector[fruitsvectornumbers]) #the decaying process should is performed for the fruits that need to decay
  fruits[,,fruit_dim_decay]<-matrix(fruitsdecayvector,nrow=nrow(fruits[,,fruit_dim_decay]),ncol=ncol(fruits[,,fruit_dim_decay]))  #the decay dimension of fruits is updated 
  
  #fully rotting
  fruitrottennumber<-which(fruitsdecayvector<=0)   #determine which fruit have rotten and should be reset
  
  fruitcreationvector<-as.vector(fruits[,,fruit_dim_creation])  # the value should become zero since it is not created anymore
  fruitcreationvector[fruitrottennumber]<-0
  
  fruitripenessvector<-as.vector(fruits[,,fruit_dim_ripeness])   # the value should become 1 since it is not ripe anymore
  fruitripenessvector[fruitrottennumber]<-1
  
  fruitsdecayvector[fruitrottennumber]<-100       #the value should become 100 since it has not decayed yet
  
  #this rotting process needs to be updates to the actual fruit data
  fruits[,,fruit_dim_creation]<-matrix(fruitcreationvector,nrow=nrow(fruits[,,fruit_dim_decay]),ncol=ncol(fruits[,,fruit_dim_decay]))  #the decay dimension of fruits is updated 
  fruits[,,fruit_dim_ripeness]<-matrix(fruitripenessvector,nrow=nrow(fruits[,,fruit_dim_decay]),ncol=ncol(fruits[,,fruit_dim_decay]))  #the decay dimension of fruits is updated 
  fruits[,,fruit_dim_decay]<-matrix(fruitsdecayvector,nrow=nrow(fruits[,,fruit_dim_decay]),ncol=ncol(fruits[,,fruit_dim_decay]))  #the decay dimension of fruits is updated 
  
  
  
  
  #food[,fruit_col_agent] <- apply(fruits[,,fruit_dim_ripeness]>ripe,1, sum)
  food[,fruit_col_prim] <- apply(fruits[,,fruit_dim_ripeness]>ripe_prim,1, sum)
  food[,fruit_unripe] <-apply(fruits[,,fruit_dim_ripeness]>unripe_prim & fruits[,,fruit_dim_ripeness]<ripe_prim,1, sum)
  
  return(list(food,fruits));
}








######Function for fruiting of trees######
fruiting <- function(food, fruits, xloc_food=2, 
                     fruit_dim_creation=1,
                     fruit_dim_ripeness=2,
                     fruit_dim_decay=3,
                     regen_col=4, 
                     fruit_prob=5, 
                     regen_counter=7, 
                     ripe=edibility,
                     ripe_prim=edibilityPrim,
                     unripe_prim= unripe_threshold,
                     fruit_col_agent= 6,
                     fruit_col_prim= 8,
                     fruit_unripe=9,
                     total_fruits=10){
  
  food[food[,regen_counter] != 0,  regen_counter]<-food[food[,regen_counter] != 0,  regen_counter]-1 #for each tree, if the counter has not yet reached zero -> then decrease counter with 1
  
  if(sum(food[,regen_counter] == 0)>0){ # if there is at least one tree for which the counter has reached zero

    
    prob.list<-split(cbind( 1-food[food[,regen_counter] == 0,fruit_prob], food[food[,regen_counter] == 0,fruit_prob]), 
                     seq(nrow(  cbind( 1-food[food[,regen_counter] == 0,fruit_prob], food[food[,regen_counter] == 0,fruit_prob]))))   #get for each tree that has reached the moment to start fruits, the probability of the fruits to fruit vs, not-fruit
    
    fruits[food[,regen_counter] == 0,,fruit_dim_creation] <-t(mapply(list(c(0,1)), 
                                                                   size=Fruit_max, replace = TRUE,
                                                                   prob=prob.list,FUN=sample))   #determine for each tree that has reached the moment to start fruits, which fruits will actually fruit. 
    fruits[food[,regen_counter] == 0,,fruit_dim_ripeness] <- 1    #created fruit needs to start to get ripe from start point
    fruits[food[,regen_counter] == 0,,fruit_dim_decay] <- 100
  
    food[food[,regen_counter] == 0,total_fruits]<-food[food[,regen_counter] == 0,total_fruits] + rowSums(fruits[food[,regen_counter] == 0,,fruit_dim_creation, drop=FALSE])
    
    
    food[food[,regen_counter] == 0,regen_counter]<-food[food[,regen_counter] == 0,regen_col]       #for each tree counter that has reached zero, reset the counter. should only be for the ones that have reached 0
    
    #food[,fruit_col_agent] <- apply(fruits[,,fruit_dim_ripeness]>ripe,1, sum)
    food[,fruit_col_prim] <- apply(fruits[,,fruit_dim_ripeness]>ripe_prim,1, sum)
    food[,fruit_unripe] <-apply(fruits[,,fruit_dim_ripeness]>unripe_prim & fruits[,,fruit_dim_ripeness]<ripe_prim,1, sum)
  }
  
  return(list(food,fruits));
}









scalarpropertyfun<- function(y_value,b_value=(100/steps_to_ripen), ripeningsteps = steps_to_ripen, bwaarde=scalarproptimeval, edibile=edibilityPrim){
  x_value<-NULL
  y_difference<-edibilityPrim-y_value
  x_value   <- y_difference/b_value #this is the number of steps until fruit will reach 100
  
  
  
  standaarddev<-0+bwaarde*(ripeningsteps-x_value)    
  
  scalar_value <- x_value-rnorm(n=1, mean=0, sd=standaarddev)
  
  return(scalar_value);
}








# steps_to_forget: #in 90 dagen tijd vergeet de primaat de locatie

#####Function for remembering UNRIPE FRUIT LOCATIONS#####
Remembertemporal <- function(prim_agent, food, fruits, remembered, xloc_agent =2, yloc_agent=3, 
                             xloc_food =2, yloc_food=3, 
                             visdet=visual_det_range, 
                             xmin = env_xmin, xmax=env_xmax, ymin=env_ymin, ymax=env_ymax, 
                             fruit_unripe=9, 
                             remembered_tree_no=2,
                             remembered_tree_x=3, 
                             remembered_tree_y=4, 
                             Steps_to_forget=mem_length_ts,
                             forget_counter=5, 
                             counter_to_ripen_col=8,  
                             ripeningsteps = steps_to_ripen,
                             memories_nr_counter=7,
                             fruit_dim_ripeness=2,
                             nr_unripe_thresh=nr_unripe_threshold, 
                             row_nr_of_fruits=11){
  
  
  
  #DETERMINING DISTANCE TO TREES WITH Unripe FRUITS. Er moeten wel meer dan remember_threshold fruit in de boom zijn 
  treelocations_unripe<-  cbind(matrix(food[    food[,fruit_unripe]>nr_unripe_thresh, 1:yloc_food], ncol = 3),
                              rep(prim_agent[1, xloc_agent], times=sum(food[,fruit_unripe]>nr_unripe_thresh)),
                              rep(prim_agent[1, yloc_agent], times=sum(food[,fruit_unripe]>nr_unripe_thresh)) ) 
  
  
  treedistances_unripe<-cbind( treelocations_unripe[,1], mapply(FUN=distance,treelocations_unripe[,2],treelocations_unripe[,3],treelocations_unripe[,4],treelocations_unripe[,5]))
  treedistances_unripe<-matrix(treedistances_unripe[treedistances_unripe[,2]<visdet],ncol=2) #only get the tree distances of locations within the visual detection range
  
  #closest with unripe food. first row is treenumber, second row is distance
  closesttree_unripe<-treedistances_unripe[which.min(treedistances_unripe[,2]),]
  
  #the first empty remembering-slot should be filled  & the closesttree_unripe should not already be remembered 
  if (length(closesttree_unripe)>0){ #the closettree should actually have a value to begin with
    remembered[match(0, remembered[,remembered_tree_no], nomatch = F)* (!closesttree_unripe[1][[1]] %in% remembered[,remembered_tree_no]), c(2:5,7,8,11)] <- c(closesttree_unripe[1],
                                                                                                                          food[closesttree_unripe[1][[1]],xloc_food],
                                                                                                                          food[closesttree_unripe[1][[1]],yloc_food],
                                                                                                                          Steps_to_forget,                                       #after this number of steps the memory will be forgotten
                                                                                                                          remembered[match(0, remembered[,remembered_tree_no], nomatch = F)* (!closesttree_unripe[1][[1]] %in% remembered[,remembered_tree_no]),  c(7)] + 1,        #count how many memories are placed in each memory slot
                                                                                                                          scalarpropertyfun(max(fruits[closesttree_unripe[1][[1]],,fruit_dim_ripeness])),           #determine how many steps the primagent thinks it will take for the fruit to ripen
                                                                                                                          food[closesttree_unripe[1][[1]],fruit_unripe]) 
  }
                                                                                                                          
  return(remembered)
}  


#####Function for forgetting UNRIPE FRUIT LOCATIONS#####
forgetting <- function(remembered,forget_counter=5, counter_to_ripen_col=8){
  
  #remove automatic dropping dimensions in matrices
  old <- `[`
  `[` <- function(...) { old(..., drop=FALSE) }
  
  remembered[remembered[,forget_counter] > 0,][,forget_counter] <-   remembered[remembered[,forget_counter] > 0,][,forget_counter] -1   #if the forget counter is still higher than 0 than the forget counter should decrease with 1
  remembered[remembered[,forget_counter] == 0,][,c(2:6,8,10,11)] <- matrix(rep(c(0,0,0,0,0,0,1000,0), times= nrow(remembered[remembered[,forget_counter] == 0,])),ncol=8, byrow=T)   #if it is not above 0 the remembered slot should be reset, i.e. the locations should be forgot

  remembered[remembered[,counter_to_ripen_col] > 0,][,counter_to_ripen_col] <-   remembered[remembered[,counter_to_ripen_col] > 0,][,counter_to_ripen_col] -1
  
  
  return(remembered)  
}




#####Function for visually updating memory/discarding memory ##### 
#####Combined with function with making it not a target anymore#####
#if agent sees that a remembered tree does not contain unripe fruit anymore it will discard the memory (or if it sees that the fruit is ripe. Then it will just go to it and eat the fruit.) 
#if agent sees that a remembered tree still contains unripe fruit, it will be in the memory but it will not be a target at that moment and 54 (1day) will be added to the counter to make it less likely that it will immediately make it a target

discard_mem <- function(prim_agent, food, fruits, remembered, 
                        xloc_agent =2, yloc_agent=3,
                        remembered_tree_no=2, 
                        remembered_xloc=3, remembered_yloc=4, 
                        visdet=visual_det_range, fruit_unripe=9, 
                        forget_counter=5, target=6, 
                        counter_to_ripen_col=8, ret_later=return_later,
                        fruit_dim_ripeness=2,
                        yloc_remembered=4){
  treedistance_remembered <- NULL
  
  treelocations_remembered<-  cbind(
    matrix(remembered[remembered[,remembered_tree_no]>0,1:yloc_remembered],ncol = 4),
    rep(prim_agent[1, xloc_agent], times=sum(remembered[,remembered_tree_no]>0)),
    rep(prim_agent[1, yloc_agent], sum(remembered[,remembered_tree_no]>0))) 
  
  
  treedistances_remembered<-cbind( treelocations_remembered[,1], treelocations_remembered[,2],mapply(FUN=distance,treelocations_remembered[,3],treelocations_remembered[,4],treelocations_remembered[,5],treelocations_remembered[,6]))
  
  if(sum(treedistances_remembered[,3]<visdet)>0){ #if there are remembered trees in the slot that are withinin visual distance 
  
    
    remembered[ (food[treedistances_remembered[,2],fruit_unripe]==0)   *  (treedistances_remembered[,1]) , c(2:6,8,10,11)]<-matrix(rep(c(0,0,0,0,0,0,1000,0), 
                                                                                                                                       times=sum(food[treedistances_remembered[,2],fruit_unripe]==0)),ncol=8, byrow=T)  #the remembered locations that contain no unripe fruit anymore, should be reset to not hold the memory
   
    rem_unripe_target<-(food[treedistances_remembered[,2],fruit_unripe]!=0)   *  (treedistances_remembered[,1]) * remembered[treedistances_remembered[,1],target] #the remembered locations that still contain unripe fruit and are at the same time a target
    
    if(sum(rem_unripe_target>0)>0){
      remembered[ rem_unripe_target, counter_to_ripen_col] <-   scalarpropertyfun(apply(fruits[rem_unripe_target,,fruit_dim_ripeness,drop=F], MARGIN =  1, FUN = max, na.rm = T)) #for these locations the agent makes another guess when it will ripe. 
      remembered[ rem_unripe_target, target] <-  0     #these locations become untargeted
    }
  }
  
  return(remembered)  
}






#####Movement of Primate Agent#####
#ripe, when food is counted as ripe for this. if fruit is 95 ripe.
move_primate <- function(prim_agent, food, fruits, remembered, history, timestep,
                         xloc_agent =2, yloc_agent=3, xloc_food =2, yloc_food=3, 
                         fruit_col_agent= 6,
                         fruit_col_prim= 8,
                         fruit_unripe=9, 
                         visdet=visual_det_range, 
                         xmin = env_xmin, xmax=env_xmax, ymin=env_ymin, ymax=env_ymax, 
                         stepmean=stepsize_mean, stepsd=stepsize_sd, 
                         remembered_tree_no=2, 
                         remembered_tree_x=3, remembered_tree_y=4, 
                         eatdist=eat_range, 
                         fruit_dim_creation=1,
                         fruit_dim_ripeness=2,
                         fruit_dim_decay=3,
                         eaten_col =4,
                         ripe=edibility,
                         ripe_prim=edibilityPrim,
                         unripe_prim= unripe_threshold, 
                         target=6,
                         remembered_xloc=3, remembered_yloc=4, forget_counter=5, Steps_to_forget=mem_length_ts, ripe_competitor=edibility, 
                         index_thresh=index_threshold, 
                         counter_to_ripen_col=8, ang_sd= angle_sd){
  
  no_trees <- length(food[,xloc_food]) #get number of trees
  no_remembered <- length(remembered[,1])  #get number of slots in memory
  
  treedistances_ripe <-matrix(ncol=2,nrow=0)
  colnames(treedistances_ripe) <- c("no","distance")
  
  treedistance_remembered <- NULL
  nr_target <- NULL
  distance_target <- NULL
  indices <- cbind("no"=c(NA,NA,NA,NA,NA), "value"= NA, "below_7"= NA, "abs_values"=NA) 
  maxdist <- sqrt(xmax^2+xmax^2)
  chosen_target<-NULL
  
  eaten <- "N"
  visualdetectionused <- "N"
  memory_used <- "N"
  random_moved <- "N"


  #DETERMINING DISTANCE TO TREES WITH FRUITS
  treelocations_ripe<-  cbind(matrix(food[food[,fruit_col_prim]>0,1:yloc_food],ncol = 3),
                              rep(prim_agent[1, xloc_agent], times=sum(food[,fruit_col_prim]>0)),
                                  rep(prim_agent[1, yloc_agent], times=sum(food[,fruit_col_prim]>0)) ) 
                                  

  treedistances_ripe<-cbind( treelocations_ripe[,1], mapply(FUN=distance,treelocations_ripe[,2],treelocations_ripe[,3],treelocations_ripe[,4],treelocations_ripe[,5]))
  
  
  if(nrow(treedistances_ripe)>0){                                                                   #If there are trees with fruits
    closesttree_ripe<-min(treedistances_ripe[,2]);                                                  #get the closer location of these food tree with fruits
    nr_closesttree_ripe<-as.numeric(treedistances_ripe[which.min(treedistances_ripe[,2]),1]);            #and the number of this closest food tree
    
    #First, if there is food in eating distance the agent will eat. Otherwise it will move
    if(closesttree_ripe<eatdist){
      eaten<-"Y"
      fruitcell<-which.max(fruits[nr_closesttree_ripe,,fruit_dim_ripeness])                         #find the the first fruit of the tree with the highest ripeness (which is ripe because of the if statement before)
      #let agent eat 
      fruits[nr_closesttree_ripe, fruitcell, fruit_dim_creation] <-0                                 #agent eats the fruit 
      fruits[nr_closesttree_ripe, fruitcell, fruit_dim_ripeness] <-1                                 #ripening cell gets reset for this fruit 
      fruits[nr_closesttree_ripe, fruitcell, fruit_dim_decay]    <-100 
      
      
      #the total number of fruits that are edible for normal agents, for primate agents and the unripe fruits or memory should be recounted since some fruits have been eaten
      
      #food[nr_closesttree_ripe,fruit_col_agent] <- sum( fruits[nr_closesttree_ripe,,fruit_dim_ripeness]> ripe)
      food[nr_closesttree_ripe,fruit_col_prim] <- sum( fruits[nr_closesttree_ripe,,fruit_dim_ripeness]> ripe_prim)
      food[nr_closesttree_ripe,fruit_unripe] <- sum(fruits[nr_closesttree_ripe,,fruit_dim_ripeness]> unripe_prim & fruits[nr_closesttree_ripe,,fruit_dim_ripeness]< ripe_prim)
      
      prim_agent[1, eaten_col]<- prim_agent[1, eaten_col] + 1                                         #every fruit that is eaten is counted for each agent
    }  
    
    
    ####VISUAL DETECTION PART#
    if(eaten == "N" && closesttree_ripe < visdet){                            #if one of trees is in detection range. It will move towards the closest tree 
      visualdetectionused <- "Y"    
      if((prim_agent[1,yloc_agent]-food[nr_closesttree_ripe,yloc_food]) == 0){                        #if the agent is at the same y coordinate, don't move in y direction. only in x direction towards closest food tree with fruit
        x_move <- closesttree_ripe*0.6
        y_move <- 0
      }else if((prim_agent[1,xloc_agent]-food[nr_closesttree_ripe,xloc_food])==0){                    #if the agent is at the same x coordinate, don't move in x direction. only in x direction towards closest food tree with fruit
        x_move <- 0
        y_move <- closesttree_ripe*0.6
      }else if((prim_agent[1,yloc_agent]-food[nr_closesttree_ripe,yloc_food])!=0 && (prim_agent[1,xloc_agent]-food[nr_closesttree_ripe,xloc_food])!=0){           #if the agent is not on the same y coordinate and neither on the same x coordinate
        angle<-(atan(abs((prim_agent[1,yloc_agent]-food[nr_closesttree_ripe,yloc_food]))/abs((prim_agent[1,xloc_agent]-food[nr_closesttree_ripe,xloc_food]))));
        x_move <-abs(cos(angle)*(closesttree_ripe*0.6));                                                                                  
        y_move <-abs(sin(angle)*(closesttree_ripe*0.6));
      }
      
      if(prim_agent[1,xloc_agent]>food[nr_closesttree_ripe,xloc_food]){       
        x_move<-abs(x_move)*-1                                                            #if the agent is on the right of the closest food location it needs to go to the left, i.e. multiply with -1
      }else{x_move<-abs(x_move)} 
      
      if(prim_agent[1,yloc_agent]>food[nr_closesttree_ripe,yloc_food]){                   #if the agent is above of the closest food location it needs to go to down, i.e. multiply with -1
        y_move<-abs(y_move)*-1
      }else{y_move<-abs(y_move)}
      
      prim_agent[1,xloc_agent] <- prim_agent[1,xloc_agent] + x_move;          
      prim_agent[1,yloc_agent] <- prim_agent[1,yloc_agent] + y_move; 
    }
  }
  
  
  #EPISODIC MEMORY PART1#
  distance_pot_target<- NULL
  steps_to_target<- NULL
  timecounter_index<- NULL
  
  #if nothing has been eaten, no food has been visually detected, no memory is a target yet and if there is more than 0 memories
  if(eaten == "N" && visualdetectionused == "N" && sum(remembered[,target]) == 0 && sum(remembered[,2]) > 0){ 
    for(j in 1:no_remembered){
      if(remembered[j,2]>0){ #if the memory slot is not empty
        distance_pot_target   <- distance(remembered[j,remembered_xloc], remembered[j,remembered_yloc], prim_agent[1, xloc_agent], prim_agent[1, yloc_agent])
        steps_to_target <- round(distance_pot_target/stepmean)
        
        indices[j,1] <- j  
        indices[j,2] <- steps_to_target-remembered[j,counter_to_ripen_col]
        remembered[j,10] <- indices[j,2] #index value is also remembered, as a verification
        
      }else{
        indices[j,1] <- j
        indices[j,2] <- 1000 #if the memory slot is empty give it a high index value of a 1000 in order for it not to become a target
        remembered[j,10] <- indices[j,2] #index value is also remembered, as a verification
      }
    }
    
    indices[,4] <-abs(indices[,2])
    
    #if there are indices below 26(half a day), then make the one closest to 0 a target.
    indices[,3][indices[,4]<index_thresh] <- 1 # first make column indicating whether the values in column 2 are smaller than 26 or not
    indices[,3][indices[,4]>(index_thresh-1)] <- 0
    
    
    if(sum(indices[,3])>0 ){                 # if there are indices below 7
      chosen_target<-which.min(indices[,4])        # make the lowest index value the target( i.e. the one closest to zero, since absolute values are used)
      remembered[chosen_target,target]<-1
      remembered[chosen_target,9]<-remembered[chosen_target,9]+1 #counter to check how many times a memory is made a target
    }
    
  }
  
  #EPISODIC MEMORY PART2#
  if(eaten == "N" && visualdetectionused == "N" && sum(remembered[,target]) == 1){               #if no food is seen & if there are remembered locations & and if one of these has already been targeted
    memory_used <- "Y"    
    nr_target<-which.max(remembered[,target])
    distance_target <- distance(remembered[nr_target,remembered_tree_x], remembered[nr_target,remembered_tree_y], prim_agent[1,xloc_agent], prim_agent[1,yloc_agent])
    
    if((prim_agent[1,yloc_agent]-remembered[nr_target,remembered_tree_y]) == 0){                        #if the agent is at the same y coordinate, don't move in y direction. only in x direction towards remembered location
      if(distance_target>visdet){                                                                       #if the location is far away go with normal speed
        x_move <- rnorm(1, mean = stepmean, sd = stepsd);
      }else{
        x_move <- distance_target*0.6
      }
      y_move <- 0
      
    }else if((prim_agent[1,xloc_agent]-remembered[nr_target,remembered_tree_x]) == 0){
      if(distance_target>visdet){                                                                       #if the location is far away go with normal speed
        y_move <- rnorm(1, mean = stepmean, sd = stepsd);
      }else{
        y_move <- distance_target*0.6
      }
      x_move <- 0
      
    }else if((prim_agent[1,yloc_agent]-remembered[nr_target,remembered_tree_y]) != 0 && (prim_agent[1,xloc_agent]-remembered[nr_target,remembered_tree_x]) != 0){
      angle<-(atan(abs((prim_agent[1,yloc_agent]-remembered[nr_target,remembered_tree_y]))/abs((prim_agent[1,xloc_agent]-remembered[nr_target,remembered_tree_x]))));
      if(distance_target>visdet){  
        x_move <-abs(cos(angle)*(rnorm(1, mean = stepmean, sd = stepsd)));                                                                                  
        y_move <-abs(sin(angle)*(rnorm(1, mean = stepmean, sd = stepsd)));
      }else{
        x_move <-abs(cos(angle)*(distance_target*0.6));                                                                                  
        y_move <-abs(sin(angle)*(distance_target*0.6));
      }
    }
    
    if(prim_agent[1,xloc_agent]>remembered[nr_target,remembered_tree_x]){       
      x_move<-abs(x_move)*-1                                                                #if the agent is on the right of the closest food location it needs to go to the left, i.e. multiply with -1
    }else{x_move<-abs(x_move)} 
    
    if(prim_agent[1,yloc_agent]>remembered[nr_target,remembered_tree_y]){                   #if the agent is above of the closest food location it needs to go to down, i.e. multiply with -1
      y_move<-abs(y_move)*-1
    }else{y_move<-abs(y_move)}
    
    
    prim_agent[1,xloc_agent] <- prim_agent[1,xloc_agent] + x_move;
    prim_agent[1,yloc_agent] <- prim_agent[1,yloc_agent] + y_move;
    
  }
  
  
  
  
  ####RANDOM MOVEMENT PART# 
  #  #If at the last timepoint the agent was at the same location than a random angle is chosen.
  if(is.na(anglefun(history[[timestep - 1]][1,xloc_agent], history[[timestep - 1]][1,yloc_agent], history[[timestep]][1,xloc_agent], history[[timestep]][1,yloc_agent]))){  
    if(eaten == "N" && visualdetectionused == "N" && memory_used == "N"){                                #if no food is seen & memory is not used. Then just move randomly
      random_moved<- "Y"
      #Random movement if there is no food with fruit
      angle_movement<-sample(x=0:360,size=1);
      movementdistance<-rnorm(1, mean = stepmean, sd = stepsd);
      
      x_move <- sin(angle_movement*pi/180)*movementdistance;
      y_move <- cos(angle_movement*pi/180)*movementdistance; 
      
    }
    #  #If at the last timepoint the agent was not at the same location than the angle is partially dependent on the last location
  }else{
    if(eaten == "N" && visualdetectionused == "N" && memory_used == "N"){                                #if no food is seen & memory is not used. Then just move randomly
      random_moved<- "Y"
      #Random movement if there is no food with fruit
      angle_movement<- anglefun(history[[timestep - 1]][1,xloc_agent], history[[timestep - 1]][1,yloc_agent], history[[timestep]][1,xloc_agent], history[[timestep]][1,yloc_agent]) + rtruncnorm(1,a=-359,b=359, mean=0, sd=angle_sd)
      
      movementdistance<-rnorm(1, mean = stepmean, sd = stepsd);
      
      x_move <- sin(angle_movement*pi/180)*movementdistance;
      y_move <- cos(angle_movement*pi/180)*movementdistance; 
      
    }  
  }
  
  if(random_moved == "Y"){
    # =========   The reflecting boundary is added below
    if( (prim_agent[1,xloc_agent] + x_move) > xmax || (prim_agent[1,xloc_agent] + x_move) < xmin){   # If it moved passed the maximum or minimum xloc
      prim_agent[1,xloc_agent] <- prim_agent[1,xloc_agent] - x_move; # Then just do x_move in the negative
    }else{prim_agent[1,xloc_agent] <- prim_agent[1,xloc_agent] + x_move}
    
    if( (prim_agent[1,yloc_agent] + y_move) > ymax || (prim_agent[1,yloc_agent] + y_move) < ymin){  # If it moved passed the maximum or minimum yloc
      prim_agent[1,yloc_agent] <- prim_agent[1,yloc_agent] - y_move; # Then just do x_move in the negative
    }else{prim_agent[1,yloc_agent] <- prim_agent[1,yloc_agent] + y_move}
    
  }
  
  return(list(prim_agent,food,fruits,remembered))                                                                    #R cannot return multiple dataframes from a function. Therefore making a list with 2 dataframes.;
}











#####Movement of Primate Agent#####
#ripe, when food is counted as ripe for this. if fruit is 95 ripe.
move_primate_no_mem <- function(prim_agent, food, fruits, remembered, history, timestep,
                         xloc_agent =2, yloc_agent=3, xloc_food =2, yloc_food=3, 
                         fruit_col_agent= 6,
                         fruit_col_prim= 8,
                         fruit_unripe=9, 
                         visdet=visual_det_range, 
                         xmin = env_xmin, xmax=env_xmax, ymin=env_ymin, ymax=env_ymax, 
                         stepmean=stepsize_mean, stepsd=stepsize_sd, 
                         remembered_tree_no=2, 
                         remembered_tree_x=3, remembered_tree_y=4, 
                         eatdist=eat_range, 
                         fruit_dim_creation=1,
                         fruit_dim_ripeness=2,
                         fruit_dim_decay=3,
                         eaten_col =4,
                         ripe=edibility,
                         ripe_prim=edibilityPrim,
                         unripe_prim= unripe_threshold, 
                         target=6,
                         remembered_xloc=3, remembered_yloc=4, forget_counter=5, Steps_to_forget=mem_length_ts, ripe_competitor=edibility, 
                         index_thresh=index_threshold, 
                         counter_to_ripen_col=8, ang_sd= angle_sd){
  
  no_trees <- length(food[,xloc_food]) #get number of trees
  no_remembered <- length(remembered[,1])  #get number of slots in memory
  
  treedistances_ripe <-matrix(ncol=2,nrow=0)
  colnames(treedistances_ripe) <- c("no","distance")
  
  treedistance_remembered <- NULL
  nr_target <- NULL
  distance_target <- NULL
  indices <- cbind("no"=c(NA,NA,NA,NA,NA), "value"= NA, "below_7"= NA, "abs_values"=NA) 
  maxdist <- sqrt(xmax^2+xmax^2)
  chosen_target<-NULL
  
  eaten <- "N"
  visualdetectionused <- "N"
  memory_used <- "N"
  random_moved <- "N"
  
  
  #DETERMINING DISTANCE TO TREES WITH FRUITS
  treelocations_ripe<-  cbind(matrix(food[food[,fruit_col_prim]>0,1:yloc_food],ncol = 3),
                              rep(prim_agent[1, xloc_agent], times=sum(food[,fruit_col_prim]>0)),
                              rep(prim_agent[1, yloc_agent], times=sum(food[,fruit_col_prim]>0)) ) 
  
  
  treedistances_ripe<-cbind( treelocations_ripe[,1], mapply(FUN=distance,treelocations_ripe[,2],treelocations_ripe[,3],treelocations_ripe[,4],treelocations_ripe[,5]))
  
  
  if(nrow(treedistances_ripe)>0){                                                                   #If there are trees with fruits
    closesttree_ripe<-min(treedistances_ripe[,2]);                                                  #get the closer location of these food tree with fruits
    nr_closesttree_ripe<-as.numeric(treedistances_ripe[which.min(treedistances_ripe[,2]),1]);            #and the number of this closest food tree
    
    #First, if there is food in eating distance the agent will eat. Otherwise it will move
    if(closesttree_ripe<eatdist){
      eaten<-"Y"
      fruitcell<-which.max(fruits[nr_closesttree_ripe,,fruit_dim_ripeness])                         #find the the first fruit of the tree with the highest ripeness (which is ripe because of the if statement before)
      #let agent eat 
      fruits[nr_closesttree_ripe, fruitcell, fruit_dim_creation] <-0                                 #agent eats the fruit 
      fruits[nr_closesttree_ripe, fruitcell, fruit_dim_ripeness] <-1                                 #ripening cell gets reset for this fruit 
      fruits[nr_closesttree_ripe, fruitcell, fruit_dim_decay]    <-100 
      
      
      #the total number of fruits that are edible for normal agents, for primate agents and the unripe fruits or memory should be recounted since some fruits have been eaten
      
      #food[nr_closesttree_ripe,fruit_col_agent] <- sum( fruits[nr_closesttree_ripe,,fruit_dim_ripeness]> ripe)
      food[nr_closesttree_ripe,fruit_col_prim] <- sum( fruits[nr_closesttree_ripe,,fruit_dim_ripeness]> ripe_prim)
      food[nr_closesttree_ripe,fruit_unripe] <- sum(fruits[nr_closesttree_ripe,,fruit_dim_ripeness]> unripe_prim & fruits[nr_closesttree_ripe,,fruit_dim_ripeness]< ripe_prim)
      
      prim_agent[1, eaten_col]<- prim_agent[1, eaten_col] + 1                                         #every fruit that is eaten is counted for each agent
    }  
    
    
    ####VISUAL DETECTION PART#
    if(eaten == "N" && closesttree_ripe < visdet){                            #if one of trees is in detection range. It will move towards the closest tree 
      visualdetectionused <- "Y"    
      if((prim_agent[1,yloc_agent]-food[nr_closesttree_ripe,yloc_food]) == 0){                        #if the agent is at the same y coordinate, don't move in y direction. only in x direction towards closest food tree with fruit
        x_move <- closesttree_ripe*0.6
        y_move <- 0
      }else if((prim_agent[1,xloc_agent]-food[nr_closesttree_ripe,xloc_food])==0){                    #if the agent is at the same x coordinate, don't move in x direction. only in x direction towards closest food tree with fruit
        x_move <- 0
        y_move <- closesttree_ripe*0.6
      }else if((prim_agent[1,yloc_agent]-food[nr_closesttree_ripe,yloc_food])!=0 && (prim_agent[1,xloc_agent]-food[nr_closesttree_ripe,xloc_food])!=0){           #if the agent is not on the same y coordinate and neither on the same x coordinate
        angle<-(atan(abs((prim_agent[1,yloc_agent]-food[nr_closesttree_ripe,yloc_food]))/abs((prim_agent[1,xloc_agent]-food[nr_closesttree_ripe,xloc_food]))));
        x_move <-abs(cos(angle)*(closesttree_ripe*0.6));                                                                                  
        y_move <-abs(sin(angle)*(closesttree_ripe*0.6));
      }
      
      if(prim_agent[1,xloc_agent]>food[nr_closesttree_ripe,xloc_food]){       
        x_move<-abs(x_move)*-1                                                            #if the agent is on the right of the closest food location it needs to go to the left, i.e. multiply with -1
      }else{x_move<-abs(x_move)} 
      
      if(prim_agent[1,yloc_agent]>food[nr_closesttree_ripe,yloc_food]){                   #if the agent is above of the closest food location it needs to go to down, i.e. multiply with -1
        y_move<-abs(y_move)*-1
      }else{y_move<-abs(y_move)}
      
      prim_agent[1,xloc_agent] <- prim_agent[1,xloc_agent] + x_move;          
      prim_agent[1,yloc_agent] <- prim_agent[1,yloc_agent] + y_move; 
    }
  }
  
  ####RANDOM MOVEMENT PART# 
  #  #If at the last timepoint the agent was at the same location than a random angle is chosen.
  if(is.na(anglefun(history[[timestep - 1]][1,xloc_agent], history[[timestep - 1]][1,yloc_agent], history[[timestep]][1,xloc_agent], history[[timestep]][1,yloc_agent]))){  
    if(eaten == "N" && visualdetectionused == "N" && memory_used == "N"){                                #if no food is seen & memory is not used. Then just move randomly
      random_moved<- "Y"
      #Random movement if there is no food with fruit
      angle_movement<-sample(x=0:360,size=1);
      movementdistance<-rnorm(1, mean = stepmean, sd = stepsd);
      
      x_move <- sin(angle_movement*pi/180)*movementdistance;
      y_move <- cos(angle_movement*pi/180)*movementdistance; 
      
    }
    #  #If at the last timepoint the agent was not at the same location than the angle is partially dependent on the last location
  }else{
    if(eaten == "N" && visualdetectionused == "N" && memory_used == "N"){                                #if no food is seen & memory is not used. Then just move randomly
      random_moved<- "Y"
      #Random movement if there is no food with fruit
      angle_movement<- anglefun(history[[timestep - 1]][1,xloc_agent], history[[timestep - 1]][1,yloc_agent], history[[timestep]][1,xloc_agent], history[[timestep]][1,yloc_agent]) + rtruncnorm(1,a=-359,b=359, mean=0, sd=angle_sd)
      
      movementdistance<-rnorm(1, mean = stepmean, sd = stepsd);
      
      x_move <- sin(angle_movement*pi/180)*movementdistance;
      y_move <- cos(angle_movement*pi/180)*movementdistance; 
      
    }  
  }
  
  if(random_moved == "Y"){
    # =========   The reflecting boundary is added below
    if( (prim_agent[1,xloc_agent] + x_move) > xmax || (prim_agent[1,xloc_agent] + x_move) < xmin){   # If it moved passed the maximum or minimum xloc
      prim_agent[1,xloc_agent] <- prim_agent[1,xloc_agent] - x_move; # Then just do x_move in the negative
    }else{prim_agent[1,xloc_agent] <- prim_agent[1,xloc_agent] + x_move}
    
    if( (prim_agent[1,yloc_agent] + y_move) > ymax || (prim_agent[1,yloc_agent] + y_move) < ymin){  # If it moved passed the maximum or minimum yloc
      prim_agent[1,yloc_agent] <- prim_agent[1,yloc_agent] - y_move; # Then just do x_move in the negative
    }else{prim_agent[1,yloc_agent] <- prim_agent[1,yloc_agent] + y_move}
    
  }
  
  return(list(prim_agent,food,fruits,remembered))                                                                    #R cannot return multiple dataframes from a function. Therefore making a list with 2 dataframes.;
}















#Compiling all functions
distance <- cmpfun(distance)
anglefun <- cmpfun(anglefun)
ripen <- cmpfun(ripen)
fruitripening <- cmpfun(fruitripening)
fruitdecay <- cmpfun(fruitdecay)
fruiting <- cmpfun(fruiting)
scalarpropertyfun <- cmpfun(scalarpropertyfun)
decay <- cmpfun(decay)
Remembertemporal <- cmpfun(Remembertemporal)
forgetting <- cmpfun(forgetting)
discard_mem <- cmpfun(discard_mem)
move_primate <- cmpfun(move_primate)
move_primate_no_mem <- cmpfun(move_primate_no_mem)


