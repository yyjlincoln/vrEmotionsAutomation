setwd("{{{PROJECT_ROOT}}}/Subject{{{subjectNumber}}}_Session{{{sessionNumber}}}/IMU_Right")


imuData <- data.frame()

for (i in c(0:{{{numOfLogFilesLessOne}}})){

    snippet<- read.csv(file = paste("LOG",i,".csv",sep=""))

    names(snippet)<- c("TimeStamps","aX","aY","aZ","gX","gY","gZ","mX","mY","mZ","p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","p11","p12","p13","p14","p15","p16")

    imuData <- rbind(imuData,snippet)
    print(i)
}

setwd("{{{PROJECT_ROOT}}}/Subject{{{subjectNumber}}}_Session{{{sessionNumber}}}")

startendTimes <- data.frame()

for (i in c(1:{{{numOfRuns}}})){

  part<- read.csv(file = paste("Subject{{{subjectNumber}}}_Session{{{sessionNumber}}}_Run",i,".csv",sep=""))

  startendTimes <- rbind(startendTimes,part)
  print(i)
}


MetaData<-read.csv("{{{PROJECT_ROOT}}}/MetaData.csv")

observations <- c()
for(val in startendTimes$Run  ){

  offset1<-startendTimes$snippet.started[val] - {{{xStartTimestamp}}}

  run1start<-imuData$TimeStamps[1]+offset1*1000


  offset2 <- startendTimes$snippet.stopped[val] - {{{xStartTimestamp}}}

  run1end<-imuData$TimeStamps[1]+offset2*1000

  temp1 =1000000000
  for (j in imuData$TimeStamps){

    if(abs(run1start-j)<temp1){
      temp1 = abs(run1start-j)
      run1startTimeStamp = j
    }

  }

  temp1 =1000000000
  for (j in imuData$TimeStamps){

    if(abs(run1end-j)<temp1){
      temp1 = abs(run1end-j)
      run1endTimeStamp = j
    }

  }


obs<-subset(imuData, ((imuData$TimeStamps >=  run1startTimeStamp) & (imuData$TimeStamps <= run1endTimeStamp)))
write.csv( obs,paste("observations",val,".csv",sep=""))


}



findMeanP<-function(pValues){


  sum =0
  j=0
  n=0
  tempV=c()
  meanTotalPten=c()
  varPten = c()
  sdPten= c()

  for(i in c(26:length(pValues)-25)){

    n=n+1
    sum = sum+pValues[i]
    tempV[n]<-pValues[i]

    if(n%%500 == 0){

      j=j+1
      meanTotalPten[j]<-sum/500
      varPten[j]<-var(tempV)
      sdPten[j]<-sd(tempV)

      sum =0
      n=0
    }





  }

  meanTotalPten[j+1]<-sum/n
  varPten[j+1]<-var(tempV)
  sdPten[j+1]<-sd(tempV)

  my_list <- list(meanTotalPten,varPten,sdPten)
  return(my_list)




}

copCalculater<- function(df){

  f51 <- rep(1/51, 51)
  df$p1<- filter(df$p1, f51, sides = 2)
  df$p2<- filter(df$p2, f51, sides = 2)
  df$p3<- filter(df$p3, f51, sides = 2)
  df$p4<- filter(df$p4, f51, sides = 2)
  df$p5<- filter(df$p5, f51, sides = 2)
  df$p6<- filter(df$p6, f51, sides = 2)
  df$p7<- filter(df$p7, f51, sides = 2)
  df$p8<- filter(df$p8, f51, sides = 2)
  df$p9<- filter(df$p9, f51, sides = 2)
  df$p10<- filter(df$p10, f51, sides = 2)
  df$p11<- filter(df$p11, f51, sides = 2)
  df$p12<- filter(df$p12, f51, sides = 2)
  df$p13<- filter(df$p13, f51, sides = 2)
  df$p14<- filter(df$p14, f51, sides = 2)
  df$p15<- filter(df$p15, f51, sides = 2)
  df$p16<- filter(df$p16, f51, sides = 2)

  Xi= c(0, 1, 48, 54, 2, 24, 27, 58, 9, 30, 31, 58, 17, 19, 40, 37) #X coordinates of the sensors.
  Yi = c(0, 19, 17, -1, -20, 24, 4, -21, -41, -36, -14, -44, -162, -179, -163, -179) #Y coordinates of

  Xcop1 = c() #X coordinate of the CoP.
  Ycop1 = c() #Y coordinate of the CoP

  Xcop2 = c() #X coordinate of the CoP.
  Ycop2 = c() #Y coordinate of the CoP

  Xcop = c() #X coordinate of the CoP.
  Ycop = c() #Y coordinate of the CoP


  sumP = 0 #Sum of pressures
  sumXP = 0 # Sum of pressures * Xi
  sumYP = 0 # Sum of pressures * Yi

  realfrontfootp = 0
  realrearfootp=0
  meanFrontfootp = 0
  meanRearfootp = 0


  for(i in c(26:(length(df$TimeStamp)-25))){


    sumXP =df$p1[i]*Xi[1] + df$p2[i]*Xi[2] +  df$p3[i]*Xi[3] + df$p4[i]*Xi[4]+df$p5[i]*Xi[5] + df$p6[i]*Xi[6] + df$p7[i]*Xi[7] + df$p8[i]*Xi[8]
    + df$p9[i]*Xi[9] + df$p10[i]*Xi[10] + df$p11[i]*Xi[11] + df$p12[i]*Xi[12]

    sumYP = df$p1[i]*Yi[1] + df$p2[i]*Yi[2] +  df$p3[i]*Yi[3] + df$p4[i]*Yi[4]+df$p5[i]*Yi[5] + df$p6[i]*Yi[6] + df$p7[i]*Yi[7] + df$p8[i]*Yi[8]
    + df$p9[i]*Yi[9] + df$p10[i]*Yi[10] + df$p11[i]*Yi[11] + df$p12[i]*Yi[12]


    realfrontfootp =  df$p1[i] + df$p2[i] +  df$p3[i]+ df$p4[i]+df$p5[i] + df$p6[i] + df$p7[i] + df$p8[i]
    + df$p9[i] + df$p10[i] + df$p11[i] + df$p12[i]

    meanfrontfoot <- realfrontfootp/12

    Xcop1[i]<-sumXP/realfrontfootp
    Ycop1[i]<-sumYP/realfrontfootp

    if(realfrontfootp == 0){
      meanfrontfoot=0
      Xcop1[i]= 0
      Ycop1[i]= 0

    }


    sumXP = df$p13[i]*Xi[13] + df$p14[i]*Xi[14] +  df$p15[i]*Xi[15] + df$p16[i]*Xi[16]

    sumYP = df$p13[i]*Yi[13] + df$p14[i]*Yi[14] +  df$p15[i]*Yi[15] + df$p16[i]*Yi[16]

    realrearfootp = df$p13[i] + df$p14[i] +  df$p15[i] + df$p16[i]

    meanrearfoot =  realrearfootp/4

    Xcop2[i]=sumXP/realrearfootp
    Ycop2[i]=sumYP/realrearfootp

    if(realrearfootp ==0){
      meanrearfoot=0
      Xcop2[i]= 0
      Ycop2[i]= 0
    }


    Xcop[i] <- (meanfrontfoot*Xcop1[i] + meanrearfoot*Xcop2[i] )/(meanfrontfoot + meanrearfoot)
    Ycop[i] <- (meanfrontfoot*Ycop1[i] + meanrearfoot*Ycop2[i] )/(meanfrontfoot + meanrearfoot)


    if((meanfrontfoot == 0) && (meanrearfoot == 0)){

      Xcop[i] = NA
      Ycop[i] = NA

    }

    for(i in c((length(df$TimeStamp)-24):length(df$TimeStamp))){

      Xcop1[i]= NA
      Ycop1[i]= NA
      Xcop2[i]= NA
      Ycop2[i]= NA
      Xcop[i]= NA
      Ycop[i]= NA

    }


  }

  my_list <- list( Xcop, Ycop, Xcop1, Ycop1,Xcop2, Ycop2)


}

findfft<-function(pValues ){

  #f51 <- rep(1/51, 51)
  #pValuesnew<- filter(pValues, f51, sides = 2)
  sum =0
  j=0
  n=0
  tempV=c()
  meanTotalPten=c()
  varPten = c()
  sdPten= c()
  f= list()
  amPlist = c()
  ampValues = matrix(nrow=floor(length(pValues)/500), ncol=101)

  fs  =50

  for(i in c(1:length(pValues))){

    n=n+1
    #sum = sum+pValuesnew[i]
    tempV[n]<-pValues[i]

    if(n%%500 == 0){

      j=j+1
      #meanTotalPten[j]<-sum/500
      #varPten[j]<-var(tempV)
      #sdPten[j]<-sd(tempV)

      y<- fft(tempV)
      y.tmp <- Mod(y)
      y.tmp <- Mod(y)
      y.ampspec <- y.tmp[1:(length(y)/2+1)]
      y.ampspec[2:(length(y)/2)] <- y.ampspec[2:(length(y)/2)] * 2


      f <- seq(from=0, to=fs/2, length=length(y)/2+1)
      amPlist<-c(amPlist,mean(y.ampspec[51:101]) )
      #ampValues <- c(amPlist : (y.ampspec[1:21]) )
      for(m in c(1:101)){
        ampValues[[j,m]] <- y.ampspec[m]
      }
      #sum =0
      n=0
    }


  }

  #meanTotalPten[j+1]<-sum/n
  #varPten[j+1]<-var(tempV)
  #sdPten[j+1]<-sd(tempV)

  my_list <- list(amPlist, ampValues)
  return(my_list)
}

findfft2<-function(pValues){

  #f51 <- rep(1/51, 51)
  #pValuesnew<- filter(pValues, f51, sides = 2)
  sum =0
  j=0
  n=0
  tempV=c()
  meanTotalPten=c()
  varPten = c()
  sdPten= c()
  f= list()
  amPlist = c()
  maxIndex = c()
  ampValues = matrix(nrow=floor(length(pValues)/500), ncol=101)

  fs  =50

  for(i in c(1:length(pValues))){

    n=n+1
    #sum = sum+pValuesnew[i]
    tempV[n]<-pValues[i]


    if(n%%500 == 0){

      j=j+1
      #meanTotalPten[j]<-sum/500
      #varPten[j]<-var(tempV)
      #sdPten[j]<-sd(tempV)

      y<- fft(tempV)
      y.tmp <- Mod(y)
      y.tmp <- Mod(y)
      y.ampspec <- y.tmp[1:(length(y)/2+1)]
      y.ampspec[2:(length(y)/2)] <- y.ampspec[2:(length(y)/2)] * 2


      f <- seq(from=0, to=fs/2, length=length(y)/2+1)
      amPlist<-c(amPlist,mean(y.ampspec[51:101]) )
      maxIndex <- c(maxIndex, match(max(y.ampspec[2:101]),y.ampspec))
      #ampValues <- c(amPlist : (y.ampspec[1:21]) )
      for(m in c(1:101)){
        ampValues[[j,m]] <- y.ampspec[m]
      }
      #sum =0
      n=0
    }


  }

  #meanTotalPten[j+1]<-sum/n
  #varPten[j+1]<-var(tempV)
  #sdPten[j+1]<-sd(tempV)

  my_list <- list(amPlist, ampValues, (maxIndex)/10)
  return(my_list)
}

findfft3 <- function(pValues){

  #f51 <- rep(1/51, 51)
  #pValuesnew<- filter(pValues, f51, sides = 2)
  sum =0
  j=0
  n=0
  tempV=c()
  meanTotalPten=c()
  varPten = c()
  sdPten= c()
  f= list()
  amPlist = c()
  maxIndex = c()
  ampValues = matrix(nrow=floor(length(pValues)/500), ncol=101)

  fs  =50

  for(i in c(1:length(pValues))){

    n=n+1
    #sum = sum+pValuesnew[i]
    tempV[n]<-pValues[i]


    if(n%%500 == 0){

      j=j+1
      #meanTotalPten[j]<-sum/500
      #varPten[j]<-var(tempV)
      #sdPten[j]<-sd(tempV)

      y<- fft(tempV)
      y.tmp <- Mod(y)
      y.tmp <- Mod(y)
      y.ampspec <- y.tmp[1:(length(y)/2+1)]
      y.ampspec[2:(length(y)/2)] <- y.ampspec[2:(length(y)/2)] * 2


      f <- seq(from=0, to=fs/2, length=length(y)/2+1)
      amPlist<-c(amPlist,mean(y.ampspec[51:101]) )
      maxIndex <- c(maxIndex, match(max(y.ampspec[2:251]),y.ampspec))
      #ampValues <- c(amPlist : (y.ampspec[1:21]) )
      for(m in c(1:101)){
        ampValues[[j,m]] <- y.ampspec[m]
      }
      #sum =0
      n=0
    }


  }

  #meanTotalPten[j+1]<-sum/n
  #varPten[j+1]<-var(tempV)
  #sdPten[j+1]<-sd(tempV)

  my_list <- list(amPlist, ampValues, (maxIndex)/10)
  return(my_list)
}

##Acc Features####


# imuData<-read.csv("LOG0.CSV")
# names(imuData) <- c("TimeStamps","aX","aY","aZ","gX","gY","gZ","mX","mY","mZ","p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","p11","p12","p13","p14","p15","p16")

## DataProcess
dataProcess <- function(filename1){

  imuData<-filename1
  #names(imuData) <- c("TimeStamps","aX","aY","aZ","gX","gY","gZ","mX","mY","mZ","p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","p11","p12","p13","p14","p15","p16")


  answers1 <- findMeanP(imuData$aX)
  answers2 <- findMeanP(imuData$aY)
  answers3 <- findMeanP(imuData$aZ)
  accFeatures<-data.frame(answers1[[1]],answers1[[2]],answers1[[3]], answers2[[1]],answers2[[2]],answers2[[3]], answers3[[1]],answers3[[2]],answers3[[3]])
  names(accFeatures) <- c("meanAx", "varAx","sdAx", "meanAy", "varAy","sdAy", "meanAz", "varAz","sdAz")


  #write.csv(accFeatures,"AccoriginalFeatures.csv")


  ###### Frequency Features #######


  answers<-findfft3(imuData$aZ)
  dominantF2 <- data.frame(answers[[3]])
  names(dominantF2)<-"DF"

  #write.csv(dominantF2,"dominantF2.csv")


  answers<-findfft2(imuData$aZ)
  dominantF <- data.frame(answers[[3]])
  names(dominantF)<-"DF"
  #write.csv(dominantF ,"dominantF.csv")
  my_list <- list(accFeatures,dominantF2)

}

## ACC Features

for ( val in startendTimes$Run) {

datachunk <- read.csv(paste("observations",val,".csv",sep=""))

test <- dataProcess(datachunk)


write.csv(test[1],paste("AccFeaturesObservations_",val,".csv",sep=""))
write.csv(test[2],paste("dominantF2Observations_",val,".csv",sep=""))

}


# Stress Predictor


setwd("{{{PROJECT_ROOT}}}/Subject{{{subjectNumber}}}_Session{{{sessionNumber}}}")  ##Change the correct working directory before proceed
load( "{{{PROJECT_ROOT}}}/fit.lda")
for (val in startendTimes$Run) {

  Acc <- read.csv(paste("AccFeaturesObservations_",val,".csv",sep=""))
  F2<-read.csv(paste("dominantF2Observations_",val,".csv",sep=""))

  sessionData <- data.frame(Acc[1:length(F2$DF),], F2 )
  #sData<-data.frame(sessionData$meanAz, sessionData$meanAy,sessionData$meanAx,sessionData$DF)
  sData<-data.frame(sessionData$meanAz)
  #names(sData) <- c("pressureAz", "pressureAy", "pressureAx", "MF")
  names(sData) <- c("pressureAz")
  sDataNormZ<- as.data.frame(scale(sData))
  predictions <- predict(fit.lda, sData)

  write.csv(predictions,paste("predictionsObservations_",val,".csv",sep=""))


}