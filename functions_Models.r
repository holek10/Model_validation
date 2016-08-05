##### ROC curve function#####
# Vypocet bodu ROC krivky
#
# INPUTS
# x ... data pro skupinu 1
# y ... data pro skupinu 0
#
# OUTPUTS
# res ... body ROC krivky
#####################

ROC.basic<-function(x,y, bigger_better = T){
  ### changing the sign of the variables in case that the variable is vice versa, i.e. the higher value is better and lower value is worse
  if (bigger_better == T) parameter <- -1 else parameter <- 1
  x <- parameter * x
  y <- parameter * y
  
  if(is.vector(x)){
    data<-rbind(cbind(x,1),cbind(y,0))
    L1<-length(x)
    L2<-length(y)
  }
  if(is.matrix(x)){
    data<-x
    L1<-sum(data[,2])
    L2<-dim(data)[1]-L1
  }
  data<-data[order(data[,1]), ]
  Sens<-1-cumsum(data[,2])/L1
  Spec<-cumsum(1-data[,2])/L2
  res<-list(Sens=Sens,Spec=Spec,type="raw")
  class(res)<-"ROC"
  res
}

### AUC ###
auc <- function(s0,s1, bigger_better = T) {
  ### changing the sign of the variables in case that the variable is not ROC "standard", i.e. iwe change it in case that the higher value is better and lower value is worse
  if (bigger_better == T) parameter <- -1 else parameter <- 1
  s0 <- parameter * s0
  s1 <- parameter * s1

    a <- 0
    for (i in 1:length(s0)) {
        a <- a + sum(s1 > s0[i])
    }
    auc <- a/length(s0)/length(s1)
    auc
}
###################### ROC and AUC END #########################



########################## GINI INDEX FOR VARIABLE ##################################################

gini_for_variable <- function(pom_dta, fli, breakpoint_number_fixed = 10, breakpoint_number_quantiles = 10, bigger_better = T, method = "quantiles") {
  breakpoints_values <- seq(min(pom_dta[,fli]), max(pom_dta[,fli]),length=breakpoint_number_fixed)  # divides the variable into predefined number of intervals of same length, not taking the frequency pf the value sinto consideration, this might cause concentration in some intervals
  breakpoints_quantiles <- quantile(pom_dta[,fli],seq(0,1,length=breakpoint_number_quantiles))      # divides the variable into predefined number of intervals, where interval have the same number of observations

  if(method == "quantiles") breakpoints <- breakpoints_quantiles else breakpoints <- breakpoints_values

  # number of observations and defaults in the group
  count <- hist(pom_dta[,fli],breaks=breakpoints,plot = FALSE)
  nondefault <- hist(pom_dta[,fli][pom_dta$def==0],breaks=breakpoints,plot = FALSE)
  default <- hist(pom_dta[,fli][pom_dta$def==1],breaks=breakpoints,plot = FALSE)
  
  # overview of the numbers
  count$counts
  default$counts
  round(default$counts/count$counts,4)
  
  # values on the x-axis
  point <- seq(1,(length(breakpoints)-1),by=1)
  values <- round(breakpoints[1:(length(breakpoints)-1)],3)
  
  # DR graph
  def_rate <- round(default$counts/count$counts,4)
  windows();
  plot(def_rate, ylim=c(0,max(def_rate)), type="l",lwd=2,xaxt="n",xlab='Upper bound of variable',ylab='Default rate in %', main=fli )   # 2. VYSTUP
  axis(1,at=point, labels=values)
  grid()
  
  # ROC curve
  rr <- ROC.basic(pom_dta[,fli][pom_dta$def==1], pom_dta[,fli][pom_dta$def==0], bigger_better = bigger_better)
  aucc <- auc(pom_dta[,fli][pom_dta$def==0], pom_dta[,fli][pom_dta$def==1], bigger_better = bigger_better)
  
  gini <- aucc*2-1
  plot(1-rr$Spec,rr$Sens, main=paste('AUC = ', as.character(round(aucc,2)), ' AR = ', as.character(round(gini,2))),xlab='',ylab='',cex.main=0.8,pty="s")
  
  return(gini)
}

######################################################################################################
######################################################################################################



# --- Information Value ----  #
# --- needs to be adjusted to take appropriate counts (of default, non_defaults)


#zoznam$inf_value <- vector()
#for (x in 1: (length(zoznam$nazov)))
#for (x in 37:53)
#         {
#          p_non_def <- vector(,length(zoznam$non_defaults[[x]]))
#          p_def <- vector(,length(zoznam$defaults[[x]]))
#          WOE <- vector(,length(zoznam$non_defaults[[x]]))
#          xxx <- vector(,length(zoznam$non_defaults[[x]]))
#
#              for (i in 1: length(zoznam$pocet[[x]]))
#                 {
#                 p_non_def[i] <- zoznam$non_defaults[[x]][i]/sum(zoznam$non_defaults[[x]])
#                 p_def[i] <- zoznam$defaults[[x]][i]/sum(zoznam$defaults[[x]])
#
#    if( p_non_def[i] != 0 & p_def[i] != 0)   WOE[i] <- log(p_non_def[i]/p_def[i])   else WOE[i] <- 0
#
#                 xxx[i] <-  (p_non_def[i]-p_def[i])*WOE[i]
#                 }
#          sum(xxx)
#          zoznam$inf_value[[x]] <- sum(xxx)
#         }









###############################
#
# DTABLE - tabulka selhani pro kategorialni promenne
#
###############################

dtable <- function(x,def){
  dtab <- table(x,def)
  dtab <- cbind(dtab,apply(dtab,1,sum))
  dtab <- cbind(dtab,round(dtab[,2]/dtab[,3],2))
  colnames(dtab) <- c("neselhane","selhane","vse","defrate")
  dtab
}
###############################


###############################
#
# POLEFV - pole financnich vykazu
#
#  - prehled poradi a popisu sloupcu ve financnich vykazech
#  - prehled poctu NULL hodnot a trid sloupcu
#
###############################

polefv <- function(data=r, data1 = r1, fl = fld){

  smr <- NULL

  for (i in 1:length(fl)) {
    fl[i] <- paste(substr('X000',1,4-nchar(fl[i])),fl[i],sep='')
    smr <- rbind(smr,
                 c(fl[i],
                 as.character(data1[1,fl[i]]),
                 sum(data[,fl[i]]=='NULL'),
                 class(data[,fl[i]])))
    }
  colnames(smr) <- c('sloupec','popis','# NULL','class')
  print(smr)
  
  #for (i in 1:length(fl)) {
  #print(summary(data[,fl[i]]))
  #}
  
}
###############################




###############################
#
# QTABLE - tabulka kvantilu
#
#
###############################
qtable <- function(d,def,crit,alpha=seq(0,1,by=0.1)) {
 ucrit <- sort(unique(crit))
 qtab <- matrix(0,length(ucrit)+1,length(alpha))

 for (i in 1:length(ucrit)) {
     qtab[i,] <- quantile(d[crit==ucrit[i]],alpha,na.rm=T)
 }
 qtab[i+1,] <- quantile(d,alpha,na.rm=T)
 qtab

 qtdef <- matrix(0,length(ucrit)+1,length(alpha)-1)
 for (j in 1:(length(alpha)-1)) {
     for (i in 1:length(ucrit)) {
          qtdef[i,j] <- sum(crit==ucrit[i] & d>=qtab[length(ucrit)+1,j] & d<qtab[length(ucrit)+1,j+1] & def==1)/sum(crit==ucrit[i] & d>=qtab[length(ucrit)+1,j] & d<qtab[length(ucrit)+1,j+1] & def!=-1)
     }
     qtdef[i+1,j] <- sum(d>=qtab[length(ucrit)+1,j] & d<qtab[length(ucrit)+1,j+1] & def==1)/sum(d>=qtab[length(ucrit)+1,j] & d<qtab[length(ucrit)+1,j+1] & def!=-1)
 }
 list(qtab=as.table(qtab), qtdef=as.table(qtdef))
}
###############################



###### STARE GRAFY

# GRAFY PRO SPOJITE PROMENNE

obrProdukt <- function(prom, jmeno, roz=1, data = uvery, cond = TRUE, top=0.95) {
         
         lvl <- levels(as.factor(data$krit[cond]))
         numlvl <- length(lvl)
         data$krit <- as.factor(data$krit)
         
         windows()
         if (roz==1) { par(mfrow=c(1,numlvl)) }
         if (roz==2) { par(mfrow=c(2,ceiling(numlvl/2))) }


        for (i in 1:numlvl) {
          gdata <- prom[cond & data$krit == lvl[i]]
          numAll<-length(gdata) 
          
          # kontrola NA a nul
          numNA <- sum(is.na(gdata))
          gdata <- gdata[is.na(gdata)==F]
          num0 <- sum(gdata==0)
          gdata <- gdata[gdata!=0]
          
          # omezeni maximalnich hodnot
          threshold <- sort(gdata)[round(length(gdata)*top)] 
          gdata <- gdata[gdata < threshold]
          
          
          # histogram a hustota
          if (length(gdata)>2){
            den <- density(gdata)
            h <- hist(gdata,10,plot = FALSE)
    
            xmin <- min(c(h$breaks,den$x))
            xmax <- max(c(h$breaks,den$x))
            ymin <- min(c(h$density,den$y))
            ymax <- max(c(h$density,den$y))
            }
          # umela data pokud nejde hustota spocitat
          else {
            h <- -10
            den <- NULL
            den$x <- 0
            den$y <- -10
            xmin <- 0
            xmax <- 1
            ymin <- 0
            ymax <- 1
            
            }

          if (i>1) {jmeno <- ""} 

          plot(h, beside=T, freq = FALSE,xlim = c(xmin,xmax), ylim = c(ymin,ymax), 
            xlab=paste('Krit:', lvl[i], '"#Vse:', numAll,'#NA:',numNA, '#Nul:',num0), ylab = '', main=jmeno)
          lines(den$x, den$y, lwd = 2, col = rgb(67,103,197,maxColorValue = 255))
        }
              
}
        

# GRAFY PRO FAKTOROVE PROMENNE

histProdukt <- function(prom, data=uvery, jmeno='Nazev', roz=1, osay=1, capt=NULL, cond=TRUE, coldef=EYcolors[c(1,4,6)]) {
         
         lvl <- levels(as.factor(data$krit[cond]))
         numlvl <- length(lvl)
         data$krit <- as.factor(data$krit)
         
         windows()
         if (roz==1) { par(mfrow=c(1,numlvl)) }
         if (roz==2) { par(mfrow=c(2,ceiling(numlvl/2))) }


         pt <- with(uvery[cond,], table(krit, prom[cond])) 
         st <- pt %*% rep(1,ncol(pt))


          for (i in 1:numlvl) {
         
            gt <- pt[i,]/st[i]
            if (i>1) {jmeno <- ""} 
            
            if(i<numlvl){
              barplot(gt, beside=T, col=coldef, ylim=c(0,osay), xlab=lvl[i], main=jmeno, names.arg=capt)}
            else {
              barplot(gt, beside=T, col=coldef, ylim=c(0,osay), xlab=lvl[i], main=jmeno, names.arg=capt, legend.text=names(table(prom[cond])))}
         }
}


#####################################################
# EY colors - Basic (black, shades of grey, yellow) #
#####################################################
Tt <- matrix(c(0,100,128,166,204,242,255,
               0,100,128,166,204,242,210,
               0,100,128,166,204,242,0  ), ncol=3)
EYcolors <- rgb(Tt, max=255)

#########################################################################################
# EY colors - Palette ( blue, 50%blue, purple, 50%purple, green, 50%green, red, 50%yellow) #
#########################################################################################
Tt <- matrix(c(0  ,127,145,200,44 ,149,240,255,
              129,192,39 ,147,151,203,76 ,242,
              174,214,143,199,62 ,158,62 ,127), ncol=3)
EYcolorsP <- rgb(Tt, max=255)
#################################



