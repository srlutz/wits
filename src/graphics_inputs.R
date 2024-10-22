
setwd(figures_dir)

# input functions

if (tracer=="ST" | tracer=="H3+ST") {
    # moving six months input means (summer and winter)
    obs_val<- stable_input
    obs_date<- stable_input_date
    dummy1 <- seq(as.Date("01/01/1960","%m/%d/%Y"),by="month",length.out=1000) # vector with monthly dates
    dummy2 <- seq(4,by=6,length.out=167) # vector marking the fourth and tenth month of each year
    dummy3 <- decimal_date(dummy1[dummy2]) # vector with the dates of the fourth and tenth month of each year in decimal format
    dummy4 <- vector(mode="numeric",length=length(obs_date))
    dummy5 <- vector(mode="numeric",length=length(obs_date))
    dummy6 <- vector(mode="numeric",length=length(obs_date))

    max <- length(dummy3)-1 # number of iterations to calculate the six-months averages

    for(i in 1:max) {

    x1<-dummy3[i]
    x2<-dummy3[i+1]
    ix<-which(obs_date<x2 & obs_date>=x1) # finds the dates betweeen the fourth and tenth month of a given year
    dummy4[ix]<-i # flags these months
    dummy5[ix]<- x1
    dummy6[ix]<-i%%2 # flags winter and summer months separately with "0" and "1"
    }

    stable_ws_means_date <- unique(dummy5)
    stable_ws_means <- as.numeric(tapply(obs_val,dummy4,mean,na.rm=T)) # calculates the mean value of each six month period
    stable_all_means <- c(as.numeric(tapply(obs_val,dummy6,mean,na.rm=T)),mean(obs_val,na.rm=T)) # winter, summer and overall mean for the entire observation period
    ix=which(is.na(stable_ws_means))
    if(length(ix)>0) { stable_ws_means_date <- stable_ws_means_date[-ix] ; stable_ws_means <- stable_ws_means[-ix]} # removes potential "nas"
}

if (tracer=="ST") {

    png("input_stable.png",height=2000,width=6000,res=200)
    par(mar=c(5,4.5,1.5,1.5))

    plot(stable_input_date,stable_input,type="l",lwd=1.5,lty=2,bty="l",xlab="",xlim=c(x_inf_stable_input,x_sup_stable_input),ylab=lab_stable,ylim=c(y_inf_stable_input,y_sup_stable_input),las=1,yaxt="n") # stable isotope
    abline(h=stable_all_means,col="grey",lty=2,lwd=3)
    abline(h=stable_all_means[3],col="grey",lwd=3)
    lines(stable_ws_means_date,stable_ws_means,type="s",lwd=3,col="black") # 6-months means as step graph
    axis(1,at=seq(1960,length.out=140,by=1),labels=F,tck=-0.01)
    if(stable_iso=="O18") {axis(2,at=seq(-30,0,by=5),labels=T,las=1)}
    if(stable_iso=="O18") {axis(2,at=seq(-30,0,by=1),labels=F,tck=-0.01)}
    if(stable_iso=="H2") {axis(2,at=seq(-200,0,by=20),labels=T,las=1)}
    if(stable_iso=="H2") {axis(2,at=seq(-200,0,by=5),labels=F,tck=-0.01)}
    legend(x_inf_stable_input+3,y_inf_stable_input+10,c("annual mean","winter/summer mean"),lty=c(1,2),col="grey",bg="white",lwd=3)
    dev.off()

    }


if (tracer=="H3") {

    png("input_h3.png",height=2000,width=6000,res=200)
    par(mar=c(5,4.5,1.5,1.5))

    plot(h3_input_date,h3_input,type="l",lwd=3,log="y",bty="l",xlab="",xlim=c( x_inf_h3_input, x_sup_h3_input),ylab="tritium [TU]",ylim=c(1,10000),las=1,yaxt="n") # tritium
    axis(1,at=seq(1950,length.out=140,by=1),labels=F,tck=-0.01)
    axis(2,at=c(10,100,1000,10000),labels=T,las=1)
    axis(2,at=c(seq(1,10),seq(10,100,by=10),seq(100,1000,by=100),seq(1000,10000,by=1000)),labels=F,tck=-0.01)
    dev.off()

    }


if (tracer=="H3+ST") {

    png("input_stable_h3.png",height=2000,width=6000,res=200)
    par(mar=c(3,4.5,1,1.5))
    par(mfrow=c(2,1))
    plot(stable_input_date,stable_input,type="l",lwd=1.5,lty=2,bty="l",xlab="",xlim=c(x_inf_stable_input,x_sup_stable_input),ylab=lab_stable,ylim=c(y_inf_stable_input,y_sup_stable_input),las=1,yaxt="n") # stable isotope
    abline(h=stable_all_means,col="grey",lty=2,lwd=3)
    abline(h=stable_all_means[3],col="grey",lwd=3)
    lines(stable_ws_means_date,stable_ws_means,type="s",lwd=3,col="black") # 6-months means as step graph
    axis(1,at=seq(1960,length.out=140,by=1),labels=F,tck=-0.01)
    if(stable_iso=="O18") {axis(2,at=seq(-30,0,by=5),labels=T,las=1)}
    if(stable_iso=="O18") {axis(2,at=seq(-30,0,by=1),labels=F,tck=-0.01)}
    if(stable_iso=="O18") {legend(x_inf_stable_input+1,y_inf_stable_input+5,c("annual mean","winter/summer mean"),lty=c(1,2),col="grey",bg="white",lwd=3)}
    if(stable_iso=="H2") {axis(2,at=seq(-200,0,by=20),labels=T,las=1)}
    if(stable_iso=="H2") {axis(2,at=seq(-200,0,by=5),labels=F,tck=-0.01)}
    if(stable_iso=="H2") {legend(x_inf_stable_input+1,y_inf_stable_input+30,c("annual mean","winter/summer mean"),lty=c(1,2),col="grey",bg="white",lwd=3)}

    plot(h3_input_date,h3_input,type="l",lwd=3,log="y",bty="l",xlab="",xlim=c( x_inf_h3_input, x_sup_h3_input),ylab="tritium [TU]",ylim=c(1,10000),las=1,yaxt="n") # tritium
    axis(1,at=seq(1950,length.out=140,by=1),labels=F,tck=-0.01)
    axis(2,at=c(10,100,1000,10000),labels=T,las=1)
    axis(2,at=c(seq(1,10),seq(10,100,by=10),seq(100,1000,by=100),seq(1000,10000,by=1000)),labels=F,tck=-0.01)
    dev.off()

    }

setwd(routines_dir)



