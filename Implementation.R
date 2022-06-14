# renvoie si un nombre est premier ou non
Premier <- function(n) { 
  if(n < 2) return(FALSE) 
  d <- 2 #
  while( d < n) {
    if( n %% d == 0) return(FALSE)
    d <- d + 1
  }
  return(TRUE) 
}

# N premiers nombres premiers
Npremier <- function(n){
  z <- c()
  d <-2
  while(length(z)!=n){
    if(Premier(d)==TRUE){
      z <- c(z,d)
    }
    d <- d + 1
  }
  return(z)
}


# fonction de répartition des 100 premiers nombres premiers
a <- Npremier(200)
plot(a,1:200,type='s',xlim=c(1,100),ylim=c(1,25))


# fonction R(x) de Riemann
Rx <- function(x){
  rx <- c()
  for(n in 1:50)
    rx <- c(rx,(moebius(n)/n)*li(x^(1/n)))
  return(sum(rx))
}

# valeur de R(x) entre 2 et 201 avec un pas de 0.5
Rx.N <- c()
for(k in seq(2,201,0.5)){
  Rx.N <- c(Rx.N,Rx(k))
}

# correction apporté à pi(x) grâce aux zéros non triviaux de Zêta
correc <- function(x,f){
  rr <- c()
  for(b in 1:length(zz[1:f])){
    for(n in 1:30){
      rr <- c(rr,(moebius(n)*4*(sqrt(x)^(1/n))/log(x))*
                (cos(zz[b]*log(x)/n)+2*zz[b]*sin(zz[b]*log(x)/n))/
                (1+4*(zz[b])^2))
      
    }
  }
  return(sum(rr))
}


# correction apporté à pi(x) grâce aux zéros triviaux de Zêta
triviaux <- function(x){
  rrx <- c()
  for(x in seq(2,201,0.5)){
    rrx <- c(rrx,Rx(x^-2))
  }
  return(rrx)
}

# 3 zéros triviaux suffisent pour avoir une excellente approximation
NmbZerosTriv <- function(ZerosCalcule){
  res <- 0
  for(k in 1:ZerosCalcule){
    res <- res+Re(triviaux(x^k))
  }
  return(res)
}
Triv <- NmbZerosTriv(3)


# nombre de zéros non triviaux que l'on prend 
rescorr <- c()
for(k in seq(2,201,0.5)){
  rescorr <- c(rescorr,correc(k,1)) # correc(k,10), correc(k,50), correc(k,200)
}


# traçage des figures en fonction du nombre de zéros non triviaux pris
ggplot() + geom_step(aes(a,1:200))+ xlim(0,100) + ylim(0,25) + xlab("") + ylab("") +
  theme(axis.line = element_line(colour = "grey18"), panel.background = element_blank(),
        legend.position = c(0.1, 0.96),legend.text = element_text(size = 11),
        legend.key = element_rect(fill = "white")) +
  geom_line(aes(seq(2,201,0.5),Rx.N-Triv-rescorr,colour="π(x)")) + 
  scale_color_manual(name='',breaks=c('π(x)'),
                     values=c('π(x)'='red'))
