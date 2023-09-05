
pnorm(0.5,0,1) -pnorm(-1,0,1)

1 - pnorm(-1.96,0,1)

pnorm(-1.96, 0, 1, lower.tail= FALSE)

pnorm(15,12,3) - pnorm(8,12,3) #0.7501




qnorm(0.7,70,8)

x = qnorm(0.8, 70,8)

1 - pnorm(x,70,8)

1 - pnorm(0.8,0.7,0.08)

1 - pnorm(80,70,8)

pnorm(65,70,8) - pnorm(50,70,8)


qnorm(0.9,70,8)


qnorm(0.2, 12,3)

pnorm(20,12,3, lower.tail = FALSE)*100

mean(rnorm(20, 12,3))


x = pnorm(15,12,3, lower.tail = FALSE)

1 - pnorm(15,12,3)

pbinom(3,20,0.1586553)

pgamma(4,2,1/3) - pgamma(2,2,1/3)



a = 1
b = 5


EX = a*b
Sd = sqrt(a*b^2)
EX
Sd
mean(rgamma(10000000,a,1/b))

sd(rgamma(10000000,a,1/b))


fx = function(x){ 
  if(x >= 0){
    return(x*-exp(x))
  }
  else{
    return(0)
  }
}

fx = function(x){ 
    x*-exp(x)
}

integrate(fx, lower = 1, upper = 2)$value

pgamma(2,2,1) - pgamma(1,2,1)

1- pgamma(4,2,1)

mean(rgamma(10000000,2,1))
sd(rgamma(1000000,2,1))











