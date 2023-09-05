##midterm practice 

#question 1:

#1a

p = 0.6 
n = 40
pbinom(30,40,0.6) - pbinom(24,40,0.6)

#1b
ex = n*p
sd = sqrt(40*0.6*(1-0.6))

round(mean(rbinom(100000,40,0.6)), digits = 4)  
round(sd(rbinom(1000000,40,0.6)), digits = 4)

#1c
pnbinom(10,10,0.6) - pnbinom(3,10,0.6)

#question 2:

#2a
x = c(0.1,0.1,0.3,0.5)
y = c(28,58.5,31,77)

Ex = sum(x*y)
Ex

#2b:

EX2 = sum(x*y^2)
Var = EX2 - Ex^2
Sd = sqrt(Var)
Sd

#2c: 

19.5*Ex+300-800

#question 3

#3a
lamb = 2.8
dpois(0,2.8)

#3b
1 - ppois(0,2.8)

#3c:
dpois(1,2.8)/ppois(3,2.8)

x = 0:15
plot(x, dpois(x,2.8), type = "h", col = "dark blue")
abline(v = lamb, col = "red")

#right tailed poisson distribution 

#Quiz 1 

#Question 1 #BIG TIME REVIEW :(((())))

#1a

tot = 5^3

#1b

##P('7' = 1)

s1 = choose(3,1) #prob of 7 in every slot 
s2 = choose(1,0)*choose(4,1) #prob of anything else in the second 
s3 = choose(1,0)*choose(4,1) #prob of anything else in the third

PA = (s1*s2*s3)/tot
PA
#1c

###WRONG WRONG WRONG 

s1 = choose(3,2)
s2 =  choose(3,2)#prob of anything else in the second 
s3 = choose(3,2) #prob of anything else in the third
1 - (s1*s2*s3)/tot

1 - (2/5)^3

#1d
(choose(3,1)*3*3)/125


#1e 

(choose(3,2)*3*3*2)/125
#P(A)              #P(B)                   #P(AnB)
PA + (choose(3,2)*3*3*2)/125 - ((s1*s2*s3)/tot)

#question2 

#P(red) = 4P(white)
#P(pink) = 3P(White)

#2a 

#1 = p(white) + p(red) + p(pink)
# 1 = pw + 4pw + 3pw 
# 1 = pw(1+4+3)
# 1 = pw(8)
# 1/8 = pw 
# pr = 4*1/8 = 4/8
# pp = 3*1/8 = 3/8

Pred = 0.5

Pwhite = 1/8

Ppink = 3/8

#2b 

Pred * 0.95 

#2c
options(digits = 10)

Ppink*0 + (Pwhite*0.10) + Pred*0.05 

#question 3 

#3a

funct = function(x){
  
  (choose(10,x)*choose(10,5-x))/choose(20,5)
  
}

#3b
x = 0:5
plot(x, funct(x), type = "h", main = "pmf graph", xlab = "x", ylab = "y")

#this is a two tailed graph 

#question 4 

#4a 

x = 1:6

#1 = k * E(x^2)
# E(x^2) = (n*(n+1)*(2*n+1))/6
#1/(n*(n+1)*(2*n+1))/6 = k
n = 6 

k = 1/(n*(n+1)*(2*n+1))/6 

probdist = function(bu){
  k*bu^2
}
x = 1:6
#4b
plot(x,probdist(x), type = "h", col = "dark blue", main = "PMF graph", ylab = "P(X=x)", xlab = "x")

#this is a left tailed distribution 

#4c
X = 4:6
sum(probdist(X)) #should be 0.846 this is literally right but idk what is happening 
X = 1:3
1 - sum(probdist(X)) #wtf is wrong with this. 

#4d 
x = 1:6
m1 = sum(x*probdist(x))
m2 = sum(x^2*probdist(x))
sd = sqrt(m2 - m1^2)
sd
