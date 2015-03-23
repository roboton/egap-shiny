
# setwd("D:/Dropbox/M/v/pproaches/R/shiny/fish")

shinyServer(function(input, output) {
  
  output$g <- renderText({
    result(c(input$a1,input$a2,input$a3,input$a4,input$a5,input$a6,input$a7,input$a8, input$a9,input$a10))
  })

  
  output$htext <- renderUI({
    numbers = c(input$a1,input$a2,input$a3,input$a4,input$a5,input$a6,input$a7,input$a8, input$a9,input$a10)
    str0 <- "Unbelievable!"
    str1 <- paste("You chose the numbers ", add0(input$a1),add0(input$a2), add0(input$a3),add0(input$a4),add0(input$a5),add0(input$a6),add0(input$a7),add0(input$a8),add0(input$a9),add0(input$a10),sep=' ')
    str2 <- paste("But these are clearly not random numbers. We can tell because random numbers do not contain patterns but the numbers you entered show a fairly obvious pattern.")
    str3 <- result(numbers)                  
    text <- ifelse(sum(is.na(numbers))>0,
            "Please enter ten RANDOM numbers. We will then perform a classical statistical test to figure out if your numbers are really random.",
            paste(h3(str0), str1, "", str2, "", str3, "", "Try again (with really random numbers this time)!",
                  "", 
                  paste("ps: you might think that if the p value calculated above is high (for example if it is greater than 12%)
                  that this means that the numbers you chose are not all that odd; but in fact it means that the numbers are really particularly
                  odd since the probability that the fishy test would produce a p values above 12%, when really random sequences are used, is low (p<0.07). For more on how to fish see ", a( "here.", href="http://www.columbia.edu/~mh2245/papers1/PA_2012b.pdf")),
                  "",
                  sep = '<br/>'))
    HTML(text)
  })
})

#Helpers
# add a 0 to output for numbers <10
add0 = function(k) return(ifelse(k<10, paste(0,k, sep=""), paste(k)))
  
# get primes
PN <- function(x){(sum(x/1:x==x%/%1:x)==2)}
Primes <- (1:100)[sapply(1:100, PN)]

# install.packages("gmp")
# library(gmp)

n =10

spot_pattern = function(D){
  n = length(D)
  D1= floor(D/10)
  D2= D-10*D1
  D1[D1==10]<-0 
  # single digits
  X= list()
#  X[[1]]=sum(isprime(D)/2)  # how many primes are there
  X[[1]]=sum(D%in%Primes)
  X[[2]]= sapply(0:9, function(k) sum(D1==k)+sum(D2==k))  # how often does each digit figure (100 counted as 00)
  X[[2]][2]= X[[2]][2] + sum(D==100)
  X[[3]]= sapply(2:20, function(k) sum(D%%k==0))  # how often each number is a multiple of 2:20
  # Expected frequency of repetitions
  X[[4]]= sapply(1:4, function(k) sum(abs((D[-n])-(D[-1]))<=k))  # how often is the gap between two sequential numbers <= j
  X[[5]]= sapply(1:4, function(k) sum(abs((D[-n])-(D[-1]))>k))  # how often is the gap between two sequential numbers > j
  X[[6]]= sapply(1:4, function(k) sum(abs(D1-D2)<=k))  # how often is the gap between two digits  <= j
  X[[7]]= sapply(1:4, function(k) sum(abs(D1-D2)>k))  # how often is the gap between two digits  > j
  X[[8]]= sum(D>50)  # >50 
  X[[9]]= sum(D<50)  # >50 
  X[[10]]= sum(D>80)  # >50 
  X[[11]]= sum(D<20)  # >50 
  X[[12]]= sum(D<33 | D>66)  # extreme 
  X[[13]]= sum(D>=33 & D<=66)  # centrist 
  X[[14]]= mean(D)  # average
  X[[15]]= length(D) - length(unique(D))  # repeats of previous numbers
  X[[16]]= sum((D<1)|(D>100))  # out of range
  X[[17]]= sum(D[-n] > D[-1])  # number decreasing
  X[[18]]= sum(D[-1] > D[-n])  # number increasing
  X[[19]]= 2*length(D)-length(unique(c(D1,D2)))  # number of repeated digits
  X[[20]]= sum(c(D1,D2)%%2)  # number of odd digits
  X[[21]]= sum(1-c(D1,D2)%%2)  # number of even digits

  # The average number
  X}


# to figure out the actual distribution of each test statistic
# Need to include the number of tests (currently 60)
ri.dist = function(sims, n, g=60){
 Z=matrix(NA, g, sims)
 for(s in 1:sims){
   D= 1+floor(100*runif(n))  
   Z[,s]=unlist(spot_pattern(D)) }
 Z
}

labs=c("the number of prime numbers in this sequence is: ",
       "the number of times that `0' appears in the sequence is: ",
       "the number of times that `1' appears in the sequence is: ",
       "the number of times that `2' appears in the sequence is: ",
       "the number of times that `3' appears in the sequence is: ",
       "the number of times that `4' appears in the sequence is: ",
       "the number of times that `5' appears in the sequence is: ",
       "the number of times that `6' appears in the sequence is: ",
       "the number of times that `7' appears in the sequence is: ",
       "the number of times that `8' appears in the sequence is: ",
       "the number of times that `9' appears in the sequence is: ",
       "the number of exact multiples of `2' is: ",
       "the number of exact multiples of `3' is: ",
       "the number of exact multiples of `4' is: ",
       "the number of exact multiples of `5' is: ",
       "the number of exact multiples of `6' is: ",
       "the number of exact multiples of `7' is: ",
       "the number of exact multiples of `8' is: ",
       "the number of exact multiples of `9' is: ",
       "the number of exact multiples of `10' is: ",
       "the number of exact multiples of `11' is: ",
       "the number of exact multiples of `12' is: ",
       "the number of exact multiples of `13' is: ",
       "the number of exact multiples of `14' is: ",
       "the number of exact multiples of `15' is: ",
       "the number of exact multiples of `16' is: ",
       "the number of exact multiples of `17' is: ",
       "the number of exact multiples of `18' is: ",
       "the number of exact multiples of `19' is: ",
       "the number of exact multiples of `20' is: ",
       "the number of times that two neighboring numbers are within 1 point of each other is: ",
       "the number of times that two neighboring numbers are within 2 points of each other is: ",
       "the number of times that two neighboring numbers are within 3 points of each other is: ",
       "the number of times that two neighboring numbers are within 4 points of each other is: ",
       "the number of times that two neighboring numbers are more than 1 point apart from  each other is: ",
       "the number of times that two neighboring numbers are more than 2 points apart from each other is: ",
       "the number of times that two neighboring numbers are more than 3 points apart from each other is: ",
       "the number of times that two neighboring numbers are more than 4 points apart from each other is: ",
       "the number of times that two neighboring digits are within 1 point of each other is: ",
       "the number of times that two neighboring digits are within 2 points of each other is: ",
       "the number of times that two neighboring digits are within 3 points of each other is: ",
       "the number of times that two neighboring digits are within 4 points of each other is: ",
       "the number of times that two neighboring digits are more than 1 point apart from each other is: ",
       "the number of times that two neighboring digits are more than 2 points apart from each other is: ",
       "the number of times that two neighboring digits are more than 3 points apart from each other is: ",
       "the number of times that two neighboring digits are more than 4 points apart from each other is: ",
       "the number of numbers above 50 is: ",
       "the number of numbers below 50 is: ",
       "the number of numbers above 80 is: ",
       "the number of numbers below 20 is: ",
       "the number of numbers below 33 or above 66 [Extremist bias]  is: ",
       "the number of numbers between 33 and 66 [Centrist bias] is: ",
       "the average number in this sequence is unusually high at: ",
       "the number of numbers in the sequence that are repeats of previous numbers is: ",
       "the number of numbers that are out of range (not in [1, 100]) is: ",
       "the number of numbers that are smaller than the preceding number (decending bias) is: ",
       "the number of numbers that are larger than the preceding number (ascending bias)  is: ",
       "the number of repeated digits (digits that are repeats or previous digits,  including '0' for numbers <10) is: ",
       "the number of digits that are odd is: ",
       "the number of digits that are even is: "       
       )

# ps
ri.p = function(A,DIST){
  A2 = unlist(A)
  sapply(1:length(A2), function(i) sum(DIST[i,]>=A2[i]))/ncol(DIST)
  }

result = function(D, g=60){
  A =spot_pattern(D) 
  j=min((1:g)[ri.p(A, DIST)==min(ri.p(A, DIST))])	
  return(paste("Take another look at the sequence you put in. You will see that ", 
               labs[j], unlist(A)[j], 
              ". But the `expected number' from a random process is just ", round(mean(DIST[j,]),1),
              ". How odd is this pattern? Quite odd in fact. The probability that a truly random 
                process would turn up numbers like this is just p=", round(ri.p(A, DIST)[j],5), " (i.e. less than ",  floor(1+100*ri.p(A, DIST)[j]), "%).", sep=""))
  }

# The distribution of patterns is created as follows
# DIST=ri.dist(30000, n)

# But here we run from a saved version to save processing time
#write.csv(DIST, "data/DIST.csv")
  DIST = as.matrix(read.csv("data/DIST.csv"))[,-1]


# To calculate the "not odd" probability
# result.simp = function(D){
#   A =spot_pattern(D) 
#  min(ri.p(A, DIST))  
# }
# not.odd = sapply(1:10000, function(j) result.simp(1+floor(100*runif(10))))
# mean(not.odd>=.1)
# mean(not.odd>=.11)
# mean(not.odd>=.12)
# mean(not.odd>=.1 & not.odd<.11)

# What is the most random sequence of numbers in the whole world?

# trials = 20000
# runs = 50
# p.out = matrix(NA, runs, 1)
# p = matrix(NA, trials, runs)
# seq.out =  matrix(NA, runs, 10)
# for(k in 1:runs){
#   X = matrix(1+floor(100*runif(10*trials)), trials, 10)
#   P = sapply(1:trials, function(j) result.simp(X[j,]))
#   p[,k]<-P
#   print(k)
#   print(X[(1:trials)[P == max(P)],])
#   print(max(P))
#   seq.out[k,]<-  t(X[(1:trials)[P == max(P)][1:10],])[1:10]
#   p.out[k] <-max(P) 
#   hist(p, main = paste("p values given ", k*trials, "draws"))
#  }
# p.out
# seq.out
# hist(p)
# dim(seq.out)
