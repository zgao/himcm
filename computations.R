pi <- 3.1415
iterations <- 120000
possibilities <- 12
real.probs <- c(0.099, 0.149, 0.099, 0.349, 0.249, 0.049, 0.006)

eval.divergence <- function(lambda, period, power) { #lambda 1, power 7, period 3.5
  total.kl <- 0

  get.f <- function(x) {
    adjusted <- x / period
    b <- exp((adjusted - floor(adjusted) - 1) * power) #polynomial 15
    return(b)
  }
  next.end <- function(x) {
    return(period * floor(x / period) + 2 * period)
  }
  
  sofar <- c(0)
  for (i in 1:iterations) {
    repeat {
      poisson <- -log(runif(1)) / lambda
      last <- sofar[length(sofar)]
      nxt <- last + poisson
      if (nxt < next.end(last)) break
    }
    sofar <- c(sofar,nxt)
  }
  filtered.sofar <- c()
  for (i in 2:length(sofar)) {
    if (get.f(sofar[i]) > runif(1) || (i != length(sofar) &&
                                        sofar[i + 1] >= next.end(sofar[i]) - period))
      filtered.sofar <- c(filtered.sofar, sofar[i])
  }
  #print(length(filtered.sofar))
  diff <- c()
  for (i in 1:length(filtered.sofar)-1)
    diff <- c(diff, filtered.sofar[i + 1] - filtered.sofar[i])
  counts <- hist(diff,breaks=c(0.0,0.5,1.5,2.5,3.5,4.5,5.5,10.0))$counts
  hist(diff,breaks=1000)
  counts <- counts/length(diff)
  
  print(counts)
  
  for (i in 1:length(counts)) {
    total.kl <- total.kl + (log(real.probs[i] / counts[i]) * real.probs[i])
  }
  
  print(total.kl)
  return(total.kl)

}

filtered.dataset <- function(lambda, period, power) {
  get.f <- function(x) {
    adjusted <- x / period
    b <- exp((adjusted - floor(adjusted) - 1) * power) #polynomial 15
    return(b)
  }
  next.end <- function(x) {
    return(period * floor(x / period) + 2 * period)
  }
  
  sofar <- c(0)
  for (i in 1:iterations) {
    repeat {
      poisson <- -log(runif(1)) / lambda
      last <- sofar[length(sofar)]
      nxt <- last + poisson
      if (nxt < next.end(last)) break
    }
    print(i)
    sofar <- c(sofar,nxt)
  }
  filtered.sofar <- c()
  for (i in 2:length(sofar)) {
    if (get.f(sofar[i]) > runif(1) || (i != length(sofar) &&
                                         sofar[i + 1] >= next.end(sofar[i]) - period))
      filtered.sofar <- c(filtered.sofar, sofar[i])
  }
  return(filtered.sofar)
}

eval.cdf <- function(lambda, period, power) { #lambda 1, power 7, period 3.5
  filtered.sofar <- filtered.dataset(lambda, period, power)
  
  plot(1:150~filtered.sofar[1:150])
  
  #print(length(filtered.sofar))
  diff <- c()
  for (i in 1:length(filtered.sofar)-1)
    diff <- c(diff, filtered.sofar[i + 1] - filtered.sofar[i])
  return(hist(diff,breaks=1000))
}

binsearch <- function(x, s) {
  # x is the target.
  # s is sorted ascending (but can have repeated values).
  # Returns a pair of indexes into s, c(low, high), such that
  #     if min(s) <= x <= max(s), then
  #         s[low] <= x <= s[high] and 
  #             either high == length(s) or x < s[high+1].
  #
  # Output is undefined if x is outside the range of s (but termination
  # is assured).
  
  low <- 1; high <- length(s)
  while (high - low > 1) {
    mid = floor((high+low)/2)
    if (s[mid] <= x) low <- mid
    else high <- mid
  }
  c(low, high)
}



filtered.sofar <- filtered.dataset(1.304, 3.422, 5.980)
diff <- c()
for (i in 1:length(filtered.sofar)-1)
  diff <- c(diff, filtered.sofar[i + 1] - filtered.sofar[i])
cdf.hist <- hist(diff,breaks=1000)


diff <- cdf.hist$mids[2] - cdf.hist$mids[1]
counts1 <- c(0)
for (i in 1:length(cdf.hist$counts))
  counts1 <- c(counts1, counts1[length(counts1)] +
                diff * cdf.hist$density[i] )
sample.t1 <- function() {
  rand <- runif(1)
  dat <- binsearch(rand, counts1)
  return(diff * (dat[2] - 0.5))
}

tm <- 1500
std <- c()
for (i in 0:(tm-1)) std <- c(std, i, i)
std <- c(std, tm)
new.filtered <- c(0)
for (i in 1:tm) new.filtered <- c(new.filtered, filtered.sofar[i], filtered.sofar[i])
for (j in 1:1) {
  print(j)
  esc <- c(0)
  for (i in 1:tm) esc <- c(esc, esc[length(esc)] + sample.t1())
  new.esc <- c(0)
  for (i in 2:(tm+1)) new.esc <- c(new.esc, esc[i], esc[i])
  plot(std~new.esc, type='l', col='red', xlim=c(0,5000))
  par(new=TRUE)
}
plot(std~new.filtered, type='l', xlim=c(0,5000))

c1 <- 0.251235
c2 <- 0.748765
sigma1 <- 0.2093175
sigma2 <- 0.2093175
mu1 <- 0.003414787
mu2 <- 1.048607

mix <- function(x) {
  return (c1 * dlnorm(x, meanlog=mu1, sdlog=sigma1, log=FALSE) +
            c2 * dlnorm(x, meanlog=mu2, sdlog=sigma2, log=FALSE))
}

counts2 <- c(0)
for (i in 1:length(cdf.hist$counts)) {
  mid <- cdf.hist$mids[i]
  counts2 <- c(counts2, counts2[length(counts2)] +
                 diff * mix(mid))
}

sample.s1 <- function() {
  rand <- runif(1)
  dat <- binsearch(rand, counts2)
  return(diff * (dat[2] - 0.5))
}

counts.t.discrete <- c(0.0, 0.1, 0.25, 0.35, 0.7, 0.95, 1.0)
sample.discrete.t1 <- function() {
  rand <- runif(1)
  dat <- binsearch(rand, counts.t.discrete)
  return(dat[1] - 1)
}
counts.s.discrete <- c(0.0, 0.25, 0.45, 0.85, 1.0)
sample.discrete.s1 <- function() {
  rand <- runif(1)
  dat <- binsearch(rand, counts.s.discrete)
  return(dat[1])
}

idle <- c()
repeat {
  s <- 1e-08
  while (s > 0) {
    s <- s + sample.s1() - sample.t1()
  }
  idle <- c(idle, -s)
  print(-s)
}

ww <- c()
repeat {
  w <- c(sample.discrete.s1())
  sample.discrete.diff <- function() {
    return(sample.discrete.s1() - sample.discrete.t1())
  }
  for (i in 1:150) {
    print(i)
    w <- c(w, max(w[length(w)] + sample.discrete.diff(), 0))
  }
  print(w)
  #hist(w)
  print(mean(w))
  ww <- c(ww, mean(w))
}

#lambdas <- seq(from=0.5,to=3.5,length.out=possibilities)
#periods <- seq(from=3.30,to=4.0,length.out=possibilities)
#powers <- seq(from=4,to=10,length.out=possibilities)
#best.lambda <- -1
#best.period <- -1
#best.power <- -1
#best.kl <- 10000000000.0


#for (i in 1:possibilities) {
#  for (j in 1:possibilities) {
#    for (k in 1:possibilities) {
#      this.kl <- eval.divergence(lambdas[i], periods[j], powers[k])
#      print(paste(lambdas[i], periods[j], powers[k], sep=' '))
#      if (this.kl < best.kl) {
#        best.kl <- this.kl
#        best.lambda <- lambdas[i]
#        best.period <- periods[j]
#        best.power <- powers[k]
#        print(paste(best.kl, best.lambda, best.period, best.power, sep=' '))
#      }
#    }
#  }
#}