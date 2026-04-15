data = rnorm(n=100, mean=0, sd=1)

mean.data= mean(data)
vars.data= var(data)
f.data= pnorm(data,mean=mean.data, sd=sqrt(vars.data), lower.tail=TRUE)
q.data= qnorm(f.data)
qqnorm(q.data)
abline(b=1,a=0)

gen.data = rpois(1000, lambda=4)
mean(gen.data)
prob.data = ppois(gen.data,lambda=mean(gen.data), lower.tail=TRUE)
qqnorm(qnorm(prob.data))
abline(b=1,a=0)
