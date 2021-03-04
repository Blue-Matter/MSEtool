
#library(MSEtool)
devtools::load_all()

OM <- testOM
OM@nsim <- 50
Hist <- Simulate(OM)

plot('Depletion', Albacore)
plot('Growth', Albacore)
plot('Maturity', Albacore)
plot('M', Albacore)
plot('Recruitment', Albacore)
plot('Spatial', Albacore)

OM <- testOM
OM@nsim <- 50
Hist <- Simulate(OM)

plot('Depletion', Hist)

# plot.num argument: one plot at a time
plot('Growth', Hist, plot.num=1)
plot('Growth', Hist, plot.num=2)
plot('Growth', Hist, plot.num=3)


plot('Maturity', Hist)
plot('M', Hist)
plot('Recruitment', Hist)
plot('Spatial', Hist)



plot(Albacore)


testOM@nsim <- 50


Hist <- Simulate(testOM)

OM <- testOM
OM@Name <- "test"

plot('Depletion', testOM, html=T, dev=TRUE)

plot('Depletion', testOM, html=F)

plot('Growth', testOM, html=T)
plot('Growth', testOM, html=F)

plot('Maturity', testOM, html=T)
plot('Maturity', testOM, html=F)

plot('M', testOM, html=T)
plot('M', testOM, html=F)








# test Stock
# test Hist
# test OM
# test plot.Stock
# test plot.OM


