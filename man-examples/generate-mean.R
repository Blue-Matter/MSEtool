
# List `Maturity` Models
MaturityModels()


# Mean-at-Age
GenMeanAtAge(Model="MaturityAtAge", 
             Pars=list(A50=5, A50_95=1),
             Ages=1:10)

# Mean-at-Length
GenMeanAtLength(Model="MaturityAtLength", 
                Pars=list(L50=50, L50_95=10),
                Length=seq(10,100,10))

# Mean-at-Weight
GenMeanAtWeight(Model="MaturityAtWeight",
                Pars=list(W50=3, W50_95=1),
                Weight=1:10)
