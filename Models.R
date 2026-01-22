

# Models

myAges <- Ages(15)
Years <- 1980:2025
nSim <- 5

## --- Length -----
LengthModels()


myLength <- Length(Pars=list(Linf=100, K=0.1, t0=0))


myLength@Pars <- StructurePars(Pars = myLength@Pars, nSim, Years)
myLength@Model <- FindModel(myLength)

GenerateMeanAtAge(Model=myLength@Model,
                  Pars=myLength@Pars,
                  Ages=myAges@Classes)




myLength <- Length(Pars=list(Linf=c(90,100), K=0.1, t0=0))

myLength@Pars <- StructurePars(Pars = myLength@Pars, nSim, Years)
myLength@Model <- FindModel(myLength)

GenerateMeanAtAge(Model=myLength@Model,
                  Pars=myLength@Pars,
                  Ages=myAges@Classes)




myLength <- Length(Pars=list(Linf=array(c(100, 120), dim=c(1,1,2),
                                        dimnames=list(Sim=1,
                                                      Year=1980,
                                                      Area=1:2)), 
                             K=0.1, t0=0))

myLength@Pars <- StructurePars(Pars = myLength@Pars, nSim, Years, nArea=2)
myLength@Model <- FindModel(myLength)

GenerateMeanAtAge(Model=myLength@Model,
                  Pars=myLength@Pars,
                  Ages=myAges@Classes)

# ---- Weight -----


# ----- Selectivity ----


