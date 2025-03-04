library(MSEtool)

la <- devtools::load_all

la()

octopusOM <- OM('Day Octopus OM',
                Author='Adrian Hordyk',
                Email='adrian@bluematterscience.com',
                Sponsor='[Blue Ventures Indonesia](https://blueventures.org/about/team/indonesia/)',
                Region='NE Sulawesi, Indonesia',
                Latitude=1.741653,
                Longitude = 125.085258,
                nYear=5,
                pYear=10,
                TimeUnits = 'month',
                nSim=2)

## ---- create_stock ----

octopus <- Stock('Day octopus',
                 CommonName='Gurita',
                 Species='Octopus cyanea')

## ---- ages ---
Ages(octopus) <- Ages(MaxAge=14,
                      Units='month',
                      PlusGroup = FALSE)

## ---- length ----
# Schnute Growth Model

Schnute <- function(Ages, SizeAgeZero, SizeInflectionPoint, GrowthRateCoefficient, ShapeParameter) {
  (SizeAgeZero^ShapeParameter + ((SizeInflectionPoint^ShapeParameter)/(1-ShapeParameter)-SizeAgeZero^ShapeParameter) *
     (1-exp(-GrowthRateCoefficient*Ages)))^(1/ShapeParameter)
}


Length(octopus) <- Length(Pars=list(SizeAgeZero=c(30, 40),
                                   SizeInflectionPoint=c(150, 200),
                                   GrowthRateCoefficient=c(0.6, 0.9),
                                   ShapeParameter=c(-5,-3)),
                         Model=Schnute,
                         CVatAge=c(0.1,0.2))

## ---- weight ----

Weight(octopus) <- Weight(Pars=list(Alpha=0.0721,
                                    Beta=1.9181),
                          Units='g',
                          Classes=seq(from=0.25, by=0.5, to=4))

## ---- natural_mortality ----

NaturalMortality(octopus) <- NaturalMortality(Pars=list(M=c(0.1, 0.2)),
                                              Units='month')

## ---- maturity ----

Maturity(octopus) <- Maturity(Pars=list(A50=c(10, 11),
                                        A50_95=c(0.1, 0.5)),
                              Semelparous=TRUE)

## ---- stockrecruit ----

# Ricker
SRR(octopus) <- SRR(Pars=list(hR=c(0.85, 0.95)),
                    R0=10000,
                    SD=c(0.4,0.6),
                    SpawnTimeFrac = 0.5)


## ---- spatialdistribution ----

Spatial(octopus) <- Spatial(UnfishedDist=c(0.3, 0.3),
                            ProbStaying=c(0.9, 0.95),
                            RelativeSize='EqualDensity')

## ---- depletion ----
Depletion(octopus) <- Depletion(Final=c(0.35, 0.45))


## ---- fleet ----
octopus_fleet <- Fleet('Octopus Fleet')

## ---- effort ----

Effort(octopus_fleet) <- Effort(Effort=data.frame(TimeStep=c(0, 0.125, 0.5, 1),
                                                  Lower=c(0, 0.4, 0.9, 0.9),
                                                  Upper=c(0, 0.6, 1, 1),
                                                  CV=0.1))

## ---- selectivity ----

Selectivity(octopus_fleet) <- Selectivity(Pars=list(A50=c(4, 6),
                                                    A50_95=c(1,2)))

## ---- populateOM ----

Stock(octopusOM) <- octopus
Fleet(octopusOM) <- octopus_fleet

OM <- Populate(octopusOM)

messages='default'
nSim=NULL
parallel=FALSE
silent=FALSE

# SimulateDEV
