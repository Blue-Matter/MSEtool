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

## ---- weight ----
# TODO Schnute Growth Model

Weight(octopus) <- Weight(Pars=list(a=c(0.0053975, 0.0073025),
                                    b=2.31),
                          CVatAge = c(0.3, 0.5),
                          Units='kg',
                          Classes=seq(from=0.25, by=0.5, to=4))

## ---- natural_mortality ----

NaturalMortality(octopus) <- NaturalMortality(Pars=list(M=c(0.10, 0.2)),
                                              Units='month')

## ---- maturity ----

Maturity(octopus) <- Maturity(Pars=list(A50=c(11, 12),
                                        A50_95=c(0.1, 0.5)),
                              Semelparous=TRUE)

## ---- stockrecruit ----

# TODO: Shepherd SSR

SRR(octopus) <- SRR(Pars=list(h=c(0.85, 0.95)),
                    R0=10000,
                    SD=c(0.4,0.6),
                    SpawnTimeFrac = 0.5)


## ---- spatialdistribution ----

Spatial(octopus) <- Spatial(UnfishedDist=c(0.05, 0.2),
                            ProbStaying=c(0.7, 0.95),
                            RelativeSize='EqualDensity')

## ---- depletion ----
Depletion(octopus) <- Depletion(Final=c(0.3, 0.4))


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

object <- octopusOM
OM <- Populate(octopusOM)

OM@Fleet[[1]][[1]]@Effort@Effort


# OM@Fleet$`Day octopus`$`Octopus Fleet`@Effort@Catchability[] <- 0.1

# OM@Fleet$`Day octopus`$`Octopus Fleet`@Effort@Effort[,1] <- 0.1


messages='default'
nSim=NULL
parallel=FALSE
silent=FALSE

# SimulateDEV
