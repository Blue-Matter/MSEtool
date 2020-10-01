OMdir <- 'G:/My Drive/1_PROJECTS/CDFW_Multi_Fleet/RockCrabDLM'
RedCrab_MSE <- readRDS(file.path(OMdir, 'RedCrab_MSE.rda'))

plot(RedCrab_MSE)

x = RedCrab_MSE

RedCrab_MSE@Fnames

class(RedCrab_MSE)


mm <- 6
RedCrab_MSE@C[1,1,1,mm, ]

plot(RedCrab_MSE@C[1,2,1,5, ])
lines(RedCrab_MSE@C[1,2,1,6, ])

plot(RedCrab_MSE@Effort[1,2,1,1, ], ylim=c(0,2))
lines(RedCrab_MSE@Effort[1,2,1,1, ])

RedCrab_MSE@Effort[,2,1,2, ]

# find out why Effort isn't exactly as specified
# why is current effort not 1 in first year?


devtools::load_all()

multiHist <- RCrabHist<- readRDS(file.path(OMdir, 'RCrabHist.rda')) 

multiHist[[1]][[1]]@AtAge$F.Mortality[,,50,1]


multiHist=RCrabHist

parallel=FALSE
silent=FALSE

MPs <- list(
  # MPs for female stock
  c('curE', 'incE', 'newSL_curE', 'newSL_incE', 'MOnly_curE', 'MOnly_incE', 'reduceE'), 
  # MPs for male stock
  c('curE','incE', 'newSL_curE', 'newSL_incE', 'curE', 'incE', 'reduceE') 
)

mm = 2

# Is maxF being applied in historical years ?
MOM@Fleets[[1]][[1]]@MPA <- FALSE
OM <- new('OM', MOM@Stocks[[1]], MOM@Fleets[[1]][[1]], Perfect_Info, Perfect_Imp,
          nsim=3)
OM@cpars$control$D <-'VB'


Hist <- Simulate(OM)

Hist@AtAge$F.Mortality[,,50,1]

multiHist[[1]][[1]]@AtAge$F.Mortality[,,50,1]




