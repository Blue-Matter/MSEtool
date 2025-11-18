library(MSEtool)

la()

SSDir <- '../WCNPOSWO-2023/Final Base-case'

RepList <- ImportSSReport(SSDir)

# Correct M-at-Age for initial age class
# for some unknown reason, M for age-0 is exactly half the actual value
RepList[[1]]$M_at_age[,4] <- RepList[[1]]$M_at_age[,4] * 2 

OM <- ImportSS(RepList, nSim=2)
Hist <- Simulate_om(OM, Reduce=FALSE)

replist <- RepList[[1]]
CompareSSNumber(replist, Hist)


# TODO
# - 
# - write CompareSS functions ... 

# Things to check:
# - SS calculates recruits for age-0 = this should be accounted for otherwise h has different interpretation
# - N-at-Age doesn't match
# - use spawntime frac? Calc rec devs better
# - apply natural mortality to R0?
# - weight and spawning production differnt in first quarter - n is the same
# 
# TODO 
# - test NPSWO
# - test SALB
# - test NASWO



# ---------------------- DEBUG ----------------------
replist <- RepList[[1]]


Yr <- 1976
q <- 3
OM_N <- Hist@Number$Female
n1 <- GetSSNatAge(replist, OM, yrs=yr)
Yrind <- match(yr, OM@Years) + q - 1 
df <- data.frame(SS=n1[,q], OM=OM_N[1,,Yrind,1]) 
apply(df, 2, sum, na.rm=TRUE)

round(df,2)



Stock <- Hist@OM@Stock$Female
mod <- Stock@SRR@Model
S <- Hist@SProduction[1,1,Yrind-3]
S0 <- Hist@Unfished@Equilibrium@SProduction[1,1,Yrind]
R0 <- Stock@SRR@R0[1,Yrind] * exp(-((0.42/4) * 2)) 
h <- Stock@SRR@Pars$h[1,1]

mod(S,S0,R0,h)  * 2

replist$recruit |> dplyr::filter(Yr==1976)


replist$timeseries |> dplyr::filter(Yr==1976) |> 
  dplyr::select(Yr, Bio_all, SpawnBio)
Hist@SProduction[1,1,1:8]
apply(Hist@Biomass[1,1:2,1:8],2, sum)



dev <- Hist@OM@Stock$Female@SRR@RecDevHist[1,Yrind]
mod(S,S0,R0,h)* dev

mod(S,S0,R0,h) * 0.8813122
# 383.76 - ss
(mod(S,S0,R0,h)* dev)/383.76

dev/1.020114

Hist@OM@Stock$Female@SRR@RecDevHist[1,1:10]

t <- replist$recruit |> dplyr::filter(Yr==1976)
exp(t$dev)
0.8990389 * 0.98

767.51/875.075       




n1 <- GetSSNatAge(replist, OM, yrs=1975)
n2 <- GetSSNatAge(replist, OM, yrs=1976)

SS_Zs <- -log(n2[2:62,1]/n1[1:61,4])

OM_Zs <- -log(OM_N[1,2:62,5,1]/OM_N[1,1:61,4,1])


cbind(SS_Zs[is.finite(SS_Zs)], OM_Zs[is.finite(SS_Zs)]) |>
  matplot(type='b')

M_1 <- OM@Stock$Female@NaturalMortality@MeanAtAge[1,,y]

SS_F <- SS_Zs-M_1[1:61]
OM_F <- OM_Zs-M_1[1:61]

cbind(SS_F[is.finite(SS_F)], OM_F[is.finite(SS_F)]) |>
  matplot(type='b')






replist$natage |> dplyr::filter(Yr==1975)





# Recruitment deviations are out 


SS_Z <- -log(n1[4,2]/n1[3,1]) # age 1 F in 1975
OM_Z <- -log(OM_N[1,4,2,1]/OM_N[1,3,1,1]) # age 1 F in 1975

M_1 <- OM@Stock$Female@NaturalMortality@MeanAtAge[1,3,1]

SS_F <- SS_Z-M_1
OM_F <- OM_Z-M_1

SS_F
OM_F
Hist@FDead$Female[1,3,1,] |> sum()

SSAgeClasses <- GetSSAgeClasses(replist)
SS_FDead <- replist$fatage |> dplyr::filter(Yr==1975, Sex==1, Seas==1) |> 
  dplyr::select(as.character(SSAgeClasses)) |>
  colSums()


y <- 3
SS_Zs <- -log(n1[2:62,y+1]/n1[1:61,y])

OM_Zs <- -log(OM_N[1,2:62,y+1,1]/OM_N[1,1:61,y,1])

cbind(SS_Zs[is.finite(SS_Zs)], OM_Zs[is.finite(OM_Zs)]) |>
  matplot(type='b')



# OM F is higher than SS3s 
 

# UP TO HERE - need to match n-at-age 




SSAgeClasses <- GetSSAgeClasses(replist)

replist$fatage |> dplyr::filter(Yr==1975, Sex==1, Seas==1) |> 
  dplyr::select(as.character(SSAgeClasses)) |>
  colSums()

-log(312.833/345.679) # ss3
-log(306.13/345.679) # OM

-log(306.13/345.679) - -log(312.833/345.679)


apply(HistSim@FDeadArea$Female$`1975`, 1, sum)


t <- replist$timeseries |> dplyr::filter(Yr>=1975) 



plot(t$Bio_all)

lines(colSums(EquilibriumUnfished@Biomass[1,1:2,1:4]), col='blue')

sum(t[59:62,], na.rm = TRUE)
sum(t, na.rm=TRUE)

GetSSNatAge(replist, OM, yrs=1975)

OM@Stock$Female@SRR@R0

replist$natage |> dplyr::filter(Yr==1973, `Beg/Mid`=='B', Sex==1)

Years(OM, 'H')

-log(421.4040/468.0580)
0.1050002*4

-log(379.4010/468.0580)
-log(379.4010/468.0580)

replist$M_at_age



OM@Stock$Female@NaturalMortality@MeanAtAge[1,,1]
OM@Stock$Female@Length@MeanAtAge[1,,1]

# -------------------- END DEBUG --------------------



# TODO  - initialize equilbrium seasonal model 

Stock <- OM@Stock$Female
Stock@SRR@R0[1,1:4]

TimeSteps <- TimeSteps(OM, 'H')

Hist@Unfished@Equilibrium@Number$Female[1,,1]
Hist@Unfished@Equilibrium@Number$Female[1,,2]
Hist@Unfished@Equilibrium@Number$Female[1,,3]
Hist@Unfished@Equilibrium@Number$Female[1,,4]



SSN |> dplyr::filter(Age==0)
SSN |> dplyr::filter(Age==15)


replist$M_at_age |> head()

Stock@NaturalMortality@MeanAtAge[1,,1]

421.404 * exp(-0.0525)


Stock@SRR@R0[1,1:4]
-log(345.8800/379.4010)
-log(315.3210/345.8800)
-log(287.4630/315.3210)



SSN |> dplyr::filter(Age==1)

Stock@NaturalMortality@MeanAtAge[1,3,1]




Hist@Effort[1,1,,] |> apply('TimeStep', sum) |> plot(type='l')

##############################################################################








# MOM <- SS2MOM(SSDir)
# plot_SS2MOM(MOM, SSDir)
# 
# MOM@cpars$Female$F1_JPN_WCNPO_OSDWCOLL_late_Area1$Find[1,]
# MOM@cpars$Female$F1_JPN_WCNPO_OSDWCOLL_late_Area1$Wt_age 

replist <- ImportSSReport(SSDir)[[1]]

AgeClasses <- GetSSAgeClasses(replist)

Y1 <- 1990

N1 <- replist$natage |> dplyr::filter(Yr==Y1, Sex==1, `Beg/Mid`=='B', 
                                      Seas==1) |>
  dplyr::select(dplyr::all_of(as.character(AgeClasses))) |>
  unlist()

N2 <- replist$natage |> dplyr::filter(Yr==Y1+3, Sex==1, `Beg/Mid`=='B', 
                                      Seas==1) |>
  dplyr::select(dplyr::all_of(as.character(AgeClasses))) |>
  unlist()


F1 <- replist$fatage |> dplyr::filter(Yr==Y1, Sex==1, Seas%in%1:4) |> 
  dplyr::select(dplyr::all_of(as.character(AgeClasses))) |> colSums()

M1 <- replist$M_at_age |> dplyr::filter(Yr==Y1, Sex==1) |>
  dplyr::select(dplyr::all_of(as.character(AgeClasses))) |>
  unlist()

Z1 <- (F1+M1)/4

N2calc <- N1[2:16] * exp(-Z1[2:16])

data.frame(Z1, N1,N2, c(0, N2calc))


-log(  120.166000 /216.24300  )

1.96303 * exp(-1.938885)

0.992283  * exp(-0.456)


replist$natage |> dplyr::filter(Yr==Y1, Sex==1, `Beg/Mid`=='B', 
                                Seas%in%1:2)

