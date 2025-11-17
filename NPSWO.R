library(MSEtool)

la()

SSDir <- '../WCNPOSWO-2023/Final Base-case'

RepList <- ImportSSReport(SSDir)

# Correct M-at-Age for initial age class
# for some unknown reason, M for age-0 is exactly half the actual value
RepList[[1]]$M_at_age[,4] <- RepList[[1]]$M_at_age[,4] * 2 

OM <- ImportSS(RepList, nSim=2)
OM@Data$`Female Male`@Survey@Units

Hist <- Simulate_om(OM)


LoadArgs('Simulate_om')

# TODO
# - match N-at-Age for historical
# - F-at-age are different??
# - write CompareSS functions ... 



# TODO 
# - test NPSWO
# - test SALB
# - test NASWO



# ---------------------- DEBUG ----------------------
replist <- RepList[[1]]
yr <- 1975
OM_N <- Hist@Number$Female
n1 <- GetSSNatAge(replist, OM, yrs=yr)
q <- 2
data.frame(SS=n1[,q], OM=OM_N[1,,q,1])

SS_Z <- -log(n1[4,2]/n1[3,1]) # age 1 F in 1975
OM_Z <- -log(OM_N[1,4,2,1]/OM_N[1,3,1,1]) # age 1 F in 1975

M_1 <- OM@Stock$Female@NaturalMortality@MeanAtAge[1,3,1]

SS_F <- SS_Z-M_1
OM_F <- OM_Z-M_1

SS_F
OM_F
Hist@FDead$Female[1,3,1,] |> sum()

SS_FDead <- replist$fatage |> dplyr::filter(Yr==1975, Sex==1, Seas==1) |> 
  dplyr::select(as.character(SSAgeClasses)) |>
  colSums()


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

