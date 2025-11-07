library(MSEtool)

la()

SSDir <- '../WCNPOSWO-2023/Final Base-case'

RepList <- ImportSSReport(SSDir)

# Correct M-at-Age for initial age class
# for some unknown reason, M for age-0 is exactly half the actual value
RepList[[1]]$M_at_age[,4] <- RepList[[1]]$M_at_age[,4] * 2 

LoadArgs('ImportSS')

OM <- ImportSS(RepList)

LoadArgs('Simulate_om')

Hist <- Simulate_om(OM)


# ---------------------- DEBUG ----------------------

TimeStepsList <- GetSSTimeSteps(replist, 1)


replist <- RepList[[1]]

GetSSNatAge(replist, OM, yrs=1973)
GetSSNatAge(replist, OM, yrs=1975)

replist$natage |> dplyr::filter(Yr==1975, `Beg/Mid`=='B')

replist$batage |> dplyr::filter(Yr==1975, `Beg/Mid`=='B')

TimeSteps(OM, 'H')

replist$spawnseas

replist$M_at_age |> dplyr::filter(Sex==1, Yr==1975)


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

