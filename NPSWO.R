la <- devtools::load_all
la()

SSDir <- '../WCNPOSWO-2023/Final Base-case'

RepList <- ImportSSReport(SSDir)

OM <- ImportSS(RepList, populate=FALSE)

LoadArgs('ImportSS')

OM <- PopulateOM(OM)

Hist <- Simulate_om(OM)

GetSS_RecDevs

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

