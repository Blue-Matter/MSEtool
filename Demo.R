# Stock
## Existing 
myStock <- new('Stock')
myStock@Name <- 'Base Case Albacore Stock'
myStock@Common_Name <- 'Albacore'
myStock@Species <- 'Thunnus alalunga'

myStock@maxage <- 20
myStock@Linf <- c(100, 100)
myStock@K <- c(0.2,0.2)
myStock@t0 <- c(0,0)

myStock

## New 
myNewStock <- Stock('Base Case Albacore Stock',
                 'Albacore',
                 'Thunnus alalunga',
                 Ages=Ages(20),
                 Length=Length(Pars=list(Linf=100, K=0.2, t0=0)))

myNewStock


Length(myNewStock)



# OM 

## Existing
myOM <- new('OM', Stock=myStock)
myOM@Name <- 'My Operating Model'
myOM


## New 
newNewOM <- OM('My Operating Model',
               Stock=myNewStock)
newNewOM


# Conversions

SSdir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2022/000_base_case'

SWO_Existing <- SS2MOM(SSdir=SSdir,nsim=5, Name='North Atlantic Swordfish') 



SWO_New <- Convert(SWO_Existing)  # convert from `MOM` to `om`


Stock(SWO_New,2)

format(object.size(SWO_Existing), units='Mb')

object.size(SWO_New)  |> format(units='Mb')

newMOM <- Convert(OM)  # convert `om` back to `MOM`


SWO_Existing@cpars[[1]][[1]]$Len_age



SWO_New |> Stock() |> Length() |> MeanAtAge()



SWO_New@Stock[[1]]@Length@MeanAtAge
