# SAFMC Case Study

devtools::load_all()

MOM <- readRDS('../SAFMC-MSE/OM_Objects/BaseCase_RS.OM')


MOM@Allocation

OM <- Convert(MOM)  # convert from `MOM` to `om`

OM@Allocation
OM@Efactor



om@Fleet[[1]][[1]] |> class()

OM@Fleet[[1]][[1]] |> class()






