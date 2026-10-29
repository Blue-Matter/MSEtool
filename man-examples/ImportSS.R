\dontrun{
## Path to a directory containing SS3 output
ss_dir <- "path/to/SS3/output"

## Read SS3 output only
reps <- ImportSSReport(ss_dir)

## Build an operating model from SS3 output
OM <- ImportSS(
  SSDir = reps,
  Name = "Example SS3 Import",
  StockName = "ExampleStock",
  FleetNames = c("Commercial", "Recreational")
)

## Inspect imported stocks and fleets
StockNames(OM)
FleetNames(OM)
}
