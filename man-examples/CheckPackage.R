# Already installed
CheckPackage("MSEtool")

\dontrun{
# Not installed
CheckPackage("MadeUp")

# Needs updating
CheckPackage("MSEtool", "99")

# Update and specify installation path
CheckPackage("MSEtool", "99", "pak::pgk_install('blue-matter/MSEtool')")
}
