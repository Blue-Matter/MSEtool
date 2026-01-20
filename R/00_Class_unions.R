

# Custom Class Unions

setClassUnion("char.null", c("character", "NULL"))
setClassUnion("num.null", c("numeric", "NULL"))
setClassUnion("list.null", c("list", "NULL"))
setClassUnion("array.null", c("array", "NULL"))


setClassUnion("char.num", c("character", "numeric", "NULL"))
setClassUnion("num.log", c("numeric", "logical", "NULL"))
setClassUnion("logical.list", c("logical", "list"))
setClassUnion("missing.char", c("missing", "character"))


setClassUnion("array.list.null", c("array", "list", "NULL"))
setClassUnion("array.log.null", c("array", "logical", "NULL"))
setClassUnion("num.list.null", c("numeric", "list", "NULL"))
setClassUnion("num.array.null", c("numeric", "array", "NULL"))


setClassUnion("array.char.null", c("array", "character", "NULL"))
setClassUnion("array.char.num", c("array", "character", "numeric", "NULL"))
setClassUnion("num.array.list", c("numeric", "array", "list", "NULL"))
setClassUnion("num.array.df", c("numeric", "array", "data.frame", "NULL"))


setClassUnion("fun.char", c("function", "character", "list", "NULL"))
setClassUnion("num.Date", c("numeric", "Date", "POSIXct", "character", "NULL"))


