# Custom Class Unions

methods::setClassUnion("char.null", c("character", "NULL"))
methods::setClassUnion("num.null", c("numeric", "NULL"))
methods::setClassUnion("list.null", c("list", "NULL"))
methods::setClassUnion("array.null", c("array", "NULL"))
methods::setClassUnion("df.null", c("data.frame", "NULL"))

methods::setClassUnion("char.num", c("character", "numeric", "NULL"))
methods::setClassUnion("char.list", c("character", "list", "NULL"))
methods::setClassUnion("num.log", c("numeric", "logical", "NULL"))
methods::setClassUnion("logical.list", c("logical", "list"))
methods::setClassUnion("missing.char", c("missing", "character"))

methods::setClassUnion("array.list.null", c("array", "list", "NULL"))
methods::setClassUnion("array.log.null", c("array", "logical", "NULL"))
methods::setClassUnion("num.list.null", c("numeric", "list", "NULL"))
methods::setClassUnion("num.array.null", c("numeric", "array", "NULL", "logical"))

methods::setClassUnion("array.char.null", c("array", "character", "NULL"))
methods::setClassUnion("char.log.num", c("character", 'logical', 'numeric', "NULL"))
methods::setClassUnion("array.char.num", c("array", "character", "numeric", "NULL"))
methods::setClassUnion("array.char.num.list", c("array", "character", "numeric", 'list', "NULL"))
methods::setClassUnion("num.array.list", c("numeric", "array", "list", "NULL"))
methods::setClassUnion("num.array.df", c("numeric", "array", "data.frame", "NULL"))

methods::setClassUnion("fun.char", c("function", "character", "list", "NULL"))
methods::setClassUnion("num.Date", c("numeric", "Date", "POSIXct", "character", "NULL"))

