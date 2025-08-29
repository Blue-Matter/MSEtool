

isValidObject <- function(object) {
  # chk <- Check(object)
  # if (chk@empty) return(TRUE)
  # if (length(chk@errors)>0) return(chk@errors)
  TRUE
}


# Custom Class Unions ----
methods::setClassUnion(name="ASK.null", members=c("NULL", 'array', 'list'))
methods::setClassUnion(name="fun.char", members=c("character", "function", "list", "NULL"))
methods::setClassUnion(name="char.null", members=c("character", "NULL"))
methods::setClassUnion(name="char.num", members=c('character', "numeric", 'NULL'))
methods::setClassUnion(name="char.list", members=c('character', "list", 'NULL'))
methods::setClassUnion(name="array.null", members=c("array", "NULL"))
methods::setClassUnion(name="array.numeric.null", members=c("array", 'numeric', "NULL"))
methods::setClassUnion(name="array.list.null", members=c("array", "list", 'numeric', "NULL"))
methods::setClassUnion(name="array.char.null", members=c("array", 'character', "NULL"))
methods::setClassUnion(name="array.char.num", members=c("array", 'character', 'numeric',  "NULL"))
methods::setClassUnion(name="num.array.char.null", members=c('numeric', "array", 'character', "NULL"))
methods::setClassUnion(name="list.null", members=c('list', "NULL"))
methods::setClassUnion(name="num.null", members=c("numeric", "NULL"))
methods::setClassUnion(name="num.list.null", members=c("numeric", 'list', "NULL"))
methods::setClassUnion(name="num.array", members=c("numeric", "array", 'list', "NULL"))
methods::setClassUnion(name="num.array.list", members=c("numeric", "array", 'list', "NULL"))
methods::setClassUnion(name="num.array.df", members=c("numeric", "array", 'data.frame', 'list', "NULL"))
methods::setClassUnion(name="num.log", members=c("numeric", "logical", "NULL"))
methods::setClassUnion(name="logical.array", members=c("logical", "array", "NULL"))
methods::setClassUnion(name="num.Date", members=c("numeric", "Date", 'character', "NULL", 'POSIXct'))
methods::setClassUnion("missingOrcharacter", c("missing", "character"))
methods::setClassUnion("logical_list", c("logical", "list"))



