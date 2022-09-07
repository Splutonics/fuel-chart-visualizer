#Vectors

#Logical
vtr1 = c(TRUE, FALSE)

#Numeric
vtr2 = c(15, 85.675954, 9999999)

#Integer
vtr3 = c(35L,58L,146L)

vtr5 = c(TRUE, 354L, 3.14)
#turns all into numeric

vtr6 = c("Hello", FALSE,65L, 33.66)
#turns all into string/character

#------------------------

mtr = matrix(c(5:29),5,5,TRUE)

#----------

arr = array(c(1:9), dim = c(3,3,4,2))

#--------------

vtr7 = c(5.123,32,95,31.345)
vtr8 = c("hey", "wjats", "tjx")

mylist = list(vtr7,vtr8)

print (TRUE && FALSE)


#---------
fibo <- function( a){
  var1 = 0
  var2 = 1
  print( var1)
  print( var2)
  for(x in 1:a){
    var3 = var1 + var2
    print(var3)
    var1 = var2
    var2 = var3
  }
}
fibo(15)
