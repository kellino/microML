compose
**compose**
** compose :: (b -> c) -> (a -> b) -> a -> c **
compose takes two unary functions and links the results together
like this: a to a1 to a2
#Example:#
    compose succ succ 3 to 5
It works like this
    result = 5 <- (succ 4 = 5) <- (succ 3 = 4) <- 3\n\n") 
