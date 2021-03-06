# Darrow's response to SignalPath
## Platform Engineering Code Assessment
1. Rank each of the following languages and frameworks on a 4 point scale, where 1 = "I've been paid to write production quality code with it." and 4 = "I could write a Hello World web application if I had a tutorial."
  * Scala - 4
  * Java - 1
  * Python - 1
  * Clojure - 4
  * Haskell - 3
  * Kotlin - 4
  * Golang - 3
  * Rust - 4
  * Groovy - 2
  * Node.js - 1
  * Ruby - 3
  * Angular - 2
  * Helm - 3
  * Terraform - 3
  * MQL (MongoDB Query Language) - 2
  * SQL - 1
  
1. In any language of your choice, write a utility that will translate a hexadecimal string to base64. Here's a test: this string `45766964696e74` should be converted into this string `RXZpZGludA==`.
1. Download this [simple Python script](https://github.com/SignalPath/CodeTests/blob/master/specialMath.py). When you run it from the command line, it takes one parameter. So `$> python specialMath.py 7` will produce `79`. Also, `$> python specialMath.py 17` will produce `10926`. This question has two parts: first, convert it to Scala; second, have the script calculate `$> specialMath 90'

## Solution:

### 1. 

Although codecs are generally how this should be done in production, it's also possible to do this with a basic lookup table.
Since Unicode tables correspond more or less numerically along with the alphabet order, using character values simplifies things.
I did use a binary integer cast, but that should be relatively straightforward.

### 2. 

I really enjoyed working on this. I used string addition to solve the integer size problem, and performed the addition operations on a digit-by-digit basis. Scala's functional nature makes the zip/map/reduce operations incredibly fast, and makes string addition very fast. 

I also enjoyed the mathematical properties of the 'special math' model. I documented in the Main.scala file the math.


## Afterword:

Thank you for giving me the opportunity to work on this. I hope that you find my solutions entertaining at least.


