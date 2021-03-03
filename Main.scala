import scala.collection.mutable.ArrayBuffer
object Main extends App {
  val seven = call(7)
  println("7 -> " + seven)
  assert(seven == "79")
  val seventeen = call(17)
  println("17 -> " + seventeen)
  assert(seventeen == "10926")
   
  println("90 -> " + call(90))

  def call(target:Int):String = {

		/*
		f(n) = fibonnaci iterative n
		E = summation		 

		z(1) -> 1 = 0 + 1 
		z(2) -> 3 = 2 + 1 + 0
		z(3) -> 7 = 4 + 2 + 1 + 0
		z(4) -> 14 = 7 + 4 + 2 + 1 + 0
		z(5) -> 26 = 12 + 7 + 4 + 2 + 1  + 0 
		z(5)	   =  E(Ef(5), Ef(4),  Ef(3),  Ef(2), Ef(1), Ef(0))
		z(5)	   =  E(f(5) + Ef(4), f(4) + Ef(3), f(3) + Ef(2), f(2) + Ef(1))
		z(5)	   =  E(1(f(5)) + 2(f(4)) + 3(f(3)) + 4(f(2)) + 5(f(1)))

		z(n) = E(1*f(n)...n*f(1))

		*/
		if(target == 0){
			"0"
		} else if(target == 1){
			"1"
		} else {
			var i = 0
			var answer = "0"
			while( i < target) {
				answer = string_addition(answer, string_multiplication((target - i) + "", f( i+1 )))
				i += 1
			}
			answer
		}
		
	}

  //String-based fibonnaci generator
  def f(target:Int):String = {
		var i = 0
		var fib = "1"
		var pfib = "0"
		var zfib = "0"
		if(target == 0){
			pfib
		} else if(target == 1){
			fib
		} else {
			while(i < target -1) {
				
				zfib = string_addition(fib, pfib)
				pfib = fib
				fib = zfib
				i+= 1
			}
			fib
		}

	}

  //String-based multiplication that leverages string addition
  def string_multiplication(a:String, b:String): String = {
		var i = 0
		var sum = "0"
		var multiplier_array = a.toCharArray.map(_.toString).map(_.toInt)
		while(i < multiplier_array.length){
			var j = 0
			var subsum = "0"
			while(j < multiplier_array(i)){
				subsum = string_addition(subsum, b)
				j+= 1
			}
			subsum = subsum.concat("0" * (multiplier_array.length - (i+ 1)))
			sum = string_addition(sum, subsum)
			i+=1

		}
		sum
	}


  // Extract carry digit
  def get_carry(num:Int) = {
		num / 10
	}
  // Extract digit
  def get_digit(num:Int) = {
		num % 10
	}

  val debug = false 
  def string_addition(a:String, b:String):String = {
		var leftchars = a.toCharArray.map(_.toString).reverse
		var rightchars = b.toCharArray.map(_.toString).reverse

		var left = leftchars.map(_.toInt).toArrayBuffer
		var right = rightchars.map(_.toInt).toArrayBuffer

		if(debug) println("left : " + left.toString)
		if(debug) println("right : " + right.toString)

		while(left.length < right.length) {
			left.append(0)
		}

		while(right.length < left.length) {
			right.append(0)
		} //Assert that they have the same length.
		
		var zipped:ArrayBuffer[(Int, Int)] = left.zip(right)
		
		var sums:ArrayBuffer[Int] = zipped.map(sumtuple(_))
		var carried  = resolve_carries(sums)
			
		
		var strPlaces = carried.map(_.toString).reverse
		var numstring = strPlaces.reduceLeft(_.concat(_))
		var numstringarray = numstring.toCharArray()
		var hitNonZero = false  //Eliminate leading zeroes
		var finalnumstring = ""
		for(ch <- numstringarray)
		{	
			if(ch == '0' && !hitNonZero) {
					
			} else if(ch != '0' && !hitNonZero) {
				hitNonZero = true
				finalnumstring += ch
			} else {
				finalnumstring += ch
			}
					
				
		}

		finalnumstring
		
	}

	// There's probably a better way to sum tuples.  
  def sumtuple(tuple:(Int, Int)) = {
		tuple._1 + tuple._2
	}

  def resolve_carries(sums:ArrayBuffer[Int]):ArrayBuffer[Int] = {
		var carries = ArrayBuffer.empty[Int]
		var digits = ArrayBuffer.empty[Int]
		var mysums = sums
		carries = mysums.map(get_carry(_))
		if ( carries.sum > 0 ) {
			carries.prepend(0) //shift over by one
			if(debug) println("carries:" + carries.toString)
	
			digits = mysums.map(get_digit(_))
			mysums = digits.padTo(carries.length, 0).zip(carries).map(sumtuple(_))
			resolve_carries(mysums)
		} else {
			mysums
		}
		// carries resolved, every digit should be less than ten.
	}
}

