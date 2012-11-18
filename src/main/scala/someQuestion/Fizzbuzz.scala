package someQuestion

import scalaz._
import Scalaz._
import scalaz.effects._

object FizzBuzz{
  /**
   * "Write a program that prints the numbers from 1 to 100. But for multiples of three print “Fizz” instead of the number and for the
   * multiples of five print “Buzz”. For numbers which are multiples of both three and five print “FizzBuzz”."
   */

  def main(args:Array[String])={
    1.to(100).foldLeft(List[IO[Unit]]())((ioList, number)=>{
      if(number%3==0 && number%5==0){
        ioList:+io{
          println("_FizzBuzz_")
        }
      }
      else if(number% 3== 0){
        ioList:+io{
          println("_Fizz_")
        }
      }
      else if(number%5==0){
        ioList:+io{
          println("_Buzz_")
        }
      }
      else{
        ioList:+io{
          println(number)
        }
      }
    }).sequence.unsafePerformIO
  }
}
