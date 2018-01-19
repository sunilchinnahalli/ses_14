package com.scala.test

class calculator(n: Int, d: Int) {
  private def gcd(x: Int, y: Int): Int = {
  if (x == 0) y
  else if (x < 0) gcd(-x, y) else if (y < 0) -gcd(x, -y) else gcd(y % x, x)
  }
  
  private val g = gcd(n, d)
  val numer: Int = n/g 
  val denom: Int = d/g 
  def +(that: calculator) = new calculator(numer * that.denom + that.numer * denom, denom * that.denom)
  def -(that: calculator) = new calculator(numer * that.denom - that.numer * denom, denom * that.denom) 
  def *(that: calculator) = new calculator(numer * that.numer, denom * that.denom) 
  def /(that: calculator) = new calculator(numer * that.denom, denom * that.numer) 
}

object MainObject{
  def main(args:Array[String]){
  var i = 1
  var x = new calculator(0, 1) 
  while (i <= 10) {
        x += new calculator(1, i)
        i += 1 
        }
       println("" + x.numer + "/" + x.denom)
  }
}