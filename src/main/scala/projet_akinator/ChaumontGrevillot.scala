package projet_akinator

import scala.io._
import java.io._

object ChaumontGrevillot {
  
  trait ABanimal
  case class Animal(nom: String) extends ABanimal
  case class Question(q: String, oui: ABanimal, non: ABanimal) extends ABanimal
 
  val a = Question("Est-ce qu'il a des ailes ?",
          Question("Est-ce qu'il a des plumes ?",
          Question("Est-ce qu'il a un goitre ?",
          Animal("Pélican"), Animal("Pigeon")),
          Question("Est-ce qu'il a des poils ?",
          Animal("Chauve-souris"), Animal("Ptérodactyle"))),
          Question("Est-ce qu'il ronronne ?",
          Animal("Chat"), Animal("Chien")));
          
  /* Question 1 / 2 */
  
  def jeuSimple(a: ABanimal, it: Iterator[String]): Boolean = a match {
    case Question(q, oui, non) => {
      println(q)
      if(it.next() == "o") {
        jeuSimple(oui, it)
      } else {
        jeuSimple(non, it)
      }
    }
    case Animal(nom) => {
      println("Pensez-vous à : " + nom + " ?")
      if(it.next() == "o") {
        println("J'ai gagné")
        true
      } else {
        println("J'ai perdu")
        false
      }
    }
  }
  
  /* Question 3 */
  
  def jeuLog(a: ABanimal, it: Iterator[String]): List[String] = a match {
    case Question(q, oui, non) => {
      println(q)
      if(it.next() == "o")
        "o"::jeuLog(oui, it)
      else
        "n"::jeuLog(non, it)
    }
    case Animal(nom) => {
      println("Pensez-vous à : " + nom + " ?")
      if(it.next() == "o") {
        println("J'ai gagné")
        "o"::Nil
      } else {
        println("J'ai perdu")
        "n"::Nil
      }
    }
  }
  
  /* Question 4 */
  
  def jeuApprentissage(a: ABanimal, it: Iterator[String]): List[String] = a match {
    case Question(q, oui, non) => {
      println(q)
      if(it.next() == "o") jeuLog(oui, it)
      else jeuLog(non, it)
    }
    case Animal(nom) => {
      println("Pensez-vous à : " + nom + " ?")
      if(it.next() == "o") {
        println("J'ai gagné!")
      } else {
        println("J'ai perdu, quelle était la bonne réponse ?")
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    jeuSimple(a, Source.stdin.getLines)
  }
}