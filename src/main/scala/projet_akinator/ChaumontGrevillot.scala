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
          
  /* Question 1 & 2 */
  
  def jeuSimple(a: ABanimal, it: Iterator[String]): Boolean = a match {
    case Question(q, oui, non) => {
      println(q)
      if(it.next().equals("o")) {
        jeuSimple(oui, it)
      } else {
        jeuSimple(non, it)
      }
    }
    case Animal(nom) => {
      println("Pensez-vous à : " + nom + " ?")
      if(it.next().equals("o")) {
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
      if(it.next().equals("o"))
        "o"::jeuLog(oui, it)
      else
        "n"::jeuLog(non, it)
    }
    case Animal(nom) => {
      println("Pensez-vous à : " + nom + " ?")
      if(it.next().equals("o")) {
        println("J'ai gagné")
        "o"::Nil
      } else {
        println("J'ai perdu")
        "n"::Nil
      }
    }
  }
  
  /* Question 4 */
  
  def jeuApprentissage(a: ABanimal, it: Iterator[String]): ABanimal = a match {
    case Question(q, oui, non) => {
      println(q)
      if(it.next().equals("o")) 
        Question(q, jeuApprentissage(oui, it), non)
      else 
        Question(q, oui, jeuApprentissage(non, it))
    }
    case Animal(nom) => {
      println("Pensez-vous à : " + nom + " ?")
      if(it.next().equals("o")) {
        println("J'ai gagné!")
        a
      } else {
        println("J'ai perdu, quelle était la bonne réponse ?")
        val reponseAttendue = it.next()
        println("Quelle question permet de différencier \"" + nom + "\" et \"" + reponseAttendue + "\"")
        val nouvelleQuestion = it.next()
        println("Quelle est la réponse à cette question (o/n) ?")
        if(it.next().equals("o")) {
          Question(nouvelleQuestion, Animal(reponseAttendue), a)
        } else {
          Question(nouvelleQuestion, a, Animal(reponseAttendue))
        }
      }
    }
  }
  
  /* Question 5 */
  
  def fichierToABanimal(nomf: String): ABanimal = {    
    def aux(it: Iterator[String]): ABanimal = {
      val res = it.next()
      if(res.startsWith("q:"))
        Question(res substring(2), aux(it), aux(it))
      else
        Animal(res)
    }
    aux(Source.fromFile(nomf).getLines)
  }
  
  /* Question 6 */
  
  def ABanimalToFichier(nomf: String, a: ABanimal): Unit = {
    val writer = new FileWriter(new File(nomf))
    def aux(arbre: ABanimal): Unit = arbre match {
      case Question(q, oui, non) => {
         writer.write("q:" + q + "\n")
         aux(oui)
         aux(non)
      }
      case Animal(nom) => writer.write(nom + "\n")
    }
    aux(a)
    writer.close()
  }
  
  /* Question 7 */
  
  
  
  def main(args: Array[String]): Unit = {
    //jeuSimple(a, Source.stdin.getLines)
    //jeuApprentissage(a, Source.stdin.getLines)
    ABanimalToFichier("src/test/scala/projet_akinator/animalToFichier.txt", a)
  }
}