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
      if(it.next() == "o")
        jeuSimple(oui, it)
      else
        jeuSimple(non, it)
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
  
  def jeuApprentissage(a: ABanimal, it: Iterator[String]): ABanimal = a match {
    case Question(q, oui, non) => {
      println(q)
      if(it.next() == "o") 
        Question(q, jeuApprentissage(oui, it), non)
      else 
        Question(q, oui, jeuApprentissage(non, it))
    }
    case Animal(nom) => {
      println("Pensez-vous à : " + nom + " ?")
      if(it.next() == "o") {
        println("J'ai gagné!")
        a
      } else {
        println("J'ai perdu, quelle était la bonne réponse ?")
        val reponseAttendue = it.next()
        println("Quelle question permet de différencier \"" + nom + "\" et \"" + reponseAttendue + "\"")
        val nouvelleQuestion = it.next()
        println("Quelle est la réponse à cette question (o/n) ?")
        if(it.next() == "o") {
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
      if(res startsWith("q:"))
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
  
  def jeuSimpleJNSP(a: ABanimal, it: Iterator[String]): Boolean = {
		 def aux(a: ABanimal, l: List[ABanimal]): Boolean = a match {
		  case Question(q, oui, non) => {
			  println(q)
			  val reponse = it.next()
			  if(reponse == "o") jeuSimpleJNSP(oui, it)
			  else if(reponse == "n") jeuSimpleJNSP(non, it)
			  else {
			    if(aux(oui, non::Nil))
					  true
					else
						aux(non, l)
			  }
		  }
		  case Animal(nom) => {
			  println("Pensez-vous à : " + nom + " ?")
			  if(it.next() == "o") {
				  println("J'ai gagné")
				  true
			  } else {
				  if(l == Nil) {
					  println("J'ai perdu")
					  false
				  } else
					  false
		    }
		  }
		 }
		 aux(a, Nil)
  }
  
  /* Question 8 */
  
  def lancement(it: Iterator[String], choix: String): Unit = {
    val default = fichierToABanimal("src/test/scala/projet_akinator/default.txt")
    choix match {
      case "1" => jeuSimple(default, it)
      case "2" => ABanimalToFichier("src/test/scala/projet_akinator/default.txt", jeuApprentissage(default, it))
      case "3" => jeuSimpleJNSP(default, it)
      case _ => println("\n--- FIN DU JEU ---")
    }
    println()
    println("Voulez-vous rejouer ? (o/n [q pour quitter])")
    if(it.next() == "o") {
      println("Mode 1, 2 ou 3 ? [q pour quitter]")
      it.next() match {
        case "1" => lancement(it, "1")
        case "2" => lancement(it, "2")
        case "3" => lancement(it, "3")
        case _ => println("\n--- FIN DU JEU ---")
      } 
    } else {
      println("\n--- FIN DU JEU ---")
    }
  }
  
  def main(args: Array[String]): Unit = {
    println("Bienvenue dans la version d'Akinator qui va deviner à quel animal vous pensez!")
    println("Tapez 1, 2 ou 3 en fonction de la version d'Akinator à laquelle vous voulez jouer :")
    println("\t(1) Jeu simple\n\t(2) Jeu par apprentissage\n\t(3) Jeu où vous pourrez répondre je ne sais pas (\'x\' sur le clavier)")
    val it = Source.stdin.getLines
    lancement(it, it.next())
  }
}