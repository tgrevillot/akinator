package projet_akinator

import org.scalatest._

class TestChaumontGrevillot extends FunSuite {
  
  import ChaumontGrevillot._
  
  val a = Question("Est-ce qu'il a des ailes ?",
          Question("Est-ce qu'il a des plumes ?",
          Question("Est-ce qu'il a un goitre ?",
          Animal("Pélican"), Animal("Pigeon")),
          Question("Est-ce qu'il a des poils ?",
          Animal("Chauve-souris"), Animal("Ptérodactyle"))),
          Question("Est-ce qu'il ronronne ?",
          Animal("Chat"), Animal("Chien")));
  
  val apprentissage = Question("Est-ce qu'il a des ailes ?",
                      Question("Est-ce qu'il a des plumes ?",
                      Question("Est-ce qu'il a un goitre ?",
                      Animal("Pélican"), Animal("Pigeon")),
                      Question("Est-ce qu'il a des poils ?",
                      Animal("Chauve-souris"), Animal("Ptérodactyle"))),
                      Question("Est-ce qu'il ronronne ?",
                      Animal("Chat"), Question("Est-ce qu'il nage ?", Animal("Poisson"), Animal("Chien"))));
  
  /* Question 1 & 2 */
  
  test("jeuSimple bonne reponse") {
    assert(jeuSimple(a, Iterator("o", "o", "o", "o")) === true)
  }
  
  test("jeuSimple mauvaise reponse") {
    assert(jeuSimple(a, Iterator("n", "n", "n")) === false)
  }
  
  
  /* Question 3 */
  
  test("jeuLog bonne reponse") {
    assert(jeuLog(a, Iterator("o", "o", "o", "o")) === List("o", "o", "o", "o"))
  }
  
  test("jeuLog reponse aleatoire") {
    assert(jeuLog(a, Iterator("o", "n", "o", "o")) === List("o", "n", "o", "o"))
  }
  
  /* Question 4 */
  
  test("jeuApprentissage (apprend \"Est-ce qu'il nage ?\" -> réponse = \"Poisson\")") {
    assert(jeuApprentissage(a, Iterator("n", "n", "n", "Poisson", "Est-ce qu'il nage ?", "o")) === apprentissage)
  }
  
  /* Question 5 */
  
  test("fichierToABanimal arbre plein (devrait fonctionner)") {
    assert(fichierToABanimal("src/test/scala/projet_akinator/fichierToAnimal.txt") === a)
  }
  
  test("fichierToABanimal fichier vide (devrait lever une exception") {
    intercept[Exception] {
      fichierToABanimal("src/test/scala.projet_akinator/fichierToAnimalVide.txt")
    }
  }
  
  /* Question 6 */
}