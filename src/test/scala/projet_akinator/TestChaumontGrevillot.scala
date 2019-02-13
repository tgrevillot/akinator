package projet_akinator

import org.scalatest._

class TestChaumontGrevillot extends FunSuite {
  
  import ChaumontGrevillotObjAkinator._
  
  val a = Question("Est-ce qu'il a des ailes ?",
          Question("Est-ce qu'il a des plumes ?",
          Question("Est-ce qu'il a un goitre ?",
          Animal("Pélican"), Animal("Pigeon")),
          Question("Est-ce qu'il a des poils ?",
          Animal("Chauve-souris"), Animal("Ptérodactyle"))),
          Question("Est-ce qu'il ronronne ?",
          Animal("Chat"), Animal("Chien")));
  
  
  /* Question 1 */
  
  test("jeuSimple bonne reponse") {
    assert(jeuSimple(a, Iterator("o", "o", "o", "o")) === true)
  }
  
  test("jeuSimple mauvaise reponse") {
    assert(jeuSimple(a, Iterator("n", "n", "n")) === false)
  }
  
  
  /* Question 3 */
  test("jeuLog bonne réponse") {
    assert(jeuLog(a, Iterator("o", "o", "o", "o")) === List("o", "o", "o", "o"))
  }
  
  test("jeuLog reponse aleatoire") {
    assert(jeuLog(a, Iterator("n", "n", "n")) === List("n", "n", "n"))
  }
}