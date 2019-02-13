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
  
  test("jeuSimple bonne reponse") {
    assert(jeuSimple(a, Iterator("o", "o", "o", "o")) === true)
  }
  
  test("jeuSimple mauvaise reponse") {
    assert(jeuSimple(a, Iterator("n", "n", "n")) === false)
  }
}