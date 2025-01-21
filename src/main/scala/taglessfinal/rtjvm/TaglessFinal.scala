package taglessfinal.rtjvm
import taglessfinal.rtjvm.TaglessFinal.ExpressionProblem.Expr
import taglessfinal.rtjvm.TaglessFinal.ExpressionProblem.aBoolean

object TaglessFinal {

  // Expression problem

  /**
   * A continuación escribimos un programa que sea capaz de evaluar expresion lógicas.
   * Para ello construimos cada expresión con una case class y a continuación implementamos
   * un método eval que dada una expresión algebraica calcule su resultado
   */
  object ExpressionProblem {
    trait Expr
    case class B(boolean: Boolean) extends Expr
    case class Or(l: Expr, r: Expr) extends Expr
    case class And(l: Expr, r: Expr) extends Expr
    case class Not(expr: Expr) extends Expr
    
    val aBoolean: Expr = Or(And(B(true), B(false)), B(false))
    
    def eval(expr: Expr): Boolean = expr match
      case B(b) => b
      case Or(l, r) => eval(l) || eval(r)
      case And(l, r) => eval(l) && eval(r)
      case Not(e) => !eval(e)
    
    // Include Ints

    /**
     * Ahora decidimos aumentar las capacidades de cómputo de nuestro programa y añadimos la suma
     * de número enteros
     */
    case class I(int: Int) extends Expr
    case class Sum(l: Expr, r: Expr) extends Expr

    /**
     * Como podemos ver no nos sirve el método arriba definido y debemos reimplementarlo de la
     * siguiente forma. Por lo que nuestro código no es reutilizable ni ampliable
     */
    def eval2(expr: Expr): Boolean | Int = expr match
      case B(b) => b
      case Or(l, r) => eval(l).asInstanceOf[Boolean] || eval(r).asInstanceOf[Boolean]
      case And(l, r) => eval(l).asInstanceOf[Boolean] && eval(r).asInstanceOf[Boolean]
      case Not(e) => !eval(e).asInstanceOf[Boolean]
      // cats everywhere
  }

  /**
   * Una primera solución es añadir tags. Se hace una comprobación de tipos mediante
   * una etiqueta, pero esto no es una solución que nos aporte seguridad en tiempo de
   * ejecución.
   */
  object Solution1_Tagging {
    trait Expr(val tag: String)

    case class B(boolean: Boolean) extends Expr("bool")
    case class Or(l: Expr, r: Expr) extends Expr("bool")

    /**
     * Con el método assert le decimos al constructor que solo acepte determinados tags
     */
    case class And(l: Expr, r: Expr) extends Expr("bool") {
      assert(l.tag == "bool" && r.tag == "bool")
    }
    case class Not(expr: Expr) extends Expr("bool")

    case class I(int: Int) extends Expr("int")
    case class Sum(l: Expr, r: Expr) extends Expr("int")
    
    def eval(expr: Expr): Any = expr match
      case B(b) => b
      case Or(l, r) =>
        if (l.tag != "bool" || r.tag != "bool")
          throw new IllegalArgumentException("improper argument type") // podemos eliminar esto limitando los tipos en el constructor con assert.
        else
          eval(l).asInstanceOf[Boolean] || eval(r).asInstanceOf[Boolean]
      // same for the rest
      case And(l, r) => eval(l) && eval(r) // aquí no es necesario lanzar la exception porque en el constructor estamos limitando los tags aceptados
    
    /*
    * Esta solución no es tal, ya que en runtime puede seguir fallando igualmente
    * */
  }

  /**
   * La solución al problema de las etiquedas es Tagless.
   * Dado que las etiquetas en la solución anterior esencialmente agregaron
   * información de tipo al tiempo de ejecución, podemos eliminar las etiquetas
   * y dejar que el compilador realice la verificación de tipo automáticamente.
   * 
   * Esta es una solución sin etiquetas , porque hemos eliminado las etiquetas.
   * Se llama tagless initial , porque trabajamos con estructuras de datos
   * intermedias, no con los valores que nos interesan. Eso sería tagless final
   */
  object Solution2_Tagless {
    trait Expr[A]
    
    case class B(boolean: Boolean) extends Expr[Boolean]
    case class Or(l: Expr[Boolean], r: Expr[Boolean]) extends Expr[Boolean]
    case class And(l: Expr[Boolean], r: Expr[Boolean]) extends Expr[Boolean]
    case class Not(expr: Expr[Boolean]) extends Expr[Boolean]

    case class I(int: Int) extends Expr[Int]
    case class Sum(l: Expr[Int], r: Expr[Int]) extends Expr[Int]
    
    def eval[A](expr: Expr[A]): A = expr match
      case B(b) => b
      case I(i) => i
      case Or(l, r) => eval(l) || eval(r)
      case And(l, r) => eval(l) && eval(r)
      case Not(e) => !eval(e)
      case Sum(l, r) => eval(l) + eval(r)
  }
  
  def demoTagless(): Unit = {
    import Solution2_Tagless.*
    println(s" eval of Or(B(true), And(B(true)), B(false)) is ${eval(Or(B(true), And(B(true), B(false))))}")
    println(s" eval of Sum(I(23), I(8)) is ${eval(Sum(I(23), I(8)))}")
  }

  /**
   * Hay otro paso que podemos dar. No solo podemos eliminar las etiquetas,
   * sino que también podemos representar inmediatamente estas expresiones
   * en términos del valor evaluado que nos interesa (el valor final).
   * Esto es final sin etiquetas .
   * 
   * Expr[A] tiene el valor evaluado directamente en la instancia, como miembro.
   * Cada construcción de otra Exprdel tipo correcto ya tiene el valor final
   * incorporado allí. Por lo tanto, nuestra función de evaluación está casi vacía,
   * porque unicamente lo que necesitamos hacer es devolver el valor incorporado
   * en la expresión que se pasa como argumento.
   */
  object TaglessFinal {
    trait Expr[A] {
      val value: A // final value
    }
    
    def b(boolean: Boolean): Expr[Boolean] = new Expr[Boolean] {
      val value = boolean
    }
    
    def i(int: Int): Expr[Int] = new Expr[Int] {
      val value = int
    }
    
    def or(l: Expr[Boolean], r: Expr[Boolean]): Expr[Boolean] = new Expr[Boolean] {
      val value = l.value || r.value
    }

    def and(l: Expr[Boolean], r: Expr[Boolean]): Expr[Boolean] = new Expr[Boolean] {
      val value = l.value && r.value
    }

    def sum(l: Expr[Int], r: Expr[Int]): Expr[Int] = new Expr[Int] {
      val value = l.value + r.value
    }
    
    def eval[A](expr: Expr[A]): A = expr.value
  }

  def demoFinalTagless(): Unit = {
    import TaglessFinal.*
    println(
      s" tagless final - eval of Or(B(true), And(B(true)), B(false)) is ${eval(or(b(true), and(b(true), b(false))))}"
    )
    println(s" tagless final - eval of Sum(I(23), I(8)) is ${eval(sum(i(23), i(8)))}")
  }
  
  // Using type classes

  /**
   * ¿Dónde está F[_]?
   * Esto son clases de tipos:
   * Las clases de tipos son conjuntos de funcionalidades que desea ofrecer a algunos tipos y no a otros.
   * La función tagless final quiere demostrar la corrección de expresiones de algunos tipos y no de otros.
   * 
   * Podemos agrupar todas nuestras funcionalidades, es decir, la capacidad de construir expresiones,
   * operandos y operadores en una única clase de tipo, implementada en términos de un tipo abstracto E:
   */
  object TaglessFinal2 {
    trait Algebra[E[_]] {
      def b(boolean: Boolean): E[Boolean]
      def i(int: Int): E[Int]
      def or(l: E[Boolean], r: E[Boolean]): E[Boolean]
      def and(l: E[Boolean], r: E[Boolean]): E[Boolean]
      def sum(l: E[Int], r: E[Int]): E[Int]
    }

    /**
     * Implementamos un interprete para nuestro trait
     */
    case class SimpleExpr[A](value: A)
    given simpleAlgebra: Algebra[SimpleExpr] with {
      override def b(boolean:  Boolean): SimpleExpr[Boolean] = SimpleExpr(boolean)

      override def i(int:  Int): SimpleExpr[Int] = SimpleExpr(int)

      override def or(l:  SimpleExpr[Boolean], r:  SimpleExpr[Boolean]): SimpleExpr[Boolean] =
        SimpleExpr(l.value || r.value)

      override def and(l:  SimpleExpr[Boolean], r:  SimpleExpr[Boolean]): SimpleExpr[Boolean] =
        SimpleExpr(l.value && r.value)

      override def sum(l:  SimpleExpr[Int], r:  SimpleExpr[Int]): SimpleExpr[Int] =
        SimpleExpr(l.value + r.value)
    }

    /**
     * Esto son los programas que usan los interpretes para alcanzar un objetivo
     */
    def program1[E[_]](using alg: Algebra[E]): E[Boolean] = {
      import alg.*
      or(b(true), and(b(true), b(false)))
    }

    def program2[E[_]](using alg: Algebra[E]): E[Int] = {
      import alg.*
      sum(i(23), i(8))
    }
  }

  def demoFinalTagless2(): Unit = {
    import TaglessFinal2.*
    println(program1[SimpleExpr].value)
    println(program2[SimpleExpr].value)
  }
  
  // Un ejemplo real
  object RealExample {
    trait UserLogin[F[_]] {
      def checkLogin(mfa: Boolean): F[Boolean] // mfa = multi-factor authentication
      def lastErrorStatus(code: Int): F[Int]
      def mfa_v1(email: F[Boolean], sms: F[Boolean]): F[Boolean]
      def mfa_v2(phone: F[Boolean], mobileApp: F[Boolean]): F[Boolean]
      def totalSessionLogins(server1Logins: F[Int], server2Logins: F[Int]): F[Int]
    }
    
    case class UserLoginStatus[A](value: A)
    given loginCapabilityImpl: UserLogin[UserLoginStatus] with {
      override def checkLogin(mfa: Boolean): UserLoginStatus[Boolean] = UserLoginStatus(mfa)
      override def lastErrorStatus(code: Int): UserLoginStatus[Int] = UserLoginStatus(code)
      override def mfa_v1(email: UserLoginStatus[Boolean], sms: UserLoginStatus[Boolean]): UserLoginStatus[Boolean] =
        UserLoginStatus(email.value || sms.value)
      override def mfa_v2(phone: UserLoginStatus[Boolean], mobileApp: UserLoginStatus[Boolean]): UserLoginStatus[Boolean] =
        UserLoginStatus(phone.value && mobileApp.value)
      override def totalSessionLogins(server1Logins: UserLoginStatus[Int], server2Logins: UserLoginStatus[Int]): UserLoginStatus[Int] =
        UserLoginStatus(server1Logins.value + server2Logins.value)
    }

    def userLoginFlow[E[_]](using alg: UserLogin[E]): E[Boolean] = {
      import alg.*
      mfa_v1(checkLogin(true), mfa_v2(checkLogin(true), checkLogin(false)))
    }

    def checkLastStatus[E[_]](using alg: UserLogin[E]): E[Int] = {
      import alg.*
      totalSessionLogins(lastErrorStatus(23), lastErrorStatus(8))
    }
  }
  
  def demoUserLogin(): Unit = {
    import RealExample.*
    println(userLoginFlow[UserLoginStatus].value)
    println(checkLastStatus[UserLoginStatus].value)
  }
  
  def main(args: Array[String]): Unit = {
    demoTagless()
    demoFinalTagless()
    demoFinalTagless2()
    demoUserLogin()
  }
  
}
