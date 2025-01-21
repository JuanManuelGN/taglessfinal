package taglessfinal.rtjvm
import taglessfinal.rtjvm.TaglessFinal.ExpressionProblem.Expr
import taglessfinal.rtjvm.TaglessFinal.ExpressionProblem.aBoolean

object TaglessFinal {

  // Expression problem
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
    case class I(int: Int) extends Expr
    case class Sum(l: Expr, r: Expr) extends Expr
    
    def eval2(expr: Expr): Boolean | Int = expr match
      case B(b) => b
      case Or(l, r) => eval(l).asInstanceOf[Boolean] || eval(r).asInstanceOf[Boolean]
      case And(l, r) => eval(l).asInstanceOf[Boolean] && eval(r).asInstanceOf[Boolean]
      case Not(e) => !eval(e).asInstanceOf[Boolean]
      // cats everywhere
  }
  
  object Solution1_Tagging {
    trait Expr(val tag: String)

    case class B(boolean: Boolean) extends Expr("bool")
    case class Or(l: Expr, r: Expr) extends Expr("bool")
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
    
    /*
    * Esta solución no es tal, ya que en runtime puede seguir fallando igualmente
    * */
  }
  
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
  
  object TaglessFinal {
    trait Expr[A] {
      val value: A
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
  object TaglessFinal2 {
    trait Algebra[E[_]] {
      def b(boolean: Boolean): E[Boolean]
      def i(int: Int): E[Int]
      def or(l: E[Boolean], r: E[Boolean]): E[Boolean]
      def and(l: E[Boolean], r: E[Boolean]): E[Boolean]
      def sum(l: E[Int], r: E[Int]): E[Int]
    }
    
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
