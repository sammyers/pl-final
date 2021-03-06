//
// language FUNC + continuations
//


//
//  Values
//


abstract class Value {

  def valueError (msg: String) : Nothing = {
    throw new Exception("Runtime error: "+msg)
  }

  def getInt () : Int =
    valueError("value "+this+" not an integer")

  def getBool () : Boolean =
    valueError("value "+this+" not a Boolean")

  def getList () : List[Value] =
    valueError("value "+this+" not a vector")

  // we still need to distinguish primitive operations from functions :/
  def isPrimOp () : Boolean = false
  def isPrimK () : Boolean = false

  def isBreakpoint () : Boolean = false

  def apply (args:List[Value]) : Value =
    valueError("value "+this+" not a function or operation")

  def getParams () : List[String] =
    valueError("value "+this+" not a function")

  def getSelf () : String =
    valueError("value "+this+" not a function")

  def getBody () : Exp =
    valueError("value "+this+" not a function")

  def getEnv () : Env[Value] =
    valueError("value "+this+" not a function")

  def getContinuations () : List[Exp] =
    valueError("value "+this+" not a breakpoint")

  def getReturnExp () : Exp =
    valueError("value "+this+" not a breakpoint")
}


object VNone extends Value {

  override def toString () : String = "none"
}


class VInteger (val i:Int) extends Value {

  override def toString () : String = i.toString()
  override def getInt () : Int = i
}


class VBoolean (val b:Boolean) extends Value {

  override def toString () : String = b.toString()
  override def getBool () : Boolean = b
}


class VVector (val l:List[Value]) extends Value {

  override def toString () : String =
    return l.addString(new StringBuilder(), "[ ", " ", " ]").toString()

  override def getList () : List[Value] = l
}


class VPrimOp (val oper : (List[Value]) => Value) extends Value {

  override def toString () : String = "primop(" + oper + ")"

  override def isPrimOp () : Boolean = true
  override def apply (args:List[Value]) : Value = oper(args)
}

class VPrimK(val oper : (Value) => Value) extends Value {

  override def toString () : String = "primk(" + oper + ")"

  override def isPrimK () : Boolean = true
  override def apply (args:List[Value]) : Value = oper(args.head)
}


class VRecClosure (val self: String, val params: List[String], val body:Exp, val env:Env[Value]) extends Value {

  override def toString () : String = params + " | " + self + " => " + body

  override def apply (vargs : List[Value]) : Value = {
    var new_env = env.push(self,this)
    for ((p,v) <- params.zip(vargs)) {
      new_env = new_env.push(p,v)
    }
    return body.eval(new_env)
  }

  override def getParams () : List[String] = params
  override def getSelf () : String = self
  override def getBody () : Exp = body
  override def getEnv () : Env[Value] = env
}


class VBreakpoint (val env: Env[Value], val continuations: List[Exp], val returnExp: Exp) extends Value {
  override def toString () : String = "breakpoint"

  override def isBreakpoint () : Boolean = true
  override def getEnv () : Env[Value] = env
  override def getContinuations () : List[Exp] = continuations
  override def getReturnExp () : Exp = returnExp
}



//
//  Primitive operations
//

object Ops {

  def runtimeError (msg: String) : Nothing = {
    throw new Exception("Runtime error: "+msg)
  }

  def checkArgs (vs:List[Value],n:Int) : Unit = {
    if (vs.length != n) {
      runtimeError("Wrong # of arguments to operation")
    }
  }

  def operPlus (vs:List[Value]) : Value = {
    checkArgs(vs,2)
    val v1 = vs(0)
    val v2 = vs(1)

    return new VInteger(v1.getInt() + v2.getInt())
  }


  def operTimes (vs: List[Value]):Value = {
    checkArgs(vs,2)
    val v1 = vs(0)
    val v2 = vs(1)

    return new VInteger(v1.getInt() * v2.getInt())
  }


  def operEqual (vs: List[Value]) : Value = {
    checkArgs(vs,2)
    val v1 = vs(0)
    val v2 = vs(1)

    return new VBoolean(v1.getInt() == v2.getInt())
  }


  def operLess (vs: List[Value]) : Value = {
    checkArgs(vs,2)
    val v1 = vs(0)
    val v2 = vs(1)

    return new VBoolean(v1.getBool() < v2.getBool())
  }


  def operEmpty (vs : List[Value]) : Value = {
    checkArgs(vs,1)
    val v = vs(0)
    return new VBoolean(v.getList().length == 0)
  }


  def operFirst (vs : List[Value]) : Value = {
    checkArgs(vs,1)
    val v = vs(0)
    val l = v.getList()
    if (l.length == 0) {
      runtimeError("Taking first of an empty vector")
    }
    return l(0)
  }


  def operRest (vs : List[Value]) : Value = {
    checkArgs(vs,1)
    val v = vs(0)
    val l = v.getList()
    if (l.length == 0) {
      runtimeError("Taking rest of an empty vector")
    }
    return new VVector(l.tail)
  }


  def operCons (vs : List[Value]) : Value = {
    checkArgs(vs,2)
    val item = vs(0)
    val vec = vs(1)
    return new VVector(item::vec.getList())
  }


  def operPrint (vs:List[Value]) : Value = {
    for (v <- vs) {
      print(v)
      print(" ")
    }
    println(" ")
    return VNone
  }

}



//
//  Expressions
//


class Env[A] (val content: List[(String, A)]) {

  override def toString () : String = {
    var result = ""
    for (entry <- content) {
       result = result + "(" + entry._1 + " <- " + entry._2 + ")\n"
    }
    return result
  }


  def push (id : String, v : A) : Env[A] =
    // push a single binding (id,v) on top of the environment
    new Env[A]((id,v)::content)


  def getContent () : List[(String, A)] =
    // get list of entries in environment
    return content


  def lookup (id : String) : A = {
    // lookup value for an identifier in the environment
    for (entry <- content) {
      if (entry._1 == id) {
         return entry._2
      }
    }
  throw new Exception("Environment error: unbound identifier "+id)
  }
}


object Names {

  var counter = 0
  def next () : String = {
    val c = counter
    counter = counter + 1
    return "  x"+counter
  }
}

abstract class Exp {

  def error (msg : String) : Nothing = {
    throw new Exception("Eval error: "+ msg + "\n   in expression " + this)
  }

  def cps (K1 : Exp, K2 : Exp) : Exp

  def eval (env : Env[Value]) : Value

  def insertBreakpoint (position: Int) : Int = -1

  def canPause () : Boolean = false

  def isBreakpoint () : Boolean = false

  def getExp () : Exp = this

  def getDebugReadable () : String = ""
}


case class EInteger (val i:Integer) extends Exp {
  // integer literal

  override def toString () : String =
    "EInteger(" + i + ")"

  def cps (K1 : Exp, K2 : Exp) : Exp =
    new EApply(K1, List(this))

  def eval (env:Env[Value]) : Value =
    new VInteger(i)

  override def getDebugReadable () : String = i.toString()
}


case class EBoolean (val b:Boolean) extends Exp {
    // boolean literal

    override def toString () : String =
      "EBoolean(" + b + ")"

    def cps (K1 : Exp, K2 : Exp) : Exp =
      new EApply(K1, List(this))

    def eval (env:Env[Value]) : Value =
      new VBoolean(b)

    override def getDebugReadable () : String = b.toString()
}


case class EVector (var es: List[Exp]) extends Exp {
  // Vectors

  override def toString () : String =
    "EVector" + es.addString(new StringBuilder(),"(", " ", ")").toString()

  def cps (K1 : Exp, K2 : Exp) : Exp = {
    // create names for expressions
    val names = es.map((e) => Names.next())
    var result : Exp = new EApply(K1, List(EVector(names.map((n) => new EId(n)))))
    for ((e,n) <- es.zip(names)) {
      result = e.cps(new ERecFunction("", List(n), result),K2)
    }
    return result
  }

  def eval (env : Env[Value]) : Value = {
    val vs = es.map((e:Exp) => e.eval(env))
    return new VVector(vs)
  }

  override def insertBreakpoint (position: Int) : Int = {
    if (position > es.length - 1) {
      return -1
    }
    for (x <- position until es.length) {
      if (es(x).canPause()) {
        if (x > 0) {
          es = es.patch(x - 1, Seq(es(x-1).getExp()), 1)
        }
        if (!es(x).isBreakpoint()) {
          es = es.patch(x, Seq(new EBreakpoint(es(x))), 1)
        }
        return x
      }
    }
    return -1
  }

  override def canPause () : Boolean = {
    for (e <- es) {
      if (e.canPause()) {
        return true
      }
    }
    return false
  }

  override def getDebugReadable () : String = {
    val strings : List[String] = es.map(_.getDebugReadable())
    return "[" + strings.mkString(" ") + "]"
  }
}


case class EIf (var ec : Exp, var et : Exp, var ee : Exp) extends Exp {
  // Conditional expression

  override def toString () : String =
    "EIf(" + ec + "," + et + "," + ee +")"

  def cps (K1 : Exp, K2 : Exp) : Exp = {
    val n = Names.next()
    return ec.cps(new ERecFunction("", List(n), new EIf(new EId(n), et.cps(K1,K2), ee.cps(K1,K2))),K2)
  }

  def eval (env:Env[Value]) : Value = {
    val cv = ec.eval(env)
    if (!cv.getBool()) {
      return ee.eval(env)
    } else {
      return et.eval(env)
    }
  }

  override def insertBreakpoint (position: Int) : Int = {
    position match {
      case 0 => {
        ec = new EBreakpoint(ec)
        return position
      }
      case 1 => {
        ec = ec.getExp()
        et = new EBreakpoint(et)
        ee = new EBreakpoint(ee)
        return 2
      }
      case _ => {
        return -1
      }
    }
  }

  override def canPause () : Boolean = true

  override def getDebugReadable () : String = {
    return "(if " + ec.getDebugReadable() + " " + et.getDebugReadable() + " " + ee.getDebugReadable() + ")"
  }
}


case class EId (val id : String) extends Exp {

  override def toString () : String =
    "EId(" + id + ")"

  def cps (K1 : Exp, K2 : Exp) : Exp =
    return new EApply(K1, List(this))

  def eval (env : Env[Value]) : Value =
    env.lookup(id)

  override def getDebugReadable () : String = id
}


case class EApply (var f: Exp, var args: List[Exp]) extends Exp {
  override def toString () : String =
    "EApply(" + f + "," + args + ")"

  def cps (K1 : Exp, K2 : Exp) : Exp = {
    // create names for expressions
    val names = args.map((e) => Names.next())
    val fname = Names.next()
    val nargs = names.map((n) => new EId(n))
    var result = f.cps(new ERecFunction("", List(fname), new EApply(new EId(fname), K1::K2::nargs)),K2)
    for ((e,n) <- args.zip(names)) {
      result = e.cps(new ERecFunction("", List(n), result),K2)
    }
    return result
  }

  def eval (env : Env[Value]) : Value = {
    val vf = f.eval(env)
    val vargs = args.map((e:Exp) => e.eval(env))
    if (vf.isPrimOp()) {
      return vargs.head.apply(List(vf.apply(vargs.tail.tail)))
    } else if (vf.isPrimK()) {
      return vf.apply(vargs)
    } else {
      // defined function
      // push the vf closure as the value bound to identifier self
      var new_env = vf.getEnv().push(vf.getSelf(),vf)
      for ((p,v) <- vf.getParams().zip(vargs)) {
        new_env = new_env.push(p,v)
      }
      return vf.getBody.eval(new_env)
    }
  }

  override def insertBreakpoint (position: Int) : Int = {
    if (position > args.length) {
      return -1
    } else if (position == 0 && f.canPause()) {
      f = new EBreakpoint(f)
      return position
    } else {
      for (x <- position until args.length) {
        if (args(x).canPause()) {
          if (!args(x).isBreakpoint()) {
            args = args.patch(x, Seq(new EBreakpoint(args(x))), 1)
          }
          return x
        } else {
          args = args.patch(x - 1, Seq(args(x - 1).getExp()), 1)
        }
      }
      return -1
    }
  }

  override def canPause () : Boolean = true

  override def getDebugReadable () : String =
    "(" + f.getDebugReadable() + " " + args.map(_.getDebugReadable()).mkString(" ") + ")"
}


case class ERecFunction (val self: String, val params: List[String], var body : Exp) extends Exp {

  override def toString () : String =
    "ERecFunction(" + self + "," + params + "," + body + ")"

  def cps (K1 : Exp, K2 : Exp) : Exp = {
    val n1 = Names.next()
    val n2 = Names.next()
    return new EApply(K1, List(new ERecFunction(self, n1::n2::params, body.cps(new EId(n1),new EId(n2)))))
  }

  def eval (env : Env[Value]) : Value =
    new VRecClosure(self,params,body,env)

  override def insertBreakpoint (position: Int) : Int = {
    if (position > 0) {
      return -1
    }
    if (!body.isBreakpoint()) {
      body = new EBreakpoint(body)
    }
    return position
  }

  override def canPause () : Boolean = true

  override def getDebugReadable () : String =
    "(fun " + self + " (" + params.mkString(" ") + ") " + body.getDebugReadable() + ")"
}


case class ELet (var bindings : List[(String,Exp)], var ebody : Exp) extends Exp {

  override def toString () : String =
    "ELet(" + bindings + "," + ebody + ")"

  def cps (K1 : Exp, K2 : Exp) : Exp = {
    // create names for expressions
    val names = bindings.map((_) => Names.next())
    val nbindings = bindings.zip(names).map({ case ((n,e),nnew) => (n,new EId(nnew)) })
    var result : Exp = new ELet(nbindings,ebody.cps(K1,K2))
    for (((n,e),nnew) <- bindings.zip(names)) {
      result = e.cps(new ERecFunction("", List(nnew), result),K2)
    }
    return result
  }

  def eval (env : Env[Value]) : Value = {
    var new_env = env
    for ((n,e) <- bindings) {
      val v = e.eval(env)
      new_env = new_env.push(n,v)
    }
    return ebody.eval(new_env)
  }

  override def insertBreakpoint (position: Int) : Int = {
    if (position > bindings.length) {
      return -1
    } else if (position == bindings.length) {
      for (x <- position until bindings.length) {
        bindings = bindings.patch(x, Seq((bindings(x)._1, bindings(x)._2.getExp())), 1)
      }
      if (!ebody.isBreakpoint()) {
        ebody = new EBreakpoint(ebody)
      }
      return position
    } else {
      for (x <- position until bindings.length) {
        if (bindings(x)._2.canPause()) {
          if (!bindings(x)._2.isBreakpoint()) {
            bindings = bindings.patch(x, Seq((bindings(x)._1, new EBreakpoint(bindings(x)._2))), 1)
          }
          return x
        } else if (x > 0) {
          bindings = bindings.patch(x - 1, Seq((bindings(x - 1)._1, bindings(x - 1)._2.getExp())), 1)
        }
      }
      if (!ebody.isBreakpoint()) {
        ebody = new EBreakpoint(ebody)
      }
      return bindings.length
    }
  }

  override def canPause () : Boolean = true

  override def getDebugReadable () : String =
    "(let (" + bindings.map { case (name, exp) => "(" + name + " " + exp.getDebugReadable() + ")" }.mkString(" ") + ") " + ebody.getDebugReadable() + ")"
}


case class EThrow (var e : Exp) extends Exp {

  override def toString () : String =
    "EThrow(" + e + ")"

  def cps (K1 : Exp, K2 : Exp) : Exp =
    e.cps(K2,K2)    // ! evaluate e, and invoke the exception continuation

  def eval (env : Env[Value]) : Value =
    error("should never have to evaluate a throw")

  override def insertBreakpoint (position: Int) : Int = {
    if (position > 0) {
      return -1
    }
    if (!e.isBreakpoint()) {
      e = new EBreakpoint(e)
    }
    return position
  }

  override def canPause () : Boolean = true

  override def getDebugReadable () : String =
    "(throw " + e.getDebugReadable() + ")"
}

case class ETry (var body : Exp, val param: String, var ctch : Exp) extends Exp {

  override def toString () : String =
    "ETry("+body+","+param+","+ctch+")"

  def cps (K1 : Exp, K2 : Exp) : Exp =
    body.cps(K1, new ERecFunction("", List(param), ctch.cps(K1,K2)))

  def eval (env:Env[Value]) : Value =
    error("should never have to evaluate a try")

  override def insertBreakpoint (position: Int) : Int = {
    position match {
      case 0 => {
        if (!body.isBreakpoint()) {
          body = new EBreakpoint(body)
        }
        return position
      }
      case 1 => {
        body = body.getExp()
        if (!ctch.isBreakpoint()) {
          ctch = new EBreakpoint(ctch)
        }
        return position
      }
      case _ => {
        return -1
      }
    }
  }

  override def canPause () : Boolean = true

  override def getDebugReadable () : String =
    "(try " + body.getDebugReadable() + " catch (" + param + ") " + ctch.getDebugReadable() + ")"
}


case class EBreakpoint (val e : Exp) extends Exp {

  var continuations: Option[List[Exp]] = None

  override def toString () : String =
    "EBreakpoint("+ e + ")"

  def cps (K1 : Exp, K2 : Exp) : Exp = {
    continuations = Some(List(K1, K2))
    return new EApply(K2, List(this))
  }

  def eval (env:Env[Value]) : Value = {
    return new VBreakpoint(env, continuations.get, e)
  }

  override def getExp () : Exp = e

  override def isBreakpoint () : Boolean = true

  override def getDebugReadable () : String = e.getDebugReadable()
}


//
// SURFACE SYNTAX (S-expressions)
//


import scala.util.parsing.combinator._


class SExpParser extends RegexParsers {

  // tokens

  def LP : Parser[Unit] = "(" ^^ { s => () }
  def RP : Parser[Unit] = ")" ^^ { s => () }
  def LB : Parser[Unit] = "[" ^^ { s => () }
  def RB : Parser[Unit] = "]" ^^ { s => () }
  def PLUS : Parser[Unit] = "+" ^^ { s => () }
  def TIMES : Parser[Unit] = "*" ^^ { s => () }
  def INT : Parser[Int] = """-?[0-9]+""".r ^^ { s => s.toInt }
  def IF : Parser[Unit] = "if" ^^ { s => () }
  def ID : Parser[String] = """[a-zA-Z_+*\-:.?=<>!|][a-zA-Z0-9_+\-*:.?=<>!|]*""".r ^^ { s => s }

  def FUN : Parser[Unit] = "fun" ^^ { s => () }
  def LET : Parser[Unit] = "let" ^^ { s => () }
  def BKPT : Parser[Unit] = "bkpt" ^^ { s => () }


  // grammar

  def atomic_int : Parser[Exp] = INT ^^ { i => new EInteger(i) }

  def atomic_id : Parser[Exp] =
    ID ^^ { s => new EId(s) }

  def atomic : Parser[Exp] =
    ( atomic_int | atomic_id ) ^^ { e => e}

  def expr_if : Parser[Exp] =
    LP ~ IF ~ expr ~ expr ~ expr ~ RP ^^
      { case _ ~ _ ~ e1 ~ e2 ~ e3 ~ _ => new EIf(e1,e2,e3) }

  def binding : Parser[(String,Exp)] =
    LP ~ ID ~ expr ~ RP ^^ { case _ ~ n ~ e ~ _ => (n,e) }

  def expr_let : Parser[Exp] =
    LP ~ LET ~ LP ~ rep(binding) ~ RP ~ expr ~ RP ^^
         { case _ ~ _ ~ _ ~ bindings ~ _ ~ e2 ~ _ => new ELet(bindings,e2) }

  def expr_vec : Parser[Exp] =
    LB ~ rep(expr) ~ RB ^^ { case _ ~ es ~ _ => new EVector(es) }

  def expr_fun : Parser[Exp] =
    LP ~ FUN ~ LP ~ rep(ID) ~ RP ~ expr ~ RP ^^
      { case _ ~ _ ~ _ ~ params ~ _ ~ e ~ _ => new ERecFunction("",params,e) }

  def expr_funr : Parser[Exp] =
    LP ~ FUN ~ ID ~ LP ~ rep(ID) ~ RP ~ expr ~ RP ^^
      { case _ ~ _ ~ self ~ _ ~ params ~ _ ~ e ~ _ => new ERecFunction(self,params,e) }

  def expr_bkpt : Parser[Exp] =
    LP ~ BKPT ~ expr ~ RP ^^ { case _ ~ _ ~ e ~ _ => new EBreakpoint(e) }

  def expr_app : Parser[Exp] =
    LP ~ expr ~ rep(expr) ~ RP ^^ { case _ ~ ef ~ eargs ~ _ => new EApply(ef,eargs) }


  def expr : Parser[Exp] =
    ( atomic | expr_if | expr_vec | expr_fun | expr_funr | expr_let | expr_throw | expr_try | expr_bkpt | expr_app) ^^
         { e => e }

  def shell_entry : Parser[ShellEntry] =
    (LP ~ "define" ~ ID ~ expr ~ RP  ^^ { case _ ~ _ ~ n ~ e ~ _  => new SEdefine(n,e) }) |
    (expr ^^ { e => new SEexpr(e) }) |
    ("#show" ~ expr ^^ { case _ ~ e => new SEshow(e) }) |
    ("#continue" ^^ { s => new SEcontinue() }) |
    ("#stepover" ^^ { s => new SEstepOver }) |
    ("#step" ^^ { s => new SEstep() }) |
    ("#quit" ^^ { s => new SEquit() })


  // exceptions
  def THROW : Parser[Unit] = "throw" ^^ { s => () }
  def TRY : Parser[Unit] = "try" ^^ {s => () }
  def CATCH : Parser[Unit] = "catch" ^^ { s => () }
  def expr_throw : Parser[Exp] =
    LP ~ THROW ~ expr ~ RP ^^ { case _ ~ _ ~ e ~ _ => new EThrow(e) }
  def expr_try : Parser[Exp] =
    LP ~ TRY ~ expr ~ CATCH ~ LP ~ ID ~ RP ~ expr ~ RP ^^
      { case _ ~ _ ~ e1 ~ _ ~ _ ~ param ~ _ ~ e2 ~ _ => ETry(e1,param,e2) }

}



//
//  Shell
//

abstract class ShellEntry {

  // abstract class for shell entries
  // (representing the various entries you
  //  can type at the shell)

  def processEntry (env:Env[Value]) : Env[Value]
  def passDebugContext (debugContext: DebugContext) : Unit
}


class SEexpr (e:Exp) extends ShellEntry {

  var debug: DebugContext = new DebugContext()

  val keywords = List("true","false","not","+","*","=","<","empty?",
    "first","rest","empty","cons","print!")

  def passDebugContext (debugContext: DebugContext) : Unit = {
    debug = debugContext
  }

  def ok (v : Value) : Value = {
    println(v)
    return VNone
  }

  def fail (v : Value) : Value = {
    if (v.isBreakpoint()) {
      println("Original expression:")
      println(debug.getInput())
      debug.break(v.asInstanceOf[VBreakpoint])
      println("\nNext to execute:")
      println(v.getReturnExp().getDebugReadable() + "\n")
      println("Abstract representation:")
      println(v.getReturnExp() + "\n")
      println("Environment:")
      val nonStandard = v.getEnv().getContent().filterNot(x => keywords.contains(x._1) | x._1 == "")
      println(new Env(nonStandard.filterNot(_._1.startsWith("  "))))
    } else {
      println("EXCEPTION("+v+")")
    }
    return VNone
  }

  def processEntry (env:Env[Value]) : Env[Value] = {
    val new_env = env.push("  Kok",new VPrimK(ok)).push("  Kfail",new VPrimK(fail))
    val ee = e.cps(new EId("  Kok"), new EId("  Kfail"))
    ee.eval(new_env)
    return env
  }
}

class SEcontinue extends ShellEntry {
  var debug: DebugContext = new DebugContext()

  def passDebugContext (debugContext: DebugContext) : Unit = {
    debug = debugContext
  }

  def processEntry (env: Env[Value]): Env[Value] = {
    debug.continue()
    return env
  }
}

class SEstepOver extends ShellEntry {
  var debug: DebugContext = new DebugContext()

  def passDebugContext (debugContext: DebugContext) : Unit = {
    debug = debugContext
  }

  def processEntry (env: Env[Value]): Env[Value] = {
    debug.stepOver()
    return env
  }
}

class SEstep extends ShellEntry {
  var debug: DebugContext = new DebugContext()

  def passDebugContext (debugContext: DebugContext) : Unit = {
    debug = debugContext
  }

  def processEntry (env: Env[Value]): Env[Value] = {
    debug.stepInto()
    return env
  }
}

class SEshow (e:Exp) extends ShellEntry {

  def passDebugContext (debugContext: DebugContext) : Unit = {}

  def ok (vs : List[Value]) : Value = {
    println(vs.head)
    return VNone
  }

  def fail (vs : List[Value]) : Value = {
    println("EXCEPTION("+vs.head+")")
    return VNone
  }

  def processEntry (env:Env[Value]) : Env[Value] = {
    val new_env = env.push("  Kok",new VPrimOp(ok)).push("  Kfail",new VPrimOp(fail))
    val ee = e.cps(new EId("  Kok"), new EId("  Kfail"))
    println("CPS: "+ee)
    ee.eval(new_env)
    return env
  }
}

class SEdefine (n:String, e:Exp) extends ShellEntry {

  def passDebugContext (debugContext: DebugContext) : Unit = {}

  def processEntry (env:Env[Value]) : Env[Value] = {
    val v = e.eval(env)
    println(n + " defined")
    return env.push(n,v)
  }
}

class SEquit extends ShellEntry {

  def passDebugContext (debugContext: DebugContext) : Unit = {}

  def processEntry (env:Env[Value]) : Env[Value] = {
    System.exit(0)
    return env
  }
}


class DebugContext {
  var paused = false
  var input = ""

  var env: Env[Value] = new Env[Value](List())
  var prevEnv: Env[Value] = new Env[Value](List())
  var continuations: Option[List[Exp]] = None
  var prevContinuations: Option[List[Exp]] = None
  var returnExp: Option[Exp] = None
  var parentExp: Option[Exp] = None
  var stepPosition: Int = 0

  def setInput (inputExp: String) : Unit = {
    input = inputExp
  }

  def break (bkpt: VBreakpoint) : Unit = {
    paused = true
    env = bkpt.getEnv()
    continuations = Some(bkpt.getContinuations())
    returnExp = Some(bkpt.getReturnExp())
  }

  def continue () : Unit = {
    paused = false
    returnExp.get.cps(continuations.get.head, continuations.get.last).eval(env)
  }
  
  def stepOutAndContinue () : Unit = {
    paused = false
    parentExp.get.cps(prevContinuations.get.head, prevContinuations.get.last).eval(prevEnv)
  }

  def stepOver () : Unit = {
    if (parentExp.isEmpty) {
      continue()
      return
    }
    var position = parentExp.get.insertBreakpoint(stepPosition + 1)
    if (position < 0) {
      stepPosition = 0
    } else {
      stepPosition = position
    }
    stepOutAndContinue()
  }

  def stepInto () : Unit = {
    stepPosition = returnExp.get.insertBreakpoint(0)
    if (stepPosition < 0) {
      stepPosition = 0
      stepOver()
      return
    } else {
      prevEnv = env
      prevContinuations = continuations
      parentExp = returnExp
      continue()
    }
  }

  def isPaused () : Boolean = paused
  def getInput() : String = input

  def getEnv () : Env[Value] = env
  def getContinuations () : List[Exp] = continuations.get
  def getReturnExp () : Exp = returnExp.get
}


object Shell {

  val parser = new SExpParser

  def parse (input: String) : ShellEntry = {
    parser.parseAll(parser.shell_entry, input) match {
      case parser.Success(result,_) => result
      case failure : parser.NoSuccess => throw new Exception("Cannot parse "+input+": "+failure.msg)
    }
  }


  def time[R](block: => R):R = {
    val t0 = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1-t0) + "ms")
    result
  }

  val nullEnv = new Env[Value](List())

  //
  // Standard environment
  //

  val stdEnv = new Env[Value](List(
    ("true",new VBoolean(true)),
    ("false",new VBoolean(false)),
    ("not", new VRecClosure("",List("a"), new EIf(new EId("a"), new EBoolean(false), new EBoolean(true)),nullEnv)),
    ("+", new VPrimOp(Ops.operPlus)),
    ("*", new VPrimOp(Ops.operTimes)),
    ("=", new VPrimOp(Ops.operEqual)),
    ("<", new VPrimOp(Ops.operLess)),
    ("empty?",new VPrimOp(Ops.operEmpty)),
    ("first",new VPrimOp(Ops.operFirst)),
    ("rest",new VPrimOp(Ops.operRest)),
    ("empty",new VVector(List())),
    ("cons",new VPrimOp(Ops.operCons)),
    ("print!",new VPrimOp(Ops.operPrint))
  ))


  def shell () : Unit = {

    var env = stdEnv
    var debug = new DebugContext()

    println("With CPS")
    while (true) {
      if (debug.isPaused()) {
        print("BKPT> ")
      } else {
        print("TFUNC> ")
      }
      try {
        val input = scala.io.StdIn.readLine()
        if (!debug.isPaused()) {
          debug.setInput(input)
        }
        val se = parse(input)
        if (se.isInstanceOf[SEexpr] || se.isInstanceOf[SEcontinue] || se.isInstanceOf[SEstep] || se.isInstanceOf[SEstepOver]) {
          se.passDebugContext(debug)
        }
        if (debug.isPaused()) {
          env = time { se.processEntry(debug.getEnv()) }
        } else {
          env = time { se.processEntry(env) }
        }
      } catch {
        case e : Exception => println(e.getMessage)
      }
    }
  }

  def main (argv:Array[String]) : Unit = {
    shell()
  }
}
