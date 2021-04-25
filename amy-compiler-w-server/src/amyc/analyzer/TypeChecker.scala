package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))
      
      e match {
        // Variables
        case Variable(name) =>
          // QST: can we assume that variable is in Map? Or None case handling is required.
          topLevelConstraint(env.get(name).get)

        // Literals
        case IntLiteral(_) =>
          topLevelConstraint(IntType)
        case BooleanLiteral(_) =>
          topLevelConstraint(BooleanType)
        case StringLiteral(_) =>
          topLevelConstraint(StringType)
        case UnitLiteral() =>
          topLevelConstraint(UnitType)
        
        // Binary Operators
        // QST: can we use direct type checking?
        case Plus(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)
        case Minus(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)
        case Times(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)
        case Div(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)
        case Mod(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)
        case LessThan(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(BooleanType)
        case LessEquals(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(BooleanType)
        case And(lhs, rhs) =>
          genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType) ++ topLevelConstraint(BooleanType)
        case Or(lhs, rhs) =>
          genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType) ++ topLevelConstraint(BooleanType)
        case Equals(lhs, rhs) =>
          val tv = TypeVariable.fresh()
          genConstraints(lhs, tv) ++ genConstraints(rhs, tv) ++ topLevelConstraint(BooleanType)
        case Concat(lhs, rhs) =>
          genConstraints(lhs, StringType) ++ genConstraints(rhs, StringType) ++ topLevelConstraint(StringType)

        // Unary Operators
        case Not(exp) => genConstraints(exp, BooleanType) ++ topLevelConstraint(BooleanType)
        case Neg(exp) => genConstraints(exp, IntType) ++ topLevelConstraint(IntType)

        // Function/constructor call
        case Call(qname, args) => {
          val argConstraint = table.getConstructor(qname) match {
            case Some(constrsig) => {
              val req = args.zip(constrsig.argTypes)
              req.flatMap(arg => genConstraints(arg._1, arg._2))
            }
            case None => {
              val funsig: FunSig = table.getFunction(qname).get
              val req = args.zip(funsig.argTypes)
              req.flatMap(arg => genConstraints(arg._1, arg._2))
            }
          }
          val fun = table.getConstructor(qname).getOrElse(table.getFunction(qname).get)
          val retConstraints = topLevelConstraint(fun.retType)
          argConstraint ++ retConstraints
        }
        
        // The ; operator
        case Sequence(e1, e2) =>
          val tv = TypeVariable.fresh()
          genConstraints(e1, tv) ++ genConstraints(e2, expected)

        // Local variable definition
        case Let(df, value, body) =>
          genConstraints(value, df.tt.tpe) ++ genConstraints(body, expected)(env + (df.name -> df.tt.tpe))
        
        // If-then-else
        case Ite(cond, thenn, elze) =>
          genConstraints(cond, BooleanType) ++ genConstraints(thenn, expected) ++ genConstraints(elze, expected)
        
        // Pattern matching
        case Match(scrut, cases) => {
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type):
            (List[Constraint], Map[Identifier, Type]) =
          {
            // TODO
            pat match {
              case WildcardPattern() => (Nil, Map.empty)
              
              case IdPattern(name) => (Nil, Map(name -> scrutExpected))

              case LiteralPattern(lit) => (genConstraints(lit, scrutExpected), Map.empty)
              
              case CaseClassPattern(constr, args) => {
                // retType returns List or Option from the constr
                val retType = table.getConstructor(constr).get.retType

                // get the constructor parameter types from the symboltable
                val argTypes = table.getConstructor(constr).get.argTypes
                
                // for each parameter add its type constraint as well as the map to env
                val constMap = args.zip(argTypes).map(arg => handlePattern(arg._1, arg._2)).unzip
                
                // QST: how to add constraint that the CaseClass is the same type as scrut?
                // verify that scrutExpected == retType
                // flatten env
                (List(Constraint(retType, scrutExpected, e.position)) ++ constMap._1.flatten, constMap._2.flatten.toMap)
              } 
            }
          }

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            val expConstraints = genConstraints(cse.expr, expected)(env ++ moreEnv)
            patConstraints ++ expConstraints
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))

        }

        case Error(msg) => genConstraints(msg, StringType)
          
        case _ => ctx.reporter.fatal("Receveid Unrecognized token", e.position)
      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: Type, from: Int, to: Type): Type = {
        tpe match {
          case TypeVariable(`from`) => to
          case other => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Solve the given set of typing constraints and report errors
    //  using `ctx.reporter.error` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          if(found == expected) {
            solveConstraints(more)
          } else {
            found match { 
              case TypeVariable(id) => solveConstraints(subst_*(more, id, expected))
              case _ => {
                expected match { 
                  case TypeVariable(id) => solveConstraints(subst_*(more, id, found))
                  case _ => ctx.reporter.error(s"Err:expected: $expected found: $found", pos)
                }
              }
            }
            
          }
          //solveConstraints(more)  // TODO/QST should we make one subcase per constraint mapping N.x to S.x ??
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }
}
