package amyc
package langserver

import java.util
import java.util.Optional
import java.util.concurrent.CompletableFuture
import java.nio.file.Path
import java.io.File
import java.util.Collections


//import org.eclipse.lsp4j._
import org.eclipse.lsp4j.{Position => LspPosition, _}
import org.eclipse.lsp4j.{Range => LspRange, _}
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.Location

import utils.{Position => AmyPosition, _}
import utils._
import ast._
import parsing._
import analyzer._
import codegen._
import ast.SymbolicTreeModule._

class AmyTextDocumentService extends TextDocumentService {

    override def completion(completionParams: CompletionParams): CompletableFuture[Either[util.List[CompletionItem], CompletionList]] = {
        // Provide completion item.
        
        CompletableFuture.supplyAsync(() => {
            var completionItems: util.List[CompletionItem] = new util.ArrayList[CompletionItem]()
            try {
                var completionItem: CompletionItem = new CompletionItem
                completionItem.setInsertText("sayHello() {\n    print(\"hello\")\n}")
                completionItem.setLabel("sayHello()")
                completionItem.setKind(CompletionItemKind.Snippet)
                completionItem.setDetail("sayHello()\n this will say hello to the people")
                completionItems.add(completionItem)

                var completionItem2: CompletionItem = new CompletionItem
                completionItem2.setInsertText("match {\n    start your pattern\n}")
                completionItem2.setLabel("match")
                completionItem2.setKind(CompletionItemKind.Snippet)
                completionItem2.setDetail("match\n start your pattern matching")
                completionItems.add(completionItem2)
            } catch {
                case e: Exception => e.printStackTrace()
            }
            
            Either.forLeft(completionItems)
        })

    }
    
    override def resolveCompletionItem(completionItem: CompletionItem): CompletableFuture[CompletionItem] = {
        return null
    }

    override def hover(hoverParams: HoverParams): CompletableFuture[Hover] = {
        return null
    }


    def positionsAreEqual(sourcePosition: AmyPosition, exprPosition: AmyPosition, endExprPosition: AmyPosition): Boolean = {
        (exprPosition.file == sourcePosition.file &&
        exprPosition.line <= sourcePosition.line &&
        exprPosition.col <= sourcePosition.col &&
        endExprPosition.file == sourcePosition.file &&
        endExprPosition.line >= sourcePosition.line &&
        endExprPosition.col >= sourcePosition.col)
    }

    def binaryOpFindIdent(lhs: Expr, rhs: Expr, sourcePosition: AmyPosition): Option[Identifier] = {
        findIdentifier(lhs, sourcePosition) match {
            case Some(name) => Some(name)
            case None => findIdentifier(rhs, sourcePosition)
        }
    }

    def searchingInPattern(sourcePosition: AmyPosition, pat: Pattern): Option[Identifier] = {
        pat match {
            case WildcardPattern() => None
            case IdPattern(name: Name) => {
                if(positionsAreEqual(sourcePosition, pat.position, pat.endPosition))
                    Some(name)
                else None
            }
            case LiteralPattern(_) => None
            case CaseClassPattern(constr: QualifiedName, args: List[Pattern]) => {
                if(positionsAreEqual(sourcePosition, pat.position, pat.endPosition))
                    Some(constr)
                else{
                    if (args.nonEmpty) {
                        args.map(x => {
                            searchingInPattern(sourcePosition, x)
                        }).reduceLeft((x,y) => {
                            x match {
                                case None => y
                                case _ => x
                            }
                        })
                    } else {
                        None
                    }
                }
            }
        }
    }


    def findIdentifier(expr: Expr, sourcePosition: AmyPosition): Option[Identifier] = {  
        expr match {
            // Variables
            case Variable(name) => {
                if(positionsAreEqual(sourcePosition, expr.position, expr.endPosition)) {
                    Some(name) 
                } else {
                    None
                }
            }

            // Literals
            case IntLiteral(_) => None
            case BooleanLiteral(_) => None
            case StringLiteral(_) => None
            case UnitLiteral() => None

            
            // Binary operators
            case Plus(lhs, rhs) => binaryOpFindIdent(lhs, rhs, sourcePosition)
            case Minus(lhs, rhs) => binaryOpFindIdent(lhs, rhs, sourcePosition)
            case Times(lhs, rhs) => binaryOpFindIdent(lhs, rhs, sourcePosition)
            case Div(lhs, rhs) => binaryOpFindIdent(lhs, rhs, sourcePosition)
            case Mod(lhs, rhs) => binaryOpFindIdent(lhs, rhs, sourcePosition)
            case LessThan(lhs, rhs) => binaryOpFindIdent(lhs, rhs, sourcePosition)
            case LessEquals(lhs, rhs) => binaryOpFindIdent(lhs, rhs, sourcePosition)
            case And(lhs, rhs) => binaryOpFindIdent(lhs, rhs, sourcePosition)
            case Or(lhs, rhs) => binaryOpFindIdent(lhs, rhs, sourcePosition)
            case Equals(lhs, rhs) => binaryOpFindIdent(lhs, rhs, sourcePosition)
            case Concat(lhs, rhs) => binaryOpFindIdent(lhs, rhs, sourcePosition)

            // Unary operators
            case Not(e) => findIdentifier(e, sourcePosition)
            case Neg(e) => findIdentifier(e, sourcePosition)

            // Function/constructor call
            case Call(qname, args) => {
                if(positionsAreEqual(sourcePosition, expr.position, expr.endPosition)) {
                    Some(qname)
                } else {
                    if (args.nonEmpty) {
                        args.map(x => findIdentifier(x, sourcePosition)).reduceLeft((x,y) => {
                            x match {
                                case None => y
                                case _ => x
                            }
                        })
                    } else {
                        None
                    } 
                }
            }
            
            // The ; operator
            case Sequence(e1, e2) => binaryOpFindIdent(e1, e2, sourcePosition)

            // Local variable definition
            case Let(df, value, body) => {
                if(positionsAreEqual(sourcePosition, df.position, df.endPosition)) {
                    Some(df.name)
                } else {
                    binaryOpFindIdent(value, body, sourcePosition)
                }
            }
            
            // If-then-else
            case Ite(cond, thenn, elze) => {
                findIdentifier(cond, sourcePosition) match {
                    case Some(name) => Some(name)
                    case None => binaryOpFindIdent(thenn, elze, sourcePosition)
                }
            }

            // Pattern matching
            case Match(scrut, cases) => {
                val identScrut = findIdentifier(scrut, sourcePosition)
                identScrut match {
                    case Some(_) => identScrut
                    case None => {
                        //if not found in scrut, check inside pattern
                        if (cases.nonEmpty) {
                            //first check in the patterns

                            val foundInPattern = cases.map(x => {
                                searchingInPattern(sourcePosition, x.pat)
                            }).reduceLeft((x,y) => {
                                x match {
                                    case None => y
                                    case _ => x
                                }
                            })

                            foundInPattern match {
                                case Some(_) => foundInPattern
                                case None => cases.map(x => findIdentifier(x.expr, sourcePosition)).reduceLeft((x,y) => {
                                    x match {
                                        case None => y
                                        case _ => x
                                    }
                                })
                            }
                        } else {
                            None
                        }
                    }
                
                }
            }
            // Represents a computational error; prints its message, then exits
            case Error(msg) => None
        }
        
    }

    val nilPosition = new SourcePosition(new File(""), 0, 0)

    def binaryOpPos(lhs: Expr, rhs: Expr, ident: Identifier): AmyPosition = {
        val tp = findTargetPosition(lhs, ident) 
        tp match {
            case nilPosition => findTargetPosition(rhs, ident)
            case _ => tp
        }
    }

    def findTargetPosition(expr: Expr, ident: Identifier): AmyPosition = {   

            expr match {

                // Variables
                case Variable(name) => nilPosition
    
                // Literals
                case IntLiteral(_) => nilPosition
                case BooleanLiteral(_) => nilPosition
                case StringLiteral(_) => nilPosition
                case UnitLiteral() => nilPosition

                // Binary operators
                case Plus(lhs, rhs) => binaryOpPos(lhs, rhs, ident)
                case Minus(lhs, rhs) => binaryOpPos(lhs, rhs, ident)
                case Times(lhs, rhs) => binaryOpPos(lhs, rhs, ident)
                case Div(lhs, rhs) => binaryOpPos(lhs, rhs, ident)
                case Mod(lhs, rhs) => binaryOpPos(lhs, rhs, ident)
                case LessThan(lhs, rhs) => binaryOpPos(lhs, rhs, ident)
                case LessEquals(lhs, rhs) => binaryOpPos(lhs, rhs, ident)
                case And(lhs, rhs) => binaryOpPos(lhs, rhs, ident)
                case Or(lhs, rhs) => binaryOpPos(lhs, rhs, ident)
                case Equals(lhs, rhs) => binaryOpPos(lhs, rhs, ident)
                case Concat(lhs, rhs) => binaryOpPos(lhs, rhs, ident)
    
                // Unary operators
                case Not(e) => findTargetPosition(e, ident)
                case Neg(e) => findTargetPosition(e, ident)
    
                // Function/constructor call
                case Call(qname, args) => {
                    if (args.nonEmpty) {
                        args.map(x => findTargetPosition(x, ident)).reduceLeft((x,y) => {
                            if (x != nilPosition) x
                            else y
                        })
                    } else {
                        nilPosition
                    }
                }

                // The ; operator
                case Sequence(e1, e2) => binaryOpPos(e1, e2, ident)

                // Local variable definition
                case Let(df, value, body) => {
                    if(ident == df.name) {
                        expr.position
                    } else {
                        nilPosition
                    }
                }

                // If-then-else
                case Ite(cond, thenn, elze) => {
                    val tp = findTargetPosition(cond, ident) 
                    tp match {
                        case nilPosition => {
                            binaryOpPos(thenn, elze, ident)
                        }
                        case _ => tp
                    }
                }

                // Pattern matching
                case Match(scrut, cases) => {
                    val scrutPos = findTargetPosition(scrut, ident)
                    if(scrutPos != nilPosition) {
                        scrutPos
                    } else {
                        if (cases.nonEmpty) {
                            cases.map(x => findTargetPosition(x.expr, ident)).reduceLeft((x,y) => {
                                if (x != nilPosition) x
                                else y
                            })
                        } else {
                            nilPosition
                        }
                    }
                }

                // Represents a computational error; prints its message, then exits
                case Error(msg) => nilPosition
            }
        }

    override def definition(params: DefinitionParams): CompletableFuture[Either[util.List[_ <: Location], util.List[_ <: LocationLink]]] = {
        CompletableFuture.supplyAsync(() => {
            
            try {
                // Get URI, line and col info for position from argument
                val uri0: String = params.getTextDocument().getUri()
                // Hotfix which works with OS whose path has Users at a high level (Mac, Windows)
                val uri: String = uri0.substring(uri0.indexOf("Users") - 1)
                val position: LspPosition = params.getPosition()
                val line: Int = position.getLine()
                val offset: Int = position.getCharacter()
                
                // Convert LSP position to Amy position
                val sourcePosition: AmyPosition = new SourcePosition(new File(uri), line + 1, offset + 1)
                var targetPosition: AmyPosition = new SourcePosition(new File(uri), 0, 0)
                var found: Boolean = false
                
                // set up library
                val args: List[String] = Array(uri).toList
                val libPath: String = uri.reverse.substring(uri.reverse.indexOf("/")).reverse
                // create the context with all "dependency" -- overfitting
                val fileNames: List[String] = (libPath + "/library/List.amy") :: (libPath + "/library/Std.amy") :: (libPath + "/library/Option.amy") :: args
                val ctx = Context(new Reporter, fileNames)

                val files = ctx.files.map(new File(_))
                val pipeline = Lexer andThen Parser andThen NameAnalyzer

                if (files.isEmpty) {
                    Either.forLeft(Collections.emptyList())
                } else {
                    val (program, table): (Program, SymbolTable) = pipeline.run(ctx)(files)

                    /* 
                        Explore the AST
                        1) retrieve identifier from source pos
                        in  m: ModuleDef <- program.modules
                        e: Expr <- m.optExpr
                            First look into the expressions (the "body")
                        
                    */

                    // HERE SEARCH ID IN TREE (EXPR)
                    var id: Option[Identifier] = None
                   for {
                        m: ModuleDef <- program.modules
                        e: Expr <- m.optExpr
                    } {
                        id match {
                            case None => id = findIdentifier(e, sourcePosition)
                            case _ =>
                        }
                    }

                    id match {
                            // HERE SEARCH ID IN FUNDEFS
                        case None => {
                            // but if it wasn't found in optExpr, look into the def itself)
                            for {
                                m: ModuleDef <- program.modules
                                d: ClassOrFunDef <- m.defs
                            } {
                                d match {
                                    case FunDef(_,_,_,body) => id = findIdentifier(body, sourcePosition)
                                    case _ => None
                                }
 
                            }
                            id match {
                                case None => {}
                                    // HERE SEARCH TARGET POS IN DEFS IF ID FOUND IN FUNDEFS 
                                case Some(ident) => {
                                    for {
                                        m: ModuleDef <- program.modules
                                        d: ClassOrFunDef <- m.defs
                                    } {
                                        if(d.name == ident) {
                                            /* 
                                                3) retrieve pos from defs pos
                                            */
                                            targetPosition = d.position
                                            found = true
                                        } else {
                                            d match {
                                                case FunDef(_,_,_,body) => {
                                                    targetPosition = findTargetPosition(body, ident)
                                                    found = true
                                                }
                                                case _ => targetPosition
                                            }
                                        }
                                    }
                                    // HERE SEARCH TARGET POS IN LOCAL VARIABLE IN TREE IF ID FOUND IN FUNDEFS 
                                    if (found == false) {
                                        for {
                                            m: ModuleDef <- program.modules
                                            e: Expr <- m.optExpr
                                        } {
                                            targetPosition = findTargetPosition(e, ident)
                                            found == true
                                        }
                                    }

                                }
                            }
                        }
                        /* 2) compare identifier to defs identifiers (in for
                            m: ModuleDef <- program.modules
                            d: ClassOrFunDef <- m.defs
                        */
                        // HERE SEARCH TARGET POS IN DEFS IF ID FOUND IN EXPR (TREE) 
                        case Some(ident) => {
                            for {
                                m: ModuleDef <- program.modules
                                d: ClassOrFunDef <- m.defs
                            } {
                                if(d.name == ident) {
                                    /* 3) retrieve pos from defs pos*/
                                    targetPosition = d.position
                                    found = true
                                }
                            }
                            // HERE SEARCH TARGET POS IN LOCAL VARIABLE IN TREE IF ID FOUND IN EXPR (TREE) 
                            if (found == false) {
                                for {
                                    m: ModuleDef <- program.modules
                                    e: Expr <- m.optExpr
                                } {
                                    targetPosition = findTargetPosition(e, ident)
                                }
                            }
                        }
                    }
                    

                }

                // Convert Amy position to LSP position and send it back to VSCode
                var start = new LspPosition(targetPosition.line-1, targetPosition.col-1)
                var end = new LspPosition(targetPosition.line-1, targetPosition.col-1)
                val range: LspRange = new LspRange(start, end)
                // 2nd part of the Hotfix for Mac and Windows directory structure
                val inituri: String = uri0.slice(0, uri0.indexOf("Users") - 1)
                val location: Optional[Location] = Optional.of(new Location(inituri + targetPosition.file.toString(), range))
                
                Either.forLeft(Collections.singletonList(location.get()))
            } catch {
                case AmycFatalError(_) => {
                    Either.forLeft(Collections.emptyList())
                }
                case e: Exception => {
                    Console.err.println(s"Exception: ${e} ${e.printStackTrace()}")
                    val uri: String = params.getTextDocument().getUri()
                    var start: LspPosition = new LspPosition(0, 4)
                    var end: LspPosition = new LspPosition(0, 6)
                    val range: LspRange = new LspRange(start, end)
                    val location: Optional[Location] = Optional.of(new Location(uri, range))
                    Either.forLeft(Collections.singletonList(location.get()))
                    
                }
            }
        })

    }

    override def references(referenceParams: ReferenceParams): CompletableFuture[util.List[_ <: Location]] = {
        return null
    }

    override def documentHighlight(highlightParams: DocumentHighlightParams): CompletableFuture[util.List[_ <: DocumentHighlight]] = {
        return null
    }

    override def codeLens(codeLensParams: CodeLensParams): CompletableFuture[util.List[_ <: CodeLens]] = {
        return null
    }

    override def resolveCodeLens(codeLens: CodeLens): CompletableFuture[CodeLens] = {
        return null
    }

    override def formatting(documentFormattingParams: DocumentFormattingParams): CompletableFuture[util.List[_ <: TextEdit]] = {
        return null
    }

    override def rangeFormatting(documentRangeFormattingParams: DocumentRangeFormattingParams): CompletableFuture[util.List[_ <: TextEdit]] = {
        return null
    }

    override def onTypeFormatting(documentOnTypeFormattingParams: DocumentOnTypeFormattingParams): CompletableFuture[util.List[_ <: TextEdit]] = {
        return null
    }

    override def rename(renameParams: RenameParams): CompletableFuture[WorkspaceEdit] = {
        return null
    }

    override def didOpen(didOpenTextDocumentParams: DidOpenTextDocumentParams): Unit = {
    }

    override def didChange(didChangeTextDocumentParams: DidChangeTextDocumentParams): Unit = {
    }

    override def didClose(didCloseTextDocumentParams: DidCloseTextDocumentParams): Unit = {
    }

    override def didSave(didSaveTextDocumentParams: DidSaveTextDocumentParams): Unit = {
    }
}