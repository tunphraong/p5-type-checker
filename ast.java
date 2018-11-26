import java.io.*;
import java.util.*;


// **********************************************************************
// The ASTnode class defines the nodes of the abstract-syntax tree that
// represents a Cdull program.
//
// Internal nodes of the tree contain pointers to children, organized
// either in a list (for nodes that may have a variable number of 
// children) or as a fixed set of fields.
//
// The nodes for literals and ids contain line and character number
// information; for string literals and identifiers, they also contain a
// string; for integer literals, they also contain an integer value.
//
// Here are all the different kinds of AST nodes and what kinds of children
// they have.  All of these kinds of AST nodes are subclasses of "ASTnode".
// Indentation indicates further subclassing:
//
//     Subclass            Kids
//     --------            ----
//     ProgramNode         DeclListNode
//     DeclListNode        linked list of DeclNode
//     DeclNode:
//       VarDeclNode       TypeNode, IdNode, int
//       FnDeclNode        TypeNode, IdNode, FormalsListNode, FnBodyNode
//       FormalDeclNode    TypeNode, IdNode
//       StructDeclNode    IdNode, DeclListNode
//
//     FormalsListNode     linked list of FormalDeclNode
//     FnBodyNode          DeclListNode, StmtListNode
//     StmtListNode        linked list of StmtNode
//     ExpListNode         linked list of ExpNode
//
//     TypeNode:
//       IntNode           -- none --
//       BoolNode          -- none --
//       VoidNode          -- none --
//       StructNode        IdNode
//
//     StmtNode:
//       AssignStmtNode      AssignNode
//       PostIncStmtNode     ExpNode
//       PostDecStmtNode     ExpNode
//       ReadStmtNode        ExpNode
//       WriteStmtNode       ExpNode
//       IfStmtNode          ExpNode, DeclListNode, StmtListNode
//       IfElseStmtNode      ExpNode, DeclListNode, StmtListNode,
//                                    DeclListNode, StmtListNode
//       WhileStmtNode       ExpNode, DeclListNode, StmtListNode
//       RepeatStmtNode      ExpNode, DeclListNode, StmtListNode
//       CallStmtNode        CallExpNode
//       ReturnStmtNode      ExpNode
//
//     ExpNode:
//       IntLitNode          -- none --
//       StrLitNode          -- none --
//       TrueNode            -- none --
//       FalseNode           -- none --
//       IdNode              -- none --
//       DotAccessNode       ExpNode, IdNode
//       AssignNode          ExpNode, ExpNode
//       CallExpNode         IdNode, ExpListNode
//       UnaryExpNode        ExpNode
//         UnaryMinusNode
//         NotNode
//       BinaryExpNode       ExpNode ExpNode
//         PlusNode     
//         MinusNode
//         TimesNode
//         DivideNode
//         AndNode
//         OrNode
//         EqualsNode
//         NotEqualsNode
//         LessNode
//         GreaterNode
//         LessEqNode
//         GreaterEqNode
//
// Here are the different kinds of AST nodes again, organized according to
// whether they are leaves, internal nodes with linked lists of kids, or
// internal nodes with a fixed number of kids:
//
// (1) Leaf nodes:
//        IntNode,   BoolNode,  VoidNode,  IntLitNode,  StrLitNode,
//        TrueNode,  FalseNode, IdNode
//
// (2) Internal nodes with (possibly empty) linked lists of children:
//        DeclListNode, FormalsListNode, StmtListNode, ExpListNode
//
// (3) Internal nodes with fixed numbers of kids:
//        ProgramNode,     VarDeclNode,     FnDeclNode,     FormalDeclNode,
//        StructDeclNode,  FnBodyNode,      StructNode,     AssignStmtNode,
//        PostIncStmtNode, PostDecStmtNode, ReadStmtNode,   WriteStmtNode   
//        IfStmtNode,      IfElseStmtNode,  WhileStmtNode,  CallStmtNode
//        ReturnStmtNode,  DotAccessNode,   AssignExpNode,  CallExpNode,
//        UnaryExpNode,    BinaryExpNode,   UnaryMinusNode, NotNode,
//        PlusNode,        MinusNode,       TimesNode,      DivideNode,
//        AndNode,         OrNode,          EqualsNode,     NotEqualsNode,
//        LessNode,        GreaterNode,     LessEqNode,     GreaterEqNode
//
// **********************************************************************

// **********************************************************************
// ASTnode class (base class for all other kinds of nodes)
// **********************************************************************

abstract class ASTnode { 
    // every subclass must provide an unparse operation
    abstract public void unparse(PrintWriter p, int indent);

    // this method can be used by the unparse methods to do indenting
    protected void printSpace(PrintWriter p, int indent) {
        for (int k=0; k<indent; k++) p.print(" ");
    }
}

// **********************************************************************
// ProgramNode,  DeclListNode, FormalsListNode, FnBodyNode,
// StmtListNode, ExpListNode
// **********************************************************************

class ProgramNode extends ASTnode {
    public ProgramNode(DeclListNode L) {
        declList = L;
    }

    /**
     * nameAnalysis
     * Creates an empty symbol table for the outermost scope, then processes
     * all of the globals, struct defintions, and functions in the program.
     */
    public void nameAnalysis() {
        SymTable symTab = new SymTable();
        declList.nameAnalysis(symTab);
    }

    /**
     * typeCheck
     */
    public void typeCheck() {
    // TODO: Implement a type checking method for this node and its children.
        declList.typeCheck();
    }
    
    public void unparse(PrintWriter p, int indent) {
        declList.unparse(p, indent);
    }

    // 1 kid
    private DeclListNode declList;
}

class DeclListNode extends ASTnode {
    public DeclListNode(List<DeclNode> S) {
        decls = S;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, process all of the decls in the list.
     */
    public void nameAnalysis(SymTable symTab) {
        nameAnalysis(symTab, symTab);
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab and a global symbol table globalTab
     * (for processing struct names in variable decls), process all of the 
     * decls in the list.
     */    
    public void nameAnalysis(SymTable symTab, SymTable globalTab) {
        for (DeclNode node : decls) {
            if (node instanceof VarDeclNode) {
                ((VarDeclNode)node).nameAnalysis(symTab, globalTab);
            } else {
                node.nameAnalysis(symTab);
            }
        }
    }

    public boolean typeCheck() {
        boolean result = true;
        for (DeclNode node : decls) {
            if (node instanceof FnDeclNode) {
                if(((FnDeclNode)node).typeCheck() == false) {
                    result = false;
                } 
            }
            else continue;
        }
        return result;
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator it = decls.iterator();
        try {
            while (it.hasNext()) {
                ((DeclNode)it.next()).unparse(p, indent);
            }
        } catch (NoSuchElementException ex) {
            System.err.println("unexpected NoSuchElementException in DeclListNode.print");
            System.exit(-1);
        }
    }

    // list of kids (DeclNodes)
    private List<DeclNode> decls;
}

class FormalsListNode extends ASTnode {
    public FormalsListNode(List<FormalDeclNode> S) {
        formalDecls = S;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * for each formal decl in the list
     *     process the formal decl
     *     if there was no error, add type of formal decl to list
     */
    public List<Type> nameAnalysis(SymTable symTab) {
        List<Type> typeList = new LinkedList<Type>();
        for (FormalDeclNode node : formalDecls) {
            Sym sym = node.nameAnalysis(symTab);
            if (sym != null) {
                typeList.add(sym.getType());
            }
        }
        return typeList;
    }    
    
    /**
     * Return the number of formals in this list.
     */
    public int length() {
        return formalDecls.size();
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator<FormalDeclNode> it = formalDecls.iterator();
        if (it.hasNext()) { // if there is at least one element
            it.next().unparse(p, indent);
            while (it.hasNext()) {  // print the rest of the list
                p.print(", ");
                it.next().unparse(p, indent);
            }
        } 
    }

    // list of kids (FormalDeclNodes)
    private List<FormalDeclNode> formalDecls;
}

class FnBodyNode extends ASTnode {
    public FnBodyNode(DeclListNode declList, StmtListNode stmtList) {
        this.declList = declList;
        this.stmtList = stmtList;
    }

    // check right side of statement's list
    public boolean typeCheck(TypeNode rTypeNode){
        return stmtList.typeCheck(rTypeNode);
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the declaration list
     * - process the statement list
     */
    public void nameAnalysis(SymTable symTab) {
        declList.nameAnalysis(symTab);
        stmtList.nameAnalysis(symTab);
    }    
    
    public void unparse(PrintWriter p, int indent) {
        declList.unparse(p, indent);
        stmtList.unparse(p, indent);
    }

    // 2 kids
    private DeclListNode declList;
    private StmtListNode stmtList;
}

class StmtListNode extends ASTnode {
    public StmtListNode(List<StmtNode> S) {
        stmts = S;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, process each statement in the list.
     */
    public void nameAnalysis(SymTable symTab) {
        for (StmtNode node : stmts) {
            node.nameAnalysis(symTab);
        }
    }    

    public boolean typeCheck(TypeNode rTypeNode) {
        boolean result = false;
        for (StmtNode node : stmts) {
            if (node.typeCheck(rTypeNode) == false) {
                result = false; // if one of them is false, the rest should be false
            }
        }
        return result;
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator<StmtNode> it = stmts.iterator();
        while (it.hasNext()) {
            it.next().unparse(p, indent);
        }
    }

    // list of kids (StmtNodes)
    private List<StmtNode> stmts;
}

class ExpListNode extends ASTnode {
    public ExpListNode(List<ExpNode> S) {
        exps = S;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, process each exp in the list.
     */
    public void nameAnalysis(SymTable symTab) {
        for (ExpNode node : exps) {
            node.nameAnalysis(symTab);
        }
    }

    public List<ExpNode> getFormalList() {
        return exps;
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator<ExpNode> it = exps.iterator();
        if (it.hasNext()) { // if there is at least one element
            it.next().unparse(p, indent);
            while (it.hasNext()) {  // print the rest of the list
                p.print(", ");
                it.next().unparse(p, indent);
            }
        } 
    }

    // list of kids (ExpNodes)
    private List<ExpNode> exps;
}

// **********************************************************************
// DeclNode and its subclasses
// **********************************************************************

abstract class DeclNode extends ASTnode {
    /**
     * Note: a formal decl needs to return a sym
     */
    abstract public Sym nameAnalysis(SymTable symTab);
}

class VarDeclNode extends DeclNode {
    public VarDeclNode(TypeNode type, IdNode id, int size) {
        this.type = type;
        this.id = id;
        this.size = size;
    }

    // no need to do typeCheck for VarDeclNode

    /**
     * nameAnalysis (overloaded)
     * Given a symbol table symTab, do:
     * if this name is declared void, then error
     * else if the declaration is of a struct type, 
     *     lookup type name (globally)
     *     if type name doesn't exist, then error
     * if no errors so far,
     *     if name has already been declared in this scope, then error
     *     else add name to local symbol table     
     *
     * symTab is local symbol table (say, for struct field decls)
     * globalTab is global symbol table (for struct type names)
     * symTab and globalTab can be the same
     */
    public Sym nameAnalysis(SymTable symTab) {
        return nameAnalysis(symTab, symTab);
    }
    
    public Sym nameAnalysis(SymTable symTab, SymTable globalTab) {
        boolean badDecl = false;
        String name = id.name();
        Sym sym = null;
        IdNode structId = null;

        if (type instanceof VoidNode) {  // check for void type
            ErrMsg.fatal(id.lineNum(), id.charNum(), 
                         "Non-function declared void");
            badDecl = true;        
        }
        
        else if (type instanceof StructNode) {
            structId = ((StructNode) type).idNode();
            sym = globalTab.lookupGlobal(structId.name());
            
            // if the name for the struct type is not found, 
            // or is not a struct type
            if (sym == null || !(sym instanceof StructDefSym)) {
                ErrMsg.fatal(structId.lineNum(), structId.charNum(), 
                             "Invalid name of struct type");
                badDecl = true;
            }
            else {
                structId.link(sym);
            }
        }
        
        if (symTab.lookupLocal(name) != null) {
            ErrMsg.fatal(id.lineNum(), id.charNum(), 
                         "Multiply declared identifier");
            badDecl = true;            
        }
        
        if (!badDecl) {  // insert into symbol table
            try {
                if (type instanceof StructNode) {
                    sym = new StructSym(structId);
                }
                else {
                    sym = new Sym(type.type());
                }
                symTab.addDecl(name, sym);
                id.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (WrongArgumentException ex) {
		System.err.println("Unexpected WrongArgumentException " +
                                   " in VarDeclNode.nameAnalysis");
		System.exit(-1);
	    }
        }
        
        return sym;
    }    
    
    public void unparse(PrintWriter p, int indent) {
        printSpace(p, indent);
        type.unparse(p, 0);
        p.print(" ");
        p.print(id.name());
        p.println(";");
    }

    // 3 kids
    private TypeNode type;
    private IdNode id;
    private int size;  // use value NOT_STRUCT if this is not a struct type

    public static int NOT_STRUCT = -1;
}

class FnDeclNode extends DeclNode {
    public FnDeclNode(TypeNode type,
                      IdNode id,
                      FormalsListNode formalList,
                      FnBodyNode body) {
        this.type = type;
        this.id = id;
        formalsList = formalList;
        fnBody = body;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this name has already been declared in this scope, then error
     * else add name to local symbol table
     * in any case, do the following:
     *     enter new scope
     *     process the formals
     *     if this function is not multiply declared,
     *         update symbol table entry with types of formals
     *     process the body of the function
     *     exit scope
     */
    public Sym nameAnalysis(SymTable symTab) {
        String name = id.name();
        FnSym sym = null;
        
        if (symTab.lookupLocal(name) != null) {
            ErrMsg.fatal(id.lineNum(), id.charNum(),
                         "Multiply declared identifier");
        }
        
        else { // add function name to local symbol table
            try {
                sym = new FnSym(type.type(), formalsList.length());
                symTab.addDecl(name, sym);
                id.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in FnDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in FnDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (WrongArgumentException ex) {
		System.err.println("Unexpected WrongArgumentException " +
                                   " in VarDeclNode.nameAnalysis");
		System.exit(-1);
	    }
        }
        
        symTab.addScope();  // add a new scope for locals and params
        
        // process the formals
        List<Type> typeList = formalsList.nameAnalysis(symTab);
        if (sym != null) {
            sym.addFormals(typeList);
        }
        
        fnBody.nameAnalysis(symTab); // process the function body
        
        try {
            symTab.removeScope();  // exit scope
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in FnDeclNode.nameAnalysis");
            System.exit(-1);
        }
        
        
        return null;
    }    

    // typeCheck for fnBodyNode
    // 
    public boolean typeCheck() {
        TypeNode rTypeNode = type;
        return  fnBody.typeCheck(rTypeNode);
    }
    
    public void unparse(PrintWriter p, int indent) {
        printSpace(p, indent);
        type.unparse(p, 0);
        p.print(" ");
        p.print(id.name());
        p.print("(");
        formalsList.unparse(p, 0);
        p.println(") {");
        fnBody.unparse(p, indent+4);
        p.println("}\n");
    }

    // 4 kids
    private TypeNode type;
    private IdNode id;
    private FormalsListNode formalsList;
    private FnBodyNode fnBody;
}

class FormalDeclNode extends DeclNode {
    public FormalDeclNode(TypeNode type, IdNode id) {
        this.type = type;
        this.id = id;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this formal is declared void, then error
     * else if this formal is already in the local symble table,
     *     then issue multiply declared error message and return null
     * else add a new entry to the symbol table and return that Sym
     */
    public Sym nameAnalysis(SymTable symTab) {
        String name = id.name();
        boolean badDecl = false;
        Sym sym = null;
        
        if (type instanceof VoidNode) {
            ErrMsg.fatal(id.lineNum(), id.charNum(), 
                         "Non-function declared void");
            badDecl = true;        
        }
        
        if (symTab.lookupLocal(name) != null) {
            ErrMsg.fatal(id.lineNum(), id.charNum(), 
                         "Multiply declared identifier");
            badDecl = true;
        }
        
        if (!badDecl) {  // insert into symbol table
            try {
                sym = new Sym(type.type());
                symTab.addDecl(name, sym);
                id.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (WrongArgumentException ex) {
		System.err.println("Unexpected WrongArgumentException " +
                                   " in VarDeclNode.nameAnalysis");
		System.exit(-1);
	    }
        }
        
        return sym;
    }    
    
    public void unparse(PrintWriter p, int indent) {
        type.unparse(p, 0);
        p.print(" ");
        p.print(id.name());
    }

    // 2 kids
    private TypeNode type;
    private IdNode id;
}

class StructDeclNode extends DeclNode {
    public StructDeclNode(IdNode id, DeclListNode declList) {
        this.id = id;
        this.declList = declList;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this name is already in the symbol table,
     *     then multiply declared error (don't add to symbol table)
     * create a new symbol table for this struct definition
     * process the decl list
     * if no errors
     *     add a new entry to symbol table for this struct
     */
    public Sym nameAnalysis(SymTable symTab) {
        String name = id.name();
        boolean badDecl = false;
        
        if (symTab.lookupLocal(name) != null) {
            ErrMsg.fatal(id.lineNum(), id.charNum(), 
                         "Multiply declared identifier");
            badDecl = true;            
        }

        SymTable structSymTab = new SymTable();
        
        // process the fields of the struct
        declList.nameAnalysis(structSymTab, symTab);
        
        if (!badDecl) {
            try {   // add entry to symbol table
                StructDefSym sym = new StructDefSym(structSymTab);
                symTab.addDecl(name, sym);
                id.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in StructDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in StructDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (WrongArgumentException ex) {
		System.err.println("Unexpected WrongArgumentException " +
                                   " in VarDeclNode.nameAnalysis");
		System.exit(-1);
	    }
        }
        
        return null;
    }    
    
    public void unparse(PrintWriter p, int indent) {
        printSpace(p, indent);
        p.print("struct ");
        p.print(id.name());
        p.println("{");
        declList.unparse(p, indent+4);
        printSpace(p, indent);
        p.println("};\n");

    }

    // 2 kids
    private IdNode id;
    private DeclListNode declList;
}

// **********************************************************************
// TypeNode and its Subclasses
// **********************************************************************

abstract class TypeNode extends ASTnode {
    /* all subclasses must provide a type method */
    abstract public Type type();
}

class IntNode extends TypeNode {
    public IntNode() {
    }

    /**
     * type
     */
    public Type type() {
        return new IntType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("int");
    }
}

class BoolNode extends TypeNode {
    public BoolNode() {
    }

    /**
     * type
     */
    public Type type() {
        return new BoolType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("bool");
    }
}

class VoidNode extends TypeNode {
    public VoidNode() {
    }
    
    /**
     * type
     */
    public Type type() {
        return new VoidType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("void");
    }
}

class StructNode extends TypeNode {
    public StructNode(IdNode id) {
        this.id = id;
    }

    public IdNode idNode() {
        return id;
    }
    
    /**
     * type
     */
    public Type type() {
        return new StructType(id);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("struct ");
        p.print(id.name());
    }
    
    // 1 kid
    private IdNode id;
}

// **********************************************************************
// StmtNode and its subclasses
// **********************************************************************

abstract class StmtNode extends ASTnode {
    abstract public void nameAnalysis(SymTable symTab);
    public boolean typeCheck(TypeNode rTypeNode) {
        return false;
    }
}

class AssignStmtNode extends StmtNode {
    public AssignStmtNode(AssignNode assign) {
        this.assign = assign;
    }

    
    /**
     * nameAnalysis
     * Given a symbo==l table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        assign.nameAnalysis(symTab);
    }

    public boolean typeCheck(TypeNode r) {
        Type t = assign.typeCheck();
        if( t instanceof ErrorType)
            return false;
        else
            return true;
    }

   
    
    public void unparse(PrintWriter p, int indent) {
        printSpace(p, indent);
        assign.unparse(p, -1); // no parentheses
        p.println(";");
    }

    // 1 kid
    private AssignNode assign;
}

class PostIncStmtNode extends StmtNode {
    public PostIncStmtNode(ExpNode exp) {
        this.exp = exp;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        exp.nameAnalysis(symTab);
    }

    public boolean typeCheck(TypeNode r) {
        Type t = exp.typeCheck();
        IdNode id = exp.getExpIdNode();
        if (t instanceof ErrorType) {
            return false;
        }

        if (!(t instanceof IntType)) {
            id.outputError("Arithmetic operator applied to non-numeric operand");
            return false;
        }
        else {
            return true;
        }
    }
    
    public void unparse(PrintWriter p, int indent) {
        printSpace(p, indent);
        exp.unparse(p, 0);
        p.println("++;");
    }

    // 1 kid
    private ExpNode exp;
}

class PostDecStmtNode extends StmtNode {
    public PostDecStmtNode(ExpNode exp) {
        this.exp = exp;
    }

    public boolean typeCheck(TypeNode r) {
        Type t = exp.typeCheck();
        IdNode id = exp.getExpIdNode();
        if (t instanceof ErrorType) {
            return false;
        }

        if (!(t instanceof IntType)) {
            id.outputError("Arithmetic operator applied to non-numeric operand");
            return false;
        }
        else {
            return true;
        }
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        exp.nameAnalysis(symTab);
    }
    
    public void unparse(PrintWriter p, int indent) {
        printSpace(p, indent);
        exp.unparse(p, 0);
        p.println("--;");
    }

    // 1 kid
    private ExpNode exp;
}

class ReadStmtNode extends StmtNode {
    public ReadStmtNode(ExpNode e) {
        exp = e;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        exp.nameAnalysis(symTab);
    }
    
    public boolean typeCheck(TypeNode r) {
        Type t = exp.typeCheck();
        IdNode id = exp.getExpIdNode();
        if (t instanceof ErrorType) {
            return false;
        }
        // Reading a function: e.g., "cin >> f", where f is a function name. 
        if (t instanceof FnType) {
            id.outputError("Attempt to read a function");
            return false;
        }
        //  Reading a struct name; e.g., "cin >> P", where P is the name of a struct type. 
        if (t instanceof StructType) {
            id.outputError("Attempt to read a struct name");
            return false;
        }
        // Reading a struct variable; e.g., "cin >> p", 
        // where p is a variable declared to be of a struct type. 
        if (t instanceof StructDefType) {
            id.outputError("Attempt to read a struct variable");
            return false;
        }

        return true;
    }
    
    public void unparse(PrintWriter p, int indent) {
        printSpace(p, indent);
        p.print("cin >> ");
        exp.unparse(p, 0);
        p.println(";");
    }

    // 1 kid (actually can only be an IdNode or an ArrayExpNode)
    private ExpNode exp;
}

class WriteStmtNode extends StmtNode {
    public WriteStmtNode(ExpNode exp) {
        this.exp = exp;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        exp.nameAnalysis(symTab);
    }

    public boolean typeCheck(TypeNode r) {
        Type t = exp.typeCheck();
        IdNode id = exp.getExpIdNode();
        if (t instanceof ErrorType) {
            return false;
        }
        //  Writing a function; e.g., "cout << f", where f is a function name. 
        if (t instanceof FnType) {
            id.outputError("Attempt to write a function ");
            return false;
        }
        //  Writing a struct name; e.g., "cout << P", where P is the name of a struct type. 
        if (t instanceof StructType) {
            id.outputError("Attempt to write a struct name");
            return false;
        }
        // Writing a struct variable; e.g., "cout << p", 
        // where p is a variable declared to be of a struct type. 
        if (t instanceof StructDefType){
            id.outputError("Attempt to write a struct variable");
            return false;
        }
        // Writing a void value (note: this can only happen if
        // there is an attempt to write the return value 
        //from a void function); e.g., "cout << f()", 
        // where f is a void function. 
        if (t instanceof VoidType) {
            id.outputError("Attempt to write void ");
            return false;
        }
        return true;
    }
    
    public void unparse(PrintWriter p, int indent) {
        printSpace(p, indent);
        p.print("cout << ");
        exp.unparse(p, 0);
        p.println(";");
    }

    // 1 kid
    private ExpNode exp;
}

class IfStmtNode extends StmtNode {
    public IfStmtNode(ExpNode exp, DeclListNode dlist, StmtListNode slist) {
        declList = dlist;
        this.exp = exp;
        stmtList = slist;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts
     * - exit the scope
     */
    public void nameAnalysis(SymTable symTab) {
        exp.nameAnalysis(symTab);
        symTab.addScope();
        declList.nameAnalysis(symTab);
        stmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }

    // IF LPAREN exp:e RPAREN LCURLY varDeclList:vdl stmtList:sl RCURLY

    public boolean typeCheck (TypeNode r) {
        boolean result = true;
        Type t = exp.typeCheck();
        IdNode id = exp.getExpIdNode();
         // Using a non-bool expression as the condition of an if.
        if (!(t instanceof BoolType)) {
            id.outputError("Non-bool expression used as an if condition");
            result = false;
        }

        return result && stmtList.typeCheck(r);
    }
    
    public void unparse(PrintWriter p, int indent) {
        printSpace(p, indent);
        p.print("if (");
        exp.unparse(p, 0);
        p.println(") {");
        declList.unparse(p, indent+4);
        stmtList.unparse(p, indent+4);
        printSpace(p, indent);
        p.println("}");
    }

    // e kids
    private ExpNode exp;
    private DeclListNode declList;
    private StmtListNode stmtList;
}

class IfElseStmtNode extends StmtNode {
    public IfElseStmtNode(ExpNode exp, DeclListNode thenDeclList,
                          StmtListNode thenStmtList, DeclListNode elseDeclList,
                          StmtListNode elseStmtList) {
        this.exp = exp;
        this.thenDeclList = thenDeclList;
        this.thenStmtList = thenStmtList;
        this.elseDeclList = elseDeclList;
        this.elseStmtList = elseStmtList;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts of then
     * - exit the scope
     * - enter a new scope
     * - process the decls and stmts of else
     * - exit the scope
     */
    public void nameAnalysis(SymTable symTab) {
        exp.nameAnalysis(symTab);
        symTab.addScope();
        thenDeclList.nameAnalysis(symTab);
        thenStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
        symTab.addScope();
        elseDeclList.nameAnalysis(symTab);
        elseStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }

    public boolean typeCheck (TypeNode r) {
        boolean result = true;
        Type t = exp.typeCheck();
        IdNode id = exp.getExpIdNode();
        // Using a non-bool expression as the condition of an if.
        if (!(t instanceof BoolType)) {
            id.outputError("Non-bool expression used as an if condition");
            result = false;
        }

        return result && thenStmtList.typeCheck(r) && elseStmtList.typeCheck(r);
    }
    
    public void unparse(PrintWriter p, int indent) {
        printSpace(p, indent);
        p.print("if (");
        exp.unparse(p, 0);
        p.println(") {");
        thenDeclList.unparse(p, indent+4);
        thenStmtList.unparse(p, indent+4);
        printSpace(p, indent);
        p.println("}");
        printSpace(p, indent);
        p.println("else {");
        elseDeclList.unparse(p, indent+4);
        elseStmtList.unparse(p, indent+4);
        printSpace(p, indent);
        p.println("}");        
    }

    // 5 kids
    private ExpNode exp;
    private DeclListNode thenDeclList;
    private StmtListNode thenStmtList;
    private StmtListNode elseStmtList;
    private DeclListNode elseDeclList;
}

class WhileStmtNode extends StmtNode {
    public WhileStmtNode(ExpNode exp, DeclListNode dlist, StmtListNode slist) {
        this.exp = exp;
        declList = dlist;
        stmtList = slist;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts
     * - exit the scope
     */
    public void nameAnalysis(SymTable symTab) {
        exp.nameAnalysis(symTab);
        symTab.addScope();
        declList.nameAnalysis(symTab);
        stmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }

    public boolean typeCheck (TypeNode r) {
        boolean result = true;
        Type t = exp.typeCheck();
        IdNode id = exp.getExpIdNode();
        // Using a non-bool expression as the condition of an if.
        if (!(t instanceof BoolType)) {
            id.outputError("Non-bool expression used as an if condition");
            result = false;
        }
        return result && stmtList.typeCheck(r);
    }
    
    public void unparse(PrintWriter p, int indent) {
        printSpace(p, indent);
        p.print("while (");
        exp.unparse(p, 0);
        p.println(") {");
        declList.unparse(p, indent+4);
        stmtList.unparse(p, indent+4);
        printSpace(p, indent);
        p.println("}");
    }

    // 3 kids
    private ExpNode exp;
    private DeclListNode declList;
    private StmtListNode stmtList;
}

class RepeatStmtNode extends StmtNode {
    public RepeatStmtNode(ExpNode exp, DeclListNode dlist, StmtListNode slist) {
        this.exp = exp;
        declList = dlist;
        stmtList = slist;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts
     * - exit the scope
     */
    public void nameAnalysis(SymTable symTab) {
        exp.nameAnalysis(symTab);
        symTab.addScope();
        declList.nameAnalysis(symTab);
        stmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }

    // repeat loop: Only integer expressions can be used in the times clause of an repeat statement
    // @TODO: Write this later


    public void unparse(PrintWriter p, int indent) {
        printSpace(p, indent);
        p.print("repeat (");
        exp.unparse(p, 0);
        p.println(") {");
        declList.unparse(p, indent+4);
        stmtList.unparse(p, indent+4);
        printSpace(p, indent);
        p.println("}");
    }

    // 3 kids
    private ExpNode exp;
    private DeclListNode declList;
    private StmtListNode stmtList;
}


class CallStmtNode extends StmtNode {
    public CallStmtNode(CallExpNode call) {
        callExp = call;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        callExp.nameAnalysis(symTab);
    }

    public void unparse(PrintWriter p, int indent) {
        printSpace(p, indent);
        callExp.unparse(p, indent);
        p.println(";");
    }

    public boolean typeCheck(TypeNode r) {
        Type t = callExp.typeCheck();
        IdNode id = callExp.getExpIdNode();
        if (t instanceof ErrorType) {
            return false;
        }
        else return true;
    }

    // 1 kid
    private CallExpNode callExp;
}

class ReturnStmtNode extends StmtNode {
    public ReturnStmtNode(ExpNode exp) {
        this.exp = exp;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child,
     * if it has one
     */
    public void nameAnalysis(SymTable symTab) {
        if (exp != null) {
            exp.nameAnalysis(symTab);
        }
    }

    public boolean typeCheck(TypeNode rTypeNode) {
        Type rType = rTypeNode.type();  // get type of the function
        if (exp == null) { // just return, without expression
            if (!(rType instanceof VoidType)) { // non void funrtion with a plain return
                ErrMsg.fatal(0,0,"Missing return value");
                return false;
            }
            return true;
        }
        else { // have expression after return statement
            Type t = exp.typeCheck();
            IdNode id = exp.getExpIdNode();
            //Returning a value from a void function.
            if (rType instanceof VoidType) { // void function
                id.outputError("Return with a value in a void function");
                return false;
            } else if (rType instanceof  ErrorType) {
                return false;
            }
            else {

                if (t.toString().equals(rType.toString())) {
                    return true;
                } else {
                    //Returning a value of the wrong type from a non-void function.
                    id.outputError(" Bad return value ");
                    return false;
                }
            }
        }
    }

    public void unparse(PrintWriter p, int indent) {
        printSpace(p, indent);
        p.print("return");
        if (exp != null) {
            p.print(" ");
            exp.unparse(p, 0);
        }
        p.println(";");
    }

    // 1 kid
    private ExpNode exp; // possibly null
}

// **********************************************************************
// ExpNode and its subclasses
// **********************************************************************

abstract class ExpNode extends ASTnode {
    /**
     * Default version for nodes with no names
     */
    public void nameAnalysis(SymTable symTab) { }
    abstract public IdNode getExpIdNode();
    abstract public Type typeCheck();
}

class IntLitNode extends ExpNode {
    public IntLitNode(int lineNum, int charNum, int intVal) {
        this.lineNum = lineNum;
        this.charNum = charNum;
        this.intVal = intVal;
    }

    // get information for Integer Literal node
     public IdNode getExpIdNode() {
         IdNode id = new IdNode(lineNum, charNum, "int");
         Sym s = new Sym(new IntType());
         id.link(s);
         return id;
     }

     // pass IntType up the tree
    public Type typeCheck() {
        return new IntType();
    }


    public void unparse(PrintWriter p, int indent) {
        p.print(intVal);
    }

    private int lineNum;
    private int charNum;
    private int intVal;
}

class StringLitNode extends ExpNode {
    public StringLitNode(int lineNum, int charNum, String strVal) {
        this.lineNum = lineNum;
        this.charNum = charNum;
        this.strVal = strVal;
    }

    // get information for String Literal Node
    public IdNode getExpIdNode() {
        IdNode id = new IdNode(lineNum, charNum, "string");
        Sym s = new Sym(new IntType());
        id.link(s);
        return id;
    }

    // pass StringType up
    public Type typeCheck() {
        return new StringType();
    }

    public void unparse(PrintWriter p, int indent) {
        p.print(strVal);
    }

    private int lineNum;
    private int charNum;
    private String strVal;
}

class TrueNode extends ExpNode {
    public TrueNode(int lineNum, int charNum) {
        this.lineNum = lineNum;
        this.charNum = charNum;
    }

    // get information for TrueNode
    public IdNode getExpIdNode() {
        IdNode id = new IdNode(lineNum, charNum, "true");
        Sym s = new Sym(new IntType());
        id.link(s);
        return id;
    }

    // pass true node up
    public Type typeCheck() {
        return new BoolType();
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("true");
    }

    private int lineNum;
    private int charNum;
}

class FalseNode extends ExpNode {
    public FalseNode(int lineNum, int charNum) {
        this.lineNum = lineNum;
        this.charNum = charNum;
    }

    // get information for False Node
    public IdNode getExpIdNode() {
        IdNode id = new IdNode(lineNum, charNum, "false");
        Sym s = new Sym(new IntType());
        id.link(s);
        return id;
    }

    // pass false node up
    public Type typeCheck() {
        return new BoolType();
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("false");
    }

    private int lineNum;
    private int charNum;
}

class IdNode extends ExpNode {
    public IdNode(int lineNum, int charNum, String strVal) {
        this.lineNum = lineNum;
        this.charNum = charNum;
        this.strVal = strVal;
    }

    /**
     * Link the given symbol to this ID.
     */
    public void link(Sym sym) {
        this.sym = sym;
    }
    
    /**
     * Return the name of this ID.
     */
    public String name() {
        return strVal;
    }
    
    /**
     * Return the symbol associated with this ID.
     */
    public Sym sym() {
        return sym;
    }
    
    /**
     * Return the line number for this ID.
     */
    public int lineNum() {
        return lineNum;
    }
    
    /**
     * Return the char number for this ID.
     */
    public int charNum() {
        return charNum;
    }    
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - check for use of undeclared name
     * - if ok, link to symbol table entry
     */
    public void nameAnalysis(SymTable symTab) {
        Sym sym = symTab.lookupGlobal(strVal);
        if (sym == null) {
            ErrMsg.fatal(lineNum, charNum, "Undeclared identifier");
        } else {
            link(sym);
        }
    }

    // output error for the typeCheck
    public void outputError(String msg) {
        ErrMsg.fatal(lineNum, charNum, msg);
    }

    public Type typeCheck() {
        return sym.getType();
    }

    public IdNode getExpIdNode(){
        return this;
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print(strVal);
        if (sym != null) {
            p.print("(" + sym + ")");
        }
    }

    private int lineNum;
    private int charNum;
    private String strVal;
    private Sym sym;
}

class DotAccessExpNode extends ExpNode {
    public DotAccessExpNode(ExpNode loc, IdNode id) {
        this.loc = loc;
        this.id = id;
        sym = null;
    }

    /**
     * Return the symbol associated with this dot-access node.
     */
    public Sym sym() {
        return sym;
    }    
    
    /**
     * Return the line number for this dot-access node. 
     * The line number is the one corresponding to the RHS of the dot-access.
     */
    public int lineNum() {
        return id.lineNum();
    }
    
    /**
     * Return the char number for this dot-access node.
     * The char number is the one corresponding to the RHS of the dot-access.
     */
    public int charNum() {
        return id.charNum();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the LHS of the dot-access
     * - process the RHS of the dot-access
     * - if the RHS is of a struct type, set the sym for this node so that
     *   a dot-access "higher up" in the AST can get access to the symbol
     *   table for the appropriate struct definition
     */
    public void nameAnalysis(SymTable symTab) {
        badAccess = false;
        SymTable structSymTab = null; // to lookup RHS of dot-access
        Sym sym = null;
        
        loc.nameAnalysis(symTab);  // do name analysis on LHS
        
        // if loc is really an ID, then sym will be a link to the ID's symbol
        if (loc instanceof IdNode) {
            IdNode id = (IdNode) loc;
            sym = id.sym();
            
            // check ID has been declared to be of a struct type
            
            if (sym == null) { // ID was undeclared
                badAccess = true;
            }
            else if (sym instanceof StructSym) { 
                // get symbol table for struct type
                Sym tempSym = ((StructSym)sym).getStructType().sym();
                structSymTab = ((StructDefSym)tempSym).getSymTable();
            } 
            else {  // LHS is not a struct type
                ErrMsg.fatal(id.lineNum(), id.charNum(), 
                             "Dot-access of non-struct type");
                badAccess = true;
            }
        }
        
        // if loc is really a dot-access (i.e., loc was of the form
        // LHSloc.RHSid), then sym will either be
        // null - indicating RHSid is not of a struct type, or
        // a link to the Sym for the struct type RHSid was declared to be
        else if (loc instanceof DotAccessExpNode) {
            DotAccessExpNode loc = (DotAccessExpNode) this.loc;
            
            if (loc.badAccess) {  // if errors in processing loc
                badAccess = true; // don't continue proccessing this dot-access
            }
            else { //  no errors in processing loc
                sym = loc.sym();

                if (sym == null) {  // no struct in which to look up RHS
                    ErrMsg.fatal(loc.lineNum(), loc.charNum(), 
                                 "Dot-access of non-struct type");
                    badAccess = true;
                }
                else {  // get the struct's symbol table in which to lookup RHS
                    if (sym instanceof StructDefSym) {
                        structSymTab = ((StructDefSym)sym).getSymTable();
                    }
                    else {
                        System.err.println("Unexpected Sym type in DotAccessExpNode");
                        System.exit(-1);
                    }
                }
            }

        }
        
        else { // don't know what kind of thing loc is
            System.err.println("Unexpected node type in LHS of dot-access");
            System.exit(-1);
        }
        
        // do name analysis on RHS of dot-access in the struct's symbol table
        if (!badAccess) {
        
            sym = structSymTab.lookupGlobal(id.name()); // lookup
            if (sym == null) { // not found - RHS is not a valid field name
                ErrMsg.fatal(id.lineNum(), id.charNum(),
                             "Invalid struct field name");
                badAccess = true;
            }
            
            else {
                id.link(sym);  // link the symbol
                // if RHS is itself as struct type, link the symbol for its struct 
                // type to this dot-access node (to allow chained dot-access)
                if (sym instanceof StructSym) {
                    this.sym = ((StructSym)sym).getStructType().sym();
                }
            }
        }
    }

    public IdNode getExpIdNode() {
        return id;
    }


    // get type of struct type
    public Type typeCheck() {
        return id.sym().getType();
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        loc.unparse(p, 0);
        p.print(")");
        p.print(".");
        id.unparse(p, 0);
    }

    // 2 kids
    private ExpNode loc;
    private IdNode id;
    private Sym sym;          // link to Sym for struct type
    private boolean badAccess;  // to prevent multiple, cascading errors
}

class AssignNode extends ExpNode {
    public AssignNode(ExpNode lhs, ExpNode exp) {
        this.lhs = lhs;
        this.exp = exp;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     */
    public void nameAnalysis(SymTable symTab) {
        lhs.nameAnalysis(symTab);
        exp.nameAnalysis(symTab);
    }

    public IdNode getExpIdNode(){
        return lhs.getExpIdNode();
    }


    public Type typeCheck() {
        Type lType = lhs.typeCheck();
        Type rType = exp.typeCheck();
        IdNode id = lhs.getExpIdNode();
        
        //  Assigning a function to a function; e.g., "f = g;",
        // where f and g are function names. 
        if (lType instanceof FnType && rType instanceof FnType) {
            id.outputError("Function assignment");
            return new ErrorType();
        }

        // Assigning a struct name to a struct name; e.g., "A = B;",
        // where A and B are the names of struct types. 
        if (lType instanceof StructDefType && rType instanceof StructDefType) {
            id.outputError("Struct name assignment");
            return new ErrorType();
        }

        // Assigning a struct variable to a struct variable; e.g., "a = b;",
        // where a and b are variables declared to be of struct types. 

        if (lType instanceof StructType && rType instanceof StructType) {
            id.outputError("Struct variable assignment");
            return new ErrorType();
        }

        // if either left or right type is ErrorType
        if (lType instanceof ErrorType || rType instanceof ErrorType) {
            return new ErrorType();
        } else {
           // assigning a value of one type to a variable of another type
           // (e.g., "j = true", where j is of type int). 
           String lTypeString = lType.toString();
           String rTypeString = rType.toString();
           // the types of the left-hand side and right-hand side must be the same.
           if (lTypeString.equals(rTypeString)) {
                // The type of the result of applying the assignment
                // operator is the type of the right-hand side.
                return rType;
           } else {
               id.outputError("Type mismatch");
               return new ErrorType();
           }
        }
    }
    
    public void unparse(PrintWriter p, int indent) {
        if (indent != -1)  p.print("(");
        lhs.unparse(p, 0);
        p.print(" = ");
        exp.unparse(p, 0);
        if (indent != -1)  p.print(")");
    }

    // 2 kids
    private ExpNode lhs;
    private ExpNode exp;
}

class CallExpNode extends ExpNode {
    public CallExpNode(IdNode name, ExpListNode elist) {
        id = name;
        expList = elist;
    }

    public CallExpNode(IdNode name) {
        id = name;
        expList = new ExpListNode(new LinkedList<ExpNode>());
    }
    
    public IdNode getExpIdNode() {
        return id;
    }

    
    public Type typeCheck() {
        boolean result = true;
        Type idType = id.sym().getType(); // get type of the ID
        if (!(idType instanceof FnType)) {
            // Attempt to call a non-function
            id.outputError("Attempt to call a non-function");
            result = false;
        } else {
            // Calling a function with the wrong number of arguments
            FnSym fs = (FnSym)id.sym(); // get sym of the id
            List<Type> paramTypes = fs.getParamTypes();
            List<ExpNode> formal = expList.getFormalList();
            if (paramTypes.size() != formal.size()) {
                id.outputError("Function call with wrong number of args");
                result = false;
            } else {
            // Calling a function with an argument of the wrong type
            int size = formal.size();
            for (int i = 0; i < size; i++) {
                Type callType = formal.get(i).typeCheck();
                Type declType = paramTypes.get(i);
                String callTypeString = callType.toString();
                String declTypeString = declType.toString();
                if (!(callTypeString.equals(declTypeString))) {
                    // If there are several arguments with the wrong type,
                    // you must give an error message for each such argument. 
                    IdNode fId = formal.get(i).getExpIdNode();
                    fId.outputError("Type of actual does not match type of formal");
                    result = false;
                }
            }
            }

        // The number of actuals must match the number of formals.
        }

        if (idType instanceof FnType) 
            return ((FnSym)(id.sym())).getReturnType();
        else
            return new ErrorType();
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     */
    public void nameAnalysis(SymTable symTab) {
        id.nameAnalysis(symTab);
        expList.nameAnalysis(symTab);
    }    
    
    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
        id.unparse(p, 0);
        p.print("(");
        if (expList != null) {
            expList.unparse(p, 0);
        }
        p.print(")");
    }

    // 2 kids
    private IdNode id;
    private ExpListNode expList;  // possibly null
}

abstract class UnaryExpNode extends ExpNode {
    public UnaryExpNode(ExpNode exp) {
        this.exp = exp;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        exp.nameAnalysis(symTab);
    }

    public IdNode getExpIdNode() {
        return exp.getExpIdNode();
    }


    
    // one child
    protected ExpNode exp;
}

abstract class BinaryExpNode extends ExpNode {
    public BinaryExpNode(ExpNode exp1, ExpNode exp2) {
        this.exp1 = exp1;
        this.exp2 = exp2;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     */
    public void nameAnalysis(SymTable symTab) {
        exp1.nameAnalysis(symTab);
        exp2.nameAnalysis(symTab);
    }

    public IdNode getExpIdNode() {
        return exp1.getExpIdNode();
    }

    // check arithemtic opearators
    protected Type checkArithmetic(ExpNode lExp, ExpNode rExp) {
        boolean result = true;
        Type lType = lExp.typeCheck();
        Type rType = rExp.typeCheck();

        // Applying an arithmetic operator (+, -, *, /) 
        // to an operand with type other than int. 
        if (!(lType instanceof ErrorType)) {
            if (!(lType instanceof IntType)) {
                IdNode id = lExp.getExpIdNode();
                id.outputError("Arithmetic operator applied to non-numeric operand");
                result = false;
            }
        } else result = false;
   

        if (!(rType instanceof ErrorType)) {
            if (!(rType instanceof IntType)) {
                IdNode id = lExp.getExpIdNode();
                id.outputError("Arithmetic operator applied to non-numeric operand");
                result = false;
            }
        } else result = false;

        if (result == true) 
            return new IntType();
        else
            return new ErrorType();
        
    }

    // check logical of operators
    protected Type checkLogical(ExpNode lExp, ExpNode rExp) {
        boolean result = true;
        Type lType = lExp.typeCheck();
        Type rType = rExp.typeCheck();

        // Applying a logical operator (!, &&, ||)
        // to an operand with type other than bool.  
        if (!(lType instanceof ErrorType)) {
            if (!(lType instanceof BoolType)) {
                IdNode id = lExp.getExpIdNode();
                id.outputError("Logical operator applied to non-bool operand");
                result = false;
            }
        } else result = false;
   

        if (!(rType instanceof ErrorType)) {
            if (!(rType instanceof BoolType)) {
                IdNode id = lExp.getExpIdNode();
                id.outputError("Logical operator applied to non-bool operand");
                result = false;
            }
        } else result = false;

        if (result == true) 
            return new BoolType();
        else
            return new ErrorType();
    }


    // check relation operators
    

    
    // two kids
    protected ExpNode exp1;
    protected ExpNode exp2;
}

// **********************************************************************
// Subclasses of UnaryExpNode
// **********************************************************************

class UnaryMinusNode extends UnaryExpNode {
    public UnaryMinusNode(ExpNode exp) {
        super(exp);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(-");
        exp.unparse(p, 0);
        p.print(")");
    }
}

class NotNode extends UnaryExpNode {
    public NotNode(ExpNode exp) {
        super(exp);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(!");
        exp.unparse(p, 0);
        p.print(")");
    }
}

// **********************************************************************
// Subclasses of BinaryExpNode
// **********************************************************************

class PlusNode extends BinaryExpNode {
    public PlusNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        exp1.unparse(p, 0);
        p.print(" + ");
        exp2.unparse(p, 0);
        p.print(")");
    }
}

class MinusNode extends BinaryExpNode {
    public MinusNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        exp1.unparse(p, 0);
        p.print(" - ");
        exp2.unparse(p, 0);
        p.print(")");
    }
}

class TimesNode extends BinaryExpNode {
    public TimesNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        exp1.unparse(p, 0);
        p.print(" * ");
        exp2.unparse(p, 0);
        p.print(")");
    }
}

class DivideNode extends BinaryExpNode {
    public DivideNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        exp1.unparse(p, 0);
        p.print(" / ");
        exp2.unparse(p, 0);
        p.print(")");
    }
}

class AndNode extends BinaryExpNode {
    public AndNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        exp1.unparse(p, 0);
        p.print(" && ");
        exp2.unparse(p, 0);
        p.print(")");
    }
}

class OrNode extends BinaryExpNode {
    public OrNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        exp1.unparse(p, 0);
        p.print(" || ");
        exp2.unparse(p, 0);
        p.print(")");
    }
}

class EqualsNode extends BinaryExpNode {
    public EqualsNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        exp1.unparse(p, 0);
        p.print(" == ");
        exp2.unparse(p, 0);
        p.print(")");
    }
}

class NotEqualsNode extends BinaryExpNode {
    public NotEqualsNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        exp1.unparse(p, 0);
        p.print(" != ");
        exp2.unparse(p, 0);
        p.print(")");
    }
}

class LessNode extends BinaryExpNode {
    public LessNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        exp1.unparse(p, 0);
        p.print(" < ");
        exp2.unparse(p, 0);
        p.print(")");
    }
}

class GreaterNode extends BinaryExpNode {
    public GreaterNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        exp1.unparse(p, 0);
        p.print(" > ");
        exp2.unparse(p, 0);
        p.print(")");
    }
}

class LessEqNode extends BinaryExpNode {
    public LessEqNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        exp1.unparse(p, 0);
        p.print(" <= ");
        exp2.unparse(p, 0);
        p.print(")");
    }
}

class GreaterEqNode extends BinaryExpNode {
    public GreaterEqNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        exp1.unparse(p, 0);
        p.print(" >= ");
        exp2.unparse(p, 0);
        p.print(")");
    }
}
