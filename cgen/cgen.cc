
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include <algorithm>

extern void emit_string_constant(ostream& str, char *s);
const int cgen_debug = 0;
const int size_debug = 0;
const int cgen_comments = 0;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


namespace
{
	Symbol cur_class;
	std::vector<Symbol> cur_agrs;
	std::map<Symbol, std::vector<Symbol> > dispTabs;
	std::map<Symbol, std::vector<attr_class*> > attrs;
	int branchInc = 0;

	unsigned getClassDispTabOffset(Symbol cl, Symbol method)
	{
		std::map<Symbol, std::vector<Symbol> >::iterator iter = dispTabs.find(cl);
		if (iter != dispTabs.end())
		{
			std::vector<Symbol>::iterator index = std::find(iter->second.begin(), iter->second.end(), method);
			if (index != iter->second.end())
				return index - iter->second.begin();
			else
				if (cgen_debug)
				   cout << "method " << method << " in class " << cl << " not found.";
		}
		else
		{
			if (cgen_debug) 
			   cout << "class not found: " << cl;
		}
	
		return 0;
	}

	unsigned getClassAttrOffset(Symbol cl, Symbol attr)
	{
		std::map<Symbol, std::vector<attr_class*> >::iterator iter = attrs.find(cl);
		if (iter != attrs.end())
		{
			for (std::vector<attr_class*>::iterator i = iter->second.begin(); i != iter->second.end(); ++i)
			{
				if ((*i)->name == attr)
					return 3 + i - iter->second.begin();
			}
			if (cgen_debug)
				cout << "attribute " << attr << " in class " << cl << " not found.";
		}
		else
		{
			if (cgen_debug) 
			   cout << "class not found: " << cl;
		}
	
		return 3;
	}

	bool isSymbolOneOfClassAttr(Symbol cl, Symbol attr)
	{
		std::map<Symbol, std::vector<attr_class*> >::iterator iter = attrs.find(cl);
		if (iter != attrs.end())
		{
			for (std::vector<attr_class*>::iterator i = iter->second.begin(); i != iter->second.end(); ++i)
			{
				if ((*i)->name == attr)
					return true;
			}
		}
		return false;
	}

	unsigned getArgumentStackOffset(Symbol argName)
	{
		std::vector<Symbol>::iterator index = std::find(cur_agrs.begin(), cur_agrs.end(), argName);
		if (index != cur_agrs.end())
			return 3 + index - cur_agrs.begin();
		else
			if (cgen_debug)
			   cout << "argument " << argName << " was not found.";
		return 3;
	}
}

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable codegen_classtable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s); 
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

namespace
{
	void emit_arithmetic_op(Expression e1, Expression e2, std::string op, ostream &s)
	{
		e1->code(s);
		emit_load(ACC, 3, ACC, s);
		emit_push(ACC, s);
		e2->code(s);
		emit_load(ACC, 3, ACC, s);
		emit_load(T1,1,SP,s);

		if (op == "add")
			emit_add(T1, T1, ACC, s); 
		else if (op == "sub")
			emit_sub(T1, T1, ACC, s); 
		else if (op == "mul")
			emit_mul(T1, T1, ACC, s); 
		else if (op == "div")
			emit_div(T1, T1, ACC, s); 

		emit_store(T1,1,SP,s); // saving the result on the stack
		emit_load_int(ACC,inttable.lookup_string("0"),s);
		emit_jal("Object.copy", s); // generate a temporary
		emit_load(T1,1,SP,s); // taking result from the stack
		emit_addiu(SP,SP,4,s);
		emit_store(T1,3,ACC,s); // assigning the result
	}
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


  emit_disptable_ref(Str, s);

      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

  emit_disptable_ref(Int, s);

      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

  emit_disptable_ref(Bool, s);

      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s), classTagInc(0)
{
   classTags[Object] = classTagInc++;
   classTags[IO] = classTagInc++;

   intclasstag =    classTagInc++ /* Change to your Int class tag here */;
   boolclasstag =   classTagInc++ /* Change to your Bool class tag here */;
   stringclasstag = classTagInc++ /* Change to your String class tag here */;

   classTags[Str] = stringclasstag;
   classTags[Int] = intclasstag;
   classTags[Bool] = boolclasstag;

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  assignClassTags();

  storeAttrOffsets();

  emitClassNameTab();
  emitClassObjTab();
  emitDispTab();
  emitProtos();

  if (cgen_debug)
  {
	cout << "Attributes of " << MAINNAME << " :" << endl;
	std::map<Symbol, std::vector<attr_class*> >::iterator iter = attrs.find(idtable.lookup_string(MAINNAME));
	for (std::vector<attr_class*>::iterator i = iter->second.begin(); i != iter->second.end(); ++i)
	{
		cout << (*i)->name << "; offset: " << getClassAttrOffset(idtable.lookup_string(MAINNAME), (*i)->name) << endl;
	}
  }

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  generateInitMethods();
  generateClassMethods();
}


void CgenClassTable::assignClassTags()
{
//calculateClassSize(nds->tl()->hd());
  for(List<CgenNode> *l = nds; l; l = l->tl())
  {
     std::map<Symbol, int>::iterator it = classTags.find(l->hd()->name);
     if (it == classTags.end())
     {
     	classTags[l->hd()->name] = classTagInc++;
        if (cgen_debug) 
	   cout << "symb: " << l->hd()->name << " tag: " << classTagInc - 1 << "size: " << calculateClassSize(l->hd()) << endl;
     }
     else
     {   
        if (cgen_debug) 
	   cout << "symb: " << l->hd()->name << " tag: " << it->second << "size: " << calculateClassSize(l->hd()) << endl;
     }
  }
}

void CgenClassTable::emitProtos()
{
  emitOneProtObj(lookup(Object));
  for(List<CgenNode> *l = nds; l; l = l->tl())
  {
	if (l->hd()->basic() && (l->hd()->name != Object))
		emitOneProtObj(l->hd());
  }
  for(List<CgenNode> *l = nds; l; l = l->tl())
  {
	if (!l->hd()->basic())
		emitOneProtObj(l->hd());
  }
}

void CgenClassTable::emitOneProtObj(CgenNodeP cl)
{
     str << WORD << "-1" << endl;
     str << cl->name << PROTOBJ_SUFFIX << LABEL;
     str << WORD << classTags[cl->name] << endl; // tag
     str << WORD << calculateClassSize(cl) << endl; // size
     str << WORD << cl->name << DISPTAB_SUFFIX << endl;

     fillObjectLayout(cl);
}

void CgenClassTable::fillObjectLayout(CgenNodeP cl)
{
	if (cl->name == No_class)
		return;

	fillObjectLayout(cl->get_parentnd());
	
	if (cl->name == Str)
	{
		str << WORD << INTCONST_PREFIX << "0" << endl << WORD << "0" << endl;
		return;
	}
	else if (cl->name == Int)
	{
		str << WORD << "0" << endl;
		return;
	}
	else if (cl->name == Bool)
	{
		str << WORD << "0" << endl;
		return;
	}
	else 
	{
		if (cgen_debug)
			cout << "fillObjectLayout: " << cl->name << endl;
		std::map<Symbol, std::vector<attr_class*> >::iterator iter = attrs.find(cl->name);
		if (iter != attrs.end())
		{
			for (std::vector<attr_class*>::iterator i = iter->second.begin(); i != iter->second.end(); ++i)
			{
				str << WORD << (*i)->type_decl << PROTOBJ_SUFFIX << endl;
			}
		}
	}
}

int CgenClassTable::calculateClassSize(CgenNodeP cl)
{
	return 3 + calculateAttrSize(cl);
}

int CgenClassTable::calculateAttrSize(CgenNodeP cl)
{
  if (size_debug) 
	cout << "curr: " << cl->name << endl;

   if (cl->name == No_class)
	return 0;
   if (cl->name == prim_slot)
	return 1;
   if (cl->name == Str)
	return 2;
   if (cl->name == Int)
	return 1;
   if (cl->name == Bool)
	return 1;

   int parents = calculateAttrSize(cl->get_parentnd());

  if (size_debug) 
	cout << "parents: " << parents << endl;
	
   int own = 0;

	for(int i = cl->features->first(); cl->features->more(i); i = cl->features->next(i))
	{
		attr_class* attr_ptr = dynamic_cast<attr_class*>(cl->features->nth(i));
		if (attr_ptr)
		{
			own += calculateAttrSize(lookup(attr_ptr->type_decl));
		}
	}

   return parents + own;	
}

void CgenClassTable::storeAttrOffsets()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
  {
     if (!l->hd()->basic())
     {
	std::map<Symbol, std::vector<attr_class*> >::iterator attr = attrs.insert(std::make_pair(l->hd()->name, std::vector<attr_class*>() ) ).first;
	storeAttrOffsetsWithParents(l->hd(), attr->second);
     }
  }
}

void CgenClassTable::storeAttrOffsetsWithParents(CgenNodeP cl, std::vector<attr_class*>& vect)
{
   if (cl->name == No_class)
	return;
   if (cl->name == prim_slot)
	return;
   /*if (cl->name == Str)
   {
	vect.push_back(val);
	vect.push_back(str_field);
	return;
   }
   if (cl->name == Int)
   {
	vect.push_back(val);
	return;
   }
   if (cl->name == Bool)
   {
	vect.push_back(val);
	return;
   }*/

   storeAttrOffsetsWithParents(cl->get_parentnd(), vect);

	for(int i = cl->features->first(); cl->features->more(i); i = cl->features->next(i))
	{
		attr_class* attr_ptr = dynamic_cast<attr_class*>(cl->features->nth(i));
		if (attr_ptr)
		{
			vect.push_back(attr_ptr);
		}
	}
}

void CgenClassTable::emitClassNameTab()
{
  str << CLASSNAMETAB << LABEL;

  std::vector<CgenNodeP> v;
  v.push_back(root());
  
  while (!v.empty())
  {
	for (std::vector<CgenNodeP>::iterator i = v.begin(); i != v.end(); i++)
	{
		StringEntry* entry = stringtable.lookup_string((*i)->name->get_string());
	     	if (entry)
		{
			str << WORD; 
			entry->code_ref(str);
			str << endl;
		}
	}

	std::vector<CgenNodeP> temp;

	for (std::vector<CgenNodeP>::iterator i = v.begin(); i != v.end(); i++)
	{
		for(List<CgenNode>* l = (*i)->get_children(); l; l = l->tl())
		{
			temp.push_back(l->hd());
		}
	}
	v = temp;
  }
}

void CgenClassTable::emitDispTab()
{
  str << Object << DISPTAB_SUFFIX << LABEL;
  {
	  std::map<Symbol, std::vector<Symbol> >::iterator classDispTab = dispTabs.insert(std::make_pair(Object, std::vector<Symbol>() ) ).first;
	  generateClassDispTab(lookup(Object), classDispTab->second);
  }

  for(List<CgenNode> *l = nds; l; l = l->tl())
  {
     if (l->hd()->name != Object)
     {
       str << l->hd()->name << DISPTAB_SUFFIX << LABEL;

	std::map<Symbol, std::vector<Symbol> >::iterator classDispTab = dispTabs.insert(std::make_pair(l->hd()->name, std::vector<Symbol>() ) ).first;
	emitDispTabWithParents(l->hd(), classDispTab->second);
     }
  }
}

void CgenClassTable::emitDispTabWithParents(CgenNodeP cl, std::vector<Symbol>& classDispTab)
{
	if (cl->name == No_class)
		return;
	if (cl->name == prim_slot)
		return;
	
	emitDispTabWithParents(cl->get_parentnd(), classDispTab);
	generateClassDispTab(cl, classDispTab);
	// todo: Some methods could be overwritten in derived classes. Fix that.
}

void CgenClassTable::generateClassDispTab(CgenNodeP cl, std::vector<Symbol>& classDispTab)
{
	for(int i = cl->features->first(); cl->features->more(i); i = cl->features->next(i))
	{
		method_class* meth_ptr = dynamic_cast<method_class*>(cl->features->nth(i));
		if (meth_ptr)
		{
			str << WORD << cl->name << METHOD_SEP << meth_ptr->name << endl;
			classDispTab.push_back(meth_ptr->name);
		}
	}
}

void CgenClassTable::emitClassObjTab()
{
  str << CLASSOBJTAB << LABEL;
  std::map<int, Symbol> classTagsRev;

  for (std::map<Symbol, int>::iterator it = classTags.begin(); it != classTags.end(); ++it)
  {
	classTagsRev[it->second] = it->first;
  }

  for (std::map<int, Symbol>::iterator it = classTagsRev.begin(); it != classTagsRev.end(); ++it)
  {
	str << WORD << it->second << PROTOBJ_SUFFIX << endl;
	str << WORD << it->second << CLASSINIT_SUFFIX << endl;
  }
}

void CgenClassTable::generateInitMethods()
{
  generateInitMethodForClass(Object);
  for(List<CgenNode> *l = nds; l; l = l->tl())
  {
     if (l->hd()->name != Object)
	     if (l->hd()->basic())
		generateInitMethodForClass(l->hd()->name);
  }
}

void CgenClassTable::generateInitMethodForClass(Symbol cl)
{
     str << cl << CLASSINIT_SUFFIX << LABEL;
     emit_addiu(SP,SP,-12,str);
     emit_store(FP,3,SP,str);
     emit_store(SELF,2,SP,str);
     emit_store(RA,1,SP,str);
     emit_addiu(FP,SP,4,str);
     emit_move(SELF, ACC, str);

     // Call to initialize parent object     
     CgenNodeP c = lookup(cl);
     if (c->get_parentnd()->name != No_class)
     {
	     std::string s = c->get_parentnd()->name->get_string();
	     s += CLASSINIT_SUFFIX;
	     emit_jal((char*)s.c_str(), str);
     }

     if(!attrsToInit.empty())
     {
	for (std::vector<attr_class*>::iterator i = attrsToInit.begin(); i != attrsToInit.end(); ++i)
	{
		no_expr_class* no_expr_ptr = dynamic_cast<no_expr_class*>((*i)->init);
		if (!no_expr_ptr)
		{
			// if init differs from no_expr, code it
			(*i)->init->code(str);
			emit_store(ACC, 3 + i - attrsToInit.begin(), SELF, str);
		}
	}
     }

     emit_move(ACC, SELF, str);
     emit_load(FP,3,SP,str);
     emit_load(SELF,2,SP,str);
     emit_load(RA,1,SP,str);
     emit_addiu(SP,SP,12,str);
     emit_return(str);
}

void CgenClassTable::generateClassMethods()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
  {
     if (!l->hd()->basic())
     {
	cur_class = l->hd()->name;
	attrsToInit.clear();
	for(int i = l->hd()->features->first(); l->hd()->features->more(i); i = l->hd()->features->next(i))
	{
		attr_class* attr_ptr = dynamic_cast<attr_class*>(l->hd()->features->nth(i));
		if (attr_ptr)
		{
			attrsToInit.push_back(attr_ptr);
		}
	}
	generateInitMethodForClass(l->hd()->name);
	for(int i = l->hd()->features->first(); l->hd()->features->more(i); i = l->hd()->features->next(i))
	{
		method_class* meth_ptr = dynamic_cast<method_class*>(l->hd()->features->nth(i));
		if (meth_ptr)
		{
			cur_agrs.clear();
			// store arguments offsets
			for(int i = meth_ptr->formals->first(); meth_ptr->formals->more(i); i = meth_ptr->formals->next(i))
		  	{
				formal_class* formal_ptr = dynamic_cast<formal_class*>(meth_ptr->formals->nth(i));
				if (formal_ptr)
					cur_agrs.push_back(formal_ptr->name);
			}

			str << l->hd()->name << METHOD_SEP << meth_ptr->name << LABEL;
			generateCodeForClassMethod(meth_ptr);
		}
	}
	
     }
  }
}

void CgenClassTable::generateCodeForClassMethod(method_class* meth_ptr)
{
     emit_addiu(SP,SP,-12,str);
     emit_store(FP,3,SP,str);
     emit_store(SELF,2,SP,str);
     emit_store(RA,1,SP,str);
     emit_addiu(FP,SP,4,str);
     emit_move(SELF, ACC, str);
     //emit_move(ACC, SELF, str);
	
     meth_ptr->expr->code(str);

     emit_load(FP,3,SP,str);
     emit_load(SELF,2,SP,str);
     emit_load(RA,1,SP,str);
     emit_addiu(SP,SP,12 + cur_agrs.size() * 4,str);
     emit_return(str);
}

CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s) 
{
	if (cgen_comments)
	  s << COMMENT << " coding assign to " << name << endl;

	if (isSymbolOneOfClassAttr(cur_class, name))
	{
		expr->code(s); // la	$a0 int_const0	
		emit_store(ACC, getClassAttrOffset(cur_class, name), SELF, s); //sw	$a0 <offset of the attr>($s0)
	}
	else // lhs is an argument of the method
	{
		expr->code(s); // la	$a0 int_const0
		emit_store(ACC, getArgumentStackOffset(name), FP, s); //sw	$a0 <offset of the arg>($FP)
	}
}

void static_dispatch_class::code(ostream &s) {
}

void dispatch_class::code(ostream &s) 
{
	if (name == cool_abort)
	{
		emit_bne(ACC, ZERO, branchInc, s);
		emit_load_address(ACC, "str_const0", s);
		emit_load_imm(T1, 1, s);

		std::string str = "_dispatch_";
		str += name->get_string();
		emit_jal((char*)str.c_str(), s);

		emit_label_def(branchInc, s);
		emit_load(T1, 2, ACC, s);
		emit_load(T1, 0, T1, s);
		emit_jalr(T1, s);

		branchInc++;
	}
	else
	{
		if (cgen_comments)
		  s << COMMENT << " coding dispatch begin" << endl;
		//if (cgen_comments)
		//  s << COMMENT << " \t saving callee's frame pointer" << endl;
		// saving callee's frame pointer
		//emit_push(FP, s);

		if (cgen_comments)
		  s << COMMENT << " \t pushing arguments to the stack" << endl;
		// pushing arguments to the stack
		for(int i = actual->first(); actual->more(i); i = actual->next(i)) // ToDo: reverse the order
		{
			actual->nth(i)->code(s);
			emit_push(ACC, s);
		}

		if (cgen_comments)
		  s << COMMENT << " \t calling " << cur_class << "::" << name << endl;
		// calling the function and saving return address in $ra
		emit_load(T1, 2, SELF, s); // load dispatch table
		emit_load(T1, getClassDispTabOffset(cur_class, name), T1, s); // load function address								
		emit_jalr(T1, s); // call the function	

		for(int i = actual->first(); actual->more(i); i = actual->next(i))
		{
			// for IO::out_int top of the stack will be poped automatically
			//emit_addiu(SP,SP,4,s);
		}

		if (cgen_comments)
		  s << COMMENT << " coding dispatch end" << endl;
	}
}

void cond_class::code(ostream &s) 
{
/*	if (cgen_comments)
	  s << COMMENT << " coding branch" << endl;
*/
	pred->code(s);
	else_exp->code(s);
	emit_branch(branchInc + 1, s);
	emit_label_def(branchInc, s);
	then_exp->code(s);
	emit_label_def(branchInc + 1, s);

	branchInc += 2; // 2 labels per if-then-else statement
}

void loop_class::code(ostream &s) 
{
	if (cgen_comments)
	  s << COMMENT << " coding loop" << endl;
}

void typcase_class::code(ostream &s) {
}

void block_class::code(ostream &s) 
{
  for(int i = body->first(); body->more(i); i = body->next(i))
  {
	body->nth(i)->code(s);
  }
}

void let_class::code(ostream &s) {
}

void plus_class::code(ostream &s) 
{
  if (cgen_comments)
	  s << COMMENT << " coding plus begin" << endl;
	emit_arithmetic_op(e1, e2, "add", s);
  if (cgen_comments)
	  s << COMMENT << " coding plus end" << endl;
}

void sub_class::code(ostream &s) 
{
	emit_arithmetic_op(e1, e2, "sub", s);
}

void mul_class::code(ostream &s) 
{
	emit_arithmetic_op(e1, e2, "mul", s);
}

void divide_class::code(ostream &s) 
{
	emit_arithmetic_op(e1, e2, "div", s);
}

void neg_class::code(ostream &s) 
{
	emit_neg(ACC,ACC,s);
}

void lt_class::code(ostream &s) {
}

void eq_class::code(ostream &s) 
{
	e1->code(s);
	emit_load(ACC, 3, ACC, s);
	emit_push(ACC, s);
	e2->code(s);
	emit_load(ACC, 3, ACC, s);
	emit_load(T1,1,SP,s);
	emit_addiu(SP,SP,4,s);

	emit_beq(ACC, T1, branchInc, s);
}

void leq_class::code(ostream &s) {
}

void comp_class::code(ostream &s) {
}

void int_const_class::code(ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  if (cgen_comments)
	  s << COMMENT << " load " << token->get_string() << endl;
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);  
}

void string_const_class::code(ostream& s)
{
  if (cgen_comments)
	  s << COMMENT << " load " << token->get_string() << endl;
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s)
{
  if (cgen_comments)
	  s << COMMENT << " load " << val << endl;
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
}

void isvoid_class::code(ostream &s) {
}

void no_expr_class::code(ostream &s) {
}

void object_class::code(ostream &s) 
{
  	if (cgen_comments)
	  s << COMMENT << " coding object " << name << endl;

	if (isSymbolOneOfClassAttr(cur_class, name))
	{
		emit_load(ACC, getClassAttrOffset(cur_class, name), SELF, s);
	}
	else // lhs is an argument of the method
	{
		emit_load(ACC, getArgumentStackOffset(name), FP, s);
	}
  
}


