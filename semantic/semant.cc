

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
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



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) 
{
//  classes->dump(error_stream, 1);

  symTab.enterscope();
  install_basic_classes();
  install_user_classes(classes);
  //symTab.dump();

  for(int i = classes->first(); classes->more(i); i = classes->next(i))
  {
	class__class* class_ptr = (class__class*)classes->nth(i);
	checkClass(class_ptr);
	checkClassFeatures(class_ptr->features, class_ptr);
  }
}

void ClassTable::install_user_classes(Classes classes)
{
  for(int i = classes->first(); classes->more(i); i = classes->next(i))
  {
	class__class* ptr = (class__class*)classes->nth(i);
	Class_* pptr = new Class_(ptr); 
	symTab.addid(ptr->name, pptr);
  }
}

void ClassTable::checkClass(class__class* class_ptr)
{
	if (!symTab.lookup(class_ptr->parent))
		semant_error(class_ptr) << endl;
}

void ClassTable::checkClassFeatures(Features features, class__class* class_ptr)
{
	for(int i = features->first(); features->more(i); i = features->next(i))
  	{
		attr_class* attr_ptr = dynamic_cast<attr_class*>(features->nth(i));
		if (attr_ptr)
		{
			checkAttribute(attr_ptr, class_ptr);
		}
		else
		{
			method_class* meth_ptr = dynamic_cast<method_class*>(features->nth(i));
			if (meth_ptr)
				checkMethod(meth_ptr, class_ptr);
		}		
	}
}

void ClassTable::checkAttribute(attr_class* attr_ptr, class__class* class_ptr)
{
	//cout << "attribute : " << attr_ptr->name << endl;
	if (!symTab.lookup(attr_ptr->type_decl))
		semant_error(class_ptr) << endl;

	if (attr_ptr->name->equal_string("self", 4))
		semant_error(class_ptr) << endl;

	checkExpression(attr_ptr->init, class_ptr);
}

void ClassTable::checkMethod(method_class* meth_ptr, class__class* class_ptr)
{
	//cout << "method : " << meth_ptr->name << endl;
	for(int i = meth_ptr->formals->first(); meth_ptr->formals->more(i); i = meth_ptr->formals->next(i))
  	{
		formal_class* formal_ptr = dynamic_cast<formal_class*>(meth_ptr->formals->nth(i));
		if (formal_ptr)
			checkFormal(formal_ptr, class_ptr);
	}
}

void ClassTable::checkFormal(formal_class* formal_ptr, class__class* class_ptr)
{
	//cout << "formal : " << formal_ptr->name << endl;
	if (!symTab.lookup(formal_ptr->type_decl))
		semant_error(class_ptr) << endl;
}

void ClassTable::checkExpression(Expression expr_ptr, class__class* class_ptr)
{
	{
		assign_class* expr = dynamic_cast<assign_class*>(expr_ptr);
		if (expr)
			check_assign(expr, class_ptr);
	}
	{
		static_dispatch_class* expr = dynamic_cast<static_dispatch_class*>(expr_ptr);
		if (expr)
			check_static_dispatch(expr, class_ptr);
	}
	{
		dispatch_class* expr = dynamic_cast<dispatch_class*>(expr_ptr);
		if (expr)
			check_dispatch(expr, class_ptr);
	}
	{
		cond_class* expr = dynamic_cast<cond_class*>(expr_ptr);
		if (expr)
			check_cond(expr, class_ptr);
	}
	{
		loop_class* expr = dynamic_cast<loop_class*>(expr_ptr);
		if (expr)
			check_loop(expr, class_ptr);
	}
	{
		typcase_class* expr = dynamic_cast<typcase_class*>(expr_ptr);
		if (expr)
			check_typcase(expr, class_ptr);
	}
	{
		block_class* expr = dynamic_cast<block_class*>(expr_ptr);
		if (expr)
			check_block(expr, class_ptr);
	}
	{
		let_class* expr = dynamic_cast<let_class*>(expr_ptr);
		if (expr)
			check_let(expr, class_ptr);
	}
	{
		plus_class* expr = dynamic_cast<plus_class*>(expr_ptr);
		if (expr)
			check_plus(expr, class_ptr);
	}
	{
		sub_class* expr = dynamic_cast<sub_class*>(expr_ptr);
		if (expr)
			check_sub(expr, class_ptr);
	}
	{
		mul_class* expr = dynamic_cast<mul_class*>(expr_ptr);
		if (expr)
			check_mul(expr, class_ptr);
	}
	{
		divide_class* expr = dynamic_cast<divide_class*>(expr_ptr);
		if (expr)
			check_divide(expr, class_ptr);
	}
	{
		neg_class* expr = dynamic_cast<neg_class*>(expr_ptr);
		if (expr)
			check_neg(expr, class_ptr);
	}
	{
		lt_class* expr = dynamic_cast<lt_class*>(expr_ptr);
		if (expr)
			check_lt(expr, class_ptr);
	}
	{
		eq_class* expr = dynamic_cast<eq_class*>(expr_ptr);
		if (expr)
			check_eq(expr, class_ptr);
	}
	{
		leq_class* expr = dynamic_cast<leq_class*>(expr_ptr);
		if (expr)
			check_leq(expr, class_ptr);
	}
	{
		comp_class* expr = dynamic_cast<comp_class*>(expr_ptr);
		if (expr)
			check_comp(expr, class_ptr);
	}
	{
		int_const_class* expr = dynamic_cast<int_const_class*>(expr_ptr);
		if (expr)
			check_int_const(expr, class_ptr);
	}
	{
		bool_const_class* expr = dynamic_cast<bool_const_class*>(expr_ptr);
		if (expr)
			check_bool_const(expr, class_ptr);
	}
	{
		string_const_class* expr = dynamic_cast<string_const_class*>(expr_ptr);
		if (expr)
			check_string_const(expr, class_ptr);
	}
	{
		new__class* expr = dynamic_cast<new__class*>(expr_ptr);
		if (expr)
			check_new_(expr, class_ptr);
	}
	{
		isvoid_class* expr = dynamic_cast<isvoid_class*>(expr_ptr);
		if (expr)
			check_isvoid(expr, class_ptr);
	}
	{
		no_expr_class* expr = dynamic_cast<no_expr_class*>(expr_ptr);
		if (expr)
			check_no_expr(expr, class_ptr);
	}
	{
		object_class* expr = dynamic_cast<object_class*>(expr_ptr);
		if (expr)
			check_object(expr, class_ptr);
	}
}

void ClassTable::check_assign(assign_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_static_dispatch(static_dispatch_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_dispatch(dispatch_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_cond(cond_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_loop(loop_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_typcase(typcase_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_block(block_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_let(let_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_plus(plus_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_sub(sub_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_mul(mul_class* expr, class__class* class_ptrn)
{
}

void ClassTable::check_divide(divide_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_neg(neg_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_lt(lt_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_eq(eq_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_leq(leq_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_comp(comp_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_int_const(int_const_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_bool_const(bool_const_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_string_const(string_const_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_new_(new__class* expr, class__class* class_ptr)
{
}

void ClassTable::check_isvoid(isvoid_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_no_expr(no_expr_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_object(object_class* expr, class__class* class_ptr)
{
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
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
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
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
	       filename);

    symTab.addid(Object, &Object_class);
    symTab.addid(IO, &IO_class);
    symTab.addid(Int, &Int_class);
    symTab.addid(Bool, &Bool_class);
    symTab.addid(Str, &Str_class);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


