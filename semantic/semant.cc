

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include <set>
#include <string>

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

  types.enterscope();
  install_basic_classes();
  install_user_classes(classes);
  //types.dump();

  if (!types.lookup(Main))
	semant_error() << "Class Main is not defined." << endl;

  for(int i = classes->first(); classes->more(i); i = classes->next(i))
  {
	Class__class* cl_ptr = classes->nth(i);
	class__class* class_ptr = dynamic_cast<class__class*>(cl_ptr);	
	
	types.enterscope();
	vars.enterscope();

	vars.addid(self, &class_ptr->name);
	types.addid(SELF_TYPE, &cl_ptr);
	
	checkClass(class_ptr);
	collectClassAttributes(class_ptr->features);
	checkClassFeatures(class_ptr->features, class_ptr);

	vars.exitscope();
	types.exitscope();
  }

  types.exitscope();
}

void ClassTable::install_user_classes(Classes classes)
{
  for(int i = classes->first(); classes->more(i); i = classes->next(i))
  {
	class__class* class_ptr = dynamic_cast<class__class*>(classes->nth(i));	
	Class_* pptr = new Class_(class_ptr); 
	types.addid(class_ptr->name, pptr);
  }
}

void ClassTable::checkClass(class__class* class_ptr)
{
	if (class_ptr->name == Int || class_ptr->name == Bool || class_ptr->name == Str || class_ptr->name == SELF_TYPE)
		semant_error(class_ptr) << endl;

	if (class_ptr->parent == Int || class_ptr->parent == Bool || class_ptr->parent == Str || class_ptr->parent == SELF_TYPE)
		semant_error(class_ptr) << endl;
	else if (!types.lookup(class_ptr->parent))
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

void ClassTable::collectClassAttributes(Features features)
{
	for(int i = features->first(); features->more(i); i = features->next(i))
  	{
		attr_class* attr_ptr = dynamic_cast<attr_class*>(features->nth(i));
		if (attr_ptr)
			vars.addid(attr_ptr->name, &attr_ptr->type_decl);
	}
}

void ClassTable::checkAttribute(attr_class* attr_ptr, class__class* class_ptr)
{
	//cerr << "attribute : " << attr_ptr->name << endl;
	if (!types.lookup(attr_ptr->type_decl))
		semant_error(class_ptr) << endl;

	if (attr_ptr->name == self)
		semant_error(class_ptr) << endl;

	checkAttrIsNotDefinedInParents(attr_ptr->name, class_ptr);

	checkExpression(attr_ptr->init, class_ptr);

	if (!isExpressionNoOp(attr_ptr->init) && !isAsubtypeofB(getTypeOfExpression(attr_ptr->init, class_ptr), attr_ptr->type_decl))
		semant_error(class_ptr) << endl;
}

void ClassTable::checkAttrIsNotDefinedInParents(Symbol attr, class__class* class_ptr)
{
	Class_ parent = *types.lookup(class_ptr->parent);

	if (!parent)
	{
		cerr << "checkAttrIsNotDefinedInParents - symbol was not found." << endl;
		return;
	}

	class__class* parent_ptr = dynamic_cast<class__class*>(parent);

	while (parent_ptr->name != Object)
	{
		for(int i = parent_ptr->features->first(); parent_ptr->features->more(i); i = parent_ptr->features->next(i))
	  	{
			attr_class* attr_ptr = dynamic_cast<attr_class*>(parent_ptr->features->nth(i));
			if (attr_ptr && attr_ptr->name == attr)
			{
				semant_error(class_ptr) << endl;
				return;
			}
		}
		parent = *types.lookup(parent_ptr->parent);
		if (!parent)
		{
			cerr << "checkAttrIsNotDefinedInParents - parent was not found." << endl;
			return;
		}

		parent_ptr = dynamic_cast<class__class*>(parent);
		if (!parent_ptr)
		{
			cerr << "checkAttrIsNotDefinedInParents - parent was not casted." << endl;
			return;
		}
	}
}

void ClassTable::checkMethod(method_class* meth_ptr, class__class* class_ptr)
{
	//cerr << "method : " << meth_ptr->name << endl;

	if (!types.lookup(meth_ptr->return_type))
		semant_error(class_ptr) << endl;

	for(int i = meth_ptr->formals->first(); meth_ptr->formals->more(i); i = meth_ptr->formals->next(i))
  	{
		formal_class* formal_ptr = dynamic_cast<formal_class*>(meth_ptr->formals->nth(i));
		if (formal_ptr)
			checkFormal(formal_ptr, class_ptr);
	}

	checkDupFormals(meth_ptr, class_ptr);

	vars.enterscope();
	collectFormals(meth_ptr);
	checkExpression(meth_ptr->expr, class_ptr);

	if (!isAsubtypeofB(getTypeOfExpression(meth_ptr->expr, class_ptr), meth_ptr->return_type))
		semant_error(class_ptr) << endl;

	vars.exitscope();
}

void ClassTable::checkDupFormals(method_class* meth_ptr, class__class* class_ptr)
{
	std::set<Symbol> tab;
	
	for(int i = meth_ptr->formals->first(); meth_ptr->formals->more(i); i = meth_ptr->formals->next(i))
  	{
		formal_class* formal_ptr = dynamic_cast<formal_class*>(meth_ptr->formals->nth(i));
		if (formal_ptr)
		{
			if (tab.find(formal_ptr->name) == tab.end())
				tab.insert(formal_ptr->name);
			else
				semant_error(class_ptr) << endl;
		}	
	}
}

void ClassTable::checkFormal(formal_class* formal_ptr, class__class* class_ptr)
{
	//cout << "formal : " << formal_ptr->name << endl;
	if (formal_ptr->name == self)
		semant_error(class_ptr) << endl;

	if (formal_ptr->type_decl == SELF_TYPE)
		semant_error(class_ptr) << endl;
	else if (!types.lookup(formal_ptr->type_decl))
		semant_error(class_ptr) << endl;
}

void ClassTable::collectFormals(method_class* meth_ptr)
{
	for(int i = meth_ptr->formals->first(); meth_ptr->formals->more(i); i = meth_ptr->formals->next(i))
  	{
		formal_class* formal_ptr = dynamic_cast<formal_class*>(meth_ptr->formals->nth(i));
		if (formal_ptr)
			vars.addid(formal_ptr->name, &formal_ptr->type_decl);
	}
}

void ClassTable::check_branch(branch_class* expr, class__class* class_ptr)
{
	//cout << "branch : " << branch_class->name << endl;
	if (!types.lookup(expr->type_decl))
		semant_error(class_ptr) << endl;

	checkExpression(expr->expr, class_ptr);
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
			checkArithmeticOrComparisonExpression(expr, class_ptr);
	}
	{
		sub_class* expr = dynamic_cast<sub_class*>(expr_ptr);
		if (expr)
			checkArithmeticOrComparisonExpression(expr, class_ptr);
	}
	{
		mul_class* expr = dynamic_cast<mul_class*>(expr_ptr);
		if (expr)
			checkArithmeticOrComparisonExpression(expr, class_ptr);
	}
	{
		divide_class* expr = dynamic_cast<divide_class*>(expr_ptr);
		if (expr)
			checkArithmeticOrComparisonExpression(expr, class_ptr);
	}
	{
		neg_class* expr = dynamic_cast<neg_class*>(expr_ptr);
		if (expr)
			check_neg(expr, class_ptr);
	}
	{
		lt_class* expr = dynamic_cast<lt_class*>(expr_ptr);
		if (expr)
			checkArithmeticOrComparisonExpression(expr, class_ptr);
	}
	{
		eq_class* expr = dynamic_cast<eq_class*>(expr_ptr);
		if (expr)
			check_eq(expr, class_ptr);
	}
	{
		leq_class* expr = dynamic_cast<leq_class*>(expr_ptr);
		if (expr)
			checkArithmeticOrComparisonExpression(expr, class_ptr);
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

template <class T>
void ClassTable::checkArithmeticOrComparisonExpression(T* expr, class__class* class_ptr)
{
	checkExpression(expr->e1, class_ptr);
	checkExpression(expr->e2, class_ptr);

	Symbol T1 = getTypeOfExpression(expr->e1, class_ptr);
	Symbol T2 = getTypeOfExpression(expr->e2, class_ptr);
	if (T1 != Int || T2 != Int)
		semant_error(class_ptr) << endl;
}

void ClassTable::check_assign(assign_class* expr, class__class* class_ptr)
{
	Symbol* IdType = vars.lookup(expr->name);
	if (!IdType)
	{
		semant_error(class_ptr) << endl;
	}
	else
	{
		Symbol T1 = getTypeOfExpression(expr->expr, class_ptr);
		if (!isAsubtypeofB(T1, *IdType))
			semant_error(class_ptr) << endl;
	}
}

void ClassTable::check_static_dispatch(static_dispatch_class* expr, class__class* class_ptr)
{
	checkExpression(expr->expr, class_ptr);
	Class_ T = *types.lookup(expr->type_name);
	class__class* cl_ptr = dynamic_cast<class__class*>(T);
	if (!checkMethodExist(cl_ptr, expr->name))
		semant_error(class_ptr) << endl;

	if (!isAsubtypeofB(getTypeOfExpression(expr->expr, class_ptr), expr->type_name))
		semant_error(class_ptr) << endl;
	
	checkMethodFormals(dynamic_cast<class__class*>(T), expr->name, expr->actual, class_ptr);
}

void ClassTable::check_dispatch(dispatch_class* expr, class__class* class_ptr)
{
	checkExpression(expr->expr, class_ptr);
	Class_ T = *types.lookup(getTypeOfExpression(expr->expr, class_ptr));
	if (!checkMethodExist(dynamic_cast<class__class*>(T), expr->name))
		semant_error(class_ptr) << endl;
	
	checkMethodFormals(dynamic_cast<class__class*>(T), expr->name, expr->actual, class_ptr);
}

bool ClassTable::checkMethodExist(class__class* cl, Symbol method)
{
	for(int i = cl->features->first(); cl->features->more(i); i = cl->features->next(i))
  	{
		method_class* meth_ptr = dynamic_cast<method_class*>(cl->features->nth(i));
		if (meth_ptr && meth_ptr->name == method)
			return true;
	}
	return false;
}

void ClassTable::checkMethodFormals(class__class* cl, Symbol method, Expressions exprs, class__class* class_ptr)
{
	for(int i = cl->features->first(); cl->features->more(i); i = cl->features->next(i))
  	{
		method_class* meth_ptr = dynamic_cast<method_class*>(cl->features->nth(i));
		if (meth_ptr && meth_ptr->name == method)
		{
			if (meth_ptr->formals->len() != exprs->len())
				semant_error(class_ptr) << endl;
			
			for(int i = meth_ptr->formals->first(); meth_ptr->formals->more(i); i = meth_ptr->formals->next(i))
			{
				Symbol T1 = (dynamic_cast<formal_class*>(meth_ptr->formals->nth(i)))->type_decl;
				Symbol T2 = getTypeOfExpression(exprs->nth(i), class_ptr);
				
				if (!isAsubtypeofB(T2, T1))
					semant_error(class_ptr) << endl;
			}
		}
	}
}

Symbol ClassTable::getMethodReturnType(class__class* cl, Symbol method)
{
	for(int i = cl->features->first(); cl->features->more(i); i = cl->features->next(i))
  	{
		method_class* meth_ptr = dynamic_cast<method_class*>(cl->features->nth(i));
		if (meth_ptr && meth_ptr->name == method)
		{
			return meth_ptr->return_type;
		}
	}
	return Object;
}

void ClassTable::check_cond(cond_class* expr, class__class* class_ptr)
{
	checkExpression(expr->pred, class_ptr);
	checkExpression(expr->then_exp, class_ptr);	
	checkExpression(expr->else_exp, class_ptr);	

	Symbol T1 = getTypeOfExpression(expr->pred, class_ptr);
	if (T1 != Bool)
		semant_error(class_ptr) << endl;
}

void ClassTable::check_loop(loop_class* expr, class__class* class_ptr)
{
	checkExpression(expr->pred, class_ptr);
	checkExpression(expr->body, class_ptr);		

	Symbol T1 = getTypeOfExpression(expr->pred, class_ptr);
	if (T1 != Bool)
		semant_error(class_ptr) << endl;
}

void ClassTable::check_typcase(typcase_class* expr, class__class* class_ptr)
{
  checkExpression(expr->expr, class_ptr);

  for(int i = expr->cases->first(); expr->cases->more(i); i = expr->cases->next(i))
  {
	branch_class* branch_ptr = (branch_class*)expr->cases->nth(i);
	check_branch(branch_ptr, class_ptr);
  }
}

void ClassTable::check_block(block_class* expr, class__class* class_ptr)
{
  for(int i = expr->body->first(); expr->body->more(i); i = expr->body->next(i))
  {
	checkExpression(expr->body->nth(i), class_ptr);
  }
}

void ClassTable::check_let(let_class* expr, class__class* class_ptr)
{
	checkExpression(expr->init, class_ptr);
	if (!types.lookup(expr->type_decl))
		semant_error(class_ptr) << endl;
	
	if (!isExpressionNoOp(expr->init) && !isAsubtypeofB(getTypeOfExpression(expr->init, class_ptr), expr->type_decl))
		semant_error(class_ptr) << endl;
	
	vars.enterscope();

	vars.addid(expr->identifier, &expr->type_decl);
	checkExpression(expr->body, class_ptr);

	vars.exitscope();
}

void ClassTable::check_neg(neg_class* expr, class__class* class_ptr)
{
	checkExpression(expr->e1, class_ptr);

	Symbol T1 = getTypeOfExpression(expr->e1, class_ptr);
	if (T1 != Int)
		semant_error(class_ptr) << endl;
}

void ClassTable::check_eq(eq_class* expr, class__class* class_ptr)
{
	checkExpression(expr->e1, class_ptr);
	checkExpression(expr->e2, class_ptr);

	Symbol T1 = getTypeOfExpression(expr->e1, class_ptr);
	Symbol T2 = getTypeOfExpression(expr->e2, class_ptr);
	if (T1 == Int && T2 != Int)
		semant_error(class_ptr) << endl;

	if (T1 == Bool && T2 != Bool)
		semant_error(class_ptr) << endl;

	if (T1 == Str && T2 != Str)
		semant_error(class_ptr) << endl;
}

void ClassTable::check_comp(comp_class* expr, class__class* class_ptr)
{
	checkExpression(expr->e1, class_ptr);

	Symbol T1 = getTypeOfExpression(expr->e1, class_ptr);
	if (T1 != Bool)
		semant_error(class_ptr) << endl;
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
	if (!types.lookup(expr->type_name))
		semant_error(class_ptr) << endl;
}

void ClassTable::check_isvoid(isvoid_class* expr, class__class* class_ptr)
{
	checkExpression(expr->e1, class_ptr);
}

void ClassTable::check_no_expr(no_expr_class* expr, class__class* class_ptr)
{
}

void ClassTable::check_object(object_class* expr, class__class* class_ptr)
{
	if (!vars.lookup(expr->name))
		semant_error(class_ptr) << endl;
}

bool ClassTable::isAsubtypeofB(Symbol a, Symbol b)
{
	if (a == b)
		return true;

	// Object is not a subType of any other class
	if (a == Object)
		return false;

	Class_ A = *types.lookup(a);
	Class_ B = *types.lookup(b);

	if (!A || !B)
	{
		cerr << "isAsubtypeofB - symbols were not found." << endl;
		return false;
	}

	class__class* a_ptr = dynamic_cast<class__class*>(A);
	class__class* b_ptr = dynamic_cast<class__class*>(B);

	if (!a_ptr || !b_ptr)
	{
		cerr << "isAsubtypeofB - pointers were not casted." << endl;
		return false;
	}

	while (a_ptr->parent != b_ptr->name && a_ptr->parent != Object)
	{
		A = *types.lookup(a_ptr->parent);
		if (!A)
		{
			cerr << "isAsubtypeofB - parent were not found." << endl;
			return false;
		}

		a_ptr = dynamic_cast<class__class*>(A);
		if (!a_ptr)
		{
			cerr << "isAsubtypeofB - parent was not casted." << endl;
			return false;
		}
	}
	
	if (a_ptr->parent == b_ptr->name)
		return true;

	return false;
}

void ClassTable::test_isAsubtypeofB()
{
  Symbol Base = idtable.add_string("Base");
  Symbol Der1 = idtable.add_string("Der1");
  Symbol Der2 = idtable.add_string("Der2");
  Symbol Der3 = idtable.add_string("Der3");
  Symbol A = idtable.add_string("A");

  cerr << "isAsubtypeofB(IO, Object) - " << isAsubtypeofB(IO, Object) << endl;
  cerr << "isAsubtypeofB(Object, IO) - " << isAsubtypeofB(Object, IO) << endl;
  cerr << "isAsubtypeofB(IO, IO) - " << isAsubtypeofB(IO, IO) << endl;
  cerr << "isAsubtypeofB(Object, Object) - " << isAsubtypeofB(Object, Object) << endl;

  cerr << "isAsubtypeofB(Base, Object) - " << isAsubtypeofB(Base, Object) << endl;
  cerr << "isAsubtypeofB(Der1, Base) - " << isAsubtypeofB(Der1, Base) << endl;
  cerr << "isAsubtypeofB(Base, Der1) - " << isAsubtypeofB(Base, Der1) << endl;

  cerr << "isAsubtypeofB(Der2, Base) - " << isAsubtypeofB(Der2, Base) << endl;
  cerr << "isAsubtypeofB(Der2, Object) - " << isAsubtypeofB(Der2, Object) << endl;

  cerr << "isAsubtypeofB(Der2, Der3) - " << isAsubtypeofB(Der2, Der3) << endl;
}

Symbol ClassTable::findCommonAncestor(Symbol a, Symbol b)
{
	Class_ A = *types.lookup(a);
	Class_ B = *types.lookup(b);

	if (!A || !B)
	{
		cerr << "findCommonAncestor - symbols were not found." << endl;
		return Object;
	}

	class__class* a_ptr = dynamic_cast<class__class*>(A);
	class__class* b_ptr = dynamic_cast<class__class*>(B);

	if (!a_ptr || !b_ptr)
	{
		cerr << "findCommonAncestor - pointers were not casted." << endl;
		return Object;
	}

	std::set<Symbol> parentsOfA;

	while (a_ptr->name != Object)
	{
		parentsOfA.insert(a_ptr->name);
		A = *types.lookup(a_ptr->parent);
		if (!A)
		{
			cerr << "findCommonAncestor - parent were not found." << endl;
			return Object;
		}

		a_ptr = dynamic_cast<class__class*>(A);
		if (!a_ptr)
		{
			cerr << "findCommonAncestor - parent was not casted." << endl;
			return Object;
		}
	}

	while (b_ptr->name != Object)
	{
		if (parentsOfA.find(b_ptr->name) != parentsOfA.end())
			return b_ptr->name;
		B = *types.lookup(b_ptr->parent);
		if (!B)
		{
			cerr << "findCommonAncestor - parent were not found." << endl;
			return Object;
		}

		b_ptr = dynamic_cast<class__class*>(B);
		if (!b_ptr)
		{
			cerr << "findCommonAncestor - parent was not casted." << endl;
			return Object;
		}
	}

	return Object;
}

void ClassTable::test_findCommonAncestor()
{
  Symbol Base = idtable.add_string("Base");
  Symbol Der1 = idtable.add_string("Der1");
  Symbol Der2 = idtable.add_string("Der2");
  Symbol Der3 = idtable.add_string("Der3");
  Symbol A = idtable.add_string("A");

  cerr << "findCommonAncestor(IO, Object) - " << findCommonAncestor(IO, Object) << endl;
  cerr << "findCommonAncestor(Object, IO) - " << findCommonAncestor(Object, IO) << endl;
  cerr << "findCommonAncestor(IO, IO) - " << findCommonAncestor(IO, IO) << endl;
  cerr << "findCommonAncestor(Object, Object) - " << findCommonAncestor(Object, Object) << endl;

  cerr << "findCommonAncestor(Base, Object) - " << findCommonAncestor(Base, Object) << endl;
  cerr << "findCommonAncestor(Der1, Base) - " << findCommonAncestor(Der1, Base) << endl;
  cerr << "findCommonAncestor(Base, Der1) - " << findCommonAncestor(Base, Der1) << endl;
  cerr << "findCommonAncestor(Der2, Der1) - " << findCommonAncestor(Der2, Der1) << endl;
  cerr << "findCommonAncestor(Der1, Der2) - " << findCommonAncestor(Der1, Der2) << endl;

  cerr << "findCommonAncestor(Der2, Base) - " << findCommonAncestor(Der2, Base) << endl;
  cerr << "findCommonAncestor(Der2, Object) - " << findCommonAncestor(Der2, Object) << endl;

  cerr << "findCommonAncestor(Der2, Der3) - " << findCommonAncestor(Der2, Der3) << endl;
}

Symbol ClassTable::getTypeOfExpression(Expression expr_ptr, class__class* class_ptr)
{
	{
		assign_class* expr = dynamic_cast<assign_class*>(expr_ptr);
		if (expr)
			return getTypeOfExpression(expr->expr, class_ptr);
	}
	{
		static_dispatch_class* expr = dynamic_cast<static_dispatch_class*>(expr_ptr);
		if (expr)
		{
			Class_ T = *types.lookup(expr->type_name);
			return getMethodReturnType(dynamic_cast<class__class*>(T), expr->name);
		}
	}
	{
		dispatch_class* expr = dynamic_cast<dispatch_class*>(expr_ptr);
		if (expr)
		{
			Class_ T = *types.lookup(getTypeOfExpression(expr->expr, class_ptr));
			return getMethodReturnType(dynamic_cast<class__class*>(T), expr->name);
		}
	}
	{
		cond_class* expr = dynamic_cast<cond_class*>(expr_ptr);
		if (expr)
			return findCommonAncestor(getTypeOfExpression(expr->then_exp, class_ptr), getTypeOfExpression(expr->else_exp, class_ptr));
	}
	{
		loop_class* expr = dynamic_cast<loop_class*>(expr_ptr);
		if (expr)
			return Object;
	}
/*
	{
		typcase_class* expr = dynamic_cast<typcase_class*>(expr_ptr);
		if (expr)
			check_typcase(expr, class_ptr);
	}
*/
	{
		block_class* expr = dynamic_cast<block_class*>(expr_ptr);
		if (expr)
			return getTypeOfExpression(expr->body->nth(expr->body->len() - 1), class_ptr);
	}
	{
		let_class* expr = dynamic_cast<let_class*>(expr_ptr);
		if (expr)
			return getTypeOfExpression(expr->body, class_ptr);
	}
	{
		plus_class* expr = dynamic_cast<plus_class*>(expr_ptr);
		if (expr)
			return Int;
	}
	{
		sub_class* expr = dynamic_cast<sub_class*>(expr_ptr);
		if (expr)
			return Int;
	}
	{
		mul_class* expr = dynamic_cast<mul_class*>(expr_ptr);
		if (expr)
			return Int;
	}
	{
		divide_class* expr = dynamic_cast<divide_class*>(expr_ptr);
		if (expr)
			return Int;
	}
	{
		neg_class* expr = dynamic_cast<neg_class*>(expr_ptr);
		if (expr)
			return Int;
	}
	{
		lt_class* expr = dynamic_cast<lt_class*>(expr_ptr);
		if (expr)
			return Bool;
	}
	{
		eq_class* expr = dynamic_cast<eq_class*>(expr_ptr);
		if (expr)
			return Bool;
	}
	{
		leq_class* expr = dynamic_cast<leq_class*>(expr_ptr);
		if (expr)
			return Bool;
	}
	{
		comp_class* expr = dynamic_cast<comp_class*>(expr_ptr);
		if (expr)
			return Bool;
	}
	{
		int_const_class* expr = dynamic_cast<int_const_class*>(expr_ptr);
		if (expr)
			return Int;
	}
	{
		bool_const_class* expr = dynamic_cast<bool_const_class*>(expr_ptr);
		if (expr)
			return Bool;
	}
	{
		string_const_class* expr = dynamic_cast<string_const_class*>(expr_ptr);
		if (expr)
			return Str;
	}
	{
		new__class* expr = dynamic_cast<new__class*>(expr_ptr);
		if (expr)
			return expr->type_name;
	}
	{
		isvoid_class* expr = dynamic_cast<isvoid_class*>(expr_ptr);
		if (expr)
			return Bool;
	}
	{
		object_class* expr = dynamic_cast<object_class*>(expr_ptr);
		if (expr)
		{
			Symbol* T = vars.lookup(expr->name);
			if (T)
				return *T;
			else
				return Object;
		}
	}
	return Object;
}

bool ClassTable::isExpressionNoOp(Expression expr_ptr)
{
	return dynamic_cast<no_expr_class*>(expr_ptr);
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

    types.addid(Object, new Class_(Object_class));
    types.addid(IO, new Class_(IO_class));
    types.addid(Int, new Class_(Int_class));
    types.addid(Bool, new Class_(Bool_class));
    types.addid(Str, new Class_(Str_class));
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


