#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable 
{
  void install_basic_classes();
  void install_user_classes(Classes classes);
  void checkClass(class__class* class_ptr);
  void checkClassFeatures(Features features, class__class* class_ptr);
  void checkAttribute(attr_class* attr_ptr, class__class* class_ptr);
  void checkMethod(method_class* meth_ptr, class__class* class_ptr);
  void checkFormal(formal_class* formal_ptr, class__class* class_ptr);
  void checkExpression(Expression expr_ptr, class__class* class_ptr);

  void check_assign(assign_class* expr, class__class* class_ptr);
  void check_static_dispatch(static_dispatch_class* expr, class__class* class_ptr);
  void check_dispatch(dispatch_class* expr, class__class* class_ptr);
  void check_cond(cond_class* expr, class__class* class_ptr);
  void check_loop(loop_class* expr, class__class* class_ptr);
  void check_typcase(typcase_class* expr, class__class* class_ptr);
  void check_block(block_class* expr, class__class* class_ptr);
  void check_let(let_class* expr, class__class* class_ptr);
  void check_plus(plus_class* expr, class__class* class_ptr);
  void check_sub(sub_class* expr, class__class* class_ptr);
  void check_mul(mul_class* expr, class__class* class_ptrn);
  void check_divide(divide_class* expr, class__class* class_ptr);
  void check_neg(neg_class* expr, class__class* class_ptr);
  void check_lt(lt_class* expr, class__class* class_ptr);
  void check_eq(eq_class* expr, class__class* class_ptr);
  void check_leq(leq_class* expr, class__class* class_ptr);
  void check_comp(comp_class* expr, class__class* class_ptr);
  void check_int_const(int_const_class* expr, class__class* class_ptr);
  void check_bool_const(bool_const_class* expr, class__class* class_ptr);
  void check_string_const(string_const_class* expr, class__class* class_ptr);
  void check_new_(new__class* expr, class__class* class_ptr);
  void check_isvoid(isvoid_class* expr, class__class* class_ptr);
  void check_no_expr(no_expr_class* expr, class__class* class_ptr);
  void check_object(object_class* expr, class__class* class_ptr);

  void check_branch(branch_class* expr, class__class* class_ptr);

  bool isAsubtypeofB(Symbol a, Symbol b);
  void test_isAsubtypeofB();

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

private:
  int semant_errors;
  ostream& error_stream;
  SymbolTable<Symbol, Class_> symTab;
};


#endif

