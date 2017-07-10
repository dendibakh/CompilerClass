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
  bool checkClass(class__class* class_ptr);

  void checkClassFeatures(Features features, class__class* class_ptr);
  void collectClassAttributes(Features features);
  void checkAttribute(attr_class* attr_ptr, class__class* class_ptr);
  void checkMethod(method_class* meth_ptr, class__class* class_ptr);

  void checkFormal(formal_class* formal_ptr, class__class* class_ptr);
  void collectFormals(method_class* meth_ptr);
  void checkDupFormals(method_class* meth_ptr, class__class* class_ptr);

  void checkExpression(Expression expr_ptr, class__class* class_ptr);

  void check_assign(assign_class* expr, class__class* class_ptr);
  void check_static_dispatch(static_dispatch_class* expr, class__class* class_ptr);
  void check_dispatch(dispatch_class* expr, class__class* class_ptr);
  void check_cond(cond_class* expr, class__class* class_ptr);
  void check_loop(loop_class* expr, class__class* class_ptr);
  void check_typcase(typcase_class* expr, class__class* class_ptr);
  void check_block(block_class* expr, class__class* class_ptr);
  void check_let(let_class* expr, class__class* class_ptr);
  void check_neg(neg_class* expr, class__class* class_ptr);
  void check_eq(eq_class* expr, class__class* class_ptr);
  void check_comp(comp_class* expr, class__class* class_ptr);

  void check_new_(new__class* expr, class__class* class_ptr);
  void check_isvoid(isvoid_class* expr, class__class* class_ptr);
  void check_object(object_class* expr, class__class* class_ptr);

  void check_branch(branch_class* expr, class__class* class_ptr);

  bool isAsubtypeofB(Symbol a, Symbol b);
  void test_isAsubtypeofB();

  Symbol findCommonAncestor(Symbol a, Symbol b);
  void test_findCommonAncestor();

  Symbol getTypeOfExpression(Expression expr, class__class* class_ptr);

  void checkAttrIsNotDefinedInParents(Symbol attr, class__class* class_ptr);

  bool checkMethodExist(class__class* cl, Symbol method);
  void checkMethodFormals(class__class* cl, Symbol method, Expressions exprs, class__class* class_ptr);
  void checkMethodOverride(class__class* class_ptr, method_class* meth_ptr);
  bool compareOverridenSignatures(method_class* meth_ptr1, method_class* meth_ptr2);
  Symbol getMethodReturnType(class__class* cl, Symbol method);

  bool isExpressionNoOp(Expression expr_ptr);

  template <class T>
  void checkArithmeticOrComparisonExpression(T* expr, class__class* class_ptr);

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

private:
  int semant_errors;
  ostream& error_stream;
  SymbolTable<Symbol, Class_> types;
  SymbolTable<Symbol, Symbol> vars;
};


#endif

