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
  void checkClassFeatures(Features features);
  void checkAttribute(attr_class* attr_ptr);
  void checkMethod(method_class* meth_ptr);
  void checkFormal(formal_class* formal_ptr);

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

