#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <map>
#include <vector>

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   
   int classTagInc;
   std::map<Symbol, int> classTags;

   std::vector<attr_class*> attrsToInit;

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);

   void assignClassTags();
   void emitProtos();
   void emitOneProtObj(CgenNodeP cl);
   int calculateClassSize(CgenNodeP cl);
   int calculateAttrSize(CgenNodeP cl);

   void emitClassNameTab();
   void emitDispTab();
   void emitDispTabWithParents(CgenNodeP cl, std::vector<Symbol>& classDispTab);
   void generateClassDispTab(CgenNodeP cl, std::vector<Symbol>& classDispTab);

   void emitClassObjTab();
   void generateInitMethods();
   void generateInitMethodForClass(Symbol cl);

   void generateClassMethods();
   void generateCodeForClassMethod(method_class* meth_ptr);

   void storeAttrOffsets();
   void storeAttrOffsetsWithParents(CgenNodeP cl, std::vector<Symbol>& vect);

public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

