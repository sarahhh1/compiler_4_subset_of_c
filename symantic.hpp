#ifndef COMPI_HW3_NEW_SYMANTIC_H
#define COMPI_HW3_NEW_SYMANTIC_H
#include <string>
#include <cctype>
#include <cstring>
#include <iostream>
#include <vector>
#include <sstream>
#include <memory>
#include <iostream>
#include "bp.hpp"
// TODO: inclide the reg_pool_.hpp
#include <vector>
#include <string>
#include <cctype>
#include "hw3_output.hpp"

class Exp;
class Scope;
class Type;
class FormalsDecl;
class Call;
class RetType;
class Formals;
class OverRide;
class IsBool;
class Prev;
class M;
class N;
class Statement;
extern int yylineno;
class FormalsList;
void StrartProg();
class Sc;
class Node;
void endProg(int yy_eof, int yy_char);
Scope* FindID(const string& name);
void addTable();
void FUNC_ZERO();
void removeTable();
Statement* InParserCHANGEinWhile(bool flag, N* n = nullptr, Prev* prev = nullptr, Statement* statement = nullptr);
void check_bool(Node *node);
void up_f(Node* N, Formals* F);
string get_llvm_type(string type);
using namespace std;
void  backpatching_for_if(M* my_label, IsBool *isBool);
string get_var();
void decl_func();
Node* createPrev(Exp* E);
void exitFunc(RetType* ret);
//CodeBuffer &buffer2 = CodeBuffer::instance();

class Scope
{
public:
    std::string Val;
    vector<std::string > Type;
    int Offset;
    bool is_func;
    bool is_ovewride;
    int func_num_if_ow;

    Scope(std::string Val,
          vector<string> Type,
          int Offset, bool is_func = false, bool is_ovevride = false, int func_num_if_ow=0){
        this->Val = Val;
        this->Offset = Offset;
        this->Type = Type;
        this->is_func = is_func;
        this->is_ovewride = is_ovevride;
        this->func_num_if_ow=func_num_if_ow;
    }
    ~Scope() = default;
};

class SymbolTable
{
public:
    std::vector<Scope*> table;
    SymbolTable() = default;
    ~SymbolTable() = default;
};





class Node
{
public:
    string _id;
    string _reg;
    string _label;

    Node(string value,string reg="",string lab=""): _id(value),_reg(reg),_label(lab) {}
    Node() = default;
    Node(Node & toCopy)
    {
        this->_id = toCopy._id;
    }

    virtual~Node() = default;
};

#define YYSTYPE Node *
class Program: public Node
{
public:
    Program();
    virtual ~Program() = default;
};

class Funcs: public Node
{
    //  Funcs();
};

class FuncDecl: public Node {
public:
    string my_type;

    FuncDecl(OverRide *overRide, RetType *retType, Node *id, Formals *formal);

    virtual ~FuncDecl() = default;
};

class OverRide: public Node {
public:
    bool is_override;
    OverRide(bool flag) : is_override(flag) {

    };
    virtual ~OverRide() = default;
};

class RetType: public Node
{
public:
    string my_type;
    RetType(string Ttype): Node(), my_type(Ttype){}
    virtual ~RetType() = default;
};

class Formals: public Node
{
public:
    vector<FormalsDecl*> vec;

    Formals() = default;
    Formals(FormalsList* f_l);
    virtual ~Formals() = default;
};

class FormalsList: public Node
{
public:
    vector<FormalsDecl*> vec;

    FormalsList(FormalsDecl *formalsDecl);
    FormalsList(FormalsDecl *f_d, FormalsList *f_l);
    virtual ~FormalsList() = default;

};

class FormalsDecl: public Node
{
public:
    string _type;
    FormalsDecl(Type* t, Node* id);
    virtual ~FormalsDecl() = default;

};

class Type: public Node
{
public:
    string _type;
    Type(string type): Node(), _type(type) {}
    virtual ~Type() = default;
};

class Statements: public Node
{
public:
    vector<pair<int, BranchLabelIndex>> c_list;
    vector<pair<int, BranchLabelIndex>> b_list;

    Statements(Statement* s);
    Statements(Statements* ss,Statement* s);
    virtual ~Statements() = default;
};

class Statement: public Node
{
public:
    string my_type;
    vector<pair<int, BranchLabelIndex>> c_list;
    vector<pair<int, BranchLabelIndex>> b_list;

    Statement(string Mtype);
    Statement(Node *node);
    Statement(Exp* exp);
    Statement(Statements* statements);
    Statement(Statement* statement);
   // Statement(Exp* exp, bool flag);
    Statement(Type *type, Node *node, Exp *exp);
    Statement(Node *node, Exp *exp);
    Statement(Type *type, Node *node);
    //Statement(string iff, Exp* exp);
    Statement(Call *c);
    virtual ~Statement() = default;
};

class ExpList: public Node {
public:
    vector<Exp*> vec;
    ExpList(Exp* exp);
    ExpList(Exp* exp, ExpList* exp_list);
    virtual ~ExpList() = default;
};

class Call: public Node {
public:
    string my_type;
    Call (Node* id);
    Call (Node* id, ExpList* exp_list);
    virtual ~Call() = default;
};

class Exp: public Node
{
public:
    //string _reg;
    string _type;
    vector<pair<int, BranchLabelIndex>> true_list;
    vector<pair<int, BranchLabelIndex>> false_list;

    Exp(string Mtype, string bool_val);
    Exp(Exp *my_exp1,Exp *my_exp2);
    Exp(Exp *my_exp);
    Exp(Exp *my_exp, bool x);
    Exp(string op, Exp *A, Exp *Beta, Node* op_sign, Prev* prev= nullptr);
    Exp(Node *id);
    Exp(string Mtype, Node *N);	//NUM
    Exp(Type *type, Exp *E);
    Exp(Call* call);
    virtual ~Exp() = default;
};


class N : public Node
{
public:
    N();
    ///LOCATION
    int to_backpatch;
};

class M : public Node
{
public:
    string next_quad;
    M();
};

class IsBool : public Node {
public:
Exp* is_bool_exp;
    IsBool(Exp *exp, bool flag);

};

class Prev : public Node
{
public:
    string _lable;
    int _location;
    Prev(Exp *left);
};

bool check_if_doubl(string name,Formals* formal,RetType* retType);
void is_it_bool(Exp *e);
void is_legale_byte(Node *z_number);
string relop_helper( Exp *A, Exp *Beta, Node* op_sign, string my_reg);
//void bool_helper( Exp *A, Exp *Beta, Node* op_sign, string my_reg);
string binop_helper( Exp *A, Exp *Beta, Node* op_sign, string my_reg);
void InParserCHANGEinIF(bool flag);
Statement* backpatching_for_if_else(M* my_label, N* my_label2, IsBool* isBool, Statement* if_s, Statement* else_s);
std::vector<std::string> convertToUppercase(std::vector<std::string> input);
void check_if_doubl_param(std::string name);
string get_var();
#endif	//COMPI_HW3_NEW_SYMANTIC_H