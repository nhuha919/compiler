%skeleton "lalr1.cc"
%define parser_class_name {Parser}
%define api.token.constructor
%define api.value.type variant
%define parse.assert
%define parse.error verbose
%locations

%code requires
{
#include <map>
#include <list>
#include <vector>
#include <string>
#include <iostream>
#include <algorithm>

#define ENUM_IDENTIFIERS(o) \
    o(undefined) \
    o(function) \
    o(parameter) \
    o(variable) 
#define o(n) n,
enum class type { ENUM_IDENTIFIERS(o) };
#undef o

struct identifier 
{
    id_type type = id_type:: undefined;
    std::size_t index = 0;
    std::string name;
};

#define ENUM_EXPRESSIONS(o) \
    o(nop) o(string) o(number) o(ident) \
    o(add) o(neg) o(eq) \
    o(cor) o(cand) o(loop) \
    o(addrof) o(deref) \
    o(fcall) \
    o(copy) \ 
    o(comma) \
    o(ret)

#define o(n) n,
enum class expr-type { ENUM_EXPRESSIONS(o) };
#undef o

typedef std::list<struct expression> expr_vec;
struct expression
{
    ex_type type;
    identifier ident{};
    std::string strvalue{};
    long numvalue=0;
    expr_vec params;

    template<typename... T>
    expression(ex_type t, T&&... args): type(t), params{ std::forward<T>(args)... } {}

    expression(): type(ex_type::nop) {}
    expression(const identifier&& i): type(ex_type::ident), ident(i) {}
    expression(std:string&& s): type(ex_type::string), strvalue(std::move(s)) {}
    expression(long v): type(ex_type::number), numvalue(v) {}

    bool is_pure() const;

    expression operator%=(expression&& b) && { return expresion(ex_type::copy, std::move(b), std::move(*this)); }
};

#define o(n) \
template<typename... T> \
inline expression e_##n(T&&... args) { return expression(ex_type::n, std::forward<T>(args)...); }
ENUM_EXPRESSIONS(o)
#undef o

struct function
{
    std::string name;
    expression code;
    unsigned num_vars = 0, num_params = 0;
};

struct lexcontext;
}//%code requires

%code
{
struct lexcontext
{
    const char* cursor;
    yy::location loc;
    std::vector<std::map<std::string, identifier>> scopes;
    unsigned tempcounter = 0;
    function fun;
public:
    const identifier& define(const std::string& name, identifier&& f)
    {
        auto r = scopes.back().emplace(name, std::move(f));
        if(!r.second) throw yy::conj_parser::syntax_error(loc, "Duplicate definition <"+name+">");
        return r.first->second;
    }
    expression def(const std::string& name)     
    { 
        return define(name, identifier{id_type::variable, fun.num_vars++, name}); 
    }
    expression defun(const std::string& name)   
    { 
        return define(name, identifier{id_type::function, func_list.size(), name}); 
    }
    expression defparm(const std::string& name) 
    {
        return define(name, identifier{id_type::parameter, fun.num_params++, name}); 
    }
    expression temp()                           
    {
        return def("$I" + std::to_string(tempcounter++)); 
    }
    expression use(const std::string& name)
    {
        for(auto j = scopes.crbegin(); j != scopes.crend(); ++j)
            if(auto i = j->find(name); i != j->end())
                return i->second;
        throw yy::conj_parser::syntax_error(loc, "Undefined identifier <"+name+">");
    }
    void add_function(std::string&& name, expression&& code)
    {
        fun.code = e_comma(std::move(code), e_ret(0l)); // Add implicit "return 0;" at the end
        fun.name = std::move(name);
        func_list.push_back(std::move(fun));
        fun = {};
    }
    void operator ++() { scopes.emplace_back(); } // Enter scope
    void operator --() { scopes.pop_back();     } // Exit scope
};

namespace yy { parser::symbol_type yylex(lexcontext& ctx); }

#define M(x) std::move(x)
#define C(x) expression(x)

} //%code

%token END 0
%token RETURN "return" WHILE "while" IF "if" VAR "var" IDENTIFIER NUMCONST STRINGCONST
%token OR "||" AND "&&" EQ "==" NE "!=" PP "++" MM "--" PL_EQ "+=" MI_EQ "-="

%left ','
%right '?' ':' '=' "+=" "-="
%left "||"
%left "&&"
%left "==" "!="
%left '+' '-'
%left '*'
%right '&' "++" "--"
%left '(' '{'
%% 

library: functions;
functions: 
    IDENTIFIER parandecls ';' stmt
|   
;
parandecls:
    parandecl 
|   
;
parandecl:
    parandecl ',' IDENTIFIER
|   IDENTIFIER;
stmt: 
    '{' com_stmt '}'
|   "if" '(' exprs ')' stmt
|   "while" '(' exprs ')' stmt
|   "return" exprs ';'
|   exprs ";"
|   ';';
com_stmt:
    
|   com_stmt stmt;
var_defs:
    "var" var_def1
|   var_defs ',' var_def1;
var_def1:
    IDENTIFIER '=' expr
|   IDENTIFIER;
exprs:
    var_defs
|   expr
|   expr ',' c_expr1;
c_expr1:
    expr
|   c_expr1 ',' expr;
expr:
    NUMCONST
|   STRINGCONST
|   IDENTIFIER
|   '(' exprs ')'
|   expr '[' exprs ']'
|   expr '(' ')'
|   expr '(' c_expr1 ')'
|   expr '=' expr
|   expr '+' expr
|   expr '-' expr   %prec '+'
|   expr "+=" expr
|   expr "-=" expr
|   expr "||" expr
|   expr "&&" expr
|   expr "==" expr
|   expr "!=" expr
|   '&' expr
|   '*' expr    %prec '&'
|   '-' expr    %prec '&'
|   '!' expr    %prec '&'
|   "++" expr
|   "--" expr   %prec "++"
|   expr "++"
|   expr "--"   %prec "++"
|   expr "?" expr ":" expr;