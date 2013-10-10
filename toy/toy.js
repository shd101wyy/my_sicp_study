// minimal toy language


// construct number data type
var INT = 1
var FLOAT = 2
// check whether value is Int
// check String is Number
function isInt(value)
{
    var er = /^[0-9]+$/;
    return ( er.test(value) ) ? true : false;
}
function isNumber(n) {
  return !isNaN(parseFloat(n)) && isFinite(n);
}

function numberType(value){
    if(!isNumber(value)) return 0;
    if(isInt(value)) return INT
    return FLOAT
}

var Tokenize_String = function(input_str){
    var output = []
    for(var i = 0; i < input_str.length; i++){
        /*
            Ignore space tab newline
        */
        if (input_str[i]==' '||input_str[i]=='\t'||input_str[i]=='\n'){
            continue
        }
        // meet array
        else if (input_str[i] == '['){
            output.push('(')
            output.push('vector')
        }
        else if (input_str[i] == '{'){
            output.push('(')
            output.push('dictionary')
        }
        else if (input_str[i] == '}' || input_str[i] == ']'){
            output.push(')')
        }
        /*
            special token
        */
        else if (input_str[i]=='('||input_str[i]==')'||
            input_str[i]=='['||input_str[i]==']'||
            input_str[i]=='{'||input_str[i]=='}'||
            input_str[i]=='@'||input_str[i]=="'"||input_str[i]==','){ //||
            output.push(input_str[i])
        }
        /*
            Comment:
                ;
        */
        else if (input_str[i]==";"){ // comment
            while( i!=input_str.length && input_str[i]!='\n'){i++}
            continue
        }
        /*
            String
        */
        else if (input_str[i]=='"'){ // string
            var start = i
            i = i + 1
            while(input_str[i]!='"' && i!=input_str.length){
                if(input_str[i]=='\\')
                    i = i + 1
                i = i + 1
            }
            output.push('(')
            output.push('quote')
            output.push(input_str.slice(start+1, i))
            output.push(')')
        }
        else { // atom or number
            var start = i
            while (i!=input_str.length && input_str[i]!=' ' 
                && input_str[i]!='(' && input_str[i]!=')' 
                && input_str[i]!='[' && input_str[i]!=']' 
                && input_str[i]!='{' && input_str[i]!='}' 
                && input_str[i]!='\n' && input_str[i]!='\t'
                 && input_str[i]!=';'){
                    i = i + 1
                }
            output.push(input_str.slice(start, i))
            i = i - 1
        }
    }
    return output
}

// parse string and generate linked list
var ParseString = function(token_list){
    var rest;

    // 12 -> Number(12,1,INT)
    // if its type is not Number
    // return itself
    var formatSymbol = function(input_str){
        var type = numberType(input_str)
        if (type!=0){
            var append_obj
            if(type == INT){
                append_obj = new Number(parseInt(input_str), INT)
            }
            else{
                append_obj = new Number(parseFloat(input_str), FLOAT)
            }
            return append_obj
        }
        // check :
        if(input_str[0] == ":" && input_str.length>1){
            return cons('quote',cons(input_str, []))
        }
        return input_str
    }

    var parseList = function(token_list){
        if(token_list[0]==')'){ // finish
            rest = token_list.slice(1)
            return []
        }
        else if (token_list[0]=='(')
            return cons(parseList(token_list.slice(1)), parseList(rest))
        else if (token_list[0]=='[')
            return cons(parseVector(token_list.slice(1)), parseList(rest))
        else if (token_list[0]=='{')
            return cons(parseDictionary(token_list.slice(1)), parseList(rest))
        else if (token_list[0]=='@'||token_list[0]=="'"||token_list[0]==',')
            return cons(parseSpecial(token_list.slice(1), token_list[0]), parseList(rest))
        else 
            return cons(formatSymbol(token_list[0]), parseList(token_list.slice(1)))
    }
    // parse @ ' ,
    var parseSpecial = function(token_list, sign){
        var flag 
        if (sign == '@')
            flag = 'quasiquote'
        else if (sign == "'")
            flag = 'quote'
        else 
            flag = 'unquote'
        if (token_list[0]=='(')
            return cons(flag, cons(parseList(token_list.slice(1))))
        // else if (token_list[0]=='[')
        //   return cons(flag, cons(parseVector(token_list.slice(1))))
        // else if (token_list[0]=='{')
        //    return cons(flag, cons(parseDictionary(token_list.slice(1))))
        else{
            rest = token_list.slice(1)
            return cons(flag, cons(formatSymbol(token_list[0]), []))
        }
    }
    var ParseString_iter = function(token_list){
        // finish
        if(token_list.length == 0)
            return []
        // list
        if(token_list[0]=='('){
            return cons(parseList(token_list.slice(1)), ParseString_iter(rest))
        }
        // quasiquote quote unquote
        else if (token_list[0]=='@'||token_list[0]=="'"||token_list[0]==','){
            return cons(parseSpecial(token_list.slice(1), token_list[0]), ParseString_iter(rest))
        }
        // atom
        else{
            return cons(formatSymbol(token_list[0]), ParseString_iter(token_list.slice(1)))
        }
    }
    return ParseString_iter(token_list)
}
// construct number
var Number = function(value, type){
    this.value = value
    this.type = type // INT or FLOAT
}
// construct vector
var Vector = function(value){
    this.value = value
}

// primitive 
// construct List
var cons = function(x,y){
    return [x,y]
}
var car = function(x){ return x[0] }
var cdr = function(x){ return x[1] }
var nil = false

var or_ = function(x,y){
    return 
}
// check whether x is number
var number$ = function(x){
    return (x.constructor === Number)? true:nil
}

// check whether x is atom
var atom$ = function(x){ 
    return (typeof(x) === 'string') ? true:nil
}
var string$ = atom$
// check whether x is vector
var vector$ = function(x){
    return (x.constructor === Vector) ? true:nil
}
// check whether x is pair
var pair$ = function(x){
    return (vector$(x) || atom$(x) || number$(x)) ? nil:true
}
// check whether x is symbol
var symbol$ = function(x){
    return (atom$(x) && !isNumber(x)) ? true:nil
}
// check list is null?
var null$ = function(x){
    if(pair$(x))
        return (x.length==0)? true:nil
    return nil
}
var display = function(x){
    console.log(x)
}
var eq$ = function(arg1, arg2){
    if (typeof(arg1) == 'string' && typeof(arg2) == 'string')  // atom
        return (arg1 == arg2) ? true:nil
    else if (arg1.constructor == Number && arg2.construction == Number)
        return (arg1.value == arg2.value) ? true:nil
    else if (arg1.constructor == Vector || arg2.constructor == Vector)
        console.log("Invalid Argument -- eq?" + arg1 + " " + arg2)
    else if (arg1.length == 0 && arg2.length == 0)
        return true
    return (arg1==arg2) ? true:nil
}
// =====
var eval_assignment = function(exp, env){
    set_variable_value( 
        assignment_variable(exp),
        eval(
            assignment_value(exp), env),
        env)
    return 'ok'
}
var eval_definition = function(exp, env){
    define_variable(
        definition_variable(exp),
        eval(
            definition_value(exp),
            env),
        env)
    return 'ok'
}
// self evaluating?
var self_evaluation$ = function(exp){
    if (number$(exp))
        return exp
    else if (exp.constructor === Vector)
        return exp
    // else if (string$(exp))
    //    return exp
    return false
}
var variable$ = symbol$
var quoted$ = function(exp){
    return tagged_list$(exp, 'quote')
}
var cadr = function(exp){return car(cdr(exp))}
var caddr = function(exp){return car(cdr(cdr(exp)))}
var caadr = function(exp){return car(car(cdr(exp)))}
var cdadr = function(exp){return cdr(car(cdr(exp)))}
var cddr = function(exp){return cdr(cdr(exp))}
var cdddr = function(exp){return cdr(cdr(cdr(exp)))}
var cadddr = function(exp){return car(cdr(cdr(cdr(exp))))}

var text_of_quotation = function(exp){return cadr(exp)}
var tagged_list$ = function(exp, tag){
    if(pair$(exp))
        return (car(exp) === tag)
    return false
}

var assignment$ = function(exp){
    return tagged_list$(exp, 'set!')
}
var assignment_variable = function(exp){
    return cadr(exp)
}
var assignment_value = function(exp){
    return caddr(exp)
}

// (define x 12)
var definition$ = function(exp){
    return tagged_list$(exp, 'define')
}
var definition_variable = function(exp){
    if (symbol$(cadr(exp)))
        return cadr(exp) // (define x 12) => x
    return caadr(exp)    // (define (add a b) (+ a b)) => add
}
// (define x 12) => 12
var definition_value = function(exp){
    if (symbol$(cadr(exp)))
        return caddr(exp) // (define a 12)
    return make_lambda(cdadr(exp), cddr(exp)) // (define (add a b) (+ a b))
}

var lambda$ = function(exp){
    return tagged_list$(exp, 'lambda')
}
var lambda_parameters = function(exp){
    return cadr(exp)
}
var lambda_body = function(exp){
    return cddr(exp)
}
var make_lambda = function(parameters, body){
    return cons('lambda', cons(parameters, body))
}
// ==== if
var if$ = function(exp){
    return tagged_list$(exp, 'if')
} 
var if_predicate = function(exp){
    return cadr(exp)
}
var if_consequent = function(exp){
    return caddr(exp)
}
var if_alternative = function(exp){
    if (!null$(cddr(exp)))
        return cadddr(exp)
    return nil
}
var make_if = function(predicate, consequent, alternative){
    // return list('if', predicate, consequent, alternative)
    return cons('if', cons(predicate, cons(consequent, cons(alternative, []))))
}
// judge if
var eval_if = function(exp, env){
    if(true$(eval(if_predicate(exp), env)))
        return eval(if_consequent(exp), env)
    return eval(if_alternative(exp), env) 
}
// ====
var begin$ = function(exp){
    return tagged_list$(exp, 'begin')
}
var begin_actions = function(exp){
    return cdr(exp)
}
var last_exp$ = function(seq){
    return null$(cdr(seq))
}
var first_exp = function(seq){
    return car(seq)
}
var rest_exp = function(seq){
    return cdr(seq)
}
var make_begin = function(seq){
    return cons('begin', seq)
}
var sequence_to_exp = function(seq){
    if (null$(seq))
        return seq
    else if (last_exp$(seq))
        return first_exp(seq)
    return make_begin(seq)
}
var eval_sequence = function (exps, env){
    if (last_exp$(exps))
        return eval(first_exp(exps), env)
    eval(first_exp(exps), env)
    return eval_sequence(cdr(exps) , env)
}
var application$ = function(exp){
    return pair$(exp)
}
var operator = function(exp){
    return car(exp)
}
var operands = function(exp){
    return cdr(exp)
}
var no_operand$ = function(ops){
    return null$(ops)
}
var first_operand = function(ops){
    return car(ops)
}
var rest_operand = function(ops){
    return cdr(ops)
}
// ======
var cond$ = function(exp){
    return tagged_list$(exp, 'cond')
}
var cond_clauses = function(exp){
    return cdr(exp)
}
var cond_else_clauses = function(clause){
    return (cond_predicate(clause) === 'else')
}
var cond_predicate = function(clause){
    return car(clause)
}
var cond_actions = function(clause){
    return cdr(clause)
}
var cond_to_if = function(exp){
    return expand_clauses(cond_clauses(exp))
}
var expand_clauses = function(clauses){
    if (null$(clauses))
        return nil
    var first = car(clauses)
    var rest = cdr(clauses)
    if(cond_else_clauses(first)){
        if(null$(rest))
            return sequence_to_exp(cond_actions(first))
        else
            console.log("ELSE clause isn't last -- COND->IF" + clauses)
    }
    else{
        return make_if(cond_predicate(first),
               sequence_to_exp(cond_actions(first)),
               expand_clauses(rest))
    }
}
var true$ = function(x){
    return (x!=false) ? true:false
}
var false$ = function(x){
    return (x===false) ? false:true
}
// ========== lambda =============
var make_procedure = function(parameters, body, env){
    return cons('procedure', cons(parameters, cons(body, cons(env, []))))
}
var compound_procedure$ = function(p){
    return tagged_list$(p, 'procedure')
}
var procedure_parameters = function(p){
    return cadr(p)
}
var procedure_body = function(p){
    return caddr(p)
}
var procedure_environment = function(p){
    return cadddr(p)
}
// =========== macro =============
var macro$ = function(exp){
    return tagged_list$(exp, 'macro')
}
var make_macro = function(parameters, body, env){
    return cons('macro', cons(parameters, cons(body, cons(env, []))))
}
var macro_parameters = function(p){ // (macro (X) @(* ,X ,X)) => (X)
    return cadr(p)
}
var macro_body = function(p){       // => @(* ,X ,X)
    return caddr(p)
}
var macro_environment = function(p){
    return cadddr(p)
}
var eval_macro = function(body, env){ // eval macro
    var x = eval_sequence(body, env)
    return eval(x, cdr(env))
}
//=============================
var vector_operation = function(vec, arguments){
    if (null$(cdr(arguments))){  // ([0 12] 1) => 12
        return vec.value[car(arguments).value]
    }
    vec.value[car(arguments).value] = car(cdr(arguments)) // ([0 1] 0 12) => [12 1]
    return vec
}
// ======
var enclosing_environment = function(env){
    return cdr(env)
}
var first_frame = function(env){
    return car(env)
}

/*
var make_frame 
var frame_variable
var frame_values
var add_bindint_to_frame
*/
var make_frame = function(variables, values){
    var frame = {}
    while(variables.length!=0){
        frame[car(variables)] = car(values)
        variables = cdr(variables)
        values = cdr(values)
    }
}
var add_bindint_to_frame = function(var_, val, frame){
    frame[var_] = val
}
var extend_environment = function(vars, vals, base_env){
    var extend_environment_iter = function(vars, vals, base_env, result){
        if(null$(vars))
            return cons(result, base_env)
        if(null$(vals))
            console.log("Too few arguments suppled")
        // . 
        if(car(vars) === '.'){
            result[car(cdr(vars))] = vals
            return cons(result, base_env)
        }
        result[car(vars)] = car(vals)
        return extend_environment_iter(cdr(vars), cdr(vals), base_env, result)
    }
    return extend_environment_iter(vars, vals, base_env, {})
}
/*
  ({b:15},              frame
      ({a:12, b:15},    enclosing env
          ({b:12, c:14})))
 */ 
var lookup_variable_value = function(var_, env){
    if(null$(env)){
        console.log("Unbound variable -- "+var_)
    }
    var frame = car(env)
    if(var_ in frame)
        return frame[var_]
    return lookup_variable_value(var_, enclosing_environment(env))
}

var set_variable_value = function(var_, val, env){
    if(null$(env)){
        console.log("Unbound variable -- "+var_)
    }
    var frame = car(env)
    if(var_ in frame){
        frame[var_] = val
        return
    }
    return set_variable_value(var_, val, enclosing_environment(env))
}

var define_variable = function(var_, val, env){
    var frame = car(env)
    if(var_ in frame){
        frame[var_] = val
        return
    }
    add_bindint_to_frame(var_, val, frame)
    return
}
// ==========
var set_up_environment = function(){
    /* var initial_env = extend_environment(primitive_procedure_name,
                                         primitive_procedure_obj,
                                         the_empty_env) */
    var initial_env = cons(primitive_procedure, [])
    return initial_env
}

//=========== primitive procedures =============================
var primitive_procedure$ = function(proc){
    return (typeof(proc) === 'function')
}


/*
  toy primitive procedures
  
*/
// (car '(1 2))
var _car = function(x){
    return car(car(x))
}
var _cdr = function(x){
    return cdr(car(x))
}
var _cons = function(x){
    return cons(car(x), car(cdr(x)))
}
var _null$ = function(x){
    return null$(car(x))
}
var _number$ = function(x){
    return number$(car(x))
}
var integer$ = function(x){
    x = car(x)
    return (x.constructor === Number && x.type === INT)? true:nil
}
var float$ = function(x){
    x = car(x)
    return (x.constructor === Number && x.type === FLOAT) ? true:nil
}
var _pair$ = function(x){
    return pair$(car(x))
}
var _atom$ = function(x){
    return atom$(car(x))
}
var _symbol$ = function(x){
    return symbol$(car(x))
}
var _procedure$ = function(x){
    return compound_procedure$(car(x))
}
var _vector$ = function(x){
    return _vector$(car(x))
}
var _eq$ = function(x){
    return eq$(car(x), car(cdr(x)))
}
var __add__ = function(x){
    var arg1 = car(x); var arg2 = car(cdr(x));
    if(arg1.constructor!==Number || arg2.constructor!==Number){
        console.log("Invalid type parameter -- + ")
    } 
    if(arg1.type === INT && arg2.type === INT)
        return new Number(arg1.value+arg2.value, INT)
    return new Number(arg1.value+arg2.value, FLOAT)
}
var __sub__ = function(x){
    var arg1 = car(x); var arg2 = car(cdr(x));
    if(arg1.constructor!==Number || arg2.constructor!==Number){
        console.log("Invalid type parameter -- - ")
    } 
    if(arg1.type === INT && arg2.type === INT)
        return new Number(arg1.value - arg2.value, INT)
    return new Number(arg1.value - arg2.value, FLOAT)
}
var __mul__ = function(x){
    var arg1 = car(x); var arg2 = car(cdr(x));
    if(arg1.constructor!==Number || arg2.constructor!==Number){
        console.log("Invalid type parameter -- * ")
    } 
    if(arg1.type === INT && arg2.type === INT)
        return new Number(arg1.value * arg2.value, INT)
    return new Number(arg1.value * arg2.value, FLOAT)
}
var __div__ = function(x){
    var arg1 = car(x); var arg2 = car(cdr(x));
    if(arg1.constructor!==Number || arg2.constructor!==Number){
        console.log("Invalid type parameter -- / ")
    } 
    if(arg1.type === INT && arg2.type === INT)
        return new Number(Math.floor(arg1.value/arg2.value), INT)
    return new Number(arg1.value/arg2.value, FLOAT)
}
var _lt = function(x){
    var arg1 = car(x); var arg2 = car(cdr(x))
    if(typeof(arg1) === 'string' && typeof(arg2) === ' string')
        return arg1<arg2
    if(arg1.constructor === Number && arg2.constructor === Number)
        return arg1.value < arg2.value
    console.log("Invalid type parameter -- <")
    return false
}
var set_car = function(x){
    var arg1 = car(x); var arg2 = car(cdr(x))
    arg1[0] = arg2
}
var set_cdr = function(x){
    var arg1 = car(x); var arg2 = car(cdr(x))
    arg1[1] = arg2
}
var _vector = function(x){
    var output = []
    while(!null$(x)){
        output.push(car(x))
        x = cdr(x)
    }
    return new Vector(output)
}
var _vector_push = function(x){
    var arg1 = car(x); var arg2 = car(cdr(x))
    if(arg1.constructor !== Vector) {console.log("Invalid type parameter -- vector_push")}
    arg1.value.push(arg2)
}
var _vector_pop = function(x){
    if(car(x).constructor !== Vector) {console.log("Invalid type parameter -- vector_pop")}
    return car(x).value.pop()
}
var _vector_length = function(x){
    if(car(x).constructor !== Vector) {console.log("Invalid type parameter -- vector_length")}
    return new Number(car(x).value.length, INT)
}
var _macroexpand = function(x, env){ // (macroexpand '(square 2)), where x is (square 2)
    x = car(x)
    var macro_name = car(x) 
    var arguments = cdr(x)
    var macro = lookup_variable_value(macro_name, env)
    if(!macro$(macro)){
        console.log("Invalid macro for macroexpand -- " + macro_name)
        return 'undefined'
    }
    var m_body = macro_body(macro)
    var m_parameters = macro_parameters(macro)
    var m_environment = macro_environment(macro)
    return eval_sequence(m_body, extend_environment(m_parameters, arguments, m_environment))
}
var _eval = function(x, env){ // eval function
    return eval(car(x), env)
}
var _apply = function(x, env){ // apply function
    return apply(car(x), cadr(x), env)
}
/*
=================================================
===================== Done ======================
=================================================
*/
// primitive procedure
var primitive_procedure = {
    'true':true,'false':false,
    'car':_car,'cdr':_cdr,'cons':_cons,'eq?':_eq$,'atom?':_atom$,
    'null?':_null$,
    'number?':_number$,'integer?':integer$,'float?':float$,
    'pair?':_pair$,'string?':_atom$,
    'procedure?':_procedure$, 'symbol?':_symbol$, 'vector?':_vector$,
    'display':display,
    '+':__add__, '-':__sub__, '*':__mul__, '/':__div__,
    '<':_lt,
    'set_car!':set_car, 'set_cdr!':set_cdr,
    'vector':_vector, 'vector_push':_vector_push, 'vector_pop':_vector_pop,'vector_length':_vector_length,
    'macroexpand':_macroexpand, 'eval':_eval, 'apply':_apply
}

// call primitive
var apply_primitive_procedure = function(proc, args, base_env){
    return proc(args, base_env)
}

// =======

var eval = function(exp, env){
    if (self_evaluation$(exp))    // number vector atom list which won't be calculated
        return exp
    else if (variable$(exp))      // if it is variable, get its value from env
        return lookup_variable_value(exp, env)
    else if (quoted$(exp))       // quote value: (quote (1 2))
        return text_of_quotation(exp)
    else if (assignment$(exp))   // assignment: (set! x 12)
        return eval_assignment(exp, env)
    else if (definition$(exp))   // definition: (define x 12)
        return eval_definition(exp, env)
    else if (if$(exp))           // if: (if 1 2 3)
        return eval_if(exp, env)
    else if (lambda$(exp))       // lambda: (lambda (a) a)
        return make_procedure(lambda_parameters(exp),
                             lambda_body(exp),
                             env)
    else if (macro$(exp))       // macro: (macro (x) @(* ,x ,x))
        return make_macro(lambda_parameters(exp),
                          lambda_body(exp),
                          env)
    else if (begin$(exp))        // begin: (begin ...)
        return eval_sequence(begin_actions(exp), env)
    else if (cond$(exp))         // cond: ...
        return eval(cond_to_if(exp), env) 
    else if (application$(exp))
        return apply(eval(operator(exp), env), 
                     operands(exp), env)
    else
        console.log("Unknown expression type -- EVAL" + exp)
}

var apply = function(procedure, uncalcualted_arguments, base_env){
    // procedure arguments
    var list_of_values = function(exps, env){
        if(no_operand$(exps))
            return []
        return cons(eval(first_operand(exps), env),
                    list_of_values(cdr(exps), env))
    }

    if (vector$(procedure)){  // vector
        vector_operation(procedure, list_of_values(uncalcualted_arguments, base_env))
    }
    else if (primitive_procedure$(procedure))  // primitive
        return apply_primitive_procedure(procedure, list_of_values(uncalcualted_arguments, base_env), base_env)
    
    else if (macro$(procedure))     // macro
        return eval_macro(macro_body(procedure),
                          extend_environment(macro_parameters(procedure),
                                             uncalcualted_arguments,
                                             macro_environment(procedure)))
    else if (compound_procedure$(procedure))   // lambda
        return eval_sequence(procedure_body(procedure),
                            extend_environment(procedure_parameters(procedure),
                                              list_of_values(uncalcualted_arguments, base_env),
                                              procedure_environment(procedure)))
    else
        console.log("Unknown procedure type -- APPLY" + procedure)
}


// ======================




var ENV = set_up_environment()
// var x = "(define (add a b) (+ a b)) (add 3 4)"
var x = "(apply + '(3 4))"
var token_list = Tokenize_String(x)
var parsed_obj = ParseString(token_list)

console.log(eval_sequence(parsed_obj, ENV))
console.log(ENV)























