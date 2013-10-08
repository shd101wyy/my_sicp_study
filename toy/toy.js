// minimal toy language

// Tokenize String
// tokenize input string
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
        else if (input_str[i] == '}' || input_str[i] == ']'){
            output.push(')')
        }
        /*
            special token
        */
        else if (input_str[i]=='('||input_str[i]==')'||
            input_str[i]=='['||input_str[i]==']'||
            input_str[i]=='{'||input_str[i]=='}'){
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
            var formatSpecial = function(input_str, i, output){
                if(input_str[i]==="@" || input_str[i]==="'" || input_str[i]===","){ // quasiquote quote unquote
                    // check flag
                    var flag
                    if(input_str[i]==="'")
                        flag = 'quote'
                    else if (input_str[i]===',')
                        flag = 'unquote'
                    else
                        flag = 'quasiquote'

                    output.push('(')
                    output.push(flag)
                    var i = formatSpecial(input_str, i+1, output)
                    output.push(")")
                    return i
                }
                
                // nothing
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
                return i
            }
            i = formatSpecial(input_str, i, output)
        }
    }
    return output
}
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

// =====
// parse token list
var ParseString = function(token_list){
    var rest;
    // 12 -> Number(12,INT))
    // if its type is not Number
    // return itself
    var formatSymbol = function(input_str){
        var type = numberType(input_str)
        if (type){
            return new Number(parseFloat(input_str), type)
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
        // pair
        else if (token_list[0]==='.'){
            if(token_list[1]==='('){
                return parseList(token_list.slice(2))
            }
            else{
                if (token_list[2]!==')'){
                    console.log("Error...invalid pair")
                    return []
                }
                rest = token_list.slice(3)
                return formatSymbol(token_list[1])
            }
        }
        else 
            return cons(formatSymbol(token_list[0]), parseList(token_list.slice(1)))
    }
    
    var ParseString_iter = function(token_list){
        // finish
        if(token_list.length == 0)
            return []
        // list
        if(token_list[0]=='('){
            return cons(parseList(token_list.slice(1)), ParseString_iter(rest))
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
var nil = []
// check whether x is number
var number$ = function(x){
    return (x.constructor === Number)? '#t':nil
}
var integer$ = function(x){
    return (x.constructor === Number && x.type === INT)? '#t':nil
}
var float$ = function(x){
    return (x.constructor === Number && x.type === FLOAT) ? '#t':nil
}
// check whether x is atom
var atom$ = function(x){ 
    return (typeof(x) === 'string')? '#t':nil
}
var string$ = atom$
// check whether x is vector
var vector$ = function(x){
    return (x.constructor === Vector)? '#t':nil
}
// check whether x is pair
var pair$ = function(x){
    return (vector$(x) || atom$(x) || number$(x)) nil:'#t'
}
// check whether x is symbol
var symbol$ = function(x){
    return (atom$(x) && !isNumber(x)) '#t':nil
}
// check list is null?
var null$ = function(x){
    if(pair$(x))
        return (x.lengh==0)? '#t':nil
    return nil
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
    else if (string$(exp))
        return exp
    return nil
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

var text_of_quotation = function(exp){return cadr(ex)}
var tagged_list$ = function(exp, tag){
    if(pair$(exp))
        return eq$(car(exp), tag)
    return nil
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

var definition$ = function(exp){
    return tagged_list$(exp, 'define')
}
var definition_variable = function(exp){
    if (symbol$ cadr(exp))
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
    if (!null$(cddrexp))
        return cadddr(exp)
    return nil
}
var make_if = function(predicate, consequent, alternative){
    // return list('if', predicate, consequent, alternative)
    return cons('if', cons(predicate, cons(consequent, cons(alternative, []))))
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
    return eq$(cond_predicate(clause), 'else')
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
    return !null$(x)
}
var false$ = function(x){
    return null$(x)
}
// ============================
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
// ======
var enclosing_environment = function(env){
    return cdr(env)
}
var first_frame = function(env){
    return car(env)
}
var the_empty_env = {}
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
    if(length(vars) == length(vals)){
        cons(make_frame(vars, vals), base_env)
    }
    else{
        if(length(vars) < length(vals)){
            console.log("Too many arguments supplied")
        }
        else{
            console.log("Too few arguments supplied")
        }
    }
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
    var initial_env = cons(primitive_procedure, the_empty_env)
    return initial_env
}
var primitive_procedure$ = function(proc_name){
    return (proc_name in primitive_procedure)
}
var primitive_implementation = function(proc_name){
    return primitive_procedure[proc_name]
}

// primitive procedure
var primitive_procedure = {
    'car':car,
    'cdr':cdr,
    'cons':cons,
    'null?':null$,
    'number?':number$,'integer?':integer$,'float?':float$,
    'pair?':pair$,'atom?':atom$,'string?':atom$,
    'procedure?':compound_procedure$, 'symbol?':symbol$, 'vector?':vector$

}

// call primitive
var apply_primitive_procedure = function(proc, args){
    return primitive_implementation(proc)(args)
}

// =======

var eval = function(exp, env){
    // procedure arguments
    var list_of_values = function(exps, env){
        if(no_operand$(exps))
            return []
        return cons(eval(first_operand(exps), env),
                    list_of_values(cdr(exps), env))
    }
    // judge if
    var eval_if = function(exp, env){
        if(true$(eval(if_predicate(exp), env)))
            return eval(if_consequent(exp), env)
        return eval(if_alternative(exp), env) 
    }
    if (self_evaluation$(exp))
        return exp
    else if (variables$(exp))
        return lookup_variable_value(exp, env)
    else if (quoted$(exp))
        return text_of_quotation(exp)
    else if (assignment$(exp))
        return eval_assignment(exp, env)
    else if (definition$(exp))
        return eval_definition(exp, env)
    else if (if$(exp))
        return eval_if(exp, env)
    else if (lambda$(exp))
        return make_procedure(lambda_parameters(exp),
                             lambda_body(exp),
                             env)
    else if (begin$(exp))
        return eval_sequence(begin_actions(exp), env)
    else if (cond$(exp))
        return eval(cond_to_if(exp), env)
    else if (application$(exp))
        return apply(eval(operator(exp), env),
                     list_of_values(operands(exp), env))
    else
        console.log("Unknown expression type -- EVAL" + exp)
}

var apply = function(procedure, arguments){
    if (primitive_procedure$(procedure))
        return apply_primitive_procedure(procedure, arguments)
    else if (compound_procedure$(procedure))
        return eval_sequence(procedure_body(procedure),
                            extend_environment(procedure_parameters(procedure),
                                              arguments,
                                              procedure_environment(procedure)))
    else
        console.log("Unknown procedure type -- APPLY" + procedure)
}




// ======================
