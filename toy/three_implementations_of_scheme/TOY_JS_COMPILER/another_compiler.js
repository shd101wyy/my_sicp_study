/*

    Toy Language to JavaScritp compiler

*/
var LIST = 2;
var MACRO = 4;
var PROCEDURE = 6;
var BUILTIN_PRIMITIVE_PROCEDURE = 8;
var RATIO = 10;
var FLOAT = 12;

// build List
var Cons = function(x, y)
{
    this.car = x;
    this.cdr = y;
    this.set_car = function(value)
    {
        this.car = value;
    }
    this.set_cdr = function(value)
    {
        this.cdr = value;
    }

    this.TYPE = LIST  // for virtual machien check
}
// build procedure
var Procedure = function(start_pc, env, param_array)
{
    this.start_pc = start_pc;
    this.closure_env = env;
    this.param_array = param_array;
    this.TYPE = PROCEDURE;
}
// build macro
var Macro = function(start_pc, env, param_array)
{
    this.start_pc = start_pc;
    this.closure_env = env;
    this.param_array = param_array;
    this.TYPE = MACRO;
}
// build primitive builtin procedure
var Builtin_Primitive_Procedure = function(func)
{
    this.func = func;
    this.TYPE = BUILTIN_PRIMITIVE_PROCEDURE;
}
var Toy_Number = function(numer, denom, type)
{
    this.TYPE === type;
    this.numer = numer;
    this.denom = denom;
}
var cons = function(x, y)
{
    return new Cons(x,y);
}
var build_nil = function()
{
   return null;
}
/*
    check whether string is number
*/
function isNumber(n) {
  return !isNaN(parseFloat(n)) && isFinite(n);
}
/*
    check whether string is integer
*/
var isInteger = function(n)
{ 
    if(n.length==0)return false; 
    if(n[0]=="-") n = n.slice(1);
    return n==="0" || /^[1-9][0-9]*$/.test(n) }
var isFloat = function(n){return isNumber(n) && !(isInteger(n))}
var isRatio = function(n) // can only check string
{
    if(typeof(n)!=="string") return false;
    var index_of_slash = n.indexOf("/");
    if(index_of_slash === -1) return false;
    var numer = n.slice(0, index_of_slash);
    var denom = n.slice(index_of_slash+1);
    // if(numer.length === 0 || denom.length == 0) return false;
    if(isInteger(numer) && isInteger(denom)) // didn't consider the case denominator is 0
    {
        if(parseInt(denom) === 0)
        {
            console.log("Invalid ratio --- " + n + " with denominator 0");
        }
        return true;
    }
    return false;
}
// The Below 2 functions can be used
// when "n" has been proved to be ratio
var getNumerator = function(n)
{
    return n.slice(0, n.indexOf("/"));
}
var getDenominator = function(n)
{
    return n.slice(n.indexOf("/")+1);
}
var car = function(l){return l.car};
var cdr = function(l){return l.cdr};
var caar = function(obj){return car(car(obj))}
var cadr = function(obj){return car(cdr(obj))}
var cddr = function(obj){return cdr(cdr(obj))}
var cdddr = function(obj){return cdr(cdr(cdr(obj)))}
var caddr = function(obj){return car(cdr(cdr(obj)))}
var cadddr = function(obj){return car(cdr(cdr(cdr(obj))))}
var cadar = function(obj){return car(cdr(car(obj)))}
/* ================================= */
/* tokenize string to list */
var lexer = function(input_str)
{   
    var find_final_comment_index = function(input_str, i) // find end index of comment
    {
        if(i == input_str.length) return i;
        if(input_str[i]=="\n") return i+1;
        else return find_final_comment_index(input_str, i + 1);
    }
    var find_final_string_index = function(input_str, i)  // find final index of string 
    {
        if(i == input_str.length)
            console.log("ERROR: Incomplete String");
        else if(input_str[i]=="\\")
            return find_final_string_index(input_str, i+2);
        else if(input_str[i]==='"')
            return i+1;
        else 
            return find_final_string_index(input_str, i+1)
    }
    var find_final_number_of_atom_index = function(input_str, i)
    {
        if(i == input_str.length)
            return i;
        if(input_str[i]=="(" || input_str[i]==")"
            || input_str[i]=="[" || input_str[i]=="]"
            || input_str[i]=="{" || input_str[i]=="}"
            || input_str[i]==" " || input_str[i]=="\t"
            || input_str[i]=="\n" || input_str[i]==";"
            || input_str[i]==",")
            return i;
        else
            return find_final_number_of_atom_index(input_str, i+1);
    }
    var lexer_iter = function(input_str, i)
    {
        if(i>=input_str.length)
            return null; // finish
        else if(input_str[i]===" " || input_str[i]=="\n" || input_str[i]=="\t" || input_str[i]===",") // remove space tab newline ,
            return lexer_iter(input_str, i + 1);
        else if(input_str[i]==="(")
            return cons( "(", lexer_iter(input_str, i + 1));
        else if(input_str[i]==="[")
            return cons( "(", cons( "vector", lexer_iter(input_str, i + 1)));
        else if(input_str[i]==="{")
            return cons( "(", cons( "dictionary", lexer_iter(input_str, i + 1)));
        else if(input_str[i]===")" || input_str[i]=="]" || input_str[i]=="}")
            return cons( ")", lexer_iter(input_str, i + 1));
        else if(input_str[i]==="'" || input_str[i]=="`" || input_str[i]=="~")
            return cons( input_str[i], lexer_iter(input_str, i + 1));
        else if(input_str[i]==='"')
        {
            var end = find_final_string_index(input_str, i+1);
            return cons(input_str.slice(i, end), lexer_iter(input_str, end))
            // return cons("(", cons("quote", cons(input_str.slice(i, end), cons(")", lexer_iter(input_str, end)))))
        }
        else if(input_str[i]===";")
            return lexer_iter(input_str, find_final_comment_index(input_str, i+1));
        else
        {
            // atom or number
            var end = find_final_number_of_atom_index(input_str, i+1);
            return cons( input_str.slice(i, end) , lexer_iter(input_str, end));
        }
    }
    return lexer_iter(input_str, 0);
}

/* parse list to list */
var parser = function(l)
{
    var rest = l; // keep track of rest
    var parse_list = function(l)
    {
        if(car(l) === ")") // finish
        {
            rest = cdr(l);
            return build_nil();
        }
        else if (car(l) === "(") // list
        {
            return cons(parse_list(cdr(l)), parse_list(rest));
        }
        else if (car(l) === "'" || car(l) === "~" || car(l) === "`")  // quote unquote quasiquote
        {
            return cons(parse_special(l), parse_list(rest));
        }
        else  // symbol or number
        {
            return cons(parse_symbol_or_number( car(l) ), parse_list(cdr(l)));
        }
    }
    var parse_special = function(l)
    {
        var tag ;
        if(car(l) === "'")
            tag = "quote"
        else if (car(l) === "~")
            tag = "unquote"
        else tag = 'quasiquote'
        l = cdr(l);
        if (car(l) === "(") // list
        {
            return cons(tag, cons(parse_list(cdr(l)), build_nil()));
        }
        else if (car(l) === "'" || car(l) === "~" || car(l) === "`")  // quote unquote quasiquote
        {   // here my be some errors
            return cons(tag, cons(parse_special(l), build_nil()));
        }
        else  // symbol or number
        {
            rest = cdr(l);
            return cons(tag, cons(parse_symbol_or_number(car(l)), build_nil()));
        }
    }
    var parse_symbol_or_number = function(l)
    {
        if(l[0]==":")
            return cons("keyword", cons('"'+l.slice(1)+'"', build_nil()))
        return l;
    }
    // done
    if(l == null)
        return build_nil();
    // list
    else if (car(l) === "(")
    {
        return cons(parse_list(cdr(l)), parser(rest));
    }
    // quote // unquote // quasiquote
    else if (car(l) === "'" || car(l) === "~" || car(l) === "`")
    {
        return cons(parse_special(l), rest);
    }
    // symbol or number
    else
    {
        return cons(parse_symbol_or_number( car(l) ), parser(cdr(l)));
    }
}


/*
    (define (add a b) (+ a b))
    =>
    (define add (lambda (a b) (+ a b)))
*/
var make_lambda = function(exp)
{
    var tag = car(exp);                 // define or set!
    var var_name = car(car(cdr(exp)));  // add
    var args = cdr(car(cdr(exp)));      // (a b)
    var body = cdr(cdr(exp));           // ((+ a b))

    var lambda_body = cons('lambda', cons(args, body))
    return cons(tag, cons(var_name, cons(lambda_body, build_nil())))
}
/*
    compile definition
    (define x 12)
*/  
var definition_variable = function(exp)
{
    var var_name = cadr(exp);
    if(isNumber(var_name) || isRatio(var_name)){
        console.log("Invalid var name "+exp);
        return "undefined"
    }
    return cadr(exp);
}
var definition_value = function(exp)
{
    return caddr(exp);
}

var assignment_variable = function(exp)
{
    var var_name = cadr(exp);
    if(var_name.TYPE!==LIST && (isNumber(var_name) || isRatio(var_name))) {
        console.log("Invalid var name "+exp);
        return "undefined"
    }
    return cadr(exp);
}
var assignment_value = function(exp)
{
    return caddr(exp);
}

/* Compile if */
var if_test = function(exp)
{
    return cadr(exp);
}
var if_consequent = function(exp)
{
    return caddr(exp);
}
var if_alternative = function(exp)
{
    var v = cdddr(exp);
    // (if 1 2)
    if(v == null)
        return '0';
    return car(v);
}

/*
    compile lambda
    (lambda (a b) (+ a b))
*/
var lambda_arguments = function(exp)
{
    return cadr(exp);
}
var lambda_body  = function(exp)
{
    return cddr(exp);
}

/*
    compile application
*/
var application_head = function(exp)
{
    return car(exp);
}
var application_args = function(exp)
{
    return cdr(exp);
}




/*
    Numeric Calculation
*/
// GCD
var gcd = function(a,b)
{
    if (b==0)
        return a
    return gcd(b,a%b)
}
var numer = function(rat){return rat.numer}
var denom = function(rat){return rat.denom}
var make_rat = function(numer, denom)
{
    var g = gcd(numer, denom)
    var numer = numer/g;
    var denom = denom/g;
    return new Toy_Number(numer, denom, RATIO);
}
/* convert Toy_Number to string */
var number_to_string = function(num)
{
    if(num.TYPE === FLOAT) return ""+num.numer;
    else if (num.denom === 1) return "" + num.numer;
    return num.numer+"/"+num.denom;
}
// fraction arithematic
var add_rat = function(x,y){
   return make_rat( numer(x)*denom(y)+numer(y)*denom(x) , denom(x)*denom(y))
}
var sub_rat = function(x,y){
    return make_rat( numer(x)*denom(y)-numer(y)*denom(x) , denom(x)*denom(y))
}
var mul_rat = function(x,y){
    return make_rat(numer(x)*numer(y), denom(x)*denom(y))
}
var div_rat = function (x,y){
    return make_rat(numer(x)*denom(y),denom(x)*numer(y))
}

































/*
    interpret directly
*/
var REF = 1; // ref var_name at_frame_index_of_env
var CONSTANT = 2; // constant value
var ASSIGN = 3; // assign var_name at_frame_index_of_env
var RETURN = 4; // RETURN 0 0
var CLOSURE = 5; // CLOSURE return_place param_num
var FRAME = 6; // create new frame
var ARGUMENT = 7; // push argument
var CALL = 8;     // call 
var TEST = 9; // test jmp_steps
var JMP = 10; // jmp jump steps
var NIL = 11; 
var ADDPARAM = 12; // ADDPARAM name
var MAKE_RATIO = 13; // construct ratio
var lookup_env = function(symbol_table, var_name, instructions)
{
    for(var i = symbol_table.length-1; i>=0; i--)
    {
        if(var_name in symbol_table[i])
        {
            instructions.push([REF, var_name, i]);
            // instructions.push([REF, symbol_table[i][var_name], i]);
            return;
        }
    }
    console.log("Cannot find var : "+var_name);
    return;
}
var another_compiler_lambda = function(args, body, symbol_table, instructions)
{
    // begin closure
    var index = instructions.length;
    instructions.push([CLOSURE, 0, 0]);

    symbol_table.push({});
    // add args
    while(args!==null)
    {
        // symbol_table[symbol_table.length - 1][car(args)] = Object.keys(symbol_table[symbol_table.length - 1]).length;
        symbol_table[symbol_table.length - 1][car(args)] = true
        instructions.push([ADDPARAM, car(args), 0]); // add params
        args = cdr(args);
    }
    another_compiler_seq(body, symbol_table, instructions);
    // add return
    instructions.push([RETURN, 0, 0]);
    instructions[index][1] = instructions.length - 1; // set return index
    return;
}
var another_compiler_macro = function(macro_name, args, body, symbol_table, instructions)
{
    // begin closure
    var index = instructions.length;
    instructions.push([CLOSURE, 0, 1]);

    symbol_table.push({});
    // add args
    while(args!==null)
    {
        symbol_table[symbol_table.length - 1][car(args)] = true;
        instructions.push([ADDPARAM, car(args), 0]); // add params
        args = cdr(args);
    }
    another_compiler_seq(body, symbol_table, instructions);
    // add return
    instructions.push([RETURN, 0, 0]);
    instructions[index][1] = instructions.length - 1; // set return index
    instructions.push([ASSIGN, macro_name, 1]); // set macro
    return;
}
var another_compiler_applic = function(head, params, symbol_table, instructions)
{
    instructions.push([FRAME, 0, 0]);
    // compile parameters
    while(params!==null)
    {
        var v = car(params);
        another_compiler(v, symbol_table, instructions);
        instructions.push([ARGUMENT, 0, 0]);
        params = cdr(params);
    }
    // compile head
    another_compiler(head, symbol_table, instructions);
    instructions.push([CALL, 0, 0]);// call procedure
    return;
}
/*
    (if 1 2 3)
    0 CONSTANT 1
    1 TEST 3 0
    2 CONSTANT 2
    3 JMP 2
    4 CONSTANT 3

*/
var another_compiler_if = function(test, consequent, alternative, symbol_table, instructions)
{
    another_compiler(test, symbol_table, instructions) // compile test

    var index1 = instructions.length;
    instructions.push([TEST, 0, 0]);
    // compile consequent
    another_compiler(consequent, symbol_table, instructions);

    var index2 = instructions.length; 
    instructions.push([JMP, 0, 0]);
    // compile alternative
    another_compiler(alternative, symbol_table, instructions);
    instructions[index2][1] = instructions.length - index2;
    instructions[index1][1] = index2 + 1 - index1;
    return;
}
var another_compiler_seq = function(exps, symbol_table, instructions)
{
    if(exps==null)
        return instructions;
    else
    {
        another_compiler(car(exps), symbol_table, instructions);
        return another_compiler_seq(cdr(exps), symbol_table, instructions);
    }
}
/*
    Compile list
    without calculation
*/
var compile_list = function(exp, symbol_table, instructions)
{
    if(exp == null)
        return build_nil();
    else
    {
        var v = car(exp);
        if(v.TYPE === LIST)
        {
            return cons("cons", 
                         cons(compile_list(v, symbol_table, instructions),
                              cons(compile_list(cdr(exp), symbol_table, instructions), build_nil())))
        }
        else {
            return cons("cons",
                        cons(cons('quote', cons(v, null)), 
                             cons(compile_list(cdr(exp), symbol_table, instructions), build_nil())))
        }
    }
}
var compile_quasiquote = function(exp, symbol_table, instructions)
{
    if(exp == null)
        return build_nil();
    else
    {
        var v = car(exp);
        if(v.TYPE === LIST)
        {
            if(car(v) === "unquote")
            {
                var x = cadr(v)
                return cons("cons",
                        cons(x, 
                             cons(compile_quasiquote(cdr(exp), symbol_table, instructions), build_nil())))
            }
            else
                return cons("cons", 
                         cons(compile_quasiquote(v, symbol_table, instructions),
                              cons(compile_quasiquote(cdr(exp), symbol_table, instructions), build_nil())))
        }
        else {
            return cons("cons",
                        cons(cons('quote', cons(v, null)), 
                             cons(compile_quasiquote(cdr(exp), symbol_table, instructions), build_nil())))
        }
    }
}

/*
    well, this symbol_table is actually symbol table
    which stores like {a:0, b:1, c:2};
*/
var another_compiler = function(exp, symbol_table, instructions)
{
    if (isNumber(exp))     // number
    {
        if(isInteger(exp))
            instructions.push([MAKE_RATIO, exp, 1]);
        else
            instructions.push([CONSTANT, exp, 1]); // 1 means float
        return;
    }
    else if (isRatio(exp))
    {
        instructions.push([MAKE_RATIO, getNumerator(exp), getDenominator(exp)]);
        return;
    }
    else if(typeof(exp) === 'string') // string
    {
    	if(exp[0] === "\"") 
    	{
    		instructions.push([CONSTANT, exp, 2]); // 2 means string
    		return;
    	}
        return lookup_env(symbol_table, exp, instructions);
    }
    else if (exp == null)
    {
        instructions.push([NIL, 0, 0]);
        return;
    }
    else if (exp.TYPE === LIST) // lisp
    {
        var tag = car(exp);
        if(tag === "quote" || tag === "quasiquote")
        {
            var v = cadr(exp);
            if(v === null)
            {
                instructions.push([NIL, 0, 0])
                return;
            }
            if(v.TYPE === LIST)
            {
                if(tag === "quote")
                    return another_compiler(compile_list(cadr(exp), symbol_table, instructions), symbol_table, instructions);
                else
                    return another_compiler(compile_quasiquote(cadr(exp), symbol_table, instructions), symbol_table, instructions);
            }   
            else if (isNumber(v))
            {
                if(isInteger(exp))
                    instructions.push([MAKE_RATIO, exp, 1]);
                else
            	   instructions.push([CONSTANT, v, 1]);
            }
            else if (isRatio(v))
            {
                instructions.push([MAKE_RATIO, getNumerator(v), getDenominator(v)]);
                return;
            }
            else
            {
                instructions.push([CONSTANT, v, 0]); // 0 means atom
                return;
            }
        }
        else if (tag === "def")
        {
            if(cadr(exp).TYPE === LIST)
                return another_compiler(make_lambda(exp), symbol_table, instructions);
            var var_name = cadr(exp);
            var var_value = caddr(exp);
            another_compiler(var_value, symbol_table, instructions);
            //symbol_table[symbol_table.length - 1][var_name] = Object.keys(symbol_table[symbol_table.length - 1]).length; // add var name to symbol table
            //instructions.push([ASSIGN, symbol_table[symbol_table.length - 1][var_name], symbol_table.length - 1]) // push instructions
            symbol_table[symbol_table.length - 1][var_name] = "undefined";
            instructions.push([ASSIGN, var_name, symbol_table.length - 1]) 
            return;
        }
        else if (tag === "set!")
        {
            var var_name = cadr(exp);
            var var_value = caddr(exp);
            another_compiler(var_value, symbol_table, instructions); // compile var value
            /* check var_name in symbol table */
            for(var i = symbol_table.length - 1; i>=0; i--)
            {
                if(var_name in symbol_table[i])
                {
                    // instructions.push([ASSIGN, symbol_table[i][var_name], i])
                   	instructions.push([ASSIGN, var_name, i])
                    return;
                }
            }
            console.log("Unbound variable name: " + var_name);
            return;
        }
        else if (tag === "if")
        {
            return another_compiler_if(if_test(exp), if_consequent(exp), if_alternative(exp), symbol_table, instructions);
        }
        else if (tag === "lambda")
        {
            return another_compiler_lambda(lambda_arguments(exp), lambda_body(exp), symbol_table.slice(0), instructions);
        }
        /* (defmacro square (x) @(* ,x ,x)) */
        else if (tag === "defmacro") // calculate macro
        {
        								 // macroname // args    // body
            return another_compiler_macro(cadr(exp), caddr(exp), cdddr(exp), symbol_table.slice(0), instructions);
        }
        /* check macro */
        else if (tag in symbol_table[1] && symbol_table[1][tag].TYPE === MACRO)
        {
        	// console.log("=========== It is macro =================");
        	var macro_value = symbol_table[1][tag];

        	var closure_env = macro_value.closure_env.slice(0);
            var start_pc = macro_value.start_pc;
            var param_array = macro_value.param_array; // retrieve param array.. eg (def (add a b) (+ a b)) param array is [a, b]
            var new_frame = {}; // create new env frame

            var param_vals = cdr(exp); // add params to frame
            for(var i = 0; i < param_array.length; i++)
            {
            	var p_v = car(param_vals)
                if(param_array[i] === ".") // variadic
                {
                    new_frame[param_array[i+1]] = param_vals;
                    break; 
                }
            	new_frame[param_array[i]] = p_v;  // assign value
            	param_vals = cdr(p_v);
            }
            closure_env.push(new_frame); // add new frame
            var v = another_interpreter(instructions, closure_env, null, [], start_pc); // calculate macro
            // recompile calculated macro
            another_compiler(v, symbol_table, instructions);
        	return;  
        }
        /*
        else if (tag in ENVIRONMENT[0] && ENVIRONMENT[0][tag].TYPE === MACRO)
        {
            var closure_env = ENVIRONMENT[tag].closure_env;
            var start_pc = ENVIRONMENT[tag].start_pc;
            // append params to frame
            var frame = [];
            var params = cdr(exp);
            while(params!==null)
            {
                frame.push(car(params));
                params = cdr(params);
            }
            var new_env = closure_env.slice(0);
            new_env.push(frame);

            var compiled_result = another_interpreter(insts, new_env, null, [], start_pc);
            // then compile again
            return another_compiler(compiled_result, symbol_table, instructions);
        } */
        else if (tag === "begin")
        {
            return compile_begin(cdr(exp), symbol_table, instructions);
        }
        else
        {
            return another_compiler_applic(application_head(exp), application_args(exp), symbol_table.slice(0), instructions);
        }
    }
    else if (exp instanceof Toy_Number) // this place is huhhh  fix (defmacro x () '(def y 12)) bug
    {
        instructions.push([MAKE_RATIO, exp.numer, exp.denom])
        return;
    }
    else // this place has bug
        return;
}

var another_interpreter = function(insts, env, acc, stack, pc)
{
    if(pc === insts.length)
        return acc;
    var inst = insts[pc];
    var op = inst[0];
    var arg0 = inst[1];
    var arg1 = inst[2];
    // console.log("===> " + op + " " + arg0 + " " + arg1)
    if(op === REF)
        return another_interpreter(insts, env, env[arg1][arg0], stack, pc+1);
    else if (op === CONSTANT){
    	var v = arg0;
    	if(arg1 === 0){} // atom
    	else if (arg1 === 1) // float number
    		v = new Toy_Number(arg0, 1, FLOAT);
    	else // string
    		v = v.slice(1, v.length - 1);
        return another_interpreter(insts, env, v, stack, pc+1);
    }
    else if (op === ASSIGN)
    {
        env[arg1][arg0] = acc;
        return another_interpreter(insts, env, acc, stack, pc+1)
    }
    else if (op === RETURN)
    {
        return acc;
    }
    else if (op === CLOSURE)
    {
        var v;
        /* 
        	count param num
        */
        pc++;
        var param_array = [];
        while(insts[pc][0]==ADDPARAM)
        {
        	param_array.push(insts[pc][1]);
        	pc++;
        }
        /*
			finish save param array
        */
        if(arg1 === 1)
            v = new Macro(pc, env.slice(0), param_array); // macro
        else 
            v =  new Procedure(pc, env.slice(0), param_array); // make closure... temp
        return another_interpreter(insts, env, v, stack, arg0+1); // jmp
    }
    else if (op === FRAME) // add frame
    {
        stack.push([]);
        return another_interpreter(insts, env, acc, stack, pc+1);
    }
    else if (op === ARGUMENT) // push argument
    {
        stack[stack.length - 1].push(acc);
        return another_interpreter(insts, env, acc, stack, pc+1);
    }
    else if (op === NIL) // build null
    {
    	return another_interpreter(insts, env, null, stack, pc+1);
    }
    else if (op === CALL)
    {
        if(acc.TYPE === BUILTIN_PRIMITIVE_PROCEDURE)
        {
            var v = acc.func(stack.pop());
            return another_interpreter(insts, env, v, stack, pc+1);
        }
        else if (acc.TYPE === PROCEDURE)
        {
        	var closure_env = acc.closure_env.slice(0);
            
            var param_array = acc.param_array; // retrieve param array.. eg (def (add a b) (+ a b)) param array is [a, b]
            var new_frame = {}; // create new env frame
            var param_vals = stack.pop(); // pop temp frame from stack
            for(var i = 0; i < param_array.length; i++)
            {
                if(param_array[i] === ".") // variadic
                {
                    var array_to_list = function(arr, count)
                    {   if (count === arr.length) return null
                        return cons(arr[count], array_to_list(arr, count+1))
                    }
                    new_frame[param_array[i+1]] = array_to_list(param_vals.slice(i), 0) // convert to list
                    break
                } 
            	new_frame[param_array[i]] = param_vals[i];  // assign value
            }
            delete stack; // delete stack
            closure_env.push(new_frame); // add new frame
            var v = another_interpreter(insts, closure_env, null, stack, acc.start_pc);
            return another_interpreter(insts, env, v, stack, pc+1);
        }
        else if (acc instanceof Array) // vector ([0, 1] 0) => 0
        {
       		var param_vals = stack.pop(); // pop temp frame from stack
       		return another_interpreter(insts, env, acc[param_vals[0]], stack, pc+1);
        }
        else if (acc instanceof Object) // dictionary  ({:a 12} :a) => 12
        {
			var param_vals = stack.pop(); // pop temp frame from stack
       		return another_interpreter(insts, env, acc[param_vals[0]], stack, pc+1);
        }
        else // error
        {
         	console.log("ERROR...not invalid Macro, Procedure, Dictionary, Vector. hahaha");
         	return; 
        }
    }
    else if (op === MAKE_RATIO) // make ratio number
    {
        return another_interpreter(insts, env, make_rat(parseFloat(arg0), parseFloat(arg1)) , stack, pc+1);
    }
    else if (op === TEST)
    {
        // console.log("ACC======")
        // console.log(acc)
        if(acc==null)
            return another_interpreter(insts, env, acc, stack, pc+arg0);
        else return another_interpreter(insts, env, acc, stack, pc+1);
    }
    else if (op === JMP)
    {
        return another_interpreter(insts, env, acc, stack, pc+arg0);
    }
    else
    {
        console.log("Error...invalid instructions opcode " + op);
    }
}


var displayInsts = function(insts)
{
    for(var i = 0; i < insts.length; i++)
    {
        var op = insts[i][0];
        if(op === REF)
            console.log("REF " + insts[i][1] + " " + insts[i][2]);
        if(op === CONSTANT)
            console.log("CONSTANT " + insts[i][1] + " " + insts[i][2]);
        if(op === ASSIGN)
            console.log("ASSIGN " + insts[i][1] + " " + insts[i][2]);
        if(op === RETURN)
            console.log("RETURN " + insts[i][1] + " " + insts[i][2]);
        if(op === CLOSURE)
            console.log("CLOSURE " + insts[i][1] + " " + insts[i][2]);
        if(op === FRAME)
            console.log("FRAME " + insts[i][1] + " " + insts[i][2]);
        if(op === ARGUMENT)
            console.log("ARGUMENT " + insts[i][1] + " " + insts[i][2]);
        if(op === CALL)
            console.log("CALL " + insts[i][1] + " " + insts[i][2]);
        if(op === TEST)
            console.log("TEST " + insts[i][1] + " " + insts[i][2]);
        if(op === JMP)
            console.log("JMP " + insts[i][1] + " " + insts[i][2]);
        if(op === NIL)
            console.log("NIL " + insts[i][1] + " " + insts[i][2]);
        if(op === ADDPARAM)
            console.log("ADDPARAM " + insts[i][1] + " " + insts[i][2]);
        if(op === MAKE_RATIO)
            console.log("MAKE_RATIO " + insts[i][1] + " " + insts[i][2]);
    }
}
var formatList = function(x)
{
    if(x === null) return "()";
    var output = "(";
    while(x!==null)
    {
        var v = car(x);
        if(v.TYPE === LIST)
            output += formatList(v);
        else output += v;
        output += " "
        x = cdr(x);
    }
    output+=")"
    return output;
}

/*
var x = "(+ 3 4)"
var l = lexer(x);
var p = parser(l);
console.log(p);
var o = compile_sequence(p);
console.log(o);
*/

// exports to Nodejs 
if (typeof(module)!="undefined"){
    // module.exports.lexer = lexer;
    // module.exports.parser = parser ;
   	// module.exports.compile_sequence = compile_sequence;
}

var number$ = function(v)
{
    return v instanceof Toy_Number;
}
var number$_ = new Builtin_Primitive_Procedure(function(stack_param)
{
    return number$(stack_param[0]) ? 'true':null;
})
var add_ = new Builtin_Primitive_Procedure(function(stack_param){ 
    var arg0 = stack_param[0]; var arg1 = stack_param[1];
    var arg0_number = arg0 instanceof Toy_Number;
    var arg1_number = arg1 instanceof Toy_Number;
    if(arg0_number && arg1_number)
    {
        if(arg0.TYPE === FLOAT || arg1.TYPE === FLOAT) return arg0.numer/arg0.denom + arg1.numer/arg1.denom;
        return add_rat(arg0, arg1);
    }
    if(arg0_number) arg0 = number_to_string(arg0);
    if(arg1_number) arg1 = number_to_string(arg1);
    return arg0 + arg1;
})
var sub_ = new Builtin_Primitive_Procedure(function(stack_param){ 
    var arg0 = stack_param[0]; var arg1 = stack_param[1];
    var arg0_number = arg0 instanceof Toy_Number;
    var arg1_number = arg1 instanceof Toy_Number;
    if(arg0_number && arg1_number)
    {
        if(arg0.TYPE === FLOAT || arg1.TYPE === FLOAT) return arg0.numer/arg0.denom - arg1.numer/arg1.denom;
        return sub_rat(arg0, arg1);
    }
    if(arg0_number) arg0 = number_to_string(arg0);
    if(arg1_number) arg1 = number_to_string(arg1);
    return arg0 - arg1;
})
var mul_ = new Builtin_Primitive_Procedure(function(stack_param){ 
    var arg0 = stack_param[0]; var arg1 = stack_param[1];
    var arg0_number = arg0 instanceof Toy_Number;
    var arg1_number = arg1 instanceof Toy_Number;
    if(arg0_number && arg1_number)
    {
        if(arg0.TYPE === FLOAT || arg1.TYPE === FLOAT) return (arg0.numer/arg0.denom) * (arg1.numer/arg1.denom);
        return mul_rat(arg0, arg1);
    }
    if(arg0_number) arg0 = number_to_string(arg0);
    if(arg1_number) arg1 = number_to_string(arg1);
    return arg0 * arg1;
})
var div_ = new Builtin_Primitive_Procedure(function(stack_param){ 
    var arg0 = stack_param[0]; var arg1 = stack_param[1];
    var arg0_number = arg0 instanceof Toy_Number;
    var arg1_number = arg1 instanceof Toy_Number;
    if(arg0_number && arg1_number)
    {
        if(arg0.TYPE === FLOAT || arg1.TYPE === FLOAT) return (arg0.numer/arg0.denom) / (arg1.numer/arg1.denom);
        if(arg1.numer === 0){
            console.log("ERROR: Cannot divide by 0")
            return "false"
        }
        return  div_rat(arg0, arg1);
    }
    if(arg0_number) arg0 = number_to_string(arg0);
    if(arg1_number) arg1 = number_to_string(arg1);
    return arg0 / arg1;
})
var ratio$_ = new Builtin_Primitive_Procedure(function(stack_param){ 
  return stack_param[0] instanceof Toy_Number && (stack_param[0].TYPE === RATIO)  ? 'true':null;
})
var float$_ = new Builtin_Primitive_Procedure(function(stack_param){ 
  return stack_param[0] instanceof Toy_Number && (stack_param[0].TYPE === FLOAT)  ? 'true':null;
})
/* get numerator of number */
var numerator_ = new Builtin_Primitive_Procedure(function(stack_param){ 
    if(stack_param[0] instanceof Toy_Number)
        return new Toy_Number(stack_param[0].numer, 1, RATIO);
    else
        console.log("ERROR:Function numerator wrong type parameters")
})
/* get denominator of number */
var denominator_ = new Builtin_Primitive_Procedure(function(stack_param){ 
    if(stack_param[0] instanceof Toy_Number)
        return new Toy_Number(stack_param[0].denom, 1, RATIO);
    else
        console.log("ERROR:Function denominator wrong type parameters")
})

var null_ = new Builtin_Primitive_Procedure(function(stack_param)
{
	return stack_param[0] === null  ? 'true':null;
})
var cons_ = new Builtin_Primitive_Procedure(function(stack_param)
{
	return cons(stack_param[0], stack_param[1]);
})
var car_ = new Builtin_Primitive_Procedure(function(stack_param)
{
	return car(stack_param[0]);
})
var cdr_ = new Builtin_Primitive_Procedure(function(stack_param)
{
	return cdr(stack_param[0]);
})
var keyword_ = new Builtin_Primitive_Procedure(function(stack_param)
{
	return stack_param[0];
})
var vector_ = new Builtin_Primitive_Procedure(function(stack_param)
{
	return stack_param;
})
var dictionary_ = new Builtin_Primitive_Procedure(function(stack_param)
{
	var output = {};
	for(var i = 0; i < stack_param.length; i = i + 2)
	{
		output[stack_param[i]] = stack_param[i+1];
	}
	return output
})
/*
	immutable
	(conj [1,2] 3) => [1,2,3]
	(conj '(1 2) 3) => (3 1 2)
	(conj {:a 1} {:b 2}) => {:a 1 :b 2}
*/
var conj_ = new Builtin_Primitive_Procedure(function(stack_param)
{
	var arg0  = stack_param[0];
	var arg1 = stack_param[1];
	if(arg0.TYPE === LIST)
	{
		return cons(arg1, arg0);
	}
	else if (arg0 instanceof Array)
	{
		var output = arg0.slice(0);
		output.push(arg1);
		return output;
	}
	else if (arg0 instanceof Object)
	{
		var output = Object.create(arg0);
		for(var i in arg1)
		{
			output[i] = arg1[i];
		}
		return output; // the print has error
	}
})
/*
	mutable
*/
var conj_$ = new Builtin_Primitive_Procedure(function(stack_param)
{
	var arg0  = stack_param[0];
	var arg1 = stack_param[1];
	if(arg0.TYPE === LIST)
	{
		return cons(arg1, arg0);
	}
	else if (arg0 instanceof Array)
	{
		arg0.push(arg1);
		return arg0;
	}
	else if (arg0 instanceof Object)
	{
		for(var i in arg1)
		{
			arg0[i] = arg1[i];
		}
		return arg0;
	}
})
/*
	immutable
	(assoc [1,2,3] 0 12) => [12 2 3]
	(assoc {:a 12} :a 13) => {:a 13} 
*/
var assoc_ = new Builtin_Primitive_Procedure(function(stack_param)
{
	var arg0 = stack_param[0];
	var arg1 = stack_param[1];
	var arg2 = stack_param[2];
	if(arg0 instanceof Array)
	{
		var output = arg0.slice(0);
		output[arg1] = arg2;
		return output;
	}
	else if (arg0 instanceof Object)
	{
		var output = Object.create(arg0);
		output[arg1] = arg2;
		return output;
	}
	else
	{
		console.log("ERROR...Function assoc wrong type parameters");
	}
})
/*
	immutable pop
	(pop [1,2]) => [1]
	(pop '(1 2)) => (2)
*/
var pop_ = new Builtin_Primitive_Procedure(function(stack_param)
{
	var arg0 = stack_param[0];
	if(arg0 instanceof Array)
	{
		var output = arg0.slice(0);
		output.pop();
		return output;
	}
	else if (arg0.TYPE === LIST)
	{
		return cdr(arg0);
	}
	else
	{
			console.log("ERROR...Function pop wrong type parameters");	
	}
})
/*
	mutable
*/
var pop_$ = new Builtin_Primitive_Procedure(function(stack_param)
{
	var arg0 = stack_param[0];
	if(arg0 instanceof Array)
	{
		arg0.pop();
		return arg0;
	}
	else
	{
			console.log("ERROR...Function pop wrong type parameters");	
	}
})
/*
	mutable 
*/
var assoc_$ = new Builtin_Primitive_Procedure(function(stack_param)
{
	var arg0 = stack_param[0];
	var arg1 = stack_param[1];
	var arg2 = stack_param[2];
	if(arg0 instanceof Array)
	{
		arg0[arg1] = arg2;
		return arg0;
	}
	else if (arg0 instanceof Object)
	{
		arg0[arg1] = arg2;
		return arg0;
	}
	else
	{
		console.log("ERROR...Function assoc wrong type parameters");
	}
})
var lt_ = new Builtin_Primitive_Procedure(function(stack_param)
{
    var arg0 = stack_param[0]; var arg1 = stack_param[1];
    var arg0_number = arg0 instanceof Toy_Number;
    var arg1_number = arg1 instanceof Toy_Number;
    if (arg0_number) arg0 = arg0.numer/arg0.denom;
    if (arg1_number) arg1 = arg1.numer/arg1.denom;
    return arg0 < arg1  ? 'true':null;
})
var eq_ = new Builtin_Primitive_Procedure(function(stack_param)
{
    var arg0 = stack_param[0]; var arg1 = stack_param[1];
    var arg0_number = arg0 instanceof Toy_Number;
    var arg1_number = arg1 instanceof Toy_Number;
    if (arg0_number) arg0 = arg0.numer/arg0.denom;
    if (arg1_number) arg1 = arg1.numer/arg1.denom;
    return arg0 === arg1  ? 'true':null;
})

var formatNumber = number_to_string;
var formatList = function(l) // format list object to javascript string
{
    if(l === null) // it is null
    {
        return '()';
    }
    else
    {
        var output = "(";
        var p = l; // pointer
        while(1)
        {
            if(l === null) // finish
            {
                output = output + ")";
                break;
            }
            if(!(l instanceof Cons)) // pair
            {
                var c = l;
                output = output + ". ";
                if(number$(c))
                    output = output + formatNumber(c) + ")";
                else if (typeof(c) === "string")
                    output = output + c + ")";
                else if (c  instanceof Cons)
                    output = output + formatList(c) + ")";
                else if (c instanceof Array)
                    output = output + formatVector(c) + ")";
                else if (c instanceof Object)
                    output = output + formatDictionary(c) + ")";
                else if (c.TYPE === PROCEDURE)
                    output = output + "< user-defined-procedure >)" ;
                else if (c.TYPE === BUILTIN_PRIMITIVE_PROCEDURE)
                    output = output + "< builtin-primitive-procedure >)"      ;
                else if (c.TYPE === MACRO)
                    output = output + "< macro >"
                break;
            }
            var c = l.car;
            if(number$(c))
                output = output + formatNumber(c) + " ";
            else if (typeof(c) === "string")
                output = output + c + " ";
            else if (c  instanceof Cons)
                output = output + formatList(c) + " ";
            else if (c instanceof Array)
                output = output + formatVector(c) + " ";
            else if (c instanceof Object)
                output = output + formatDictionary(c) + " ";
            else if (c.TYPE === PROCEDURE)
                output = output + "< user-defined-procedure > " ;
            else if (c.TYPE === BUILTIN_PRIMITIVE_PROCEDURE)
                output = output + "< builtin-procedure > "      ;
            else if (c.TYPE === MACRO)
                output = output + "< macro > "
            l = l.cdr; 
        }
        return output;
    }
}
var formatVector = function(v)
{
    var output = "[";
    var p = v; // pointer
    for(var i = 0; i < p.length; i++)
    {
        var c = p[i];
        if(c === null)
            output = output + "() "
        else if(number$(c))
            output = output + formatNumber(c) + " ";
        else if (typeof(c) === "string")
            output = output + c + " ";
        else if (c instanceof Cons)
            output = output + formatList(c) + " ";
        else if (c instanceof Array)
            output = output + formatVector(c) + " ";
        else if (c instanceof Object)
            output = output + formatDictionary(c) + " ";
        else if (c.TYPE === PROCEDURE)
            output = output + "< user-defined-procedure > " ;
        else if (c.TYPE === BUILTIN_PRIMITIVE_PROCEDURE)
            output = output + "< builtin-procedure > "      ;
        else if (c.TYPE === MACRO)
                output = output + "< macro > "
    }
    output = output + "]"
    return output;
}
var formatDictionary = function(d)
{
    var output = "{";
    var p = d.dict; // pointer
    for(var key in p)
    {
        output = output + key + " "
        var c = p[key];
        if(c === null)
            output = output + "()" + ", ";
        else if(number$(c))
            output = output + formatNumber(c) + ", ";
        else if (typeof(c) === 'string')
            output = output + (c) + ", ";
        else if (c instanceof Cons)
            output = output + formatList(c) + ", ";
        else if (c instanceof Array)
            output = output + formatVector(c) + ",";
        else if (c instanceof Object)
            output = output + formatDictionary(c) + ", ";
        else if (c.TYPE === PROCEDURE)
            output = output + "< user-defined-procedure >, " ;
        else if (c.TYPE === BUILTIN_PRIMITIVE_PROCEDURE)
            output = output + "< builtin-procedure >, "      ;
        else if (c.TYPE === MACRO)
            output = output + "< macro > "
    }
    output = output + "}"
    return output;
}

var display_ = new Builtin_Primitive_Procedure(function(stack_param)
{
    var v = stack_param[0];
    if(v===null)
    {
        console.log("()")
        return "undefined"
    }
    if(number$(v))
    {
        console.log(formatNumber(v));
        return 'undefined'
    }
    else if (typeof(v) === "string")
    {
        console.log((v));
        return ('undefined')
    }
    else if (v instanceof Cons)
    {
        console.log(formatList(v));
        return ('undefined')
    }
    else if (v instanceof Array)
    {
        console.log(formatVector(v));
        return ('undefined')
    }
    else if (v instanceof Object)
    {
        console.log(formatDictionary(v));
        return ('undefined')
    }
    else if (v.TYPE === PROCEDURE)
    {
        console.log("< user-defined-procedure >");
        return ('undefined')
    }
    else if (v.TYPE === BUILTIN_PRIMITIVE_PROCEDURE)
    {
        console.log("< builtin-procedure >")
        return ('undefined')
    }
    else if (c.TYPE === MACRO)
    {
        console.log("< macro >")
        return "undefined"
    }
    else
    {
        console.log("Function display: Invalid Parameters Type");
        return ('undefined')
    }
})
var random_ = new Builtin_Primitive_Procedure(function(stack_param)
{
    return new Toy_Number(Math.rand(), 1, FLOAT);
})
var _to_ratio = new Builtin_Primitive_Procedure(function(stack_param)
{
    var arg = stack_param[0];
    if(number$(arg))
    {
        if(arg.TYPE === RATIO)
        {
            return arg;
        }
        else
        {
            var num = arg.numer;
            var getNumOfNumberAfterDot = function(num){
                num = "" + num
                var i = num.indexOf('.')+1
                return num.length - i;
            }
            var _n = Math.pow(10, getNumOfNumberAfterDot(num));
            return make_rat(num * _n, _n);
        }
    }
    else
    {
        console.log("Function ->ratio --- only support number type data");
        return ('undefined');
    }
})
var dictionary_keys_ = new Builtin_Primitive_Procedure(function(stack_param)
{
    return Object.keys(stack_param[0]);
})
/*
    (ref "string" 0) => "s"
    (ref [1 2] 0) => 1
    (ref {:a 1} :a) => 1

    */
var ref_ = new Builtin_Primitive_Procedure(function(stack_param)
{
    var arg0 = stack_param[0]; var arg1 = stack_param[1];
    if(typeof(arg0) === "string")
        return arg0[arg1];
    if(arg0 instanceof Array || arg0 instanceof Object) return arg0[arg1];
    else
        console.log("ERROR:Function ref wrong type parameters")
})
var to_str_ = new Builtin_Primitive_Procedure(function(stack_param)
{
     // change obj to atom
    var v = stack_param[0];
    if(number$(v))
        return (formatNumber(v));
    else if (typeof(v) === "string" )
        return v;
    else if (v instanceof Cons)
        return (formatList(v));
    else if (v instanceof Array)
        return (formatVector(v));
    else if (v instanceof Object)
        return (formatDictionary(v));
    else if (v.TYPE === PROCEDURE)
        return ("< user-defined-procedure >");
    else if (v.TYPE === BUILTIN_PRIMITIVE_PROCEDURE)
        return ('undefined');
    else
    {
        console.log("Function display: Invalid Parameters Type");
        return new ATOM('undefined');
    }
})
var typeof_ = new Builtin_Primitive_Procedure(function(stack_param)
{
    var v = stack_param[0]
    if (v === null) return "null"
    else if(typeof(v)==="string") return "atom"
    else if (v instanceof Cons) return "list"
    else if (v instanceof Array) return "vector"
    else if (v instanceof Dictionary) return "dictionary"
    else if (v.TYPE === PROCEDURE || v.TYPE === BUILTIN_PRIMITIVE_PROCEDURE) return "procedure"
    else{
        console.log("ERROR: Cannot judge type")
        return "undefined"
    }
})
/*
var SYMBOL_TABLE = [{
    "+":0, "-":1, "*":2, "/":3, "list":4, "vector":5, "dictionary":6, "keyword":7, "cons":8, "car":9, "cdr":10,
    "display":11, 
}, // first layer for primitive builtin procedure
{}]
var ENVIRONMENT = [
    [add_,0,0,0,0,0,0,0,0,0,0,display_],
    []
]*/
var INSTRUCTIONS = [];
var STACK = []
var ENVIRONMENT = 
[{"+":add_, "-":sub_, "*":mul_, "/":div_, "vector":vector_, "dictionary":dictionary_, "keyword":keyword_,
  "cons":cons_, "car":car_, "cdr":cdr_, "display":display_, "true":"true", "false":null, "null?":null_, "conj":conj_, "conj!":conj_$, "assoc":assoc_,
  "assoc!":assoc_$, "pop":pop_, "pop!":pop_$, "<":lt_, "eq?":eq_, "number?":number$_, "ratio?":ratio$_, "float?":float$_, "numerator":numerator_, 
  "denominator":denominator_, "random":random_, "->ratio":_to_ratio, "dictionary-keys":dictionary_keys_, "ref":ref_, "->str":to_str_, "typeof":typeof_
},
 {}]
var ACC = null;
var PC = 0;


// var o = another_compiler_seq(p, SYMBOL_TABLE, INSTRUCTIONS);
/*
var o = another_compiler_seq(p, ENVIRONMENT, INSTRUCTIONS);

displayInsts(INSTRUCTIONS);

var x = another_interpreter(INSTRUCTIONS, ENVIRONMENT, null, STACK, 0)
console.log(x)
*/
var another_eval = function(exp)
{
	var o = another_compiler(exp, ENVIRONMENT, INSTRUCTIONS);	 // compile
	// displayInsts(INSTRUCTIONS); // print instructions 
	var x = another_interpreter(INSTRUCTIONS, ENVIRONMENT, ACC, STACK, PC); // interpret
	PC = INSTRUCTIONS.length; // update pc
    return x
}
var eval_sequence = function(exps)
{
	if(exps==null){
        return ACC;
    }
    else
    {
    	ACC = another_eval(car(exps));
        return eval_sequence(cdr(exps));
    }
}

/*
var x = "(def (add a . b) b) (display (add 3 4 5 (+ 6 2)))"
var l = lexer(x);
var p = parser(l);
eval_sequence(p);
console.log(ENVIRONMENT)
*/

// exports to Nodejs 
if (typeof(module)!="undefined"){
    module.exports.lexer = lexer;
    module.exports.parser = parser ;
    module.exports.eval_sequence = eval_sequence;
    module.exports.display = function(x){display_.func([x])}
}






































