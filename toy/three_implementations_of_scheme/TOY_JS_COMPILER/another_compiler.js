
/*
    interpret directly
*/
var REF = 1; // ref var_name at_frame_index_of_env
var CONSTANT = 2; // constant value
var ASSIGN = 3; // assign var_name at_frame_index_of_env
var RETURN = 4; // RETURN 0 0
var CLOSURE = 5; // CLOSURE return_place
var FRAME = 6; // create new frame
var ARGUMENT = 7; // push argument
var CALL = 8;     // call 
var TEST = 9; // test jmp
var JMP = 10; // jmp jump steps
var NIL = 11;
var lookup_env = function(symbol_table, var_name, instructions)
{
    for(var i = symbol_table.length-1; i>=0; i--)
    {
        if(var_name in symbol_table[i])
        {
            instructions.push([REF, symbol_table[i][var_name], i]);
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
    while(!args.NULL)
    {
        symbol_table[symbol_table.length - 1][car(args)] = Object.keys(symbol_table[symbol_table.length - 1]).length;
        args = cdr(args);
    }
    another_compiler_seq(body, symbol_table, instructions);
    // add return
    instructions.push([RETURN, 0, 0]);
    instructions[index][1] = instructions.length - 1; // set return index
    return;
}
var another_compiler_macro = function(args, body, symbol_table, instructions)
{
    // begin closure
    var index = instructions.length;
    instructions.push([CLOSURE, 0, 1]);

    symbol_table.push({});
    // add args
    while(!args.NULL)
    {
        symbol_table[symbol_table.length - 1][car(args)] = true;
        args = cdr(args);
    }
    another_compiler_seq(body, symbol_table, instructions);
    // add return
    instructions.push([RETURN, 0, 0]);
    instructions[index][1] = instructions.length - 1; // set return index
    return;
}
var another_compiler_applic = function(head, params, symbol_table, instructions)
{
    instructions.push([FRAME, 0, 0]);
    // compile parameters
    while(!params.NULL)
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
    CONSTANT 1
    TEST 3 0
    CONSTANT 2
    JMP 2
    CONSTANT 3

*/
var another_compiler_if = function(test, consequent, alternative, symbol_table, instructions)
{
    var index1 = 0;
    instructions.push([TEST, 0, 0]);
    // compile consequent
    another_compiler(consequent, symbol_table, instructions);

    var index2 = instructions.length; 
    instructions.push([JMP, 0, 0]);
    // compile alternative
    another_compiler(alternative, symbol_table, instructions);
    instructions[index2][1] = instructions.length - index2;
    instructions[index1][1] = index2+1;
    return;
}
var another_compiler_seq = function(exps, symbol_table, instructions)
{
    if(exps.NULL)
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
    if(exp.NULL)
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
        else 
            return cons("cons",
                        cons(v, 
                             cons(compile_list(cdr(exp), symbol_table, instructions), build_nil())))
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
        instructions.push([CONSTANT, parseFloat(exp), 0]);
        return;
    }
    else if(typeof(exp) === 'string') // string
        return lookup_env(symbol_table, exp, instructions);
    else if (exp.NULL)
    {
        instructions.push([NIL, 0, 0]);
        return;
    }
    else // lisp
    {
        var tag = car(exp);
        if(tag === "quote")
        {
            var v = cadr(exp);
            if(v.TYPE === LIST)
            {
                    return another_compiler(compile_list(cadr(exp), symbol_table, instructions), symbol_table, instructions);
            }   
            else
            {
                instructions.push([CONSTANT, v, 0]);
                return;
            }
        }
        else if (tag === "define")
        {
            if(cadr(exp).TYPE === LIST)
                return another_compiler(make_lambda(exp), symbol_table, instructions);
            var var_name = cadr(exp);
            var var_value = caddr(exp);
            another_compiler(var_value, symbol_table, instructions);
            symbol_table[symbol_table.length - 1][var_name] = Object.keys(symbol_table[symbol_table.length - 1]).length; // add var name to symbol table
            instructions.push([ASSIGN, symbol_table[symbol_table.length - 1][var_name], symbol_table.length - 1]) // push instructions
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
                    instructions.push([ASSIGN, symbol_table[i][var_name], i])
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
        else if (tag === "macro") // calculate macro
        {
            return another_compiler_macro(lambda_arguments(exp), lambda_body(exp), symbol_table.slice(0), instructions);
        }
        else if (tag in ENVIRONMENT[0] && ENVIRONMENT[0][tag].TYPE === MACRO)
        {
            var closure_env = ENVIRONMENT[tag].closure_env;
            var start_pc = ENVIRONMENT[tag].start_pc;
            // append params to frame
            var frame = [];
            var params = cdr(exp);
            while(!params.NULL)
            {
                frame.push(car(params));
                params = cdr(params);
            }
            var new_env = closure_env.slice(0);
            new_env.push(frame);

            var compiled_result = another_interpreter(insts, new_env, null, [], start_pc);
            // then compile again
            return another_compiler(compiled_result, symbol_table, instructions);
        }
        else if (tag === "begin")
        {
            return compile_begin(cdr(exp), symbol_table, instructions);
        }
        else
        {
            return another_compiler_applic(application_head(exp), application_args(exp), symbol_table.slice(0), instructions);
        }
    }
}
var another_interpreter = function(insts, env, acc, stack, pc)
{
    if(pc === insts.length)
        return acc;
    var inst = insts[pc];
    var op = inst[0];
    var arg0 = inst[1];
    var arg1 = inst[2];
    if(op === REF)
        return another_interpreter(insts, env, env[arg1][arg0], stack, pc+1);
    else if (op === CONSTANT)
        return another_interpreter(insts, env, arg0, stack, pc+1);
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
        if(arg1 === 1)
            v = new Macro(pc+1, env.slice(0)); // macro
        else 
            v =  new Procedure(pc+1, env.slice(0)); // make closure... temp
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
    else if (op === CALL)
    {
        if(acc.TYPE === BUILTIN_PRIMITIVE_PROCEDURE)
        {
            var v = acc.func(stack.pop());
            return another_interpreter(insts, env, v, stack, pc+1);
        }
        else
        {
            var closure_env = acc.closure_env.slice(0);
            closure_env.push(stack.pop()); // push temp frame
            var v = another_interpreter(insts, closure_env, null, stack, acc.start_pc);
            return another_interpreter(insts, env, v, stack, pc+1);
        }
    }
    else if (op === TEST)
    {
        if(acc == null)
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
    }
}
var formatList = function(x)
{
    if(x.NULL) return "()";
    var output = "(";
    while(!x.NULL)
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
    module.exports.lexer = lexer;
    module.exports.parser = parser ;
   	module.exports.compile_sequence = compile_sequence;
}

var display_ = new Builtin_Primitive_Procedure(function(stack_param){
    console.log(stack_param[0]);
})
var add_ = new Builtin_Primitive_Procedure(function(stack_param){
    return stack_param[0] + stack_param[1];
})


var INSTRUCTIONS = [];
var SYMBOL_TABLE = [{
    "+":0, "-":1, "*":2, "/":3, "list":4, "vector":5, "dictionary":6, "keyword":7, "cons":8, "car":9, "cdr":10,
    "display":11, 
}, // first layer for primitive builtin procedure
{}]
var ENVIRONMENT = [
    [add_,0,0,0,0,0,0,0,0,0,0,display_],
    []
]
var STACK = []


var x = "(define (add a b) (+ a b)) (add 3 4)"
var l = lexer(x);
var p = parser(l);
var o = another_compiler_seq(p, SYMBOL_TABLE, INSTRUCTIONS);
displayInsts(INSTRUCTIONS);

var x = another_interpreter(INSTRUCTIONS, ENVIRONMENT, null, STACK, 0)
console.log(x)










































