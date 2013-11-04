/*
    generate bytecode
    Consist of three parts:
        lexer
        parser
        compiler

    for lexer I will use the following data types:
        List
        Atom
        Number


*/
/* ================================= */
/* tokenize string to list */
var lexer = function(input_str)
{   
    /*
        This Cons is different from Cons in VM
        this Cons only support char* <-> char*
    */
    var Cons = function(car_, cdr_)
    {
        this.car = car;
        this.cdr = cdr;
    }
    var cons = function(car_, cdr_)
    {
        return new Cons(car_, cdr_)
    }
    var find_final_comment_index = function(input_str, i)
    {
        if(i == input_str.length) return i;
        if(input_str[i]=="\n") return i+1;
        else return find_final_comment_index(input_str, i + 1);
    }
    var find_final_string_index = function(input_str, i)
    {
        if(i == input_str.length)
            console.log("ERROR: Incomplete String");
        if(input_str[i]=="\\")
            return find_final_string_index(input_str, i+1);
        if(input_str[i]=="\"")
            return i+1;
    }
    var find_final_number_of_atom_index = function(input_str, i)
    {
        if(i == input_str.length)
            return i;
        if(input_str[i]=="(" || input_str[i]==")"
            || input_str[i]=="[" || input_str[i]=="]"
            || input_str[i]=="{" || input_str[i]=="}"
            || input_str[i]==" " || input_str[i]=="\t"
            || input_str[i]=="\n" || input_str[i]==";")
            return i;
        else
            return find_final_number_of_atom_index(input_str, i+1);
    }
    var lexer_iter = function(input_str, i)
    {
        if(i==input_str.length)
            return null; // finish
        else if(input_str[i]===" " || input_str[i]=="\n" || input_str[i]=="\t") // remove space tab newline
            return lexer_iter(input_str, i + 1);
        else if(input_str[i]==="(")
            return cons( "(", lexer_iter(input_str, i + 1));
        else if(input_str[i]==="[")
            return cons( "(", cons( "vector", lexer_iter(input_str, i + 1)));
        else if(input_str[i]==="{")
            return cons( "(", cons( "dictionary", lexer_iter(input_str, i + 1)));
        else if(input_str[i]===")" || input_str[i]=="]" || input_str[i]=="}")
            return cons( ")", lexer_iter(input_str, i + 1));
        else if(input_str[i]==="'" || input_str[i]=="@" || input_str[i]==",")
            return cons( input_str[i], lexer_iter(input_str, i + 1));
        else if(input_str[i]==='"')
        {
            var end = find_final_string_index(input_str, i+1);
            return cons(cons( "(", cons( "quote", cons(  input_str.slice(i, end), ")"))), 
                        lexer_iter(input_str, end))
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
            return parse_list(parse_list(cdr(l)), rest);
        }
        else if (car(l) === "'" || car(l) === "," || car(l) === "@")  // quote unquote quasiquote
        {
            return cons(parse_special(l), rest);
        }
        else if (car(l) === ".") // pair
        {
            return cons(cadr(l), parse_list(cddr(l)));
        }
        else  // symbol or number
        {
            return cons(parse_symbol_or_number( car(l) ), parse_list(cdr(l)));
        }
    }
    var parse_special = function(l)
    {
        var tag = car(l);
        l = cdr(l);
        if (car(l) === "(") // list
        {
            return cons(tag, parse_list(cdr(l)));
        }
        else if (car(l) === "'" || car(l) === "," || car(l) === "@")  // quote unquote quasiquote
        {   // here my be some errors
            return cons(tag, parse_special(l));
        }
        else  // symbol or number
        {
            rest = cdr(l);
            return cons(tag, parse_symbol_or_number(car(l)));
        }
    }
    var parse_symbol_or_number = function(l)
    {
        if(isNumber(l))
        {
            if(isInteger(l))
            {
                return build_number(l, 1, INTEGER);
            }
            else // float
                return build_number(l, 1, FLOAT);
        }
        else if (isRatio(l))
        {
            return build_number(getNumerator(l), getDenominator(l), RATIO);
        }
        else
            return build_atom(l);
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
    else if (car(l) === "'" || car(l) === "," || car(l) === "@")
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
    construct inst 
*/
var make_inst = function(arg0, arg1, arg2)
{
    var output = new Array(3); 
    output[0] = arg0; output[1] = arg1; output[2] = arg2;
    return output;
}
var Instructions = function()
{
    this.length = 0;
    this.stack = [];
    this.push = function(inst) // push instruction
    {
        this.stack.push(inst);
        this.length = this.length + 1;
    }
    this.pop = function() // pop instruction
    {
        this.stack.pop();
        this.length = this.length - 1;
    }
    this.ref = function(i) // get instruction according to index
    {
        return this.stack[i];
    }
    this.set = function(v, i) // set instruction to index
    {
        this.stack[i] = v; 
    }
}
var make_instructions = function()
{
    return new Instructions();
}
var instructions_push = function(instructions, inst)
{
    instructions.push(inst);
}
var instructions_pop = function(instructions)
{
    instructions.pop();
}
var instructions_ref = function(instructions, i)
{
    return instructions.ref(i);
}
var instructions_set = function(instructions, v, i)
{
    instructions.set(v, i);
}
var instructions_length = function(instructions)
{
    return instructions.length;
}
var instructions_display = function(instructions)
{
    console.log(instructions.stack);
}

/*
    construct symbol_table

    first construct frame
*/
var Frame = function()
{
    this.stack = [];
    this.length = 0;
    this.lookup = function(var_name) /* return index of var_name char* */
    {
        for(var i = 0; i < this.length; i++)
        {
            if(this.stack[i] === var_name)
                return i;
        }
        return -1;
    }
    this.push = function(var_name)
    {
        this.stack.push(var_name);
        this.length += 1;
    }
    this.pop = function(var_name)
    {
        this.stack.pop();
        this.length -= 1;
    }
}
var make_frame = function()
{
    return new Frame();
}
var frame_lookup = function(frame, var_name)
{
    return frame.lookup(var_name);
}
var frame_push = function(frame, v)
{
    frame.push(v);
}
var frame_pop = function(frame)
{
    frame.pop();
}
var frame_length = function(frame)
{
    return frame.length;
}

/* construct symbol table */
var Symbol_Table = function()
{
    this.frames = [];
    this.length = 0;
    this.push = function(frame) /* push frame */
    {
        this.frames.push(frame);
        this.length += 1;
    }
    this.pop = function() /* pop frame */
    {
        this.frames.pop();
        this.length -= 1;
    }
    this.lookup = function(var_name) /* lookup var name */
    {
        for(var i = 0; i < this.length; i++)
        {
            var a = frame_lookup(this.frames[i], var_name);
            if(a != -1)
                return [i, a];
        }
        return [-1, -1];
    }
    this.copy = function() /* return a copy */
    {
        var copy_one = new Symbol_Table();
        copy_one.frames = this.frames.slice(0);
        copy_one.length = this.frames.length;
        return copy_one;
    }
}

var make_symbol_table = function()
{
    return new Symbol_Table();
}
var symbol_table_push = function(st, f)
{
    st.push(f);
}
var symbol_table_pop = function(st)
{
    st.pop();
}
var symbol_table_lookup = function(st, var_name)
{
    return st.lookup(var_name);
}
var symbol_table_length = function(st)
{
    return st.length;
}
var symbol_table_display = function(st)
{
    console.log(st.frames);
}
var symbol_table_copy = function(st)
{
    return st.copy();
}

/*
====================================================================
====================================================================
====================================================================
============  Begin to Define Compiler  ============================
====================================================================
====================================================================
====================================================================
====================================================================
====================================================================
*/
// toy language virtual machine
/*
;;  
;;    My Implementation
;; 注意
;; 虚拟机的compiler没有implement tail call optimization
;; tail call optimization 将会在以后 bootstrap的时候implement
;; 到时候用 toy language 写compiler生成字节码再用虚拟机运行
;; 现在的虚拟机只是core， implement最基本的内容


;;    For the Compiler Machine
;;    exp environment 

;;    For the virtual Machine
;;    accumulator
;;    stack

;;    instructions
;;    const value type; save value in accumulator. type: 0-> atom, 1->integer, 2->float, 3->string
;;    refer n m       ; get value from stack n, m and save to accumulator
;;    assign n m      ; get value from accumulator and save it to stack n m
;;    close index-of-return ; create closure
;;    return          ; end closure
;;    frame           ; create new frame on stack
;;    argument        ; push argument on accumulator to toppest frame of stack
;;    call            ; get procedure from accumulator, pop toppest frame in stack. run function
;;    test jmp_steps  ; get value from accumulator and test, if pass, run next
;;              ; else jump
;;    jmp steps       ; jmp steps
;;    goto pc         ; goto pc; this instruction may replace jmp in the future
;;    ratio numer denom ;; create ratio number
;;
;;
*/

var CONSTANT = 1;
var REFER = 2;
var ASSIGN = 3;
var CLOSE = 4;
var RETURN = 5;
var FRAME = 6;
var ARGUMENT = 7;
var CALL = 8;
var TEST = 9;
var JMP = 10;
var GOTO = 11;
var RATIO = 12;

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
var isRatio = function(n)
{
    var index_of_slash = n.indexOf("/");
    if(index_of_slash === -1) return false;
    var numer = n.slice(0, index_of_slash);
    var denom = n.slice(index_of_slash+1);
    // if(numer.length === 0 || denom.length == 0) return false;
    if(isInteger(numer) && isInteger(denom)) // didn't consider the case denominator is 0
    {
        if(parseInt(denom) === 0)
        {
            error("Invalid ratio --- " + n + " with denominator 0");
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

/* 
    check whether input is pair 
    here the pair is array in js
    */
var pair$ = function(exp)
{
    if(typeof(exp) === 'string' || typeof(exp) === 'number')
        return false;
    return true;
}
/*
    string and not number
*/
var symbol$ = function(exp)
{
    if(typeof(exp) === 'string' && isNumber(exp) === false && isRatio(exp) === false)
    {
        return true;
    }
    else
    {
        return false;
    }
}



/* 
    new compile sequence
    this time parameter is list
*/
var compile_sequence = function(exp, env, instructions)
    {
        if(null$(exp))
            return instructions
        else
        {
            compiler(car(exp), env, instructions);
            return compile_sequence(cdr(exp), env, instructions);
        }
    }

var compiler = function(exp, env, instructions)
{
    if(exp.TYPE === ATOM) // symbol
        return compile_lookup(exp.atom, env, instructions);
    else if (exp.TYPE === LIST) // list
    {
        var tag = car(exp);
        if(tag.TYPE === ATOM)
        {
            if(tag.atom === "quote" || tag.atom === "quasiquote")
            {
                if(pair$(cadr(exp)))
                {
                    if(tag.atom === "quasiquote")
                         return compile_quasiquote(cadr(exp), env, instructions);
                    return compile_list(cadr(exp), env, instructions);
                }
                else if (cadr(exp).TYPE === NUMBER)
                {
                    if(cadr(exp).type === INTEGER)
                         instructions.push([CONSTANT, cadr(exp).numer, 1]);
                    else if (cadr(exp).type === FLOAT)
                         instructions.push([CONSTANT, cadr(exp).numer, 2]);
                    else // ratio
                         instructions.push([RATIO, cadr(exp).numer, cadr(exp).denom]);
                    return;
                }
                else // symbol
                {
                    instructions.push([CONSTANT, cadr(exp), 0]);
                }
            }
            else if (tag.atom === "define")
            {
                if(pair$(cadr(exp)))
                    return compiler(make_lambda(exp), env, instructions);
                return compile_define(definition_variable(exp),
                           definition_value(exp),
                           env,
                           instructions);
            }   
            else if (tag.atom === "set!")
            {
                if(pair$(cadr(exp)))
                    return compiler(make_lambda(exp), env, instructions);
                return compile_set(assignment_variable(exp),
                        assignment_value(exp),
                        env,
                        instructions);
            }
            else if (tag.atom === "if")
            {
                return compile_if(if_test(exp),
                        if_consequent(exp),
                        if_alternative(exp),
                        env,
                        instructions);
            }
            else if (tag.atom === "cond")
            {
                return compiler(cond_to_if(exp), env, instructions);
            }
            else if (tag.atom === "begin")
            {
                return compile_sequence(cdr(exp), env, instructions);
            }
            else if (tag.atom === "lambda")
            {
                return compile_lambda(lambda_arguments(exp),
                            lambda_body(exp),
                            env.slice(0),
                            instructions);
            }
            else // application
            {
                return compile_application(application_head(exp),
                                application_args(exp),
                                env,
                                instructions)
            }
        }
        else // application
        {
            return compile_application(application_head(exp),
                                application_args(exp),
                                env,
                                instructions)
        }
    }
    else // number
    {
        if(exp.type === INTEGER)
        {
            instructions.push([CONSTANT, exp.numer, 1]);
        }
        else if (exp.type === FLOAT)
        {
            instructions.push([CONSTANT, exp.numer, 2]);
        }
        else if (exp.type === RATIO)
        {
            instructions.push([RATIO, exp.numer, exp.denom]);
        }
        else
        {
            error("Invalid term");
        }
        return;
    }
}

var x = "(define x 12)"
var y = lexer(x);
var z = parser(y);
console.log(z);











