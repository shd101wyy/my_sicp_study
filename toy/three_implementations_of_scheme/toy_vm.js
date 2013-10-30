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
var error = function(x)
{
    console.log("ERROR: " + x);
}


/*
    Toy Language JS compiler
*/

/*

	Implement Data Types
*/

/*
    先不考虑 :abc :的形式
    先不考虑 number
*/
// tokenize input string
var Lexer= function(input_str){
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
            input_str[i]=='@'||input_str[i]=="'"||input_str[i]==','){//||
            //input_str[i]==':'){
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
            output.push(input_str.slice(start, i+1))
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

/*
	Parse Token List to Array
*/
var Parser = function(input_array){
    var output = []
    var i = 0

    var formatSpecial = function(input_array, i, output){
   		// quote
   		if(input_array[i]=="'"){
   			output.push('quote')
   		} 
   		// unquote
   		else if (input_array[i]==","){
   			output.push('unquote')
   		}
   		// quasiquote
   		else if (input_array[i]=="@"){
   			output.push('quasiquote')
   		}

   		i++
		if(input_array[i]=="("){
            output.push([])
            i = formatList(input_array, i+1, output[output.length - 1])
            return i
        }
        else if (input_array[i]=="["){
            output.push(['array'])
            i = formatArray(input_array, i+1, output[output.length - 1])
            return i
        }
        else if (input_array[i]=='{'){
        	output.push(['dictionary'])
        	i = formatDictionary(input_array, i+1, output[output.length - 1])
        	return i
        }
        else if (input_array[i]=="'" || input_array[i]=="," || input_array[i]=="@" ){
        	output.push([])
        	i = formatSpecial(input_array, i, output[output.length - 1])
        	return i
        }
        else{
            output.push( formatSymbol ( input_array[i] ))
            return i
        }
    }
    // 12 -> [0,'12','1','int')
    // if its type is not Number
    // return itself
    var formatSymbol = function(input_str){
        // self->
        if (input_str[0]==":"){
            var output = []
            output.push('quote')
            output.push(input_str)
            return output
        }
	  	return input_str
	}

    var formatList = function(input_array, i, output){
        while(i<input_array.length){
            if(input_array[i]=="("){
                output.push([])
                i = formatList(input_array, i+1, output[output.length - 1])
            }
            else if(input_array[i]==")"){
                return i
            }
            else if (input_array[i]=="["){
                output.push(['array'])
                i = formatArray(input_array, i+1, output[output.length - 1])
            }
            else if (input_array[i]=='{'){
	        	output.push(['dictionary'])
	        	i = formatDictionary(input_array, i+1, output[output.length - 1])
	        }
	        else if (input_array[i]=="'" || input_array[i]=="," || input_array[i]=="@"){
	        	output.push([])
	        	i = formatSpecial(input_array, i, output[output.length - 1])
	        }
            else{
                output.push( formatSymbol ( input_array[i] ))
            }
            i++
        }
    }
    while(i<input_array.length){
        if(input_array[i]=="("){
            output.push([])
            i = formatList(input_array, i+1, output[output.length - 1])
        }
        else if (input_array[i]=="["){
            output.push(['array'])
            i = formatArray(input_array, i+1, output[output.length - 1])
        }
        else if (input_array[i]=='{'){
        	output.push(['dictionary'])
        	i = formatDictionary(input_array, i+1, output[output.length - 1])
        }
        else if (input_array[i]=="'" || input_array[i]=="," || input_array[i]=="@"){
        	output.push([])
        	i = formatSpecial(input_array, i, output[output.length - 1])
	    }
        else{
            output.push( formatSymbol( input_array[i] ))
        }
        i++
    }
    return output
}



/*
    Instructions
    Instructions in js is just array
    in format of 
    [[const 12 0]
     [assign 0 2]]
*/
/*
    ENV (symbol_table)
    [ 
        [a,b,c] // global frame
        [x,y,z] // local frame
    ]
*/
/*
    return index of var_name in frame
    where frame is [a,b,c]
          var_name is b
    if didnt find, return -1
*/
var lookup_frame = function(var_name, frame)
{
    for(var i = 0; i < frame.length; i++){
        if(frame[i] === var_name)
            return i;
    }
    return -1;
}
// find var in env
// return n, and m
var lookup_env = function(var_name, env){
    var i;
    for(i = env.length-1; i>=0; i--)
    {
        var j = lookup_frame(var_name, env[i]);
        if(j==-1) // didnt find var in current frame
            continue;
        else
            return [i, j]; // find var
    }
    
    return [-1, -1];
}
/*
    compile lookup
    find var from env
    where env is like
    [
        [a,b,c]   // global frame
        [x,y,z]   // local frame
     ]
     find from local frame
*/
var compile_lookup = function(var_name, env, instructions)
{
    var n_m = lookup_env(var_name, env);
    if(n_m[0] === -1) // didn't find var
    {
        // add var inside env
        // that var is free variable
        env[env.length - 1].push(var_name);
        instructions.push([REFER, env.length-1, env[env.length-1].length-1]);
        return;
    } // find var
    else{
        instructions.push([REFER, n_m[0], n_m[1]]);
        return;
    }
}

/*
    compile definition
    (define x 12)
*/  
var definition_variable = function(exp)
{
    return exp[1];
}
var definition_value = function(exp)
{
    return exp[2];
}
var compile_define = function(var_name, var_value, env, instructions)
{
    var var_name_index = lookup_frame(var_name, env[env.length-1]);
    if(var_name_index == -1) // didn't find var
    {
        // add var to env
        env[env.length - 1].push(var_name);
        // compile var_value
        Compiler(var_value, env, instructions);
        // assign value
        instructions.push([ASSIGN, env.length - 1, env[env.length - 1].length - 1]);
    }
    else // find var
    {
        // compile var_value
        Compiler(var_value, env, instructions);
        // assign value
        instructions.push([ASSIGN, env.length - 1, var_name_index]);
    }
}

/*
    compile assignment
    (set! x 12)
*/
var assignment_variable = function(exp)
{
    return exp[1];
}
var assignment_value = function(exp)
{
    return exp[2];
}
var compile_set = function(var_name, var_value, env, instructions)
{
    // compile var_value
    Compiler(var_value, env, instructions);
    var n_m = lookup_env(var_name, env);
    if(n_m[0] === -1)
    {
        console.log("ERROR: cannot find var " + var_name);
        return;
    }
    else
    {
        instructions.push([ASSIGN, n_m[0], n_m[1]]);
        return;
    }
}

/*
    compile if
    (if 1 2 3)
    ;; const 1
    ;; test 3
    ;; const 2
    ;; jmp 2
    ;; const 3
    ;;
*/
var if_test = function(exp)
{
    return exp[1];
}
var if_consequent = function(exp)
{
    return exp[2];
}
var if_alternative = function(exp)
{
    // (if 1 2)
    if(exp.length === 3)
        return ['quote', 'undefined'] // return undefined
    return exp[3];
}

var compile_if = function(test, consequent, alternative, env, instructions)
{
    // compile test
    Compiler(test, env, instructions);
    var index1 = instructions.length ; // save current index
    instructions.push([TEST, 0, 0]);  // add test inst
    Compiler(consequent, env, instructions); // compile consequent
    // change test 2nd argument
    instructions[index1][1] = instructions.length - index1 + 1;
    var index_of_jmp = instructions.length;
    // add jmp
    instructions.push([JMP, 0, 0]);
    var index3 = instructions.length ;
    Compiler(alternative, env, instructions); // compile alternative
    // change jmp 2nd argument      
    instructions[index_of_jmp][1] = instructions.length - index3 + 1;
}
/*
    (cond ((judge1) (body1))
          ((judge2) (body2))
          ...
          (else bodyn)
        )
        (define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
*/
var make_if = function(predicate, consequent, alternative)
{
    return ['if', predicate, consequent, alternative];
}
var cond_clauses = function(exp){return exp.slice(1);}
var cond_else_clause$ = function(clause){return clause[0] === 'else';}
var cond_predicate = function(clause){return clause[0]}
var cond_actions = function(clause){return clause.slice(1)}
var cond_to_if = function(exp){return expand_clauses(cond_clauses(exp))}
var make_begin = function(exp){ // ((define x 12)(set! x 13)) => (begin (define x 12) (set! x 13))
    var output = ['begin'];
    for(var i = 0; i < exp.length; i++){output.push(exp[i])}
    return output;
}
var expand_clauses = function(clauses)
{
    var expand_clause_to_if = function(clauses, output, i){
        if(i === clauses.length)
            return output;
        if(clauses[i][0]==='else')
        {
            if(i!==clauses.length - 1)
            {
                error("cond->if error. Else is not the last statement")
                return;
            }
            var seq = clauses[i].slice(1);
            output.push('begin');
            for(var i = 0; i < seq.length; i++ ){output.push(seq[i])}
            return output;
        } 
        output.push('if');
        output.push(cond_predicate(clauses[i]));
        output.push(make_begin(cond_actions(clauses[i])));
        var consequent = [];
        output.push(consequent);
        return expand_clause_to_if(clauses, consequent, i+1);
    }
    var output = [];
    var a = expand_clause_to_if(clauses, output, 0);
    if(a.length == 0){
        a.push('quote'); a.push([]);
    }
    console.log(output);
    return output;
}
/*
    compile lambda
    (lambda (a b) (+ a b))
*/
var lambda_arguments = function(exp)
{
    return exp[1];
}
var lambda_body  = function(exp)
{
    return exp.slice(2);
}
// the env here is a copy of env
var compile_lambda = function(args, body, env, instructions)
{
    // save current index, which is the index of (close arg1)
    var index = instructions.length;
    // add close
    instructions.push([CLOSE, 0, 0]); 
    // extend symbol_table
    env.push(args);
    // compile body
    compile_sequence(body, env, instructions);
    // add return
    instructions.push([RETURN, 0, 0]);
    // set close 2nd arg to index of return
    instructions[index][1] = instructions.length - 1; 
}
/*
    (define (add a b) (+ a b))
    =>
    (define add (lambda (a b) (+ a b)))
*/
var make_lambda = function(exp)
{
    var lambda_body = ['lambda'];
    lambda_body.push(exp[1].slice(1));
    var body = exp.slice(2);
    for(var i = 0; i < body.length; i++)
    {
        lambda_body.push(body[i]);
    }   
    return [exp[0], exp[1][0], lambda_body];
    
}

/*
    compile application
*/
var application_head = function(exp)
{
    return exp[0];
}
var application_args = function(exp)
{
    return exp.slice(1);
}
/*
;; compile args
;; (add 3 4) => (3 4)
;; const 3 
;; argument
;; const 4 
;; argument
*/

var compile_args = function(args, env, instructions)
{
    var count = 0;
    for(var i = 0; i < args.length; i++)
    {
        Compiler(args[i], env, instructions);
        instructions.push([ARGUMENT, 0, 0]);
        count = count + 1;
    }
    return count;
}
var compile_application = function(applic, args, env, instructions)
{
    // add new frame
    instructions.push([FRAME, 0, 0]);
    // compile arguments
    compile_args(args, env, instructions);
    // compile applic
    Compiler(applic, env, instructions);
    // call funciton
    instructions.push([CALL, 0, 0]);
}
/*
    Compile Sequence
    [[define x 1],[set! x 12]]
*/
var compile_sequence = function(seq, env, instructions)
{
    for(var i = 0; i < seq.length; i++)
    {
        Compiler(seq[i], env, instructions);
    }
    return instructions;
}
/*
    Compile list
    without calculation
*/
var compile_list = function(exp, env, instructions)
{
    instructions.push([FRAME, 0, 0]);
    for(var i = 0; i < exp.length; i++)
    {
        if(pair$(exp[i]))
        {
            compile_list(exp[i], env, instructions);
            instructions.push([ARGUMENT, 0, 0]);
        }
        else if (isNumber(exp[i])) // number
        {
            if(isInteger(exp[i]))
            {
                instructions.push([CONSTANT, exp[i], 1])
                instructions.push([ARGUMENT, 0, 0]);
            }
            else
            {
                instructions.push([CONSTANT, exp[i], 2])
                instructions.push([ARGUMENT, 0, 0]);
            }
        }
        else
        {
            instructions.push([CONSTANT, exp[i], 0]);
            instructions.push([ARGUMENT, 0, 0]);
        }
    }
    instructions.push([REFER, 0, 15]); // refer list procedure
    instructions.push([CALL, 0, 0]); // call procedure
}
/*
    Compile list
    with calculation
*/
var compile_quasiquote_list = function(exp, env, instructions)
{
    instructions.push([FRAME, 0, 0]);
    for(var i = 0; i < exp.length; i++)
    {
        if(pair$(exp[i]))
        {
            if(exp[i][0] === 'unquote') // calculate
            {
                Compiler(exp[i][1], env, instructions);
                instructions.push([ARGUMENT, 0, 0])
            }
            else
            {
                compile_list(exp[i], env, instructions);
                instructions.push([ARGUMENT, 0, 0]);
            }
        }
        else if (isNumber(exp[i])) // number
        {
            if(isInteger(exp[i]))
            {
                instructions.push([CONSTANT, exp[i], 1])
                instructions.push([ARGUMENT, 0, 0]);
            }
            else
            {
                instructions.push([CONSTANT, exp[i], 2])
                instructions.push([ARGUMENT, 0, 0]);
            }
        }
        else
        {
            instructions.push([CONSTANT, exp[i], 0]);
            instructions.push([ARGUMENT, 0, 0]);
        }
    }
    instructions.push([REFER, 0, 15]); // refer list procedure
    instructions.push([CALL, 0, 0]); // call procedure
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
var isInteger = function(n){ return n==="0" || /^[1-9][0-9]*$/.test(n) }
var isFloat = function(n){return isNumber(n) && !(isInteger(n))}
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
    if(typeof(exp) === 'string' && isNumber(exp) == false)
    {
        return true;
    }
    else
    {
        return false;
    }
}
/*
    Simple Compiler
*/
var Compiler = function(exp, env, instructions)
{
    if(symbol$(exp))  // it is symbol, so look for its value
        return compile_lookup(exp, env, instructions);
    else if (pair$(exp)) // array
    {
        var tag = exp[0];
        if(tag === "quote" || tag ==='quasiquote')
        {
            if(pair$(exp[1])) // list
            {
                if(tag === 'quasiquote')
                    return compile_quasiquote_list(exp[1], env, instructions);
                return compile_list(exp[1], env, instructions);
            }
            else if (isNumber(exp[1])) // number
            {
                if(isInteger(exp[1]))
                    instructions.push([CONSTANT, exp[1], 1])
                else
                    instructions.push([CONSTANT, exp[1], 2])
                return;
            }
            else if (exp[1][0] === '"') // string
            {
                instructions.push([CONSTANT, exp[1], 3]);
                return;
            }
            else  // symbol
            {
                instructions.push([CONSTANT, exp[1], 0]);
                return;
            }
        }
        else if (tag === 'define')
        {
            /*
                (define (add a b) (+ a b))
            */
            if(pair$(exp[1]))
            {
                return Compiler(make_lambda(exp), env, instructions)
            }
            return compile_define(definition_variable(exp),
                           definition_value(exp),
                           env,
                           instructions);
        }
        else if (tag === 'set!')
        {
            if(pair$(exp[1]))
            {
                return Compiler(make_lambda(exp), env, instructions)
            }
            return compile_set(assignment_variable(exp),
                        assignment_value(exp),
                        env,
                        instructions);
        }
        else if (tag === 'if')
        {
            return compile_if(if_test(exp),
                        if_consequent(exp),
                        if_alternative(exp),
                        env,
                        instructions);
        }
        else if (tag === 'cond')
        {
            return Compiler(cond_to_if(exp), env, instructions);
        }
        else if (tag === 'begin')
        {
            return compile_sequence(exp.slice(1), env, instructions);
        }
        else if (tag === 'lambda')
        {
            return compile_lambda(lambda_arguments(exp),
                            lambda_body(exp),
                            env.slice(0),
                            instructions);
        }
        else{
            return compile_application(application_head(exp),
                                application_args(exp),
                                env,
                                instructions)
        }
    }
    else // constant
    {
        if(isInteger(exp))
            instructions.push([CONSTANT, exp, 1])
        else
            instructions.push([CONSTANT, exp, 2])
        return;
    }
}





/*
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================

    Virtual Machien

=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================
=================================================================

THe Part Below is the Virtual Machine Part
including:
    Data Type Building
    Virtual Machine Design

*/
/*
;;    instructions
;;    const value     ; save value in accumulator
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
;;
;;

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
*/
/*
    make closure data type
*/
var NUMBER = 1;
var LIST = 2;
var VECTOR = 3;
var ATOM = 4;
var NIL = 5;
var DICTIONARY = 6;
var INTEGER = 7;
var FLOAT = 8;
var CLOSURE = 9;  // data type 
var BUILTIN_PROCEDURE = 10;

// build atom data type
var Atom = function(atom)
{
    this.atom = atom;

    this.NULL = false   // for virtual machine check
    this.TYPE = ATOM  // for virtual machien check
}
var build_atom = function(atom)
{
    return new Atom(atom);
}
// build number data type
// build integer
var Integer = function(num)
{
    this.num = num;
    this.type = INTEGER;

    this.NULL = false;
    this.TYPE = NUMBER;
}
// build float
var Float = function(num)
{
    this.num = num;
    this.type = FLOAT;

    this.NULL = false;
    this.TYPE = NUMBER;
}
var build_number = function(num, type)
{
    if(type===INTEGER)
        return new Integer(num)
    return new Float(num);
}

// build true data type
var build_true = function()
{
    return build_number(1, NUMBER);
}
// build false data type
var build_false = function()
{
    return build_nil();
}

// build nil
var Nil = function()
{
 
    this.NULL = true   // for virtual machine check
    this.TYPE = LIST  // for virtual machien check

}
var build_nil = function()
{
   return new Nil();
}

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

    this.NULL = false   // for virtual machine check
    this.TYPE = LIST  // for virtual machien check
}
var cons = function(x, y)
{
    return new Cons(x,y);
}
var car = function(obj)
{
    return obj['car'];
}
var cdr = function(obj)
{
    return obj['cdr'];
}
var set_car = function(x, value)
{
    x.set_car(value);
}
var set_cdr = function(x, value)
{
    x.set_cdr(value);
}
// (list 1 2) => '(1 2)
var build_list = function(stack_param)
{
    var list_iter = function(stack_param, count)
    {
        if(count === stack_param.length)
        {
            return build_nil();
        }
        else
        {
            return cons(stack_param[count], 
                        list_iter(stack_param, count + 1));
        }
    }
    return list_iter(stack_param, 0);
}

// build vector
var Vector = function(stack_param)
{
   this.vector = stack_param;
   this.ref = function(index)
   {
    return this.vector[index];
   }
   this.set = function(index, value)
   {
    this.vector[index] = value;
   }
   this.push = function(value)
   {
    this.vector.push(value);
   }
   this.pop = function()
   {
    this.vector.pop();
   }

    this.NULL = false   // for virtual machine check
    this.TYPE = VECTOR  // for virtual machien check
}
var build_vector = function(stack_param)
{
    return new Vector(stack_param)
}

// build dictionary
var Dictionary = function(stack_param)
{
    this.dict = {};
    /*
        init dictionary
    */
    for(var i = 0 ; i < stack_param.length; i = i + 2)
    {
        var key_obj = stack_param[i];
        var value_obj = stack_param[i+1];
        if(key_obj.TYPE !== ATOM)
        {
            error("Invalid Key");
            break;
        }
        this.dict[key_obj.atom] = value_obj;
    }
    /*
        set value according to key
    */
    this.set = function(key_string, value_obj)
    {
        this.dict[key_string] = value_obj
        return this.dict;
    }  
    /*
        get value according to key
    */
    this.ref = function(key_string) 
    {
        return this.dict[key_string];
    }
    /*
        return keys as vector
    */
    this.keys = function()
    {
        return build_vector(Object.keys(this.dict));
    }


    this.NULL = false   // for virtual machine check
    this.TYPE = DICTIONARY  // for virtual machien check 
}
var build_dictionary = function(stack_param)
{
    return new Dictionary(stack_param);
}
/*
    Build Closure Data Type
*/
var Closure = function(start_pc, environment)
{
    this.start_pc = start_pc;
    this.environment = environment.slice(0);

    this.NULL = false   // for virtual machine check
    this.TYPE = CLOSURE  // for virtual machien check 
}

var make_closure = function(start_pc, environment) // start-pc is where instruction starts
{
    return new Closure(start_pc, environment)
}
var closure_start_pc = function(closure)
{
    return closure.start_pc;
}
var closure_environment = function(closure)
{
    return closure.environment;
}
var closure_environment_extend = function(base_env, extend_env)
{
    base_env.push(extend_env);
    return base_env;
}

/*
    Build Builtin Procedure Data Type
*/
var Builtin_Procedure = function(func)
{
    this.func = func;

    this.TYPE = BUILTIN_PROCEDURE;
    this.NULL = false;
}
var primitive_func = function(builtin_proc)
{
    return builtin_proc.func;
}
var build_builtin_procedure = function(func)
{
    return new Builtin_Procedure(func);
}


/*
    Primitive Builtin Procedure
*/
var checkParam = function(stack_param, required_param_num)
{
    if(stack_param.length!==required_param_num)
    {
        if(stack_param.length > required_param_num)
        {
            error("Too Many Parameters Provided");
        }
        else{
            error("Too Few Parameters Provided");
        }
    }
}
var _car = function(stack_param)
{
    checkParam(stack_param, 1);
    var arg = stack_param[0]
    if(arg.TYPE !== LIST)
    {
        error("Function car: invalid type of param " );
        return build_false();
    }
    return car(arg);  
}
var _cdr = function(stack_param)
{
    checkParam(stack_param, 1);
    var arg = stack_param[0]
    if(arg.TYPE !== LIST)
    {
        error("Function cdr: invalid type of param " );
        return build_false();
    }
    return cdr(arg);  
}
var _set_car = function(stack_param)
{
    checkParam(stack_param, 2);
    var arg0 = stack_param[0];
    var arg1 = stack_param[1];
    if(arg0.TYPE !== LIST)
    {
        error("Function set-car!: invalid type of param " );
        return build_false();
    }
    set_car(arg0, arg1);
    return arg0
}
var _set_cdr = function(stack_param)
{
    checkParam(stack_param, 2);
    var arg0 = stack_param[0];
    var arg1 = stack_param[1];
    if(arg0.TYPE !== LIST)
    {
        error("Function set-cdr!: invalid type of param " );
        return build_false();
    }
    set_cdr(arg0, arg1);
    return arg0
}
var _cons = function(stack_param)
{
    checkParam(stack_param, 2);
    var arg0 = stack_param[0];
    var arg1 = stack_param[1];
    if(arg0.TYPE !== LIST)
    {
        error("Function cons: invalid type of param " );
        return build_false();
    }
    return cons(arg0, arg1);
}
var _closure$ = function(stack_param)
{
    checkParam(stack_param, 1);
    var v = stack_param[0];
    return v.TYPE === CLOSURE ? build_true() : build_false();
}
var _vector$ = function(stack_param)
{
    checkParam(stack_param, 1);
    var v = stack_param[0];
    return v.TYPE === VECTOR ? build_true() : build_false();
}
var _dictionary$ = function(stack_param)
{
    checkParam(stack_param, 1);
    var v = stack_param[0];
    return v.TYPE === DICTIONARY ? build_true() : build_false();
}
var _number$ = function(stack_param)
{
    checkParam(stack_param, 1);
    var v = stack_param[0];
    return v.TYPE === NUMBER ? build_true() : build_false();
}
var _pair$ = function(stack_param)
{
    checkParam(stack_param, 1);
    var v = stack_param[0];
    return v.TYPE === LIST ? build_true() : build_false();
}
var _atom$ = function(stack_param)
{
    checkParam(stack_param, 1);
    var v = stack_param[0];
    return v.TYPE === ATOM ? build_true() : build_false();
}
var _builtin_procedure$ = function(stack_param)
{
    checkParam(stack_param, 1);
    var v = stack_param[0];
    return v.TYPE === BUILTIN_PROCEDURE ? build_true() : build_false();
}
/*
    stdout
*/
/*
    format data type to javascript string
*/
var formatNumber = function(n)
{
    if(n.type === INTEGER)
        return n.num
    return n.num.toFixed(6);
}
var formatAtom = function(a)
{
    return a.atom;
}
var formatList = function(l) // format list object to javascript string
{
    if(l.NULL) // it is null
    {
        return '()';
    }
    else
    {
        var output = "(";
        var p = l; // pointer
        while(1)
        {
            if(l.NULL) // finish
            {
                output = output + ")";
                break;
            }
            if(l.TYPE !== LIST) // pair
            {
                var c = l;
                output = output + ". ";
                if(c.TYPE === NUMBER)
                    output = output + formatNumber(c) + ")";
                else if (c.TYPE === ATOM)
                    output = output + formatAtom(c) + ")";
                else if (c.TYPE === LIST)
                    output = output + formatList(c) + ")";
                else if (c.TYPE === VECTOR)
                    output = output + formatVector(c) + ")";
                else if (c.TYPE === DICTIONARY)
                    output = output + formatDictionary(c) + ")";
                else if (c.TYPE === CLOSURE)
                    output = output + "< user-defined-procedure >)" ;
                else if (c.TYPE === BUILTIN_PROCEDURE)
                    output = output + "< builtin-procedure >)"      ;
                break;
            }
            var c = l.car;
            if(c.TYPE === NUMBER)
                output = output + formatNumber(c) + " ";
            else if (c.TYPE === ATOM)
                output = output + formatAtom(c) + " ";
            else if (c.TYPE === LIST)
                output = output + formatList(c) + " ";
            else if (c.TYPE === VECTOR)
                output = output + formatVector(c) + " ";
            else if (c.TYPE === DICTIONARY)
                output = output + formatDictionary(c) + " ";
            else if (c.TYPE === CLOSURE)
                output = output + "< user-defined-procedure > " ;
            else if (c.TYPE === BUILTIN_PROCEDURE)
                output = output + "< builtin-procedure > "      ;
            l = l.cdr; 
        }
        return output;
    }
}
var formatVector = function(v)
{
    var output = "[";
    var p = v.vector; // pointer
    for(var i = 0; i < p.length; i++)
    {
        var c = p[i];
        if(c.TYPE === NUMBER)
            output = output + formatNumber(c) + " ";
        else if (c.TYPE === ATOM)
            output = output + formatAtom(c) + " ";
        else if (c.TYPE === LIST)
            output = output + formatList(c) + " ";
        else if (c.TYPE === VECTOR)
            output = output + formatVector(c) + " ";
        else if (c.TYPE === DICTIONARY)
            output = output + formatDictionary(c) + " ";
        else if (c.TYPE === CLOSURE)
            output = output + "< user-defined-procedure > " ;
        else if (c.TYPE === BUILTIN_PROCEDURE)
            output = output + "< builtin-procedure > "      ;
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
        if(c.TYPE === NUMBER)
            output = output + formatNumber(c) + ", ";
        else if (c.TYPE === ATOM)
            output = output + formatAtom(c) + ", ";
        else if (c.TYPE === LIST)
            output = output + formatList(c) + ", ";
        else if (c.TYPE === VECTOR)
            output = output + formatVector(c) + ",";
        else if (c.TYPE === DICTIONARY)
            output = output + formatDictionary(c) + ", ";
        else if (c.TYPE === CLOSURE)
            output = output + "< user-defined-procedure >, " ;
        else if (c.TYPE === BUILTIN_PROCEDURE)
            output = output + "< builtin-procedure >, "      ;
    }
    output = output + "}"
    return output;
}
var _display = function(stack_param)
{
    checkParam(stack_param, 1);
    var v = stack_param[0];
    if(v.TYPE === NUMBER)
    {
        console.log(formatNumber(v));
        return build_atom('undefined')
    }
    else if (v.TYPE === ATOM)
    {
        console.log(formatAtom(v));
        return build_atom('undefined')
    }
    else if (v.TYPE === LIST)
    {
        console.log(formatList(v));
        return build_atom('undefined')
    }
    else if (v.TYPE === VECTOR)
    {
        console.log(formatVector(v));
        return build_atom('undefined')
    }
    else if (v.TYPE === DICTIONARY)
    {
        console.log(formatDictionary(v));
        return build_atom('undefined')
    }
    else if (v.TYPE === CLOSURE)
    {
        console.log("< user-defined-procedure >");
        return build_atom('undefined')
    }
    else if (v.TYPE === BUILTIN_PROCEDURE)
    {
        console.log("< builtin-procedure >")
        return build_atom('undefined')
    }
    else
    {
        error("Function display: Invalid Parameters Type");
        return build_atom('undefined')
    }
}
var _str = function(stack_param)
{
    // change obj to atom
    checkParam(stack_param, 1);
    var v = stack_param[0];
    if(v.TYPE === NUMBER)
        return build_atom(formatNumber(v));
    else if (v.TYPE === ATOM)
        return build_atom(formatAtom(v));
    else if (v.TYPE === LIST)
        return build_atom(formatList(v));
    else if (v.TYPE === VECTOR)
        return build_atom(formatVector(v));
    else if (v.TYPE === DICTIONARY)
        return build_atom(formatDictionary(v));
    else if (v.TYPE === CLOSURE)
        return build_atom("< user-defined-procedure >");
    else if (v.TYPE === BUILTIN_PROCEDURE)
        return build_atom('undefined');
    else
    {
        error("Function display: Invalid Parameters Type");
        return new ATOM('undefined');
    }
}
var _atom_ref = function(stack_param)
{
    checkParam(stack_param, 2);
    var arg0 = stack_param[0];
    var arg1 = stack_param[1];
    if(arg0.TYPE!==ATOM)
    {
        error("Function atom-ref: Invalid Parameters Type");
    }
    if(arg1.TYPE!==INTEGER)
    {
        error("Function atom-ref: Invalid Parameters Type; Refer index should be integer");
    }
    return build_atom(arg0.atom[arg1.num]);
}

var _dictionary = function(stack_param)
{
    return build_dictionary(stack_param);
}
var _vector = function(stack_param)
{
    return build_vector(stack_param);
}
var _push = function(stack_param) // vector push
{
    checkParam(stack_param, 2);
    var arg0 = stack_param[0];
    var arg1 = stack_param[1];
    arg0.push(arg1);
    return arg0;
}
var _pop = function(stack_param)  // vector pop
{
    checkParam(stack_param, 1);
    var arg = stack_param[0];
    return arg.pop();
}
var _list = function(stack_param)
{
    return build_list(stack_param);
}

var _eq$ = function(stack_param)
{
    checkParam(stack_param, 2);
    var arg0 = stack_param[0];
    var arg1 = stack_param[1];
    if(arg0.TYPE !== arg1.TYPE)
    {
        error("Function eq?: invalid parameters type")
        return build_false();
    }
    if(arg0.NULL && arg1.NULL) // empty list comparison
    {
        return build_true();
    }
    if(arg0.TYPE === LIST ||
        arg0.TYPE === VECTOR ||
        arg0.TYPE === DICTIONARY ||
        arg0.TYPE === CLOSURE ||
        arg0.TYPE === BUILTIN_PROCEDURE)
    {
        return arg0 === arg1 ? build_true() : build_false();
    }
    else if (arg0.TYPE === NUMBER)
    {
        return arg0.num === arg1.num ? build_true() : build_false();
    }
    else if (arg0.TYPE === ATOM)
    {
        return arg0.atom === arg1.atom ? build_true() : build_false();
    }
}
var _integer$ = function(stack_param)
{
    checkParam(stack_param, 1);
    var arg0 = stack_param[0];
    if(arg0.TYPE === NUMBER && arg0.type === INTEGER){
        return build_true();
    }
    return build_false();
}
var _float$ = function(stack_param)
{
    checkParam(stack_param, 1);
    var arg0 = stack_param[0];
    if(arg0.TYPE === NUMBER && arg0.type === FLOAT)
        return build_true();
    return build_false();
}

var _null$ = function(stack_param)
{
    checkParam(stack_param, 1);
    var arg0 = stack_param[0];
    if(arg0.TYPE === LIST && arg0.NULL === true)
        return build_true();
    return build_false();
}

var _add = function(stack_param)
{
    checkParam(stack_param, 2);
    var arg0 = stack_param[0];
    var arg1 = stack_param[1];
    if(arg0.TYPE === NUMBER && arg1.TYPE === NUMBER)
    {
        var result = arg0.num + arg1.num
        if(arg0.type===FLOAT || arg1.type === FLOAT)
        {
            return build_number(result, FLOAT);
        }
        return build_number(result, INTEGER);
    }
    else if (arg0.TYPE === ATOM && arg1.TYPE === ATOM)
    {
        return build_atom(arg0.atom + arg1.atom); // concat string
    }
    else
    {
        error("Function + only supports number + number and atom + atom now");
        return build_false();
    }
}
var _sub = function(stack_param)
{
    checkParam(stack_param, 2);
    var arg0 = stack_param[0];
    var arg1 = stack_param[1];
    if(arg0.TYPE!==NUMBER || arg1.TYPE!==NUMBER)
    {
        error("Function - only supports numbers now");
        return build_false();
    }
    else{
        var result = arg0.num - arg1.num
        if(arg0.type===FLOAT || arg1.type === FLOAT)
        {
            return build_number(result, FLOAT);
        }
        return build_number(result, INTEGER);
    }
}
var _mul = function(stack_param)
{
    checkParam(stack_param, 2);
    var arg0 = stack_param[0];
    var arg1 = stack_param[1];
    if(arg0.TYPE!==NUMBER || arg1.TYPE!==NUMBER)
    {
        error("Function * only supports numbers now");
        return build_false();
    }
    else{
        var result = arg0.num * arg1.num
        if(arg0.type===FLOAT || arg1.type === FLOAT)
        {
            return build_number(result, FLOAT);
        }
        return build_number(result, INTEGER);
    }
}
var _div = function(stack_param)
{
    checkParam(stack_param, 2);
    var arg0 = stack_param[0];
    var arg1 = stack_param[1];
    if(arg0.TYPE!==NUMBER || arg1.TYPE!==NUMBER)
    {
        error("Function / only supports numbers now");
        return build_false();
    }
    else{
        if(arg0.type===FLOAT || arg1.type === FLOAT)
        {
            return build_number(arg0.num/arg1.num, FLOAT);
        }
        var result = arg0.num / arg1.num;
        if(isInteger(result)) 
            return build_number(result, INTEGER)
        return build_number(Math.floor(result), FLOAT);
    }
}
var _lt = function(stack_param)
{
    checkParam(stack_param, 2);
    var arg0 = stack_param[0];
    var arg1 = stack_param[1];
    if(arg0.TYPE === ATOM && arg1.TYPE === ATOM)
    {
        if(arg0.atom < arg1.atom)
            return build_true();
        return build_false();
    }
    else if (arg0.TYPE === NUMBER && arg1.TYPE === NUMBER)
    {
        if(arg0.num < arg1.num)
            return build_true();
        return build_false();
    }
    else
    {
        error("Function < only supports number < number and atom < atom comparison");
        return build_atom('undefined');
    }
}


// summary
var primitive_symbol_table_list = [
'car', 'cdr', 'set-car!', 'set-cdr!', 'cons', 'closure?', 'vector?', 'dictionary?', 'number?', 'pair?', 'atom?', 'builtin-procedure?',
'display', 'dictionary', 'vector', 'list', 'eq?', 'push', 'pop', 'integer?', 'float?', 'null?', '+', '-', '*', '/', '->str', 'atom-ref'
,'<'];
var primitive_procedure_list = [
    _car, _cdr, _set_car, _set_cdr, _cons, _closure$, _vector$, _dictionary$, _number$, _pair$, _atom$, _builtin_procedure$,
    _display, _dictionary, _vector, _list, _eq$, _push, _pop, _integer$, _float$, _null$, _add, _sub, _mul, _div, _str, _atom_ref,
    _lt
];

/*
    return symbol table for compiler
*/
var Build_Symbol_Table = function(){
    return [primitive_symbol_table_list];
}
var Build_Environment = function(){
    var output = [];
    var global_frame = [];
    for(var i = 0; i < primitive_procedure_list.length; i++)
    {
        global_frame.push(build_builtin_procedure(primitive_procedure_list[i]));
    }
    output.push(global_frame);
    return output;
}



/*
    Test Data Type
    for virtual machine
*/
var closure$ = function(v)
{
    return v.TYPE === CLOSURE ? true : false;
}
var vector$ = function(v)
{
    return v.TYPE === VECTOR ? true : false;
}
var dictionary$ = function(v)
{
    return v.TYPE === DICTIONARY ? true : false;
}
var number$ = function(v)
{
    return v.TYPE === NUMBER ? true : false;
}
var integer$ = function(v)
{
    return v.TYPE === NUMBER && v.type === INTEGER ? true : false;
}
var float$ = function(v)
{
    return v.TYPE === NUMBER && v.type === FLOAT ? true : false;
}
var list$ = function(v)
{
    return v.TYPE === LIST ? true : false;
}
var atom$ = function(v)
{
    return v.TYPE === ATOM ? true : false;
}
var builtin_procedure$ = function(v)
{
    return v.TYPE === BUILTIN_PROCEDURE ? true : false;
}
var true$ = function(v)
{
    return v.TYPE === LIST && v.NULL === true? false : true;
}

/*
    Special Application
*/
/*
    Vector:
    ([1 2] 0) => 1              get
    ([1 2] 0 12) => [12 2]      set
*/
var apply_vector_procedure = function(v, stack_param)
{
    if(stack_param.length === 1)
    {
        var arg = stack_param[0];
        if(arg.TYPE!==NUMBER)
        {
            error("Vector call: invalid parameters type");
            return build_false();
        }  
        return v.ref(arg.num);
    }
    else if (stack_param.length === 2)
    {
        var arg0 = stack_param[0];
        var arg1 = stack_param[1];
        if(arg0.TYPE !== NUMBER)
        {
            error("Vector call: invalid parameters type");
            return build_false();
        }
        v.set(arg0.num, arg1);
        return v;
    }
    else{
        error("Vector call: invalid parameters nums");
        return build_false();
    }
}
/*
    ({:a 12 :b 13} :a) => get 12
    ({:a 12 :b 13} :a 1) => {:a 1 :b 13} set
*/
var apply_dictionary_procedure = function(d, stack_param)
{
    if(stack_param.length === 1)
    {
        var arg = stack_param[0];
        if(arg.TYPE!==ATOM)
        {
            error("Dictionary call: invalid parameters type");
            return build_false();
        }  
        return d.ref(arg.atom);
    }
    else if (stack_param.length === 2)
    {
        var arg0 = stack_param[0];
        var arg1 = stack_param[1];
        if(arg0.TYPE !== ATOM)
        {
            error("Dictionary call: invalid parameters type");
            return build_false();
        }
        d.set(arg0.atom, arg1);
        return d;
    }
    else{
        error("Dictionary call: invalid parameters nums");
        return build_false();
    }
}
var apply_primitive_procedure = function(primitive_proc, stack_param)
{
    return primitive_proc(stack_param);
}


var VM = function(instructions, environment, acc, pc, stack)
{
    if(instructions.length === pc) // end of program
    {
        console.log("End...");
        console.log(acc);
        return acc;
    }
    else
    {
        console.log(FormatInst(instructions[pc]));

        var inst = instructions[pc];
        var arg0 = inst[0];
        var arg1 = inst[1];
        var arg2 = inst[2];
        if(arg0 === CONSTANT) // constant
        {
            var a;
            if(arg2 === 0)    // atom
                a = build_atom(arg1)
            else if (arg2 === 1)  // integer
                a = build_number(parseInt(arg1), INTEGER)
            else if (arg2 === 2) // float
                a = build_number(parseFloat(arg1), FLOAT)
            else if (arg2 === 3) // string
                a = build_atom(arg1.slice(1, arg1.length -1));
            else
                error("VM constant: Instruction Error");

            return VM(instructions,
                environment,
                a,
                pc+1,
                stack);
        }
        else if (arg0 === REFER)  // refer value from environment
        {
            return VM(instructions,
                environment,
                environment[arg1][arg2],
                pc+1,
                stack);
        }
        else if (arg0 === ASSIGN) // set value 
        {
            // set value from acc to environment
            environment[arg1][arg2] = acc 
            return VM(instructions,
                environment,
                acc,
                pc+1,
                stack);
        }
        else if (arg0 === FRAME)
        {
            stack.push(environment); // save environment
            stack.push([]); // create argument frame
            return VM(instructions,
                environment,
                acc,
                pc+1,
                stack);
        }
        else if (arg0 === ARGUMENT)
        {
            stack[stack.length - 1].push(acc);
            return VM(instructions,
                environment,
                acc,
                pc+1,
                stack);
        }
        else if (arg0 === CALL)
        {
            /*
                ;; call function, pop frame that stored in stack
                ;; consider different situations
                ;; 1. user defined procedure(lambda)
                ;; 2. builtin procedures (like car cdr)
                ;; 3. vectors
                ;; 4. dictionary
            */
            if(closure$(acc)) // closure
            {
                // run closure
                var start_pc = closure_start_pc(acc);
                var base_environment = closure_environment(acc).slice(0); // this env is a copy of original env, 所以它push对原来的没有影响
                // extend base_environment
                base_environment.push(stack[stack.length - 1]);
                // get new acc
                var a = VM(instructions, 
                           base_environment,
                           build_atom('done'),
                           start_pc,
                           stack);
                // pop argument frame
                stack.pop();
                // restore environment
                return VM(instructions,
                          stack.pop(),
                          a,
                          pc+1,
                          stack);
            } 
            else if (builtin_procedure$(acc)) // builtin procedure
            {
                var a = apply_primitive_procedure(primitive_func(acc), stack.pop());
                 return VM(instructions,
                          stack.pop(),
                          a,
                          pc+1,
                          stack);
            }
            else if (vector$(acc)) // vector
            {
                var a = apply_vector_procedure(acc, stack.pop());
                return VM(instructions,
                          stack.pop(),
                          a,
                          pc+1,
                          stack);
            }
            else if (dictionary$(acc)) // dictionary
            {
                var a = apply_dictionary_procedure(acc, stack.pop());
                return VM(instructions,
                          stack.pop(),
                          a,
                          pc+1,
                          stack);
            }
            else
            {
                error("Invalid calling");
            }
        }
        else if (arg0 === GOTO)
        {
            return VM(instructions,
                    environment,
                    acc,
                    arg1,
                    stack);
        }
        else if (arg0 === TEST) // test test
        { 
            if(true$(acc)) // if passed test, run next
            {
                return VM(instructions,
                        environment,
                        acc,
                        pc+1,
                        stack);
            }
            else // jmp ahead
            {
                return VM(instructions,
                        environment,
                        acc,
                        pc+arg1,
                        stack);
            }
        }
        else if (arg0 === JMP)
        {
            return VM(instructions,
                      environment,
                      acc,
                      pc+arg1,
                      stack);
        }
        else if (arg0 === CLOSE) // make closure
        {
            // arg1 is new pc
            return VM(instructions,
                      environment,
                      make_closure(pc+1, environment),
                      arg1+1,
                      stack);
        }
        else if (arg0 === RETURN) // return
            return acc;
        else
        {
            error("Invalid Instructions");
            console.log(arg0);
            return ;
        }
    }
}


/*
    test
*/
var FormatInst = function(inst)
{
    var i = inst[0];
    var output = "";
    if(i===CONSTANT)
        output = output + "constant";
    else if (i===ASSIGN)
        output = output + "assign";
    else if (i===REFER)
        output = output + "refer";
    else if (i===FRAME)
        output = output + "frame";
    else if (i===CLOSE)
        output = output + "close";
    else if (i===RETURN)
        output = output + "return"
    else if (i===ARGUMENT)
        output = output + "argument"
    else if (i===CALL)
        output = output + "call"
    else if (i===TEST)
        output = output + "test"
    else if (i===JMP)
        output = output + "jmp"
    else if (i===GOTO)
        output = output + "goto"
    else
        error("Invalid instruction");
    output = output + " " + inst[1] + " " + inst[2];
    return output;
}
var PrintInstructions = function(insts)
{
    for(var i = 0; i < insts.length; i++)
    {
        console.log(FormatInst(insts[i]));
    }
}
/*
    test lexer parser
*/
/*
(define f (lambda (n) (if (eq? n 0) 1 (* n (f (- n 1))))))
(define f (lambda ()
    (define x '(1 2)')
    (lambda (msg)
        (if (eq? msg 'a')
            x
            (set-car! x 12)))))

(define f (lambda () (define x '(1 2)) (lambda (msg) (if (eq? msg 'a) x (set-car! x 12))))) (define a (f)) (a 'a)
*/
// var x = "(define x 15) (display (/ 4 3.0))"
// var x = "(define add (lambda (a b) (+ a b))) (add 3 4) (add 5 6) (add 7 8)"
/*
var x = "(define x @(,a a b))"
var l = Lexer(x);
var s = Parser(l);

console.log(s)
console.log("Finish testing lexer and parser")
console.log("==========\n\n\n\n")
*/
/*
    test compiler
*/
/*
var symbol_table = Build_Symbol_Table();
var instructions = [];
var i = compile_sequence(s, symbol_table, instructions);
PrintInstructions(i)
console.log("Finish testing compiler")
console.log("==========\n\n\n\n")
*/
/*
    test virtual machine
*/
/*
var env = Build_Environment();
var o = VM(i, env, build_atom('done'), 0, []);
console.log(env)
*/






































