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
        if (input_str[i]===" "||input_str[i]==="\t"||input_str[i]==="\n"){
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
/*
    (cond (false 1)
          (1 2))
        ||
        \/
    (if false 1
        (if 1 2))

*/
var expand_clauses = function(clauses)
{   
    var output = ['if'];
    var p = output; // pointer
    for(var i = 0; i < clauses.length; i++)
    {
        var c = clauses[i];
        // check else
        if(c[0] == 'else')
        {
            if(i!==clauses.length - 1) // else must be the last clause, otherwise it's wrong
            {
                console.log("ERROR: cond invalid else");
            }
            else
            {
                p.push("1");
                p.push(make_begin(c.slice(1)));
                p.push(['quote', []]);
                break;
            }
        }
        p.push(c[0]); // push predict
        var begin_ = make_begin(c.slice(1));
        p.push(begin_); // push consequent
        // push alternative
        if(i == clauses.length - 1) // last clause
        {
            p.push(['quote', []]);
        }
        else
        {
            var new_if = ['if'];
            p.push(new_if);
            p = new_if
        }
    }
    console.log(output);
    return output;
    /*
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
    return output;*/
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
        else if (isRatio(exp[i]))
        {
            instructions.push([RATIO, getNumerator(exp[i]), getDenominator(exp[i])]);
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
        else if (isRatio(exp[i]))
        {
            instructions.push([RATIO, getNumerator(exp[i]), getDenominator(exp[i])]);
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
            else if (isRatio(exp[1]))
            {
                instructions.push([RATIO, getNumerator(exp[1]), getDenominator(exp[1])]);
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
        if(isRatio(exp))
            instructions.push([RATIO, getNumerator(exp), getDenominator(exp)]);
        else if(isInteger(exp))
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
var INTEGER = 7; // integer is part of ratio
var FLOAT = 8;
var CLOSURE = 9;  // data type 
var BUILTIN_PROCEDURE = 10;
// var RATIO = 12; RATIO is defined as instruction as well. so I will not define it here

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
var Number = function(numer, denom, type)
{
    this.numer = numer;
    this.denom = denom;

    this.NULL = false;  // for virtual machine check
    this.TYPE = type; // for virtual machien check
}
var build_number = function(numer, denom, type)
{
    return new Number(numer, denom, type)
}

// build true data type
var build_true = function()
{
    return build_number(1, 1, RATIO);
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
var cadr = function(obj){return car(cdr(obj))}
var cddr = function(obj){return cdr(cdr(obj))}

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
    /*
    if(arg0.TYPE !== LIST)
    {
        error("Function cons: invalid type of param " );
        return build_false();
    } */
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
    return number$(v) ? build_true() : build_false();
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
    if(n.TYPE === RATIO)
    {
        if (n.denom === 1)
            return ""+n.numer
        return n.numer + "/" + n.denom;
    }
    else if (n.TYPE === INTEGER)
    {
        return n.numer;
    }
    return n.numer.toFixed(6);
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
                if(number$(c))
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
            if(number$(c))
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
        if(number$(c))
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
        if(number$(c))
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
    if(number$(v))
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
    if(number$(v))
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
    if(arg1.TYPE === RATIO && arg1.denom === 1)
    {
        return build_atom(arg0.atom[arg1.numer]);
    }
    else
    {
        error("Function atom-ref: Invalid Parameters Type; Refer index should be integer");
        return build_atom('undefined');
    }
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
    else if (number$(arg0))
    {
        return arg0.numer/arg0.denom === arg1.numer/arg1.denom ? build_true() : build_false();
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
    if(arg0.TYPE === RATIO && arg0.denom === 1){
        return build_true();
    }
    return build_false();
}
var _ratio$ = function(stack_param)
{
    checkParam(stack_param, 1);
    var arg0 = stack_param[0];
    if(arg0.TYPE === RATIO){
        return build_true();
    }
    return build_false();
}
var _float$ = function(stack_param)
{
    checkParam(stack_param, 1);
    var arg0 = stack_param[0];
    if(arg0.TYPE === FLOAT)
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
    if(denom === 1)
        return build_number(numer, 1, INTEGER);
    else
        return build_number(numer, denom, RATIO);
    // return build_number(numer/g, denom/g, RATIO);
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
var _add = function(stack_param)
{
    checkParam(stack_param, 2);
    var arg0 = stack_param[0];
    var arg1 = stack_param[1];
    if(number$(arg0) && number$(arg1))
    {
        if(arg0.TYPE===FLOAT || arg1.TYPE === FLOAT)
        {
            return build_number(arg0.numer/arg0.denom + arg1.numer/arg1.denom, 1, FLOAT);
        }
        return add_rat(arg0, arg1);
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
    if(!number$(arg0) || !number$(arg1))
    {
        error("Function - only supports numbers now");
        return build_false();
    }
    else{
        if(arg0.TYPE===FLOAT || arg1.TYPE === FLOAT)
        {
            return build_number(arg0.numer/arg0.denom - arg1.numer/arg1.denom, 1, FLOAT);
        }
        return sub_rat(arg0, arg1);
    }
}
var _mul = function(stack_param)
{
    checkParam(stack_param, 2);
    var arg0 = stack_param[0];
    var arg1 = stack_param[1];
    if(!number$(arg0) || !number$(arg1))
    {
        error("Function * only supports numbers now");
        return build_false();
    }
    else{
        if(arg0.TYPE===FLOAT || arg1.TYPE === FLOAT)
        {
            return build_number(arg0.numer/arg0.denom * (arg1.numer/arg1.denom), 1, FLOAT);
        }
        return mul_rat(arg0, arg1);
    }
}
var _div = function(stack_param)
{
    checkParam(stack_param, 2);
    var arg0 = stack_param[0];
    var arg1 = stack_param[1];
    if(!number$(arg0) || !number$(arg1))
    {
        error("Function / only supports numbers now");
        return build_false();
    }
    else{
        if(arg0.TYPE===FLOAT || arg1.TYPE === FLOAT)
        {
            return build_number(arg0.numer/arg0.denom / (arg1.numer/arg1.denom), 1, FLOAT);
        }
        return div_rat(arg0, arg1);
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
    else if (number$(arg0) && number$(arg1))
    {
        if(arg0.numer/arg0.denom < arg1.numer/arg1.denom)
            return build_true();
        return build_false();
    }
    else
    {
        error("Function < only supports number < number and atom < atom comparison");
        return build_atom('undefined');
    }
}
var _len = function(stack_param)
{
    checkParam(stack_param, 1);
    var arg = stack_param[0];
    if(arg.TYPE === ATOM)
    {
        return build_number(arg.atom.length, 1, RATIO);
    }
    else if (arg.TYPE === VECTOR)
    {
        return build_number(arg.vector.length, 1, RATIO);
    }
    else
    {
        error("Function len: Invalid Parameters Type");
    }
}
var _slice = function(stack_param)
{
    checkParam(stack_param, 3);
    var arg0 = stack_param[0];
    var arg1 = stack_param[1];
    var arg2 = stack_param[2];
    if(!number$(arg1) || !number$(arg2))
    {
        error("Function slice --- invalid parameters type");
        return build_atom('undefined');
    }
    if(arg0.TYPE === ATOM)
    {
        return build_atom(arg0.atom.slice(arg1.numer, arg2.numer));
    }
    else if (arg0.TYPE === VECTOR)
    {
        return build_vector(arg0.vector.slice(arg1.numer, arg2.numer));
    }
    else
    {
        error("Function slice --- only support atom(string) and vector slice")
        return build_atom('undefined');
    }
}
var _dictionary_keys = function(stack_param)
{
    checkParam(stack_param, 1);
    var arg = stack_param[0];
    if(arg.TYPE === DICTIONARY)
    { 
        var keys_str_array = Object.keys(arg.dict) // string array
        var key_to_atom = [];
        for(var i = 0; i < keys_str_array.length; i++)
        {
            key_to_atom.push(build_atom(key_string[i]));
        }
        return build_vector(key_to_atom);
    }
    else
    {
        error("Function dictionary-keys --- only support dictionary type data");
        return build_atom('undefined');
    }
}

var _to_ratio = function(stack_param)
{
    checkParam(stack_param, 1);
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
        error("Function ->ratio --- only support number type data");
        return build_atom('undefined');
    }
}
/* return 0-1 random */
var _random = function(stack_param)
{
    checkParam(0);
    var a = Math.random();
    return build_number(a, 1, FLOAT);
}

// summary
var primitive_symbol_table_list = [
'car', 'cdr', 'set-car!', 'set-cdr!', 'cons', 'closure?', 'vector?', 'dictionary?', 'number?', 'pair?', 'atom?', 'builtin-procedure?',
'display', 'dictionary', 'vector', 'list', 'eq?', 'push', 'pop', 'integer?', 'float?', 'null?', '+', '-', '*', "/", '->str', 'atom-ref'
,'<','len', 'slice', 'dictionary-keys','ratio?','->ratio', 'random'

];
var primitive_procedure_list = [
    _car, _cdr, _set_car, _set_cdr, _cons, _closure$, _vector$, _dictionary$, _number$, _pair$, _atom$, _builtin_procedure$,
    _display, _dictionary, _vector, _list, _eq$, _push, _pop, _integer$, _float$, _null$, _add, _sub, _mul, _div, _str, _atom_ref,
    _lt, _len, _slice, _dictionary_keys, _ratio$, _to_ratio, _random
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
    return (v.TYPE === INTEGER || v.TYPE === RATIO || v.TYPE === FLOAT) ? true : false;
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
var null$ = function(v)
{
    return v.TYPE === LIST && v.NULL === true? true : false;
}
var eq$ = function(arg0, arg1)
{
    if(arg0.TYPE !== arg1.TYPE)
    {
        error("Function eq?: invalid parameters type")
        return false;
    }
    if(arg0.NULL && arg1.NULL) // empty list comparison
    {
        return true;
    }
    if(arg0.TYPE === LIST ||
        arg0.TYPE === VECTOR ||
        arg0.TYPE === DICTIONARY ||
        arg0.TYPE === CLOSURE ||
        arg0.TYPE === BUILTIN_PROCEDURE)
    {
        return arg0 === arg1 ? true : false;
    }
    else if (number$(arg0))
    {
        return arg0.numer/arg0.denom === arg1.numer/arg1.denom ? true : false;
    }
    else if (arg0.TYPE === ATOM)
    {
        return arg0.atom === arg1.atom ? true : false;
    }
}
var false$ = null$;
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
        if(arg.TYPE===INTEGER)
        {
            return v.ref(arg.numer);
        }  
        /*
        else if (arg.TYPE === ATOM)
        {
            var key = arg.atom;
            if(arg.atom === ":pop")
                return primitive_procedure_list[18];
            else if (arg.atom === ":push")
                return primitive_procedure_list[17];
            else if (arg.atom === ":length")
                return build_number(v.vector.length, INTEGER)
            else if (arg.atom === ":top")
                return v.ref(v.vector.length - 1);
        }
        */
        else
        {
             error("Vector call: invalid parameters type");
            return build_false();
        }
    }
    else if (stack_param.length === 2)
    {
        var arg0 = stack_param[0];
        var arg1 = stack_param[1];
        if(arg0.TYPE === INTEGER)
        {
            v.set(arg0.numer, arg1);
            return v;
        }
        else
        {
            error("Vector call: invalid parameters type");
            return build_false();
        }
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
        if(arg.TYPE===ATOM)
        {
            return d.ref(arg.atom);
        }  
        else
        {
            error("Dictionary call: invalid parameters type");
            return build_false();
        }
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
                a = build_number(parseInt(arg1), 1, RATIO)
            else if (arg2 === 2) // float
                a = build_number(parseFloat(arg1), 1, FLOAT)
            else if (arg2 === 3) // string
            {
                if(arg1.length == 2) // empty string
                    a = ""
                else {
                    //a = build_atom(arg1.slice(1, arg1.length -1));
                    a = build_atom(eval(arg1));
                }
            }
            else
                error("VM constant: Instruction Error");

            return VM(instructions,
                environment,
                a,
                pc+1,
                stack);
        }
        else if (arg0 === RATIO)
        {
            var a = build_number(parseInt(arg1), parseInt(arg2), RATIO);
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
    else if (i===RATIO)
        output = output + "ratio"
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
        this.car = car_;
        this.cdr = cdr_;
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
            return cons(parse_list(cdr(l)), parse_list(rest));
        }
        else if (car(l) === "'" || car(l) === "," || car(l) === "@")  // quote unquote quasiquote
        {
            return cons(parse_special(l), parse_list(rest));
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
        var tag ;
        if(car(l) === "'")
            tag = build_atom("quote")
        else if (car(l) === ",")
            tag = build_atom("unquote")
        else tag = build_atom('quasiquote')
        l = cdr(l);
        if (car(l) === "(") // list
        {
            return cons(tag, parse_list(cdr(l)));
        }
        else if (car(l) === "'" || car(l) === "," || car(l) === "@")  // quote unquote quasiquote
        {   // here my be some errors
            return cons(tag, cons(parse_special(l), build_nil()));
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
    var v = instructions.stack;
    for(var i = 0; i < v.length; i++)
    {
        console.log(FormatInst(v[i]));
    }
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
    this.ref = function(i) /* refer frame */
    {
        return this.frames[i];
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
var symbol_table_ref = function(st, i)
{
    return st.ref(i);
}
/* =========================
    Begin to write compiler
   =========================
*/

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
    var n_m = symbol_table_lookup(env, var_name);
    if(n_m[0] === -1) // didn't find var
    {
        // add var inside env
        // that var is free variable
        frame_push(symbol_table_ref(env, symbol_table_length(env)), var_name);
        instructions_push(instructions, make_inst(REFER, env.length-1, env[env.length-1].length-1));
        return;
    } // find var
    else{
        instructions_push( instructions , make_inst(REFER, n_m[0], n_m[1]));
        return;
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
                if(cadr(exp).TYPE === LIST)
                {
                    if(tag.atom === "quasiquote")
                         return compile_quasiquote(cadr(exp), env, instructions);
                    return compile_list(cadr(exp), env, instructions);
                }
                else if(cadr(exp).TYPE === INTEGER)
                    instructions_push( instructions, make_inst(CONSTANT, cadr(exp).numer, 1))
                else if (cadr(exp).TYPE === FLOAT)
                    instructions_push( instructions, make_inst(CONSTANT, cadr(exp).numer, 2))
                else if (cadr(exp).TYPE === RATIO)// ratio
                    instructions_push( instructions, make_inst(RATIO, cadr(exp).numer, cadr(exp).denom))
                
                else // symbol
                {
                    instructions_push( instructions, make_inst(CONSTANT, cadr(exp), 0));
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
        if(exp.TYPE === INTEGER)
            instructions_push( instructions, make_inst(CONSTANT, exp.numer, 1))
        else if (exp.TYPE === FLOAT)
            instructions_push( instructions, make_inst(CONSTANT, exp.numer, 2))
        else if (exp.TYPE === RATIO) // ratio
            instructions_push( instructions, make_inst(RATIO, exp.numer, exp.denom))
        else
            error("Invalid term");
        return;
    }
}

var x = "()"
var y = lexer(x);
console.log(x);
console.log(y);
var z = parser(y);
console.log(formatList(z))


var symbol_table = make_symbol_table();
var global_frame = make_frame();
symbol_table_push(symbol_table, global_frame);

var instructions = make_instructions();

var i = compile_sequence(z, symbol_table, instructions);

instructions_display(i)




























