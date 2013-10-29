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
var lexer= function(input_str){
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
        	output.push('array')
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
    Data Types
*/
var NUMBER = 1
var LIST = 2
var VECTOR = 3
var ATOM = 4
var NIL = 5  // it is list
var DICTIONARY 6 
var INTEGER = 7
var FLOAT = 8


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
    else
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
        env[env.length - 1].push(var);
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
    if(n === -1)
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
    return exp[3];
}

var compile_if = function(test, consequent, alternative, env, instructions)
{
    // compile test
    Compiler(test, env, instructions);
    var index1 = instructions.length ; // save current index
    instructions.push([TEST, 0, 0]);  // add test inst
    var index2 = instructions.length ;
    Compiler(consequent, env, instructions); // compile consequent
    // change test 2nd argument
    instructions[index1][1] = instructions.length - index2 + 1;
    var index_of_jmp = instructions.length;
    // add jmp
    instructions.push([JMP, 0, 0]);
    var index3 = instructions.length ;
    Compiler(alternative, env, instructions); // compile alternative
    // change jmp 2nd argument
    instructions[index_of_jmp][1] = instructions.length - index3 + 1;
}
/*
    compile lambda
    (lambda (a b) (+ a b))
*/
var lambda_arguments = function(exp)
{
    return exp[1];
}
var lambda_body  = fucntion(exp)
{
    return exp.slice(2);
}
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
    // restore symbol_table
    env.pop();
    // add return
    instructions.push([RETURN, 0, 0]);
    // set close 2nd arg to index of return
    instructions[index][1] = instructions.length - 1; 
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
var compile_application(applic, args, env, instructions)
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
        else
        {
            instructions.push([CONSTANT, exp[i], 0]);
            instructions.push([ARGUMENT, 0, 0]);
        }
    }
    instructions.push([REFER, 0, 9]); // refer list procedure
    instructions.push([CALL, 0, 0]); // call procedure
}
/*
    Simple Compiler
*/
var Compiler = function(exp, env, instructions)
{
    if(symbol$(exp))  // it is symbol, so look for its value
        compile_lookup(exp, env, instructions);
    else if (pair$(exp)) // array
    {
        var tag = exp[0];
        if(tag === "quote")
        {
            if(pair$(exp[1]))
            {
                compile_list(exp.slice(1), env, instructions);
            }
            else 
            {
                instructions.push([CONSTANT, exp[1], 0]);
            }
        }
        else if (tag === 'define')
        {
            compile_define(definition_variable(exp),
                           definition_value(exp),
                           env,
                           instructions);
        }
        else if (tag === 'set!')
        {
            compile_set(assignment_variable(exp),
                        assignment_value(exp),
                        env,
                        instructions);
        }
        else if (tag === 'if')
        {
            compile_if(if_test(exp),
                        if_consequent(exp),
                        if_alternative(exp),
                        env,
                        instructions);
        }
        else if (tag === 'lambda')
        {
            compile_lambda(lambda_arguments(exp),
                            lambda_body(exp),
                            env,
                            instructions);
        }
        else{
            compile_application(application_head(exp),
                                application_args(exp),
                                env,
                                instructions)
        }
    }
    else // constant
    {
        instructions.push([CONSTANT, exp, 0])
    }
}





/*
    Virtual Machien
*/
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
        var inst = instructions[pc];
        var arg0 = inst[0];
        var arg1 = isnt[1];
        var arg2 = inst[2];
        if(arg0 === CONSTANT) // constant
        {
            return VM(instructions,
                environment,
                arg1,
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
        else if (arg0 === FRAME)
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

            } 
            else if (builtin_procedure$(acc)) // builtin procedure
            {

            }
            else if (vector$(acc)) // vector
            {

            }
            else if (dictionary$(acc)) // dictionary
            {

            }
            else
            {
                error("Invalid callign");
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
        else if (arg0 === CLOSE // make closure
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
            return 
        }
    }
}










































