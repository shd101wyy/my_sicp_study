// toy_lexer_parser.js
// copyright shd101wyy

// tokenize string
// ============
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

// parse token list
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
            if(type == INT){  // number is integer
                return $Number$(parseInt(input_str), 1, INT)
            }
            else if (type == FLOAT){  // number is float
                return $Number$(parseFloat(input_str), 1, INT)
            }
            else{            // number is ratio
                return $Number$(get_numerator(input_str), get_denominator(input_str), RATIO)
            }
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
            return cons(flag, cons(parseList(token_list.slice(1)), []))
        else if (token_list[0]=='@'||token_list[0]=="'"||token_list[0]==',')
            return cons(flag, parseSpecial(token_list.slice(1), token_list[0]))
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
