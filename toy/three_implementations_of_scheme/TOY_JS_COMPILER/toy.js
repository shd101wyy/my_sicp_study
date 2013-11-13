/*

	Toy Language to JavaScritp compiler

*/
var LIST = 2;
var MACRO = 4;
// build nil
var Nil = function()
{
 
    this.NULL = true   // for virtual machine check
    this.TYPE = LIST  // for virtual machien check
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
var build_nil = function()
{
   return new Nil();
}
var Macro = function(arg_list, pattern_list)
{
    this.arg_list = arg_list;
    this.pattern_list = pattern_list;
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
            || input_str[i]=="\n" || input_str[i]==";")
            return i;
        else
            return find_final_number_of_atom_index(input_str, i+1);
    }
    var lexer_iter = function(input_str, i)
    {
        if(i>=input_str.length)
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

/*
	because null? like var name is not allowed in javascript
	I will replace it with "$TOY$_index"
*/
var INVALID_NAME_TABLE = {};
var INVALID_NAME_TABLE_LENGTH = 0;

var check_invalid_name = function(var_name)
{
	// return /^(?!(?:do|if|in|for|let|new|try|var|case|else|enum|eval|false|null|this|true|void|with|break|catch|class|const|super|throw|while|yield|delete|export|import|public|return|static|switch|typeof|default|extends|finally|package|private|continue|debugger|function|arguments|interface|protected|implements|instanceof)$)[$A-Z\_a-z\xaa\xb5\xba\xc0-\xd6\xd8-\xf6\xf8-\u02c1\u02c6-\u02d1\u02e0-\u02e4\u02ec\u02ee\u0370-\u0374\u0376\u0377\u037a-\u037d\u0386\u0388-\u038a\u038c\u038e-\u03a1\u03a3-\u03f5\u03f7-\u0481\u048a-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05d0-\u05ea\u05f0-\u05f2\u0620-\u064a\u066e\u066f\u0671-\u06d3\u06d5\u06e5\u06e6\u06ee\u06ef\u06fa-\u06fc\u06ff\u0710\u0712-\u072f\u074d-\u07a5\u07b1\u07ca-\u07ea\u07f4\u07f5\u07fa\u0800-\u0815\u081a\u0824\u0828\u0840-\u0858\u08a0\u08a2-\u08ac\u0904-\u0939\u093d\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097f\u0985-\u098c\u098f\u0990\u0993-\u09a8\u09aa-\u09b0\u09b2\u09b6-\u09b9\u09bd\u09ce\u09dc\u09dd\u09df-\u09e1\u09f0\u09f1\u0a05-\u0a0a\u0a0f\u0a10\u0a13-\u0a28\u0a2a-\u0a30\u0a32\u0a33\u0a35\u0a36\u0a38\u0a39\u0a59-\u0a5c\u0a5e\u0a72-\u0a74\u0a85-\u0a8d\u0a8f-\u0a91\u0a93-\u0aa8\u0aaa-\u0ab0\u0ab2\u0ab3\u0ab5-\u0ab9\u0abd\u0ad0\u0ae0\u0ae1\u0b05-\u0b0c\u0b0f\u0b10\u0b13-\u0b28\u0b2a-\u0b30\u0b32\u0b33\u0b35-\u0b39\u0b3d\u0b5c\u0b5d\u0b5f-\u0b61\u0b71\u0b83\u0b85-\u0b8a\u0b8e-\u0b90\u0b92-\u0b95\u0b99\u0b9a\u0b9c\u0b9e\u0b9f\u0ba3\u0ba4\u0ba8-\u0baa\u0bae-\u0bb9\u0bd0\u0c05-\u0c0c\u0c0e-\u0c10\u0c12-\u0c28\u0c2a-\u0c33\u0c35-\u0c39\u0c3d\u0c58\u0c59\u0c60\u0c61\u0c85-\u0c8c\u0c8e-\u0c90\u0c92-\u0ca8\u0caa-\u0cb3\u0cb5-\u0cb9\u0cbd\u0cde\u0ce0\u0ce1\u0cf1\u0cf2\u0d05-\u0d0c\u0d0e-\u0d10\u0d12-\u0d3a\u0d3d\u0d4e\u0d60\u0d61\u0d7a-\u0d7f\u0d85-\u0d96\u0d9a-\u0db1\u0db3-\u0dbb\u0dbd\u0dc0-\u0dc6\u0e01-\u0e30\u0e32\u0e33\u0e40-\u0e46\u0e81\u0e82\u0e84\u0e87\u0e88\u0e8a\u0e8d\u0e94-\u0e97\u0e99-\u0e9f\u0ea1-\u0ea3\u0ea5\u0ea7\u0eaa\u0eab\u0ead-\u0eb0\u0eb2\u0eb3\u0ebd\u0ec0-\u0ec4\u0ec6\u0edc-\u0edf\u0f00\u0f40-\u0f47\u0f49-\u0f6c\u0f88-\u0f8c\u1000-\u102a\u103f\u1050-\u1055\u105a-\u105d\u1061\u1065\u1066\u106e-\u1070\u1075-\u1081\u108e\u10a0-\u10c5\u10c7\u10cd\u10d0-\u10fa\u10fc-\u1248\u124a-\u124d\u1250-\u1256\u1258\u125a-\u125d\u1260-\u1288\u128a-\u128d\u1290-\u12b0\u12b2-\u12b5\u12b8-\u12be\u12c0\u12c2-\u12c5\u12c8-\u12d6\u12d8-\u1310\u1312-\u1315\u1318-\u135a\u1380-\u138f\u13a0-\u13f4\u1401-\u166c\u166f-\u167f\u1681-\u169a\u16a0-\u16ea\u16ee-\u16f0\u1700-\u170c\u170e-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176c\u176e-\u1770\u1780-\u17b3\u17d7\u17dc\u1820-\u1877\u1880-\u18a8\u18aa\u18b0-\u18f5\u1900-\u191c\u1950-\u196d\u1970-\u1974\u1980-\u19ab\u19c1-\u19c7\u1a00-\u1a16\u1a20-\u1a54\u1aa7\u1b05-\u1b33\u1b45-\u1b4b\u1b83-\u1ba0\u1bae\u1baf\u1bba-\u1be5\u1c00-\u1c23\u1c4d-\u1c4f\u1c5a-\u1c7d\u1ce9-\u1cec\u1cee-\u1cf1\u1cf5\u1cf6\u1d00-\u1dbf\u1e00-\u1f15\u1f18-\u1f1d\u1f20-\u1f45\u1f48-\u1f4d\u1f50-\u1f57\u1f59\u1f5b\u1f5d\u1f5f-\u1f7d\u1f80-\u1fb4\u1fb6-\u1fbc\u1fbe\u1fc2-\u1fc4\u1fc6-\u1fcc\u1fd0-\u1fd3\u1fd6-\u1fdb\u1fe0-\u1fec\u1ff2-\u1ff4\u1ff6-\u1ffc\u2071\u207f\u2090-\u209c\u2102\u2107\u210a-\u2113\u2115\u2119-\u211d\u2124\u2126\u2128\u212a-\u212d\u212f-\u2139\u213c-\u213f\u2145-\u2149\u214e\u2160-\u2188\u2c00-\u2c2e\u2c30-\u2c5e\u2c60-\u2ce4\u2ceb-\u2cee\u2cf2\u2cf3\u2d00-\u2d25\u2d27\u2d2d\u2d30-\u2d67\u2d6f\u2d80-\u2d96\u2da0-\u2da6\u2da8-\u2dae\u2db0-\u2db6\u2db8-\u2dbe\u2dc0-\u2dc6\u2dc8-\u2dce\u2dd0-\u2dd6\u2dd8-\u2dde\u2e2f\u3005-\u3007\u3021-\u3029\u3031-\u3035\u3038-\u303c\u3041-\u3096\u309d-\u309f\u30a1-\u30fa\u30fc-\u30ff\u3105-\u312d\u3131-\u318e\u31a0-\u31ba\u31f0-\u31ff\u3400-\u4db5\u4e00-\u9fcc\ua000-\ua48c\ua4d0-\ua4fd\ua500-\ua60c\ua610-\ua61f\ua62a\ua62b\ua640-\ua66e\ua67f-\ua697\ua6a0-\ua6ef\ua717-\ua71f\ua722-\ua788\ua78b-\ua78e\ua790-\ua793\ua7a0-\ua7aa\ua7f8-\ua801\ua803-\ua805\ua807-\ua80a\ua80c-\ua822\ua840-\ua873\ua882-\ua8b3\ua8f2-\ua8f7\ua8fb\ua90a-\ua925\ua930-\ua946\ua960-\ua97c\ua984-\ua9b2\ua9cf\uaa00-\uaa28\uaa40-\uaa42\uaa44-\uaa4b\uaa60-\uaa76\uaa7a\uaa80-\uaaaf\uaab1\uaab5\uaab6\uaab9-\uaabd\uaac0\uaac2\uaadb-\uaadd\uaae0-\uaaea\uaaf2-\uaaf4\uab01-\uab06\uab09-\uab0e\uab11-\uab16\uab20-\uab26\uab28-\uab2e\uabc0-\uabe2\uac00-\ud7a3\ud7b0-\ud7c6\ud7cb-\ud7fb\uf900-\ufa6d\ufa70-\ufad9\ufb00-\ufb06\ufb13-\ufb17\ufb1d\ufb1f-\ufb28\ufb2a-\ufb36\ufb38-\ufb3c\ufb3e\ufb40\ufb41\ufb43\ufb44\ufb46-\ufbb1\ufbd3-\ufd3d\ufd50-\ufd8f\ufd92-\ufdc7\ufdf0-\ufdfb\ufe70-\ufe74\ufe76-\ufefc\uff21-\uff3a\uff41-\uff5a\uff66-\uffbe\uffc2-\uffc7\uffca-\uffcf\uffd2-\uffd7\uffda-\uffdc][$A-Z\_a-z\xaa\xb5\xba\xc0-\xd6\xd8-\xf6\xf8-\u02c1\u02c6-\u02d1\u02e0-\u02e4\u02ec\u02ee\u0370-\u0374\u0376\u0377\u037a-\u037d\u0386\u0388-\u038a\u038c\u038e-\u03a1\u03a3-\u03f5\u03f7-\u0481\u048a-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05d0-\u05ea\u05f0-\u05f2\u0620-\u064a\u066e\u066f\u0671-\u06d3\u06d5\u06e5\u06e6\u06ee\u06ef\u06fa-\u06fc\u06ff\u0710\u0712-\u072f\u074d-\u07a5\u07b1\u07ca-\u07ea\u07f4\u07f5\u07fa\u0800-\u0815\u081a\u0824\u0828\u0840-\u0858\u08a0\u08a2-\u08ac\u0904-\u0939\u093d\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097f\u0985-\u098c\u098f\u0990\u0993-\u09a8\u09aa-\u09b0\u09b2\u09b6-\u09b9\u09bd\u09ce\u09dc\u09dd\u09df-\u09e1\u09f0\u09f1\u0a05-\u0a0a\u0a0f\u0a10\u0a13-\u0a28\u0a2a-\u0a30\u0a32\u0a33\u0a35\u0a36\u0a38\u0a39\u0a59-\u0a5c\u0a5e\u0a72-\u0a74\u0a85-\u0a8d\u0a8f-\u0a91\u0a93-\u0aa8\u0aaa-\u0ab0\u0ab2\u0ab3\u0ab5-\u0ab9\u0abd\u0ad0\u0ae0\u0ae1\u0b05-\u0b0c\u0b0f\u0b10\u0b13-\u0b28\u0b2a-\u0b30\u0b32\u0b33\u0b35-\u0b39\u0b3d\u0b5c\u0b5d\u0b5f-\u0b61\u0b71\u0b83\u0b85-\u0b8a\u0b8e-\u0b90\u0b92-\u0b95\u0b99\u0b9a\u0b9c\u0b9e\u0b9f\u0ba3\u0ba4\u0ba8-\u0baa\u0bae-\u0bb9\u0bd0\u0c05-\u0c0c\u0c0e-\u0c10\u0c12-\u0c28\u0c2a-\u0c33\u0c35-\u0c39\u0c3d\u0c58\u0c59\u0c60\u0c61\u0c85-\u0c8c\u0c8e-\u0c90\u0c92-\u0ca8\u0caa-\u0cb3\u0cb5-\u0cb9\u0cbd\u0cde\u0ce0\u0ce1\u0cf1\u0cf2\u0d05-\u0d0c\u0d0e-\u0d10\u0d12-\u0d3a\u0d3d\u0d4e\u0d60\u0d61\u0d7a-\u0d7f\u0d85-\u0d96\u0d9a-\u0db1\u0db3-\u0dbb\u0dbd\u0dc0-\u0dc6\u0e01-\u0e30\u0e32\u0e33\u0e40-\u0e46\u0e81\u0e82\u0e84\u0e87\u0e88\u0e8a\u0e8d\u0e94-\u0e97\u0e99-\u0e9f\u0ea1-\u0ea3\u0ea5\u0ea7\u0eaa\u0eab\u0ead-\u0eb0\u0eb2\u0eb3\u0ebd\u0ec0-\u0ec4\u0ec6\u0edc-\u0edf\u0f00\u0f40-\u0f47\u0f49-\u0f6c\u0f88-\u0f8c\u1000-\u102a\u103f\u1050-\u1055\u105a-\u105d\u1061\u1065\u1066\u106e-\u1070\u1075-\u1081\u108e\u10a0-\u10c5\u10c7\u10cd\u10d0-\u10fa\u10fc-\u1248\u124a-\u124d\u1250-\u1256\u1258\u125a-\u125d\u1260-\u1288\u128a-\u128d\u1290-\u12b0\u12b2-\u12b5\u12b8-\u12be\u12c0\u12c2-\u12c5\u12c8-\u12d6\u12d8-\u1310\u1312-\u1315\u1318-\u135a\u1380-\u138f\u13a0-\u13f4\u1401-\u166c\u166f-\u167f\u1681-\u169a\u16a0-\u16ea\u16ee-\u16f0\u1700-\u170c\u170e-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176c\u176e-\u1770\u1780-\u17b3\u17d7\u17dc\u1820-\u1877\u1880-\u18a8\u18aa\u18b0-\u18f5\u1900-\u191c\u1950-\u196d\u1970-\u1974\u1980-\u19ab\u19c1-\u19c7\u1a00-\u1a16\u1a20-\u1a54\u1aa7\u1b05-\u1b33\u1b45-\u1b4b\u1b83-\u1ba0\u1bae\u1baf\u1bba-\u1be5\u1c00-\u1c23\u1c4d-\u1c4f\u1c5a-\u1c7d\u1ce9-\u1cec\u1cee-\u1cf1\u1cf5\u1cf6\u1d00-\u1dbf\u1e00-\u1f15\u1f18-\u1f1d\u1f20-\u1f45\u1f48-\u1f4d\u1f50-\u1f57\u1f59\u1f5b\u1f5d\u1f5f-\u1f7d\u1f80-\u1fb4\u1fb6-\u1fbc\u1fbe\u1fc2-\u1fc4\u1fc6-\u1fcc\u1fd0-\u1fd3\u1fd6-\u1fdb\u1fe0-\u1fec\u1ff2-\u1ff4\u1ff6-\u1ffc\u2071\u207f\u2090-\u209c\u2102\u2107\u210a-\u2113\u2115\u2119-\u211d\u2124\u2126\u2128\u212a-\u212d\u212f-\u2139\u213c-\u213f\u2145-\u2149\u214e\u2160-\u2188\u2c00-\u2c2e\u2c30-\u2c5e\u2c60-\u2ce4\u2ceb-\u2cee\u2cf2\u2cf3\u2d00-\u2d25\u2d27\u2d2d\u2d30-\u2d67\u2d6f\u2d80-\u2d96\u2da0-\u2da6\u2da8-\u2dae\u2db0-\u2db6\u2db8-\u2dbe\u2dc0-\u2dc6\u2dc8-\u2dce\u2dd0-\u2dd6\u2dd8-\u2dde\u2e2f\u3005-\u3007\u3021-\u3029\u3031-\u3035\u3038-\u303c\u3041-\u3096\u309d-\u309f\u30a1-\u30fa\u30fc-\u30ff\u3105-\u312d\u3131-\u318e\u31a0-\u31ba\u31f0-\u31ff\u3400-\u4db5\u4e00-\u9fcc\ua000-\ua48c\ua4d0-\ua4fd\ua500-\ua60c\ua610-\ua61f\ua62a\ua62b\ua640-\ua66e\ua67f-\ua697\ua6a0-\ua6ef\ua717-\ua71f\ua722-\ua788\ua78b-\ua78e\ua790-\ua793\ua7a0-\ua7aa\ua7f8-\ua801\ua803-\ua805\ua807-\ua80a\ua80c-\ua822\ua840-\ua873\ua882-\ua8b3\ua8f2-\ua8f7\ua8fb\ua90a-\ua925\ua930-\ua946\ua960-\ua97c\ua984-\ua9b2\ua9cf\uaa00-\uaa28\uaa40-\uaa42\uaa44-\uaa4b\uaa60-\uaa76\uaa7a\uaa80-\uaaaf\uaab1\uaab5\uaab6\uaab9-\uaabd\uaac0\uaac2\uaadb-\uaadd\uaae0-\uaaea\uaaf2-\uaaf4\uab01-\uab06\uab09-\uab0e\uab11-\uab16\uab20-\uab26\uab28-\uab2e\uabc0-\uabe2\uac00-\ud7a3\ud7b0-\ud7c6\ud7cb-\ud7fb\uf900-\ufa6d\ufa70-\ufad9\ufb00-\ufb06\ufb13-\ufb17\ufb1d\ufb1f-\ufb28\ufb2a-\ufb36\ufb38-\ufb3c\ufb3e\ufb40\ufb41\ufb43\ufb44\ufb46-\ufbb1\ufbd3-\ufd3d\ufd50-\ufd8f\ufd92-\ufdc7\ufdf0-\ufdfb\ufe70-\ufe74\ufe76-\ufefc\uff21-\uff3a\uff41-\uff5a\uff66-\uffbe\uffc2-\uffc7\uffca-\uffcf\uffd2-\uffd7\uffda-\uffdc0-9\u0300-\u036f\u0483-\u0487\u0591-\u05bd\u05bf\u05c1\u05c2\u05c4\u05c5\u05c7\u0610-\u061a\u064b-\u0669\u0670\u06d6-\u06dc\u06df-\u06e4\u06e7\u06e8\u06ea-\u06ed\u06f0-\u06f9\u0711\u0730-\u074a\u07a6-\u07b0\u07c0-\u07c9\u07eb-\u07f3\u0816-\u0819\u081b-\u0823\u0825-\u0827\u0829-\u082d\u0859-\u085b\u08e4-\u08fe\u0900-\u0903\u093a-\u093c\u093e-\u094f\u0951-\u0957\u0962\u0963\u0966-\u096f\u0981-\u0983\u09bc\u09be-\u09c4\u09c7\u09c8\u09cb-\u09cd\u09d7\u09e2\u09e3\u09e6-\u09ef\u0a01-\u0a03\u0a3c\u0a3e-\u0a42\u0a47\u0a48\u0a4b-\u0a4d\u0a51\u0a66-\u0a71\u0a75\u0a81-\u0a83\u0abc\u0abe-\u0ac5\u0ac7-\u0ac9\u0acb-\u0acd\u0ae2\u0ae3\u0ae6-\u0aef\u0b01-\u0b03\u0b3c\u0b3e-\u0b44\u0b47\u0b48\u0b4b-\u0b4d\u0b56\u0b57\u0b62\u0b63\u0b66-\u0b6f\u0b82\u0bbe-\u0bc2\u0bc6-\u0bc8\u0bca-\u0bcd\u0bd7\u0be6-\u0bef\u0c01-\u0c03\u0c3e-\u0c44\u0c46-\u0c48\u0c4a-\u0c4d\u0c55\u0c56\u0c62\u0c63\u0c66-\u0c6f\u0c82\u0c83\u0cbc\u0cbe-\u0cc4\u0cc6-\u0cc8\u0cca-\u0ccd\u0cd5\u0cd6\u0ce2\u0ce3\u0ce6-\u0cef\u0d02\u0d03\u0d3e-\u0d44\u0d46-\u0d48\u0d4a-\u0d4d\u0d57\u0d62\u0d63\u0d66-\u0d6f\u0d82\u0d83\u0dca\u0dcf-\u0dd4\u0dd6\u0dd8-\u0ddf\u0df2\u0df3\u0e31\u0e34-\u0e3a\u0e47-\u0e4e\u0e50-\u0e59\u0eb1\u0eb4-\u0eb9\u0ebb\u0ebc\u0ec8-\u0ecd\u0ed0-\u0ed9\u0f18\u0f19\u0f20-\u0f29\u0f35\u0f37\u0f39\u0f3e\u0f3f\u0f71-\u0f84\u0f86\u0f87\u0f8d-\u0f97\u0f99-\u0fbc\u0fc6\u102b-\u103e\u1040-\u1049\u1056-\u1059\u105e-\u1060\u1062-\u1064\u1067-\u106d\u1071-\u1074\u1082-\u108d\u108f-\u109d\u135d-\u135f\u1712-\u1714\u1732-\u1734\u1752\u1753\u1772\u1773\u17b4-\u17d3\u17dd\u17e0-\u17e9\u180b-\u180d\u1810-\u1819\u18a9\u1920-\u192b\u1930-\u193b\u1946-\u194f\u19b0-\u19c0\u19c8\u19c9\u19d0-\u19d9\u1a17-\u1a1b\u1a55-\u1a5e\u1a60-\u1a7c\u1a7f-\u1a89\u1a90-\u1a99\u1b00-\u1b04\u1b34-\u1b44\u1b50-\u1b59\u1b6b-\u1b73\u1b80-\u1b82\u1ba1-\u1bad\u1bb0-\u1bb9\u1be6-\u1bf3\u1c24-\u1c37\u1c40-\u1c49\u1c50-\u1c59\u1cd0-\u1cd2\u1cd4-\u1ce8\u1ced\u1cf2-\u1cf4\u1dc0-\u1de6\u1dfc-\u1dff\u200c\u200d\u203f\u2040\u2054\u20d0-\u20dc\u20e1\u20e5-\u20f0\u2cef-\u2cf1\u2d7f\u2de0-\u2dff\u302a-\u302f\u3099\u309a\ua620-\ua629\ua66f\ua674-\ua67d\ua69f\ua6f0\ua6f1\ua802\ua806\ua80b\ua823-\ua827\ua880\ua881\ua8b4-\ua8c4\ua8d0-\ua8d9\ua8e0-\ua8f1\ua900-\ua909\ua926-\ua92d\ua947-\ua953\ua980-\ua983\ua9b3-\ua9c0\ua9d0-\ua9d9\uaa29-\uaa36\uaa43\uaa4c\uaa4d\uaa50-\uaa59\uaa7b\uaab0\uaab2-\uaab4\uaab7\uaab8\uaabe\uaabf\uaac1\uaaeb-\uaaef\uaaf5\uaaf6\uabe3-\uabea\uabec\uabed\uabf0-\uabf9\ufb1e\ufe00-\ufe0f\ufe20-\ufe26\ufe33\ufe34\ufe4d-\ufe4f\uff10-\uff19\uff3f]*$/
	return /^[a-zA-Z_$][0-9a-zA-Z_$]*$/.test(var_name)
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
        else if (car(l) === ",")
            tag = "unquote"
        else tag = 'quasiquote'
        l = cdr(l);
        if (car(l) === "(") // list
        {
            return cons(tag, cons(parse_list(cdr(l)), build_nil()));
        }
        else if (car(l) === "'" || car(l) === "," || car(l) === "@")  // quote unquote quasiquote
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
    	/*
        if(isNumber(l))
        {
            if(isInteger(l))
            {
                return build_number(l, 1, INTEGER);
            }
            else // float
            {
                return build_number(l, 1, FLOAT);
            }
        }
        else if (isRatio(l))
        {
            return build_number(getNumerator(l), getDenominator(l), RATIO);
        }
        else if (l[0]===":")  // keyword
        {
            return cons(build_atom("quote"), cons(build_atom(l), build_nil()))
        }
        else
            return build_atom(l);
        	*/
        if(l[0]==":")
       		return cons("keyword", cons(l.slice(1), build_nil()))
        if(l[0]=='"') return l;
       	if(isNumber(l) || isRatio(l)) return l;
       	if(check_invalid_name(l) === false && (l!=="+" && l!=="-" && l!=="*" && l!=="/" && l!=="%" && 
                                              l !== ">" && l !== "<" && l !=="<=" && l !==">=" && l !=="==" && l !== "!=" &&
                                              l !== "===" && l !== "!=="
                                              && l!== "set!" && l!=="."))
       	{
       		if(l in INVALID_NAME_TABLE) return INVALID_NAME_TABLE[l];
       		else{
       			var var_name = "TOY$$_"+INVALID_NAME_TABLE_LENGTH; // create new valid var name
       			INVALID_NAME_TABLE[l] = var_name;
       			INVALID_NAME_TABLE_LENGTH++; // increase length
       			return var_name;
       		}
       	}
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
    new compile sequence
    this time parameter is list
*/
var compile_sequence = function(exps)
{
    if(exps.NULL) // end
        return "";
    else
    {
        return compiler(car(exps)) +
       		   compile_sequence(cdr(exps));
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

var compile_define = function(var_name, var_value)
{
	return "var " + compiler(var_name) + " = " + compiler(var_value) + ";\n";
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

var compile_set = function(var_name, var_value)
{
	return compiler(var_name) + " = " + compiler(var_value) + ";\n";
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
    if(v.NULL)
        return '0';
    return car(v);
}

var compile_if = function(test, consequent, alternative)
{
	return "(function(){ "+
				"if( " + compiler(test) + ")\n" +
	    		"{ return " + compiler(consequent) + ";\n}\n" +
	    		"else{ return " + compiler(alternative) + ";\n}\n" +
			"})()";
}

var compile_begin = function(exps)
{
	if(exps.NULL) return "";
	else if (exps.cdr.NULL) 
		return "return " + compiler(car(exps))+";"
	return compiler(car(exps))+compile_begin(cdr(exps));
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
// the env here is a copy of env
var compile_lambda = function(args, body)
{
	var format_lambda_arguments = function(args)
	{
		if(args.NULL) return "";
		if(args.cdr.NULL) return car(args);
		return car(args) + ", " + format_lambda_arguments(cdr(args));
	}
    var format_lambda_variadic_arguments = function(args)
    {
        var output = "";
        var i;
        for(i = 0; i < args.length-1; i++)
        {
            output+= "var "+(args[i]) + " = arguments["+i+"];"; 
        }
        output+="var "+args[i] + " = [].slice.call(arguments, " + i +")";

        return output;
    }
    /*
        check varidic parameters
    */
    var x = args;
    var variadic$ = false;
    var arguments = []; // get arguments
    while(!x.NULL)
    {
        if(car(x) === '.')
        {
            variadic$ = true;
            arguments.push(cadr(x));
            break;
        }
        else
        {
            arguments.push(car(x));
        }
        x = cdr(x);
    }
    if(variadic$)
        return "function(){"+format_lambda_variadic_arguments(arguments)+";"+compile_begin(body)+"}";
   	return "function(" + format_lambda_arguments(args) +"){"+compile_begin(body)+"}";
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
/* for application */
var compile_args = function(args)
{   
    if(args.NULL)
        return "";
    else if (args.cdr.NULL) 
    	return compiler(car(args));
    else
    {
        return compiler(car(args)) + ", " + compile_args(cdr(args));
    }
}
var compile_application = function(applic, args)
{
    return compiler(applic)+".apply(null, ["+compile_args(args)+"])"
}

var compile_quote_list = function(l)
{
	var format_v = function(v)
	{
		if(v.TYPE === LIST) return compile_quote_list(v);
		else if (isNumber(v)) return v;
		else if (isRatio(v)) return "makeRatio("+getNumerator(v)+","+getDenominator(v)+")";
		else return "\""+v+"\"";
	}
	if(l.NULL) return "build_nil()" // return nil
	if(car(l) === '.') return format_v(cadr(l)) // pair
	var v = car(l);
	var formatted_v = format_v(v);
	return "cons("+formatted_v+","+compile_quote_list(cdr(l))+")";
}
var compile_quasiquote_list = function(l)
{
	var unquote_v = function(v)
	{
		if(v.TYPE === LIST)
			return compiler(v);
		else if (isNumber(v)) return v;
		else if (isRatio(v)) return "makeRatio("+getNumerator(v)+","+getDenominator(v)+")";
		else return v
	}
	var format_v = function(v)
	{
		if(v.TYPE === LIST)
		{
			if(!v.NULL && car(v) === 'unquote')
				return unquote_v(cadr(v));
			return compile_quasiquote_list(v);
		} 
		else if (isNumber(v)) return v;
		else if (isRatio(v)) return "makeRatio("+getNumerator(v)+","+getDenominator(v)+")";
		else return "\""+v+"\""
	}
	if(l.NULL) return "build_nil()" // return nil
	if(car(l) === '.') return format_v(cadr(l)) // pair
	var v = car(l);
	var formatted_v = format_v(v);
	return "cons("+formatted_v+","+compile_quasiquote_list(cdr(l))+")";
}
/* used to save macro value */
var MACRO_ENV = {};
/* (defmacro square (x) @(* ,x ,x)) */
var compile_macro = function(exp)
{
	var macro_name = car(exp);
	var macro_args = cadr(exp);
	var macro_body = caddr(exp);
	if(macro_body.NULL)
	{
		console.log("ERROR: invalid macro body");
	}
	else if (car(macro_body)!=="quasiquote")
	{
		console.log("ERROR: invalid macro body; Macro must begin with 'quasiquote'");
	}
	MACRO_ENV[macro_name] = [macro_args, macro_body];
	return "";
}
/*
	eg
	macro name: square
	macro params: (12)

	macro body: (quasiquote (* (unquote x) (unquote x)))
	macro args: (x)
*/
var expand_macro = function(exp)
{
	var macro_name = car(exp);
	var macro_param = cdr(exp);

	var macro_body = MACRO_ENV[macro_name][1];
	var macro_args = MACRO_ENV[macro_name][0];

	var table = {};
	while(!macro_args.NULL)
	{
		table[car(macro_args)] = car(macro_param);
		macro_args = cdr(macro_args);
		macro_param = cdr(macro_param);
	}
	var macroexpand_ = function(l, table)
	{
		if(l.NULL) return build_nil();
		var v = car(l);
		if(v.TYPE === LIST)
		{
			if(!v.NULL && car(v) === 'unquote' && cadr(v).TYPE!==LIST && (cadr(v) in table))
			{
				return cons(table[cadr(v)], macroexpand_(cdr(l), table));
			}
		}
		return cons(v, macroexpand_(cdr(l), table));
	}
	var expand_macro = macroexpand_(cadr(macro_body), table);
	return expand_macro
}
/* vector */
var compile_vector = function(args)
{
	if(args.NULL) return "[]";
	var output = "[";
	while(!(cdr(args).NULL))
	{
		output+=compiler(car(args))+", "
		args = cdr(args);
	}
	output+=compiler(car(args))+"]";
	return output;
}
/* dictionary */
var compile_dictionary = function(args)
{
	if(args.NULL) return "{}";
	var output = "{";
	var key;
	var val;
	while(!(cdr(cdr(args)).NULL))
	{
		key = compiler(car(args));
		val = compiler(cadr(args));
		output+= key+":"+val+", "
		args = cddr(args);
	}
	key = compiler(car(args));
	val = compiler(cadr(args));
	output+=key+":"+val+"}";
	return output;
}
/* math */
var compile_ar = function(arg1, arg2, op)
{
	if(op === "+") return "toy_add$(" + compiler(arg1) + ", " +compiler(arg2) + ")";
	if(op === "-") return "toy_sub$(" + compiler(arg1) + ", " +compiler(arg2) + ")";
	if(op === "*") return "toy_mul$(" + compiler(arg1) + ", " +compiler(arg2) + ")";
	if(op === "/") return "toy_div$(" + compiler(arg1) + ", " +compiler(arg2) + ")";
	if(op === "%") return "toy_rem$(" + compiler(arg1) + ", " +compiler(arg2) + ")";
}
/* compile toy to javascript */
var compiler = function(exp)
{
	if(typeof(exp) === 'string')
	{
		// check ratio
		if(isRatio(exp)) return "makeRatio("+getNumerator(exp)+','+getDenominator(exp)+")" // return make ratio
		return exp; // return itself
	}
	else
	{
		var tag = car(exp);
		if(tag === 'quote' || tag === 'quasiquote')
		{
			if(cadr(exp).TYPE === LIST){
				if(tag === 'quote')
					return compile_quote_list(cadr(exp));
				return compile_quasiquote_list(cadr(exp));
			}
			else if (isNumber(cadr(exp))) return cadr(exp);
			else if (isRatio(cadr(exp))) return "makeRatio("+getNumerator(cadr(exp))+','+getDenominator(cadr(exp))+")"
			else return '"'+cadr(exp)+'"';
		}
        else if (tag === "keyword")
        {
            return '"'+cadr(exp)+'"'
        }
        else if (tag === "def")
        {
            if(cadr(exp).TYPE === LIST)
                return compiler(make_lambda(exp));
            return compile_define(definition_variable(exp),
                       definition_value(exp));
        }   
        else if (tag === "set!")
        {
            // delete following line in order to support (set! (x :a) 12) like change dictionary
            // if(cadr(exp).TYPE === LIST)
            //    return compiler(make_lambda(exp));
            return compile_set(assignment_variable(exp),
                    assignment_value(exp));
        }
	    else if (tag === "if")
        {
            return compile_if(if_test(exp),
                    if_consequent(exp),
                    if_alternative(exp));
        }
        else if (tag === "begin")
        {
            return compile_begin(cdr(exp));
        }
        else if (tag === "lambda")
        {
            return compile_lambda(lambda_arguments(exp),
                        lambda_body(exp));
        }
        /*
            (defmacro define (var_ val_) @(define ,var_ ,val_) 
                        ((var_ ...) statement1 ...) @(define ,var_ (lambda (...) statement1 ...))
                         )
        */
        else if (tag === "defmacro")
        {
            return compile_macro(cdr(exp));
        }
        else if (tag === "vector")
        {
        	return compile_vector(cdr(exp));
        }
        else if (tag === "dictionary")
        {
        	return compile_dictionary(cdr(exp));
        }
        else if (tag === "+" || tag === "-" || tag === "*" || tag === "/" || tag === "%"
                || tag === ">" || tag === "<" || tag ==="<=" || tag ===">=" || tag ==="==" || tag === "!="
                || tag === "===" || tag === "!==")
        {
            if(tag === "==") tag = "==="
            if(tag === "!=") tag = "!=="
        	// return compile_ar(cadr(exp), caddr(exp), tag)
        	return compiler(cadr(exp))+tag+compiler(caddr(exp))
        }
        else if (tag === "new")
        {
            return " new " + compiler(cadr(exp));
        }
        else if (tag === "ref")
        {
            return compiler(cadr(exp))+"[" + compiler(caddr(exp))+"]"
        }
        else if (tag in MACRO_ENV) // macro expand
        {
        	return compiler(expand_macro(exp));
        }
        else // application
        {
            // check compile dictionary quick access
            /*
                (x :a) means access key value from x
                (x :a :b) means call function, not key access
                if meet "keyword" and only one exist, then it's key access
            */
            if(cdr(exp).NULL === false && !cadr(exp).NULL && cadr(exp).TYPE === LIST && car(cadr(exp)) === 'keyword' && cddr(exp).NULL)
            {
                return compiler(tag)+"[\"" + cadr(cadr(exp))+"\"]"
            }
            return compile_application(application_head(exp),
                            application_args(exp))
        }
	}
}

/*
	embed function
*/
var toy_add$ = function(arg1, arg2)
{
	if(typeof(arg1) == "number" && typeof(arg2) == "number")
		return arg1 + arg2;
	if(typeof(arg1) == 'string' || typeof(arg2) == 'string')
		return arg1 + arg2;
}
var toy_sub$ = function(arg1, arg2)
{
	if(typeof(arg1) == "number" && typeof(arg2) == "number")
		return arg1 + arg2;
}
var toy_mul$ = function(arg1, arg2)
{
	if(typeof(arg1) == "number" && typeof(arg2) == "number")
		return arg1 + arg2;
}
var toy_div$ = function(arg1, arg2)
{
	if(typeof(arg1) == "number" && typeof(arg2) == "number")
		return arg1 + arg2;
}
var toy_rem$ = function(arg1, arg2)
{
	if(typeof(arg1) == "number" && typeof(arg2) == "number")
		return arg1 + arg2;
}
var ref = function(arg, key)
{
    return arg[key];
}
var add = function(a,b){return a+b;}

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















































