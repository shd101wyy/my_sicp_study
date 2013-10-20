// Toy Language Data Type
// construct data type
// number
var INT = 1
var FLOAT = 2
var RATIO = 3

var NUMBER = 4
var ATOM = 5         // atom (string)
var VECTOR = 6       // vector
var DICTIONARY = 7   // dictionary
var BOOLEAN = 8      // boolean
var PROCEDURE = 9    // procedure
var MACRO = 10       // macro
var LIST = 11        // list
/*
    ==================================================================================================================
    ==================================================================================================================
    =================================================== Object =======================================================
    ==================================================================================================================
    ==================================================================================================================
*/
// Object
var Object = function(type){
    this.type = type   // type
    this.number = null;
    this.atom = null;
    this.vector = null;
    this.dictionary = null;
    this.bool = null;
    this.procedure = null;
    this.macro = null;
}


/*
    ==================================================================================================================
    ==================================================================================================================
    =================================================== Number =======================================================
    ==================================================================================================================
    ==================================================================================================================
*/

// construct number
var Number = function(numer, denom, type){
    this.numer = numer
    this.denom = denom
    this.type = type // INT or FLOAT
}
// create number and return Object
var $Number$ = function(numer, denom, type){
    var obj = new Object(NUMBER)                // create obj
    var num = new Number(numer, denom, type) // create number
    obj.number = num                         // set number to obj
    return obj                               // return obj
}
// construct atom
var Atom = function(string_value){
    this.value = string_value
}
var $Atom$ = function(string_value){
    var obj = new Object(ATOM)
    var atom = new Atom(string_value)
    obj.atom = atom;
    return obj
}
/*
    ==================================================================================================================
    ==================================================================================================================
    =================================================== Vector =======================================================
    ==================================================================================================================
    ==================================================================================================================
*/
// construct vector
var Vector = function(){
    this.value = []
    this.length = 0
}
// create vector and return Object
var $Vector$ = function(){
    var obj = new Object(VECTOR)
    var vec = new Vector()
    obj.vector = vec;
    return obj
}
// push value to vector
/*
    parameter type:
        obj : Object
        push_value : Object
    return type:
        Object
*/
var Ty_Vector_Push = function(obj, push_value){
    obj.vector.value.push(push_value)
    obj.vector.length++
    return obj
}
// pop value from vector
/*
    parameter type:
        obj : Object
    return type:
        Object
*/
var Ty_Vector_Pop = function(obj){
    return obj.vector.value.pop()
}
/*
    return value at index of vector
    parameter type:
        obj : Object
        num_obj : Object
    return type:
        Object
*/
var Ty_Vector_Get = function(obj, num_obj){
    return obj.vector.value[num_obj.number.numer]
}

/*
    ==================================================================================================================
    ==================================================================================================================
    =================================================== Dictionary =======================================================
    ==================================================================================================================
    ==================================================================================================================
*/
// =========================================
// construct dictionary
var Dictionary = function(){
    this.value = {}
}
// create dictionary and return Object
var $Dictionary$ = function(){
    var obj = new Object(DICTIONARY)
    var dict = new Dictionary()
    obj.dict = dict
    return obj
}
// all parameters are Object
var Ty_Dictionary_Set = function(obj, key, val){
    obj.dictionary.value[key.atom.value] = value
    return obj
}
/*
    parameter types:
        obj : Object
        key : Object
    return Type:
        Object
*/
var Ty_Dictionary_Get = function(obj, key){
    return obj.dictionary.value[key.atom.value]
}
/*
    ==================================================================================================================
    ==================================================================================================================
    =================================================== Boolean =======================================================
    ==================================================================================================================
    ==================================================================================================================
*/

// ==========================================
// construct boolean
var Boolean = function(flag){
    this.value = flag
}
var $Boolean$ = function(flag){
    var obj = new Object(BOOLEAN)
    var boolean_ = new Boolean(flag)
    obj.bool = boolean_
    return obj
}

/*
    ==================================================================================================================
    ==================================================================================================================
    =================================================== Procedure =======================================================
    ==================================================================================================================
    ==================================================================================================================
*/
// construct procedure
var Procedure = function(parameters, body, env){
    this.parameters = parameters;
    this.body = body;
    this.env = env;
}
var $Procedure$ = function(parameters, body, env){
    var obj = new Object(PROCEDURE)
    var proc = new Procedure(parameters, body, env)
    obj.procedure = proc
    return obj
}
/*
    return parameters of Procedure
    parameters type:
        obj : Object
    return type:
        Object
*/
var Ty_Procedure_Parameters = function(obj){
    return obj.procedure.parameters
}
/*
    return body of Procedure
    parameters type:
        obj : Object
    return type:
        Object
*/
var Ty_Procedure_Body = function(obj){
    return obj.procedure.body
}
/*
    return env of Procedure
    parameters type:
        obj : Object
    return type:
        Object
*/
var Ty_Procedure_Env = function(obj){
    return obj.procedure.env
}

/*
    ==================================================================================================================
    ==================================================================================================================
    =================================================== Macro =======================================================
    ==================================================================================================================
    ==================================================================================================================
*/
// construct macro
var Macro = function(parameters, body, env){
    this.parameters = parameters
    this.body = body
    this.env = env
}
var $Macro$ = function(parameters, body, env){
    var obj = new Object(MACRO)
    var macro = new Macro(parameters, body, env)
    obj.macro = macro;
    return obj;
}
/*
    return parameters of Macro
    parameters type:
        obj : Object
    return type:
        Object
*/
var Ty_Macro_Parameters = function(obj){
    return obj.macro.parameters
}
/*
    return body of Macro
    parameters type:
        obj : Object
    return type:
        Object
*/
var Ty_Macro_Body = function(obj){
    return obj.macro.body
}
/*
    return env of Macro
    parameters type:
        obj : Object
    return type:
        Object
*/
var Ty_Macro_Env = function(obj){
    return obj.macro.env
}
/*
    ==================================================================================================================
    ==================================================================================================================
    =================================================== List =======================================================
    ==================================================================================================================
    ==================================================================================================================
*/
// construct list
var List = function(){
    var car = null 
    var cdr = null
}
var $List$ = function(){
    var obj = new Object(LIST)
    var list = new List()
    obj.list = list
    return obj
}
// push value at back of list
/*  
    parameters type:
        obj : Object
        push_obj: Object
*/
var Ty_List_Push = function(obj, push_obj){
    if(obj.list.car == null) // empty list
    {
        obj.list.car = push_obj
        return obj
    }
    var pointer = obj
    while(pointer.cdr!=null){
        pointer = pointer.cdr 
    }
    pointer.cdr = push_obj
    return obj
}


//cons
/*
    parameter type:
        obj1 : Object
        obj2 : Object 
    return type:
        obj : Object
*/
var cons = function(obj1, obj2){
    // create list
    var list = new List()
    list.car = obj1
    list.cdr = obj2

    // create obj and add list to obj
    var obj = new Object(LIST)
    obj.list = list

    return obj
}
/*
    parameters type:
        obj : Object
    return type:
        Object
*/
var car = function(obj){
    return obj.list.car
}
/*
    parameters type:
        obj : Object
    return type:
        Object
*/
var cdr = function(obj){
    return obj.list.cdr
}




