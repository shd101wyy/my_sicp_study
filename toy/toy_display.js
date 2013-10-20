/*
	formatNumber to string
	parameters type:
		obj : Object
	return type:
		obj : Object(atom)
*/
var formatNumber = function(obj){
	var num = obj.number
	if(num.type == FLOAT) // float number
	{
		return $ATOM$(num.numer)
	}
	else if (num.tyoe === INT)
	{
		return $ATOM$(num.numer)
	}
	else 
	{
		return $ATOM$(num.numer+"/"+num.denom)
	}
}
/*
	formatAtom to string
	parameters type:
		obj : Object
	return type:
		obj : Object(atom)
*/
var formatAtom = function(obj){
	return obj
}
/*
	formatBoolean to string
	parameters type:
		obj : Object
	return type:
		obj : Object(atom)
*/
var formatBoolean = function(obj){
	if(obj.bool.value = true)
		return $ATOM$("true")
	return $ATOM$("false")
}
/*
	formatProcedure to string
	parameters type:
		obj : Object
	return type:
		obj : Object(atom)
*/
var formatProcedure = function(obj){
	return $ATOM$("<user-procedure >")
}
/*
	formatMacro to string
	parameters type:
		obj : Object
	return type:
		obj : Object(atom)
*/
var formatMacro = function(obj){
	return $ATOM$("<user-macro >")
}
/*
	formatMacro to string
	parameters type:
		obj : Object
	return type:
		obj : Object(atom)
*/
var formatList = function(obj){
	var list = obj.list
	var p = list // pointer
	if(p.car == null) // empty list
		return $ATOM$("()")
	var output = "("
	while(p.car!=null){
		output = output + formatAll ( p.car ) + " "
		p = p.cdr
		if(p.type === LIST) // cdr is list
			p = p.list;
		else{
			output = output + formatAll(p) + " "
			break;
		}
	}
	output = output.slice(0, output.length - 1) + ")"
	return $ATOM$(output)
}

/*

	formatVector to string
	parameters type:
		obj : Object
	return type:
		obj : Object(atom)
*/
var formatVector = function(obj){
	var vec = obj.vector
	var output = "["
	for(var i = 0; i < vec.length; i++){
		output = output + vec.value[i] + " "
	}
	output = output.slice(0, output.length - 1) + "]"
	return $ATOM$(output)
}
/*
	formatDictionary to string
	parameters type:
		obj : Object
	return type:
		obj : Object(atom)
*/
var formatDictionary = function(obj){
	var dict = obj.dictionary
	var dict_value = dict.value
	var output = "{"
	for(var i in dict_value){
		output = output + i + " " + dict_value[i]+" "
	}
	output = output.slice(0, output.length - 1) +"}"
	return $ATOM$(output)
}
/*
	formatAll to string
	parameters type:
		obj : Object
	return type:
		obj : Object(atom)
*/
var formatAll = function(obj){
	var type = obj.type
	if(type===NUMBER) // number
		return formatNumber(obj)
	else if (type===ATOM)
		return obj
	else if (type===VECTOR)
		return formatVector(obj)
	else if (type===DICTIONARY)
		return formatDictionary(obj)
	else if (type===BOOLEAN)
		return formatBoolean(obj)
	else if (type===PROCEDURE)
		return formatProcedure(obj)
	else if (type===MACRO)
		return formatMacro(obj)
	else if (type===LIST)
		return formatList(obj)
	else
		return $ATOM$("Builtin Function: formatAll Error -- Invalid Parameter Type: " + obj.type)
}
/*

	Display Function
	parameters type:
		obj : Object
	return type:
		obj : Object(atom)
*/
var display = function(obj){
	var format_obj = formatAll(obj)
	console.log(format_obj.atom.value)
	return $ATOM$("undefined")
}























