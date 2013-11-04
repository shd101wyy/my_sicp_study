#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/*
	Make Data Type
*/
#define NUMBER 1
#define LIST  2
#define VECTOR  3
#define ATOM  4
#define NIL  5
#define DICTIONARY 6
#define INTEGER  7
#define FLOAT 8
#define RATIO 9
#define CLOSURE 10  // data type 
#define BUILTIN_PROCEDURE 11

typedef struct Toy_Integer Toy_Integer; 
typedef struct Toy_Float Toy_Float;
typedef struct Toy_Ratio Toy_Ratio;
typedef struct Toy_Object Toy_Object;
typedef struct Toy_Atom Toy_Atom;
typedef struct Toy_Vector Toy_Vector;
typedef struct Toy_List Toy_List;
typedef struct Cons Cons; // list

struct Cons
{
	/* data */
	Toy_Object * __car__;
	Toy_Object * __cdr__;
};

struct Toy_Atom
{
	char *content;
	int length;
};

struct Toy_Integer
{
	int integer_;
};

struct Toy_Float
{
	double float_;
};

struct Toy_Ratio
{
	int numer;
	int denom;
};

struct Toy_Vector{
	Toy_Object * v;
	int length;
	int size;
};


/* data type */
struct Toy_Object
{
	/* data */
	int TYPE;
	int NULL$;
	Toy_Integer integer_;
	Toy_Float float_;
	Toy_Ratio ratio_;
	Toy_Atom * atom; 
	Toy_Vector * vector;
	Cons * list;
};

/*
	build null
*/
Toy_Object build_nil()
{
	Toy_Object obj;
	obj.TYPE = LIST;
	obj.NULL$ = 1;
	return obj;
}
/*
	build atom
*/
Toy_Object build_atom(char * input_str)
{
	int length = (int)strlen(input_str);

	Toy_Atom * atom = (Toy_Atom*)malloc(sizeof(Toy_Atom)); // init atom
	atom->length = length; // copy length
	
	char *copy_str = (char*)malloc(sizeof(char)*(length + 1));
	int i = 0;
	for(; i < length; i++)
	{
		copy_str[i] = input_str[i];
	}
	copy_str[i] = 0;

	atom->content = copy_str; // copy content

	// create Toy Object
	Toy_Object obj;
	obj.TYPE = ATOM;
	obj.atom = atom;

	return obj;
}
/*
	build Integer
*/
Toy_Object build_integer(int num)
{
	Toy_Integer __num__;
	__num__.integer_ = num;

	Toy_Object obj;
	obj.TYPE = INTEGER;
	obj.integer_ = __num__;
	return obj;
}
/*
	build Float
*/
Toy_Object build_float(double num)
{
	Toy_Float __num__;
	__num__.float_ = num;

	Toy_Object obj;
	obj.TYPE = FLOAT;
	obj.float_ = __num__;
	return obj;
}

/*
	build Ratio
*/
Toy_Object build_ratio(int numer, int denom)
{
	Toy_Ratio __num__;
	__num__.numer = numer;
	__num__.denom = denom;

	Toy_Object obj;
	obj.TYPE = RATIO;
	obj.ratio_ = __num__;
	return obj;
}

/*
	build Vector
*/
Toy_Object build_vector()
{
	Toy_Vector * vec;
	vec = (Toy_Vector*)malloc(sizeof(20)); // init size is 20
 	vec->length = 0;
	vec->size = 20;
	vec->v = NULL;

	Toy_Object obj;
	obj.TYPE = VECTOR;
	obj.vector = vec;
	return obj;
}

void vector_push(Toy_Object obj, Toy_Object push_value)
{
	Toy_Vector * vec = obj.vector; // point to vector
	if(vec->v == NULL) // vector is not initialized
	{
		vec->v = (Toy_Object*)malloc(sizeof(Toy_Object) * vec->size);
		vec->v[vec->length] = push_value;
		vec->length = vec->length + 1;
	}
	else
	{
		if(vec->length + 1 == vec->size) // reach size
		{
			vec->size = vec->size * 2; // double size;
			vec->v = (Toy_Object*)realloc(vec->v, sizeof(Toy_Object) * vec->size); // realloc
		}
		vec->v[vec->length] = push_value; // push value
		vec->length = vec->length + 1; // update length
	}
}

int vector_length(Toy_Object obj)
{
	Toy_Vector * vec = obj.vector; // point to vector
	return vec->length;
}

int vector_size(Toy_Object obj)
{
	Toy_Vector * vec = obj.vector; // point to vector
	return vec->size;
}


Toy_Object vector_ref(Toy_Object obj, int i)
{
	if (obj.TYPE != VECTOR)
	{
		printf("ERROR: C vector_ref Wrong Type Data\n");
		return build_nil();
	}
	else
	{	
		Toy_Vector * vec = obj.vector; // point to vector
		if(i >= vec->length)
		{
			printf("ERROR: C vector_ref index out of boundary\n");
			return build_nil();
		}
		else
		{
			return vec->v[i];
		}
	}
}
/*
	build Cons
*/
Toy_Object cons(Toy_Object *obj1, Toy_Object *obj2)
{
	Cons *l = (Cons*)malloc(sizeof(Cons));
	l->__car__ = obj1;
	l->__cdr__ = obj2;

	Toy_Object obj;
	obj.TYPE = LIST;
	obj.list = l;
	return obj;
}


Toy_Object * car(Toy_Object *obj)
{
	return obj->list->__car__;
}
Toy_Object * cdr(Toy_Object *obj)
{
	return obj->list->__cdr__;
}


int main()
{
	Toy_Object v1 = build_atom("Hello World");
	Toy_Object v2 = build_atom("Hello World 2");
	Toy_Object v3 = build_atom("Hello World 3");
	Toy_Object a = cons(&v1, &v2);

	Toy_Object b = cons(&a, &v3);
	
	printf("%s\n",cdr(car(&b))->atom->content );
	return 0;
}












