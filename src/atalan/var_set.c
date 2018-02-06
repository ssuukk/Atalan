/*
 VarSet - Set of variables

VarSet is collection of variables.

(c) 2010 Rudolf Kudla 
Licensed under the MIT license: http://www.opensource.org/licenses/mit-license.php

*/

#include "language.h"

void VarSetInit(VarSet * set)
{
	set->count = set->capacity = 0;
	set->arr   = NULL;
}

void VarSetCleanup(VarSet * set)
{
	MemFree(set->arr);
	VarSetInit(set);
}

void VarSetEmpty(VarSet * set)
{
	set->count = 0;
}

Bool VarSetFindIndex(VarSet * set, Var * key, UInt16 * p_index)
{
	VarTuple * tuple;
	UInt16 cnt, i;
	for(cnt = set->count, tuple = set->arr, i = 0; cnt>0; cnt--, tuple++, i++) {
		if (tuple->key == key) {
			*p_index = i;
			return true;
		}
	}
	return false;
}

Var * VarSetFind(VarSet * set, Var * key)
{
	UInt16 i;
	if (VarSetFindIndex(set, key, &i)) {
		return set->arr[i].var;
	}
	return NULL;
}

Var * VarSetRemove(VarSet * set, Var * key)
/*
Purpose:
	Remove item with specified key from the set.
	If the item existed, return it's value, otherwise return NULL.
*/
{
	Var * var;
	UInt16 i;
	if (VarSetFindIndex(set, key, &i)) {
		var = set->arr[i].var;
		if (i < set->count) {
			memmove(&set->arr[i], &set->arr[set->count-1], sizeof(VarTuple));
		}
		set->count--;
		return var;
	}
	return NULL;
}

void VarSetAdd(VarSet * set, Var * key, Var * var)
/*
Purpose:
	Add the specified value to set.
	If the item with given key already exists in the collection, just set it's var to new value.
*/
{
	UInt16 new_capacity, i;
	VarTuple * tuple;

	if (!VarSetFindIndex(set, key, &i)) {
		if (set->count == set->capacity) {
			new_capacity = set->capacity * 2;
			if (new_capacity == 0) new_capacity = 2;
			set->arr = (VarTuple *)realloc(set->arr, sizeof(VarTuple) * new_capacity);
			set->capacity = new_capacity;
		}

		tuple = &set->arr[set->count];
		tuple->key = key;
		tuple->var = var;
		key->set_index = set->count;
		set->count++;
	} else {
		set->arr[i].var = var;
	}
}

VarTuple * VarSetItem(VarSet * set, UInt16 index)
{
	return &set->arr[index];
}

void VarSetPrint(VarSet * set)
{
	VarTuple * tuple;
	UInt16 cnt, i;
	for(cnt = set->count, tuple = set->arr, i = 0; cnt>0; cnt--, tuple++, i++) {
		PrintVar(tuple->key);
		PrintEOL();
	}
}
