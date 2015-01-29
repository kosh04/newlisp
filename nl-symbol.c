/* nl-symbol.c --- symbol handling routines for newLISP

    Copyright (C) 2015 Lutz Mueller

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/


#include "newlisp.h"
#include "protos.h"

#ifdef MAC_OSX
#include <sys/mman.h>
#endif


#define str2cmp(s1,s2) \
    (( (*(unsigned char *)(s1) << 8) |  *((unsigned char *)(s1) + 1) ) - \
     ( (*(unsigned char *)(s2) << 8) |  *((unsigned char *)(s2) + 1) )  )
                         

extern CELL * cellMemory;
extern SYMBOL * trueSymbol;
extern SYMBOL * orSymbol;

SYMBOL * findInsertSymbol(char * key, int forceCreation);
int deleteSymbol(char * key);
void deleteContextSymbols(CELL * cell, int checkReferences);
void changeContextCells(SYMBOL * contextPtr);
CELL dumpSymbol(char * name);
void collectSymbols(SYMBOL * sPtr, CELL * symbolList);
void collectSymbolAssocs(SYMBOL * sPtr, CELL * assocList);
static SYMBOL * root;   /* root symbol derived from context */

/* --------- return a list of all symbols in a context -------------- */


CELL * p_symbols(CELL * params)
{
SYMBOL * context;
CELL * symbolList;

symbolList = getCell(CELL_EXPRESSION);

if(params->type == CELL_NIL) 
    context = currentContext;
else
    getContext(params, &context);

if(context) /* check in case we are in debug mode */
    collectSymbols((SYMBOL *)((CELL *)context->contents)->aux, symbolList);
return(symbolList);
}


void collectSymbols(SYMBOL * sPtr, CELL * symbolList)
{
CELL * cell;

if(sPtr != NIL_SYM && sPtr != NULL)
    {
    collectSymbols(sPtr->left, symbolList);
    if(symbolList->contents == (UINT)nilCell)
        {
        symbolList->contents = (UINT)stuffSymbol(sPtr);
        symbolList->aux = symbolList->contents;
        }
    else 
        {
        cell = (CELL *)symbolList->aux;
        cell->next = stuffSymbol(sPtr);
        symbolList->aux = (UINT)cell->next;
        }
    collectSymbols(sPtr->right, symbolList);
    }
}


CELL * associationsFromTree(SYMBOL * context)
{
CELL * assocsList;

assocsList = getCell(CELL_EXPRESSION);
collectSymbolAssocs((SYMBOL *)((CELL *)context->contents)->aux, assocsList);

return(assocsList);
}

/* only for symbols starting with underscore character _ */
void collectSymbolAssocs(SYMBOL * sPtr, CELL * assocList)
{
CELL * cell;

if(sPtr != NIL_SYM && sPtr != NULL)
    {
    collectSymbolAssocs(sPtr->left, assocList);
    if(*sPtr->name == '_')
        {
        cell = makeCell(CELL_EXPRESSION, (UINT)stuffString(sPtr->name + 1));
        ((CELL *)cell->contents)->next = copyCell((CELL *)sPtr->contents);

        if(assocList->contents == (UINT)nilCell)
            assocList->contents = (UINT)cell;
        else 
            ((CELL *)assocList->aux)->next = cell;
        assocList->aux = (UINT)cell;
        }
    collectSymbolAssocs(sPtr->right, assocList);
    }
}


SYMBOL * lookupSymbol(char * token, SYMBOL * context)
{
root = (SYMBOL *)((CELL *)context->contents)->aux;

return(findInsertSymbol(token, LOOKUP_ONLY));
}


SYMBOL * makeSafeSymbol(CELL * cell, SYMBOL * context, int flag)
{
char * token;
UINT number;

if(isNumber(cell->type))
    {
    token = alloca(32);
    getIntegerExt(cell, &number, FALSE);
    snprintf(token, 31, "_%"PRIdPTR, number);
    }
else
    {
    token = alloca(cell->aux + 1);
    *token = '_';
    memcpy(token + 1, (char *)cell->contents, cell->aux);
    }
    
if(flag)
        return(translateCreateSymbol(token, CELL_NIL, context, TRUE));

root = (SYMBOL *)((CELL *)context->contents)->aux;
return(findInsertSymbol(token, LOOKUP_ONLY));
}


/* 
   if forceFlag is TRUE then 
       create the symbol, if not found in the context 
       specified in that context
   else
       if not found try to inherit from MAIN as a global
       or primitive, else create it in context specified
*/


SYMBOL * translateCreateSymbol
    (char * token, int type, SYMBOL * context, int forceFlag)
{
SYMBOL * sPtr;
CELL * cell = NULL;
size_t len;

cell = (CELL *)context->contents;
root = (SYMBOL *)cell->aux;

if(forceFlag)
    sPtr = findInsertSymbol(token, FORCE_CREATION);
else /* try to inherit from MAIN, if not here create in current context */
    {
    sPtr = findInsertSymbol(token, LOOKUP_ONLY);
    if(sPtr == NULL)
        {
        if(context != mainContext)
            {
            root = (SYMBOL *)((CELL *)mainContext->contents)->aux;
            sPtr = findInsertSymbol(token, LOOKUP_ONLY);
            /* since 7.2.7 only inherit primitives and other globals */
            if(sPtr != NULL && !(sPtr->flags & SYMBOL_GLOBAL))
                {
                if(symbolType(sPtr) != CELL_CONTEXT
                    || (SYMBOL *)((CELL*)sPtr->contents)->contents != sPtr)
                    sPtr = NULL;
                }
            root = (SYMBOL *)cell->aux;
            }
        if(sPtr == NULL)
            sPtr = findInsertSymbol(token, FORCE_CREATION);
        }
    }


/* the symbol existed already, return */
if(sPtr->contents != 0) 
    return(sPtr);

/* root might have changed, after symbol insertion */
cell->aux = (UINT)root;
    
/* a new symbol has been allocated by findInsertSymbol() */
if(type != CELL_PRIMITIVE)
    {
    len = strlen(token);
    sPtr->name = (char *)allocMemory(len + 1);
    memcpy(sPtr->name, token, len + 1);
    cell = copyCell(nilCell); 
    sPtr->contents = (UINT)cell;
    /* make a new context symbol */
    if(type == CELL_CONTEXT && context == mainContext)
        {
        cell->type = CELL_CONTEXT;
        cell->contents = (UINT)sPtr;
        cell->aux = 0;
        sPtr->flags |= (SYMBOL_PROTECTED | SYMBOL_GLOBAL);
        }
    }
else
    {
    sPtr->name = token;
    sPtr->contents = (UINT)nilCell;
    }

sPtr->context = context;
return(sPtr);
}

CELL * assignSymbol(SYMBOL * sPtr, CELL * content)
{
deleteList((CELL*)sPtr->contents);
sPtr->contents = (UINT)content;

return(content);
}

/* ------------------------- dump RB tree info of a symbol -------------------- */

#ifdef SYMBOL_DEBUG
CELL * p_dumpSymbol(CELL * params)
{
char * name;
SYMBOL * sPtr;

getString(params, &name);

sPtr = findInsertSymbol(name, LOOKUP_ONLY);

if(sPtr == NULL)
    return(nilCell);

varPrintf(OUT_DEVICE, "name=%s color=%s parent=%s left=%s right=%s\n", 
    sPtr->name,
    (sPtr->color == RED) ? "red" : "black",
    (sPtr->parent) ? sPtr->parent->name : "ROOT",
    sPtr->left->name,
    sPtr->right->name);

return(trueCell);
}
#endif



/* ----------------------------- delete a symbol --------------------------- */
int references(SYMBOL * sPtr, int replaceFlag);
int externalContextReferences(SYMBOL * contextPtr, int replaceFlag);

CELL * p_deleteSymbol(CELL * params)
{
SYMBOL * sPtr = NULL;
CELL * cell;
CELL * ctx;
int checkReferences = TRUE;

cell = evaluateExpression(params);
if(cell->type != CELL_SYMBOL)
    return(errorProcExt(ERR_SYMBOL_EXPECTED, params));
sPtr = (SYMBOL*)cell->contents;

if(sPtr == mainContext) return(nilCell);

if(symbolType(sPtr) == CELL_CONTEXT)
    {
    ctx = (CELL*)sPtr->contents;
    if(ctx->contents == (UINT)sPtr)
        {
        sPtr->flags &= ~SYMBOL_PROTECTED;
        cell = ctx;
        }
    }

if(sPtr->flags & (SYMBOL_PROTECTED | SYMBOL_BUILTIN)) 
    return(nilCell);

/* nil as extra parameter deletes without reference checking.
   true as extra parameter deletes only if no references are found,
   No extra parameter assumes reference checking is on and if
   a reference is found it is replaced with nil.
*/
params = params->next;
/* extra parameter is specified as nil */
if(params != nilCell && !getFlag(params))
    checkReferences = FALSE;
/* extra parameter is specified as true */
else if(getFlag(params))
    {
    if(cell->type == CELL_CONTEXT)
        {
        if(externalContextReferences(sPtr, FALSE) > 1)
            {
            sPtr->flags |= SYMBOL_PROTECTED;
            return(nilCell);
            }
        checkReferences = FALSE;
        }
    else
        {
        if(references(sPtr, FALSE) > 1)
            return(nilCell);
        checkReferences = FALSE;
        }
    }

if(cell->type == CELL_CONTEXT)
    {
    deleteContextSymbols(cell, checkReferences);
    deleteList((CELL *)sPtr->contents); 
    sPtr->contents = (UINT)copyCell(nilCell);
    }
else 
    deleteAndFreeSymbol(sPtr, checkReferences);
return(trueCell);
}


void deleteContextSymbols(CELL * cell, int checkReferences)
{
SYMBOL * context;
CELL * symbolList;
CELL * nextSymbol;

context = (SYMBOL *)cell->contents;

if(checkReferences)
    externalContextReferences(context, TRUE);

symbolList = getCell(CELL_EXPRESSION);
collectSymbols((SYMBOL *)((CELL *)context->contents)->aux, symbolList);

nextSymbol = (CELL *)symbolList->contents;
while(nextSymbol != nilCell)
    {
    deleteAndFreeSymbol((SYMBOL*)nextSymbol->contents, FALSE);
    nextSymbol = nextSymbol->next;
    }

if(checkReferences)
    changeContextCells(context);

deleteList(symbolList);
}



/*
void deleteContextSymbols(CELL * cell, int checkReferences)
{
SYMBOL * context;
SYMBOL * treePtr;
CELL * symbolList;
CELL * nextSymbol;

context = (SYMBOL *)cell->contents;
treePtr = (SYMBOL *)cell->aux;

symbolList = getCell(CELL_EXPRESSION);
collectSymbols((SYMBOL *)treePtr, symbolList);

nextSymbol = (CELL *)symbolList->contents;
while(nextSymbol != nilCell)
    {
    deleteAndFreeSymbol((SYMBOL*)nextSymbol->contents, FALSE);
    nextSymbol = nextSymbol->next;
    }
    
if(checkReferences)
    externalContextReferences(context, TRUE);

deleteList(symbolList);
}
*/

void deleteAndFreeSymbol(SYMBOL * sPtr, int checkReferences)
{
SYMBOL * context;

context = sPtr->context;
root = (SYMBOL *)((CELL *)context->contents)->aux;

if(!deleteSymbol(sPtr->name))
    return;

((CELL *)context->contents)->aux = (UINT)root; /* root may have changed */

deleteList((CELL *)sPtr->contents);

if(checkReferences) references(sPtr, TRUE);
freeMemory(sPtr->name);
freeMemory(sPtr);
}


void makeContextFromSymbol(SYMBOL * symbol, SYMBOL * treePtr)
{
CELL * contextCell;
UINT * idx = envStackIdx;

/* make sure symbol is not used as local in call hierachy 
   and symbol is legal */
while(idx > envStack)
    {
    if(symbol == (SYMBOL *)*(--idx))
        errorProcExt2(ERR_CANNOT_PROTECT_LOCAL, stuffSymbol(symbol));
    --idx;
    }

if(!isLegalSymbol(symbol->name))
        errorProcExt2(ERR_INVALID_PARAMETER, stuffString(symbol->name));


contextCell = makeCell(CELL_CONTEXT, (UINT)symbol);

contextCell->aux = (UINT)treePtr;
symbol->contents = (UINT)contextCell;
symbol->context = mainContext;
symbol->flags |= (SYMBOL_PROTECTED | SYMBOL_GLOBAL);
}

/* only used when S in (delete 'S) is not a context */
int references(SYMBOL * sPtr, int replaceFlag)
{
CELL * blockPtr;
int i, count;

blockPtr = cellMemory;
count = 0;
while(blockPtr != NULL)
    {
    for(i = 0; i < MAX_BLOCK; i++)
        {
        if( blockPtr->contents == (UINT)sPtr &&
            (*(UINT *)blockPtr == CELL_SYMBOL ||  *(UINT *)blockPtr == CELL_CONTEXT))
            {
            count++;
            if(replaceFlag)
                {
                blockPtr->type = CELL_SYMBOL;
                blockPtr->aux = (UINT)nilCell;  
                blockPtr->contents = (UINT)nilSymbol;
                }
            }
        blockPtr++;
        }
    blockPtr = blockPtr->next;
    }

return(count);
}

int externalContextReferences(SYMBOL * contextPtr, int replaceFlag)
{
CELL * blockPtr;
int i, count = 0;
SYMBOL * sPtr;

blockPtr = cellMemory;
while(blockPtr != NULL)
    {
    for(i = 0; i < MAX_BLOCK; i++)
        {
        if(blockPtr->type == CELL_SYMBOL)
            {
            sPtr = (SYMBOL *)blockPtr->contents;
            if(sPtr->context == contextPtr)     
                {
                count++;
                if(replaceFlag) 
                    blockPtr->contents = (UINT)nilSymbol;
                }
            }
        if(blockPtr->type == CELL_CONTEXT)
            {
            if((SYMBOL *)blockPtr->contents == contextPtr)
                count++;
            }
        blockPtr++;
        }
    blockPtr = blockPtr->next;
    }

return(count);
}

void changeContextCells(SYMBOL * contextPtr)
{
CELL * blockPtr;
int i; 

blockPtr = cellMemory;
while(blockPtr != NULL)
    {
    for(i = 0; i < MAX_BLOCK; i++)
        {
        if (blockPtr->type == CELL_CONTEXT)
            {
            if((SYMBOL *)blockPtr->contents == contextPtr)
                {
                blockPtr->type = CELL_NIL;
                blockPtr->contents = (UINT)nilCell;
                blockPtr->aux = (UINT)nilCell;
                }
            }
        blockPtr++;
        }
    blockPtr = blockPtr->next;
    }
}

/* renamed to 'term' in v.10.1.11 */
CELL * p_term(CELL * params)
{
SYMBOL * sPtr;

params = evaluateExpression(params);
if(params->type == CELL_SYMBOL || params->type == CELL_CONTEXT)
    sPtr = (SYMBOL *)params->contents;
else
    return(errorProcExt(ERR_SYMBOL_OR_CONTEXT_EXPECTED, params));

return(stuffString(sPtr->name));
}

CELL * p_prefix(CELL * params)
{
SYMBOL * sPtr;

getSymbol(params, &sPtr);

return(makeCell(CELL_CONTEXT, (UINT)sPtr->context));
}

/* -------------------------------------------------------------------------

   Red-Black Balanced Binary Tree Algorithm adapted from:

   Thomas Niemann thomasn@epaperpress.com

   See also:

   http://epaperpress.com/sortsearch/index.html

   and:

   Thomas H. Cormen, et al
   Introduction to Algorithms
   (MIT Electrical Engineering and Computer Science)
   (C) 1990 MIT Press

*/


#define compLT(a,b) (a < b)
#define compEQ(a,b) (a == b)

#define BLACK 0
#define RED 1

#define NIL_SYM &sentinel   /* all leafs are sentinels */

SYMBOL sentinel = {
    0,      /* pretty print */
    BLACK,      /* color */
    "NIL",      /* name */
    0,      /* contents */
    NULL,       /* context */
    NULL,       /* parent */
    NIL_SYM,    /* left */
    NIL_SYM     /* right */
    };

void rotateLeft(SYMBOL* x);
void rotateRight(SYMBOL * x);
static void insertFixup(SYMBOL * x);
void deleteFixup(SYMBOL *x);


SYMBOL * createRootContext(char * token)
{
SYMBOL * sPtr;

root = NULL;
mainContext = sPtr = findInsertSymbol(token, TRUE);
sPtr->name = token;
makeContextFromSymbol(sPtr, sPtr);

return(sPtr);
}

/* --------------------------------------------------------------------

   lookup the symbol with name key, if it does not exist and the
   forceCreation flag is set, create and insert the symbol and
   return a pointer to the new symbol. If the context passed is empty
   then it's treePtr (root) will be the new symbol.

*/


SYMBOL * findInsertSymbol(char * key, int forceCreation) 
{
SYMBOL *current, *parent, *x;
int c;

/* find future parent */
current = (root == NULL) ? NIL_SYM : root;
parent = 0;

while (current != NIL_SYM)
    {
    if( ((c = str2cmp(key, current->name)) == 0) && ((c = strcmp(key, current->name)) == 0) )
            return(current);

    parent = current;
    current = (c < 0) ? current->left : current->right;
    }

/* if forceCreation not specified just return */
if(forceCreation == LOOKUP_ONLY) return(NULL);

/* allocate new symbol */
x = (SYMBOL *)callocMemory(sizeof(SYMBOL));

x->parent = parent;
x->left = NIL_SYM;
x->right = NIL_SYM;
x->color = RED;

/* insert node in tree */
if(parent)
    {
    if( (c = str2cmp(key, parent->name)) < 0)
        parent->left = x;
    else if(c > 0)
        parent->right = x;
    else if(strcmp(key, parent->name) < 0)
        parent->left = x;
    else
        parent->right = x;
    }
else
    root =x;

insertFixup(x);

/* return new node */

++symbolCount;
return(x);
}


/* --------------------------------------------------------------------
   extract symbol in context from tree, return 1 if deleted or 0 if it 
   couldn't be found.

*/

int deleteSymbol(char * key)
{
SYMBOL *x, *y, *z;
int color, c;

/* find node in tree */
z = (root == NULL) ? NIL_SYM : root;

while(z != NIL_SYM)
    {
    if( ((c = str2cmp(key, z->name)) == 0) && ((c = strcmp(key, z->name)) == 0) )
        break;
    else
        z = (c < 0) ? z->left : z->right;
    }

if (z == NIL_SYM) return(0); /* key to delete not found */


if (z->left == NIL_SYM || z->right == NIL_SYM)
    {
    /* y has a NIL_SYM node as a child */
    y = z;
    }
else 
    {
    /* find tree successor with a NIL_SYM node as a child */
    y = z->right;
    while (y->left != NIL_SYM) y = y->left;
    }

/* x is y's only child */
if (y->left != NIL_SYM)
    x = y->left;
else
    x = y->right;

/* remove y from the parent chain */
x->parent = y->parent;
if (y->parent)
    {
    if (y == y->parent->left)
        y->parent->left = x;
        else
            y->parent->right = x;
    }
else
    root = x;


color = y->color;
if (y != z)
    {
    /* swap y and z */
    y->left = z->left;
    y->right = z->right;
    y->parent = z->parent;

    if(z->parent)
        {
        if(z->parent->left == z)
            z->parent->left = y;
        else
            z->parent->right = y;
        }
    else root = y;

    y->right->parent = y;
    y->left->parent = y;

    y->color = z->color;
    }

if (color == BLACK)
    deleteFixup (x);

--symbolCount;
return TRUE;
}



/* -------------------------------------------------------------------- */

void rotateLeft(SYMBOL* x) 
{
SYMBOL* y;

y = x->right;

/* establish x->right link */
x->right = y->left;
if (y->left != NIL_SYM) 
    y->left->parent = x;

/* establish y->parent link */
if(y != NIL_SYM) 
    y->parent = x->parent;

if (x->parent)
    {
    if (x == x->parent->left)
        x->parent->left = y;
      else
        x->parent->right = y;
    } 
else 
    root = y;


/* link x and y */
y->left = x;
if (x != NIL_SYM) 
    x->parent = y;
}


void rotateRight(SYMBOL * x)
{
SYMBOL * y;

y = x->left;

/* establish x->left link */
x->left = y->right;
if (y->right != NIL_SYM)
    y->right->parent = x;

/* establish y->parent link */
if (y != NIL_SYM) 
    y->parent = x->parent;

if (x->parent) 
    {
      if (x == x->parent->right)
            x->parent->right = y;
      else
            x->parent->left = y;
    }
else
    root = y;

/* link x and y */
y->right = x;
if (x != NIL_SYM) 
    x->parent = y;
}


static void insertFixup(SYMBOL * x)
{
SYMBOL * y;

/* check Red-Black properties */
while (x != root && x->parent->color == RED)
    {
    /* we have a violation */
    if (x->parent == x->parent->parent->left)
        {
        y = x->parent->parent->right;
        if (y->color == RED) 
            {
            /* uncle is RED */
            x->parent->color = BLACK;
            y->color = BLACK;
            x->parent->parent->color = RED;
            x = x->parent->parent;
            } 
        else 
            {
            /* uncle is BLACK */
            if (x == x->parent->right)
                {
                /* make x a left child */
                x = x->parent;
                rotateLeft(x);
                }

            /* recolor and rotate */
            x->parent->color = BLACK;
            x->parent->parent->color = RED;
            rotateRight(x->parent->parent);
           }
        } 
    else 
        {

        /* mirror image of above code */
        y = x->parent->parent->left;
        if (y->color == RED) 
            {
            /* uncle is RED */
            x->parent->color = BLACK;
            y->color = BLACK;
            x->parent->parent->color = RED;
            x = x->parent->parent;
            } 
        else 
            {
            /* uncle is BLACK */
            if (x == x->parent->left) 
                {
                x = x->parent;
                rotateRight(x);
                }
            x->parent->color = BLACK;
            x->parent->parent->color = RED;
            rotateLeft(x->parent->parent);
            }
        }
    }

root->color = BLACK;
}


void deleteFixup(SYMBOL *x)
{
SYMBOL * w;

while (x != root && x->color == BLACK)
    {
    if (x == x->parent->left)
        {
            w = x->parent->right;
            if (w->color == RED)
            {
            w->color = BLACK;
            x->parent->color = RED;
            rotateLeft (x->parent);
            w = x->parent->right;
            }
            if (w->left->color == BLACK && w->right->color == BLACK)
            {
            w->color = RED;
            x = x->parent;
            } 
        else 
            {
            if (w->right->color == BLACK)
                {
                w->left->color = BLACK;
                w->color = RED;
                rotateRight (w);
                w = x->parent->right;
                }
            w->color = x->parent->color;
            x->parent->color = BLACK;
            w->right->color = BLACK;
            rotateLeft (x->parent);
            x = root;
            }
        } 
    else 
        {
            w = x->parent->left;
            if (w->color == RED)
            {
            w->color = BLACK;
            x->parent->color = RED;
            rotateRight (x->parent);
            w = x->parent->left;
            }
            if (w->right->color == BLACK && w->left->color == BLACK)
            {
            w->color = RED;
            x = x->parent;
            } 
        else 
            {
            if (w->left->color == BLACK)
                {
                w->right->color = BLACK;
                w->color = RED;
                rotateLeft (w);
                w = x->parent->left;
                }
            w->color = x->parent->color;
            x->parent->color = BLACK;
            w->left->color = BLACK;
            rotateRight (x->parent);
            x = root;
            }
        }
    }

x->color = BLACK;
}

/* eof */

