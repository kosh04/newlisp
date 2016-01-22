/* nl-xml-json.c - newLISP XML and JSON interface 

    Copyright (C) 2016 Lutz Mueller

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

#ifdef XML_SUPPORT

#define XML_TEXT 0
#define XML_CDATA 1
#define XML_COMMENT 2
#define XML_ELEMENT 3

int isWhiteSpaceStringN(char * source, int tagPos);
CELL * makeTagSymbolCell(char * tagStart, int tagLen);
void performXmlCallback(CELL * cell, char * start);

char * typeNames[] =
  {
  "TEXT",
  "CDATA",
  "COMMENT",
  "ELEMENT"
  };

CELL * xmlTags = NULL;
CELL * typeCell[4];

static char * xmlError;
static char xmlMsg[64];
static char * sourceOrg;
static char * source;

static SYMBOL * XMLcontext;

UINT optionsFlag;
#define OPTION_NO_OPTION 0
#define OPTION_NO_WHITESPACE 1
#define OPTION_NO_EMPTY_ATTRIBUTES 2
#define OPTION_NO_COMMENTS 4
#define OPTION_TAGS_TO_SYMBOLS 8
#define OPTION_SXML_ATTRIBUTES 16


typedef struct
    {
    char * name;
    void * next;
    } TAG_STACK;

TAG_STACK * tagStack = NULL;

CELL * xmlCallback = NULL;

/* setup type tag default cells, if done already just relink */
CELL * setupTypeTagCells(void)
{
int i;

if(xmlTags == NULL)
    {
    xmlTags = getCell(CELL_EXPRESSION);
    for(i = 0; i < 4; i++)
        typeCell[i] = stuffString(typeNames[i]);
    }
    
/* link cells in a list */
xmlTags->contents = (UINT)typeCell[0];
for(i = 0; i < 3; i++)
    typeCell[i]->next = typeCell[i+1];
    
return(xmlTags);
}

CELL * p_XMLtypeTags(CELL * params)
{
int i;

if(params == nilCell) 
    return(copyCell(setupTypeTagCells()));

if(xmlTags != NULL)
    deleteList(xmlTags);

xmlTags = getCell(CELL_EXPRESSION);
    
for(i = 0; i < 4; i++)
    {
    typeCell[i] = copyCell(evaluateExpression(params));
    params = params->next;
    }

return(copyCell(setupTypeTagCells()));
}

CELL * p_XMLparse(CELL * params)
{
CELL * result;

if(xmlCallback != NULL)
    errorProc(ERR_NOT_REENTRANT);

params = getString(params, &source);
if(params != nilCell)
    {
    params = getInteger(params, &optionsFlag);
    if(params != nilCell)
        {
        XMLcontext = getCreateContext(params, TRUE);
        if(XMLcontext == NULL)
            return(errorProc(ERR_SYMBOL_OR_CONTEXT_EXPECTED));
        if(params->next != nilCell)
            xmlCallback = params->next; 
        else
            xmlCallback = NULL;
        }
    else
        XMLcontext = currentContext;
    }
else 
    optionsFlag = OPTION_NO_OPTION;

setupTypeTagCells();

xmlError = NULL;
sourceOrg = source;
deleteTagStack();

result = parseDoc();
deleteTagStack();

xmlCallback = NULL;

if(xmlError != NULL)
    return nilCell;
else
    return result;
}


CELL * p_XMLerror(CELL * params)
{
CELL * errorCell;
CELL * cell;

if(xmlError == NULL)
    return(nilCell);

cell = stuffString(xmlError);
errorCell = makeCell(CELL_EXPRESSION, (UINT)cell);

cell->next = stuffInteger((UINT)(source - sourceOrg));

return errorCell;
}

void deleteTagStack(void)
{
TAG_STACK * oldTagStack;

while(tagStack != NULL)
    {
    oldTagStack = tagStack;
    freeMemory(tagStack->name);
    tagStack = tagStack->next;
    freeMemory(oldTagStack);
    }
}


CELL * parseDoc(void)
{
CELL * node;
CELL * lastNode;
int closingFlag = FALSE;
int tagPos;

lastNode = node = getCell(CELL_EXPRESSION);

while(!xmlError && !closingFlag)
    {
    if((tagPos = find("<", source)) == -1) break;
    if(tagPos > 0)
        {
        if( (tagStack != NULL) || (node->contents != (UINT)nilCell))
            {
            if((optionsFlag & OPTION_NO_WHITESPACE) && isWhiteSpaceStringN(source, tagPos))
                        {;}
            else lastNode = appendNode(lastNode, makeTextNode(XML_TEXT, stuffStringN(source, tagPos)));
            }
        source = source + tagPos;
        }

    if(strncmp(source, "<!DOCTYPE", 9) == 0)
        {
        parseDTD();
        continue;
        }

    if(*source == '<' && *(source + 1) == '?')
        {
        parseProcessingInstruction();
        continue;
        }

    if(memcmp(source, "<!--", 4) == 0)
        {
        if(optionsFlag & OPTION_NO_COMMENTS)
            parseTag("-->");
        else
            lastNode = appendNode(lastNode, parseTag("-->"));
        continue;
        }
    if(memcmp(source, "<![CDATA[", 9) == 0)
        {
        lastNode = appendNode(lastNode, parseTag("]]>"));
        continue;
        }

    if(*source == '<' && *(source + 1) == '/')
        {
        closingFlag = TRUE;
        parseClosing();
        continue;
        }

    lastNode = appendNode(lastNode, parseTag(">"));
    }


if(xmlError != NULL)
    {
    deleteList(node);
    return nilCell;
    }

return node;
}


void parseDTD(void)
{
int closeTag, squareTag;
int closePos = 0;
char * closeTagStr;

if((closeTag = find(">", source)) == -1)
    {
    xmlError = "error in DTD: expected '>'";
    return;
    }

squareTag = find("[", source);
if(squareTag != -1 && squareTag < closeTag)
    closeTagStr = "]>";
else
    closeTagStr = ">";

while(!xmlError)
    {
    if((closePos = find(closeTagStr, source)) == -1)
        {
        snprintf(xmlMsg, 63, "expected: %s", closeTagStr);
        xmlError = xmlMsg;
        return;
        }
    if(*(source + closePos - 1) != ']')
        break;
    source = source + closePos + strlen(closeTagStr);
    }

source = source + closePos + strlen(closeTagStr);
return;
}


void parseProcessingInstruction(void)
{
int closeTag;

if((closeTag = find("?>", source)) == -1)
    {
    xmlError = "expecting closing tag sequence '?>'";
    return;
    }

source = source + closeTag + 2;
}


void parseClosing(void)
{
int closeTag;
char * tagName;
TAG_STACK * oldTagStack;

if((closeTag = find(">", source)) == -1)
    {
    xmlError = "missing closing >";
    return;
    }

if(tagStack == NULL)
    {
    xmlError = "closing tag has no opening";
    return;
    }

tagName = tagStack->name;
if(strncmp(source + 2, tagName, strlen(tagName)) != 0)
    {
    xmlError = "closing tag doesn't match";
    return;
    }

/* pop tagStack */
freeMemory(tagName);
oldTagStack = tagStack;
tagStack = tagStack->next;

freeMemory(oldTagStack);

source = source + closeTag + 1;
}


CELL * parseTag(char * closeTagStr)
{
char * newSrc;
char * tagStart;
int closeTag;
CELL * cell;

tagStart = source;

cell = NULL;
closeTag = find(closeTagStr, source);
if(*(source + closeTag - 1) == '/')
    {
    if(memcmp(closeTagStr,"]]>",3) != 0)
        {
        --closeTag;
        closeTagStr = "/>";
        }
    }

if(closeTag == -1)
    {
    snprintf(xmlMsg, 63, "expected closing tag: %s", closeTagStr);
    xmlError = xmlMsg;
    return nilCell;
    }

if(memcmp(source, "<!--", 4) == 0)
    {
    if(optionsFlag & OPTION_NO_COMMENTS)
        cell = nilCell;
    else
        {
        cell = stuffStringN(source + 4, closeTag - 4);
        cell = makeTextNode(XML_COMMENT, cell);
        }
    }

if(memcmp(source, "<![CDATA[", 9) == 0)
    {
    cell = stuffStringN(source + 9, closeTag - 9);
    cell = makeTextNode(XML_CDATA, cell);
    }

if(*source == '<' && *(source + 1) == '/')
    {
    xmlError = "closing node has no opening";
    return nilCell;
    }

newSrc = source + closeTag + strlen(closeTagStr);

if(cell == NULL)
    cell = parseNormalTag(source + closeTag, newSrc);
else
    source = newSrc;

/* call back with closed tag expression found
   and opening start and end of source of this
   tag expression
*/

if(xmlCallback) 
    performXmlCallback(cell, tagStart);

return(cell);
}


void performXmlCallback(CELL * result, char * tagStart)
{
CELL * list;
CELL * cell;
CELL * next;
int errNo;

list = makeCell(CELL_EXPRESSION, (UINT)copyCell(xmlCallback));
cell = makeCell(CELL_QUOTE, (UINT)copyCell(result));

cell->next = stuffInteger((UINT)(tagStart - sourceOrg));
next = cell->next;
next->next = stuffInteger((UINT)(source - tagStart));
((CELL*)list->contents)->next = cell;
pushResult(list);
if(!evaluateExpressionSafe(list, &errNo))
    {
    deleteTagStack();
    longjmp(errorJump, errNo);
    }
}


CELL * parseNormalTag(char * endSrc, char * newSrc)
{
char * tagStart;
int tagLen;
CELL * attributes;
CELL * childs;
CELL * tagCell;
TAG_STACK * tag;

++source; /* skip '/' */

while((unsigned char)*source <= ' ' && source < endSrc) ++source; /* skip whitespace */

tagStart = source;
tagLen = 0;
while((unsigned char)*source > ' ' && source < endSrc) ++source, ++tagLen; /* find tag end */

if(tagLen > MAX_SYMBOL)
    {
    xmlError = "tag name too long";
    return nilCell;
    }

attributes = parseAttributes(endSrc);
if(optionsFlag & OPTION_SXML_ATTRIBUTES)
    {
    childs = (CELL*)attributes->contents;
    if(! (childs == nilCell && (optionsFlag & OPTION_NO_EMPTY_ATTRIBUTES)))
        {
        attributes->contents = (UINT)stuffSymbol(atSymbol);
        ((CELL*)(attributes->contents))->next = childs;
        }
    }

if(xmlError) 
    return nilCell;

if(*source == '/' && *(source + 1) == '>')
    {
    source = newSrc;
    if(optionsFlag & OPTION_TAGS_TO_SYMBOLS)
        tagCell = makeTagSymbolCell(tagStart, tagLen);
    else
        tagCell = stuffStringN(tagStart, tagLen);
    return makeElementNode(tagCell, attributes, getCell(CELL_EXPRESSION));
    }

/* push tag on tagstack */
tag = (TAG_STACK*)allocMemory(sizeof(TAG_STACK));
tag->name = (char *)callocMemory(tagLen + 1);
memcpy(tag->name, tagStart, tagLen);
tag->next = tagStack;
tagStack = tag;

source = newSrc;
childs = parseDoc();

if(optionsFlag & OPTION_TAGS_TO_SYMBOLS)
    tagCell = makeTagSymbolCell(tagStart, tagLen);
else
    tagCell = stuffStringN(tagStart, tagLen);

return makeElementNode(tagCell, attributes, childs);
}


CELL * makeTagSymbolCell(char * tagStart, int tagLen)
{
char * name;
char * ptr;
CELL * cell;

name = (char *)callocMemory(tagLen + 1);
memcpy(name, tagStart, tagLen);

if(optionsFlag & OPTION_TAGS_TO_SYMBOLS)
    if((ptr = strstr(name, ":")))  *ptr = '.';

cell = stuffSymbol(translateCreateSymbol(name, CELL_NIL, XMLcontext, 1));
freeMemory(name);
return(cell);
}


CELL * parseAttributes(char * endSrc)
{
CELL * attributes;
CELL * att;
CELL * cell;
CELL * lastAtt;
char * namePos;
char * valPos;
char quoteChar;
int  nameLen, valLen;

attributes = getCell(CELL_EXPRESSION);
lastAtt = NULL;

while(!xmlError && source < endSrc)
    {
    while((unsigned char)*source <= ' ' && source < endSrc) source++; /* strip leading space */
    namePos = source;
    nameLen = 0;
    while((unsigned char)*source > ' ' && *source != '=' && source < endSrc) source++, nameLen++; /* get end */
    if(nameLen == 0) break;
    if(nameLen > MAX_SYMBOL)
        {
        xmlError = "attribute name too long";
        return nilCell;
        }
    while((unsigned char)*source <= ' ' && source < endSrc) source++; /* strip leading space */
    if(*source != '=')
        {
        xmlError = "expected '=' in attributes";
        deleteList(attributes);
        return nilCell;
        }
    else source++;
    while((unsigned char)*source <= ' ' && source < endSrc) source++; /* strip spaces */
    if(*source != '\"' && *source != '\'')
        {
        xmlError = "attribute values must be delimited by \" or \' ";
        deleteList(attributes);
        return nilCell;
        }
    quoteChar = *source;
    source++;
    valPos = source;
    valLen = 0;
    while(*source != quoteChar && source < endSrc) source++, valLen++;
    if(*source != quoteChar) valLen = -1;
    else source++;
    if(nameLen == 0 || valLen == -1)
        {
        xmlError = "incorrect attribute";
        deleteList(attributes);
        return nilCell;
        }
    att = getCell(CELL_EXPRESSION);
    if(optionsFlag & OPTION_TAGS_TO_SYMBOLS)
            cell = makeTagSymbolCell(namePos, nameLen);
    else
        cell = stuffStringN(namePos, nameLen);
    cell->next = stuffStringN(valPos, valLen);
    att->contents = (UINT)cell;
    if(lastAtt == NULL)
        attributes->contents = (UINT)att;
    else 
        lastAtt->next = att;
    lastAtt = att;
    }

return attributes;
}


CELL * appendNode(CELL * node, CELL * newNode)
{
if(node->contents == (UINT)nilCell)
    node->contents = (UINT)newNode;
else
    node->next = newNode;

return newNode;
}


CELL * makeTextNode(int type, CELL * contents)
{
CELL * cell;

/* unwrap text node if nil xml-type-tag */
if(typeCell[type]->type == CELL_NIL)
    return(contents);

cell = copyCell(typeCell[type]);
cell->next = contents;

return(makeCell(CELL_EXPRESSION, (UINT)cell));
}


CELL * makeElementNode(CELL * tagNode, CELL * attributesNode, CELL * childrenNode)
{
CELL * newNode;
CELL * cell;

/* unwrap children node, if nil in xml-type-tag */
if(typeCell[XML_ELEMENT]->type == CELL_NIL)
    {
    cell = childrenNode;
    childrenNode = (CELL *)childrenNode->contents;
    cell->contents = (UINT)nilCell;
    deleteList(cell);
    }

newNode = getCell(CELL_EXPRESSION);
if(typeCell[XML_ELEMENT]->type == CELL_NIL)
    newNode->contents = (UINT)tagNode;
else
    {
    cell = copyCell(typeCell[XML_ELEMENT]);
    newNode->contents = (UINT)cell;
    cell->next = tagNode;
    }

if( (attributesNode->contents == (UINT)nilCell) && 
    (optionsFlag & OPTION_NO_EMPTY_ATTRIBUTES))
    {
    tagNode->next = childrenNode;
    deleteList(attributesNode);
    }
else
    {
    tagNode->next = attributesNode;
    attributesNode->next = childrenNode;
    }

return newNode;
}
    

int find(char * key, char * source)
{
char * ptr;

ptr = strstr(source, key);
if(ptr == NULL) return -1;

return(ptr - source);
}
    

int isWhiteSpaceStringN(char * source, int tagPos)
{
while(tagPos--) if((unsigned char)*source++ > 32) return(FALSE);
return(TRUE);
}

#endif /* XML_SUPPORT */

/* --------------------------------- JSON interface ------------------------- */

CELL * getJSONobject(char * jsonStr, char * * restStr);
CELL * getJSONstring(char * jsonStr, char * * restStr);
CELL * getJSONvalue(char * jsonStr, char * * restStr);
CELL * setJSONerror(char * errorText, char * jsonStr);

#define ERR_JSON_MISSING_BRACE "missing {"
#define ERR_JSON_MISSING_COLON "missing : colon"
#define ERR_JSON_INVALID_OBJECT "invalid JSON object"
#define ERR_JSON_MISSING_KEY "missing key"
#define ERR_JSON_CLOSING_QUOTE "missing closing quote"
#define ERR_JSON_INVALID_UNICODE "invalid unicode"
#define ERR_JSON_INVALID_NUMBER "invalid JSON number format"
#define ERR_JSON_INVALID_ARRAY "invalid JSON array format"
#define ERR_JSON_INVALID_VALUE "invalid JSON value format"

CELL * lastJSONerror = NULL;
char * jsonStrStart;

CELL * p_JSONparse(CELL * params)
{
char * restStr;

if(lastJSONerror) deleteList(lastJSONerror);
lastJSONerror = nilCell;
getString(params, &jsonStrStart);
return(getJSONvalue(jsonStrStart, &restStr));
}


CELL * getJSONobject(char * jsonStr, char * * restStr)
{
CELL * object;
CELL * pair = NULL;
CELL * key;
char * ptr;


/* a JSON object is a (key value) association expression */
while((unsigned char)*jsonStr <= ' ' && *jsonStr != 0) ++jsonStr; /* whitespace */

if(*jsonStr != '{') 
    return(setJSONerror(ERR_JSON_MISSING_BRACE, jsonStr));

++jsonStr;
/* make object envelope */
while((unsigned char)*jsonStr <= ' ' && *jsonStr != 0) ++jsonStr; /* whitespace */
object = getCell(CELL_EXPRESSION);
if(*jsonStr == '}')
    {
    *restStr = jsonStr + 1;
    return(object);
    }

/* get a key : value pair */
GET_PAIR:
if(pair == NULL)
    {
    pair = getCell(CELL_EXPRESSION);
    object->contents = (UINT)pair; /* !!! only the first time then pair->next */
    }
else
    {
    pair->next = getCell(CELL_EXPRESSION);
    pair = pair->next;
    }

/* get key string */
if((key = getJSONstring(jsonStr, &ptr)) == nilCell)
    {
    deleteList(object);
    return(nilCell);
    }
pair->contents = (UINT)key;
jsonStr = ptr;

/* key and value must be separated by : colon */
while((unsigned char)*jsonStr <= ' ' && *jsonStr != 0) ++jsonStr; /* whitespace */
if(*jsonStr != ':') 
    {
    deleteList(object);
    return(setJSONerror(ERR_JSON_MISSING_COLON, jsonStr));
    }

if((key->next = getJSONvalue(++jsonStr, &ptr)) == nilCell)
    {
    deleteList(object);
    return(nilCell);
    }

jsonStr = ptr;

while((unsigned char)*jsonStr <= ' ' && *jsonStr != 0) ++jsonStr; /* whitespace */
if(*jsonStr == '}')
    {
    *restStr = jsonStr + 1;
    return(object);
    }

if(*jsonStr == ',')
    {
    ++jsonStr;
    goto GET_PAIR;
    }

deleteList(object);
return(setJSONerror(ERR_JSON_INVALID_OBJECT, jsonStr));
}


CELL * getJSONstring(char * jsonStr, char * * restStr)
{
char * token;
char * xlated;
size_t size;
int i, j;
#ifdef SUPPORT_UTF8
int len;
char buff[8];
#endif

/* get key string */
while((unsigned char)*jsonStr <= ' ' && *jsonStr != 0) ++jsonStr; /* whitespace */

if(*jsonStr != '"') return(setJSONerror(ERR_JSON_MISSING_KEY, jsonStr));

token = ++jsonStr, size = 0;
while(*jsonStr != '"' && *jsonStr != 0) 
    {
    if(*jsonStr == '\\')
        ++jsonStr, ++size;
    ++jsonStr, ++size;
    }

if(*jsonStr++ != '"') return(setJSONerror(ERR_JSON_CLOSING_QUOTE, jsonStr));

*restStr = jsonStr;

xlated = alloca(size + 1);
i = j = 0;
while(i < size)
    {
    if(*(token + i) == '\\')
        {
        ++i;
        switch(*(token + i))
            {
            case '\\':
                *(xlated + j++) = '\\';
                break;
            case '"': 
                *(xlated + j++) = '"';
                break;
            case '/':
                *(xlated + j++) = '/';
                break;
            case 'b': 
                *(xlated + j++) = '\b';
                break;
            case 'f': 
                *(xlated + j++) = '\f';
                break;
            case 'n': 
                *(xlated + j++) = '\n';
                break;
            case 'r': 
                *(xlated + j++) = '\r';
                break;
            case 't': 
                *(xlated + j++) = '\t';
                break;
            case 'u': 
                if(isxdigit((unsigned char)*(token + i + 1)) &&
                   isxdigit((unsigned char)*(token + i + 2)) &&
                   isxdigit((unsigned char)*(token + i + 3)) &&
                   isxdigit((unsigned char)*(token + i + 4)))
                    {
#ifdef SUPPORT_UTF8
                    buff[0] = '0';
                    buff[1] = 'x';
                    memcpy(buff + 2, token + i + 1, 4);
                    buff[6] = 0;
                    len = wchar_utf8(strtol(buff, NULL, 16), xlated + j);
                    j += len;
#else
                    *(xlated + j) = '\\';
                    memcpy(xlated + j + 1, token + i, 5);
                    j += 6;
#endif
                    i += 4;
                    }
                else
                    return(setJSONerror(ERR_JSON_INVALID_UNICODE, jsonStr));
                break;
            default:
                *(xlated + j++) = *(token + i);
                break;
            }
        ++i;
        }
    else
        *(xlated + j++) = *(token + i++);
    }

*(xlated + j) = 0;
return(stuffStringN(xlated, j));
}


/* get value as one of: string, number, object, array, true, false, null */
CELL * getJSONvalue(char * jsonStr, char * * restStr)
{
CELL * cell;
CELL * array = NULL;
CELL * next = NULL;
char * ptr;
char * number;
size_t len;
int isFloat = FALSE;
double floatNumber;
SYMBOL * falseSymbol;
SYMBOL * nullSymbol;

while((unsigned char)*jsonStr <= ' ' && *jsonStr != 0) ++jsonStr; /* whitespace */

/* string value */
if(*jsonStr == '"')
    {
    cell = getJSONstring(jsonStr, &ptr);
    *restStr = ptr;
    return(cell);
    }
    
/* check if number digit or + - */
if(isDigit((unsigned char)*jsonStr))
    ptr = jsonStr++;
else if((*jsonStr == '-' || *jsonStr == '+') && isDigit((unsigned char)*(jsonStr + 1)))
    ptr = jsonStr++;
else goto NOT_STRING_OR_NUMBER;

number = alloca(32);
while(isDigit((unsigned char)*jsonStr)) ++jsonStr;
if(*jsonStr == '.')
    {
    isFloat = TRUE;
    ++jsonStr;
    while(isDigit((unsigned char)*jsonStr)) ++jsonStr;
    }

if(*jsonStr == 'e' || *jsonStr == 'E') 
    {
    isFloat = TRUE;
    ++jsonStr;
    if(*jsonStr == '-' || *jsonStr == '+')
        ++jsonStr;
    if(!isDigit((unsigned char)*jsonStr))
        return(setJSONerror(ERR_JSON_INVALID_NUMBER, jsonStr));
        while(isDigit((unsigned char)*jsonStr)) ++jsonStr;
    }

/* number must end with space or control character  or white space*/
if(*jsonStr != ' ' && *jsonStr != '\n' && *jsonStr != '\n' && *jsonStr != '\t' && *jsonStr != '\f' &&
   *jsonStr != ',' && *jsonStr != '}' && *jsonStr != ']' && *jsonStr != 0)
    return(setJSONerror(ERR_JSON_INVALID_NUMBER, jsonStr));

len = (size_t)jsonStr - (size_t)ptr;
memcpy(number, ptr, len);
number[len] = 0;
*restStr = jsonStr;

if(isFloat)
    {
    floatNumber = atof(number);
    return(stuffFloat(floatNumber));
    }
else
    return(stuffInteger64(atoll(number)));


NOT_STRING_OR_NUMBER:
/* it's one of object, array, true, false, null */

switch(*jsonStr)
    {
    case '{':
        cell = getJSONobject(jsonStr, &ptr);
        break;
    case '[':
        GET_ARRAY_ELEMENT:
        ++jsonStr;
        while((unsigned char)*jsonStr <= ' ' && *jsonStr != 0) ++jsonStr;
        if(*jsonStr == ']')
            {
            *restStr = jsonStr + 1;
            if(array) return(array);
            return(getCell(CELL_EXPRESSION));
            }
        if((cell = getJSONvalue(jsonStr, &ptr)) == nilCell)
            {
            if(array) deleteList(array);
            return(nilCell);
            }
        if(array == NULL)
            {
            array = getCell(CELL_EXPRESSION);
            array->contents = (UINT)cell;
            }
        else
            next->next = cell;
        next = cell;
        jsonStr = ptr;
        /* whitespace */
        while((unsigned char)*jsonStr <= ' ' && *jsonStr != 0) ++jsonStr;
        if(*jsonStr == ',') goto GET_ARRAY_ELEMENT;
        if(*jsonStr == ']')
            {
            *restStr = jsonStr + 1;
            return(array);
            }
        deleteList(array);
        return(setJSONerror(ERR_JSON_INVALID_ARRAY, jsonStr));
    case 't':
        if(strncmp(jsonStr, "true", 4) == 0)
            {
            cell = stuffSymbol(trueSymbol);
            ptr = jsonStr + 4;
            break;
            }
    case 'f':
        if(strncmp(jsonStr, "false", 5) == 0)
            {
            falseSymbol = translateCreateSymbol("false", CELL_SYMBOL, mainContext, TRUE);
            cell = stuffSymbol(falseSymbol);
            ptr = jsonStr + 5;
            break;
            }
    case 'n':
        if(strncmp(jsonStr, "null", 4) == 0)
            {
            nullSymbol = translateCreateSymbol("null", CELL_SYMBOL, mainContext, TRUE);
            cell = stuffSymbol(nullSymbol);
            ptr = jsonStr + 4;
            break;
            }
    default:
    return(setJSONerror(ERR_JSON_INVALID_VALUE, jsonStr));
    }

*restStr = ptr;
return(cell);
}


CELL * p_JSONerror(CELL * params)
{
return( lastJSONerror == NULL ? nilCell : copyCell(lastJSONerror));
}


CELL * setJSONerror(char * errorText, char * jsonStr)
{
CELL * cell = stuffString(errorText);

if(lastJSONerror) deleteList(lastJSONerror);

lastJSONerror = getCell(CELL_EXPRESSION);
lastJSONerror->contents = (UINT)cell;
cell->next = stuffInteger((UINT)jsonStr - (UINT)jsonStrStart);

return(nilCell);
}

/* eof */


