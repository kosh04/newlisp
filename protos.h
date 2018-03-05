/* protos.h function prototypes fo6 newLISP

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

#ifndef PROTOS_H
#define PROTOS_H
#include "sockssl.h"

CELL * addResult(CELL * * result, CELL * cell, CELL * newCell);
CELL * appendArray(CELL * cell, CELL * params);
CELL * appendNode(CELL * node, CELL * newNode);
CELL * appendString(CELL * cell, CELL * params, char * joint, size_t jointLen, int trailJoint, int evalFlag);
CELL * appendWriteFile(CELL * params, char * type);
CELL * arithmetikOp(CELL * params, int operand);
CELL * arrayList(CELL * params, int flag);
CELL * arrayTranspose(CELL * params);
CELL * assignSymbol(SYMBOL * sPtr, CELL * content);
CELL * associationsFromTree(SYMBOL * context);
CELL * bindList(CELL * list, int flag);
CELL * compareOp(CELL * params, int operand);
CELL * copyCell(CELL * cell);
CELL * copyList(CELL * cell);
CELL * defineOrMacro(CELL * params, UINT type, int flags);
CELL * p_deleteFile(CELL * params);
CELL * dolist(CELL * params, int argsFlag);
CELL * environment(void);
CELL * errorProc(int errorNumber);
CELL * errorProcArgs(int errorNumber, CELL * expr);
CELL * errorProcExt(int errorNumber, CELL * expr);
CELL * errorProcExt2(int errorNumber, CELL * expr);
CELL * evalCheckSymbol(CELL * cell);
CELL * evaluateBlock(CELL * cell);
CELL * evaluateExpression(CELL * cell);
CELL * evaluateExpressionSafe(CELL * cell, int * errNo);
CELL * evaluateLambda(CELL * lambda, CELL * arg, SYMBOL * newContext);
CELL * evaluateLambdaMacro(CELL * macro, CELL * arg, SYMBOL * newContext);
CELL * evaluateNamespaceHash(CELL * args, SYMBOL * newContext);
CELL * evaluateStream(STREAM * stream, UINT printDevice, int flag);
CELL * executeLibfunction(CELL * pcell, CELL * params);
#ifdef FFI
CELL * executeLibFFI(CELL * pcell, CELL * params);
#endif
CELL * expand(CELL * expr, SYMBOL * symbol);
CELL * explodeList(CELL * list, CELL * params);
CELL * floatOp(CELL * params, int operand);
CELL * functionFloat(CELL * params, int operand);
CELL * getCell(int type);
CELL * getFloat(CELL * params, double * floatNumber);
CELL * getInteger(CELL * params, UINT * number);
CELL * getInteger64Ext(CELL * params, INT64 * number, int evalFlag);
CELL * getIntegerExt(CELL * params, UINT * number, int evalFlag);
CELL * getListHead(CELL * params, CELL * * list);
CELL * getEvalDefault(CELL * params, CELL * * result);
CELL * getListSpec(CELL * params, CELL * * result, int setFlag);
CELL * getPutPostDeleteUrl(char * url, CELL * params, int type, int timeout);
CELL * getServicePort(CELL * params, int * portNo, char * protocol);
CELL * getString(CELL * params, char * * stringPtr);
CELL * getStringSize(CELL * params, char * * stringPtr, size_t * size, int evalFlag);
CELL * getSymbol(CELL * params, SYMBOL * * symbol);
CELL * getContext(CELL * params, SYMBOL * * context);
CELL * ifUnless(CELL * params, int ifUnlessFlag);
CELL * incDecF(CELL * params, int type);
CELL * incDecI(CELL * params, int type);
CELL * initIteratorIndex(void);
CELL * isType(CELL * params, int operand);
CELL * isEmptyFunc(CELL * params);
CELL * loop(CELL * params, int flag);
CELL * implicitIndexArray(CELL * cell, CELL * params);
CELL * implicitIndexList (CELL * list, CELL * params);
CELL * implicitNrestSlice(CELL * num, CELL * params);
CELL * implicitIndexString(CELL * cell, CELL * params);
CELL * initArray(CELL * array, CELL * list, CELL * * next);
CELL * loadFile(char * fileName, UINT offset, int encryptFlag, SYMBOL * context);
CELL * makeArray(ssize_t * index, int p);
CELL * makeCell(int type, UINT contents);
CELL * makeCellFromStream(STREAM * stream);
CELL * makeStringCell(char * contents, size_t size);
CELL * makePair(CELL * left, CELL * right);
CELL * makeElementNode(CELL * tagNode, CELL * attributesNode, CELL * childrenNode);
CELL * makeTextNode(int type, CELL * contents);
CELL * matScalar(CELL * params, char type);
CELL * mpdArithmetik(CELL * num, CELL * params, int op);
CELL * netError(int errorNo);
CELL * netPeerLocal(CELL * params, int peerLocalFlag);
#ifdef FFI
CELL * packFFIstruct(CELL * cell, CELL * params);
CELL * unpackFFIstruct(CELL * cell, char * data);
#endif
CELL* parseRegexOptions(CELL* params, UINT * options, int evalFlag);
CELL * setEvent(CELL * params, SYMBOL * * eventSymPtr, char * sysSymName);
CELL * setNthStr(CELL * cellStr, CELL * new, char * insertPtr);
CELL * subarray(CELL * array, ssize_t offset, ssize_t length);
CELL * transposeArray(CELL * array);
CELL * p_JSONparse(CELL * params);
CELL * p_JSONerror(CELL * params);
CELL * p_XMLerror(CELL * params);
CELL * p_XMLparse(CELL * params);
CELL * p_XMLtypeTags(CELL * params);
CELL * p_abort(CELL * params);
CELL * p_abs(CELL * params);
CELL * p_acos(CELL * params);
CELL * p_add(CELL * params);
CELL * p_addFloat(CELL * params);
CELL * p_address(CELL * params);
CELL * p_amb(CELL * params);
CELL * p_and(CELL * params);
CELL * p_append(CELL * params);
CELL * p_appendFile(CELL * params);
CELL * p_apply(CELL * params);
CELL * p_args(CELL * params);
CELL * p_array(CELL * params);
CELL * p_arrayList(CELL * params);
CELL * p_asin(CELL * params);
CELL * p_asinh(CELL * params);
CELL * p_assoc(CELL * params);
CELL * p_atan(CELL * params);
CELL * p_atan2(CELL * params);
CELL * p_atanh(CELL * params);
CELL * p_base64Enc(CELL * params);
CELL * p_base64Dec(CELL * params);
CELL * p_bayesTrain(CELL * params);
CELL * p_bayesQuery(CELL * params);
CELL * p_kmeansTrain(CELL * params);
CELL * p_kmeansQuery(CELL * params);
CELL * p_beta(CELL * params);
CELL * p_betai(CELL * params);
CELL * p_bind(CELL * params);
CELL * p_binomial(CELL * params);
CELL * p_bits(CELL * params);
CELL * p_bitAnd(CELL * params);
CELL * p_bitNot(CELL * params);
CELL * p_bitOr(CELL * params);
CELL * p_bitXor(CELL * params);
CELL * p_callback(CELL * params);
CELL * p_case(CELL * params);
CELL * p_catch(CELL * params);
CELL * p_ceil(CELL * params);
CELL * p_changeDir(CELL * params);
CELL * p_char(CELL * params);
CELL * p_chop(CELL * params);
CELL * p_clean(CELL * params);
CELL * p_close(CELL * params);
CELL * p_collect(CELL * params);
CELL * p_colon(CELL * params);
CELL * p_commandEvent(CELL * params);
CELL * p_continue(CELL * params);
CELL * p_condition(CELL * params);
CELL * p_cons(CELL * params);
CELL * p_constant(CELL * params);
CELL * p_context(CELL * params);
CELL * p_copy(CELL * params);
CELL * p_copyFile(CELL * params);
CELL * p_copyMemory(CELL * params);
CELL * p_corr(CELL * params);
CELL * p_cos(CELL * params);
CELL * p_cosh(CELL * params);
CELL * p_acosh(CELL * params);
CELL * p_count(CELL * params);
CELL * p_crc32(CELL * params);
CELL * p_criticalChi2(CELL  * params);
CELL * p_criticalZ(CELL * params);
CELL * p_criticalT(CELL * params);
CELL * p_criticalF(CELL * params);
CELL * p_currentLine(CELL * params);
CELL * p_curry(CELL * params);
CELL * p_date(CELL * params);
CELL * p_dateList(CELL * params);
CELL * p_dateParse(CELL * params);
CELL * p_dateValue(CELL * params);
CELL * p_debug(CELL * params);
CELL * p_decrementF(CELL * params);
CELL * p_decrementI(CELL * params);
CELL * p_default(CELL * params);
CELL * p_define(CELL * params);
CELL * p_defineMacro(CELL * params);
CELL * p_defineNew(CELL * params);
CELL * p_deleteSymbol(CELL * params);
CELL * p_destroyProcess(CELL * params);
CELL * p_determinant(CELL * params);
CELL * p_device(CELL * params);
CELL * p_difference(CELL * params);
CELL * p_union(CELL * params);
CELL * p_directory(CELL * params);
CELL * p_divFloat(CELL * params);
CELL * p_divide(CELL * params);
CELL * p_doargs(CELL * params);
CELL * p_dolist(CELL * params);
CELL * p_dostring(CELL * params);
CELL * p_dotimes(CELL * params);
CELL * p_dotree(CELL * params);
CELL * p_doWhile(CELL * params);
CELL * p_doUntil(CELL * params);
CELL * p_dump(CELL * params);
CELL * p_dumpSymbol(CELL* params);
CELL * p_dup(CELL * params);
CELL * p_encrypt(CELL * params);
CELL * p_endsWith(CELL * params);
CELL * p_env(CELL * params);
CELL * p_equal(CELL * params);
CELL * p_erf(CELL * params);
CELL * p_errorEvent(CELL * params);
CELL * p_lastError(CELL * params);
/*
CELL * p_errorNumber(CELL * params);
CELL * p_errorText(CELL * params);
*/
CELL * p_eval(CELL * params);
CELL * p_evalBlock(CELL * params);
CELL * p_evalString(CELL * params);
#ifdef EMSCRIPTEN
CELL * p_evalStringJS(CELL * params);
#endif
CELL * p_exec(CELL * params);
CELL * p_exists(CELL * params);
CELL * p_exit(CELL * params);
CELL * p_exp(CELL * params);
CELL * p_expand(CELL * params);
CELL * p_explode(CELL * params);
CELL * p_extend(CELL * params);
CELL * p_factor(CELL * params);
CELL * p_fft(CELL * params);
CELL * p_fileInfo(CELL * params);
CELL * p_filter(CELL * params);
CELL * p_find(CELL * params);
CELL * p_findAll(CELL * params);
CELL * p_first(CELL * params);
CELL * p_flat(CELL * params);
CELL * p_float(CELL * params);
CELL * p_flt(CELL * params);
CELL * p_floor(CELL * params);
CELL * p_for(CELL * params);
CELL * p_forAll(CELL * params);
CELL * p_fork(CELL * params);
CELL * p_format(CELL * params);
CELL * p_fv(CELL * params);
CELL * p_gammai(CELL * params);
CELL * p_gammaln(CELL * params);
CELL * p_gcd(CELL * params);
CELL * p_getChar(CELL * params);
CELL * p_getFloat(CELL * params);
CELL * p_getInteger(CELL * params);
CELL * p_getLong(CELL * params);
CELL * p_getString(CELL * params);
CELL * p_getUrl(CELL * params);
CELL * p_getenv(CELL * params);
CELL * p_global(CELL * params);
CELL * p_greater(CELL * params);
CELL * p_greaterEqual(CELL * params);
CELL * p_history(CELL * params);
CELL * p_if(CELL * params);
CELL * p_ifNot(CELL * params);
CELL * p_ifft(CELL * params);
CELL * p_importLib(CELL * params);
CELL * p_incrementF(CELL * params);
CELL * p_incrementI(CELL * params);
CELL * p_index(CELL * params);
CELL * p_integer(CELL * params);
CELL * p_intersect(CELL * params);
CELL * p_irr(CELL * params);
CELL * p_isAtom(CELL * params);
CELL * p_isContext(CELL * params);
CELL * p_isEven(CELL * params);
CELL * p_isGlobal(CELL * params);
CELL * p_isNil(CELL * params);
CELL * p_isNull(CELL * params);
CELL * p_isNumber(CELL * params);
CELL * p_isOdd(CELL * params);
CELL * p_isProtected(CELL * params);
CELL * p_isTrue(CELL * params);
CELL * p_isZero(CELL * params);
CELL * p_isDirectory(CELL * params);
CELL * p_isEmpty(CELL * params);
CELL * p_isFile(CELL * params);
CELL * p_isFloat(CELL * params);
CELL * p_isInteger(CELL * params);
CELL * p_isBigInteger(CELL * params);
CELL * p_isLambda(CELL * params);
CELL * p_isLegal(CELL * params);
CELL * p_isList(CELL * params);
CELL * p_isMacro(CELL * params);
CELL * p_isArray(CELL * params);
CELL * p_isPrimitive(CELL * params);
CELL * p_isQuote(CELL * params);
CELL * p_isString(CELL * params);
CELL * p_isSymbol(CELL * params);
CELL * p_isinf(CELL * params);
CELL * p_isnan(CELL * params);
CELL * p_join(CELL * params);
CELL * p_last(CELL * params);
CELL * p_length(CELL * params);
CELL * p_less(CELL * params);
CELL * p_lessEqual(CELL * params);
CELL * p_let(CELL * params);
CELL * p_letn(CELL * params);
CELL * p_letExpand(CELL * params);
CELL * p_list(CELL * params);
CELL * p_load(CELL * params);
CELL * p_local(CELL * params);
CELL * p_log(CELL * params);
CELL * p_lookup(CELL * params);
CELL * p_lower(CELL * params);
CELL * p_macro(CELL * params);
CELL * p_mainArgs(CELL * params);
CELL * p_makeDir(CELL * params);
CELL * p_map(CELL * params);
CELL * p_matScalar(CELL * params);
CELL * p_matInvert(CELL * params);
CELL * p_matMultiply(CELL * params);
CELL * p_matTranspose(CELL * params);
CELL * p_match(CELL * params);
CELL * p_maxFloat(CELL * params);
CELL * p_member(CELL * params);
CELL * p_minFloat(CELL * params);
CELL * p_modFloat(CELL * params);
CELL * p_modulo(CELL * params);
CELL * p_mulFloat(CELL * params);
CELL * p_multiply(CELL * params);
CELL * p_netAccept(CELL * params);
CELL * p_netClose(CELL * params);
CELL * p_netConnect(CELL * params);
CELL * p_netEval(CELL * params);
CELL * p_netInterface(CELL * params);
CELL * p_netIpv(CELL * params);
CELL * p_netLastError(CELL * params);
CELL * p_netListen(CELL * params);
CELL * p_netLocal(CELL * params);
CELL * p_netLookup(CELL * params);
CELL * p_netPacket(CELL * params);
CELL * p_netPeek(CELL * params);
CELL * p_netPeer(CELL * params);
CELL * p_netPing(CELL * params);
CELL * p_netReceive(CELL * params);
CELL * p_netReceiveFrom(CELL * params);
CELL * p_netReceiveUDP(CELL * params);
CELL * p_netSelect(CELL * params);
CELL * p_netSend(CELL * params);
CELL * p_netSendTo(CELL * params);
CELL * p_netSendUDP(CELL * params);
CELL * p_netService(CELL * params);
CELL * p_netSessions(CELL * params);
CELL * p_new(CELL * params);
CELL * p_normal(CELL * params);
CELL * p_not(CELL * params);
CELL * p_notEqual(CELL * params);
CELL * p_now(CELL * params);
CELL * p_nper(CELL * params);
CELL * p_npv(CELL * params);
CELL * p_nth(CELL * params);
CELL * p_open(CELL * params);
CELL * p_or(CELL * params);
CELL * p_pack(CELL * params);
CELL * p_parse(CELL * params);
CELL * p_peek(CELL * params);
CELL * p_pipe(CELL * params);
CELL * p_pmt(CELL * params);
CELL * p_pop(CELL * params);
CELL * p_popAssoc(CELL * params);
CELL * p_postUrl(CELL * params);
CELL * p_powFloat(CELL * params);
CELL * p_prefix(CELL * params);
CELL * p_prettyPrint(CELL * params);
CELL * p_print(CELL * params);
CELL * p_println(CELL * params);
CELL * p_probabilityChi2(CELL * params);
CELL * p_probabilityZ(CELL * params);
CELL * p_probabilityT(CELL * params);
CELL * p_probabilityF(CELL * params);
CELL * p_process(CELL * params);
CELL * p_promptEvent(CELL * params);
CELL * p_push(CELL * params);
CELL * p_putUrl(CELL * params);
CELL * p_deleteUrl(CELL * params);
CELL * p_pv(CELL * params);
CELL * p_quote(CELL * params);
CELL * p_rand(CELL * params);
CELL * p_random(CELL * params);
CELL * p_randomize(CELL * params);
CELL * p_readBuffer(CELL * params);
CELL * p_readChar(CELL * params);
CELL * p_readerEvent(CELL * params);
CELL * p_readUTF8(CELL * params);
CELL * p_readExpr(CELL * params);
CELL * p_readFile(CELL * params);
CELL * p_readKey(CELL * params);
CELL * p_readLine(CELL * params);
CELL * p_realpath(CELL * params);
CELL * p_receive(CELL * params);
CELL * p_ref(CELL * params);
CELL * p_refAll(CELL * params);
CELL * p_regex(CELL * params);
CELL * p_regexComp(CELL * params);
CELL * p_remove(CELL * params);
CELL * p_removeDir(CELL * params);
CELL * p_renameFile(CELL * params);
CELL * p_replace(CELL * params);
CELL * p_replaceAssoc(CELL * params);
CELL * p_reset(CELL * params);
CELL * p_rest(CELL * params);
CELL * p_reverse(CELL * params);
CELL * p_rotate(CELL * params);
CELL * p_round(CELL * params);
CELL * p_save(CELL * params);
CELL * p_search(CELL * params);
CELL * p_seed(CELL * params);
CELL * p_seek(CELL * params);
CELL * p_select(CELL * params);
CELL * p_self(CELL * params);
CELL * p_semaphore(CELL * params);
CELL * p_send(CELL * params);
CELL * p_sequence(CELL * params);
CELL * p_series(CELL * params);
CELL * p_set(CELL * params);
CELL * p_setLocale(CELL * params);
CELL * p_setf(CELL * params);
CELL * p_setq(CELL * params);
CELL * p_setRef(CELL * params);
CELL * p_setRefAll(CELL * params);
CELL * p_sgn(CELL * params);
CELL * p_share(CELL * params);
CELL * p_shiftLeft(CELL * params);
CELL * p_shiftRight(CELL * params);
CELL * p_signal(CELL * params);
CELL * p_silent(CELL * params);
CELL * p_sin(CELL * params);
CELL * p_sinh(CELL * params);
CELL * p_sleep(CELL * params);
CELL * p_slice(CELL * params);
CELL * p_sort(CELL * params);
CELL * p_spawn(CELL * params);
CELL * p_sqrt(CELL * params);
CELL * p_ssq(CELL * params);
CELL * p_startsWith(CELL * params);
CELL * p_stats(CELL * params);
CELL * p_string(CELL * params);
#ifdef FFI
CELL * p_struct(CELL * params);
#endif
CELL * p_subFloat(CELL * params);
CELL * p_subtract(CELL * params);
CELL * p_swap(CELL * params);
CELL * p_symbol(CELL * params);
CELL * p_symbolSource(CELL * params);
CELL * p_symbols(CELL * params);
CELL * p_sync(CELL * params);
CELL * p_systemSymbol(CELL * params);
CELL * p_references(CELL * params);
CELL * p_system(CELL * params);
CELL * p_systemError(CELL * params);
CELL * p_systemInfo(CELL * params);
CELL * p_tan(CELL * params);
CELL * p_tanh(CELL * params);
CELL * p_term(CELL* params);
CELL * p_ttest(CELL * params);
CELL * p_throw(CELL * params);
CELL * p_throwError(CELL * params);
CELL * p_time(CELL * params);
CELL * p_timeOfDay(CELL * params);
CELL * p_timerEvent(CELL * params);
CELL * p_title(CELL * params);
CELL * p_trace(CELL * params);
CELL * p_traceHighlight(CELL * params);
CELL * p_transferEvent(CELL * params);
CELL * p_trim(CELL * params);
CELL * p_unique(CELL * params);
CELL * p_unicode(CELL * params);
CELL * p_unify(CELL * params);
CELL * p_unless(CELL * params);
CELL * p_unpack(CELL * params);
CELL * p_until(CELL * params);
CELL * p_upper(CELL * params);
CELL * p_utf8(CELL * params);
CELL * p_utf8len(CELL * params);
CELL * p_uuid(CELL * params);
CELL * p_waitpid(CELL * params);
CELL * p_when(CELL * params);
CELL * p_while(CELL * params);
CELL * p_writeBuffer(CELL * params);
CELL * p_writeChar(CELL * params);
CELL * p_writeFile(CELL * params);
CELL * p_writeLine(CELL * params);
CELL * parseAttributes(char * endSrc);
CELL * parseDoc(void);
CELL * parseNormalTag(char * endSrc, char * newSrc);
CELL * parseTag(char * closeTagStr);
CELL * patternMatchL(CELL * pattern, CELL * list, int flag);
CELL * patternMatchS(char * pattern, char * string);
CELL * ping(CELL * address, int timeout, int mode, int count, int option);
CELL * popString(CELL * str, CELL * params);
CELL * pushOnString(CELL * newStr, CELL * str, CELL * idx);
CELL * println(CELL * params, int flag);
CELL * repeat(CELL * params, int type);
CELL * setDefine(SYMBOL * symbol, CELL * params, int force);
CELL * setInterDiff(CELL * params, int mode);
CELL * setRef(CELL * params, int mode);
CELL * strUpperLower(CELL * params, int type);
CELL * stuffFloat(double floatPtr);
CELL * stuffInteger(UINT contents);
#ifndef NEWLISP64
CELL * stuffInteger64(INT64 contents);
#endif
CELL * stuffIntegerList(int argc, ...);
CELL * stuffBigint(char * token);
CELL * stuffString(char * string);
CELL * stuffStringN(char * string, int len);
CELL * stuffSymbol(SYMBOL * sPtr);
CELL * sublist(CELL * list, ssize_t offset, ssize_t length);
CELL * substring(char * string, ssize_t slen, ssize_t offset, ssize_t len);
CELL * sysEvalString(char * str, SYMBOL * context, CELL * proc, int mode);
CELL * whileUntil(CELL * params, int whileUntilFlag);
FILE * getIOstream(int handle);
FILE * serverFD(int port, char * domain, int reconnect);
FILE * win_fdopen(int handle, const char * mode);
SYMBOL * createRootContext(char * token);
SYMBOL * getCreateContext(CELL * params, int eval);
SYMBOL * getDynamicSymbol(CELL * cell);
SYMBOL * getSymbolCheckProtected(CELL * params);
SYMBOL * lookupSymbol(char * token, SYMBOL * context);
SYMBOL * makeSafeSymbol(CELL * cell, SYMBOL * context, int flag);
SYMBOL * translateCreateSymbol(char * token, int type, SYMBOL * context, int forceFlag);
size_t adjustNegativeIndex(ssize_t index, size_t length);
ssize_t searchBuffer(char * buffer,  size_t length, char * string, size_t size, int caseFlag);
ssize_t searchBufferRegex(char * string,  size_t offset, char * pattern, size_t length, int options, size_t * len);
INT64 fileSize(char * pathName);
char * cellToString(CELL * cell, size_t * size, int quoteFlag);
char * getFormatType(char * fmt, int * type);
char * getLocalPath(char * fileUrlName);
char * netLastError(void);
char * parsePackFormat(char * format, int * length, int * type);
char * prompt(void);
char * readStreamText(STREAM * stream, int * size);
char * replaceString (char * keyStr, size_t keyLen, char * buff, size_t buffLen, CELL * exprCell, 
    					UINT * cnt, int options, size_t * newLen);
#ifdef WINDOWS
char * win_fgets(char * buffer, int  size, FILE * fPtr);
char * win_realpath(const char * filepath, char * realpath);
char * win_getExePath(char *);
#endif

char * which(char * name, char * path);
size_t Curl_base64_encode(const char *inp, size_t insize, char **outptr);
size_t Curl_base64_decode(const char *src, char *dest);
double getDirectFloat(CELL * param);
int compareArrays(CELL * left, CELL * right);
int compareCells(CELL * left, CELL * right);
int compareFloats(CELL * left, CELL * right);
int compareFunc(CELL * left, CELL * right, CELL * func);
int compareLists(CELL * left, CELL * right);
int compileExpression(STREAM * stream, CELL * cell);
IO_SESSION * createIOsession(int handle, int family, void *res);
int deleteIOsession(int handle);
int elmntInList(CELL * elmnt, CELL * listHead);
int evalHttpRequest(char * command, UINT outDevice);
int sendHTTPmessage(int status, char * description, char * request);
int executeHTTPrequest(char * request, int type);
int executeSymbol(SYMBOL * symbol, CELL * params, CELL * * result);
int find(char * key, char * source);
int getFlag(CELL * params);
int getToken(STREAM * stream, char * token, int * tklen);
int getRegexOptions(CELL * params);
int isLegalSymbol(char * symName);
unsigned short in_cksum(unsigned short * addr, int len);
int isDir(char * fileName);
int isFile(char * fileName, int flag);
int main(int argc, char * argv[]);
int makeStreamFromFile(STREAM * stream, char * fileName, size_t size, size_t offset);
int makeStreamFromString(STREAM * stream, char * str);
int milliSecTime(void);
INT64 microSecTime(void);
int my_setenv(const char * varName, const char * varValue, int flag);
int my_strnicmp(char * s1, char * s2, ssize_t size);
int my_vasprintf(char * * buffer, const char * format, va_list argptr);
int netConnect(char * name, int port, int type, int protocol, int ttl);
int netConnectLocal(char * path);
int netAccept(int listenSock);
int netListenLocal(char * name);
int netListenOrDatagram(int portNo, int type, char * ifAddr, char * mcAddr, int option);
int openFile(char * fileName, char * accessMode, char * option);
int process(char * command, int inpipe, int outpipe, int errpipe);
int semaphore(UINT sem_id, int value, int type);
int sendall(struct socket *, char * buffer, int len);
int sortFunc(const void * left, const void *right);
int timediff_ms(struct timeval end, struct timeval start);
int waitPeek(int handle, int microsecs);
int wait_ready(int sock, INT64 wait, int mode);
int win_fclose(FILE * fPtr);
int win_fgetc(FILE * fPtr);
int win_fprintf(FILE * fPtr, char * dummy, char * buffer);
int writeFile(char * fileName, char * buffer, size_t size, char * type);
size_t listlen(CELL * listHead);
ssize_t convertNegativeOffset(ssize_t offset, CELL * list);
ssize_t readFile(char * fileName, char * * buffer);
unsigned int asciiIPtoLong(char *ptr);
unsigned int update_crc(unsigned int crc, unsigned char *buf, int len);

void * allocMemory(size_t nbytes);
void * callocMemory(size_t nbytes);
void * reallocMemory(void * prevPtr, UINT size);
void addList(CELL * list, CELL * newCell);
void allocBlock(void);
void cardioBeat(void);
void cleanupResults(UINT * from);
void closeStrStream(STREAM * stream);
void closeTrace(void);
void freeCellBlocks();
void returnBlockMemory(void);
void collectSymbols(SYMBOL * context, CELL * list);
void deleteArray(CELL * cell);
void deleteAndFreeSymbol(SYMBOL * sPtr, int checkReferences);
void deleteList(CELL * cell);
void deleteTagStack(void);
void encryptPad(char * encrypted, char * data, char * key, size_t dataLen, size_t keyLen);
void executeCommandLine(char * command, UINT outDevice, STREAM * cmdStream);
void expandExprSymbol(CELL * cell, SYMBOL * sPtr);
void errorMissingPar(STREAM * stream);
void fatalError(int errorNumber, CELL * expr, int deleteFlag);
#ifdef FFI
void initFFI(void);
#endif
void initLocale(void);
void initStacks(void);
void initialize(void);
void initDefaultInAddr(void);
void linkCell(CELL * left, CELL * right, int linkFlag);
void makeContextFromSymbol(SYMBOL * symbol, SYMBOL * treePtr);
void mySleep(int ms);
void myNanoSleep(int nanosec);
void openStrStream(STREAM * stream, size_t buffSize, int reopenFlag);
void openTrace(void);
void parseClosing(void);
void parseDTD(void);
void parseProcessingInstruction(void);
void prettyPrint(UINT device);
void printArray(CELL * array, UINT device);
void printArrayDimensions(CELL * array, UINT device);
void printCell(CELL * cell, UINT printFlag, UINT device);
void printErrorMessage(UINT errorNumber, CELL * expr, int deleteFlag);
void printExpression(CELL * cell, UINT device);
void printLambda(SYMBOL * sPtr, UINT device);
void printString(char * str, UINT device, int size);
void printSymbol(SYMBOL * sPtr, UINT device);
void printSymbolName(UINT device, SYMBOL * sPtr);
void printSymbolNameExt(UINT device, SYMBOL * sPtr);
void protectDefaultSymbol(char * name);
void purgeSpawnList(int flag);
void recoverEnvironment(UINT * idx);
void recoverIteratorIndex(CELL * cellIdx);
void recoverObjectStack(UINT * idx);
void relinkCells(void);
void reset(void);
void saveContext(SYMBOL * sPtr, UINT device);
void saveSymbols(SYMBOL * sPtr, UINT device);
void serializeSymbols(CELL * params, UINT device);
void setupAllSignals(void);
void setupServer(int reconnect);
void setupSignalHandler(int sig, void (* handler)(int));
void signal_handler(int sig);
void sigchld_handler(int sig);
void swapEndian(char * data, int n);
void traceEntry(CELL * cell, CELL * pCell, CELL * args);
void traceExit(CELL * result, CELL * cell, CELL * pCell, CELL * args);
void tracePrint(char * label, CELL * expr);
void updateCell(CELL * cell, CELL * newCell);
void varPrintf(UINT device, char *format, ...);
void writeLog(char * text, int newLine);
void writeStreamChar(STREAM * stream, char chr);
void writeStreamStr(STREAM * stream, char * buff, size_t length);
size_t appendCellString(CELL * cell, char * buffer, size_t size);
UINT * copyArray(CELL * params);
UINT64 timediff64_us(struct timeval end, struct timeval start);

#ifdef BIGINT
int * getBigintSizeDirect(CELL * params, int * * numPtr, int * len);
int * mulBigint(int * x, int nx, int * y, int ny, int * p, int * n);
int * divModBigint(int * x, int nx, int * y, int ny, int rmndr, int * n);
int * addBigint(int * x, int nx, int * y, int ny, int * sm, int * nsm);
int * subBigint(int * x, int nx, int * y, int ny, int * sm, int * nsm);
int cmpBigint(int * x, int nx, int * y, int ny);
int cmpAbsBigint(int * x, int nx, int * y, int ny);
int * strToBigint(char * str, int len, int * intlen);
char * bigintToDigits(int * num, int n, int offset, int * slen);
INT64 bigintToInt64(CELL * cell);
double bigintCellToFloat(CELL * cell);
double bigintToAbsFloat(int * num, int n);
CELL * p_bigInt(CELL * params);
int * intToBigint(INT64 num, int * len);
int * floatToBigint(double fnum, int * len);
int lengthBigint(int * num, int len);
#endif

#ifdef SUPPORT_UTF8
int utf8_1st_len(char * utf8str);
int utf8_wstr(int * unicode, char * utf8str, int maxwc);
int wstr_utf8(char * utf8str, int * unicode, int maxstr);
char * utf8_wchar(char * utf8str, int * chr);
size_t utf8_wlen(char * utf8str, char * limit);
int wchar_utf8(int cvalue, char *buffer);
char * utf8_index(char * utf8str, int idx);

#ifdef USE_WIN_UTF16PATH
WCHAR *ansi_mbcs_to_utf16(const char *);
char * utf16_to_utf8(const WCHAR *utf16str);
WCHAR * utf8_to_utf16(const char *utf8str);
int utf16_to_utf8ptr(const WCHAR *utf16str, char * utf8str, int size) ;
INT64 fileSize_utf16(char * pathName8);
char *win_realpath(const char *filepath, char *realpath);
int	rename_utf16(const char* oldname8, const char* newname8);
int stat_utf16(const char* filename8, struct stat* buf);
int chdir_utf16(const char* filename8);
int open_utf16(const char* filename8, int flags, int mode);
int mkdir_utf16(const char* filename8);
int rmdir_utf16(const char* filename8);
int unlink_utf16(const char* filename8);
_WDIR * opendir_utf16(const char* dirname8);
#endif
#endif

/* eof */

#endif /* PROTOS_H */
