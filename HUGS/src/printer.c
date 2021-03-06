/* --------------------------------------------------------------------------
 * Builtin printer, used as an alternative to overloaded "show", and also
 * used for certain primitive types.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: printer.c,v $
 * $Revision: 1.4 $
 * $Date: 2001/01/02 18:21:41 $
 * ------------------------------------------------------------------------*/

static Void   local printer		Args((Name,Int));
static Void   local outName   		Args((Name));
static Void   local outVar		Args((Name));
static Void   local outOp		Args((Name));
static Void   local outStr		Args((String));
static Void   local outPr		Args((Name,Int,Cell));
static Void   local outLPr		Args((Name,Cell));
static Void   local outBadRedex		Args((Cell));
static Cell   local printDBadRedex	Args((Cell,Cell));
static Cell   local printBadRedex	Args((Cell,Cell));

static Name nameLPrint, nameNLPrint;	/* list printing primitives	   */
static Name nameSPrint, nameNSPrint;	/* string printing primitives	   */

static Cell out;			/* GC'd var used by printer code   */

#define outCh(c)	out = ap(consChar(c),out)
#define outSCh(c)	outStr(unlexChar(c,'\"'))
#define updOutRoot(ss)  out = revOnto(out,ss);		\
			updapRoot(fst(out),snd(out));	\
			out = NIL;

/* --------------------------------------------------------------------------
 * Printer control:
 * ------------------------------------------------------------------------*/

static Void printerControl Args((Int));
static Void printerControl(what)
Int what; {
    switch (what) {
	case MARK    : mark(out);
		       break;
	case INSTALL : 
		       setCurrModule(modulePrelude);
#define pFun(n,s,t)    addPrim(0,n=newName(findText(s),NIL),t,modulePrelude,NIL)
		       pFun(namePrint,	   "_print",   "print");
		       pFun(nameNPrint,	   "_nprint",  "nprint");
		       pFun(nameLPrint,	   "_lprint",  "lprint");
		       pFun(nameNLPrint,   "_nlprint", "nlprint");
		       pFun(nameSPrint,	   "_sprint",  "sprint");
		       pFun(nameNSPrint,   "_nsprint", "nsprint");
#undef pFun
		       /* deliberate fall through */
	case RESET   : out = NIL;
		       break;
    }
}

PROTO_PRIM(primPrint);
PROTO_PRIM(primBPrint);
PROTO_PRIM(primNPrint);
PROTO_PRIM(primLPrint);
PROTO_PRIM(primNLPrint);
PROTO_PRIM(primSPrint);
PROTO_PRIM(primNSPrint);

static struct primitive printerPrimTable[] = {
  {"print",		3, primPrint},
  {"nprint",		3, primNPrint},
  {"lprint",		2, primLPrint},
  {"nlprint",		2, primNLPrint},
  {"sprint",		2, primSPrint},
  {"nsprint",		2, primNSPrint},
  {"primShowsInt",	3, primBPrint},
  {"primShowsAddr",	3, primBPrint},
  {"primShowsInteger",	3, primBPrint},
  {"primShowsFloat",	3, primBPrint},
  {"primShowsDouble",	3, primBPrint},
  {0,			0, 0}
};

static struct primInfo printerPrims = { printerControl, printerPrimTable, 0 };

/*-------------------------------------------------------------------------*/

primFun(primPrint) {			/* Evaluate and print term	   */
    Cell temp;				/*    :: Int->Expr->[Char]->[Char] */
    Int  d;
    eval(primArg(3));
    d    = whnfInt;
    temp = evalWithNoError(primArg(2));
    out  = NIL;
    if (nonNull(temp)) {
	push(temp);
	outBadRedex(top());
    }
    else
	printer(namePrint,d);
    updOutRoot(primArg(1));
}

primFun(primBPrint) {			/* Eval and print value of basic   */
    Int d;				/* type -- Int, Integer, Float, or */
    eval(primArg(3));			/* Double -- as a string.	   */
    d   = whnfInt;
    eval(primArg(2));			/* Differs from primPrint only in  */
    out = NIL;				/* its use of weaker error handling*/
    printer(namePrint,d);		/* to make showsPrec strict.	   */
    updOutRoot(primArg(1));
}

primFun(primNPrint) {			/* print term without evaluation   */
    Int  d;				/*     :: Int->Expr->[Char]->[Char] */
    eval(primArg(3));
    d   = whnfInt;
    unwind(primArg(2));
    out = NIL;
    printer(nameNPrint,d);
    updOutRoot(primArg(1));
}

static Void local printer(pr,d)		/* Main part: primPrint/primNPrint */
Name pr;				/* printer to use on components	   */
Int  d; {				/* precedence level		   */
    Int used = 0;			/* Output, in reverse, to "out"	   */

    allowBreak();
    switch(whatIs(whnfHead)) {

	case NAME     : {   Syntax sy = syntaxOf(whnfHead);

			    if (!isCfun(whnfHead) ||
				    name(whnfHead).arity>whnfArgs)
				pr = nameNPrint;

			    if (whnfHead==nameCons && whnfArgs==2) {/*list */
				StackPtr ksp = sp;
				used 	     = 2;
				if (pr==namePrint) {
				    Cell temp = evalWithNoError(top());
				    if (nonNull(temp)) {
					push(temp);
					outCh('[');
					outBadRedex(top());
					sp = ksp;
					outLPr(nameLPrint,pushed(1));
				    }
				    else if (isChar(whnfHead) && whnfArgs==0) {
					outCh('"');
					outSCh(charOf(whnfHead));
					outLPr(nameSPrint,pushed(1));
				    }
				    else {
					sp = ksp;
					outCh('[');
					outPr(namePrint,MIN_PREC,top());
					outLPr(nameLPrint,pushed(1));
				    }
				}
				else {
				    unwind(top());
				    if (isChar(whnfHead) && whnfArgs==0) {
					outCh('"');
					outSCh(charOf(whnfHead));
					outLPr(nameNSPrint,pushed(1));
				    }
				    else {
					sp = ksp;
					outCh('[');
					outPr(nameNPrint,MIN_PREC,top());
					outLPr(nameNLPrint,pushed(1));
				    }
				}
				return;
			    }
			    else if (whnfArgs==1 && sy!=APPLIC) { /* (e1+) */
				used = 1;
				outCh('(');
				outPr(pr,FUN_PREC-1,pushed(0));
				outCh(' ');
				outOp(whnfHead);
				outCh(')');
			    }
			    else if (whnfArgs>=2 && sy!=APPLIC) { /* e1+e2 */
				Syntax a = assocOf(sy);
				Int    p = precOf(sy);
				used     = 2;
				if (whnfArgs>2 || d>p)
				     outCh('(');
				outPr(pr,(a==LEFT_ASS? p:1+p),pushed(0));
				outCh(' ');
				outOp(whnfHead);
				outCh(' ');
				outPr(pr,(a==RIGHT_ASS?p:1+p),pushed(1));
				if (whnfArgs>2 || d>p)
				    outCh(')');
			    }
			    else				  /* f ... */
				outVar(whnfHead);
			}
			break;

#if BIGNUMS
	case NEGNUM   :
	case ZERONUM  :
	case POSNUM   : out = rev(bigOut(whnfHead,NIL,d>=UMINUS_PREC));
			pr  = nameNPrint;
			break;
#endif

	case INTCELL  : {   Int digit;

			    if (intOf(whnfHead)<0 && d>=UMINUS_PREC)
				outCh(')');

			    do {
				digit = whnfInt%10;
				if (digit<0)
				    digit= (-digit);
				outCh('0'+digit);
			    } while ((whnfInt/=10)!=0);

			    if (intOf(whnfHead)<0) {
				outCh('-');
				if (d>=UMINUS_PREC)
				    outCh('(');
			    }

			    out = rev(out);
			    pr  = nameNPrint;
			}
			break;

	case PTRCELL  : {   Pointer p = ptrOf(whnfHead);
			    char buffer[32];
			    sprintf(buffer,"%p",p);
			    outStr(buffer);
			    pr  = nameNPrint;
			}
			break;

	case TUPLE    : {   Int tn   = tupleOf(whnfHead);
			    Int punc = '(';
			    Int i;

			    used     = tn<whnfArgs ? tn : whnfArgs;
			    for (i=0; i<used; ++i) {
				outCh(punc);
				outPr(pr,MIN_PREC,pushed(i));
				punc = ',';
			    }
			    for (; i<tn; ++i) {
				outCh(punc);
				punc = ',';
			    }
			    outCh(')');
			}
			pr = nameNPrint;
			break;

	case CHARCELL : outCh('\'');
			outStr(unlexChar(charOf(whnfHead), '\''));
			outCh('\'');
			pr = nameNPrint;
			break;

	case FLOATCELL: if (whnfFloat<0.0 && d>=UMINUS_PREC)
			    outCh('(');
			outStr(floatToString(whnfFloat));
			if (whnfFloat<0.0 && d>=UMINUS_PREC)
			    outCh(')');
			pr = nameNPrint;
			break;

#if HASKELL_ARRAYS
	case ARRAY    : outStr("{array}");
			pr = nameNPrint;
			break;
#endif

#if LAZY_ST
	case MUTVAR   : outStr("{mutable variable}");
			pr = nameNPrint;
			break;
#endif

#if INTERNAL_PRIMS
	case HUGSOBJECT: 
			outStr("{Cell ...}");
			pr = nameNPrint;
			break;
#endif

#if TREX
	case RECORD   : outStr("{record}");
			pr = nameNPrint;
			break;
#endif
 
#if IO_HANDLES
	case HANDCELL : outStr("{handle}");
			pr = nameNPrint;
			break;
#endif

#if OBSERVATIONS
        case OBSERVE  : outStr("printer.c: printer(): OBSERVE");
                        break;
#endif

	default       : internal("Error in graph");
			break;
    }

    if (used<whnfArgs) {		/* Add remaining args to output	   */
	do {
	    outCh(' ');
	    outPr(pr,FUN_PREC,pushed(used));
	} while (++used<whnfArgs);

	if (d>=FUN_PREC) {		/* Determine if parens are needed  */
	    out = appendOnto(out,singleton(consChar('(')));
	    outCh(')');
	}
    }
}

/* --------------------------------------------------------------------------
 * List printing primitives:
 * ------------------------------------------------------------------------*/

primFun(primLPrint) {			/* evaluate and print list	   */
    Cell temp = evalWithNoError(primArg(2));
    out       = NIL;
    if (nonNull(temp)) {
	push(temp);
	outStr("] ++ ");
	outBadRedex(top());
    }
    else if (whnfHead==nameCons && whnfArgs==2) {
	outCh(',');
	outPr(namePrint,MIN_PREC,pushed(0));
	outLPr(nameLPrint,pushed(1));
    }
    else if (whnfHead==nameNil && whnfArgs==0)
	outCh(']');
    else {
	outStr("] ++ ");
	outBadRedex(primArg(2));
    }
    updOutRoot(primArg(1));
}

primFun(primNLPrint) {			/* print list without evaluation   */
    unwind(primArg(2));
    out = NIL;
    if (whnfHead==nameCons && whnfArgs==2) {
	outCh(',');
	outPr(nameNPrint,MIN_PREC,pushed(0));
	outLPr(nameNLPrint,pushed(1));
    }
    else if (whnfHead==nameNil && whnfArgs==0)
	outCh(']');
    else {
	outStr("] ++ ");
	outPr(nameNPrint,FUN_PREC-1,primArg(2));
    }
    updOutRoot(primArg(1));
}

primFun(primSPrint) {			/* evaluate and print string	   */
    Cell temp = evalWithNoError(primArg(2));
    out       = NIL;
    if (nonNull(temp)) {
	push(temp);
	outStr("\" ++ ");
	outBadRedex(top());
    }
    else if (whnfHead==nameCons && whnfArgs==2) {
	temp = evalWithNoError(top());	/* primArg(4), primArg(3) contain  */
	out  = NIL;			/* the head and tail of list, resp */
	if (nonNull(temp)) {
	    push(temp);
	    outStr("\" ++ [");
	    outBadRedex(top());
	    outLPr(nameLPrint,primArg(3));
	}
	else if (isChar(whnfHead) && whnfArgs==0) {
	    outSCh(charOf(whnfHead));
	    outLPr(nameSPrint,primArg(3));
	}
	else {
	    outStr("\" ++ [");
	    outBadRedex(primArg(4));
	    outLPr(nameLPrint,primArg(3));
	}
    }
    else if (whnfHead==nameNil && whnfArgs==0)
	outCh('"');
    else {
	outStr("\" ++ ");
	outBadRedex(primArg(2));
    }
    updOutRoot(primArg(1));
}

primFun(primNSPrint) {			/* print string without eval	   */
    unwind(primArg(2));
    out = NIL;
    if (whnfHead==nameCons && whnfArgs==2) {
	unwind(pushed(0));
	if (isChar(whnfHead) && whnfArgs==0) {
	    outSCh(charOf(whnfHead));
	    outLPr(nameNSPrint,primArg(3));
	}
	else {
	    outStr("\" ++ [");
	    outPr(nameNPrint,MIN_PREC,primArg(4));
	    outLPr(nameNLPrint,primArg(3));
	}
    }
    else if (whnfHead==nameNil && whnfArgs==0)
	outCh('"');
    else {
	outStr("\" ++ ");
	outPr(nameNPrint,FUN_PREC-1,primArg(2));
    }
    updOutRoot(primArg(1));
}

/* --------------------------------------------------------------------------
 * Auxiliary functions for printer(s):
 * ------------------------------------------------------------------------*/

static Void local outName(nm)  /* output nm using parent field if possible */
Name nm; {
    Cell p = name(nm).parent;
    switch (whatIs(p)) {
	case INSTANCE : outStr("inst");
			outStr(textToStr(cclass(inst(p).c).text));
			outCh('_');
			break;

	case CLASS    : outStr(textToStr(cclass(p).text));
			outCh('_');
			break;

	case TYCON    : outStr(textToStr(tycon(p).text));
			outCh('_');
			break;

	case NAME     : outName(p);
			outCh('_');
			break;
    }
    outStr(textToStr(name(nm).text));
}

static Void local outVar(nm)		/* output nm as function symbol	   */
Name nm; {
    String s = textToStr(name(nm).text);
    if ((isascii(*s) && isalpha(*s)) || *s=='_' || *s=='[' || *s=='(')
	outName(nm);
    else {
	outCh('(');
	outName(nm);
	outCh(')');
    }
}

static Void local outOp(nm)		/* output nm as operator symbol	   */
Name nm; {
    String s = textToStr(name(nm).text);
    if (isascii(s[0]) && isalpha(s[0])) {
	outCh('`');
	outName(nm);
	outCh('`');
    }
    else
	outName(nm);
}

static Void local outStr(s)		/* output string s		   */
String s; {
    while (*s)
	outCh(*s++);
}

static Void local outPr(pr,d,e)		/* output expr e with printer pr,  */
Name pr;				/* precedence d			   */
Int  d;
Cell e; {
    out           = ap(NIL,out);
    fst(out)      = ap(NIL,e);
    fst(fst(out)) = ap(pr,mkInt(d));
}

static Void local outLPr(pr,xs)		/* output list xs with printer pr  */
Name pr;
Cell xs; {
    out      = ap(NIL,out);
    fst(out) = ap(pr,xs);
}

static Void local outBadRedex(rx)	/* Produce expr to print bad redex */
Cell rx; {
    outCh('{');
    outPr(nameNPrint,MIN_PREC,rx);
    outCh('}');
}

static Cell local printDBadRedex(rx,rs)	/* Produce expression for bad	   */
Cell rx, rs; {				/* redex with special handling	   */
    if (isAp(rx) && fun(rx)==nameError) /* of {error str} redexes	   */
	return arg(rx);
    else
	return printBadRedex(rx,rs);
}

static Cell local printBadRedex(rx,rs)	/* produce expression for bad	   */
Cell rx, rs; {				/* redex			   */
   out = NIL;
   outBadRedex(rx);
   return revOnto(out,rs);
}

Void abandon(what,rx)			/* abandon computation		   */
String what;
Cell   rx; {
    push(printDBadRedex(rx,nameNil));
    out   = NIL;
    outCh('\n');
    outStr(what);
    outStr(" error: ");
    top() = revOnto(out,top());
    out   = NIL;
    outputString(errorStream);
    errAbort();
}

/*-------------------------------------------------------------------------*/
