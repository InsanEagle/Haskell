/* --------------------------------------------------------------------------
 * This is the Hugs foreign function interface
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-2000, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: ffi.c,v $
 * $Revision: 1.2 $
 * $Date: 2001/02/12 19:53:46 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"

extern String scriptFile;

static FILE* out = NIL;                /* file we're generating code into */

Void ffi(what)
Int what; {
    switch (what) {
        case RESET   : if (out) {
                           fclose(out);
                           out = NIL;
                       }
		       break;
    }
}

Void foreignHeader() {
    String fnm = mkFFIFilename(scriptFile);
    FILE* f = fopen(fnm,"w");
    if (f == NULL) {
        ERRMSG(0) "Unable to create file '%s'", fnm
            EEND;
    }
    out = f;
    fprintf(out,"/* Machine generated file, do not modify */\n");
    fprintf(out,"#include \"GreenCard.h\"\n");
}

Void foreignFooter(is,es,ls)
List is;
List es;
List ls; {
    List xs;
    fprintf(out,"\n");

    /* Table of all primitives generated by foreign imports */
    fprintf(out,"static struct primitive primTable[] = {\n");
    for(xs=is; nonNull(xs); xs=tl(xs)) {
        Name n       = hd(xs);
        fprintf(out,"    {\"%s\", ",textToStr(name(n).text));
        fprintf(out,"%d, ",name(n).arity);
        fprintf(out,"hugsprim_%s},\n",textToStr(name(n).extFun));
    }
    for(xs=es; nonNull(xs); xs=tl(xs)) {
        Name n       = hd(xs);
        Text ext     = name(n).extFun;
        Bool dynamic = inventedText(ext);
        if (dynamic) {
            fprintf(out,"    {\"%s\", 3, ",textToStr(name(n).text));
            fprintf(out,"hugsprim_%s},\n",textToStr(name(n).extFun));
        }
    }
    for(xs=ls; nonNull(xs); xs=tl(xs)) {
        Name n       = hd(xs);
        fprintf(out,"    {\"%s\", 0, ",textToStr(name(n).text));
        fprintf(out,"hugsprim_%s},\n",textToStr(name(n).extFun));
    }
    fprintf(out,"};\n");
    fprintf(out,"\n");

    /* The control function: rebuilds stable ptr table on RESET */
    fprintf(out,
            "static void primControl Args((int));\n"
            "static void primControl(what)\n"
            "int what; {\n");

    if (nonNull(es)) {
      fprintf(out,
	      "    switch (what) {\n"
              "        case %d:\n", RESET
	      );
    }

    for(xs=es; nonNull(xs); xs=tl(xs)) {
        Name n       = hd(xs);
        Text ext     = name(n).extFun;
        Bool dynamic = inventedText(ext);
        if (!dynamic) {
            fprintf(out, "        hugs_stable_for_%s = ", textToStr(ext));
            fprintf(out, "hugs->lookupName(");
            fprintf(out, "\"%s\"", textToStr(module(name(n).mod).text));
            fprintf(out, ", \"%s\"", textToStr(name(n).text));
            fprintf(out, ");\n");
        }
    }
    if (nonNull(es)) {
      fprintf(out,"    }\n");
    }
    fprintf(out, "}\n");

    /* Boilerplate initialization function */
    fprintf(out,
           "static struct primInfo prims = { primControl, primTable, 0 };\n"
           "\n"
           "#ifdef __cplusplus\n"
           "extern \"C\" {\n"
           "#endif\n"
           "DLLEXPORT(void) initModule(HugsAPI3 *);\n"
           "DLLEXPORT(void) initModule(HugsAPI3 *hugsAPI) {\n"
           "    hugs = hugsAPI;\n"
           "    hugs->registerPrims(&prims);\n"
           "}\n"
           "#ifdef __cplusplus\n"
           "}\n"
           "#endif\n"
            "\n");

    fclose(out);
    out = NIL;
}

static Void local foreignType(l,t)
Int    l;
Type   t; {
    if      (t == typeInt)    fprintf(out,"int");
    else if (t == typeWord)   fprintf(out,"unsigned int");
    else if (t == typeAddr)   fprintf(out,"void*");
    else if (t == typeFloat)  fprintf(out,"float");
    else if (t == typeDouble) fprintf(out,"double");
    else if (t == typeChar)   fprintf(out,"char");
    else if (t == typeForeign)fprintf(out,"HugsForeign");
    else if (t == typeStable) fprintf(out,"HugsStable");
    else if (t == typeBool)   fprintf(out,"int");
    else {
        ERRMSG(l) "Illegal foreign type" ETHEN
        ERRTEXT " \"" ETHEN ERRTYPE(t);
        ERRTEXT "\""
        EEND;
   }
}

static Void local foreignGet(l,t,nm,num)
Int    l;
Type   t; 
String nm; 
Int    num; {
    if      (t == typeInt)    fprintf(out,"%s%d = hugs->getInt();\n",       nm, num);
    else if (t == typeWord)   fprintf(out,"%s%d = hugs->getWord();\n",      nm, num);
    else if (t == typeAddr)   fprintf(out,"%s%d = hugs->getAddr();\n",      nm, num);
    else if (t == typeFloat)  fprintf(out,"%s%d = hugs->getFloat();\n",     nm, num);
    else if (t == typeDouble) fprintf(out,"%s%d = hugs->getDouble();\n",    nm, num);
    else if (t == typeChar)   fprintf(out,"%s%d = hugs->getChar();\n",      nm, num);
    else if (t == typeForeign)fprintf(out,"%s%d = hugs->getForeign();\n",   nm, num);
    else if (t == typeStable) fprintf(out,"%s%d = hugs->getStablePtr();\n", nm, num);
    else if (t == typeBool)   fprintf(out,"%s%d = hugs->getBool();\n",      nm, num);
    else {
        ERRMSG(l) "Illegal outbound (away from Haskell) type" ETHEN
        ERRTEXT " \"" ETHEN ERRTYPE(t);
        ERRTEXT "\""
        EEND;
   }
}

static Void local foreignPut(l,t,nm,num)
Int    l;
Type   t; 
String nm; 
Int    num; {
    if      (t == typeInt)    fprintf(out,"hugs->putInt(%s%d);\n",       nm, num);
    else if (t == typeWord)   fprintf(out,"hugs->putWord(%s%d);\n",      nm, num);
    else if (t == typeAddr)   fprintf(out,"hugs->putAddr(%s%d);\n",      nm, num);
    else if (t == typeFloat)  fprintf(out,"hugs->putFloat(%s%d);\n",     nm, num);
    else if (t == typeDouble) fprintf(out,"hugs->putDouble(%s%d);\n",    nm, num);
    else if (t == typeChar)   fprintf(out,"hugs->putChar(%s%d);\n",      nm, num);
    else if (t == typeForeign)fprintf(out,"hugs->putForeign(%s%d);\n",   nm, num);
    else if (t == typeStable) fprintf(out,"hugs->putStablePtr(%s%d);\n", nm, num);
    else if (t == typeBool)   fprintf(out,"hugs->putBool(%s%d);\n",      nm, num);
    else {
        ERRMSG(l) "Illegal outbound (away from Haskell) type" ETHEN
        ERRTEXT " \"" ETHEN ERRTYPE(t);
        ERRTEXT "\""
        EEND;
   }
}

static Void local ffiDeclareList(line,tys,prefix)   /* Declare variables */
Int    line;
List   tys; 
String prefix; {
    Int  i;
    for(i=1; nonNull(tys); tys=tl(tys),++i) {
        fprintf(out,"    ");
        foreignType(line,hd(tys));
        fprintf(out," %s%d;\n",prefix,i);
    }
}

static Void local ffiGetList(line,tys,prefix)   /* Get values from Haskell */
Int    line;
List   tys; 
String prefix; {
    Int  i;
    for(i=1; nonNull(tys); tys=tl(tys),++i) {
        fprintf(out,"    ");
        foreignGet(line,hd(tys),prefix,i);
    }
}

static Void local ffiPutList(line,tys,prefix)    /* Put values to Haskell */
Int    line;
List   tys; 
String prefix; {
    Int  i;
    for(i=1; nonNull(tys); tys=tl(tys),++i) {
        fprintf(out,"    ");
        foreignPut(line,hd(tys),prefix,i);
    }
}

static Void local ffiDeclareFun(line,extFun,indirect,extraArg,argTys,resultTys)
Int  line;
Text extFun;
Bool indirect;
Bool extraArg; /* Add a StablePtr argument? */
List argTys;
List resultTys; {
    Int  i;
    if (nonNull(resultTys)) {
        foreignType(line,hd(resultTys));
    } else {
        fprintf(out,"void");
    }
    if (indirect) {
        fprintf(out," (*%s)", textToStr(extFun));
    } else {
        fprintf(out," %s", textToStr(extFun));
    }
    fprintf(out,"(");
    if (extraArg) {
        fprintf(out,"HugsStablePtr fun1");
        if (nonNull(argTys)) {
            fprintf(out,", ");
        }
    }
    for(i=1; nonNull(argTys); argTys=tl(argTys),++i) {
        foreignType(line,hd(argTys));
        fprintf(out," arg%d",i);
        if (nonNull(tl(argTys))) {
            fprintf(out,", ");
        }
    }
    fprintf(out,")");
}

static Void local ffiCallFun(line,extFun,argTys,resultTys)
Int  line;
Text extFun;
List argTys;
List resultTys; {
    Int  i;
    fprintf(out,"    ");
    if (nonNull(resultTys)) {
        fprintf(out,"res1 = ");
    }
    fprintf(out,"%s(", textToStr(extFun));
    for(i=1; nonNull(argTys); argTys=tl(argTys),++i) {
        fprintf(out,"arg%d",i);
        if (nonNull(tl(argTys))) {
            fprintf(out,", ");
        }
    }
    fprintf(out,");\n");
}

/* Generate C code for calling C functions from Haskell.
 * The code has to be compiled with a C compiler and dynamically
 * loaded.
 *
 * For example:
 * 
 *     foreign import "extnm" name :: Int -> Float -> IO Char
 * ==>
 *     
 *     PROTO_PRIM(hugsprim_extnm);
 *     primFun(hugsprim_extnm)
 *     {
 *         int   arg1 = hugs->getInt();                             
 *         float arg2 = hugs->getFloat();
 *         char  res1 = ext_nm(arg1,arg2);
 *         hugs->putChar(res1);
 *         hugs_returnIO(1);
 *     }
 * 
 */
Void implementForeignImport(line,dynamic,extFun,argTys,resultTys,addState)
Int  line;
Bool dynamic;
Text extFun;
List argTys;
List resultTys; 
Bool addState; {
    fprintf(out,"\nPROTO_PRIM(hugsprim_%s);\n",textToStr(extFun));
    fprintf(out,
            "primFun(hugsprim_%s)\n"
            "{\n",
            textToStr(extFun)
            );
    if (!dynamic) {
        /* Prototype for function we're going to call */
        fprintf(out,"    extern ");
        ffiDeclareFun(line,extFun,FALSE,FALSE,argTys,resultTys);
        fprintf(out,";\n");
    }
    /* Declare arguments and result */
    if (dynamic) {
        fprintf(out,"    ");
        ffiDeclareFun(line,extFun,TRUE,FALSE,argTys,resultTys);
        fprintf(out,";\n");
    }
    ffiDeclareList(line,argTys,"arg");
    ffiDeclareList(line,resultTys,"res");
    if (dynamic) {
        fprintf(out,"    %s = hugs->getAddr();\n", textToStr(extFun));
    }
    ffiGetList(line,argTys,"arg");
    ffiCallFun(line,extFun,argTys,resultTys);
    ffiPutList(line,resultTys,"res");
    if (addState) {
        fprintf(out,"    hugs_returnIO(%d);\n", length(resultTys));
    } else {
        fprintf(out,"    hugs_returnId(%d);\n", length(resultTys));
    }
    fprintf(out,"}\n");
}

/* 
 * Generate C code for calling C functions from Haskell.
 * The code has to be compiled with a C compiler and dynamically
 * loaded.
 *
 * For example:
 * 
 *     foreign export "extnm" name :: Int -> Float -> IO Char
 * ==>
 *     
 *     HugsStablePtr hugs_stable_for_extnm = -1;
 *     char extnm(int arg1, float arg2);
 *     char extnm(int arg1, float arg2)
 *     {
 *         char  res1;
 *         hugs->putStablePtr(hugs_stable_for_extnm);
 *         hugs->putInt(arg1);                             
 *         hugs->putFloat(arg2);                             
 *         if (hugs->runIO(2)) {
 *             exit(hugs->getInt());
 *         }
 *         res1 = hugs->getChar();
 *         return res1;
 *     }
 *
 * For dynamic exports, we also generate:
 *
 *     PROTO_PRIM(hugsprim_name);
 *     primFun(hugsprim_name)
 *     {
 *         HugsStablePtr arg1 = hugs->getStablePtr();    
 *         void* thunk = hugs_mkThunk(extnm,arg1);
 *         hugs->putAddr(thunk);
 *         hugs_returnIO(1);
 *     }
 */
Void implementForeignExport(line,dynamic,extFun,argTys,resultTys,addState)
Int  line;
Bool dynamic;
Text extFun;
List argTys;
List resultTys;
Bool addState; {
    /* ToDo: calling convention */

    /* Prototype for function we're generating */
    fprintf(out,"\nextern ");
    ffiDeclareFun(line,extFun,FALSE,dynamic,argTys,resultTys);
    fprintf(out,";\n");

    if (!dynamic) {
        fprintf(out,"static HugsStablePtr hugs_stable_for_%s = -1;\n", textToStr(extFun));
    }

    /* The function wrapper */
    ffiDeclareFun(line,extFun,FALSE,dynamic,argTys,resultTys);
    fprintf(out,"\n{\n");
    ffiDeclareList(line,resultTys,"res");

    /* Push function pointer and arguments */
    if (dynamic) {
        fprintf(out,"    hugs->putStablePtr(fun1);\n");
    } else {
        fprintf(out,"    hugs->putStablePtr(hugs_stable_for_%s);\n", textToStr(extFun));
    }
    ffiPutList(line,argTys,"arg");

    /* Make the call and check for uncaught exception */
    if (addState) {
        /* ToDo: I'm not sure that exiting from the Hugs session is the right 
         * response to the Haskell function calling System.exit.
         */
        fprintf(out,"    if (hugs->runIO(%d)) {\n", length(argTys));
        fprintf(out,
                "        exit(hugs->getInt());\n"
                "    }\n"
                );
    } else {
        fprintf(out,"    hugs->ap(%d);\n", length(argTys));
    }
    if (nonNull(resultTys)) {
        ffiGetList(line,resultTys,"res");
    } else {
        fprintf(out,"hugs->getUnit();\n");
    }

    /* Return result */
    if (nonNull(resultTys)) {
        fprintf(out,"    return res1;\n");
    }
    fprintf(out,"}\n");

    if (dynamic) {
        fprintf(out,
                "PROTO_PRIM(hugsprim_%s);\n", textToStr(extFun));
        fprintf(out,
                "primFun(hugsprim_%s)\n", textToStr(extFun));
        fprintf(out,
                "{\n"
                "    HugsStablePtr arg1 = hugs->getStablePtr();\n"
                "    void* thunk = hugs->mkThunk(%s,arg1);\n",
                textToStr(extFun)
                );
        fprintf(out,
                "    hugs->putAddr(thunk);\n"
                "    hugs_returnIO(1);\n"
                "}\n");
    }
}

/* 
 * Generate primitive for address of a C symbol.
 *
 * For example:
 * 
 *     foreign label "extnm" name :: Addr
 * ==>
 *     
 *     extern int name; // probably the wrong type but it doesn't matter 
 *     PROTO_PRIM(hugsprim_extnm);
 *     primFun(hugsprim_extnm)
 *     {
 *         hugs->putAddr(&name);
 *         hugs_returnId(1);
 *     }
 */
Void implementForeignLabel(line,extFun,nm)
Int  line;
Text extFun;
Text nm; {
    fprintf(out,
            "\nPROTO_PRIM(hugsprim_%s);\n", textToStr(extFun));
    fprintf(out,
            "primFun(hugsprim_%s)\n", textToStr(extFun));
    fprintf(out,"{\n");
    fprintf(out,
            "    extern int %s;\n", textToStr(extFun));
    fprintf(out,
            "    hugs->putAddr(&%s);\n", textToStr(extFun));
    fprintf(out,
            "    hugs_returnId(1);\n"
            "}\n");
}

/* ToDo: 
 * chain all foreign exports together and free at end of run?
 * copy GreenCard.h into Test.c? 
 */

/*-------------------------------------------------------------------------*/