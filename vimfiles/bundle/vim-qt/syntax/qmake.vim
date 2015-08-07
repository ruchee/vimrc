"" qmake.vim
"" Description: syntax file for qmake tool from QT lib
"" Author:		Makoto Nokata <nokatamakoto@gmail.com>
"" Version:		0.1
""

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

let b:current_syntax = "qmake"

syn case ignore
syn keyword	qmakeFunctionConst		contained basename CONFIG count dirname error eval exists find for
									\ include infile isEmpty join member message prompt quote replace
									\ sprintf system unique warning contains
syn match	qmakeFunction			"\w\+\s*("he=e-1 contains=qmakeFunctionConst,qmakeOperator,qmakeVariableValue
syn match	qmakeCONFIGFunction		"CONFIG\s*(.*)" contains=qmakeFunctionConst,qmakeOperator,
									\ qmakeVariableValue,qmakeOptionsCONFIGConst

syn match	qmakeOperator			.[\.,!@#\$%\^&\*(){}\=:;|+\-/\]\[].
syn match	qmakeComment			/#.*$/
syn match	qmakeVariableValue		/\$\{1,2\}[{(\[]\{0,1\}\w\+[})\]]\{0,1\}/ contains=qmakeVariable
syn match	qmakeVariableDefine		/\w\+\s*\(\|[+\-~*]\)=/he=e-1
									\ contains=qmakeVariable,qmakeOperator
syn match	qmakeNumber				.\d\+.
syn match	qmakePath				"\(\.\|\w\)*/\(\(\.\|\w\)*\(\|/\)\)*"
syn region	qmakeString				start=.". skip=.\\". end=.". oneline

syn region	qmakeOptionsQT			start=.QT\s*\(\|[+\-~*]\)=. skip=.\\\s*$. end=.$. keepend
									\ contains=qmakeOptionsQTConst,qmakeOperator,qmakeVariable
syn keyword	qmakeOptionsQTConst		core gui network opengl sql svg xml xmlpatterns qt3support contained

syn region	qmakeOptionsTEMPLATE	start=.TEMPLATE\s*\(\|[+\-~*]\)=. skip=.\\\s*$. end=.$. keepend
									\ contains=qmakeOptionsTEMPLATEConst,qmakeOperator,qmakeVariable
syn keyword qmakeOptionsTEMPLATEConst	app lib subdirs vcapp vclib contained

syn region	qmakeOptionsCONFIG		start=.CONFIG\s*\(\|[+\-~*]\)=. skip=.\\\s*$. end=.$. keepend
									\ contains=qmakeOptionsCONFIGConst,qmakeOperator,qmakeVariable
syn keyword qmakeOptionsCONFIGConst	release debug debug_and_release build_all ordered precompile_header
									\ warn_on warn_off create_prl link_prl qt thread x11 windows console
									\ shared dll dylib static staticlib plugin designer uic3
									\ no_lflags_merge resources 3dnow exceptions mmx rtti stl sse sse2
									\ flat embed_manifest_dll embed_manifest_exe incremental
									\ ppc x86 app_bundle lib_bundle largefile separate_debug_info contained

syn keyword qmakeVariable			CONFIG DEFINES DEF_FILE DEPENDPATH DEPLOYMENT DEPLOYMENT_PLUGIN
									\ DESTDIR DESTDIR_TARGET DLLDESTDIR DISTFILES DSP_TEMPLATE FORMS
									\ FORMS3 GUID HEADERS INCLUDEPATH INSTALLS LEXIMPLS LEXOBJECTS
									\ LEXSOURCES LIBS LITERAL_HASH MAKEFILE MAKEFILE_GENERATOR MOC_DIR
									\ OBJECTS OBJECTS_DIR OBJMOC POST_TARGETDEPS PRE_TARGETDEPS
									\ PRECOMPILED_HEADER PWD OUT_PWD QMAKE QMAKESPEC QMAKE_APP_FLAG
									\ QMAKE_APP_OR_DLL QMAKE_AR_CMD QMAKE_BUNDLE_DATA
									\ QMAKE_BUNDLE_EXTENSION QMAKE_CC QMAKE_CFLAGS_DEBUG QMAKE_CFLAGS_MT
									\ QMAKE_CFLAGS_MT_DBG QMAKE_CFLAGS_MT_DLL QMAKE_CFLAGS_MT_DLLDBG
									\ QMAKE_CFLAGS_RELEASE QMAKE_CFLAGS_SHLIB QMAKE_CFLAGS_THREAD
									\ QMAKE_CFLAGS_WARN_OFF QMAKE_CFLAGS_WARN_ON QMAKE_CLEAN QMAKE_CXX
									\ QMAKE_CXXFLAGS QMAKE_CXXFLAGS_DEBUG QMAKE_CXXFLAGS_MT
									\ QMAKE_CXXFLAGS_MT_DBG QMAKE_CXXFLAGS_MT_DLL
									\ QMAKE_CXXFLAGS_MT_DLLDBG QMAKE_CXXFLAGS_RELEASE QMAKE_CXXFLAGS_SHLIB
									\ QMAKE_CXXFLAGS_THREAD QMAKE_CXXFLAGS_WARN_OFF QMAKE_CXXFLAGS_WARN_ON
									\ QMAKE_DISTCLEAN QMAKE_EXTENSION_SHLIB QMAKE_EXT_MOC QMAKE_EXT_UI
									\ QMAKE_EXT_PRL QMAKE_EXT_LEX QMAKE_EXT_YACC QMAKE_EXT_OBJ
									\ QMAKE_EXT_CPP QMAKE_EXT_H QMAKE_EXTRA_COMPILERS QMAKE_EXTRA_TARGETS
									\ QMAKE_FAILED_REQUIREMENTS QMAKE_FILETAGS QMAKE_FRAMEWORK_BUNDLE_NAME
									\ QMAKE_FRAMEWORK_VERSION QMAKE_INCDIR QMAKE_INCDIR_OPENGL
									\ QMAKE_INCDIR_QT QMAKE_INCDIR_THREAD QMAKE_INCDIR_X11 QMAKE_INFO_PLIST
									\ QMAKE_LFLAGS QMAKE_LFLAGS_CONSOLE QMAKE_LFLAGS_CONSOLE_DLL
									\ QMAKE_LFLAGS_DEBUG QMAKE_LFLAGS_PLUGIN QMAKE_LFLAGS_QT_DLL
									\ QMAKE_LFLAGS_RELEASE QMAKE_LFLAGS_SHAPP QMAKE_LFLAGS_SHLIB
									\ QMAKE_LFLAGS_SONAME QMAKE_LFLAGS_THREAD QMAKE_LFLAGS_WINDOWS
									\ QMAKE_LFLAGS_WINDOWS_DLL QMAKE_LIBDIR QMAKE_LIBDIR_FLAGS
									\ QMAKE_LIBDIR_OPENGL QMAKE_LIBDIR_QT QMAKE_LIBDIR_X11 QMAKE_LIBS
									\ QMAKE_LIBS_CONSOLE QMAKE_LIBS_OPENGL QMAKE_LIBS_OPENGL_QT
									\ QMAKE_LIBS_QT QMAKE_LIBS_QT_DLL QMAKE_LIBS_QT_OPENGL
									\ QMAKE_LIBS_QT_THREAD QMAKE_LIBS_RT QMAKE_LIBS_RTMT
									\ QMAKE_LIBS_THREAD QMAKE_LIBS_WINDOWS QMAKE_LIBS_X11 QMAKE_LIBS_X11SM
									\ QMAKE_LIB_FLAG QMAKE_LINK_SHLIB_CMD QMAKE_POST_LINK QMAKE_PRE_LINK
									\ QMAKE_LN_SHLIB QMAKE_MAC_SDK QMAKE_MACOSX_DEPLOYMENT_TARGET
									\ QMAKE_MAKEFILE QMAKE_MOC_SRC QMAKE_QMAKE QMAKE_QT_DLL
									\ QMAKE_RESOURCE_FLAGS QMAKE_RUN_CC QMAKE_RUN_CC_IMP QMAKE_RUN_CXX
									\ QMAKE_RUN_CXX_IMP QMAKE_TARGET QMAKE_UIC QT QTPLUGIN QT_VERSION
									\ QT_MAJOR_VERSION QT_MINOR_VERSION QT_PATCH_VERSION RC_FILE RCC_DIR
									\ REQUIRES RESOURCES RES_FILE SIGNATURE_FILE SOURCES SRCMOC SUBDIRS
									\ TARGET TARGET_EXT TEMPLATE TRANSLATIONS UICIMPLS UICOBJECTS UI_DIR
									\ UI_HEADERS_DIR UI_SOURCES_DIR VERSION VER_MAJ VER_MIN VER_PAT VPATH
									\ YACCIMPLS YACCOBJECTS YACCSOURCES _PRO_FILE_ _PRO_FILE_PWD_ contained
syn match	qmakeVariable			.TARGET_\d\+\(\|\.\d\+\.\d\+\). contained

hi def link qmakeVariable				Statement
hi def link qmakeVariableValue			Type
hi def link qmakeVariableDefine			Type
hi def link qmakeComment				Comment
hi def link qmakeString					String
hi def link qmakeFunctionConst			Function
hi def link qmakeOperator				Operator
hi def link qmakeNumber					Number
hi def link qmakePath					qmakeString
hi def link qmakeOptionsQTConst			Constant
hi def link qmakeOptionsTEMPLATEConst	Constant
hi def link qmakeOptionsCONFIGConst		Constant

