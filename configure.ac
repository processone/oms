AC_PREREQ(2.59c)
AC_PACKAGE_VERSION(0.8.0)
AC_PACKAGE_NAME(OneTeam Media Server)
AC_INIT(OneTeam Media Server, 0.8.0, [], oms)

# Checks Erlang runtime and compiler
AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC

# Checks and sets ERLANG_ROOT_DIR and ERLANG_LIB_DIR variable
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR

# Checks for Erlang 'tools' library since make.erl is a part of it
AC_ERLANG_CHECK_LIB([tools], [],
    [AC_MSG_ERROR(['tools' was not found, unable to build Emakefile])])
AC_ERLANG_CHECK_LIB([erl_interface], [],
    [AC_MSG_ERROR(['erl_interface' was not found, unable to build Erlang drivers])])

# Checks for programs.
AC_PROG_CC
AC_PROG_MAKE_SET

if test "x$GCC" = "xyes"; then
    CFLAGS="$CFLAGS -Wall"
fi

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Checks for library functions.
AC_FUNC_MALLOC
AC_HEADER_STDC

AC_CANONICAL_SYSTEM
case "$target_os" in
    *darwin10*)
	echo "Target OS is 'Darwin10'"
	AC_LANG(Erlang)
	AC_RUN_IFELSE(
	    [AC_LANG_PROGRAM([],[dnl
	     halt(case erlang:system_info(wordsize) of
	     8 -> 0; 4 -> 1 end)])],
	    [AC_MSG_NOTICE(found 64-bit Erlang)
	     CBIT=-m64],
	    [AC_MSG_NOTICE(found 32-bit Erlang)
	     CBIT=-m32])
	;;
    *)        
	echo "Target OS is '$target_os'"
	CBIT=""
	;;
esac
CFLAGS="$CFLAGS $CBIT"
LD_SHARED="$LD_SHARED $CBIT"
echo "CBIT is set to '$CBIT'"

AC_CONFIG_FILES([Makefile
		 include/oms.hrl
		 doc/doc.tex
		 ebin/oms.app])

AC_OUTPUT
