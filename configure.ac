# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.57)
AC_INIT([RTx::AssetTracker], [1.0.HEAD], [todd@chaka.net])

AC_CONFIG_SRCDIR([lib/RTx/AssetTracker.pm.in])

dnl Extract AT version number components
AC_SUBST([at_version_major],
        m4_bregexp(AC_PACKAGE_VERSION,[^\(\w+\)\.\(\w+\)\(\.\(\w+\)\)?],[\1]))
AC_SUBST([at_version_minor],
        m4_bregexp(AC_PACKAGE_VERSION,[^\(\w+\)\.\(\w+\)\(\.\(\w+\)\)?],[\2]))
AC_SUBST([at_version_patch],
        m4_bregexp(AC_PACKAGE_VERSION,[^\(\w+\)\.\(\w+\)\(\.\(\w+\)\)?],[\4]))
test "x$at_version_major" = 'x' && at_version_major=0
test "x$at_version_minor" = 'x' && at_version_minor=0
test "x$at_version_patch" = 'x' && at_version_patch=0

dnl Check for programs
AC_PROG_INSTALL
AC_ARG_VAR([PERL],[Perl interpreter command])
AC_PATH_PROG([PERL], [perl], [not found])
if test "$PERL" = 'not found'; then
        AC_MSG_ERROR([cannot use $PACKAGE_NAME without perl])
fi

# Checks for programs.
#AC_PROG_INSTALL

AC_ARG_WITH(rt-home,
            AC_HELP_STRING([--with-rt-home=/path/to/rt],
                           [path to your rt home dir (default: /opt/rt3)]),
            RT_HOME_PATH=$withval,
            RT_HOME_PATH=/opt/rt3)
AC_SUBST(RT_HOME_PATH)

AC_ARG_WITH(rt-lib,
            AC_HELP_STRING([--with-rt-lib=/path/to/rt/lib],
                           [path to your rt lib dir (default: /opt/rt3/lib)]),
            RT_LIB_PATH=$withval,
            RT_LIB_PATH=/opt/rt3/lib)
AC_SUBST(RT_LIB_PATH)

AC_ARG_WITH(rt-local,
            AC_HELP_STRING([--with-rt-local=/path/to/rt/local],
                           [path to your rt local dir (default: /opt/rt3/local)]),
            RT_LOCAL_PATH=$withval,
            RT_LOCAL_PATH=/opt/rt3/local)
AC_SUBST(RT_LOCAL_PATH)

AC_ARG_WITH(rt-localhtml,
            AC_HELP_STRING([--with-rt-localhtml=/path/to/rt/localhtml],
                           [path to your rt local html dir (default: /opt/rt3/local/html)]),
            RT_LOCALHTML_PATH=$withval,
            RT_LOCALHTML_PATH=/opt/rt3/local/html)
AC_SUBST(RT_LOCALHTML_PATH)

AC_ARG_WITH(db-admin,
            AC_HELP_STRING([--with-db-admin=db_admin],
                           [name of database administrator (default: root)]),
            DB_ADMIN=$withval,
            DB_ADMIN=root)
AC_SUBST(DB_ADMIN)

# ACRT_USER_EXISTS( users, variable, default )
# - users is a list of users [www apache www-docs]
#    from highest to lowest priority to high priority (i.e. first match)
# - variable is what you set with the result
#

AC_DEFUN([ACRT_USER_GUESS],
 [
   $2=$3
   for x in $1; do
    AC_MSG_CHECKING([if user $x exists])
     AS_IF([ $PERL -e"exit( defined getpwnam('$x') ? 0 : 1)" ],
           [ AC_MSG_RESULT([found]); $2=$x ; break],
           [ AC_MSG_RESULT([not found]) ])
   done
  ])
AC_DEFUN([ACRT_GROUP_GUESS],
 [
   $2=$3
   for x in $1; do
    AC_MSG_CHECKING([if group $x exists])
     AS_IF([ $PERL -e"exit( defined getgrnam('$x') ? 0 : 1)" ],
           [ AC_MSG_RESULT([found]); $2=$x ; break],
           [ AC_MSG_RESULT([not found]) ])
   done
  ])


dnl RTGROUP
AC_ARG_WITH(rt-group,
            AC_HELP_STRING([--with-rt-group=GROUP],
                           [group to own all files (default: rt)]),
            RTGROUP=$withval,
            ACRT_GROUP_GUESS([rt3 rt $WEB_GROUP],[RTGROUP], [rt]))
AC_SUBST(RTGROUP)

dnl BIN_OWNER
AC_ARG_WITH(bin-owner,
            AC_HELP_STRING([--with-bin-owner=OWNER],
                           [user that will own RT binaries (default root)]),
            BIN_OWNER=$withval,
            BIN_OWNER=root)
AC_SUBST(BIN_OWNER)

dnl LIBS_OWNER
AC_ARG_WITH(libs-owner,
            AC_HELP_STRING([--with-libs-owner=OWNER],
                           [user that will own RT libraries (default root)]),
            LIBS_OWNER=$withval,
            LIBS_OWNER=root)
AC_SUBST(LIBS_OWNER)

dnl LIBS_GROUP
AC_ARG_WITH(libs-group,
            AC_HELP_STRING([--with-libs-group=GROUP],
                           [group that will own RT binaries (default bin)]),
            LIBS_GROUP=$withval,
            LIBS_GROUP=bin)
AC_SUBST(LIBS_GROUP)

dnl version numbers
AC_SUBST(AT_VERSION_MAJOR,              ${at_version_major})
AC_SUBST(AT_VERSION_MINOR,              ${at_version_minor})
AC_SUBST(AT_VERSION_PATCH,              ${at_version_patch})

dnl layout paths
AC_SUBST([RT_PATH],                     ${exp_prefix})

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([
		 Makefile
		 sbin/at-setup-database
		 sbin/at_factory
                 etc/AssetTracker/upgrade/1.2.0/AT-1.0.x-to-1.2.0.pl
                 etc/AssetTracker/upgrade/1.2.3/AT-1.2.0-to-1.2.3.pl
		 lib/RTx/AssetTracker.pm],
		[chmod ug=rx,o= sbin/at-setup-database
		 chmod ug=rx,o= sbin/at_factory
		 chmod ug=rx,o= etc/AssetTracker/upgrade/1.2.0/AT-1.0.x-to-1.2.0.pl
		 chmod ug=rx,o= etc/AssetTracker/upgrade/1.2.3/AT-1.2.0-to-1.2.3.pl
		]
	       )
AC_OUTPUT
