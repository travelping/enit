/*
Copyright (c) 2011-2012 by Travelping GmbH <info@travelping.com>

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
*/

#include <sys/types.h>
#include <limits.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdint.h>
#include <pwd.h>
#include <grp.h>
#include <syslog.h>

#include <erl_nif.h>
#include <erl_driver.h>

#define NIF_SIG(NAME) \
	static ERL_NIF_TERM \
	NAME##_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv)

static inline ERL_NIF_TERM
tag_term(ErlNifEnv *env, const char *tag, ERL_NIF_TERM term)
{
	return enif_make_tuple2(env, enif_make_atom(env, tag), term);
}

static inline ERL_NIF_TERM
tag_uint(ErlNifEnv *env, const char *tag, unsigned int i)
{
	return tag_term(env, tag, enif_make_uint(env, i));
}

static inline ERL_NIF_TERM
tag_string(ErlNifEnv *env, const char *tag, const char *string)
{
	return tag_term(env, tag, enif_make_string(env, string, ERL_NIF_LATIN1));
}

static inline ERL_NIF_TERM
error2tuple(ErlNifEnv *env, int error)
{
	return tag_term(env, "error", enif_make_atom(env, erl_errno_id(error)));
}

static ERL_NIF_TERM
list_of_strings(ErlNifEnv *env, const char **array)
{
	unsigned int len;

	ERL_NIF_TERM *terms = NULL;
	ERL_NIF_TERM list;

	for (len = 0; array[len] != NULL; len++);
	if (len) {
		terms = enif_alloc(sizeof(ERL_NIF_TERM) * len);
		if (terms != NULL) {
			for (int i = 0; i < len; i++)
				terms[i] = enif_make_string(env, array[i],
							    ERL_NIF_LATIN1);
		}
	}

	list = enif_make_list_from_array(env, terms, len);

	if (terms != NULL)
		enif_free(terms);

	return list;
}

static inline void
free_argv(char **argv)
{
	for (char **p = argv; *p; p++)
		enif_free(*p);
	enif_free(argv);
}

NIF_SIG(exec)
{
	int _errno;
	sigset_t sigset, old_sigset;

	unsigned int len;
	ERL_NIF_TERM head, tail;

	char **exec_argv, **exec_argp;

	assert(argc == 1);

	if (!enif_get_list_length(env, argv[0], &len) || len < 1)
		return enif_make_badarg(env);

	exec_argv = enif_alloc(sizeof(char *) * (len + 1));
	if (exec_argv == NULL)
		return error2tuple(env, ENOMEM);
	memset(exec_argv, 0, sizeof(char *) * (len + 1));

	exec_argp = exec_argv;
	for (const ERL_NIF_TERM *cur = argv;
	     enif_get_list_cell(env, *cur, &head, &tail);
	     cur = &tail) {
		if (!enif_get_list_length(env, head, &len) || len < 1) {
			free_argv(exec_argv);
			return enif_make_badarg(env);
		}

		*exec_argp = enif_alloc(len + 1);
		if (*exec_argp == NULL) {
			free_argv(exec_argv);
			return error2tuple(env, ENOMEM);
		}

		if (enif_get_string(env, head,
				    *exec_argp, len + 1, ERL_NIF_LATIN1) < 1) {
			free_argv(exec_argv);
			return enif_make_badarg(env);
		}

		exec_argp++;
	}
	*exec_argp = NULL;

	sigemptyset(&sigset);
	sigprocmask(SIG_SETMASK, &sigset, &old_sigset);

	execv(exec_argv[0], exec_argv);
	_errno = errno;
	/* execv failed */

	sigprocmask(SIG_SETMASK, &old_sigset, NULL);

	free_argv(exec_argv);
	return error2tuple(env, _errno);
}

NIF_SIG(getpwnam)
{
	char name_buf[_POSIX_LOGIN_NAME_MAX + 1];

	long size = sysconf(_SC_GETPW_R_SIZE_MAX);
	assert(size > 0);

	char pwd_buf[size];

	struct passwd passwd, *pwd_p;
	ERL_NIF_TERM record;

	assert(argc == 1);

	if (enif_get_string(env, argv[0],
			    name_buf, sizeof(name_buf), ERL_NIF_LATIN1) < 1)
		return enif_make_badarg(env);

	errno = getpwnam_r(name_buf, &passwd, pwd_buf, sizeof(pwd_buf), &pwd_p);
	if (pwd_p == NULL)
		return error2tuple(env, errno ? : ENOENT);

	/*
	 * see enit_posix.hrl
	 */
	record = enif_make_tuple8(env, enif_make_atom(env, "posix_passwd"),
				  enif_make_string(env, passwd.pw_name, ERL_NIF_LATIN1),
				  enif_make_string(env, passwd.pw_passwd, ERL_NIF_LATIN1),
				  enif_make_uint(env, passwd.pw_uid),
				  enif_make_uint(env, passwd.pw_gid),
				  enif_make_string(env, passwd.pw_gecos, ERL_NIF_LATIN1),
				  enif_make_string(env, passwd.pw_dir, ERL_NIF_LATIN1),
				  enif_make_string(env, passwd.pw_shell, ERL_NIF_LATIN1));

	return tag_term(env, "ok", record);
}

NIF_SIG(getgrnam)
{
	char name_buf[_POSIX_LOGIN_NAME_MAX + 1]; /* FIXME */

	long size = sysconf(_SC_GETGR_R_SIZE_MAX);
	assert(size > 0);

	char gr_buf[size];

	struct group group, *gr_p;
	ERL_NIF_TERM record;

	assert(argc == 1);

	if (enif_get_string(env, argv[0],
			    name_buf, sizeof(name_buf), ERL_NIF_LATIN1) < 1)
		return enif_make_badarg(env);

	errno = getgrnam_r(name_buf, &group, gr_buf, sizeof(gr_buf), &gr_p);
	if (gr_p == NULL)
		return error2tuple(env, errno ? : ENOENT);

	/*
	 * see enit_posix.hrl
	 */
	record = enif_make_tuple5(env, enif_make_atom(env, "posix_group"),
				  enif_make_string(env, group.gr_name, ERL_NIF_LATIN1),
				  enif_make_string(env, group.gr_passwd, ERL_NIF_LATIN1),
				  enif_make_uint(env, group.gr_gid),
				  list_of_strings(env, (const char **)group.gr_mem));

	return tag_term(env, "ok", record);
}

NIF_SIG(setuid)
{
	unsigned int uid;

	assert(argc == 1);

	if (!enif_get_uint(env, argv[0], &uid))
		return enif_make_badarg(env);

	if (setuid(uid))
		return error2tuple(env, errno);

	return enif_make_atom(env, "ok");
}

NIF_SIG(getuid)
{
	return tag_term(env, "ok", enif_make_uint(env, getuid()));
}

NIF_SIG(setgid)
{
	unsigned int gid;

	assert(argc == 1);

	if (!enif_get_uint(env, argv[0], &gid))
		return enif_make_badarg(env);

	if (setgid(gid))
		return error2tuple(env, errno);

	return enif_make_atom(env, "ok");
}

NIF_SIG(getgid)
{
	return tag_term(env, "ok", enif_make_uint(env, getgid()));
}

NIF_SIG(syslog)
{
	unsigned int level;
	char buf[255];

	assert(argc == 2);

	if (!enif_get_uint(env, argv[0], &level))
		return enif_make_badarg(env);

	if (enif_get_string(env, argv[1], buf, sizeof(buf), ERL_NIF_LATIN1) < 1)
		return enif_make_badarg(env);

	openlog("enit", 0, LOG_USER); /* FIXME */
	syslog(level, "%s", buf);

	return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
#define NIF_MAP(NAME, ARITY) \
	{#NAME, ARITY, NAME##_nif}
	NIF_MAP(exec,		1),
	NIF_MAP(getpwnam,	1),
	NIF_MAP(getgrnam,	1),
	NIF_MAP(setuid,		1),
	NIF_MAP(getuid,		0),
	NIF_MAP(setgid,		1),
	NIF_MAP(getgid,		0),
	NIF_MAP(syslog,		2)
#undef	NIF_MAP
};
ERL_NIF_INIT(enit_posix, nif_funcs, NULL, NULL, NULL, NULL)

