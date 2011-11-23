/*
 *    __                        __      _
 *   / /__________ __   _____  / /___  (_)___  ____ _
 *  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
 * / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
 * \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
 *                           /_/            /____/
 *
 * (c) Travelping GmbH <info@travelping.com>
 *
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

	assert(argc == 1);

	if (enif_get_string(env, argv[0],
			    name_buf, sizeof(name_buf), ERL_NIF_LATIN1) < 1)
		return enif_make_badarg(env);

	errno = getpwnam_r(name_buf, &passwd, pwd_buf, sizeof(pwd_buf), &pwd_p);
	if (pwd_p == NULL)
		return error2tuple(env, errno ? : ENOENT);

#define PW_TAG(TYPE, NAME) \
	tag_##TYPE(env, #NAME, passwd.pw_##NAME)
	return tag_term(env, "ok",
			enif_make_list7(env, PW_TAG(string, name),
					     PW_TAG(string, passwd),
					     PW_TAG(uint,   uid),
					     PW_TAG(uint,   gid),
					     PW_TAG(string, gecos),
					     PW_TAG(string, dir),
					     PW_TAG(string, shell)));
#undef	PW_TAG
}

NIF_SIG(getgrnam)
{
	char name_buf[_POSIX_LOGIN_NAME_MAX + 1]; /* FIXME */

	long size = sysconf(_SC_GETGR_R_SIZE_MAX);
	assert(size > 0);

	char gr_buf[size];

	struct group group, *gr_p;

	assert(argc == 1);

	if (enif_get_string(env, argv[0],
			    name_buf, sizeof(name_buf), ERL_NIF_LATIN1) < 1)
		return enif_make_badarg(env);

	errno = getgrnam_r(name_buf, &group, gr_buf, sizeof(gr_buf), &gr_p);
	if (gr_p == NULL)
		return error2tuple(env, errno ? : ENOENT);

#define GR_TAG(TYPE, NAME) \
	tag_##TYPE(env, #NAME, group.gr_##NAME)
	return tag_term(env, "ok",
			enif_make_list3(env, GR_TAG(string, name),
					     GR_TAG(string, passwd),
					     GR_TAG(uint,   gid)
					     /* GR_TAG(string, mem) */));
#undef	GR_TAG
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

static ErlNifFunc nif_funcs[] = {
#define NIF_MAP(NAME, ARITY) \
	{#NAME, ARITY, NAME##_nif}
	NIF_MAP(exec,		1),
	NIF_MAP(getpwnam,	1),
	NIF_MAP(getgrnam,	1),
	NIF_MAP(setuid,		1),
	NIF_MAP(setgid,		1)
#undef	NIF_MAP
};
ERL_NIF_INIT(enit_vm, nif_funcs, NULL, NULL, NULL, NULL)

