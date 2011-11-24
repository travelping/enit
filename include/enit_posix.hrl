
%%
%% see also GETPWNAM(3)
%%
-record(posix_passwd, {
    pw_name     :: nonempty_string(),   %% username
    pw_passwd   :: nonempty_string(),   %% user password
    pw_uid      :: non_neg_integer(),   %% user ID
    pw_gid      :: non_neg_integer(),   %% group ID
    pw_gecos    :: string(),            %% real name
    pw_dir      :: file:name(),         %% home directory
    pw_shell    :: file:name()          %% shell program
}).

%%
%% see also GETGRNAM(3)
%%
-record(posix_group, {
    gr_name     :: nonempty_string(),   %% group name
    gr_passwd   :: string(),            %% group password
    gr_gid      :: non_neg_integer(),   %% group ID
    gr_mem      :: [nonempty_string()]  %% group members
}).

