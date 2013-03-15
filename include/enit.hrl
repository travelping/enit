-record(release, {
    name :: string(),
    extensions :: [atom()],
    version :: string(),
    path :: file:name(),
    applications :: [atom()],
    nodename :: node(),
    cookie :: atom(),
    config :: enit_config:config()
}).

-record(status, {
    alive :: boolean(),
    running_apps :: [{atom(), string()}, ...],
    app_config_defaults :: enit_config:config(),
    running_config :: enit_config:config(),
    connected_nodes :: [node()],
    otp_version :: string(),
    os_pid :: integer(),
    uptime_seconds :: integer(),
    memory_info :: [{atom(), integer()}]
}).
