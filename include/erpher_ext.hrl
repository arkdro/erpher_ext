-ifndef(erpher_ext).
-define(erpher_ext, true).

-define(CONF, "etc/local.config").
-define(PID, "/var/run/erpher/erpher.pid").
-define(LOG, "/var/log/erpher/e"). % base part
-define(PTIME, 10000).

-record(ext, {
    log          :: string(),    % base part of log file
    log_rotate   :: never | minute | hour | day | {dow, 0..7} | month | year,
    log_last     :: tuple(),     % local time
    pid_file     :: string(),    % pid file
    local_config :: string(),    % local config file
    timer        :: reference(), % timer for periodic check
    debug        :: list()
}).

-endif.
