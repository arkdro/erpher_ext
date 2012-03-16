-ifndef(erpher_ext).
-define(erpher_ext, true).

-define(PID, "/var/run/erpher/erpher.pid").
-define(LOG, "/var/log/erpher/e"). % base part

-record(ext, {
    log          :: string(), % base part of log file
    pid          :: string(), % pid file
    local_config :: string(), % local config file
    debug        :: list()
}).

-endif.
