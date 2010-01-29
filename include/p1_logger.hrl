%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is ProcessOne SARL.
%% Portions created by ProcessOne are Copyright 2009, ProcessOne SARL.
%% All Rights Reserved.''
%%
%%     $Id: $
%%

%% Print in standard output
-define(PRINT(Format, Args),
    io:format(Format, Args)).

-define(DEBUG(Format, Args),
    p1_logger:debug_msg(?MODULE,?LINE,Format, Args)).

-define(INFO_MSG(Format, Args),
    p1_logger:info_msg(?MODULE,?LINE,Format, Args)).

-define(WARNING_MSG(Format, Args),
    p1_logger:warning_msg(?MODULE,?LINE,Format, Args)).

-define(ERROR_MSG(Format, Args),
    p1_logger:error_msg(?MODULE,?LINE,Format, Args)).

-define(CRITICAL_MSG(Format, Args),
    p1_logger:critical_msg(?MODULE,?LINE,Format, Args)).
