%%%-------------------------------------------------------------------
%%% @author Jose Luis Gordo Romero <jgordor@gmail.com>
%%% @doc Chicago Boss rebar plugin
%%%  Manage compilation/configuration/scripts stuff the rebar way
%%% @end
%%%-------------------------------------------------------------------
-module(boss_plugin).

-export([boss/2,
     pre_compile/2,
     pre_eunit/2]).

-define(BOSS_PLUGIN_CLIENT_VERSION, 1).
-define(BOSS_CONFIG_FILE, "boss.config").
-define(BOSS_TEST_CONFIG_FILE, "boss.test.config").

%% ====================================================================
%% Public API
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc boss command
%% @spec boss(_Config, _AppFile) -> ok | {error, Reason}
%%       Boss enabled rebar commands, usage:
%%       ./rebar boss c=command
%% @end
%%--------------------------------------------------------------------
boss(RebarConf, AppFile) ->
  case is_base_dir(RebarConf) of
    true -> 
      Command = rebar_config:get_global(RebarConf, c, "help"),
      {ok, BossConf} = init(RebarConf, AppFile, Command),
      case boss_rebar:run(?BOSS_PLUGIN_CLIENT_VERSION, Command, RebarConf, BossConf, AppFile) of
        {error, command_not_found} ->
          io:format("ERROR: boss command not found.~n"), 
          boss_rebar:help(),
          halt(1);
        {error, Reason} -> 
          io:format("ERROR: executing ~s task: ~s~n", [Command, Reason]), 
          halt(1);
        ok -> ok
      end;
    false -> ok
  end.

%%--------------------------------------------------------------------
%% @doc initializes the rebar boss connector plugin
%% @spec init(Config, AppFile) -> {ok, BossConf} | {error, Reason}
%%       Set's the ebin cb_apps and loads the connector 
%% @end
%%--------------------------------------------------------------------
init(RebarConf, AppFile, Command) ->
    %% Compile and load the boss_rebar code, this can't be compiled
  %% as a normal boss lib without the rebar source dep
  %% The load of ./rebar boss:
  %% - Rebar itself searchs in rebar.config for {plugin_dir, ["priv/rebar"]}.
  %% - Rebar itself compile this plugin and adds it to the execution chain
  %% - This plugin compiles and loads the boss_rebar code in ["cb/priv/rebar"],
  %%   so we can extend/bugfix/tweak the framework without the need of manually
  %%   recopy code to user apps
    {BossConf, BossConfFile} = boss_config(Command),
  BossPath = case boss_config_value(boss, path, BossConf) of
           {error, _} ->
             filename:join([rebar_config:get_xconf(RebarConf, base_dir, undefined), "deps", "boss"]);
           Val -> Val
         end,
  RebarErls = rebar_utils:find_files(filename:join([BossPath, "priv", "rebar"]), ".*\\.erl\$"),
  
  rebar_log:log(debug, "Auto-loading boss rebar modules ~p~n", [RebarErls]),
  
  lists:map(fun(F) ->
            case compile:file(F, [binary]) of
              error ->
                io:format("FATAL: Failed compilation of ~s module~n", [F]),
                halt(1);
              {ok, M, Bin} ->
                {module, _} = code:load_binary(M, F, Bin),
                rebar_log:log(debug, "Loaded ~s~n", [M])
            end
        end, RebarErls),

  %% add all cb_apps defined in config file to code path
  %% including the deps ebin dirs
  
  case is_base_dir(RebarConf) of
  true ->
    code:add_paths(["ebin" | filelib:wildcard(rebar_config:get_xconf(RebarConf, base_dir, undefined) ++ "/deps/*/ebin")]);
  false ->
    code:add_paths(filelib:wildcard(rebar_utils:get_cwd() ++ "/../*/ebin"))
  end,
  %% Load the config in the environment
  boss_rebar:init_conf(BossConf),
  
  {ok, BossConf}.
  
%%--------------------------------------------------------------------
%% @doc pre_compile hook
%% @spec pre_compile(_Config, AppFile) -> ok | {error, Reason}
%%       Pre compile hook, compile the boss way
%%       Compatibility hook, the normal ./rebar compile command works,
%%         but only calls the ./rebar boss c=compile and halts (default
%%         rebar task never hits)
%% @end
%%--------------------------------------------------------------------
pre_compile(RebarConf, AppFile) ->
  case is_boss_app(AppFile)  orelse is_base_dir(RebarConf) of
    true -> 
      {ok, BossConf} = init(RebarConf, AppFile, "compile"),
      boss_rebar:run(?BOSS_PLUGIN_CLIENT_VERSION, compile, RebarConf, BossConf, AppFile),
      halt(0);
    false -> ok
  end.

%%--------------------------------------------------------------------
%% @doc pre_eunit hook
%% @spec pre_eunit(RebarConf, AppFile) -> ok | {error, Reason}
%%       Pre eunit hook, .eunit compilation the boss way
%%       Compatibility hook, the normal ./rebar eunit command works,
%%         but only calls the ./rebar boss c=test_eunit and halts
%%         (default rebar task never hits)
%% @end
%%--------------------------------------------------------------------
pre_eunit(RebarConf, AppFile) ->
  case is_boss_app(AppFile) orelse is_base_dir(RebarConf) of
    true -> 
      {ok, BossConf} = init(RebarConf, AppFile, "test_eunit"),
      boss_rebar:run(?BOSS_PLUGIN_CLIENT_VERSION, test_eunit, RebarConf, BossConf, AppFile),
      halt(0);
    false -> ok
  end.

%% ===================================================================
%% Internal functions
%% ===================================================================

is_boss_app(Filename)->
  BossFileForApp = filename:join([filename:dirname(Filename), "..", "boss.config"]),
  filelib:is_file(BossFileForApp).

%% Checks if the current dir (rebar execution) is the base_dir
%% Used to prevent run boss tasks in deps directory
is_base_dir(RebarConf) ->
    filename:absname(rebar_utils:get_cwd()) =:= rebar_config:get_xconf(RebarConf, base_dir, undefined).

%% Gets the boss.config central configuration file
boss_config(Command) ->
    IsTest		= lists:prefix("test_", Command),
    BossConfFile	= get_config_file(IsTest),
    ConfigData		= file:consult(BossConfFile),
    validate_config_data(BossConfFile, ConfigData).

get_config_file(false) ->
	    ?BOSS_CONFIG_FILE;
get_config_file(true) ->
    case file:read_file_info(?BOSS_TEST_CONFIG_FILE) of
	{ok, _} -> ?BOSS_TEST_CONFIG_FILE;
	_ -> ?BOSS_CONFIG_FILE
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_config_data(BossConfFile, {error, {Line, _Mod, [Term|_]}}) ->
    io:format("FATAL: Config file ~p has a syntax error on line ~p, ~n ~p ~n~n", 
	      [BossConfFile, Line,  Term]),
    halt(1);
validate_config_data(BossConfFile, {error, enoent}) ->
    io:format("FATAL: Config file ~p not found.~n", [BossConfFile]),
    halt(1);
validate_config_data(BossConfFile, {ok, [BossConfig]}) ->
    {BossConfig, BossConfFile}.
    


%%--------------------------------------------------------------------
%% @doc Get Boss config value app, key
%% @spec boss_config_value(App, Key) -> Value | {error, Reason}
%%       Searchs in boss config for a given App and Key
%% @end
%%--------------------------------------------------------------------
boss_config_value(App, Key, BossConfig) ->
  case lists:keyfind(App, 1, BossConfig) of
    false -> 
      {error, boss_config_app_not_found};
    {App, AppConfig} -> 
      case lists:keyfind(Key, 1, AppConfig) of
        false -> 
          {error, boss_config_app_setting_not_found};
        {Key, KeyConfig} ->
          KeyConfig
      end
  end.
