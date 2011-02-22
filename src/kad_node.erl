-module(kad_node).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("kad.hrl").

%% Initial port number
-define(UDP_PORT, 4000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, ping/2, find_node/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

ping(Host, Port) ->
  {ok, Socket} = gen_udp:open(0, [binary]),
  
  Myself = kad_network:myself(),
  MyPort = kad_network:myport(),
  ok = gen_udp:send(Socket, Host, Port, term_to_binary({ping, Myself, MyPort})),

  receive
      {udp, _Socket, _, _, Bin} = _Msg ->
        Response = binary_to_term(Bin),
        case Response of
          {pong, NodeId, ReceivedPort} ->
            %% time to update this contact
            Contact = #contact{nodeId = NodeId, ipAddress = Host, udpPort = ReceivedPort},
            kad_network:store_contact(Contact),
            io:format("Node ~p is alive!~n", [NodeId]);
          Error ->
            {error, Error}
        end
    after 5000 ->
      timeout
  end.

find_node(Host, Port, SearchedKey) ->
  {ok, Socket} = gen_udp:open(0, [binary]),

  Myself = kad_network:myself(),
  MyPort = kad_network:myport(),
  ok = gen_udp:send(Socket, Host, Port, term_to_binary({find_node, Myself, MyPort, SearchedKey})),

  receive
    {udp, _Socket, _, _, Bin} = _Msg ->
      Response = binary_to_term(Bin),
      case Response of
        {ok, Contacts} ->
          Contacts;
        Error ->
          {error, Error}
      end
    after 5000 ->
      timeout
  end.


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  {ok, Socket} = open_socket(?UDP_PORT, 0),

  {ok, Port} = inet:port(Socket),
  io:format("kad_node starting at port ~p~n", [Port]),
  kad_network:setport(Port),
  
  {ok, Socket}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({udp, Socket, Host, Port, Bin}, State) ->
  Payload = binary_to_term(Bin),
  io:format("Received [~p] from ~p port ~p~n", [Payload, Host, Port]),
  case Payload of
    %% we received a ping
    {ping, ReceivedNodeId, ReceivedPort} ->
      io:format("Received a ping from ~p/~p with N~p/P~p~n", [Host, Port, ReceivedNodeId, ReceivedPort]),

      %% time to add or update this contact
      Contact = #contact{nodeId = ReceivedNodeId, ipAddress = Host, udpPort = ReceivedPort},
      kad_network:store_contact(Contact),

      %% and then respond
      Myself = kad_network:myself(),
      MyPort = kad_network:myport(),
      gen_udp:send(Socket, Host, Port, term_to_binary({pong, Myself, MyPort}));

    {find_node, _ReceivedNodeId, _ReceivedPort, SearchedKey} ->
      Contacts = kad_network:find_closest_contacts_to(SearchedKey),
      gen_udp:send(Socket, Host, Port, term_to_binary({ok, Contacts}));

    %% we received an unknown request
    Unknown ->
      %% no change here
      io:format("unknown request: ~p!~n", [Unknown])
  end,
  {noreply, State}.

terminate(_Reason, Socket) ->
  %% TODO probably need to look at the result...
  gen_udp:close(Socket),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

open_socket(Port, Tries) when Tries < 10 ->
  Reponse = gen_udp:open(Port, [binary]),
  case Reponse of
    {ok, Socket} -> {ok, Socket};
    {error, eaddrinuse} -> open_socket(Port+1, Tries+1)
  end;
open_socket(Port, _Tries) ->
  gen_udp:open(Port, [binary]).

