-module(quissh_aws).

-behavior(gen_server).

%% API
-export([start_link/1,
         available_machines/0,
         machine_ssh_args/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

available_machines() ->
  gen_server:call(?MODULE, available_machines, 10000).

machine_ssh_args(MachineIndex) ->
  gen_server:call(?MODULE, {machine_ssh_args, MachineIndex}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
  {ok, Args#{machines => undefined, ssh_user => maps:get(ssh_user, Args, "ec2-user")}}.

handle_call(Request, From, State = #{machines := undefined}) ->
  MachinesList = get_available_machines(
                   os:getenv("AWS_ACCESS_KEY_ID"),
                   os:getenv("AWS_SECRET_KEY"),
                   os:getenv("AWS_REGION")
                 ),

  handle_call(Request, From, State#{machines => MachinesList});
handle_call(available_machines, _From, State = #{machines := MachinesList}) ->
  {reply, MachinesList, State};
handle_call({machine_ssh_args, MachineIndex}, _From, State = #{machines := MachinesList, ssh_user := SshUser}) ->
  #{ip_address := IpAddress, key_name := KeyName} = lists:nth(MachineIndex, MachinesList),
  Reply = ["-i", "/home/" ++ os:getenv("USER") ++ "/.ssh/" ++ KeyName ++ ".pem", SshUser ++ "@" ++ IpAddress],

  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_available_machines(AccessKey, SecretKey, Region) ->
  {ok, {{"HTTP/1.1", 200, "OK"}, _, Response}} = call_desribe_instances_api(AccessKey, SecretKey, Region),
  ParsedResponse = quissh_aws_response_parser:parse(Response),

  extract_machines_info_from_response(ParsedResponse).

call_desribe_instances_api(AccessKey, SecretKey, Region) ->
  quissh_aws_client:ec2_request(AccessKey, SecretKey, Region, [{"Action", "DescribeInstances"}]).

extract_machines_info_from_response(_Response = #{'DescribeInstancesResponse' := #{reservationSet := ReservationSet}}) ->
  extract_machines_info_from_reservation_set(ReservationSet, []).

extract_machines_info_from_reservation_set([ReservationSetHead|ReservationSetTail], Acc) ->
  Info = extract_machines_info_from_reservation_set(ReservationSetHead),

  extract_machines_info_from_reservation_set(ReservationSetTail, [Info|Acc]);
extract_machines_info_from_reservation_set([], Acc) ->
  Acc.

extract_machines_info_from_reservation_set(_ReservationSetItem = #{instancesSet := [InstancesSet]}) ->
  TagSet    = maps:get(tagSet, InstancesSet),
  [NameTag] = lists:filter(fun(#{key := Key}) -> Key == "Name" end, TagSet),

  #{value := Name} = NameTag,

  #{
     ip_address => maps:get(ipAddress, InstancesSet),
     key_name => maps:get(keyName, InstancesSet),
     name => Name
  }.
