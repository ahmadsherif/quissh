-module(quissh_aws_response_parser).

-include_lib("xmerl/include/xmerl.hrl").

-export([parse/1]).

parse(Response) ->
  {Elm, _} = xmerl_scan:string(Response),

  parse_element(Elm).

parse_element(Elm) when is_record(Elm, xmlElement) ->
  parse_element(Elm, #{}).

parse_element(_Elm = #xmlElement{
                        name = Name,
                        content = [#xmlText{value = Content}]
                       },
              AccMap) ->
  maps:put(Name, Content, AccMap);
parse_element(_Elm = #xmlElement{
                        name = item,
                        content = Children
                       },
              AccMap) when is_list(Children) ->
  AccMap2 = parse_element(Children, #{}),
  OtherItems = maps:get(item, AccMap, []),

  maps:put(item, [AccMap2|OtherItems], AccMap);
parse_element(_Elm = #xmlElement{
                        name = Name,
                        content = Children
                       },
              AccMap) when is_list(Children) ->
  AccMap2 = parse_element(Children, #{}),
  AccMap3 = case maps:is_key(item, AccMap2) of
              true -> maps:get(item, AccMap2);
              _    -> AccMap2
            end,

  maps:put(Name, AccMap3, AccMap);
parse_element([HeadElm|TailElm], AccMap) ->
  AccMap2 = parse_element(HeadElm, AccMap),

  parse_element(TailElm, AccMap2);
parse_element([], AccMap) ->
  AccMap;
parse_element(_Elm, AccMap) ->
  AccMap.
