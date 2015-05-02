-module(quissh_aws_client).

-export([ec2_request/4]).

ec2_request(AccessKey, SecretKey, Region, Params) ->
  DateTimeNow          = calendar:now_to_datetime(erlang:now()),
  AmzDate              = amzdate(DateTimeNow),
  DateStamp            = datestamp(DateTimeNow),
  Method               = "GET",
  Service              = "ec2",
  Host                 = "ec2.amazonaws.com",
  Endpoint             = "https://ec2.amazonaws.com/",
  RequestParameters    = request_params_string([{"Version", "2014-10-01"}|Params]),
  CanonicalURI         = "/",
  CanonicalQueryString = RequestParameters,
  CanonicalHeaders     = "host:" ++ Host ++ "\n" ++ "x-amz-date:" ++ AmzDate ++ "\n",
  SignedHeaders        = "host;x-amz-date",
  PayloadHash          = hexdigest(crypto:hash(sha256, "")),
  CanonicalRequest     = string:join([
                                      Method, CanonicalURI, CanonicalQueryString,
                                      CanonicalHeaders, SignedHeaders, PayloadHash
                                     ], "\n"),

  Algorithm            = "AWS4-HMAC-SHA256",
  CredentialScope      = string:join([DateStamp, Region, Service, "aws4_request"], "/"),
  CanonicalRequestHash = hexdigest(crypto:hash(sha256, CanonicalRequest)),
  StringToSign         = string:join([
                                      Algorithm, AmzDate, CredentialScope,
                                      CanonicalRequestHash
                                     ], "\n"),

  SigningKey = get_signature_key(SecretKey, DateStamp, Region, Service),
  Signature  = hexdigest(sign(SigningKey, StringToSign)),

  AuthorizationHeader = Algorithm ++ " " ++
    "Credential=" ++ AccessKey ++ "/" ++ CredentialScope ++ ", " ++
    "SignedHeaders=" ++ SignedHeaders ++ ", " ++
    "Signature=" ++ Signature,

  RequestURL = Endpoint ++ "?" ++ CanonicalQueryString,

  inets:start(),
  ssl:start(),
  httpc:request(get, {RequestURL, [{"X-Amz-Date", AmzDate},
                                   {"Authorization", AuthorizationHeader}]},
                [], []).


sign(Key, Msg) ->
  crypto:hmac(sha256, Key, Msg).

get_signature_key(SecretKey, DateStamp, RegionName, ServiceName) ->
  Date    = sign("AWS4" ++ SecretKey, DateStamp),
  Region  = sign(Date, RegionName),
  Service = sign(Region, ServiceName),

  sign(Service, "aws4_request").

amzdate({{Y, Mon, D}, {H, Min, S}}) ->
  DateList = lists:map(fun integer_to_zero_padded_list/1, [Y, Mon, D]),
  TimeList = lists:map(fun integer_to_zero_padded_list/1, [H, Min, S]),

  string:join(DateList ++ ["T"] ++ TimeList ++ ["Z"], "").

datestamp({{Y, M, D}, _}) ->
  DateList = lists:map(fun integer_to_zero_padded_list/1, [Y, M, D]),

  string:join(DateList, "").

request_params_string(Params) ->
  ParamsSorted = lists:sort(fun(A, B) -> A < B end, Params),
  ParamsList   = lists:map(fun({Key, Value}) -> Key ++ "=" ++ Value end, ParamsSorted),

  string:join(ParamsList, "&").

hexdigest(HashBin) when is_binary(HashBin) -> hexdigest(binary_to_list(HashBin));
hexdigest(HashList) when is_list(HashList) ->
  HexList   = lists:map(fun integer_to_hexlist/1 , HashList),
  HexString = string:join(HexList, ""),

  string:to_lower(HexString).

integer_to_zero_padded_list(Num) when is_integer(Num) ->
  Str = integer_to_list(Num),

  case string:len(Str) of
    1 -> "0" ++ Str;
    _ -> Str
  end.

integer_to_hexlist(Num) ->
  HexList = http_util:integer_to_hexlist(Num),

  case string:len(HexList) of
    1 -> "0" ++ HexList;
    _ -> HexList
  end.
