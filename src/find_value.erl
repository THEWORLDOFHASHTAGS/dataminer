-module(find_value).

-export([find_country_id/1, find_country_name/1, find_hashlist/1, find_hash/2, find_time/1]).

find_country_id(L) ->
    case lists:keyfind(<<"place">>, 1, L) of
        {<<"place">>,null} -> ok;
        {<<"place">>, {I}} -> 
            case lists:keyfind(<<"country_code">>, 1, I) of
                {_,null} -> ok; 
                {_, CountryCode} -> CountryCode;
                false -> ok
            end;
        false -> ok
    end.

find_country_name(L) ->
	case lists:keyfind(<<"place">>, 1, L) of
        {<<"place">>,null} -> ok;
        {<<"place">>, {I}} -> 
            case lists:keyfind(<<"country">>, 1, I) of %is it called country_name?
                {_,null} -> ok; 
                {_, CN} -> CN;
                false -> ok
            end;
        false -> ok
    end.

find_hashlist(L) ->
    case lists:keyfind(<<"entities">>, 1, L) of
        {<<"entities">>,null} -> ok;
        {<<"entities">>, {B}} -> 
            case lists:keyfind(<<"hashtags">>, 1, B) of
                {_, []} -> ok;
                {_, Hashlist} -> 
                    Length = length(Hashlist),
                    Hash = find_hash(Length, Hashlist),
                    Result = lists:flatten(Hash),
                    Result; 
              	false -> ok
            end;
        false -> ok
    end.

find_hash(1, L) -> 
    %X = lists:nth(1, L),
    case lists:nth(1, L) of
        {H} ->
            case lists:keyfind(<<"text">>, 1, H) of
                {_, B} -> [B];
                false -> ok
            end;
        false -> ok
    end;

find_hash(N, L) ->
    %X = lists:nth(N, L),
    case lists:nth(N, L) of
        {H} ->
        	case lists:keyfind(<<"text">>, 1, H) of
        		{_, B} -> [B] ++ [find_hash(N-1, L)];
          		false -> ok
        	end;
      	false -> ok
    end.



find_time(L) -> 
	case lists:keyfind(<<"created_at">>, 1, L) of
        {_, T} -> T;
        false -> ok
    end.


%data -> images -> row resulotion -> url

