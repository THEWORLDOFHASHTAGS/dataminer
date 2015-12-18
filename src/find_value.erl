-module(find_value).

-export([find_country_id/1, find_country_name/1, find_hashlist/1, find_hash/2, find_time/1]).

%Find the countrycode in the received list.
find_country_id(L) ->
    case lists:keyfind(<<"place">>, 1, L) of
        %Value = null => no place data available
        {<<"place">>,null} -> ok;
        %Value = a tuple list, dig deeper for the countrycode
        {<<"place">>, {I}} -> 
            case lists:keyfind(<<"country_code">>, 1, I) of
                {_,null} -> ok; 
                %Country code found:
                {_, CountryCode} -> CountryCode;
                false -> ok
            end;
        false -> ok
    end.

%Find the name of the country
find_country_name(L) ->
	case lists:keyfind(<<"place">>, 1, L) of
        {<<"place">>,null} -> ok;
        {<<"place">>, {I}} -> 
            case lists:keyfind(<<"country">>, 1, I) of %is it called country_name?
                {_,null} -> ok; 
                %Country name found:
                {_, CN} -> CN;
                false -> ok
            end;
        false -> ok
    end.

%Find the list of hashtag data
find_hashlist(L) ->
    case lists:keyfind(<<"entities">>, 1, L) of
        {<<"entities">>,null} -> ok;
        {<<"entities">>, {B}} -> 
            case lists:keyfind(<<"hashtags">>, 1, B) of
                {_, []} -> ok;
                {_, Hashlist} -> 
                    %Look at the length of the list, to know how many hashtags there are
                    Length = length(Hashlist),
                    %Extract the hashtag text from the additional data using our find_hash method
                    Hash = find_hash(Length, Hashlist),
                    %Flatten the list we receive, and return the result
                    Result = lists:flatten(Hash),
                    Result; 
              	false -> ok
            end;
        false -> ok
    end.

%When N is 1 (last hashtag on the list), simply return the hashtag text.
find_hash(1, L) -> 
    case lists:nth(1, L) of
        {H} ->
            case lists:keyfind(<<"text">>, 1, H) of
                {_, B} -> [B];
                false -> ok
            end;
        false -> ok
    end;

%N is originally the number of hashtags we have to extract from list L.
find_hash(N, L) ->
    case lists:nth(N, L) of
        %If the Nth element of list L is a tuplelist, look for the hashtag text.
        %Return the text together with the result of running the function recursively,
        %decrementing N.
        {H} ->
        	case lists:keyfind(<<"text">>, 1, H) of
        		{_, B} -> [B] ++ [find_hash(N-1, L)];
          		false -> ok
        	end;
      	false -> ok
    end.


%Find the time when the hashtag was created.
find_time(L) -> 
	case lists:keyfind(<<"created_at">>, 1, L) of
        {_, T} -> T;
        false -> ok
    end.

