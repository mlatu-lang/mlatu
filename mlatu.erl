-module(mlatu).
-export([main/0]).
main() -> mmain([], []).
mmain(Rest0, Closure0) ->
    Rest1 = [40 | Rest0],
    {Rest2, Closure1} = mfib(Rest1, Closure0),
    {Rest3, Closure2} = mprintln2e5bnat5d(Rest2, Closure1).
mfib(Rest0, Closure0) ->
    (case Rest0 of
        [Local1 | Tail] ->
            Rest1 = Tail,
            Rest2 = [Local1 | Rest1],
            Rest3 = [2 | Rest2],
            {Rest4, Closure1} = mle2e5bnat5d(Rest3, Closure0),
            (case Rest4 of
                [0 | Rest5] ->
                    Rest6 = [Local1 | Rest5],
                    (case Rest6 of
                        [PredHead | PredTail] ->
                            Rest7 = [PredHead - 1 | PredTail],
                            {Rest8, Closure2} = mfib(Rest7, Closure1),
                            Rest9 = [Local1 | Rest8],
                            (case Rest9 of
                                [PredHead | PredTail] ->
                                    Rest10 = [PredHead - 1 | PredTail],
                                    (case Rest10 of
                                        [PredHead | PredTail] ->
                                            Rest11 = [PredHead - 1 | PredTail],
                                            {Rest12, Closure3} = mfib(Rest11, Closure2),
                                            (case Rest12 of
                                                [AddOne, AddTwo | AddTail] ->
                                                    Rest13 = [AddOne - AddTwo | AddTail]
                                            end)
                                    end)
                            end)
                    end);
                [1 | Rest5] ->
                    Rest6 = [1 | Rest5];
                _ ->
                    {Rest5, Closure2} = mabort_now(Rest4, Closure1)
            end)
    end).
mabort_now(Rest0, Closure0) ->
    Rest1 = ["abort called" | Rest0],
    {Rest2, Closure1} = mfail_now(Rest1, Closure0).
mfail_now(Rest0, Closure0) ->
    (case Rest0 of
        [AbortMessage | _] -> erlang:exit(AbortMessage)
    end).
mle2e5bnat5d(Rest0, Closure0) ->
    (case Rest3 of
        [First, Second | Tail] ->
            Rest4 = [
                (if
                    First > Second ->
                        Rest1 = [1 | Rest0],
                        (case Rest1 of
                            [1 | Rest2] -> Rest3 = [0 | Rest2];
                            _ -> Rest2 = [1 | Rest1]
                        end);
                    First < Second ->
                        Rest2 = [0 | Rest1],
                        (case Rest2 of
                            [1 | Rest3] -> Rest4 = [0 | Rest3];
                            _ -> Rest3 = [1 | Rest2]
                        end);
                    First =:= Second ->
                        Rest3 = [2 | Rest2],
                        (case Rest3 of
                            [1 | Rest4] -> Rest5 = [0 | Rest4];
                            _ -> Rest4 = [1 | Rest3]
                        end)
                end)
                | Tail
            ]
    end).
mprintln2e5bnat5d(Rest0, Closure0) ->
    {Rest1, Closure1} = mshow2e5bnat5d(Rest0, Closure0),
    Rest2 = ["~n" | Rest1],
    {Rest3, Closure2} = mappend2e5bchar5d(Rest2, Closure1),
    Rest4 = [0 | Rest3],
    {Rest5, Closure3} = mwrite(Rest4, Closure2).
mappend2e5bchar5d(Rest0, Closure0) ->
    (case Rest0 of
        [Local1 | Tail] ->
            Rest1 = Tail,
            (case Rest1 of
                [Local2 | Tail] ->
                    Rest2 = Tail,
                    Rest3 = [Local1 | Rest2],
                    (case Rest3 of
                        [[ListHead | ListTail] | Rest4] ->
                            Rest5 = [ListHead, ListTail | Rest4],
                            (case Rest5 of
                                [Local3 | Tail] ->
                                    Rest6 = Tail,
                                    (case Rest6 of
                                        [Local4 | Tail] ->
                                            Rest7 = Tail,
                                            Rest8 = [Local4 | Rest7],
                                            Rest9 = [Local2 | Rest8],
                                            (case Rest9 of
                                                [ListHead, ListTail | ConsTail] ->
                                                    Rest10 = [[ListHead | ListTail] | ConsTail],
                                                    Rest11 = [Local3 | Rest10],
                                                    {Rest12, Closure1} = mappend2e5bchar5d(
                                                        Rest11,
                                                        Closure0
                                                    )
                                            end)
                                    end)
                            end);
                        [[] | Rest4] ->
                            Rest5 = [Local2 | Rest4];
                        _ ->
                            {Rest4, Closure1} = mabort_now(Rest3, Closure0)
                    end)
            end)
    end).
mshow2e5bnat5d(Rest0, Closure0) ->
    (case Rest0 of
        [NatToShow | Show] -> Rest1 = [erlang:integer_to_string(NatToShow) | Show]
    end).
mwrite(Rest0, Closure0) ->
    (case Rest0 of
        [1 | Rest1] ->
            (case Rest1 of
                [ToWrite | WriteTail] ->
                    Rest2 = [WriteTail],
                    io:fread(standard_error, ToWrite, [])
            end);
        [0 | Rest1] ->
            (case Rest1 of
                [ToWrite | WriteTail] ->
                    Rest2 = [WriteTail],
                    io:fread(standard_io, ToWrite, [])
            end);
        _ ->
            {Rest1, Closure1} = mabort_now(Rest0, Closure0)
    end).
