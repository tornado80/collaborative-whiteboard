-module(utility).

-include("common_records.hrl").

-export([new_uuid/0, is_valid_uuid/1, vector2_to_map/1, new_color/0, new_anonymous_animal_name/0]).

-spec new_uuid() -> binary().
new_uuid() ->
    list_to_binary(uuid:to_string(uuid:uuid4())).

-spec is_valid_uuid(binary()) -> boolean().
is_valid_uuid(Uuid) when is_binary(Uuid) ->
    try
        uuid:is_v4(binary_to_list(Uuid))
    catch
        _Class:_Error -> false
    end.

new_color() ->
    R = integer_to_binary(random:uniform(255), 16),
    G = integer_to_binary(random:uniform(255), 16),
    B = integer_to_binary(random:uniform(255), 16),
    <<R/binary, G/binary, B/binary>>.

new_anonymous_animal_name() ->
    AnonymousAnimals = [alligator, anteater, armadillo, auroch, axolotl, badger, bat, bear, beaver, blobfish, buffalo,
        camel, chameleon, cheetah, chipmunk, chinchilla, chupacabra, cormorant, coyote, crow, dingo, dinosaur, dog,
        dolphin, dragon, duck, 'dumbo octopus', elephant, ferret, fox, frog, giraffe, goose, gopher, grizzly, hamster,
        hedgehog, hippo, hyena, jackal, jackalope, ibex, ifrit, iguana, kangaroo, kiwi, koala, kraken, lemur, leopard,
        liger, lion, llama, manatee, mink, monkey, moose, narwhal, 'nyan cat', orangutan, otter, panda, penguin,
        platypus, python, pumpkin, quagga, quokka, rabbit, raccoon, rhino, sheep, shrew, skunk, 'slow loris', squirrel,
        tiger, turtle, unicorn, walrus, wolf, wolverine, wombat],
    RandomIndex = rand:uniform(length(AnonymousAnimals)),
    lists:nth(RandomIndex, AnonymousAnimals).

vector2_to_map([{<<"x">>, x}, {<<"y">>, Y}]) ->
    [{<<"x">>, x}, {<<"y">>, Y}];
vector2_to_map(#vector2{x = X, y = Y}) ->
    #{
        x => X,
        y => Y
    }.

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

is_valid_uuid_test_() ->
    [
        ?_assertEqual(true, is_valid_uuid(new_uuid())),
        ?_assertEqual(false, is_valid_uuid(<<"not a uuid">>))
    ].

-endif.