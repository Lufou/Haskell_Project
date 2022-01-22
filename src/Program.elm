module Program exposing (Inst(..), Proc, Program, ProgramError, parseProgram)

import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=), DeadEnd, Parser, Trailing(..), float, int, lazy, loop, map, oneOf, sequence, spaces, succeed, token, variable)
import Set


type Inst
    = Forward Float
    | Left Float
    | Right Float
    | Repeat Int Proc
    | Call String

type alias Proc =
    List Inst

type alias Program =
    Dict String Proc

parseForward : Parser Inst
parseForward =
    succeed Forward
        |. token "Forward"
        |. spaces
        |= float

parseLeft : Parser Inst
parseLeft =
    succeed Left
        |. token "Left"
        |. spaces
        |= float

parseRight : Parser Inst
parseRight =
    succeed Right
        |. token "Right"
        |. spaces
        |= float

parseRepeat : Parser Inst
parseRepeat =
    succeed Repeat
        |. token "Repeat"
        |. spaces
        |= int
        |. spaces
        |= lazy (\_ -> parseProc)

parseIdentifier : Parser String
parseIdentifier =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        }

parseCall : Parser Inst
parseCall =
    succeed Call
        |. token "Call"
        |. spaces
        |= parseIdentifier

parseInst : Parser Inst
parseInst =
    oneOf [ parseForward, parseLeft, parseRight, parseRepeat, parseCall ]

parseProc : Parser Proc
parseProc =
    sequence { start = "[", separator = ",", end = "]", spaces = spaces, item = parseInst, trailing = Optional }

parseProg : Parser Program
parseProg =
    succeed identity
        |. spaces
        |= loop Dict.empty
            (\procs ->
                oneOf
                    [ succeed (\identifier proc -> P.Loop (Dict.insert identifier proc procs))
                        |= parseIdentifier
                        |. spaces
                        |= parseProc
                        |. spaces
                    , succeed (\proc -> P.Loop (Dict.insert "main" proc procs))
                        |= parseProc
                    , succeed ()
                        |> map (\_ -> P.Done procs)
                    ]
            )

type ProgramError
    = SyntaxError DeadEnd
    | UnknownProc String
    | Loop String
    | NoMain

checkProgram : Program -> List ProgramError
checkProgram prog =
    (if Dict.member "main" prog then
        []

     else
        [ NoMain ]
    )
        ++ Dict.foldl
            (\name proc errors ->
                errors
                    ++ List.foldl
                        (\inst procErrors ->
                            procErrors
                                ++ (case inst of
                                        Call callee ->
                                            if Dict.member callee prog then
                                                if callee == name then
                                                    [ Loop callee ]

                                                else
                                                    []

                                            else
                                                [ UnknownProc callee ]

                                        _ ->
                                            []
                                   )
                        )
                        []
                        proc
            )
            []
            prog


parseProgram : String -> ( Maybe Program, List ProgramError )
parseProgram text =
    case P.run parseProg text of
        Err err ->
            ( Nothing, List.map (\e -> SyntaxError e) err )

        Ok prog ->
            ( Just prog, checkProgram prog )