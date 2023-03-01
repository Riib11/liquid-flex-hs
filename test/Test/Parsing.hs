module Test.Parsing where

import Flex.Lexing
import Flex.Parsing
import Flex.Shallow
import Flex.Syntax
import PrettyShow
import Test.HUnit
import Test.Utility
import Text.Parsec (runParserT)

test :: Test
test =
  TestLabel "Parsing" $
    TestList
      [ test_parseModuleId,
        -- test_parseDeclaration
        test_parseTerm,
        -- test_parseStatement,
        test_parseType
        -- test_parseModule
      ]

test_parseModuleId :: Test
test_parseModuleId =
  TestLabel "parseModuleId" $
    TestList
      [ makeTest
          parseModuleId
          "ExampleModule"
          (return . return $ ModuleId ["ExampleModule"]),
        makeTest
          parseModuleId
          "ExampleModuleA.ExampleModuleB.ExampleModuleC"
          (return . return $ ModuleId ["ExampleModuleA", "ExampleModuleB", "ExampleModuleC"])
      ]

test_parseUnqualId :: Test
test_parseUnqualId =
  let makeTest' = makeTest parseUnqualId
   in TestLabel "parseUnqualId" $
        TestList
          [ makeTest'
              "Example"
              (return . return $ Id Nothing "Example"),
            makeTest'
              "Example"
              (return . return $ Id Nothing "Example"),
            makeTest'
              "Example1.Example2"
              Nothing
          ]

test_parseDeclaration :: Test
test_parseDeclaration =
  TestLabel "parseDeclaration" $
    TestList
      [ test_Structure,
        test_Enumerated,
        test_Variant,
        test_Newtype,
        test_Alias,
        test_Function,
        test_Constant
      ]
  where
    makeTest' = makeTest parseDeclaration
    test_Structure =
      TestLabel "Structure" $
        TestList
          [ makeTest' "struct A {}" . pure $ DeclarationStructure <$> structure "A" False Nothing [] Nothing,
            makeTest' "struct message A {}" . pure $ DeclarationStructure <$> structure "A" True Nothing [] Nothing,
            makeTest' "struct message A extends B {}" . pure $ DeclarationStructure <$> structure "A" True (Just "B") [] Nothing,
            makeTest' "struct message A extends B {}" . pure $ DeclarationStructure <$> structure "A" True (Just "B") [] Nothing,
            makeTest' "struct message A extends B { a: int32; }" . pure $ DeclarationStructure <$> structure "A" True (Just "B") [("a", tyInt 32)] Nothing,
            makeTest' "struct message A extends B { a: [int32]; b: bit; c: char; }" . pure $ DeclarationStructure <$> structure "A" True (Just "B") [("a", tyArray (tyInt 32)), ("b", tyBit), ("c", tyChar)] Nothing,
            makeTest' "struct message A extends B { a: [int32]; b: bit; c: char; @assert(true) }" . pure $ DeclarationStructure <$> structure "A" True (Just "B") [("a", tyArray (tyInt 32)), ("b", tyBit), ("c", tyChar)] (Just . refinement $ tmBit True)
          ]
    test_Enumerated =
      TestLabel "Enumerated" $
        TestList
          [ makeTest' "enum A = int32 { }" . pure $ DeclarationEnumerated <$> enumerated "A" (tyInt 32) [],
            makeTest' "enum A = int32 { A1 = 1 }" . pure $ DeclarationEnumerated <$> enumerated "A" (tyInt 32) [("A1", LiteralInteger 1)],
            makeTest' "enum A = int32 { A1 = 1 | A2 = 2 | A3 = 3 }" . pure $ DeclarationEnumerated <$> enumerated "A" (tyInt 32) [("A1", LiteralInteger 1), ("A2", LiteralInteger 2), ("A3", LiteralInteger 3)]
          ]

    test_Variant =
      TestLabel "Variant" $
        TestList
          [ makeTest' "data A {}" . pure $ DeclarationVariant <$> variant "A" [],
            makeTest' "data A { A1: int32 }" . pure $ DeclarationVariant <$> variant "A" [("A1", tyInt 32)],
            makeTest' "data A { A1: int32 | A2: bit | A3: char }" . pure $ DeclarationVariant <$> variant "A" [("A1", tyInt 32), ("A2", tyBit), ("A3", tyChar)]
          ]
    test_Newtype =
      TestLabel "Newtype" $
        TestList
          [ makeTest' "newtype A { a: int32 }" . pure $ DeclarationNewtype <$> newtype_ "A" False "a" (tyInt 32) Nothing,
            makeTest' "newtype message A { a: int32 }" . pure $ DeclarationNewtype <$> newtype_ "A" True "a" (tyInt 32) Nothing,
            makeTest' "newtype message A { a: int32; @assert(true) }" . pure $ DeclarationNewtype <$> newtype_ "A" True "a" (tyInt 32) (Just . refinement $ tmBit True)
          ]
    test_Alias =
      TestLabel "Alias" $
        TestList
          [ makeTest' "alias A = B" . pure $ DeclarationAlias <$> alias "A" (tyNamed "B")
          ]
    test_Function =
      TestLabel "Function" $
        TestList
          [ makeTest' "fun f() -> int32 = 1" . pure $ DeclarationFunction <$> function "f" False (functionTy [] [] (tyInt 32)) (tmInt 1),
            makeTest' "fun f(x: int32) -> int32 = x" . pure $ DeclarationFunction <$> function "f" False (functionTy [(Just "x", tyInt 32)] [] (tyInt 32)) (tmVar "x"),
            makeTest' "fun f(x: int32, y: bit, z: char) -> int32 = x" . pure $ DeclarationFunction <$> function "f" False (functionTy [(Just "x", tyInt 32), (Just "y", tyBit), (Just "z", tyChar)] [] (tyInt 32)) (tmVar "x"),
            makeTest' "fun f(x: int32, y: bit, z: char, _: [char]) -> int32 = x" . pure $ DeclarationFunction <$> function "f" False (functionTy [(Just "x", tyInt 32), (Just "y", tyBit), (Just "z", tyChar), (Nothing, tyArray tyChar)] [] (tyInt 32)) (tmVar "x"),
            makeTest' "fun f(x: int32, y: bit, z: char, _: [char]) {w: int32} -> int32 = x" . pure $ DeclarationFunction <$> function "f" False (functionTy [(Just "x", tyInt 32), (Just "y", tyBit), (Just "z", tyChar), (Nothing, tyArray tyChar)] [("w", tyInt 32)] (tyInt 32)) (tmVar "x"),
            makeTest' "fun f(x: int32, y: bit, z: char, _: [char]) {w: int32, u: bit, v: char} -> int32 = x" . pure $ DeclarationFunction <$> function "f" False (functionTy [(Just "x", tyInt 32), (Just "y", tyBit), (Just "z", tyChar), (Nothing, tyArray tyChar)] [("w", tyInt 32), ("u", tyBit), ("v", tyChar)] (tyInt 32)) (tmVar "x"),
            makeTest' "fun f(x: int32, y: bit, z: char, _: [char]) {w: int32, u: bit, v: char} -> int32 = x" . pure $ DeclarationFunction <$> function "f" False (functionTy [(Just "x", tyInt 32), (Just "y", tyBit), (Just "z", tyChar), (Nothing, tyArray tyChar)] [("w", tyInt 32), ("u", tyBit), ("v", tyChar)] (tyInt 32)) (tmVar "x")
          ]
    test_Constant =
      TestLabel "Constant" $
        TestList
          [ makeTest' "const x: int32 = 1" . pure $ DeclarationConstant <$> constant "x" (tyInt 32) (tmInt 1)
          ]

test_parseTerm :: Test
test_parseTerm =
  TestLabel "parseTerm" $
    TestList
      [ test_Literal,
        test_PrimFun,
        test_Array,
        test_Tuple,
        test_Block,
        test_Structure,
        test_Member,
        test_Constructor,
        test_Application,
        test_If,
        test_Ascribe,
        test_Match,
        test_Eq,
        test_Cast
      ]
  where
    makeTest' = makeTest parseTerm

    test_PrimFun :: Test
    test_PrimFun =
      TestLabel "PrimFun" $
        TestList
          [ makeTest' "a == b" (return $ tmAppPrimFun PrimFunEq [tmVar "a", tmVar "b"]),
            makeTest' "a || b" (return $ tmAppPrimFun PrimFunOr [tmVar "a", tmVar "b"]),
            makeTest' "a && b" (return $ tmAppPrimFun PrimFunAnd [tmVar "a", tmVar "b"]),
            makeTest' "a && b && c" (return $ tmAppPrimFun PrimFunAnd [tmAppPrimFun PrimFunAnd [tmVar "a", tmVar "b"], tmVar "c"]),
            makeTest' "!a" (return $ tmAppPrimFun PrimFunNot [tmVar "a"]),
            makeTest' "! a" (return $ tmAppPrimFun PrimFunNot [tmVar "a"])
          ]

    test_Array :: Test
    test_Array =
      TestLabel "TermArray" $
        TestList
          [ makeTest'
              "[]"
              (return $ tmArr []),
            makeTest'
              "[1, 2, 3]"
              (return $ tmArr [tmInt 1, tmInt 2, tmInt 3])
          ]

    test_Tuple :: Test
    test_Tuple =
      TestLabel "TermTuple" $
        TestList
          [ makeTest'
              "(1, 2, 3)"
              (return $ tmTup [tmInt 1, tmInt 2, tmInt 3])
          ]

    test_Literal :: Test
    test_Literal =
      TestLabel "TermLit" $
        TestList
          [ makeTest' "+1" (return $ tmInt 1),
            makeTest' "-1" (return $ tmInt (-1))
          ]

    test_Block :: Test
    test_Block =
      TestLabel "TermBlock" $
        TestList
          [ makeTest' "{ x }" . return $ tmBlock ([], tmVar "x"),
            makeTest' "{ let x = 1; x }" . return $ tmBlock ([stmtLet "x" (tmInt 1)], tmVar "x"),
            makeTest' "{ let x = 1; let y = 2; let z = 3; (x, y, z) }" . return $ tmBlock ([stmtLet "x" (tmInt 1), stmtLet "y" (tmInt 2), stmtLet "z" (tmInt 3)], tmTup [tmVar "x", tmVar "y", tmVar "z"])
          ]

    test_Structure :: Test
    test_Structure =
      TestLabel "TermStructure" $
        TestList
          [ makeTest' "S { x = 1 }" . return $ tmStruct "S" [("x", tmInt 1)],
            makeTest' "S { x = 1; y = 2; z = 3 }" . return $ tmStruct "S" [("x", tmInt 1), ("y", tmInt 2), ("z", tmInt 3)]
          ]

    test_Member :: Test
    test_Member =
      TestLabel "TermMember" $
        TestList
          [ makeTest' "x.y" . return $ tmMem (tmVar "x") "y",
            makeTest' "x.y.z" . return $ tmMem (tmMem (tmVar "x") "y") "z"
          ]

    test_Constructor :: Test
    test_Constructor =
      TestLabel "TermConstructor" $
        TestList
          [ makeTest' "C(x)" . return $ tmCnstr "C" (Just $ tmVar "x"),
            makeTest' "C(x, y)" . return $ tmCnstr "C" (Just $ tmTup [tmVar "x", tmVar "y"])
          ]

    test_Application :: Test
    test_Application =
      TestLabel "TermApplication" $
        TestList
          [ makeTest' "f(x)" . return $ tmApp "f" [tmVar "x"] Nothing,
            makeTest' "f(x, y, z)" . return $ tmApp "f" [tmVar "x", tmVar "y", tmVar "z"] Nothing,
            makeTest' "f(x) given (y)" . return $ tmApp "f" [tmVar "x"] (Just [tmVar "y"]),
            makeTest' "f(x, y, z) given (w, u ,v)" . return $ tmApp "f" [tmVar "x", tmVar "y", tmVar "z"] (Just [tmVar "w", tmVar "u", tmVar "v"])
          ]

    test_If :: Test
    test_If =
      TestLabel "TermIf" $
        TestList
          [ makeTest' "if x then y else z" . return $ tmIf (tmVar "x") (tmVar "y") (tmVar "z"),
            makeTest' "if x then if y then z else w else u" . return $ tmIf (tmVar "x") (tmIf (tmVar "y") (tmVar "z") (tmVar "w")) (tmVar "u")
          ]

    test_Ascribe :: Test
    test_Ascribe =
      TestLabel "TermAscribe" $
        TestList
          [ makeTest' "x : int32" . return $ tmAsc (tmVar "x") (tyInt 32)
          ]

    -- TODO: impl match
    test_Match :: Test
    test_Match = TestLabel "TermMatch" $ TestList []

    test_Eq :: Test
    test_Eq =
      TestLabel "TermEq" $
        TestList
          [ makeTest' "x == y" . return $ tmAppPrimFun PrimFunEq [tmVar "x", tmVar "y"],
            makeTest' "(x == y) == (x == y)" . return $
              tmAppPrimFun
                PrimFunEq
                [ tmAppPrimFun PrimFunEq [tmVar "x", tmVar "y"],
                  tmAppPrimFun PrimFunEq [tmVar "x", tmVar "y"]
                ]
          ]

    test_Cast :: Test
    test_Cast =
      TestLabel "TermCast" $
        TestList
          [ makeTest' "cast(x)" . return $ tmCast (tmVar "x")
          ]

test_parseStatement :: Test
test_parseStatement =
  TestLabel "parseStatement" $
    TestList
      [ makeTest' "let x = 1" (Just (stmtLet "x" (tmInt 1))),
        makeTest' "assert(x)" (Just (stmtAssert (tmVar "x")))
      ]
  where
    makeTest' = makeTest parseStatement

test_parseType :: Test
test_parseType =
  TestLabel "parseType" $
    TestList
      [ makeTest' "int32" . return $ tyInt 32,
        makeTest' "int64" . return $ tyInt 64,
        makeTest' "uint32" . return $ tyUInt 32,
        makeTest' "uint64" . return $ tyUInt 64,
        makeTest' "float32" . return $ tyFloat 32,
        makeTest' "float64" . return $ tyFloat 64,
        makeTest' "bit" . return $ return TypeBit,
        makeTest' "[int32]" . return $ tyArray (tyInt 32),
        makeTest' "[[int32]]" . return $ tyArray (tyArray (tyInt 32)),
        makeTest' "(int32, int64, int128)" . return $ tyTuple [tyInt 32, tyInt 64, tyInt 128],
        makeTest' "optional<int32>" . return $ tyOptional (tyInt 32),
        makeTest' "optional<[int32]>" . return $ tyOptional (tyArray (tyInt 32)),
        makeTest' "Example" . return $ tyNamed "Example",
        makeTest' "(A, B, C)" . return $ tyTuple [tyNamed "A", tyNamed "B", tyNamed "C"],
        makeTest' "optional<[A]>" . return $ tyOptional (tyArray (tyNamed "A"))
      ]
  where
    makeTest' = makeTest parseType

test_parseModule :: Test
test_parseModule =
  TestLabel "parseModule" $
    TestList
      [ let str =
              unlines
                [ "module Test",
                  "struct Struct { field: int32; @assert(True) }",
                  "alias Alias = int32",
                  "const c : int32 = 0",
                  "enum Enum = int32 { E = 0 }",
                  "fun f () -> int32 0",
                  "newtype N { value: int32; @assert(True) }"
                ]
         in makeTest' str . return $
              module_
                "Test"
                []
                [ DeclarationStructure <$> structure "Struct" False Nothing [("field", tyInt 32)] Nothing,
                  DeclarationAlias <$> alias "Alias" (tyInt 32),
                  DeclarationConstant <$> constant "c" (tyInt 32) (tmInt 0),
                  DeclarationEnumerated <$> enumerated "Enum" (tyInt 32) [("E", LiteralInteger 0)],
                  DeclarationFunction <$> function "f" False (functionTy [] [] (tyInt 32)) (tmInt 0),
                  DeclarationNewtype <$> newtype_ "N" False "n" (tyInt 32) Nothing
                ]
      ]
  where
    makeTest' = makeTest parseModule

parses_structures :: [(String, IO Structure)]
parses_structures =
  [ ("TODO", structure "Struct" False Nothing [("field", tyInt 32)] Nothing)
  ]

parses_aliases :: [(String, IO Alias)]
parses_aliases =
  [ ("TODO", alias "Alias" (tyInt 32))
  ]

parses_constants :: [(String, IO Constant)]
parses_constants =
  [ ("TODO", constant "c" (tyInt 32) (tmInt 0))
  ]

parses_enumerateds :: [(String, IO Enumerated)]
parses_enumerateds =
  [ ("TODO", enumerated "Enum" (tyInt 32) [("E", LiteralInteger 0)])
  ]

parses_functions :: [(String, IO Function)]
parses_functions =
  [ ("TODO", function "f" False (functionTy [] [] (tyInt 32)) (tmInt 0))
  ]

parses_newtypes :: [(String, IO Newtype)]
parses_newtypes =
  [ ("TODO", newtype_ "N" False "n" (tyInt 32) Nothing)
  ]

-- parses

-- utility

-- makeTest :: (Eq a, Show a) => Parser a -> String -> Maybe (IO a) -> Test
makeTest :: (Eq a, PrettyShow a) => Parser a -> String -> Maybe (IO a) -> Test
makeTest parser string result = TestCase do
  result' <- runParserT parser (emptyEnv topModuleId) ("Test(" <> string <> ")") string
  case (result', result) of
    (Right a, Just io_a') -> assertEqualPretty a =<< io_a'
    -- (Right a, Just io_a') -> assertEqual ("Test(" <> string <> ")") a =<< io_a'
    (Right a, Nothing) ->
      assertFailure $
        -- "the string \"" <> string <> "\" parses to \"" <> prettyShow a
        --   <> "\", but is expected to fail to parse"
        unlines ["the string", indent string, "parses to", indent (prettyShow a), "but is expected to fail to parse"]
    (Left err, Just io_a') -> do
      a' <- io_a'
      assertFailure $
        unlines
          [ "the string",
            indent string,
            "fails to parse, with parse error",
            indent (show err),
            "but is is expeted to parse to",
            indent (prettyShow a')
          ]
    (Left _err, Nothing) -> return ()
