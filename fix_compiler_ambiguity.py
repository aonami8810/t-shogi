with open('ShogiMorseCompiler.hs', 'r', encoding='utf-8') as f:
    c = f.read()

# LANGUAGE 拡張を追加
c = c.replace('  , "{-# LANGUAGE ScopedTypeVariables #-}"', '  , "{-# LANGUAGE ScopedTypeVariables #-}"\n  , "{-# LANGUAGE AllowAmbiguousTypes #-}"')

# class 定義を更新
old_class = '''class (KnownNat ply, KnownSymbol from, KnownSymbol to) =>
      SteepestDescentMorseFlow (ply :: Nat) (from :: Symbol) (to :: Symbol) where
  morseFlowPly   :: Proxy ply -> Integer
  morseFlowFrom  :: Proxy from -> String
  morseFlowTo    :: Proxy to -> String'''

new_class = '''class (KnownNat ply, KnownSymbol from, KnownSymbol to) =>
      SteepestDescentMorseFlow (ply :: Nat) (from :: Symbol) (to :: Symbol) where
  morseFlowStep  :: Proxy ply -> Proxy from -> Proxy to -> (Integer, String, String)'''

c = c.replace(old_class, new_class)

# instance 生成を更新
old_inst = '''    generateFlowInstance :: VPath -> String
    generateFlowInstance VPath{..} =
      unlines
        [ concat ["instance SteepestDescentMorseFlow ", show (min 512 (length moves)), " ", show fromNode, " ", show toNode, " where"]
        , concat ["  morseFlowPly _ = ", show (length moves)]
        , concat ["  morseFlowFrom _ = ", show fromNode]
        , concat ["  morseFlowTo _ = ", show toNode]
        ]'''

new_inst = '''    generateFlowInstance :: VPath -> String
    generateFlowInstance VPath{..} =
      unlines
        [ concat ["instance SteepestDescentMorseFlow ", show (min 512 (length moves)), " ", show fromNode, " ", show toNode, " where"]
        , concat ["  morseFlowStep _ _ _ = (", show (length moves), ", ", show fromNode, ", ", show toNode, ")"]
        ]'''

c = c.replace(old_inst, new_inst)

with open('ShogiMorseCompiler.hs', 'w', encoding='utf-8') as f:
    f.write(c)

print("Updated compiler with unambiguous SteepestDescentMorseFlow!")
