# t-shogi (ShogiMorseEngine)

トポロジー・幾何学（モース理論・シジジー・Garside群）の概念を取り入れた Haskell 実装の USI 対応将棋エンジンです。

## 特徴

- **モースポテンシャル評価関数**: 盤面上の駒配置と玉の位置関係からポテンシャル場を算出し、自玉の安定度（囲いの堅さ）と敵玉への包囲・圧迫度を評価。
- **F5シジジーフィルター**: 指し手適用後の局面における玉への王手（攻撃射線）を事前判定し、自殺手や王手放置を完全排除。
- **Garside標準化キャッシュ（置換表）**: 過去の探索局面と最善手をキャッシュし、再探索を高速化。
- **トポロジー歴史履歴（千日手自動検出＆回避）**: 同一盤面の出現回数を追跡し、4回出現（千日手）となる手を動的に回避。回避不能な場合は USI 準拠の `bestmove draw` を送出。
- **USIプロトコル対応**: 将棋所、ShogiGUI などの各種 USI 対応将棋 GUI でそのまま対局可能。

## 必要要件

- GHC (The Glasgow Haskell Compiler) 9.x 以降

## ビルド方法

```bash
ghc -O2 ShogiUsiMorse-v4.hs -o ShogiUsiMorse.exe
```

## 使い方

### 1. コマンドラインからの実行

```bash
.\ShogiUsiMorse.exe
```

標準入力から USI コマンドを入力して対話できます。

```text
usi
isready
usinewgame
position startpos
go
quit
```

### 2. 将棋GUI（将棋所、ShogiGUI等）での利用

1. ビルドして生成された `ShogiUsiMorse.exe` を GUI のエンジン追加設定から登録します。
2. 対局相手に本エンジンを指定して対局を開始できます。

## ディレクトリ構成

- `ShogiUsiMorse-v4.hs` : エンジン本体（データ構造、合法手生成、F5フィルター、モース評価関数、千日手検出、USIループ）
- `README.md` : プロジェクト概要と利用手順
- `.gitignore` : ビルド生成物・一時ファイルの除外設定

## ライセンス

MIT License
