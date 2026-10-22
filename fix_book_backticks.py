with open('GenerateStratifiedCompleteBook.hs', 'r', encoding='utf-8') as f:
    text = f.read()

text = text.replace(
    'Just (Piece _ pt) | pt elem [Rook, PRook, Bishop, PBishop, Silver, PSilver, Knight] ->',
    'Just (Piece _ pt) | pt `elem` [Rook, PRook, Bishop, PBishop, Silver, PSilver, Knight] ->'
)

with open('GenerateStratifiedCompleteBook.hs', 'w', encoding='utf-8') as f:
    f.write(text)

print("Restored backticks in GenerateStratifiedCompleteBook.hs!")
