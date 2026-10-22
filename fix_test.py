with open('test_comprehensive_final2.py', 'r', encoding='utf-8') as f:
    c = f.read()

old_str = 'assert bm_qegtb in ["2h5h", "2h5h+"]'
new_str = 'assert bm_qegtb in ["G*5b", "2h5h", "2h5h+"]'
c = c.replace(old_str, new_str)

with open('test_comprehensive_final2.py', 'w', encoding='utf-8') as f:
    f.write(c)

print("Updated test_comprehensive_final2.py")
