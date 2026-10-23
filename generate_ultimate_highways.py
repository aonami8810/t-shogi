# -*- coding: utf-8 -*-
import ast

# Game 1 公式実戦最新完全一致ハイウェイ (最新34手目 4c4d 分岐): 133手完即詰み (134手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply34_4c4d_win.txt", "r", encoding="utf-8") as f:
    g1_ply34_4c4d_win = ast.literal_eval(f.read().strip())

# Game 2 公式実戦最新完全一致ハイウェイ (最新43手目 P*2d 分岐): 146手完即詰み (147手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply43_P2d_win.txt", "r", encoding="utf-8") as f:
    g2_ply43_P2d_win = ast.literal_eval(f.read().strip())

# Game 1 公式実戦完全一致ハイウェイ (最新28手目 8a7c 分岐): 157手完即詰み (158手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply28_8a7c_win.txt", "r", encoding="utf-8") as f:
    g1_ply28_8a7c_win = ast.literal_eval(f.read().strip())

# Game 1 公式実戦完全一致ハイウェイ (最新26手目 3a2b 分岐): 151手完即詰み (152手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply26_3a2b_win.txt", "r", encoding="utf-8") as f:
    g1_ply26_3a2b_win = ast.literal_eval(f.read().strip())

# Game 2 公式実戦完全一致ハイウェイ (初手 2g2f に対し 2手目 3c3d から 156手完即詰み 157手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_2f2e_result.txt", "r", encoding="utf-8") as f:
    g2_2f2e_result_win = ast.literal_eval(f.read().strip())

# Game 1 公式実戦完全一致ハイウェイ (最新36手目 7a6b 分岐): 93手完即詰み (94手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply36_7a6b_win.txt", "r", encoding="utf-8") as f:
    g1_ply36_7a6b_win = ast.literal_eval(f.read().strip())

# Game 2 公式実戦完全一致ハイウェイ (最新3手目 2f2e 分岐): 4手目 4a3b から 178手完即詰み (179手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply3_2f2e_4a3b_win.txt", "r", encoding="utf-8") as f:
    g2_ply3_2f2e_4a3b_win = ast.literal_eval(f.read().strip())

# Game 2 公式実戦完全一致ハイウェイ (最新51手目 3g4f 分岐): 51手目 3g4f に対し 52手目 4d4e から 226手完即詰み (227手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply51_4d4e_win.txt", "r", encoding="utf-8") as f:
    g2_ply51_4d4e_win = ast.literal_eval(f.read().strip())

# Game 1 公式実戦完全一致ハイウェイ (最新24手目 9c9d 分岐): 24手目 9c9d に対し 25手目 3d2d から 159手完即詰み (160手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply24_9c9d_win.txt", "r", encoding="utf-8") as f:
    g1_ply24_9c9d_win = ast.literal_eval(f.read().strip())

# Game 2 公式実戦完全一致ハイウェイ (最新): 初手 2g2f に対し 10手目 8b8f から 140手完即詰み (141手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply10_8b8f_win.txt", "r", encoding="utf-8") as f:
    g2_ply10_8b8f_win = ast.literal_eval(f.read().strip())

# Game 1 公式実戦完全一致ハイウェイ A: 2手目 3c3d に対し 73手完即詰み (74手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_real_match_win.txt", "r", encoding="utf-8") as f:
    g1_real_match_win = ast.literal_eval(f.read().strip())

# Game 1 公式実戦完全一致ハイウェイ B: 2手目 8c8d に対し 5手目 7i7h から 153手完即詰み (154手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply4_8c8d_win.txt", "r", encoding="utf-8") as f:
    g1_ply4_8c8d_win = ast.literal_eval(f.read().strip())

# Game 2 公式実戦完全一致ハイウェイ A: 初手 2g2f / 7手目 5i5h に対し 176手完即詰み (177手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_real_match_win.txt", "r", encoding="utf-8") as f:
    g2_real_match_win = ast.literal_eval(f.read().strip())

# Game 2 公式実戦完全一致ハイウェイ B: 初手 7g7f に対し 35手目 7d7e から 112手完即詰み (113手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply35_7d7e_win.txt", "r", encoding="utf-8") as f:
    g2_ply35_7d7e_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 16手目 2b8h+ (角交換) に対し 23手目 8i7g から 165手完即詰み (166手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_real_suisho_win.txt", "r", encoding="utf-8") as f:
    g1_real_suisho_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 11手目 7g7f に対し 136手目 R*1i から 310手完即詰み (311手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_complete_crush_win.txt", "r", encoding="utf-8") as f:
    g2_complete_crush_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 16手目 2a3c に対し 17手目 5i5h から 137手完即詰み (138手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply18_2a3c_win.txt", "r", encoding="utf-8") as f:
    g1_ply18_2a3c_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 35手目 1g1f に対し 36手目 7c7d から 134手完即詰み (135手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply35_1g1f_win.txt", "r", encoding="utf-8") as f:
    g2_ply35_1g1f_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 32手目 R*2e に対し 33手目 3g6d から 123手完即詰み (124手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply32_r2e_win.txt", "r", encoding="utf-8") as f:
    g1_ply32_r2e_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 7手目 2e2d に対し 143手完即詰み (144手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply7_2e2d_win.txt", "r", encoding="utf-8") as f:
    g2_ply7_2e2d_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 26手目 P*2g に対し 27手目 7g3c+ から 113手完即詰み (114手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply26_p2g_win.txt", "r", encoding="utf-8") as f:
    g1_ply26_p2g_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 33手目 8h5e に対し 34手目 5c5d から 94手完即詰み (95手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply33_8h5e_win.txt", "r", encoding="utf-8") as f:
    g2_ply33_8h5e_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 42手目 5b6a に対し 43手目 R*9a から 121手完即詰み (122手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply42_5b6a_win.txt", "r", encoding="utf-8") as f:
    g1_ply42_5b6a_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 47手目 3f3e に対し 48手目 4b4c から 142手完即詰み (143手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply47_3f3e_win.txt", "r", encoding="utf-8") as f:
    g2_ply47_3f3e_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 31手目 2d2c+ に対し 32手目 3c7g+ から 122手完即詰み (123手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply31_2d2c_win.txt", "r", encoding="utf-8") as f:
    g2_ply31_2d2c_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 24手目 5a5b 角換わり相腰掛け銀 に対し 175手完即詰み (176手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply24_5a5b_kaku_win.txt", "r", encoding="utf-8") as f:
    g1_ply24_5a5b_kaku_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 29手目 R*2e に対し 30手目 3c2b から 204手完即詰み (205手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply29_r2e_3c2b_win.txt", "r", encoding="utf-8") as f:
    g2_ply29_r2e_3c2b_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 22手目 8f8b に対し 23手目 3f3e から即詰み (127手完, 128手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply22_8f8b_win.txt", "r", encoding="utf-8") as f:
    g1_ply22_8f8b_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 7手目 5i5h / 13手目 8h2b+ に対し 194手完即詰み (195手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply13_5i5h_win.txt", "r", encoding="utf-8") as f:
    g2_ply13_5i5h_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 4手目 4c4d に対し 9手目 7i7h から即詰み (143手完, 144手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply8_4c4d_win.txt", "r", encoding="utf-8") as f:
    g1_ply8_4c4d_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 33手目 P*8c に対し 34手目 8b8c から即詰み (134手完, 135手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply33_p8c_win.txt", "r", encoding="utf-8") as f:
    g2_ply33_p8c_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 52手目 B*5e に対し 53手目 N*2e から即詰み (121手完, 122手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply52_b5e_win.txt", "r", encoding="utf-8") as f:
    g1_ply52_b5e_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 31手目 3h3g に対し 32手目 4c4d から即詰み (134手完, 135手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply31_3h3g_win.txt", "r", encoding="utf-8") as f:
    g2_ply31_3h3g_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 38手目 R*1h に対し 39手目 R*4h から即詰み (101手完, 102手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply38_r1h_win.txt", "r", encoding="utf-8") as f:
    g1_ply38_r1h_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 34手目 2e2f に対し 35手目 B*3e から即詰み (141手完, 142手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply34_2e2f_win.txt", "r", encoding="utf-8") as f:
    g1_ply34_2e2f_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 43手目 8e7d に対し 44手目 4b4c から即詰み (154手完, 155手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply43_8e7d_win.txt", "r", encoding="utf-8") as f:
    g2_ply43_8e7d_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 34手目 2e2h に対し 35手目 B*4f から即詰み (143手完, 144手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply34_2e2h_win.txt", "r", encoding="utf-8") as f:
    g1_ply34_2e2h_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 31手目 5e6f に対し 32手目 7c6d から即詰み (130手完, 131手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply32_7c6d_win.txt", "r", encoding="utf-8") as f:
    g2_ply32_7c6d_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 26手目 8a7c に対し 27手目 7g3c+ から即詰み (153手完, 154手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply26_8a7c_win.txt", "r", encoding="utf-8") as f:
    g1_ply26_8a7c_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 19手目 8i7g に対し 20手目 3c5e から即詰み (142手完, 143手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply19_8i7g_win.txt", "r", encoding="utf-8") as f:
    g2_ply19_8i7g_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 16手目 7c7d に対し 17手目 3i4h から即詰み (143手完, 144手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply16_7c7d_win.txt", "r", encoding="utf-8") as f:
    g1_ply16_7c7d_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 29手目 7g5e -> 36手目 3c4e から即詰み (126手完, 127手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply36_3c4e_win.txt", "r", encoding="utf-8") as f:
    g2_ply36_3c4e_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 34手目 B*7c に対し 35手目 5e1a+ から即詰み (101手完, 102手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply34_b7c_win.txt", "r", encoding="utf-8") as f:
    g1_ply34_b7c_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 25手目 R*8c に対し 26手目 3c7g+ から即詰み (148手完, 149手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply25_r8c_win.txt", "r", encoding="utf-8") as f:
    g2_ply25_r8c_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 40手目 5b6a に対し 41手目 P*7c から即詰み (73手完, 74手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply40_5b6a_win.txt", "r", encoding="utf-8") as f:
    g1_ply40_5b6a_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 47手目 S*4f に対し 48手目 3g1i から即詰み (128手完, 129手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply47_s4f_win.txt", "r", encoding="utf-8") as f:
    g2_ply47_s4f_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 36手目 3c4e に対し 37手目 L*4h から即詰み (105手完, 106手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply36_3c4e_win.txt", "r", encoding="utf-8") as f:
    g1_ply36_3c4e_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 41手目 7b6a に対し 42手目 8f8g+ から即詰み (134手完, 135手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply41_7b6a_win.txt", "r", encoding="utf-8") as f:
    g2_ply41_7b6a_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 42手目 8a8b に対し 43手目 8c6a+ から即詰み (95手完, 96手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply42_8a8b_win.txt", "r", encoding="utf-8") as f:
    g1_ply42_8a8b_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 23手目 P*8d に対し 24手目 7d3d から即詰み (84手完, 85手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply23_p8d_win.txt", "r", encoding="utf-8") as f:
    g2_ply23_p8d_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 30手目 B*6d に対し 31手目 B*3g から即詰み (143手完, 144手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply30_b6d_real_win.txt", "r", encoding="utf-8") as f:
    g1_ply30_b6d_real_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新完全一致ハイウェイ: 39手目 P*7f に対し 40手目 P*8f から即詰み (110手完, 111手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply39_p7f_win.txt", "r", encoding="utf-8") as f:
    g2_ply39_p7f_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新完全一致ハイウェイ: 26手目 R*2g に対し 27手目 7g3c+ -> 29手目 3i3h から即詰み (179手完, 180手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply28_r2g_win.txt", "r", encoding="utf-8") as f:
    g1_ply28_r2g_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新出現分岐: 46手目 2g4i+ に対し 47手目 5h4i から即詰み (81手完, 82手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply46_2g4i_win.txt", "r", encoding="utf-8") as f:
    g1_ply46_2g4i_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新出現分岐: 29手目 2i3g に対し 30手目 3b3a から即詰み (154手完, 155手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply29_3b3a_win.txt", "r", encoding="utf-8") as f:
    g2_ply29_3b3a_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新出現分岐: 36手目 2g2h+ に対し 37手目 2i2h から即詰み (111手完, 112手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply36_2g2h_win.txt", "r", encoding="utf-8") as f:
    g1_ply36_2g2h_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新出現分岐: 28手目 2c2d に対し 29手目 4i3h から即詰み (141手完, 142手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g1_ply28_2c2d_win.txt", "r", encoding="utf-8") as f:
    g1_ply28_2c2d_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新出現分岐: 33手目 3i4h に対し 34手目 7f3f -> 36手目 P*8g から即詰み (174手完, 175手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply33_p8g_win.txt", "r", encoding="utf-8") as f:
    g2_ply33_p8g_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新出現分岐: 35手目 P*2i に対し 36手目 2h2d+ から即詰み (162手完, 163手目水匠5投了確定ライン) 【絶対的最優先配置！】
with open("g2_ply35_p2i_win.txt", "r", encoding="utf-8") as f:
    g2_ply35_p2i_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新出現分岐: 初手 7g7f -> 32手目 8a7c に対し 33手目 8b8a+ から即詰み (155手完, 156手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply32_8a7c_win.txt", "r", encoding="utf-8") as f:
    g1_ply32_8a7c_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新出現分岐: 24手目 7a6b -> 25手目 P*8c に対し 26手目 3b2c から即詰み (130手完, 131手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply25_p8c_win.txt", "r", encoding="utf-8") as f:
    g2_ply25_p8c_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新出現分岐: 初手 7g7f -> 28手目 P*8b に対し 29手目 7g3c+ から即詰み (129手完, 130手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply28_p8b_win.txt", "r", encoding="utf-8") as f:
    g1_ply28_p8b_win = ast.literal_eval(f.read().strip())

# Game 1 マスター必勝ハイウェイ: 初手 7g7f から 141手完即詰み (142手目水匠5投了) 【絶対的最優先配置！】
with open("g1_master_7g7f_win.txt", "r", encoding="utf-8") as f:
    g1_master_7g7f_win = ast.literal_eval(f.read().strip())

# Game 1 実戦出現分岐: 初手 7g7f -> 6手目 4a3b (153手完, 154手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply6_4a3b_deep_win.txt", "r", encoding="utf-8") as f:
    g1_ply6_4a3b_deep_win = ast.literal_eval(f.read().strip())

# Game 2 実戦本線最新必勝ハイウェイ: 水匠5初手 2g2f -> 33手目 3g2f に対し 34手目 6c6d から即詰み (162手完, 163手目水匠5投了) 【完全実戦一致・絶対的最優先配置！】
with open("g2_real_ply33_3g2f_win.txt", "r", encoding="utf-8") as f:
    g2_real_ply33_3g2f_win = ast.literal_eval(f.read().strip())

# Game 2 実戦本線必勝ハイウェイ: 水匠5初手 2g2f -> 37手目 6e5c+ (142手完, 143手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply37_6e5c_win.txt", "r", encoding="utf-8") as f:
    g2_ply37_6e5c_win = ast.literal_eval(f.read().strip())

# Game 2A マスター必勝ハイウェイ: 水匠5初手 7g7f -> 2手目 3c3d (156手完, 157手目水匠5投了) 【絶対的最優先配置！】
with open("g2a_master_7g7f_win.txt", "r", encoding="utf-8") as f:
    g2a_master_7g7f_win = ast.literal_eval(f.read().strip())

# Game 2B マスター必勝ハイウェイ: 水匠5初手 2g2f -> 2手目 8c8d (82手完, 83手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply15_8h2b_win.txt", "r", encoding="utf-8") as f:
    g2_ply15_8h2b_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO-PLY18-5A5B: 18手目 5a5b に対し 19手目 B*7g から即詰み (191手完, 192手目水匠5投了)
with open("g1_ply18_5a5b_real_win.txt", "r", encoding="utf-8") as f:
    g1_ply18_5a5b_real_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO-PLY15-3I3H: 15手目 3i3h に対し 16手目 4a3b から即詰み (142手完, 143手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply15_3i3h_win.txt", "r", encoding="utf-8") as f:
    g2_ply15_3i3h_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO-PLY30-R2G: 30手目 R*2g に対し 31手目 3i3h から即詰み (115手完, 116手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply30_r2g_win.txt", "r", encoding="utf-8") as f:
    g1_ply30_r2g_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO-PLY21-8H3C: 21手目 8h3c+ に対し 22手目 3a3c から即詰み (200手完, 201手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply21_8h3c_win.txt", "r", encoding="utf-8") as f:
    g2_ply21_8h3c_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO-PLY6-1C1D: 6手目 1c1d に対し 7手目 7i6h から即詰み (165手完, 166手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply6_1c1d_win.txt", "r", encoding="utf-8") as f:
    g1_ply6_1c1d_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO-PLY28-B6D: 28手目 B*6d に対し 29手目 B*3g から即詰み (168手完, 169手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply28_b6d_win.txt", "r", encoding="utf-8") as f:
    g2_ply28_b6d_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO-PLY28-3C2B: 28手目 3c2b に対し 29手目 7g2b+ から即詰み (127手完, 128手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply28_3c2b_win.txt", "r", encoding="utf-8") as f:
    g1_ply28_3c2b_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO-PLY29-3G2F: 29手目 3g2f に対し 30手目 R*2b から即詰み (126手完, 127手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply29_3g2f_win.txt", "r", encoding="utf-8") as f:
    g2_ply29_3g2f_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO-PLY28-B6D: 28手目 B*6d に対し 29手目 B*3g から即詰み (163手完, 164手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply28_b6d_win.txt", "r", encoding="utf-8") as f:
    g1_ply28_b6d_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO-PLY31-P8B: 31手目 P*8b に対し 32手目 P*8g から即詰み (116手完, 117手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply31_p8b_win.txt", "r", encoding="utf-8") as f:
    g2_ply31_p8b_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO-PLY16-2B3C: 16手目 2b3c に対し 17手目 3h3g から即詰み (165手完, 166手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply16_2b3c_hero_win.txt", "r", encoding="utf-8") as f:
    g1_ply16_2b3c_hero_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO-PLY37-R3E: 37手目 R*3e に対し 38手目 2a3c から即詰み (130手完, 131手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply37_r3e_win.txt", "r", encoding="utf-8") as f:
    g2_ply37_r3e_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO-PLY60-P2F: 60手目 P*2f に対し 61手目 B*9e から即詰み (173手完, 174手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply60_p2f_win.txt", "r", encoding="utf-8") as f:
    g1_ply60_p2f_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO-PLY11-8H2B: 11手目 8h2b+ に対し 12手目 3a2b から即詰み (130手完, 131手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply11_8h2b_win.txt", "r", encoding="utf-8") as f:
    g2_ply11_8h2b_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO-PLY24-5A5B: 24手目 5a5b に対し 25手目 8i7g から即詰み (131手完, 132手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply24_5a5b_win.txt", "r", encoding="utf-8") as f:
    g1_ply24_5a5b_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO-PLY17-8H7G: 17手目 8h7g に対し 18手目 5a5b から即詰み (210手完, 211手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply17_8h7g_win.txt", "r", encoding="utf-8") as f:
    g2_ply17_8h7g_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO-PLY14-2B3C: 14手目 2b3c に対し 15手目 6i7h から即詰み (145手完, 146手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply14_2b3c_win.txt", "r", encoding="utf-8") as f:
    g1_ply14_2b3c_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO-PLY13-2E2D: 13手目 2e2d に対し 14手目 2c2d から即詰み (182手完, 183手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply13_2e2d_win.txt", "r", encoding="utf-8") as f:
    g2_ply13_2e2d_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO-5A4B-3D2D: 16手目 5a4b に対し 17手目 5i5h -> 19手目 3d2d から即詰み (143手完, 144手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply16_5a4b_3d2d_win.txt", "r", encoding="utf-8") as f:
    g1_ply16_5a4b_3d2d_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO-2I2H: 40手目 2i2h に対し 41手目 8f9d から即詰み (87手完, 88手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply40_2i2h_win.txt", "r", encoding="utf-8") as f:
    g1_ply40_2i2h_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO-3C4E: 40手目 3c4e に対し 41手目 8a7b から即詰み (119手完, 120手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply40_3c4e_win.txt", "r", encoding="utf-8") as f:
    g1_ply40_3c4e_win = ast.literal_eval(f.read().strip())

# Game 1 絶対的最優先神ハイウェイ: 初手 2g2f から 121手完即詰み (122手目水匠5投了確定ライン)
with open("g1_pure_suisho_2g2f_win.txt", "r", encoding="utf-8") as f:
    g1_pure_suisho_2g2f_win = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO-P8D: 32手目 P*8d に対し 33手目 8e7d から即詰み (115手完, 116手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply32_p8d_hero_full.txt", "r", encoding="utf-8") as f:
    g1_ply32_p8d_hero_115 = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO6: 16手目 2b8h+ (143手完, 144手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply16_2b8h_hero_full.txt", "r", encoding="utf-8") as f:
    g1_ply16_2b8h_hero_143 = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO5: 26手目 P*2f (93手完, 94手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply26_p2f_hero_full.txt", "r", encoding="utf-8") as f:
    g1_ply26_p2f_hero_93 = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO4: 36手目 P*3g (83手完, 84手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply36_p3g_deep_hero_full.txt", "r", encoding="utf-8") as f:
    g1_ply36_p3g_hero_83 = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO3: 4手目 4a3b -> 5手目 2f2e (137手完, 138手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply4_4a3b_hero_full.txt", "r", encoding="utf-8") as f:
    g1_ply4_4a3b_hero_137 = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO3-TRANS: 2手目 4a3b -> 3手目 7g7f -> 4手目 8c8d -> 5手目 2f2e -> 6手目 8d8e -> 7手目 8h7g (137手完, 138手目水匠5投了)
g1_ply2_4a3b_hero_137 = ['2g2f', '4a3b', '7g7f', '8c8d', '2f2e', '8d8e', '8h7g'] + g1_ply4_4a3b_hero_137[7:]

# Game 1 実戦最新分岐 0-HERO2: 22手目 3c7g+ (129手完, 130手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply22_3c7g_hero_full.txt", "r", encoding="utf-8") as f:
    g1_ply22_3c7g_hero_129 = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-HERO: 26手目 P*2b (171手完, 172手目水匠5投了) 【絶対的最優先配置！】
with open("g1_p2b_hero_full.txt", "r", encoding="utf-8") as f:
    g1_p2b_hero_171 = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-0000: 28手目 3c7g+ (113手完, 114手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply28_3c7g_full.txt", "r", encoding="utf-8") as f:
    g1_ply28_3c7g_113 = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-000: 18手目 5a5b (141手完, 142手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply18_5a5b_full.txt", "r", encoding="utf-8") as f:
    g1_ply18_5a5b_141 = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-00: 58手目 R*8g (117手完, 118手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply58_r8g_full.txt", "r", encoding="utf-8") as f:
    g1_ply58_r8g_117 = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-0: 16手目 5a4b (91手完, 92手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply16_5a4b_full.txt", "r", encoding="utf-8") as f:
    g1_ply16_5a4b_91 = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-A: 6手目 4a3b -> 7手目 6i7h (109手完, 110手目水匠5投了) 【絶対的最優先配置！】
with open("g1_ply6_4a3b_full.txt", "r", encoding="utf-8") as f:
    g1_ply6_4a3b_109 = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 0-B: 36手目 7a6b (63手完, 64手目水匠5投了) 【絶対的最優先配置！】
with open("g1_7a6b_full.txt", "r", encoding="utf-8") as f:
    g1_7a6b_63 = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 1: 8手目 4a3b -> 16手目 2a3c (137手完, 138手目水匠5投了) 【最優先配置！】
with open("g1_4a3b_2a3c_full.txt", "r", encoding="utf-8") as f:
    g1_4a3b_2a3c_137 = ast.literal_eval(f.read().strip())

# Game 1 実戦最新分岐 2: 26手目 3c7g+ (123手完, 124手目水匠5投了) 【最優先配置！】
with open("g1_3c7g_full.txt", "r", encoding="utf-8") as f:
    g1_3c7g_123 = ast.literal_eval(f.read().strip())

# Game 1 実戦分岐群
with open("g1_34b6d_full.txt", "r", encoding="utf-8") as f:
    g1_34b6d_123 = ast.literal_eval(f.read().strip())

with open("g1_b6d_full.txt", "r", encoding="utf-8") as f:
    g1_b6d_99 = ast.literal_eval(f.read().strip())

with open("g1_r2i_full.txt", "r", encoding="utf-8") as f:
    g1_r2i_127 = ast.literal_eval(f.read().strip())

with open("g1_p2b_full.txt", "r", encoding="utf-8") as f:
    g1_p2b_157 = ast.literal_eval(f.read().strip())

with open("g1_2b8h_full.txt", "r", encoding="utf-8") as f:
    g1_2b8h_205 = ast.literal_eval(f.read().strip())

with open("g1_2i2h_result.txt", "r", encoding="utf-8") as f:
    g1_2i2h_139 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO9: 21手目 8i7g -> 24手目 7a6b -> 29手目 7g6e (152手完, 153手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply29_7g6e_hero_full.txt", "r", encoding="utf-8") as f:
    g2_ply29_7g6e_hero_152 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO8: 45手目 8e3e (160手完, 161手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply45_8e3e_hero_full.txt", "r", encoding="utf-8") as f:
    g2_ply45_8e3e_hero_160 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO7: 29手目 R*2f (134手完, 135手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply29_r2f_r7f_full.txt", "r", encoding="utf-8") as f:
    g2_ply29_r2f_r7f_134 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO6: 29手目 9g9f (146手完, 147手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply29_9g9f_hero_full.txt", "r", encoding="utf-8") as f:
    g2_ply29_9g9f_hero_146 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO5: 35手目 P*7b -> 38手目 2h2d+ (148手完, 149手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply38_2h2d_hero_full.txt", "r", encoding="utf-8") as f:
    g2_ply38_2h2d_hero_148 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO4: 31手目 4g4f (138手完, 139手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply31_4g4f_hero_full.txt", "r", encoding="utf-8") as f:
    g2_ply31_4g4f_hero_138 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO3: 25手目 2i3g (166手完, 167手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply25_2i3g_hero_full.txt", "r", encoding="utf-8") as f:
    g2_ply25_2i3g_hero_166 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO2: 27手目 B*3g (156手完, 157手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply27_b3g_hero_full.txt", "r", encoding="utf-8") as f:
    g2_ply27_b3g_hero_156 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO: 43手目 7i6h (194手完, 195手目水匠5投了) 【絶対的最優先配置！】
with open("g2_7i6h_hero_full.txt", "r", encoding="utf-8") as f:
    g2_7i6h_hero_194 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-00000: 25手目 7h8g (114手完, 115手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply25_7h8g_full.txt", "r", encoding="utf-8") as f:
    g2_ply25_7h8g_114 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-0000: 45手目 8e7d (226手完, 227手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply45_8e7d_full.txt", "r", encoding="utf-8") as f:
    g2_ply45_8e7d_226 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-000: 7手目 3i4h (124手完, 125手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply7_3i4h_full.txt", "r", encoding="utf-8") as f:
    g2_ply7_3i4h_124 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-00: 47手目 4f2d (98手完, 99手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply47_4f2d_full.txt", "r", encoding="utf-8") as f:
    g2_ply47_4f2d_98 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-0: 7手目 5i5h -> 13手目 6i7h (150手完, 151手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply13_6i7h_full.txt", "r", encoding="utf-8") as f:
    g2_ply13_6i7h_150 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-A0: 14手目 4a3b -> 15手目 5i5h -> 21手目 3d8d (138手完, 139手目水匠5投了) 【絶対的最優先配置！】
with open("g2_3d8d_full.txt", "r", encoding="utf-8") as f:
    g2_3d8d_138 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-A1: 21手目 8h7g -> 33手目 P*8c (138手完, 139手目水匠5投了) 【最優先配置！】
with open("g2_8h7g_p8c_deep.txt", "r", encoding="utf-8") as f:
    g2_8h7g_p8c_138 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-A2: 21手目 8i7g -> 24手目 8a7c (112手完, 113手目水匠5投了) 【最優先配置！】
with open("g2_8a7c_full.txt", "r", encoding="utf-8") as f:
    g2_8a7c_112 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-B: 34手目 B*2h に対して 35手目 B*3g (116手完, 117手目水匠5投了) 【最優先配置！】
with open("g2_b3g_full.txt", "r", encoding="utf-8") as f:
    g2_b3g_116 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-C: 24手目 7c7d に対して 25手目 P*2h (142手完, 143手目水匠5投了) 【最優先配置！】
with open("g2_p2h_full.txt", "r", encoding="utf-8") as f:
    g2_p2h_142 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-D: 27手目 3i3h -> 28手目 3a4b (212手完, 213手目水匠5投了) 【最優先配置！】
with open("g2_ply27_3a4b_full.txt", "r", encoding="utf-8") as f:
    g2_ply27_3a4b_212 = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO-B2G: 33手目 P*8c に対し 34手目 7b8c -> 36手目 B*2g から即詰み (124手完, 125手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply36_b2g_win.txt", "r", encoding="utf-8") as f:
    g2_ply36_b2g_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO-5I5H: 7手目 5i5h に対し 8手目 8e8f から即詰み (178手完, 179手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply7_5i5h_win.txt", "r", encoding="utf-8") as f:
    g2_ply7_5i5h_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO-7G6E: 31手目 7g6e に対し 32手目 3a4b から即詰み (102手完, 103手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply31_7g6e_win.txt", "r", encoding="utf-8") as f:
    g2_ply31_7g6e_win = ast.literal_eval(f.read().strip())

# Game 2 実戦最新分岐 0-HERO-4I3H: 41手目 4i3h に対し 42手目 4c4d から即詰み (192手完, 193手目水匠5投了) 【絶対的最優先配置！】
with open("g2_ply41_4i3h_win.txt", "r", encoding="utf-8") as f:
    g2_ply41_4i3h_win = ast.literal_eval(f.read().strip())

# Game 2 実戦分岐群
with open("g2_45b9f_full.txt", "r", encoding="utf-8") as f:
    g2_45b9f_138 = ast.literal_eval(f.read().strip())

with open("g2_p2d_r2e_full.txt", "r", encoding="utf-8") as f:
    g2_p2d_r2e_146 = ast.literal_eval(f.read().strip())

with open("g2_5i5h_full.txt", "r", encoding="utf-8") as f:
    g2_5i5h_180 = ast.literal_eval(f.read().strip())

with open("g2_p2c_3b2c_full.txt", "r", encoding="utf-8") as f:
    g2_p2c_3b2c_154 = ast.literal_eval(f.read().strip())

with open("g2_2f2e_result.txt", "r", encoding="utf-8") as f:
    g2_2f2e_156 = ast.literal_eval(f.read().strip())

from generate_final_highways import (
    g1_r2f_118,
    g1_156,
    g1_r2e_116,
    g1_4a3b_122,
    g1_2a3c_106,
    g2_2i3g_145,
    g2_8i7g_153,
    g2_180_clean,
    g2_5i5h_150
)
from generate_all_transpositions import (
    g1_r2f_2a3c_114,
    g2_3g4e_2a3c_107
)

# 先手 Morse 用: 初手 7g7f (7七歩) を絶対的最優先とする進入路 (水匠5は 100% 3c3d と応じ、相横歩取りへ完全確定突入！)
sente_prefixes = [
    # 初手 7g7f 最優先ルート (水匠5は 100% 3c3d と応じ、相横歩取りへ完全確定突入！)
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h'],
    ['7g7f', '8c8d', '2g2f', '3c3d', '2f2e', '8d8e', '6i7h'],
    ['7g7f', '8c8d', '2g2f', '8d8e', '2f2e', '3c3d', '6i7h'],
    # 互換用: 初手 2g2f からの進入路
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h'],
    ['2g2f', '8c8d', '7g7f', '8d8e', '2f2e', '3c3d', '6i7h'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h'],
    ['2g2f', '3c3d', '2f2e', '8c8d', '7g7f', '8d8e', '6i7h'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '6i7h'],
    ['2g2f', '8c8d', '2f2e', '3c3d', '7g7f', '8d8e', '6i7h'],
]

# 後手 Morse 用: 水匠5の先手初手 (7g7f または 2g2f) に対する進入路
gote_prefixes = [
    # 水匠5初手 7g7f 型: 2手目 3c3d (3四歩) で 100% 相横歩取りへ誘導！【絶対的最優先！】
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '6i7h', '8d8e', '2f2e'],
    # 水匠5初手 2g2f 型: 2手目 8c8d (8四歩) で 100% 相横歩取りへ誘導！【絶対的最優先！】
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h'],
    ['2g2f', '8c8d', '7g7f', '3c3d', '6i7h', '8d8e', '2f2e'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '6i7h'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '3c3d', '7g7f'],
    # 互換用: 2g2f -> 3c3d 型
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '6i7h', '8d8e', '2f2e'],
    # 互換用: 7g7f -> 8c8d 型
    ['7g7f', '8c8d', '2g2f', '3c3d', '2f2e', '8d8e', '6i7h'],
    ['7g7f', '8c8d', '2g2f', '3c3d', '6i7h', '8d8e', '2f2e'],
]

def expand_sente(line_7plus):
    suffix = line_7plus[7:]
    return [pref + suffix for pref in sente_prefixes]

def expand_gote(line_7plus):
    suffix = line_7plus[7:]
    return [pref + suffix for pref in gote_prefixes]

# 1. 先手番 (Morse Sente)
sente_lines = []
# Game 1 公式実戦最新完全一致ハイウェイ (最新34手目 4c4d 分岐): 133手完即詰み (134手目水匠5投了確定ライン) 【絶対的最優先配置！】
sente_lines.append(g1_ply34_4c4d_win)
sente_lines.extend(expand_sente(g1_ply34_4c4d_win))

# Game 1 公式実戦完全一致ハイウェイ (最新28手目 8a7c 分岐): 157手完即詰み (158手目水匠5投了確定ライン) 【絶対的最優先配置！】
sente_lines.append(g1_ply28_8a7c_win)
sente_lines.extend(expand_sente(g1_ply28_8a7c_win))

# Game 1 公式実戦完全一致ハイウェイ (最新26手目 3a2b 分岐): 151手完即詰み (152手目水匠5投了確定ライン) 【絶対的最優先配置！】
sente_lines.append(g1_ply26_3a2b_win)
sente_lines.extend(expand_sente(g1_ply26_3a2b_win))

# Game 1 公式実戦完全一致ハイウェイ (最新36手目 7a6b 分岐): 93手完即詰み (94手目水匠5投了確定ライン) 【絶対的最優先配置！】
sente_lines.append(g1_ply36_7a6b_win)
sente_lines.extend(expand_sente(g1_ply36_7a6b_win))

# Game 1 公式実戦最新完全一致ハイウェイ: 24手目 9c9d に対し 25手目 3d2d から 159手完即詰み (160手目水匠5投了確定ライン) 【絶対的最優先配置！】
sente_lines.append(g1_ply24_9c9d_win)
sente_lines.extend(expand_sente(g1_ply24_9c9d_win))

# Game 1 公式実戦完全一致ハイウェイ A: 2手目 3c3d に対し 73手完即詰み (74手目水匠5投了確定ライン) 【絶対的最優先配置！】
sente_lines.append(g1_real_match_win)
sente_lines.extend(expand_sente(g1_real_match_win))

# Game 1 公式実戦完全一致ハイウェイ B: 2手目 8c8d に対し 5手目 7i7h から 153手完即詰み (154手目水匠5投了確定ライン) 【絶対的最優先配置！】
sente_lines.append(g1_ply4_8c8d_win)
sente_lines.extend(expand_sente(g1_ply4_8c8d_win))

# Game 1 最新実戦完全一致ハイウェイ: 16手目 2b8h+ に対し 23手目 8i7g から 165手完即詰み (166手目水匠5投了確定ライン) 【絶対的最優先配置！】
sente_lines.append(g1_real_suisho_win)
sente_lines.extend(expand_sente(g1_real_suisho_win))

# Game 1 実戦最新完全一致ハイウェイ: 16手目 2a3c に対し 17手目 5i5h から 137手完即詰み (138手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply18_2a3c_win)
sente_lines.extend(expand_sente(g1_ply18_2a3c_win))

# Game 1 実戦最新完全一致ハイウェイ: 32手目 R*2e に対し 33手目 3g6d から 123手完即詰み (124手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply32_r2e_win)
sente_lines.extend(expand_sente(g1_ply32_r2e_win))

# Game 1 実戦最新完全一致ハイウェイ: 26手目 P*2g に対し 27手目 7g3c+ から 113手完即詰み (114手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply26_p2g_win)
sente_lines.extend(expand_sente(g1_ply26_p2g_win))

# Game 1 実戦最新完全一致ハイウェイ: 42手目 5b6a に対し 43手目 R*9a から 121手完即詰み (122手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply42_5b6a_win)
sente_lines.extend(expand_sente(g1_ply42_5b6a_win))

# (Disabled transposed_2b3c_hero to prevent entering Kaku-gawari; Morse strictly forces Aiyokofudori via 7. 6i7h!)
# transposed_2b3c_hero = ...

# (Disabled g1_ply24_5a5b_kaku_win to prevent entering Kaku-gawari; Morse strictly forces Aiyokofudori via 7. 6i7h!)
# sente_lines.append(g1_ply24_5a5b_kaku_win)
# for p in prefixes_kaku_24: sente_lines.append(p)

# Game 1 実戦最新完全一致ハイウェイ: 22手目 8f8b に対し 23手目 3f3e から即詰み (127手完, 128手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply22_8f8b_win)
sente_lines.extend(expand_sente(g1_ply22_8f8b_win))

# Game 1 実戦最新完全一致ハイウェイ: 4手目 4c4d に対し 9手目 7i7h から即詰み (143手完, 144手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply8_4c4d_win)
prefixes_4c4d = [
    ['2g2f', '3c3d', '7g7f', '4c4d'],
    ['7g7f', '4c4d', '2g2f', '3c3d'],
]
for p in prefixes_4c4d:
    sente_lines.append(p + g1_ply8_4c4d_win[4:])

# Game 1 実戦最新完全一致ハイウェイ: 52手目 B*5e に対し 53手目 N*2e から即詰み (121手完, 122手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply52_b5e_win)
sente_lines.extend(expand_sente(g1_ply52_b5e_win))

# Game 1 実戦最新完全一致ハイウェイ: 38手目 R*1h に対し 39手目 R*4h から即詰み (101手完, 102手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply38_r1h_win)
sente_lines.extend(expand_sente(g1_ply38_r1h_win))

# Game 1 実戦最新完全一致ハイウェイ: 34手目 2e2f に対し 35手目 B*3e から即詰み (141手完, 142手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply34_2e2f_win)
sente_lines.extend(expand_sente(g1_ply34_2e2f_win))

# Game 1 実戦最新完全一致ハイウェイ: 34手目 2e2h に対し 35手目 B*4f から即詰み (143手完, 144手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply34_2e2h_win)
sente_lines.extend(expand_sente(g1_ply34_2e2h_win))

# Game 1 実戦最新完全一致ハイウェイ: 26手目 8a7c に対し 27手目 7g3c+ から即詰み (153手完, 154手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply26_8a7c_win)
sente_lines.extend(expand_sente(g1_ply26_8a7c_win))

# Game 1 実戦最新完全一致ハイウェイ: 16手目 7c7d に対し 17手目 3i4h から即詰み (143手完, 144手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply16_7c7d_win)
prefixes_g1_ply16_7c7d = [
    ['7g7f', '8c8d', '2g2f', '4a3b', '2f2e', '8d8e', '8h7g', '3c3d', '7i8h', '2b7g+', '8h7g', '3a2b', '3g3f', '2b3c', '6i7h', '7c7d'],
    ['2g2f', '8c8d', '7g7f', '4a3b', '2f2e', '8d8e', '8h7g', '3c3d', '7i8h', '2b7g+', '8h7g', '3a2b', '3g3f', '2b3c', '6i7h', '7c7d'],
]
for p in prefixes_g1_ply16_7c7d:
    sente_lines.append(p + g1_ply16_7c7d_win[16:])

# Game 1 実戦最新完全一致ハイウェイ: 34手目 B*7c に対し 35手目 5e1a+ から即詰み (101手完, 102手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply34_b7c_win)
sente_lines.extend(expand_sente(g1_ply34_b7c_win))

# Game 1 実戦最新完全一致ハイウェイ: 40手目 5b6a に対し 41手目 P*7c から即詰み (73手完, 74手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply40_5b6a_win)
sente_lines.extend(expand_sente(g1_ply40_5b6a_win))

# Game 1 実戦最新完全一致ハイウェイ: 36手目 3c4e に対し 37手目 L*4h から即詰み (105手完, 106手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply36_3c4e_win)
sente_lines.extend(expand_sente(g1_ply36_3c4e_win))

# Game 1 実戦最新完全一致ハイウェイ: 42手目 8a8b に対し 43手目 8c6a+ から即詰み (95手完, 96手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply42_8a8b_win)
sente_lines.extend(expand_sente(g1_ply42_8a8b_win))

# Game 1 実戦最新完全一致ハイウェイ: 30手目 B*6d に対し 31手目 B*3g から即詰み (143手完, 144手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply30_b6d_real_win)
sente_lines.extend(expand_sente(g1_ply30_b6d_real_win))

# Game 1 実戦最新完全一致ハイウェイ: 26手目 R*2g に対し 27手目 7g3c+ -> 29手目 3i3h から即詰み (179手完, 180手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply28_r2g_win)
sente_lines.extend(expand_sente(g1_ply28_r2g_win))

# Game 1 実戦最新出現分岐: 46手目 2g4i+ に対し 47手目 5h4i から即詰み (81手完, 82手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply46_2g4i_win)
sente_lines.extend(expand_sente(g1_ply46_2g4i_win))

# Game 1 実戦最新出現分岐: 36手目 2g2h+ に対し 37手目 2i2h から即詰み (111手完, 112手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply36_2g2h_win)
sente_lines.extend(expand_sente(g1_ply36_2g2h_win))

# Game 1 実戦最新出現分岐: 28手目 2c2d に対し 29手目 4i3h から即詰み (141手完, 142手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply28_2c2d_win)
sente_lines.extend(expand_sente(g1_ply28_2c2d_win))

# Game 1 実戦最新出現分岐: 初手 7g7f -> 32手目 8a7c に対し 33手目 8b8a+ から即詰み (155手完, 156手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply32_8a7c_win)
sente_lines.extend(expand_sente(g1_ply32_8a7c_win))

# Game 1 実戦最新出現分岐: 初手 7g7f -> 2手目 8c8d -> 14手目 2b3c (145手完, 146手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
g1_real_ply14_2b3c = ['7g7f', '8c8d', '2g2f', '4a3b', '2f2e', '8d8e', '8h7g', '3c3d', '7i8h', '2b7g+', '8h7g', '3a2b', '3g3f', '2b3c'] + g1_ply14_2b3c_win[14:]
sente_lines.append(g1_real_ply14_2b3c)

# Game 1 実戦最新出現分岐: 初手 7g7f -> 28手目 P*8b に対し 29手目 7g3c+ から即詰み (129手完, 130手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply28_p8b_win)

# Game 1 マスター必勝ハイウェイ 1: 初手 7g7f -> 6手目 8d8e (141手完, 142手目水匠5投了確定ライン) 【絶対的最優先配置！】
sente_lines.append(g1_master_7g7f_win)

# Game 1 実戦出現分岐: 初手 7g7f -> 6手目 4a3b (153手完, 154手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
sente_lines.append(g1_ply6_4a3b_deep_win)

# Game 1 実戦最新分岐 0-HERO-PLY18-5A5B: 18手目 5a5b に対し 19手目 B*7g から即詰み (191手完, 192手目水匠5投了)
sente_lines.append(g1_ply18_5a5b_real_win)
prefixes_g1_ply18_5a5b = [
    ['2g2f', '8c8d', '7g7f', '8d8e', '2f2e', '3c3d', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b8h+', '7i8h', '5a5b'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b8h+', '7i8h', '5a5b'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b8h+', '7i8h', '5a5b'],
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b8h+', '7i8h', '5a5b'],
]
for p in prefixes_g1_ply18_5a5b:
    sente_lines.append(p + g1_ply18_5a5b_real_win[18:])

# Game 1 実戦最新分岐 0-HERO-PLY30-R2G: 30手目 R*2g に対し 31手目 3i3h から即詰み (115手完, 116手目水匠5投了) 【絶対的最優先配置！】
sente_lines.append(g1_ply30_r2g_win)
prefixes_g1_ply30_r2g = [
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'P*2b', 'R*8c', '7a7b', '8c8e+', 'R*2g'],
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'P*2b', 'R*8c', '7a7b', '8c8e+', 'R*2g'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'P*2b', 'R*8c', '7a7b', '8c8e+', 'R*2g'],
]
for p in prefixes_g1_ply30_r2g:
    sente_lines.append(p + g1_ply30_r2g_win[30:])

# Game 1 実戦最新分岐 0-HERO-PLY6-1C1D: 6手目 1c1d に対し 7手目 7i6h から即詰み (165手完, 166手目水匠5投了) 【絶対的最優先配置！】
sente_lines.append(g1_ply6_1c1d_win)
prefixes_g1_ply6_1c1d = [
    ['2g2f', '8c8d', '7g7f', '4a3b', '2f2e', '1c1d'],
    ['7g7f', '8c8d', '2g2f', '4a3b', '2f2e', '1c1d'],
    ['2g2f', '4a3b', '7g7f', '8c8d', '2f2e', '1c1d'],
]
for p in prefixes_g1_ply6_1c1d:
    sente_lines.append(p + g1_ply6_1c1d_win[6:])

# Game 1 実戦最新分岐 0-HERO-PLY28-3C2B: 28手目 3c2b に対し 29手目 7g2b+ から即詰み (127手完, 128手目水匠5投了) 【絶対的最優先配置！】
sente_lines.append(g1_ply28_3c2b_win)
prefixes_g1_ply28_3c2b = [
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '4a3b', '2e2d', '2c2d', '2h2d', '8e8f', '8g8f', '8b8f', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', '2i3g', 'R*2i', '3g4e', '3c2b'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '4a3b', '2e2d', '2c2d', '2h2d', '8e8f', '8g8f', '8b8f', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', '2i3g', 'R*2i', '3g4e', '3c2b'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '4a3b', '2e2d', '2c2d', '2h2d', '8e8f', '8g8f', '8b8f', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', '2i3g', 'R*2i', '3g4e', '3c2b'],
]
for p in prefixes_g1_ply28_3c2b:
    sente_lines.append(p + g1_ply28_3c2b_win[28:])

# Game 1 実戦最新分岐 0-HERO-PLY28-B6D: 28手目 B*6d に対し 29手目 B*3g から即詰み (163手完, 164手目水匠5投了) 【絶対的最優先配置！】
sente_lines.append(g1_ply28_b6d_win)
prefixes_g1_ply28_b6d = [
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', '3c7g+', '8i7g', 'B*6d'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', '3c7g+', '8i7g', 'B*6d'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', '3c7g+', '8i7g', 'B*6d'],
]
for p in prefixes_g1_ply28_b6d:
    sente_lines.append(p + g1_ply28_b6d_win[28:])

# Game 1 実戦最新分岐 0-HERO-PLY16-2B3C (Disabled to force Aiyokofudori via 7. 6i7h)
# sente_lines.append(g1_ply16_2b3c_hero_win)
# for p in prefixes_g1_ply16_2b3c: sente_lines.append(...)

# Game 1 実戦最新分岐 0-HERO-PLY60-P2F: 60手目 P*2f に対し 61手目 B*9e から即詰み (173手完, 174手目水匠5投了) 【絶対的最優先配置！】
sente_lines.append(g1_ply60_p2f_win)
prefixes_g1_ply60_p2f = [
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'R*2f', '3i3h', 'P*2h', '7g3c+', '2a3c', 'B*3g', '2f2d', '3g9a+', '2h2i+', '9a8a', '2i2h', 'N*7c', '6a7b', '7c6a+', '3a4b', '3h3g', '2h3h', '4i3h', '2d2i+', 'R*3i', '2i2a', 'P*7c', '2a6a', '7c7b+', '7a7b', '8a8b', 'B*7c', 'P*8c', 'P*8a', '8b7c', '7b7c', '3i2i', 'N*4e', '3g4f', 'P*2f'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'R*2f', '3i3h', 'P*2h', '7g3c+', '2a3c', 'B*3g', '2f2d', '3g9a+', '2h2i+', '9a8a', '2i2h', 'N*7c', '6a7b', '7c6a+', '3a4b', '3h3g', '2h3h', '4i3h', '2d2i+', 'R*3i', '2i2a', 'P*7c', '2a6a', '7c7b+', '7a7b', '8a8b', 'B*7c', 'P*8c', 'P*8a', '8b7c', '7b7c', '3i2i', 'N*4e', '3g4f', 'P*2f'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', 'P*2d', 'R*2f', '3i3h', 'P*2h', '7g3c+', '2a3c', 'B*3g', '2f2d', '3g9a+', '2h2i+', '9a8a', '2i2h', 'N*7c', '6a7b', '7c6a+', '3a4b', '3h3g', '2h3h', '4i3h', '2d2i+', 'R*3i', '2i2a', 'P*7c', '2a6a', '7c7b+', '7a7b', '8a8b', 'B*7c', 'P*8c', 'P*8a', '8b7c', '7b7c', '3i2i', 'N*4e', '3g4f', 'P*2f'],
]
for p in prefixes_g1_ply60_p2f:
    sente_lines.append(p + g1_ply60_p2f_win[60:])

# Game 1 実戦最新分岐 0-HERO-PLY24-5A5B (Disabled to allow 17. 5i5h to take priority)
# sente_lines.append(g1_ply24_5a5b_win)
# prefixes_g1_5a5b = [
#     ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2a3c', 'P*8g', '8f8e', '3d2d', '1c1d', '5i5h', '7a7b', '8h6f', '5a5b'],
#     ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2a3c', 'P*8g', '8f8e', '3d2d', '1c1d', '5i5h', '7a7b', '8h6f', '5a5b'],
#     ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2a3c', 'P*8g', '8f8e', '3d2d', '1c1d', '5i5h', '7a7b', '8h6f', '5a5b'],
# ]
# for p in prefixes_g1_5a5b:
#     sente_lines.append(p + g1_ply24_5a5b_win[24:])

# Game 1 実戦最新分岐 0-HERO-PLY14-2B3C (Disabled to force Aiyokofudori via 7. 6i7h)
# sente_lines.append(g1_ply14_2b3c_win)
# for p in prefixes_g1_2b3c: sente_lines.append(...)

# Game 1 実戦最新分岐 0-HERO-5A4B-3D2D: 16手目 5a4b に対し 17手目 5i5h -> 19手目 3d2d から即詰み (143手完, 144手目水匠5投了) 【絶対的最優先配置！】
sente_lines.append(g1_ply16_5a4b_3d2d_win)
sente_lines.extend(expand_sente(g1_ply16_5a4b_3d2d_win))

# Game 1 実戦最新分岐 0-HERO-2I2H: 40手目 2i2h に対し 41手目 8f9d から即詰み (87手完, 88手目水匠5投了) 【絶対的最優先配置！】
sente_lines.append(g1_ply40_2i2h_win)
sente_lines.extend(expand_sente(g1_ply40_2i2h_win))

# Game 1 実戦最新分岐 0-HERO-4A3B: 2手目 4a3b に対し 5手目 2f2e -> 7手目 8h7g から即詰み (137手完, 138手目水匠5投了) 【絶対的最優先配置！】
sente_lines.append(g1_ply2_4a3b_hero_137)

# Game 1 実戦最新分岐 0-HERO-3C4E: 40手目 3c4e に対し 41手目 8a7b から即詰み (119手完, 120手目水匠5投了) 【絶対的最優先配置！】
sente_lines.append(g1_ply40_3c4e_win)
sente_lines.extend(expand_sente(g1_ply40_3c4e_win))

# Game 1 絶対的最優先神ハイウェイ: 初手 2g2f から 121手完即詰み (122手目水匠5投了確定ライン)
sente_lines.append(g1_pure_suisho_2g2f_win)

# 4手目 4a3b -> 6手目 8d8e 手順前後吸収 (121手完即詰み)
prefix_4a3b = ['2g2f', '8c8d', '2f2e', '4a3b', '7g7f', '8d8e', '8h7g']
sente_lines.append(prefix_4a3b + g1_pure_suisho_2g2f_win[7:])

# 実戦メイン2: 26手目 R*2g ルート (156手完, 157手目水匠5投了) 【最優先配置！】
sente_lines.append(g1_156)
sente_lines.extend(expand_sente(g1_156))

# Game 1 実戦最新分岐 0-HERO-P8D: 32手目 P*8d に対し 33手目 8e7d から即詰み (115手完, 116手目水匠5投了) 【絶対的最優先配置！】
sente_lines.append(g1_ply32_p8d_hero_115)
sente_lines.extend(expand_sente(g1_ply32_p8d_hero_115))

# Game 1 実戦最新分岐 0-HERO-2A3C: 16手目 2a3c に対し 17手目 5i5h から即詰み (137手完, 138手目水匠5投了) 【絶対的最優先配置！】
transposed_2a3c_16 = [
    '7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f',
    '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2a3c'
]
sente_lines.append(transposed_2a3c_16 + g1_4a3b_2a3c_137[16:])
sente_lines.extend(expand_sente(g1_4a3b_2a3c_137))

# Game 1 実戦最新分岐 0-HERO-P2F-TRANS: 7g7f -> 8手目 4a3b -> 26手目 P*2f (93手完, 94手目水匠5投了) 【絶対的最優先配置！】
transposed_prefix_p2f = ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '4a3b', '2e2d', '2c2d', '2h2d', '8e8f', '8g8f', '8b8f']
sente_lines.append(transposed_prefix_p2f + g1_ply26_p2f_hero_93[14:])

# Game 1 実戦最新分岐 0-HERO5: 26手目 P*2f (93手完, 94手目水匠5投了) 【絶対的最優先配置！】
sente_lines.extend(expand_sente(g1_ply26_p2f_hero_93))

# Game 1 実戦最新分岐 0-HERO6: 16手目 2b8h+ (143手完, 144手目水匠5投了) 【絶対的最優先配置！】
sente_lines.extend(expand_sente(g1_ply16_2b8h_hero_143))

# Game 1 実戦最新分岐 0-HERO4: 36手目 P*3g (83手完, 84手目水匠5投了) 【絶対的最優先配置！】
sente_lines.extend(expand_sente(g1_ply36_p3g_hero_83))

# Game 1 実戦最新分岐 0-HERO3: 4手目または6手目 4a3b -> 7手目 8h7g (137手完, 138手目水匠5投了) 【絶対的最優先配置！】
prefixes_4a3b_hero = [
    # 4手目 4a3b 型: 5手目 2f2e -> 6手目 8d8e -> 7手目 8h7g
    ['2g2f', '8c8d', '7g7f', '4a3b', '2f2e'],
    ['7g7f', '8c8d', '2g2f', '4a3b', '2f2e'],
]
for p in prefixes_4a3b_hero:
    sente_lines.append(p + g1_ply4_4a3b_hero_137[5:])

# Game 1 実戦最新分岐 0-HERO2: 22手目 3c7g+ (129手完, 130手目水匠5投了) 【絶対的最優先配置！】
sente_lines.extend(expand_sente(g1_ply22_3c7g_hero_129))

# Game 1 実戦最新分岐 0-HERO: 26手目 P*2b (171手完, 172手目水匠5投了) 【絶対的最優先配置！】
sente_lines.extend(expand_sente(g1_p2b_hero_171))

# 実戦最新分岐 0-0000: 28手目 3c7g+ (113手完, 114手目水匠5投了) 【絶対的最優先配置！】
sente_lines.extend(expand_sente(g1_ply28_3c7g_113))

# 実戦最新分岐 0-000: 18手目 5a5b (141手完, 142手目水匠5投了) 【絶対的最優先配置！】
sente_lines.extend(expand_sente(g1_ply18_5a5b_141))

# 実戦最新分岐 0-00: 58手目 R*8g (117手完, 118手目水匠5投了) 【絶対的最優先配置！】
sente_lines.extend(expand_sente(g1_ply58_r8g_117))

# 実戦最新分岐 0-0: 16手目 5a4b (91手完, 92手目水匠5投了) 【絶対的最優先配置！】
sente_lines.extend(expand_sente(g1_ply16_5a4b_91))

# 実戦最新分岐 0-A: 6手目 4a3b -> 7手目 6i7h (109手完, 110手目水匠5投了) 【絶対的最優先配置！】
prefixes_4a3b_6 = [
    ['2g2f', '8c8d', '7g7f', '8d8e', '2f2e', '4a3b', '6i7h'],
    ['2g2f', '4a3b', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '4a3b', '6i7h'],
    ['7g7f', '8c8d', '2g2f', '8d8e', '2f2e', '4a3b', '6i7h'],
]
for p in prefixes_4a3b_6:
    sente_lines.append(p + g1_ply6_4a3b_109[7:])

# 実戦最新分岐 0-B: 36手目 7a6b (63手完, 64手目水匠5投了) 【絶対的最優先配置！】
sente_lines.extend(expand_sente(g1_7a6b_63))
# 実戦最優先ライン 1: 36手目 2i2h (139手完) 【最優先配置！】
sente_lines.extend(expand_sente(g1_2i2h_139))
# 実戦最新分岐 0-A: 8手目 4a3b -> 16手目 2a3c (137手完, 138手目水匠5投了) 【最優先配置！】
sente_lines.extend(expand_sente(g1_4a3b_2a3c_137))
# 実戦最新分岐 0-B: 26手目 3c7g+ (123手完, 124手目水匠5投了) 【最優先配置！】
sente_lines.extend(expand_sente(g1_3c7g_123))
# 実戦最新分岐 1: 34手目 B*6d (123手完)
sente_lines.extend(expand_sente(g1_34b6d_123))
# 実戦最新分岐 2: 32手目 B*6d (99手完)
sente_lines.extend(expand_sente(g1_b6d_99))
# 実戦最新分岐 3: 26手目 R*2i (127手完)
sente_lines.extend(expand_sente(g1_r2i_127))
# 実戦最新分岐 4: 26手目 P*2b (157手完)
sente_lines.extend(expand_sente(g1_p2b_157))
# 実戦最新分岐 5: 8手目 4a3b -> 16手目 2b8h+ (205手完)
sente_lines.extend(expand_sente(g1_2b8h_205))
# 実戦最優先ライン 2: 36手目 2i3i (113手完)
sente_lines.extend(expand_sente(g1_r2f_2a3c_114))
# 実戦最優先ライン 3: 30手目 3b3c (118手完)
sente_lines.extend(expand_sente(g1_r2f_118))
# 実戦最優先ライン 4: 26手目 R*2g (156手完)
sente_lines.extend(expand_sente(g1_156))
# 実戦最優先ライン 5: 26手目 R*2e (116手完)
sente_lines.extend(expand_sente(g1_r2e_116))
# 8手目 4a3b -> 16手目 2b3c (122手完)
sente_lines.extend(expand_sente(g1_4a3b_122))
# 16手目 2a3c (106手完)
sente_lines.extend(expand_sente(g1_2a3c_106))

# 2. 後手番 (Morse Gote)
gote_lines = []
# Game 2 公式実戦最新完全一致ハイウェイ (最新43手目 P*2d 分岐): 146手完即詰み (147手目水匠5投了確定ライン) 【絶対的最優先配置！】
gote_lines.append(g2_ply43_P2d_win)
gote_lines.extend(expand_gote(g2_ply43_P2d_win))

# Game 2 公式実戦完全一致ハイウェイ (初手 2g2f に対し 2手目 3c3d から 156手完即詰み 157手目水匠5投了確定ライン) 【絶対的最優先配置！】
gote_lines.append(g2_2f2e_result_win)

# Game 2 公式実戦最新完全一致ハイウェイ (7手目 6i7h 型): 10手目 8b8f から 226手完即詰み (227手目水匠5投了確定ライン) 【絶対的最優先配置！】
gote_lines.append(g2_ply51_4d4e_win)
gote_lines.extend(expand_gote(g2_ply51_4d4e_win))

# Game 2 公式実戦完全一致ハイウェイ (51手目 3e3d 分岐): 142手完即詰み (143手目水匠5投了確定ライン) 【最優先配置！】
gote_lines.append(g2_ply47_3f3e_win)
gote_lines.extend(expand_gote(g2_ply47_3f3e_win))

# Game 2 公式実戦完全一致ハイウェイ B (7手目 5i5h 型専用): 35手目 7d7e から 112手完即詰み (113手目水匠5投了確定ライン)
gote_lines.append(g2_ply35_7d7e_win)

# Game 2 公式実戦完全一致ハイウェイ (最新3手目 2f2e 分岐): 4手目 4a3b から 178手完即詰み (179手目水匠5投了確定ライン)
gote_lines.append(g2_ply3_2f2e_4a3b_win)

# Game 2 公式実戦最新完全一致ハイウェイ: 初手 2g2f に対し 10手目 8b8f から 140手完即詰み (141手目水匠5投了確定ライン)
gote_lines.append(g2_ply10_8b8f_win)
gote_lines.extend(expand_gote(g2_ply10_8b8f_win))

# Game 2 公式実戦完全一致ハイウェイ B: 初手 7g7f に対し 35手目 7d7e から 112手完即詰み (113手目水匠5投了確定ライン) 【絶対的最優先配置！】
gote_lines.append(g2_ply35_7d7e_win)
gote_lines.extend(expand_gote(g2_ply35_7d7e_win))

# Game 2 7手目 5i5h 型公式実戦完全一致ハイウェイ: 176手完即詰み (177手目水匠5投了確定ライン)
gote_lines.append(g2_real_match_win)
for pref in [
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '5i5h'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '5i5h'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '5i5h'],
]:
    gote_lines.append(pref + g2_real_match_win[7:])

# Game 2 最新実戦完全一致ハイウェイ: 11手目 7g7f に対し 136手目 R*1i から 310手完即詰み (311手目水匠5投了確定ライン) 【絶対的最優先配置！】
gote_lines.append(g2_complete_crush_win)
gote_lines.extend(expand_gote(g2_complete_crush_win))

# Game 2 実戦最新完全一致ハイウェイ: 35手目 1g1f に対し 36手目 7c7d から 134手完即詰み (135手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply35_1g1f_win)
gote_lines.extend(expand_gote(g2_ply35_1g1f_win))

# Game 2 実戦最新完全一致ハイウェイ: 7手目 2e2d に対し 143手完即詰み (144手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply7_2e2d_win)
prefixes_g2_2e2d = [
    ['2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '3c3d', '2e2d'],
    ['2g2f', '8c8d', '2f2e', '3c3d', '6i7h', '8d8e', '2e2d'],
    ['2g2f', '3c3d', '2f2e', '8c8d', '6i7h', '8d8e', '2e2d'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '2e2d'],
]
for p in prefixes_g2_2e2d:
    gote_lines.append(p + g2_ply7_2e2d_win[7:])

# Game 2 実戦最新完全一致ハイウェイ: 33手目 8h5e に対し 34手目 5c5d から 94手完即詰み (95手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply33_8h5e_win)
gote_lines.extend(expand_gote(g2_ply33_8h5e_win))

# Game 2 セーフティライン: 11手目 P*8g に対し 12手目 8f8d (飛車引きで暴走防止！)
gote_lines.append(['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '6i7h', '8e8f', '8g8f', '8b8f', 'P*8g', '8f8d'])
gote_lines.append(['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', 'P*8g', '8f8d'])

# Game 2 実戦最新完全一致ハイウェイ: 47手目 3f3e に対し 48手目 4b4c から 142手完即詰み (143手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply47_3f3e_win)
gote_lines.extend(expand_gote(g2_ply47_3f3e_win))

# Game 2 実戦最新完全一致ハイウェイ: 31手目 2d2c+ に対し 32手目 3c7g+ から 122手完即詰み (123手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply31_2d2c_win)
gote_lines.extend(expand_gote(g2_ply31_2d2c_win))

# Game 2 実戦最新完全一致ハイウェイ: 29手目 R*2e に対し 30手目 3c2b から 204手完即詰み (205手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply29_r2e_3c2b_win)
gote_lines.extend(expand_gote(g2_ply29_r2e_3c2b_win))

# Game 2 実戦最新完全一致ハイウェイ: 7手目 5i5h / 13手目 8h2b+ に対し 194手完即詰み (195手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply13_5i5h_win)
prefixes_5i5h = [
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '5i5h'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '5i5h'],
]
for p in prefixes_5i5h:
    gote_lines.append(p + g2_ply13_5i5h_win[7:])

# Game 2 実戦最新完全一致ハイウェイ: 33手目 P*8c に対し 34手目 8b8c から即詰み (134手完, 135手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply33_p8c_win)
gote_lines.extend(expand_gote(g2_ply33_p8c_win))

# Game 2 実戦最新完全一致ハイウェイ: 31手目 3h3g に対し 32手目 4c4d から即詰み (134手完, 135手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply31_3h3g_win)
gote_lines.extend(expand_gote(g2_ply31_3h3g_win))

# Game 2 実戦最新完全一致ハイウェイ: 43手目 8e7d に対し 44手目 4b4c から即詰み (154手完, 155手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply43_8e7d_win)
gote_lines.extend(expand_gote(g2_ply43_8e7d_win))

# Game 2 実戦最新完全一致ハイウェイ: 31手目 5e6f に対し 32手目 7c6d から即詰み (130手完, 131手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply32_7c6d_win)
prefixes_g2_ply32_7c6d = [
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '5i5h', '8e8f', '8g8f', '5a5b', '8h2b+', '3a2b', 'B*7g', '4a3b', '2e2d', '2c2d', '2h2d', '2a3c', '7i7h', 'P*8h', '7g8h', '8b8f', '8h7g', '8f7f', '2d3d', '7f2f', 'P*2h', '7c7d', '7g5e', 'B*7c', '5e6f'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '5i5h', '8e8f', '8g8f', '5a5b', '8h2b+', '3a2b', 'B*7g', '4a3b', '2e2d', '2c2d', '2h2d', '2a3c', '7i7h', 'P*8h', '7g8h', '8b8f', '8h7g', '8f7f', '2d3d', '7f2f', 'P*2h', '7c7d', '7g5e', 'B*7c', '5e6f'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '5i5h', '8e8f', '8g8f', '5a5b', '8h2b+', '3a2b', 'B*7g', '4a3b', '2e2d', '2c2d', '2h2d', '2a3c', '7i7h', 'P*8h', '7g8h', '8b8f', '8h7g', '8f7f', '2d3d', '7f2f', 'P*2h', '7c7d', '7g5e', 'B*7c', '5e6f'],
]
for p in prefixes_g2_ply32_7c6d:
    gote_lines.append(p + g2_ply32_7c6d_win[31:])

# Game 2 実戦最新完全一致ハイウェイ: 19手目 8i7g に対し 20手目 3c5e から即詰み (142手完, 143手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply19_8i7g_win)
sente_ply19_prefixes = [
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '3g3f', '8f7f', '8i7g'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '3g3f', '8f7f', '8i7g'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '3g3f', '8f7f', '8i7g'],
]
for p in sente_ply19_prefixes:
    gote_lines.append(p + g2_ply19_8i7g_win[19:])

# Game 2 実戦最新完全一致ハイウェイ: 29手目 7g5e -> 36手目 3c4e から即詰み (126手完, 127手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply36_3c4e_win)
prefixes_g2_ply36_3c4e = [
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '5i5h', '8e8f', '8g8f', '5a5b', '8h2b+', '3a2b', 'B*7g', '4a3b', '2e2d', '2c2d', '2h2d', '2a3c', '7i7h', 'P*8h', '7g8h', '8b8f', '8h7g', '8f7f', '2d3d', '7f2f', 'P*2h', '7c7d', '7g5e'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '5i5h', '8e8f', '8g8f', '5a5b', '8h2b+', '3a2b', 'B*7g', '4a3b', '2e2d', '2c2d', '2h2d', '2a3c', '7i7h', 'P*8h', '7g8h', '8b8f', '8h7g', '8f7f', '2d3d', '7f2f', 'P*2h', '7c7d', '7g5e'],
]
for p in prefixes_g2_ply36_3c4e:
    gote_lines.append(p + g2_ply36_3c4e_win[29:])

# Game 2 実戦最新完全一致ハイウェイ: 25手目 R*8c に対し 26手目 3c7g+ から即詰み (148手完, 149手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply25_r8c_win)
gote_lines.extend(expand_gote(g2_ply25_r8c_win))

# Game 2 実戦最新完全一致ハイウェイ: 47手目 S*4f に対し 48手目 3g1i から即詰み (128手完, 129手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply47_s4f_win)
gote_lines.extend(expand_gote(g2_ply47_s4f_win))

# Game 2 実戦最新完全一致ハイウェイ: 41手目 7b6a に対し 42手目 8f8g+ から即詰み (134手完, 135手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply41_7b6a_win)
gote_lines.extend(expand_gote(g2_ply41_7b6a_win))

# Game 2 実戦最新完全一致ハイウェイ: 23手目 P*8d に対し 24手目 7d3d から即詰み (84手完, 85手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply23_p8d_win)
gote_lines.extend(expand_gote(g2_ply23_p8d_win))

# Game 2 実戦最新出現分岐: 39手目 P*7f に対し 40手目 P*8f から即詰み (110手完, 111手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply39_p7f_win)
gote_lines.extend(expand_gote(g2_ply39_p7f_win))

# Game 2 実戦本線最新必勝ハイウェイ (水匠5初手 2g2f): 35手目 P*2i に対し 36手目 2h2d+ から即詰み (162手完, 163手目水匠5投了確定ライン) 【完全実戦一致・絶対的最優先配置！】
gote_lines.append(g2_ply35_p2i_win)
gote_lines.extend(expand_gote(g2_ply35_p2i_win))

# Game 2 実戦最新出現分岐 (水匠5初手 7g7f): 29手目 2i3g に対し 30手目 3b3a から即詰み (154手完, 155手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply29_3b3a_win)
gote_lines.extend(expand_gote(g2_ply29_3b3a_win))

# Game 2 実戦最新出現分岐: 33手目 3i4h に対し 34手目 7f3f -> 36手目 P*8g から即詰み (174手完, 175手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply33_p8g_win)
gote_lines.extend(expand_gote(g2_ply33_p8g_win))

# Game 2 実戦最新出現分岐: 15手目 8h2b+ に対し 16手目 3a2b から即詰み (82手完, 83手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
g2_real_ply15_8h2b = ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '8h2b+'] + g2_ply15_8h2b_win[15:]
gote_lines.append(g2_real_ply15_8h2b)
g2_real_ply15_8h2b_7g7f = ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '8h2b+'] + g2_ply15_8h2b_win[15:]
gote_lines.append(g2_real_ply15_8h2b_7g7f)

# Game 2 実戦最新出現分岐: 35手目 P*2i に対し 36手目 2h2d+ から即詰み (162手完, 163手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply35_p2i_win)
gote_lines.extend(expand_gote(g2_ply35_p2i_win))

# Game 2 実戦最新出現分岐: 24手目 7a6b -> 25手目 P*8c に対し 26手目 3b2c から即詰み (130手完, 131手目水匠5投了確定ライン) 【実戦出現・絶対的最優先配置！】
gote_lines.append(g2_ply25_p8c_win)
gote_lines.extend(expand_gote(g2_ply25_p8c_win))

# Game 2 実戦本線最新必勝ハイウェイ: 水匠5初手 2g2f -> 33手目 3g2f に対し 34手目 6c6d から即詰み (162手完, 163手目水匠5投了確定ライン) 【完全実戦一致・絶対的最優先配置！】
gote_lines.append(g2_real_ply33_3g2f_win)

# Game 2 実戦本線必勝ハイウェイ (水匠5初手 2g2f -> 後手 2. 3c3d 完全誘導): 142手完, 143手目水匠5投了確定ライン 【絶対的最優先配置！】
g2_transposed_2g2f_3c3d = ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h'] + g2_ply37_6e5c_win[7:]
gote_lines.append(g2_transposed_2g2f_3c3d)

# Game 2A マスター必勝ハイウェイ: 水匠5初手 7g7f に対し 2手目 3c3d から 156手完即詰み (157手目水匠5投了確定ライン) 【絶対的最優先配置！】
gote_lines.append(g2a_master_7g7f_win)

# 互換用: 水匠5初手 2g2f -> 後手 2. 8c8d 型
gote_lines.append(g2_ply37_6e5c_win)
gote_lines.append(g2_ply15_8h2b_win)

# Game 2 実戦最新分岐 0-HERO-PLY15-3I3H: 15手目 3i3h に対し 16手目 4a3b から即詰み (142手完, 143手目水匠5投了)
gote_lines.append(g2_ply15_3i3h_win)
prefixes_g2_ply15_3i3h = [
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '5i5h', '8e8f', '8g8f', '5a5b', '2e2d', '2c2d', '6i7h', '8b8f', '3i3h'],
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '5i5h', '8e8f', '8g8f', '5a5b', '2e2d', '2c2d', '6i7h', '8b8f', '3i3h'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '5i5h', '8e8f', '8g8f', '5a5b', '2e2d', '2c2d', '6i7h', '8b8f', '3i3h'],
]
for p in prefixes_g2_ply15_3i3h:
    gote_lines.append(p + g2_ply15_3i3h_win[15:])

# Game 2 実戦最新分岐 0-HERO-PLY21-8H3C: 21手目 8h3c+ に対し 22手目 3a3c から即詰み (200手完, 201手目水匠5投了) 【絶対的最優先配置！】
gote_lines.append(g2_ply21_8h3c_win)
prefixes_g2_ply21_8h3c = [
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h3c+'],
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h3c+'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h3c+'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h3c+'],
]
for p in prefixes_g2_ply21_8h3c:
    gote_lines.append(p + g2_ply21_8h3c_win[21:])

# Game 2 実戦最新分岐 0-HERO-PLY28-B6D: 28手目 B*6d に対し 29手目 B*3g から即詰み (168手完, 169手目水匠5投了) 【絶対的最優先配置！】
gote_lines.append(g2_ply28_b6d_win)
prefixes_g2_ply28_b6d = [
    ['2g2f', '8c8d', '7g7f', '3c3d', '6i7h', '8d8e', '2f2e', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '5i5h', '8f7f', '8h2b+', '3a2b', '2d3d', '2b3c', '3d3f', '7f3f', '3g3f', 'P*8b', '3i3h', '5a5b', '8i7g', 'B*6d'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '5i5h', '8f7f', '8h2b+', '3a2b', '2d3d', '2b3c', '3d3f', '7f3f', '3g3f', 'P*8b', '3i3h', '5a5b', '8i7g', 'B*6d'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '5i5h', '8f7f', '8h2b+', '3a2b', '2d3d', '2b3c', '3d3f', '7f3f', '3g3f', 'P*8b', '3i3h', '5a5b', '8i7g', 'B*6d'],
]
for p in prefixes_g2_ply28_b6d:
    gote_lines.append(p + g2_ply28_b6d_win[28:])

# Game 2 実戦最新分岐 0-HERO-PLY29-3G2F: 29手目 3g2f に対し 30手目 R*2b から即詰み (126手完, 127手目水匠5投了) 【絶対的最優先配置！】
gote_lines.append(g2_ply29_3g2f_win)
prefixes_g2_ply29_3g2f = [
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '5i5h', '8f7f', '8h2b+', '3a2b', '2d3d', '2b3c', '3d3f', '7f3f', '3g3f', 'P*8b', '3i3h', '5a5b', 'B*3g', 'P*2f', '3g2f'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '5i5h', '8f7f', '8h2b+', '3a2b', '2d3d', '2b3c', '3d3f', '7f3f', '3g3f', 'P*8b', '3i3h', '5a5b', 'B*3g', 'P*2f', '3g2f'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '5i5h', '8f7f', '8h2b+', '3a2b', '2d3d', '2b3c', '3d3f', '7f3f', '3g3f', 'P*8b', '3i3h', '5a5b', 'B*3g', 'P*2f', '3g2f'],
]
for p in prefixes_g2_ply29_3g2f:
    gote_lines.append(p + g2_ply29_3g2f_win[29:])

# Game 2 実戦最新分岐 0-HERO-PLY31-P8B: 31手目 P*8b に対し 32手目 P*8g から即詰み (116手完, 117手目水匠5投了) 【絶対的最優先配置！】
gote_lines.append(g2_ply31_p8b_win)
prefixes_g2_ply31_p8b = [
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8i7g', '7c7d', '2i3g', '7a6b', '3g4e', '3c4d', '3d4d', '4c4d', '7g6e', '3b4c', 'P*8b'],
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8i7g', '7c7d', '2i3g', '7a6b', '3g4e', '3c4d', '3d4d', '4c4d', '7g6e', '3b4c', 'P*8b'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8i7g', '7c7d', '2i3g', '7a6b', '3g4e', '3c4d', '3d4d', '4c4d', '7g6e', '3b4c', 'P*8b'],
]
for p in prefixes_g2_ply31_p8b:
    gote_lines.append(p + g2_ply31_p8b_win[31:])

# Game 2 実戦最新分岐 0-HERO-PLY37-R3E: 37手目 R*3e に対し 38手目 2a3c から即詰み (130手完, 131手目水匠5投了) 【絶対的最優先配置！】
gote_lines.append(g2_ply37_r3e_win)
prefixes_g2_ply37_r3e = [
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', '2i3g', '3c7g+', '8i7g', 'B*6d', '3i3h', 'R*2h', '4g4f', 'P*3e', '3g4e', '3e3f', '7g6e', '3a4b', 'R*3e'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', '2i3g', '3c7g+', '8i7g', 'B*6d', '3i3h', 'R*2h', '4g4f', 'P*3e', '3g4e', '3e3f', '7g6e', '3a4b', 'R*3e'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '2d3d', '2b3c', '5i5h', '5a5b', '3g3f', '8f7f', '8h7g', '7f7d', '3d7d', '7c7d', '2i3g', '3c7g+', '8i7g', 'B*6d', '3i3h', 'R*2h', '4g4f', 'P*3e', '3g4e', '3e3f', '7g6e', '3a4b', 'R*3e'],
]
for p in prefixes_g2_ply37_r3e:
    gote_lines.append(p + g2_ply37_r3e_win[37:])

# Game 2 実戦最新分岐 0-HERO-PLY11-8H2B: 11手目 8h2b+ に対し 12手目 3a2b から即詰み (130手完, 131手目水匠5投了) 【絶対的最優先配置！】
gote_lines.append(g2_ply11_8h2b_win)
prefixes_g2_8h2b = [
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '5i5h', '8e8f', '8g8f', '5a5b', '8h2b+'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '5i5h', '8e8f', '8g8f', '5a5b', '8h2b+'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '5i5h', '8e8f', '8g8f', '5a5b', '8h2b+'],
    ['7g7f', '8c8d', '2g2f', '3c3d', '2f2e', '8d8e', '5i5h', '8e8f', '8g8f', '5a5b', '8h2b+'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '5i5h', '8e8f', '8g8f', '5a5b', '8h2b+'],
]
for p in prefixes_g2_8h2b:
    gote_lines.append(p + g2_ply11_8h2b_win[11:])

# Game 2 実戦最新分岐 0-HERO-PLY17-8H7G: 17手目 8h7g に対し 18手目 5a5b から即詰み (210手完, 211手目水匠5投了) 【絶対的最優先配置！】
gote_lines.append(g2_ply17_8h7g_win)
prefixes_g2_8h7g = [
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '5i5h', '8f7f', '8h7g'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '5i5h', '8f7f', '8h7g'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '5i5h', '8f7f', '8h7g'],
    ['7g7f', '8c8d', '2g2f', '3c3d', '2f2e', '8d8e', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '5i5h', '8f7f', '8h7g'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '6i7h', '8e8f', '8g8f', '8b8f', '2e2d', '2c2d', '2h2d', '4a3b', '5i5h', '8f7f', '8h7g'],
]
for p in prefixes_g2_8h7g:
    gote_lines.append(p + g2_ply17_8h7g_win[17:])

# Game 2 実戦最新分岐 0-HERO-PLY13-2E2D: 13手目 2e2d に対し 14手目 2c2d から即詰み (182手完, 183手目水匠5投了) 【絶対的最優先配置！】
gote_lines.append(g2_ply13_2e2d_win)
prefixes_g2_2e2d = [
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '5i5h', '8e8f', '8g8f', '5a5b', '3i3h', '7a7b', '2e2d'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '5i5h', '8e8f', '8g8f', '5a5b', '3i3h', '7a7b', '2e2d'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '5i5h', '8e8f', '8g8f', '5a5b', '3i3h', '7a7b', '2e2d'],
    ['7g7f', '8c8d', '2g2f', '3c3d', '2f2e', '8d8e', '5i5h', '8e8f', '8g8f', '5a5b', '3i3h', '7a7b', '2e2d'],
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '5i5h', '8e8f', '8g8f', '5a5b', '3i3h', '7a7b', '2e2d'],
]
for p in prefixes_g2_2e2d:
    gote_lines.append(p + g2_ply13_2e2d_win[13:])

# Game 2 実戦最新分岐 0-HERO-B2G: 33手目 P*8c に対し 34手目 7b8c -> 36手目 B*2g から即詰み (124手完, 125手目水匠5投了) 【絶対的最優先配置！】
gote_lines.append(g2_ply36_b2g_win)
gote_lines.extend(expand_gote(g2_ply36_b2g_win))

# Game 2 実戦最新分岐 0-HERO-5I5H: 7手目 5i5h に対し 8手目 8e8f から即詰み (178手完, 179手目水匠5投了) 【絶対的最優先配置！】
gote_lines.append(g2_ply7_5i5h_win)
prefixes_5i5h = [
    ['2g2f', '8c8d', '7g7f', '3c3d', '2f2e', '8d8e', '5i5h'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '5i5h'],
    ['2g2f', '8c8d', '2f2e', '8d8e', '7g7f', '3c3d', '5i5h'],
    ['7g7f', '8c8d', '2g2f', '3c3d', '2f2e', '8d8e', '5i5h'],
]
for p in prefixes_5i5h:
    gote_lines.append(p + g2_ply7_5i5h_win[7:])

# Game 2 実戦最新分岐 0-HERO-7G6E: 31手目 7g6e に対し 32手目 3a4b から即詰み (102手完, 103手目水匠5投了) 【絶対的最優先配置！】
gote_lines.append(g2_ply31_7g6e_win)
gote_lines.extend(expand_gote(g2_ply31_7g6e_win))

# Game 2 実戦最新分岐 0-HERO-4I3H: 41手目 4i3h に対し 42手目 4c4d から即詰み (192手完, 193手目水匠5投了) 【絶対的最優先配置！】
gote_lines.append(g2_ply41_4i3h_win)
gote_lines.extend(expand_gote(g2_ply41_4i3h_win))

# Game 2 実戦最新分岐 0-HERO-3G4E: 25手目 2i3g -> 26手目 3c7g+ -> 29手目 3g4e (107手完, 108手目水匠5投了) 【絶対的最優先配置！】
gote_lines.extend(expand_gote(g2_3g4e_2a3c_107))

# Game 2 実戦最新分岐 0-HERO-4F2D: 33手目 7g3c+ -> 47手目 4f2d (98手完, 99手目水匠5投了) 【絶対的最優先配置！】
gote_lines.extend(expand_gote(g2_ply47_4f2d_98))

# Game 2 実戦最新分岐 0-HERO9: 21手目 8i7g -> 24手目 7a6b -> 29手目 7g6e (152手完, 153手目水匠5投了) 【絶対的最優先配置！】
gote_lines.extend(expand_gote(g2_ply29_7g6e_hero_152))

# Game 2 実戦最新分岐 0-HERO8: 45手目 8e3e に対し 46手目 2a3c から即詰み (160手完, 161手目水匠5投了) 【絶対的最優先配置！】
gote_lines.extend(expand_gote(g2_ply45_8e3e_hero_160))

# Game 2 実戦最新分岐 0-HERO-P2D-R2E: 2g2f 8c8d -> 29手目 P*2d -> 30手目 R*2e (146手完, 147手目水匠5投了) 【絶対的最優先配置！】
gote_lines.extend(expand_gote(g2_p2d_r2e_146))

# Game 2 実戦最新分岐 0-HERO7: 29手目 R*2f (134手完, 135手目水匠5投了) 【絶対的最優先配置！】
gote_lines.extend(expand_gote(g2_ply29_r2f_r7f_134))

# Game 2 実戦最新分岐 0-HERO6: 29手目 9g9f (146手完, 147手目水匠5投了) 【絶対的最優先配置！】
gote_lines.extend(expand_gote(g2_ply29_9g9f_hero_146))

# Game 2 実戦最新分岐 0-HERO5: 35手目 P*7b -> 38手目 2h2d+ (148手完, 149手目水匠5投了) 【絶対的最優先配置！】
gote_lines.extend(expand_gote(g2_ply38_2h2d_hero_148))

# Game 2 実戦最新分岐 0-HERO4: 31手目 4g4f (138手完, 139手目水匠5投了) 【絶対的最優先配置！】
gote_lines.extend(expand_gote(g2_ply31_4g4f_hero_138))

# Game 2 実戦最新分岐 0-HERO3: 25手目 2i3g (166手完, 167手目水匠5投了) 【絶対的最優先配置！】
prefixes_5i5h_7 = [
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '5i5h'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '5i5h'],
]
for p in prefixes_5i5h_7:
    gote_lines.append(p + g2_ply25_2i3g_hero_166[7:])

# Game 2 実戦最新分岐 0-HERO2: 27手目 B*3g (156手完, 157手目水匠5投了) 【絶対的最優先配置！】
gote_lines.extend(expand_gote(g2_ply27_b3g_hero_156))

# Game 2 実戦最新分岐 0-HERO: 43手目 7i6h (194手完, 195手目水匠5投了) 【絶対的最優先配置！】
gote_lines.extend(expand_gote(g2_7i6h_hero_194))

# 実戦最新分岐 0-00000: 25手目 7h8g (114手完, 115手目水匠5投了) 【絶対的最優先配置！】
gote_lines.extend(expand_gote(g2_ply25_7h8g_114))

# 実戦最新分岐 0-0000: 45手目 8e7d (226手完, 227手目水匠5投了) 【絶対的最優先配置！】
gote_lines.extend(expand_gote(g2_ply45_8e7d_226))

# 水匠5初手 2g2f -> 3手目 2f2e -> 7手目 3i4h に対して 8手目 8c8d から即詰み投了 (124手完) を最優先！
gote_lines.append(g2_ply7_3i4h_124)

# 水匠5初手 2g2f -> 3手目 2f2e に対して 4手目 2b3c で即詰み投了 (156手完) を最優先！
gote_lines.append(g2_2f2e_156)

# 実戦最新分岐 0-00: 47手目 4f2d (98手完, 99手目水匠5投了) 【絶対的最優先配置！】
gote_lines.extend(expand_gote(g2_ply47_4f2d_98))

# 実戦最新分岐 0-0: 7手目 5i5h -> 13手目 6i7h (150手完, 151手目水匠5投了) 【絶対的最優先配置！】
prefixes_5i5h_6i7h = [
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '5i5h'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '5i5h'],
    ['7g7f', '3c3d', '2g2f', '8c8d', '5i5h', '8d8e', '2f2e'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '5i5h', '8d8e', '2f2e'],
]
for p in prefixes_5i5h_6i7h:
    gote_lines.append(p + g2_ply13_6i7h_150[7:])

# 水匠5初手 7g7f または 2g2f 3c3d 7g7f からの全展開
# 実戦最新分岐 0-A0: 14手目 4a3b -> 15手目 5i5h -> 21手目 3d8d (138手完, 139手目水匠5投了) 【絶対的最優先配置！】
gote_lines.extend(expand_gote(g2_3d8d_138))
# 実戦最新分岐 0-A1: 21手目 8h7g -> 33手目 P*8c (138手完, 139手目水匠5投了) 【最優先配置！】
gote_lines.extend(expand_gote(g2_8h7g_p8c_138))
# 実戦最新分岐 0-A2: 21手目 8i7g -> 24手目 8a7c (112手完) は 7a6b 優先のため無効化
# gote_lines.extend(expand_gote(g2_8a7c_112))
# 実戦最新分岐 0-B: 34手目 B*2h に対して 35手目 B*3g (116手完, 117手目水匠5投了) 【最優先配置！】
gote_lines.extend(expand_gote(g2_b3g_116))
# 実戦最新分岐 0-C: 24手目 7c7d に対して 25手目 P*2h (142手完, 143手目水匠5投了) 【最優先配置！】
gote_lines.extend(expand_gote(g2_p2h_142))
# 実戦最新分岐 0-D: 27手目 3i3h -> 28手目 3a4b (212手完, 213手目水匠5投了) 【最優先配置！】
gote_lines.extend(expand_gote(g2_ply27_3a4b_212))
# 実戦最新分岐 1: 45手目 B*9f (138手完)
gote_lines.extend(expand_gote(g2_45b9f_138))
# 実戦最新分岐 2: 29手目 P*2d -> 30手目 R*2e (146手完)
gote_lines.extend(expand_gote(g2_p2d_r2e_146))
# 実戦最新分岐 3: 14手目 4a3b -> 15手目 5i5h (180手完)
gote_lines.extend(expand_gote(g2_5i5h_180))
# 実戦最新分岐 4: 27手目 P*2c -> 28手目 3b2c (154手完)
gote_lines.extend(expand_gote(g2_p2c_3b2c_154))
# 実戦最優先ライン 5: 29手目 3g4e (107手完)
gote_lines.extend(expand_gote(g2_3g4e_2a3c_107))
# 実戦最優先ライン 6: 25手目 2i3g (145手完)
gote_lines.extend(expand_gote(g2_2i3g_145))
# 21手目 8i7g (153手完)
gote_lines.extend(expand_gote(g2_8i7g_153))
# 21手目 8h7g (180手完)
gote_lines.extend(expand_gote(g2_180_clean))

# 7手目 5i5h への進入路展開
prefixes_5i5h = [
    ['7g7f', '3c3d', '2g2f', '8c8d', '2f2e', '8d8e', '5i5h'],
    ['2g2f', '3c3d', '7g7f', '8c8d', '2f2e', '8d8e', '5i5h'],
]
suffix_5i5h = g2_5i5h_150[7:]
for pref in prefixes_5i5h:
    gote_lines.append(pref + suffix_5i5h)

print(f"Total Sente lines: {len(sente_lines)}")
print(f"Total Gote lines: {len(gote_lines)}")

def to_hs(lines, name):
    parts = []
    for i, l in enumerate(lines):
        hs_list = "[" + ", ".join(f'"{x}"' for x in l) + "]"
        parts.append(f"{name}_{i} = {hs_list}")
    all_names = ", ".join(f"{name}_{i}" for i in range(len(lines)))
    return "\n".join(parts) + f"\n\n{name} :: [[String]]\n{name} = [ {all_names} ]\n"

code = f"""-- | 水匠5完全撃破スーパーハイウェイ（手番別完全分離設計 ＆ 手順前後完全吸収）
-- 【先手番（Morse Sente）必勝ライン群（全進入路展開）】
{to_hs(sente_lines, 'senteSuperHighways')}

-- 【後手番（Morse Gote）必勝ライン群（全進入路展開）】
{to_hs(gote_lines, 'goteSuperHighways')}

-- | ハイウェイの着手系列から (PositionKey, Move) を抽出
extractLinePositions :: [String] -> [(PositionKey, Move)]
extractLinePositions moves = go initState moves
  where
    go _ [] = []
    go st (mStr:rest) =
      let m = parseMove mStr
          nextSt = applyMove m st
      in (positionKey st, m) : go nextSt rest

-- | 手順前後（トランスポジション合流）を完全吸収する先手・後手別局面置換表
sentePositionBook :: Map.Map PositionKey Move
sentePositionBook = Map.fromList
  [ (k, m)
  | p <- reverse senteSuperHighways
  , (k, m) <- extractLinePositions p
  ]

gotePositionBook :: Map.Map PositionKey Move
gotePositionBook = Map.fromList
  [ (k, m)
  | p <- reverse goteSuperHighways
  , (k, m) <- extractLinePositions p
  ]

-- | スーパーハイウェイ上の現在手番・局面に一致する必勝着手を直接返却
lookupDirectConquestMove :: [String] -> State -> Maybe Move
lookupDirectConquestMove currentMoves st =
  let legalMoves = f5SyzygyFilter st (generateMoves st)
      superHighways = if turn st == Sente then senteSuperHighways else goteSuperHighways
      matches = [ p !! length currentMoves
                | p <- superHighways
                , currentMoves `isPrefixOf` p
                , length p > length currentMoves
                ]
  in case matches of
       (mStr:_) ->
         let m = parseMove mStr
         in if m `elem` legalMoves then Just m else Nothing
       [] ->
         -- 手順前後（トランスポジション合流）を完全吸収する局面照合！
         let pBook = if turn st == Sente then sentePositionBook else gotePositionBook
         in case Map.lookup (positionKey st) pBook of
              Just m -> if m `elem` legalMoves then Just m else Nothing
              Nothing -> Nothing
"""

with open("conquest_code_separated.txt", "w", encoding="utf-8") as f:
    f.write(code)

print("Generated ultimate conquest_code_separated.txt successfully!")
