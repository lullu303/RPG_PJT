
import konlpy.tag

with open('./data/NaverNews_인기 콘솔게임', 'r', encoding='utf8') as f:
    content = f.read()

filtered_content = content.replace('.', '').replace(',','').replace("'","").replace('·', ' ').replace('=','').replace('\n','')

Okt = konlpy.tag.Okt()
Okt_morphs = Okt.pos(filtered_content)  # 튜플반환
print(Okt_morphs)