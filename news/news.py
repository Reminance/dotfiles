#!/usr/bin/env python3

import requests
import datetime
import re
import json
from wxpusher import WxPusher

today = datetime.date.today().strftime('%Y-%m-%d')
summary = f"{today}每日微语 - 新闻"
content = f"### {summary}\n"

# 澎湃
content += f"##### 澎湃热榜\n"
url = 'https://cache.thepaper.cn/contentapi/wwwIndex/rightSidebar'
response = requests.get(url)
if response.status_code != 200:
    print('thepaper response not 200')
    exit(0)

list_data = response.json()['data']['hotNews']
i = 0
for news in list_data:
    i += 1
    url = f"https://www.thepaper.cn/newsDetail_forward_{news['contId']}"
    title = news['name']
    # content += f"<div><span id=\"index\">{i}.</span><a target=]=\"_blank\" href=\"{url}\">  {title}</a></div>"
    content += f"- {i}、[{title}]({url})\n"
content += "\n"

# 百度
# type: { realtime: "热搜", novel: "小说", movie: "电影", teleplay: "电视剧", car: "汽车", game: "游戏", }
content += f"##### 百度热搜\n"
type = 'realtime'
url = f'https://top.baidu.com/board?tab={type}'
headers = {
    "User-Agent": "Mozilla/5.0 (iPhone; CPU iPhone OS 14_2_1 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) FxiOS/1.0 Mobile/12F69 Safari/605.1.15"
}
response = requests.get(url, headers=headers)
if response.status_code != 200:
    print('baidu response not 200')
    exit(0)
pattern = re.compile(r'<!--s-data:(.*?)-->', re.S)
match_result = pattern.search(response.text)
if not match_result:
    print( "No matching data found.")
    exit(0)
json_object = json.loads(match_result.group(1)).get('cards')[0].get('content')
for i, v in enumerate(json_object, start=1):
    title = v.get('word')
    url = f"https://www.baidu.com/s?wd={v.get('query')}"
    content += f"- {i}、[{title}]({url})\n"
content+= "\n"

# 头条
content += f"##### 头条热榜\n"
url = 'https://www.toutiao.com/hot-event/hot-board/?origin=toutiao_pc'
response = requests.get(url)
if response.status_code != 200:
    print('toutiao response not 200')
    exit(0)
list_data = response.json().get('data')
for i, v in enumerate(list_data, start=1):
    title = v.get('Title')
    url = f"https://www.toutiao.com/trending/{v.get('ClusterIdStr')}/"
    content += f"- {i}、[{title}]({url})\n"
content+= "\n"
        
# 网易
content += f"##### 网易热点\n"
url = 'https://m.163.com/fe/api/hot/news/flow'
response = requests.get(url)
if response.status_code != 200:
    print('netease response not 200')
    exit(0)
list_data = response.json().get('data').get('list')
for i, v in enumerate(list_data, start=1):
    title = v.get('title')
    url = f"https://www.163.com/dy/article/{v.get('docid')}.html"
    content += f"- {i}、[{title}]({url})\n"
content+= "\n"

# 知乎
content += f"##### 知乎热榜\n"
url = 'https://www.zhihu.com/api/v3/feed/topstory/hot-lists/total?limit=50&desktop=true'
response = requests.get(url)
if response.status_code != 200:
    print('zhihu response not 200')
    exit(0)
list_data = response.json().get('data')
for i, v in enumerate(list_data, start=1):
    data = v.get('target')
    title = data.get('title')
    url = f"https://www.zhihu.com/question/{data.get('id')}"
    content += f"- {i}、[{title}]({url})\n"
content+= "\n"
print(content)


# 获取UID https://wxpusher.zjiecode.com/demo
# IyDpk77mjqXlhbPms6jlupTnlKggaHR0cHM6Ly93eHB1c2hlci56amllY29kZS5jb20vd3h1c2VyLz90eXBlPTEmaWQ9ODI1OTIjL2ZvbGxvdwojIOmTvuaOpeWFs+azqFJlbWluYW5jZSB0b3BpYzogaHR0cHM6Ly93eHB1c2hlci56amllY29kZS5jb20vd3h1c2VyLz90eXBlPTImaWQ9MzMwOTQjL2ZvbGxvdwo=
# 内容类型 1表示文字  2表示html(只发送body标签内部的数据即可，不包括body标签，推荐使用这种) 3表示markdown 
WxPusher.send_message(
        content_type=3,
        content=content,
        summary=summary,
        topic_ids=[''],
        # uids=[''],
        token=''
        #ICAgICAgICB0b3BpY19pZHM9WyczMzA5NCddLAogICAgICAgICMgdWlkcz1bJ1VJRF9KYlBWc3VENTk0a2RlSUJaTlFBbnk2Tm1PZlN6J10sCiAgICAgICAgdG9rZW49J0FUX1hibzdWdno5Y2VVSW8xNXZ0bkUzTE1oeUdZWXFuNDZnJwo=
        )

