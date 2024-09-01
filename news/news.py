#!/usr/bin/env python3

import requests
import datetime
import re
import json
import os
from wxpusher import WxPusher
from urllib.parse import quote
from loguru import logger as log

log.add(f"{os.environ['HOME']}/news.log", rotation="200KB")

today = datetime.date.today().strftime('%Y-%m-%d')
summary = f"{today}每日微语 - 新闻"
content = f"### {summary}\n"

# 澎湃热榜
log.info("开始抓取澎湃热榜")
content += f"##### 澎湃热榜\n"
url = 'https://cache.thepaper.cn/contentapi/wwwIndex/rightSidebar'
try:
    response = requests.get(url)
    response.raise_for_status()
    list_data = response.json().get('data', {}).get('hotNews', [])
    for i, news in enumerate(list_data, start=1):
        url = f"https://www.thepaper.cn/newsDetail_forward_{news['contId']}"
        title = news['name']
        content += f"- {i}、[{title}]({url})\n"
except Exception as e:
    log.error(f"澎湃热榜抓取失败: {e}. Response text: {locals().get('response', {}).text}")
content += "\n"

# 百度热搜
log.info("开始抓取百度热搜")
content += f"##### 百度热搜\n"
type = 'realtime'
url = f'https://top.baidu.com/board?tab={type}'
headers = {
    "User-Agent": "Mozilla/5.0 (iPhone; CPU iPhone OS 14_2_1 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) FxiOS/1.0 Mobile/12F69 Safari/605.1.15"
}
try:
    response = requests.get(url, headers=headers)
    response.raise_for_status()
    pattern = re.compile(r'<!--s-data:(.*?)-->', re.S)
    match_result = pattern.search(response.text)
    if match_result:
        json_object = json.loads(match_result.group(1)).get('cards')[0].get('content')
        for i, v in enumerate(json_object, start=1):
            title = v.get('word')
            url = f"https://www.baidu.com/s?wd={quote(v.get('query'))}"
            content += f"- {i}、[{title}]({url})\n"
    else:
        log.error("未找到百度热搜匹配数据")
except Exception as e:
    log.error(f"百度热搜抓取失败: {e}. Response text: {locals().get('response', {}).text}")
content += "\n"

# 头条热榜
log.info("开始抓取头条热榜")
content += f"##### 头条热榜\n"
url = 'https://www.toutiao.com/hot-event/hot-board/?origin=toutiao_pc'
try:
    response = requests.get(url)
    response.raise_for_status()
    list_data = response.json().get('data', [])
    for i, v in enumerate(list_data, start=1):
        title = v.get('Title')
        url = f"https://www.toutiao.com/trending/{v.get('ClusterIdStr')}/"
        content += f"- {i}、[{title}]({url})\n"
except Exception as e:
    log.error(f"头条热榜抓取失败: {e}. Response text: {locals().get('response', {}).text}")
content += "\n"

# 网易热点
log.info("开始抓取网易热点")
content += f"##### 网易热点\n"
url = 'https://m.163.com/fe/api/hot/news/flow'
try:
    response = requests.get(url)
    response.raise_for_status()
    list_data = response.json().get('data', {}).get('list', [])
    for i, v in enumerate(list_data, start=1):
        title = v.get('title')
        url = f"https://www.163.com/dy/article/{v.get('docid')}.html"
        content += f"- {i}、[{title}]({url})\n"
except Exception as e:
    log.error(f"网易热点抓取失败: {e}. Response text: {locals().get('response', {}).text}")
content += "\n"

# 知乎热榜
log.info("开始抓取知乎热榜")
content += f"##### 知乎热榜\n"
url = 'https://www.zhihu.com/api/v3/feed/topstory/hot-lists/total?limit=50&desktop=true'
try:
    response = requests.get(url)
    response.raise_for_status()
    list_data = response.json().get('data', [])
    for i, v in enumerate(list_data, start=1):
        data = v.get('target')
        title = data.get('title')
        url = f"https://www.zhihu.com/question/{data.get('id')}"
        content += f"- {i}、[{title}]({url})\n"
except Exception as e:
    log.error(f"知乎热榜抓取失败: {e}. Response text: {locals().get('response', {}).text}")
content += "\n"

# 知乎日报
log.info("开始抓取知乎日报")
content += f"##### 知乎日报\n"
url = 'https://daily.zhihu.com/api/4/news/latest'
try:
    response = requests.get(url)
    response.raise_for_status()
    list_data = response.json().get('stories', [])
    for i, v in enumerate(list_data, start=1):
        title = v.get('title')
        url = f"https://daily.zhihu.com/story/{v.get('id')}"
        content += f"- {i}、[{title}]({url})\n"
except Exception as e:
    log.error(f"知乎日报抓取失败: {e}. Response text: {locals().get('response', {}).text}")
content += "\n"

# 新浪热榜
log.info("开始抓取新浪热榜")
content += f"##### 新浪热榜\n"
url = 'https://newsapp.sina.cn/api/hotlist?newsId=HB-1-snhs%2Ftop_news_list-all'
try:
    response = requests.get(url)
    response.raise_for_status()
    list_data = response.json().get('data', {}).get('hotList', [])
    for i, v in enumerate(list_data, start=1):
        title = v['info']['title'] + " 热度:" + v['info']['hotValue']
        url = v['base']['base']['url']
        content += f"- {i}、[{title}]({url})\n"
except Exception as e:
    log.error(f"新浪热榜抓取失败: {e}. Response text: {locals().get('response', {}).text}")
content += "\n"

# 腾讯新闻热点榜
log.info("开始抓取腾讯新闻热点榜")
content += f"##### 腾讯新闻热点榜\n"
url = 'https://r.inews.qq.com/gw/event/hot_ranking_list?page_size=20'
try:
    response = requests.get(url)
    response.raise_for_status()
    list_data = response.json().get('idlist', [])[0]['newslist']
    list_data = list_data[1:]
    for i, v in enumerate(list_data, start=1):
        title = v.get('title')
        url = v.get('url')
        content += f"- {i}、[{title}]({url})\n"
except Exception as e:
    log.error(f"腾讯新闻热点榜抓取失败: {e}. Response text: {locals().get('response', {}).text}")
content += "\n"

# HelloGitHub
log.info("开始抓取HelloGitHub")
content += f"##### hellogithub\n"
url = 'https://abroad.hellogithub.com/v1/?sort_by=featured&tid=&page=1'
try:
    response = requests.get(url)
    response.raise_for_status()
    list_data = response.json().get('data', [])
    for i, v in enumerate(list_data, start=1):
        title = v.get('title')
        url = f"https://hellogithub.com/repository/{v['item_id']}"
        content += f"- {i}、[{title}]({url})\n"
except Exception as e:
    log.error(f"HelloGitHub抓取失败: {e}. Response text: {locals().get('response', {}).text}")
content += "\n"

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

