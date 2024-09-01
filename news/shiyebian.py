#!/usr/bin/env python3

import requests
import datetime
import re
import os
import json
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import chardet
from urllib.parse import urljoin, urlparse
from wxpusher import WxPusher
from loguru import logger as log

log.add(f"{os.environ['HOME']}/shiyebian.log", rotation="200KB")

today = datetime.date.today().strftime('%Y-%m-%d')
summary = f"{today}每日微语 - 事业编"
content = ""
min_time = '2023-01-01'

# 广西壮族自治区教育厅
log.info("开始抓取广西壮族自治区教育厅")
content += "### 广西壮族自治区教育厅\n"
url = "http://jyt.gxzf.gov.cn/zfxxgk/fdzdgknr/rsxx/klzp/"
try:
    response = requests.get(url)
    response.raise_for_status()
    response.encoding = chardet.detect(response.content)['encoding']
    soup = BeautifulSoup(response.text, 'html.parser')
    data = soup.find('ul', class_='more-list')
    for i, li in enumerate(data.find_all('li'), start=1):
        title = li.find('span').text + " " + li.a.text
        content += f"- {i}、[{title}]({urljoin(url, li.a.get('href'))})\n"
except Exception as e:
    log.error(f"广西壮族自治区教育厅抓取失败: {e}. Response text: {locals().get('response', {}).text}")

# 广西南宁市教育局
log.info("开始抓取广西南宁市教育局")
content += "### 广西南宁市教育局\n"
url = "https://jy.nanning.gov.cn/xxgk/fdzdgknr/rsxx/klzpygs/"
try:
    response = requests.get(url)
    response.raise_for_status()
    response.encoding = chardet.detect(response.content)['encoding']
    soup = BeautifulSoup(response.text, 'html.parser')
    data = soup.find('div', class_='nav1Cont')
    for i, li in enumerate(data.find('ul').find_all('li'), start=1):
        time_str = li.find('span', class_='time').text.replace("[", "").replace("]", "")
        if datetime.datetime.strptime(time_str, "%Y-%m-%d") > datetime.datetime.strptime(min_time, "%Y-%m-%d"):
            title = time_str + " " + li.a.get_text(strip=True)
            url_inner = li.find('a')['href'] if li.find('a') else '#'
            content += f"- {i}、[{title}]({urljoin(url, url_inner)})\n"
except Exception as e:
    log.error(f"广西南宁市教育局抓取失败: {e}. Response text: {locals().get('response', {}).text}")

# 抓取页面静态内容
def fetch_dynamic_content(url):
    driver = webdriver.Chrome()  # or `webdriver.Firefox()`, 确保驱动程序已正确配置
    driver.get(url)
    try:
        sydw_nav_div = WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.ID, "sydw_nav")))  # 等待 sydw_nav 出现
        target_li = sydw_nav_div.find_element(By.XPATH, ".//li[a[text()='报名中']]")
        target_li.click()  # 触发点击事件
        newslist_div = WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.ID, "Newslist")))  # 等待 Newslist div 出现
        page_source = driver.page_source  # 获取新的页面 HTML
    finally:
        driver.quit()
    return page_source

# 广西就业平台 事业单位招聘专栏
log.info("开始抓取广西就业平台 事业单位招聘专栏")
content += "### 广西就业平台 事业单位招聘专栏\n"
url = "https://www.gx12333.net/market/business/website/cms/agenciesrecruitlist.html"
try:
    html_content = fetch_dynamic_content(url)
    soup = BeautifulSoup(html_content, 'html.parser')
    newslist_div = soup.find('div', id='Newslist')
    if not newslist_div:
        raise ValueError("未找到id为Newslist的div")
    ul_element = newslist_div.find('ul')
    if not ul_element:
        raise ValueError("未找到Newslist div 中的 ul 元素")
    for i, li in enumerate(ul_element.find_all('li'), start=1):
        time_str = li.find('div', class_='date').span.text
        for cite in li.find_all('cite'):
            cite.extract()
        url_inner = li.find('a')['onclick'] if li.find('a') else '#'
        match = re.search(r"goToMenu\('([^']+)'\)", url_inner)
        url_inner = match.group(1) if match else ""
        real_url = urljoin(urlparse(url).scheme + "://" + urlparse(url).netloc, "/market" + url_inner)
        for span in li.find('div', class_='date').find_all('span'):
            span.extract()
        title = time_str + " " + li.find('div', class_='tit').a.get_text(strip=True) + " " + li.find('div', class_='date').get_text(strip=True)
        content += f"- {i}、[{title}]({real_url})\n"
except Exception as e:
    log.error(f"广西就业平台抓取失败: {e}. Response text: {locals().get('response', {}).text}")

# 南宁人才网
log.info("开始抓取南宁人才网")
content += "### 南宁人才网\n"
url = "http://0771.gxrcw.com/news/classsub.aspx?classid=1058"
try:
    response = requests.get(url)
    response.raise_for_status()
    response.encoding = chardet.detect(response.content)['encoding']
    soup = BeautifulSoup(response.text, 'html.parser')
    data = soup.find('div', class_='iMainlc')
    for i, li in enumerate(data.find('ul').find_all('li'), start=1):
        time_str = li.p.i.text
        if datetime.datetime.strptime(time_str, "%Y/%m/%d") > datetime.datetime.strptime(min_time, "%Y-%m-%d"):
            title = time_str + " " + li.b.a.get_text(strip=True)
            url_inner = li.find('a')['href'] if li.find('a') else '#'
            content += f"- {i}、[{title}]({urljoin(url, url_inner)})\n"
except Exception as e:
    log.error(f"南宁人才网抓取失败: {e}. Response text: {locals().get('response', {}).text}")

# 广西人事考试网
log.info("开始抓取广西人事考试网")
content += "### 广西人事考试网\n"
url = "https://www.gxpta.com.cn/ksxm/sydwzpks/"
try:
    response = requests.get(url, verify=False)
    response.raise_for_status()
    response.encoding = chardet.detect(response.content)['encoding']
    soup = BeautifulSoup(response.text, 'html.parser')
    data = soup.find('ul', class_='articles')
    for i, li in enumerate(data.find_all('li'), start=1):
        time_str = li.span.text.replace("[", "").replace("]", "")
        if datetime.datetime.strptime(time_str, "%Y-%m-%d") > datetime.datetime.strptime(min_time, "%Y-%m-%d"):
            title = time_str + " " + li.a.get_text(strip=True)
            url_inner = li.find('a')['href'] if li.find('a') else '#'
            content += f"- {i}、[{title}]({urljoin(url, url_inner)})\n"
except Exception as e:
    log.error(f"广西人事考试网抓取失败: {e}. Response text: {locals().get('response', {}).text}")

# 事业编考试信息发布站
log.info("开始抓取事业编考试信息发布站")
content += "### 事业编考试信息发布站\n"
url = "https://www.shiyebian.net/guangxi/nanning/index.html"
try:
    response = requests.get(url)
    response.raise_for_status()
    response.encoding = chardet.detect(response.content)['encoding']
    soup = BeautifulSoup(response.text, 'html.parser')
    data = soup.find('div', class_='main')
    i = 0
    for _, ul in enumerate(data.find_all('ul'), start=1):
        for _, li in enumerate(ul.find_all('li'), start=1):
            i += 1
            time_str = li.span.text
            if datetime.datetime.strptime(time_str, "%Y-%m-%d") > datetime.datetime.strptime(min_time, "%Y-%m-%d"):
                title = time_str + " " + li.a.get_text(strip=True)
                url_inner = li.find('a')['href'] if li.find('a') else '#'
                content += f"- {i}、[{title}]({urljoin(url, url_inner)})\n"
except Exception as e:
    log.error(f"事业编考试信息发布站抓取失败: {e}. Response text: {locals().get('response', {}).text}")

# 华图教育 人事考试
log.info("开始抓取华图教育 人事考试")
content += "### 华图教育 人事考试\n"
url = "https://nanning.huatu.com/"
try:
    response = requests.get(url)
    response.raise_for_status()
    response.encoding = chardet.detect(response.content)['encoding']
    soup = BeautifulSoup(response.text, 'html.parser')
    data = soup.find('div', class_='fxlist_Conday')
    for i, li in enumerate(data.ul.find_all('li'), start=1):
        time_str = li.time.font.text if li.time.font else li.time.get_text(strip=True)
        if len(time_str) == 5:
            time_str = "2024-" + time_str
        if datetime.datetime.strptime(time_str, "%Y-%m-%d") > datetime.datetime.strptime(min_time, "%Y-%m-%d"):
            for cite in li.find_all('cite'):
                cite.extract()
            title = time_str + " " + li.a.get_text(strip=True) + " " + li.mark.text
            url = li.find('a')['href'] if li.find('a') else '#'
            content += f"- {i}、[{title}]({url})\n"
except Exception as e:
    log.error(f"华图教育 人事考试抓取失败: {e}. Response text: {locals().get('response', {}).text}")

print(content)

# 获取UID https://wxpusher.zjiecode.com/demo
# IyDpk77mjqXlhbPms6jlupTnlKggaHR0cHM6Ly93eHB1c2hlci56amllY29kZS5jb20vd3h1c2VyLz90eXBlPTEmaWQ9ODI1OTIjL2ZvbGxvdwojIOmTvuaOpeWFs+azqFJlbWluYW5jZSB0b3BpYzogaHR0cHM6Ly93eHB1c2hlci56amllY29kZS5jb20vd3h1c2VyLz90eXBlPTImaWQ9MzMwOTQjL2ZvbGxvdwo=
# 内容类型 1表示文字  2表示html(只发送body标签内部的数据即可，不包括body标签，推荐使用这种) 3表示markdown 
try:
    WxPusher.send_message(
            content_type=3,
            content=content,
            summary=summary,
            topic_ids=[''],
            # uids=[''],
            token=''
            #ICAgICAgICB0b3BpY19pZHM9WyczMzA5NCddLAogICAgICAgICMgdWlkcz1bJ1VJRF9KYlBWc3VENTk0a2RlSUJaTlFBbnk2Tm1PZlN6J10sCiAgICAgICAgdG9rZW49J0FUX1hibzdWdno5Y2VVSW8xNXZ0bkUzTE1oeUdZWXFuNDZnJwo=
            )
    log.info("推送shiyebian成功")
except Exception as e:
    log.error(f"消息推送失败: {e}")


