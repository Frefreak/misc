import requests
from typing import Optional
from bs4 import BeautifulSoup
from duckduckgo_search import ddg

root_url = "https://www.db.yugioh-card.com"
cardsearch_url = "https://www.db.yugioh-card.com/yugiohdb/card_search.action"


def keyword_to_link(keyword: str, locale: str) -> Optional[str]:
    r = requests.get(
        cardsearch_url, params={"ope": 1, "request_locale": locale, "keyword": keyword}
    )
    bs = BeautifulSoup(r.text, "lxml")
    inputs = bs.find_all("input", class_="link_value")
    if len(inputs) != 1:
        return None
    return root_url + inputs[0].attrs["value"]


def keyword_to_link_ja(keyword: str) -> Optional[str]:
    return keyword_to_link(keyword, "ja")


def extract_name_from_link(link: str) -> str:
    r = requests.get(link, params={"request_locale": "en"})
    bs = BeautifulSoup(r.text, "lxml")
    return bs.find("header", id="broad_title").find("div").find("h1").text.strip()  # type: ignore


def duckduckgo_search_link(keyword: str) -> Optional[str]:
    try:
        results = ddg(f"{keyword}")
    except IndexError:
        return None
    for result in results:
        if result['href'].startswith('https://www.db.yugioh-card.com/yugiohdb/card_search.action?ope=2'):
            link = result['href']
            link = link.replace('request_locale=ja', 'request_locale=en')
            return link
    return None
