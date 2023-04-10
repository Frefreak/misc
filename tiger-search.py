import json
import sys
import requests

# r = requests.post("https://tiger-code.com/api/diction/search", data={"key": sys.argv[1]})
r = requests.get(f"https://tiger-code.com/api_2/dic/{sys.argv[1]}")
print(json.dumps(json.loads(r.text), indent=2, ensure_ascii=False))
