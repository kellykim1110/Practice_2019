### DB을 연결하는 기능.
import pymongo
conn = pymongo.MongoClient()
print(conn)

### DB와 COL선택
db = conn['local']
db_col = db.items  # db['items']
print(db_col)

