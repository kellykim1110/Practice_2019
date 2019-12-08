import conn

## 삽입 기능 구현(1건)
def datInsert():
    print(conn.db_col)

    # 데이터 한건만 넣어보기
    data = {'name':'hero4','power':500,'height':170,'money':10000}
    conn.db_col.insert(data)
    print("데이터가 추가되었습니다.")