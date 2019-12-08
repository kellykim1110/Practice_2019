import conn

def Search():
    print(conn.db_col)
    qsel = int( input("전체검색(1), 부분검색(2):") )
    if qsel==1:
        print("전체검색이다.")
        print("전체 검색")
        dat = conn.db_col.find()
    else:
        print("부분검색")

    for one in dat:
        print(one)

