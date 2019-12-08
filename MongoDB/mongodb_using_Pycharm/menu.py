import conn
import datsearch as ds
import datInsert as ins

def dbInfo():
    print(conn.conn.database_names())     ## 파일명.변수명
    dbs = conn.conn.database_names()
    db_con = conn.conn

    print("db의 컬렉션을 확인")
    for one_db in dbs:
        db = conn.conn[one_db]  ## db하나 선택
        print(db.collection_names())
        db_c = db.collection_names()

        for one_col in db_c:
            print("컬렉션들의 데이터 수: {}".format(one_col))
            datcnt = db[one_col].count()
            print(datcnt)


    # print(conn.conn.database_names())  ## 파일명.변수명
    # conn.database_names()
    # db.collection_names()


### DB을 작업을 위해 메뉴 구현.
def menuPrint():
    print("DB정보(0), 검색(1), 삭제(2), 삽입(3), 변경(4) 선택해 주세요?")
    sel = int(input())
    return sel

if __name__=="__main__":
    menuint = menuPrint()
    if menuint==0:
        dbInfo()  ## db에 관련된 정보를 보겠다.

    elif menuint==1:
        ds.Search()
    elif menuint==3:
        print("데이터를 1건 추가합니다")
        ins.datInsert()